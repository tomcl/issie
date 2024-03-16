module SheetBeautifyD1
//-----------------Module for D1 beautify Helper functions--------------------------//
// I try to section the helpers out handling symbol/custom components
// next are wire helpers


open Optics
open Optics.Operators
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Helpers
open Symbol
open BlockHelpers
open SheetBeautifyHelpers
open RotateScale
open BusWire
    module D1Helpers =
        // D1 HELPER FUNCTIONS ----------------------------------------------------------------------
    
        /// returns all singly compoenents with one output xor input port
        let getSinglyComp (sheet: SheetT.Model): Symbol list =
            Optic.get symbols_ sheet.Wire.Symbol
            |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                match getComponentProperties sym.Component.Type sym.Component.Label with
                | (0, 1, _, _) -> sym :: acc
                | (1, 0,_,_) -> sym :: acc
                | _ -> acc) []
            |> List.rev
    
        /// returns all multiply connected components on a sheet
        let getMultiplyComp (sheet: SheetT.Model): Symbol list =
            Optic.get symbols_ sheet.Wire.Symbol
            |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                let (numInputs, numOutputs, _, _) = getComponentProperties sym.Component.Type sym.Component.Label
                match (numInputs, numOutputs) with
                | (i, o) when i > 1 || o > 1 -> sym :: acc  // Components with more than one input or output
                | _ -> acc) []  // Ignore components with one or no inputs/outputs
            |> List.rev
    
        
        /// returns the ports for a given symbol
        let getPortSym (model: SymbolT.Model) (sym: Symbol): Map<string,Port>  =
            let portId = sym ^. (portMaps_ >-> orientation_) |> Map.keys
            portId
            |> Seq.fold (fun acc id -> 
                match Map.tryFind id model.Ports with
                | Some port -> Map.add id port acc
                | None -> acc) Map.empty
    
        /// Finds the corresponding WId(s) and other port(s) for a given Port based on connections.
        /// `port` is the given port to find matches for.
        /// `connections` is the list of all connections to search through.
        let findOtherPorts (port: Port) (connections: Connection list): (string * Port) list =
            connections |> List.collect (fun connection ->
                match connection with
                | { Id = id; Source = src; Target = tgt } when src = port -> [(id,tgt)] // Port is the source; collect the target
                | { Id = id; Source = src; Target = tgt } when tgt = port -> [(id,src)] // Port is the target; collect the source
                | _ -> [] // No match, proceed to the next connection
            )
        
        /// return the corresponding Symbol for a given Port
        let getSym (sheet:SheetT.Model) (port:Port): Symbol =
            let sym =
                port.HostId
                |> ComponentId
                |> fun id -> sheet.Wire.Symbol.Symbols[id]
            sym

        /// For a given symbol and edge, will return the number of ports on that edge
        let getPortCount (sym: Symbol) (edge: Edge): int =
            sym 
            |> Optic.get portMaps_
            |> (fun pm -> pm.Order)
            |> (fun orderMap -> Map.find edge orderMap)
            |> List.length

        /// Returns the offset of two ports relative to portA
        let getPortOffset (sheet:SheetT.Model) (portA: Port) (portB:Port): XYPos =
            let symA = getSym sheet portA
            let symB = getSym sheet portB
            let offset =
                {
                    X = (calculatePortRealPos (makePortInfo symB portB)).X - (calculatePortRealPos (makePortInfo symA portA)).X;
                    Y = (calculatePortRealPos (makePortInfo symB portB)).Y - (calculatePortRealPos (makePortInfo symA portA)).Y
                }
            offset

        /// For Components with multiple output ports, will return the most suitable Port to align
        let choosePortAlign (sheet:SheetT.Model) (ports: Port list): Port * Port =
            let connections = extractConnections sheet.Wire
            let portPair =
                ports
                |> List.collect (fun port -> 
                    let portOrientation = getSymbolPortOrientation (getSym sheet port) port
                    findOtherPorts port connections
                    |> List.choose (fun (_, otherPort) ->
                        let otherPortOrientation = getSymbolPortOrientation (getSym sheet otherPort) otherPort
                        if portOrientation = otherPortOrientation.Opposite then
                            let offsetXY = getPortOffset sheet port otherPort
                            let offset = match portOrientation with
                                         | Top | Bottom -> offsetXY.X
                                         | Left | Right -> offsetXY.Y
                            Some (offset, (port, otherPort))
                        else
                            None))
                |> List.minBy fst
                |> snd
            portPair
        

        /// Chooses other Symbol to align a port based on the condition that they are opposite parallel edges
        /// and will choose the symbol with the smallest euclidean distance to help avoid symbol overlaps
        let chooseSymAlign (port: Port) (otherPorts: (string * Port) list) (sheet:SheetT.Model): Symbol Option=
            if List.length otherPorts >1 then
                let wModel = sheet.Wire
                let portOrientation = getSymbolPortOrientation (getSym sheet port) port
                
                let filteredAndMappedPorts =
                    otherPorts
                    |> List.choose (fun (_, otherPort) ->
                        let otherPortOrientation = getSymbolPortOrientation (getSym sheet otherPort) otherPort
                        if portOrientation = otherPortOrientation.Opposite then //check if the other component port are on opposite edges
                            let offsetXY = getPortOffset sheet port otherPort
                            let offset =
                                match portOrientation with
                                | Top | Bottom -> offsetXY.X
                                | Left | Right -> offsetXY.Y
                            Some (offset, otherPort)
                        else
                            None) 
                match filteredAndMappedPorts with
                | [] -> None // No matching ports
                | _ ->
                    filteredAndMappedPorts
                    |> List.minBy fst //Choose the port with the smallest offset (X Y Is chosen depending on orientation)
                    |> snd
                    |> (fun sym -> Some (getSym sheet sym))
            else
                let otherSym =
                    otherPorts
                    |> List.tryHead
                    |> Option.map (fun (_,port) -> getSym sheet port)
                otherSym
    
        
        /// If SymA and SymB have multiple Parallel lines connected, Will resize either SymA or SymB to align ports
        /// This will only work for custom components
        let scaleMultiplyComp (wModel: BusWireT.Model) (syma: Symbol) (edgea:Edge) (symb: Symbol) (edgeb:Edge) : BusWireT.Model =
            // Determine scaleSym and otherSym based on port counts
            let scaleSym, otherSym =
                if (getPortCount syma edgea < getPortCount symb edgeb) then symb, syma else syma, symb
            let scaleSym' = reSizeSymbol wModel scaleSym otherSym
            let wModel' = Optic.set (symbolOf_ scaleSym.Id) scaleSym' wModel
            BusWireSeparate.routeAndSeparateSymbolWires wModel' scaleSym.Id

        let alignMultiplyComp (sheet: SheetT.Model) (syms: Symbol list): BusWireT.Model =
            syms
            |> List.fold (fun wModel' sym ->
                // Assuming getPortSym returns Map<string,Port> and we only look at output ports
                let outputPorts = getPortSym wModel'.Symbol sym
                                 |> Map.filter (fun _ port -> port.PortType = PortType.Output)
                                 |> Map.values
                                 |> Seq.toList
                match outputPorts with
                | [] -> wModel' // If there are no output ports, return the sheet as is
                | _ -> 
                    let portPair = choosePortAlign sheet outputPorts
                    let otherPort = snd portPair
                    let otherSym = getSym sheet (snd portPair)
                    alignSymbols wModel' sym otherSym
             ) sheet.Wire
               

    module Beautify =
        open D1Helpers
    
        let alignSinglyComp (sheet: SheetT.Model): SheetT.Model =
            let singlyComp = getSinglyComp sheet
            let connections = extractConnections sheet.Wire   
            singlyComp
            |> List.fold (fun sheet sym ->
                let portsMap = getPortSym sheet.Wire.Symbol sym
                // Since it's a singly component, assume there is only one port
                let portOption = portsMap |> Map.values |> Seq.tryHead 
                match portOption with
                | Some port ->
                    let otherPorts = findOtherPorts port connections 
                    let otherSymOpt = chooseSymAlign port otherPorts sheet
                    match otherSymOpt with
                    | Some otherSym ->
                        let wModel' = alignSymbols sheet.Wire sym otherSym
                        let sheet' = Optic.set SheetT.wire_ wModel' sheet
                        sheet'
                    | None -> sheet
                | _ -> sheet // If no port is found, proceed to the next symbol
            ) sheet
    
        /// Handles scaling and aligning for non-singly components
        let alignScaleComp (sheet: SheetT.Model): BusWireT.Model =
            let multiplySyms = getMultiplyComp sheet
            let connections = extractConnections sheet.Wire
            let alignSyms, customSyms = List.partition (fun sym -> sym.Component.Type = Custom) multiplySyms
            let sheetScale =
                customSyms List.fold (wModel sym ->
                                        let wModel' = scaleMultiplyComp wModel
                                        
            


    
    
        let sheetAlignScale (sheet: SheetT.Model): SheetT.Model =
            let multiplyModel = alignScaleComp sheet
            let multiplySheet = Optic.set SheetT.wire_ multiplyModel sheet
            let singlyModel = alignSinglyComp multiplySheet
            let sheet' = Optic.set SheetT.wire_ singlyModel multiplySheet
            sheet'
