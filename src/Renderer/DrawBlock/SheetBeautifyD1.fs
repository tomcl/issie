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
        let getSinglyComp (wModel: BusWireT.Model): Symbol list =
            Optic.get symbols_ wModel.Symbol
            |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                match getComponentProperties sym.Component.Type sym.Component.Label with
                | (0, 1, _, _) -> sym :: acc
                | (1, 0,_,_) -> sym :: acc
                | _ -> acc) []
            |> List.rev
    
        /// returns all multiply connected components on a sheet
        let getMultiplyComp (wModel: BusWireT.Model): Symbol list =
            Optic.get symbols_ wModel.Symbol
            |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                let (numInputs, numOutputs, _, _) = getComponentProperties sym.Component.Type sym.Component.Label
                match (numInputs, numOutputs) with
                | (i, o) when i > 1 || o > 1 -> sym :: acc  // Components with more than one input or output
                | _ -> acc) []  // Ignore components with one or no inputs/outputs
            |> List.rev

        /// For a list of connecting Symbol Pairs, return the list of pairs where both symbols are custom Components
        let filterCustomComp (portPair: (Symbol * Symbol) list): ((Symbol * Symbol) list) =
            portPair
            |> List.filter (fun (syma, symb) -> 
                match syma.Component.Type, symb.Component.Type with
                | Custom _, Custom _ -> true
                | _ -> false)

    
        
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
        let getSym (wModel:BusWireT.Model) (port:Port): Symbol =
            let sym =
                port.HostId
                |> ComponentId
                |> fun id -> wModel.Symbol.Symbols[id]
            sym

        /// For a given symbol and edge, will return the number of ports on that edge
        let getPortCount (sym: Symbol) (edge: Edge): int =
            sym 
            |> Optic.get portMaps_
            |> (fun pm -> pm.Order)
            |> (fun orderMap -> Map.find edge orderMap)
            |> List.length

        /// Returns the offset of two ports relative to portA
        let getPortOffset (wModel:BusWireT.Model) (portA: Port) (portB:Port): XYPos =
            let symA = getSym wModel portA
            let symB = getSym wModel portB
            let offset =
                {
                    X = (calculatePortRealPos (makePortInfo symB portB)).X - (calculatePortRealPos (makePortInfo symA portA)).X;
                    Y = (calculatePortRealPos (makePortInfo symB portB)).Y - (calculatePortRealPos (makePortInfo symA portA)).Y
                }
            offset

        /// For Components with multiple output ports, will return the most suitable Port to align
        let choosePortAlign (wModel:BusWireT.Model) (ports: Port list): Port * Port =
            let connections = extractConnections wModel
            let portPair =
                ports
                |> List.collect (fun port -> 
                    let portOrientation = getSymbolPortOrientation (getSym wModel port) port
                    findOtherPorts port connections
                    |> List.choose (fun (_, otherPort) ->
                        let otherPortOrientation = getSymbolPortOrientation (getSym wModel otherPort) otherPort
                        if portOrientation = otherPortOrientation.Opposite then
                            let offsetXY = getPortOffset wModel port otherPort
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
        /// and will choose the symbol with the smallest offset distance to help avoid symbol overlaps
        let chooseSymAlign (port: Port) (otherPorts: (string * Port) list) (wModel:BusWireT.Model): Symbol Option=
            if List.length otherPorts >1 then
                let portOrientation = getSymbolPortOrientation (getSym wModel port) port
                
                let filteredAndMappedPorts =
                    otherPorts
                    |> List.choose (fun (_, otherPort) ->
                        let otherPortOrientation = getSymbolPortOrientation (getSym wModel otherPort) otherPort
                        if portOrientation = otherPortOrientation.Opposite then //check if the other component port are on opposite edges
                            let offsetXY = getPortOffset wModel port otherPort
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
                    |> (fun sym -> Some (getSym wModel sym))
            else
                let otherSym =
                    otherPorts
                    |> List.tryHead
                    |> Option.map (fun (_,port) -> getSym wModel port)
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

        let alignMultiplyComp (wModel: BusWireT.Model) (syms: Symbol list): BusWireT.Model =
            syms
            |> List.fold (fun wModel' sym ->
                let outputPorts = getPortSym wModel'.Symbol sym
                                 |> Map.filter (fun _ port -> port.PortType = PortType.Output)
                                 |> Map.values
                                 |> Seq.toList
                match outputPorts with
                | [] -> wModel' // If there are no output ports, return the sheet as is
                | _ -> 
                    let portPair = choosePortAlign wModel' outputPorts
                    let otherPort = snd portPair
                    let otherSym = getSym wModel' otherPort
                    alignSymbols wModel' sym otherSym
             ) wModel
               
               
    module Beautify =
        open D1Helpers

        let alignSinglyComp (wModel: BusWireT.Model): BusWireT.Model =
            let singlyComp = getSinglyComp wModel
            let connections = extractConnections wModel
            singlyComp
            |> List.fold (fun wModel' sym ->
                let portsMap = getPortSym wModel'.Symbol sym
                // Since it's a singly component, assume there is only one port
                let portOption = portsMap |> Map.values |> Seq.tryHead 
                match portOption with
                | Some port ->
                    let otherPorts = findOtherPorts port connections 
                    let otherSymOpt = chooseSymAlign port otherPorts wModel'
                    match otherSymOpt with
                    | Some otherSym ->
                        let wModel'' = alignSymbols wModel' sym otherSym
                        wModel''
                    | None -> wModel'
                | _ -> wModel' // If no port is found, proceed to the next symbol
            ) wModel
    

        let alignScaleComp (wModel: BusWireT.Model): BusWireT.Model =
            let multiplySyms = getMultiplyComp wModel
            let connPairs = getConnSyms wModel
            let custPairs = filterCustomComp connPairs
            let customSymbols = custPairs |> List.collect (fun (a, b) -> [a; b])
            let alignSyms = List.filter (fun sym -> not (List.contains sym customSymbols)) multiplySyms
            
            // Iterate over each pair in custPairs for processing
            let wModel' =
                List.fold (fun wModel' (symA, symB) -> 
                    match getOppEdgePortInfo wModel' symA symB with
                    | Some (portInfoA, portInfoB) ->
                        let custEdges = (portInfoA.side, portInfoB.side)
                        scaleMultiplyComp wModel' symA (fst custEdges) symB (snd custEdges)                   
                    | None -> wModel'
                ) wModel custPairs

            
            let wModel'' = alignMultiplyComp wModel' alignSyms
            // Return the final model after adjustments
            wModel''

        let sheetSingly (sheet: SheetT.Model): SheetT.Model =
            let singlyModel = alignSinglyComp sheet.Wire
            let sheet' = Optic.set SheetT.wire_ singlyModel sheet
            sheet'

        let sheetMultiply (sheet: SheetT.Model): SheetT.Model =
            let multiplyModel = alignScaleComp sheet.Wire
            let multiplySheet = Optic.set SheetT.wire_ multiplyModel sheet
            multiplySheet

        let sheetAlignScale (sheet: SheetT.Model): SheetT.Model =
           let multiplyModel = alignScaleComp sheet.Wire
           let multiplySheet = Optic.set SheetT.wire_ multiplyModel sheet
           let singlyModel = alignSinglyComp multiplySheet.Wire
           let sheet' = Optic.set SheetT.wire_ singlyModel multiplySheet
           sheet'  
