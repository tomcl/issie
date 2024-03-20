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
        // SYMBOL/CUSTOM COMPONENT HELPER FUNCTIONS ----------------------------------------------------------------------
    
        /// Returns the offset of two ports relative to portA
        let getPortOffset (sModel: SymbolT.Model) (portA: Port) (portB:Port): XYPos =
            {
                X = (getPortLocation None sModel portB.Id).X - (getPortLocation None sModel portA.Id).X;
                Y = (getPortLocation None sModel portB.Id).Y - (getPortLocation None sModel portA.Id).Y
            }

        /// get list of all Port Positions - This can help in testing with 
        let getAllPortPos (model: SymbolT.Model) =
            model.Ports
            |> Map.toList
            |> List.map fst
            |> List.map (fun portId -> getPortPos portId model)
    
        //This helper may be useful when aligning same-type components
        /// returns all the Symbols in a sheet grouped by Component Type
        let getSameTypeSymbol (sheet: SheetT.Model) =
            let allSymbols = sheet.Wire.Symbol.Symbols |> Map.toList |> List.map snd
            let compGroups = 
                allSymbols
                |> List.groupBy (fun symbol -> symbol.Component.Type)
            compGroups
    
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


        let getPortCount (sym: Symbol) (edge: Edge): int =
            sym 
            |> Optic.get portMaps_
            |> (fun pm -> pm.Order)
            |> (fun orderMap -> Map.find edge orderMap)
            |> List.length


        /// Chooses other Symbol to align a port based on the condition that they are opposite parallel edges
        /// and will choose the symbol with the smallest euclidean distance to help avoid symbol overlaps
        let chooseSymAlign (port: Port) (otherPorts: (string * Port) list) (sheet:SheetT.Model): Symbol Option=
            if List.length otherPorts >1 then
                let wModel = sheet.Wire
                let portOrientation = getSymbolPortOrientation (getSym sheet port) port
                
                let filteredAndMappedPorts =
                    otherPorts
                    |> List.map (fun (_, otherPort) ->
                        let otherPortOrientation = getSymbolPortOrientation (getSym sheet otherPort) otherPort
                        if portOrientation = otherPortOrientation.Opposite then //check if the other component port are on opposite edges
                            let offsetXY = getPortOffset sheet.Wire.Symbol port otherPort
                            let offset =
                                match portOrientation with
                                | Top | Bottom -> offsetXY.X
                                | Left | Right -> offsetXY.Y
                            Some (offset, otherPort)
                        else
                            None)
                    |> List.choose id // Remove None values and unwrap Some values
    
                match filteredAndMappedPorts with
                | [] -> None // No matching ports
                | _ ->
                    filteredAndMappedPorts
                    |> List.minBy fst //Choose the port with the smallest offset (X Y Is chosen depending on orientation)
                    |> snd
                    |> fun closestPort ->
                        closestPort.HostId
                        |> ComponentId
                        |> fun id -> Some (sheet.Wire.Symbol.Symbols[id])
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
    
        let alignMultiplyComp (wModel: BusWireT.Model) (syma: Symbol) (symb: Symbol): BusWireT.Model =
            wModel

    
        




    (*    /// Filters Ports by Symbol.
    /// HLP23: AUTHOR dgs119
    let filterPortBySym (ports: Port list) (sym: Symbol) =
        ports |> List.filter (fun port -> ComponentId port.HostId = sym.Id)
    
        // WIRE HELPER FUNCTIONS ------------------------------------------------------------------------------------------------
    
        // from Derek Lai's code. These helpers will be used to detect segment crossings
        /// Returns true if two 1D line segments intersect
        let overlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
            let a_min, a_max = min a1 a2, max a1 a2
            let b_min, b_max = min b1 b2, max b1 b2
            a_max >= b_min && b_max >= a_min
        /// Returns true if two Boxes intersect, where each box is passed in as top right and bottom left XYPos tuples
        let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
            (overlap1D (a1.X, a2.X) (b1.X, b2.X)) && (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))
    
        // May be useful when handingling wire/segment intersections
        /// Returns a list of all the wires in the given model 
        let getWireList (model: Model) =
            model.Wires
            |> Map.toList
            |> List.map snd
    
        // The XYPos of segments will be a useful helper for tracking segment locations on the sheet 
        /// Convert a wire and its segment displacement into actual segment start and end positions
        let getSegmentPositions (sheet:SheetT.Model) wire =
            let startPos = wire.StartPos
            SegmentHelpers.visibleSegments wire.WId sheet
            |> List.fold (fun (acc, lastPos) seg ->
                let newPos = lastPos + seg
                ((lastPos, newPos) :: acc, newPos) // Prepend to list for efficiency
            ) ([], startPos)
            |> fst
            |> List.rev // Reverse the list to maintain original order*)
    

    

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
    
        (*/// Handles scaling and aligning for non-singly components
        let alignScaleComp (sheet: SheetT.Model): SheetT.Model =
            let multiplyComp = getMultiplyComp sheet
            let connections = extractConnections sheet.Wire*)
    
    
        let sheetAlignScale (sheet: SheetT.Model): SheetT.Model =
        // let sheetMultiply = alignScaleComp sheet
            let sheetSingly = alignSinglyComp sheet
            sheetSingly
