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
module Helpers =
    // SYMBOL/CUSTOM COMPONENT HELPER FUNCTIONS ----------------------------------------------------------------------

    // Given D1 we are scaling custom components, we must have r/w to the dimensions

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

    /// returns the ports for a given symbol
    let getPortSym (model: SymbolT.Model) (sym: Symbol): Map<string,Port>  =
        let portId = sym ^. (portMaps_ >-> orientation_) |> Map.keys
        portId
        |> Seq.fold (fun acc id -> 
            match Map.tryFind id model.Ports with
            | Some port -> Map.add id port acc
            | None -> acc) Map.empty

    /// Finds the corresponding other port(s) for a given port based on connections.
    /// `port` is the given port to find matches for.
    /// `connections` is the list of all connections to search through.
    let findOtherPorts (port: Port) (connections: Connection list): Port list =
        connections |> List.collect (fun connection ->
            match connection with
            | { Source = src; Target = tgt } when src = port -> [tgt] // Port is the source; collect the target
            | { Source = src; Target = tgt } when tgt = port -> [src] // Port is the target; collect the source
            | _ -> [] // No match, proceed to the next connection
        )

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
                let otherPort = findOtherPorts port connections |> List.head
                let otherSymId = ComponentId otherPort.HostId
                let otherSym = sheet.Wire.Symbol.Symbols[otherSymId]
                let wModel' = alignSymbols sheet.Wire sym otherSym
                let sheet' = Optic.set SheetT.wire_ wModel' sheet
                sheet'
            | _ -> sheet // If no port is found, proceed to the next symbol
        ) sheet
        

       
    let inline getOrientationOfEdge (edge: Edge) = 
     match edge with
     | CommonTypes.Top | CommonTypes.Bottom -> Vertical
     | CommonTypes.Left | CommonTypes.Right -> Horizontal



    /// Filters Ports by Symbol.
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
        |> List.rev // Reverse the list to maintain original order
    
    /// Update BusWire model with given wires. Can also be used to add new wires.
    let updateModelWires (model: BusWireT.Model) (wiresToAdd: Wire list) : BusWireT.Model =
        model
        |> Optic.map wires_ (fun wireMap ->
            (wireMap, wiresToAdd)
            ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))

    

module Beautify =
    // this will be exremely useful in aligning same-type components. This function may need some editing
    /// Attempts to align two symbols together  
    let alignSymbols
        (wModel: BusWireT.Model)
        (symbolToSize: Symbol)
        (otherSymbol: Symbol)
        : BusWireT.Model =
    
        match RotateScale.getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
        | None -> wModel
        | Some(movePortInfo, otherPortInfo) ->
            let offset = RotateScale.alignPortsOffset movePortInfo otherPortInfo
            let symbol' = moveSymbol offset symbolToSize
            let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
            BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id
