module SmartHelpers

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open BusWire
open BusWireUpdateHelpers
open SymbolHelpers

open Optics
open Operators

//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//

(*
HOW TO USE THIS MODULE.

(1) Add well-documented useful functions - see updateModelSymbols and updateModelWires
    for examples. You do not need to add performance information as in updateModelSymbols. 
    Your priority should be writing clear code. Try to avoid very inefficient implementations
    if possible (e.g. 100X slower than a similar complexity solution), but do not worry 
    about this.
(2) Note from my examples distinction between XML documentation and additional details
    in header comments.
(3) HLP23: Note comments here labelled "HLP23" which are for HLP23 class and would be deleted in
    production (Group phase) code.
(2) HLP23: Each function must have a single author specified by "HLP23: AUTHOR" in an XML comment
    as in my example: give name as Family name only (unique within teams).
(3) HLP23: Inform other members that you have written a function they could maybe use.
(4) HLP23: If two people end up with near-identical functions here team phase can rationalise if
    needed normally you are expected to share when this makes code writing faster.
(5) Note best practice here using Optics for nested record update. This is NOT curently required
    in Issie but used appropriately results in better code. Use it if you are comfortable doing so.
(5) Note on qualifying types: do this when not doing it would be ambiguous - e.g. here
    the BusWire and Symbol Model types.
(6) Note on code layout. A limit of 100 characters per line is used here. Seems about right.
*)

//----------------------------------------------------------------------------------------------//

/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
//  Performance scales as O(M*log2(N)) - M = number of symbols added, N = number of existing
//  Symbols. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelSymbols (model: BusWireT.Model) (symbols: Symbol list) : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols, symbols)
        ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap)

    Optic.set (symbol_ >-> symbols_) symbols' model

/// Update BusWire model with given wires. Can also be used to add new wires.
/// This uses a fold on the Map to add wires which makes it fast in the case that the number
/// of wires added is small.
//  Performance scales as O(M*log2(N)) - M = number of wires added, N = number of existing
//  wires. Changing large maps is relatively expensive hence the care here.
//  This function uses best practice for nested record update with Optics. See Wiki for
//  details. Note that Optics are probably a little bit slower than F# record update (factor of 2)
//  however in this case it does not matter because the time taken is << the Map update time.
/// HLP23: AUTHOR Clarke
let updateModelWires (model: BusWireT.Model) (wiresToAdd: Wire list) : BusWireT.Model =
    //
    // HLP23: note on fold implementation. In this (typical) example Map is
    // sequentially updated by the fold. A common and difficult to see coding mistake is to use the
    // original wireMap (argument of Optic map function) not the updated one (wireMap argument of
    // List.map folder) in the fold function! That is not possible here because both have the same
    // name so the inner bound updated wireMap is always what is used in the folder function.
    // This is good practice, and if you have ever debugged this type of mistake you will know it
    // is very necessary!

    // HLP23: note on this use of Optics.map in a pipeline. It is more "functional" than the
    // equivalent implementation using a let definition and Optics.set. Is it clearer? Or less clear?
    // Standard logic says we should prefer the pipeline if the name of the let definition adds
    // nothing which is the case here. I have given you both ways of using Optics here so you can
    // compare the two implementations and decide. NB - you are NOT required to use Optics in your
    // own code.
    //
    // HLP23: Note how multiple updates to different parts of the model can be neatly pipelined
    // like this using a separate Optic.map or Optic.set for each.
    //
    // HLP23: note that if the operation here was larger or part of some pipeline the
    // 2nd argument to Optic.map - which defines the model change - could be given a name and
    // turned into a local function making the Optic.map line like:
    // |> Optic.map wires_ myNameForThisWireMapUpdate
    model
    |> Optic.map wires_ (fun wireMap ->
        (wireMap, wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))


/// Returns true if two 1D line segments intersect
/// HLP23: Derek Lai (ddl20)
let overlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
    let a_min, a_max = min a1 a2, max a1 a2
    let b_min, b_max = min b1 b2, max b1 b2
    a_max >= b_min && b_max >= a_min

/// Returns true if two Boxes intersect, where each box is passed in as top right and bottom left XYPos tuples
/// HLP23: Derek Lai (ddl20)
let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
    (overlap1D (a1.X, a2.X) (b1.X, b2.X)) && (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

/// Returns true if two Boxes intersect, where each box is passed in as a BoundingBox
/// HLP23: Derek Lai (ddl20)
let overlap2DBox (bb1: BoundingBox) (bb2: BoundingBox) : bool =
    let bb1Coords =
        { X = bb1.TopLeft.X; Y = bb1.TopLeft.Y },
        { X = bb1.TopLeft.X + bb1.W
          Y = bb1.TopLeft.Y + bb1.H }

    let bb2Coords =
        { X = bb2.TopLeft.X; Y = bb2.TopLeft.Y },
        { X = bb2.TopLeft.X + bb2.W
          Y = bb2.TopLeft.Y + bb2.H }

    overlap2D bb1Coords bb2Coords

/// Retrieves XYPos of every vertex in a wire
/// HLP23: Derek Lai (ddl20)
let getWireSegmentsXY (wire: Wire) =
    let tupToXY (l: (float * float)) : XYPos = { X = fst l; Y = snd l }

    segmentsToIssieVertices wire.Segments wire
    |> List.map (fun (x, y, _) -> (x, y))
    |> List.map tupToXY

/// Retrieves all wires which intersect an arbitrary bounding box
/// HLP23: Derek Lai (ddl20)
let getWiresInBox (box: BoundingBox) (model: Model) : Wire list =
    let wires = (List.ofSeq (Seq.cast model.Wires.Values))

    let bottomRight =
        { box.TopLeft with
            X = box.TopLeft.X + box.W
            Y = box.TopLeft.Y + box.H }

    let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool) (segment: Segment) : bool =
        state || overlap2D (startPos, endPos) (box.TopLeft, bottomRight)

    List.filter (foldOverNonZeroSegs checkOverlapFolder false) wires

/// Checks if a wire intersects any symbol or not.
/// Returns list of bounding boxes of symbols intersected by wire.
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let findWireSymbolIntersections (model: Model) (wire: Wire) : (ComponentId * BoundingBox) list =
    let allSymbolBoundingBoxes = Symbol.getBoundingBoxes model.Symbol

    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let segVertices = List.pairwise wireVertices[1 .. wireVertices.Length - 2] // do not consider the nubs

    let boxesIntersectedBySegment startPos endPos =
        allSymbolBoundingBoxes
        |> Map.filter (fun compID boundingBox ->
            match segmentIntersectsBoundingBox boundingBox startPos endPos with
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection
        )
        |> Map.toList

    segVertices
    |> List.collect (fun (startPos, endPos) -> boxesIntersectedBySegment startPos endPos)
    |> List.distinct

/// Get the start and end positions of a wire.
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let getStartAndEndWirePos (wire: Wire) : XYPos * XYPos =
    let wireVertices =
        segmentsToIssieVertices wire.Segments wire
        |> List.map (fun (x, y, _) -> { X = x; Y = y })

    let currentStartPos = wireVertices.Head
    let currentEndPos = wireVertices[wireVertices.Length - 2]

    currentStartPos, currentEndPos

/// Returns length of wire
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let getWireLength (wire: Wire) : float =
    (0., wire.Segments) ||> List.fold (fun acc seg -> acc + (abs seg.Length))

/// Checks if a wire is part of a net.
/// If yes, return the netlist. Otherwise, return None
/// HLP23: AUTHOR Jian Fu Eng (jfe20)
let isWireInNet (model: Model) (wire: Wire) : (OutputPortId * (ConnectionId * Wire) list) option =
    let nets = partitionWiresIntoNets model

    nets
    |> List.tryFind (fun (outputPortID, netlist) -> wire.OutputPort = outputPortID && netlist.Length > 1)

/// Checks if a port is part of a Symbol.
/// HLP23: AUTHOR dgs119
let isPortInSymbol (portId: string) (symbol: Symbol) : bool =
    symbol.PortMaps.Orientation |> Map.containsKey portId

/// Checks if wire is connected to two given symbols. Neglects self connections.
/// HLP23: AUTHOR dgs119
let isConnBtwnSyms (wire: Wire) (symbolA: Symbol) (symbolB: Symbol) : bool =
    let inId, outId =
        getInputPortIdStr wire.InputPort, getOutputPortIdStr wire.OutputPort

    match inId, outId with
    | inId, outId when (isPortInSymbol inId symbolA) && (isPortInSymbol outId symbolB) -> true
    | inId, outId when (isPortInSymbol inId symbolB) && (isPortInSymbol outId symbolA) -> true
    | _ -> false

/// Gets connections between symbols.
/// HLP23: AUTHOR dgs119
let getConnBtwnSyms (wModel: BusWireT.Model) (symbolA: Symbol) (symbolB: Symbol) : Map<ConnectionId, Wire> =
    wModel.Wires |> Map.filter (fun _ wire -> isConnBtwnSyms wire symbolA symbolB)

/// Gets Wires between symbols.
/// HLP23: AUTHOR dgs119
let getWireBtwnSyms (wModel: BusWireT.Model) (symbolA: Symbol) (symbolB: Symbol) : Wire list =
    getConnBtwnSyms wModel symbolA symbolB |> Map.toList |> List.map snd

/// Filters Ports by Symbol.
/// HLP23: AUTHOR dgs119
let fiterPortBySym (ports: Port list) (symbol: Symbol) =
    ports |> List.filter (fun port -> ComponentId port.HostId = symbol.Id)

/// Gets Ports From a List of Wires.
/// HLP23: AUTHOR dgs119
let getPortsFrmWires (model: BusWireT.Model) (wires: Wire list) =
    wires
    |> List.map (fun wire ->
        [ getPort model.Symbol (getInputPortIdStr wire.InputPort)
          getPort model.Symbol (getOutputPortIdStr wire.OutputPort) ])
    |> List.concat
    |> List.distinct

/// Groups Wires by their net.
/// HLP23: AUTHOR dgs119
let partitionWiresByNet (conns: Map<ConnectionId, Wire>) =
    conns
    |> Map.toList
    |> List.groupBy (fun (_, wire) -> wire.OutputPort)
    |> List.map (snd >> List.map snd)

/// Scales a symbol so it has the provided height and width
/// HLP23: AUTHOR BRYAN TAN
let setCustomCompHW (h: float) (w: float) (sym: Symbol) =
    let hScale = w / sym.Component.W
    let vScale = h / sym.Component.H

    { sym with
        HScale = Some hScale
        VScale = Some vScale }
