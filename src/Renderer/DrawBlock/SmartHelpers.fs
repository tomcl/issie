
module SmartHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

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
let updateModelSymbols 
    (model: BusWireT.Model) 
    (symbols: Symbol list)
        : BusWireT.Model =
    // HLP23: note on fold implementation. symMap is both argument and result of the
    // fold function => sequential set of updates. In thsi case much more efficient than Map.map
    // over all symbols.
    // HLP23 - see also similar updateModelWires
    let symbols' =
        (model.Symbol.Symbols,symbols)
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
let getPortXY (symbol: Symbol) (portId: PortId): XYPos =
    let H = symbol.Component.H
    let W = symbol.Component.W
    let portMap = symbol.PortMaps
    let index = List.findIndex (fun x -> x = portId) 
    let symbolXY = symbol.Pos
    let xyPos = { X = 9.0; Y = 9.0 }
    xyPos


let updateModelWires 
    (model: BusWireT.Model) 
    (wiresToAdd: Wire list)
        : BusWireT.Model =
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
    |> Optic.map wires_ (fun wireMap  ->
        (wireMap,wiresToAdd)
        ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))


/// This function returns (XYPos * XYPos) list corresponding to the Segment list 
/// The list of tupled XYPos coordinate, correspond to each segment in the segment list
/// HLP23: AUTHOR Rahimi
let getXYPosPairsOfSegments (segments: list<Segment>) (startPos: XYPos) (initialOrientation: Orientation) = 
        ([], segments)
        ||> List.fold (fun xyPosPairs segment -> 
            match xyPosPairs, (initialOrientation), (segment.Index % 2 = 0) with
                |[], Horizontal, true -> xyPosPairs@[(startPos), ((+) (startPos) {X=segment.Length; Y=0.0})]
                |[], Vertical, true -> xyPosPairs@[(startPos), ((+) (startPos) {X=0.0; Y=segment.Length})]
                |_ , Horizontal, true -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=segment.Length; Y=0.0})]
                |_ , Vertical, true -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=0.0; Y=segment.Length})]
                |_ , Vertical, false -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=segment.Length; Y=0.0})]
                |_ , Horizontal, false -> xyPosPairs@[snd(xyPosPairs[xyPosPairs.Length-1]), 
                                        ((+) (snd(xyPosPairs[xyPosPairs.Length-1])) {X=0.0; Y=segment.Length})])

/// This function returns the minimum and maximum distance of Bounding Box (BB) from the XYPos tuple
/// The minimum distance is typically leftmost and top-most distance of BB from vertical and horizontal segment respectively
/// and the maximum distance is typically rightmost and bottom-most distance of BB from vertical and horizontal segment respectively.
/// The segment correspond to the segment that generate the XYPos tuple, but this can also be used to check the BB exists in some
/// XYPos tuple coordinates.
/// HLP23: AUTHOR Rahimi
let getMinMaxDistOfBBfromXYPosPair (model:Model) (xyPosPair: XYPos * XYPos) =
    let allBoundingBoxes: Map<ComponentId,BoundingBox> = Symbol.getBoundingBoxes model.Symbol
    let allLabelBoundingBoxes = Symbol.getLabelBoundingBoxes model.Symbol

    let xyPosPairOrientation = 
        match xyPosPair with
        |(A, B) when A.X = B.X -> Vertical
        |_,_ -> Horizontal
    
    let xyPosPairXmin =
        match xyPosPair with
        |(A, B) when A.X <= B.X -> A.X
        |(A, B) -> B.X

    let xyPosPairXmax =
        match xyPosPair with
        |(A, B) when A.X <= B.X -> B.X
        |(A, B) -> A.X

    let xyPosPairYmin =
        match xyPosPair with
        |(A, B) when A.Y <= B.Y -> A.Y
        |(A, B) -> B.Y

    let xyPosPairYmax =
        match xyPosPair with
        |(A, B) when A.Y <= B.Y -> B.Y
        |(A, B) -> A.Y

    let allBBList =
        allBoundingBoxes
        |> Map.filter (match xyPosPairOrientation with
                        |Horizontal -> fun key bb -> 
                                        (bb.TopLeft.X < xyPosPairXmax) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin) &&
                                        (bb.TopLeft.Y < xyPosPairYmin) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin)
                        | Vertical -> fun key bb -> 
                                        (bb.TopLeft.Y < xyPosPairYmax) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin) &&
                                        (bb.TopLeft.X < xyPosPairXmin) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin))
        |> Map.toList
    
    let allLabelBBList = 
        allLabelBoundingBoxes
        |> Map.filter (match xyPosPairOrientation with
                        |Horizontal -> fun key bb -> 
                                        (bb.TopLeft.X < xyPosPairXmax) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin) &&
                                        (bb.TopLeft.Y < xyPosPairYmin) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin)
                        | Vertical -> fun key bb -> 
                                        (bb.TopLeft.Y < xyPosPairYmax) && ((bb.TopLeft.Y + bb.H) > xyPosPairYmin) &&
                                        (bb.TopLeft.X < xyPosPairXmin) && ((bb.TopLeft.X + bb.W) > xyPosPairXmin))
        |> Map.toList
    
    (allBBList @ allLabelBBList)
    |> List.map (fun (a,b) -> b)
    |> List.fold (match xyPosPairOrientation with
                    |Horizontal -> (fun state bb -> 
                                    match state with 
                                    |A, B -> ((min A (bb.TopLeft.Y - xyPosPairYmin)), (max B (bb.TopLeft.Y + bb.H - xyPosPairYmin))))
                    |Vertical  -> (fun state bb -> 
                                    match state with 
                                    |A, B -> ((min A (bb.TopLeft.X - xyPosPairXmin)), (max B (bb.TopLeft.X + bb.W - xyPosPairXmin)))))
                    (0.0,0.0)
                    