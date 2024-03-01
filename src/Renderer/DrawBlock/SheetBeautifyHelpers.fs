module SheetBeautifyHelpers

open Optics

open CommonTypes
open DrawModelType
open BusWireT

open Symbol

//-----------------------------------------------------------------------------------------------
// visibleSegments is included here as ahelper for info, and because it is needed in project work
//-----------------------------------------------------------------------------------------------
let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

    let wire = model.Wire.Wires[wId] // get wire from model

    /// helper to match even and off integers in patterns (active pattern)
    let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

    /// Convert seg into its XY Vector (from start to end of segment).
    /// index must be the index of seg in its containing wire.
    let getSegmentVector (index:int) (seg: BusWireT.Segment) =
        // The implicit horizontal or vertical direction  of a segment is determined by 
        // its index in the list of wire segments and the wire initial direction
        match index, wire.InitialOrientation with
        | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
        | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

    /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// if this is possible, otherwise return segVecs unchanged.
    /// Index must be in range 1..segVecs
    let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
        if index < segVecs.Length-1 && segVecs[index] =~ XYPos.zero // fix suggested by Timothy Chung
        then
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
        else
            segVecs

    wire.Segments
    |> List.mapi getSegmentVector
    |> (fun segVecs ->
            (segVecs,[1..segVecs.Length-2])
            ||> List.fold tryCoalesceAboutIndex)

// Could use XYPos instead of a new type
// However it wouldn't be as clear which is the width or height
type Dimension = {
    W: float
    H: float
}

// B1R, B1W RW Low
/// <summary>Return the dimensions of a custom component symbol after applying scaling</summary>
let getCCSymbolDimension (sym : SymbolT.Symbol) =
    match (sym.HScale, sym.VScale) with
    | (None, None) -> { W = sym.Component.W ; H = sym.Component.H }
    | (Some hScale, Some vScale) -> { W = sym.Component.W * hScale ; H = sym.Component.H * vScale }
    | (Some hScale, None) -> { W = sym.Component.W * hScale ; H = sym.Component.H }
    | (None, Some vScale) -> { W = sym.Component.W ; H = sym.Component.H * vScale }

/// <summary>Return a custom component symbol with updated scaled dimensions</summary>
let setCCSymbolDimension (dim : Dimension) (sym : SymbolT.Symbol) =
    { sym with HScale = Some (dim.W/sym.Component.W); VScale = Some (dim.H/sym.Component.H) }
    // NB: keep the W and H of the component the same, as this is calculated when creating the custom component
    // if the user performs an illegal sizing, we can revert back to this original size

let CCSymbolDimensions_ = Lens.create getCCSymbolDimension setCCSymbolDimension

// B2W Med
/// <summary>Update the position of a symbol on the sheet</summary>
let setSymbolPosOnSheet (sheetModel : SheetT.Model) (symId : ComponentId) (pos : XYPos) : SheetT.Model = 
    let updateSymPos (sym : SymbolT.Symbol) : SymbolT.Symbol = { sym with Pos = pos }
    let newSymModel: SymbolT.Model = SymbolUpdate.updateSymbol updateSymPos symId sheetModel.Wire.Symbol

    sheetModel
    |> Optic.set SheetT.symbol_ newSymModel

// B3R, B3W RW Med
/// <summary>Return the order of ports on a specified edge of a symbol</summary>
let getSymPortOrder (sym : SymbolT.Symbol) (side : Edge) : string list =
    sym.PortMaps.Order[side]

/// <summary>Set the order of ports on a specified edge of a symbol</summary>
let setSymPortOrder (orderedPorts : string list) (side : Edge) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    let newPortOrder = sym.PortMaps.Order |> Map.add side orderedPorts

    { sym with PortMaps = { sym.PortMaps with Order = newPortOrder } }

// NB: A lens is not directly possible as we also need to specify the specific side
// let symPortOrder_ = Lens.create getSymPortOrder setSymPortOrder // this won't work

// B4R, B4W RW Low 
/// <summary>Get the reverse state of the inputs of a MUX2</summary>
let getMux2IsReversed (sym : SymbolT.Symbol) : bool option =
    sym.ReversedInputPorts

/// <summary>Set the reverse state of the inputs of a MUX2</summary>
let setMux2IsReversed (reverseState : bool option) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with ReversedInputPorts = reverseState }

let mux2IsReversed_ = Lens.create getMux2IsReversed setMux2IsReversed

// B5R R Low 
/// <summary>Returns the position of a specified port on a symbol.</summary>
let getPortPosOnSheet (sym : SymbolT.Symbol) (portId : PortId) : XYPos =
    let port = sym.Component.getPort portId
    match port with
    | Some port -> getPortPos sym port
    | None -> failwithf "Port with id %A not found in symbol" portId
    
// B6R R Low 
/// <summary>Get the Bounding box of a symbol outline</summary>
let getSymOutlineBoundingBox (sym : SymbolT.Symbol) : BoundingBox =
    getSymbolBoundingBox sym

// B7R, B7W RW Low 
/// <summary>Get the rotation state of a symbol</summary>
let getSymRotation (sym : SymbolT.Symbol) : Rotation =
    sym.STransform.Rotation

/// <summary>Get the rotation state of a symbol</summary>
let setSymRotation (rotation : Rotation) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Rotation = rotation } }

let symRotation_ = Lens.create getSymRotation setSymRotation

// B8R, B8W RW Low 
/// <summary>Get the flip state of a symbol</summary>
let isSymFlip (sym : SymbolT.Symbol) : bool =
    sym.STransform.Flipped

/// <summary>Set the flip state of a symbol</summary>
let setSymFlip (isFlipped : bool) (sym : SymbolT.Symbol) : SymbolT.Symbol =
    { sym with STransform = { sym.STransform with Flipped = isFlipped } }

let symFlip_ = Lens.create isSymFlip setSymFlip

/// T1R R Low 
/// <summary>Count the total number of distinct symbol pairs that intersect each other</summary>
let countSymIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire
    let boxes =
        Map.toList sheetModel.BoundingBoxes
        |> List.mapi (fun n box -> n,box)
    
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox (snd box1) (snd box2))
    |> List.length
    

/// T2R R Low 
/// <summary>The total number of distinct visible wire segments that intersect with one or more symbols. </summary>
let countSegIntersectSym (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire

    wireModel.Wires
    |> Map.filter (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
    |> Map.count

// ------------ Helpers for T3R & T4R ------------
/// helper to get all unique combinations of elements in a list
let rec allPairsWithoutRepeats list =
    match list with
    | [] -> []
    | hd::tl -> (List.map (fun elm -> (hd, elm)) tl) @ allPairsWithoutRepeats tl

/// helper to get the input port of a segment from the sheet model
let getInputPortOfSeg (seg : BusWireT.Segment) (sheetModel : SheetT.Model) =
    let wireMap : Map<ConnectionId,Wire> = sheetModel.Wire.Wires
    wireMap[seg.WireId].OutputPort

/// helper to check if two segments are from the same net in the sheet model
let isSegFromSameNet (seg1 : BusWireT.Segment) (seg2 : BusWireT.Segment) (sheetModel : SheetT.Model) =
    getInputPortOfSeg seg1 sheetModel = getInputPortOfSeg seg2 sheetModel

/// helper to calculate the overlap length of two segments
let calcASegOverlapLength (seg1: BusWireT.ASegment) (seg2: BusWireT.ASegment) = 
    let min1, min2 = min seg1.Start seg1.End, min seg2.Start seg2.End
    let max1, max2 = max seg1.Start seg1.End, max seg2.Start seg2.End
    
    let vector = min max1 max2 - max min1 min2
    max (abs vector.X) (abs vector.Y) // assume that the segments are parallel, either X or Y will be cancelled out

// -----------------------------------------------

// T3R R Low 
/// <summary>The number of distinct pairs of segments that cross each other at right angles. Does
/// not include 0 length segments or segments on same net intersecting at one end.</summary>
let countSegmentIntersections (sheetModel : SheetT.Model) =
    let wires: Map<ConnectionId,Wire> = sheetModel.Wire.Wires 
    let ASegments = 
        wires
        |> Map.toList
        |> List.map (fun (id, wire) -> wire)
        |> List.collect BlockHelpers.getNonZeroAbsSegments

    let intersectFilter (seg1 : BusWireT.ASegment) (seg2 : BusWireT.ASegment) =
        // we require segments to be on different nets, be orthogonal to each other and overlap
        not (isSegFromSameNet seg1.Segment seg2.Segment sheetModel)
        && (seg1.Orientation <> seg2.Orientation) 
        && BlockHelpers.overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
    
    ASegments
    |> allPairsWithoutRepeats
    |> List.filter (fun (seg1, seg2) -> intersectFilter seg1 seg2)
    |> List.length

// T4R R Medium 
/// <summary>Return the sum of all visible wiring segments length.</summary>
let calcVisibleWiringLength (sheetModel : SheetT.Model) : float =
    // Summary:
    // Visible Length = (1) + (2)
    // (1) Sum of segment length without any overlap
    // (2) Sum of the maximum overlap length of segments in the N same net
    // Approach for getting (2):
    // Find all segment pairs that overlap
    // They MUST have either start or end points at the same position
    // Group the pairs by their shared point (start/end) => same N net pairs
    // Calculate the max overlap length for all the pairs in each group
    // Sum the above
    let wires: Map<ConnectionId,Wire> = sheetModel.Wire.Wires 
    let ASegments = 
        wires
        |> Map.toList
        |> List.map snd
        |> List.collect BlockHelpers.getNonZeroAbsSegments

    // we require segments to be parallel to each other and overlap 
    let overlapPairs = 
        let overlapFilter (seg1 : BusWireT.ASegment) (seg2 : BusWireT.ASegment) =
            (seg1.Orientation = seg2.Orientation) 
            && BlockHelpers.overlap2D (seg1.Start, seg1.End) (seg2.Start, seg2.End)
        ASegments
        |> allPairsWithoutRepeats
        |> List.filter (fun (seg1, seg2) -> overlapFilter seg1 seg2)

    let segWithoutOverlapLength =
        // identify all segments that overlap
        let allSegsWithOverlap =
            overlapPairs
            |> List.unzip
            |> (fun (seg1, seg2) -> seg1 @ seg2)
            |> List.distinct

        ASegments
        |> List.except allSegsWithOverlap
        |> List.map (fun seg -> abs seg.Segment.Length)
        |> List.sum
    
    let segInSameNetOverlapLength =
        let findSharedPoint (seg1: BusWireT.ASegment, seg2: BusWireT.ASegment) = 
            // seg1.Start // NB: assume start and end points of ASegments are guaranteed to be in order
            // NB: However, use this if the start and end points are not guaranteed to be in order
            if seg1.Start = seg2.Start then seg1.Start 
            elif seg1.Start = seg2.End then seg1.Start
            elif seg1.End = seg2.Start then seg1.End
            elif seg1.End = seg2.End then seg1.End
            else {X=0.; Y=0.}
            // failwithf "No shared overlap starting point found."
        overlapPairs
        |> List.filter (fun (seg1, seg2) -> isSegFromSameNet seg1.Segment seg2.Segment sheetModel)
        |> List.groupBy findSharedPoint // group by the shared point of segments
        |> List.map (fun (netId, segs) -> 
            segs
            |> List.map (fun (seg1, seg2) -> calcASegOverlapLength seg1 seg2)
            |> List.max // calculate the maximum overlap length for each group
            ) 
        |> List.sum
    printfn "(noOverlapLength, overlapLength) = %A, %A" segWithoutOverlapLength segInSameNetOverlapLength
    segWithoutOverlapLength + segInSameNetOverlapLength

// T5R R Low 
/// <summary>The number of right angles formed by visible segments. </summary>
let countWireRightAngles (sheetModel : SheetT.Model) : int =
    let wireModel = sheetModel.Wire

    wireModel.Wires
    |> Map.toList
    |> List.map (fun (wid, wire) -> visibleSegments wid sheetModel)
    |> List.fold (fun acc segs -> acc+segs.Length-1) 0 // a single segment has no right angles

// T6R R High 
/// <summary> Return two lists of retracing segments. The first include all retracing segments. 
/// The second include only segments near the ports that cause intersection with symbol.</summary>
let getRetracingSegment (sheetModel : SheetT.Model) =
    let wires = sheetModel.Wire.Wires
    let segments = 
        wires
        |> Map.toList
        |> List.map (fun (wid, wire) -> wire.Segments)

    let getSeg (wid: ConnectionId) (sid: int) : Segment Option =
        let folder acc seg =
            if seg.Index = sid 
            then Some seg
            else acc
        List.fold folder None wires[wid].Segments
    
    let getRetracingNeighbours (seg: Segment) : Segment list =
        let prevOpt = getSeg seg.WireId (seg.Index-1)
        let nextOpt = getSeg seg.WireId (seg.Index+1)
        match prevOpt, nextOpt with
        | Some (prevSeg: Segment), Some (nextSeg: Segment) -> 
            if prevSeg.Length * nextSeg.Length < 0.0 // opposite signs
            then [nextSeg]
            else []
        | _ -> []

    let allRetracingSegments =
        segments
        |> List.concat
        |> List.filter (fun (seg: Segment) -> seg.IsZero)
        |> List.collect getRetracingNeighbours

    let endOfWireRetracingSegments =
        allRetracingSegments
        |> List.filter (fun (seg: Segment) -> seg.Index = 3 || seg.Index = wires[seg.WireId].Segments.Length - 5)
        |> List.map (fun seg -> BusWire.getASegmentFromId sheetModel.Wire seg.GetId)
        |> List.filter (
            fun aseg ->
                sheetModel.BoundingBoxes 
                |> Map.toList
                |> List.map snd
                |> List.exists(
                    fun box -> 
                        match BlockHelpers.segmentIntersectsBoundingBox box aseg.Start aseg.End with
                        | Some _ -> true
                        | None -> false
                )
        )

    allRetracingSegments, endOfWireRetracingSegments