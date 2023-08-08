module BusWireRoutingHelpers

open Fable.Core
open CommonTypes
open DrawModelType.BusWireT
open Symbol
open BusWire
open BlockHelpers
open Optics
open Operators

//-----------------------------------------------------------------------------------------------//
//---------------------------HELPERS FOR SMART DRAW BLOCK ADDITIONS------------------------------//
//-----------------------------------------------------------------------------------------------//
(*
/// Update BusWire model with given symbols. Can also be used to add new symbols.
/// This uses a fold on the Map to add symbols which makes it fast in the case that the number
/// of symbols added is very small.
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
let updateModelWires (model: BusWireT.Model) (wiresToAdd: Wire list) : BusWireT.Model =
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

/// Retrieves all wires which intersect an arbitrary bounding box & the index
/// of the segment which intersects the box
/// HLP23: Derek Lai (ddl20)
let getWiresInBox (box: BoundingBox) (model: Model) : (Wire * int) list =
    let wires = (List.ofSeq (Seq.cast model.Wires.Values))

    let bottomRight =
        { box.TopLeft with
            X = box.TopLeft.X + box.W
            Y = box.TopLeft.Y + box.H }

    // State Tuple - (overlapping: bool, overlapping_wire_index: int)
    let checkOverlapFolder (startPos: XYPos) (endPos: XYPos) (state: bool * int) (segment: Segment) : bool * int =
        let overlap = overlap2D (startPos, endPos) (box.TopLeft, bottomRight)
        (fst state || overlap), if overlap then segment.Index else snd state

    List.map (fun w -> foldOverNonZeroSegs checkOverlapFolder (false, -1) w, w) wires
    |> List.filter (fun l -> fst (fst l))
    |> List.map (fun ((_, index), w) -> w, index)

/// Used to fix bounding box with negative width and heights
/// HLP23: Derek Lai (ddl20)
let fixBoundingBox (box: BoundingBox): BoundingBox =
    let x = min (box.TopLeft.X + box.W) box.TopLeft.X
    let y = min (box.TopLeft.Y + box.H) box.TopLeft.Y
    {TopLeft = {X = x; Y = y}; W = abs box.W; H = abs box.H}*)



//-------------------------------------------------------------------------------------------------//
//------------------------TYPES USED INTERNALLY FOR SEPARATION AND ORDERING------------------------//
//-------------------------------------------------------------------------------------------------//

module Constants =
    let wireSeparationFromSymbol = 7. // must be smaller than Buswire.nubLength
    let maxCallsToShiftHorizontalSeg = 5
    /// Must be smaller than Buswire.nubLength
    let minWireSeparation = 7.
    let smallOffset = 0.0001
    let maxSegmentSeparation = 30.
    /// lines within this distance of each other are considered to overlap
    let overlapTolerance = 2.
    /// corners with max length edge larger than this are not removed
    let separateCaptureOverlap = 35. // Larger than as overlapTolerance
    let minWireLengthToSeparate = 10. // prevents short wires being squiggly
    let maxCornerSize = 100.
    /// How close are segment extensions caused by corner removal allowed to
    /// get to other elements? Maybe needs to be smaller than some otehr things for
    /// successful corner removal?
    let extensionTolerance = 3.
/// Screen direction in which objects are moved

[<StringEnum>]
type DirectionToMove =
    | Up_
    | Down_
    | Left_
    | Right_

/// swap X and Y coordinates if orientation = Vertical
let swapXY (pos: XYPos) (orientation: Orientation) : XYPos =
    match orientation with
    | Horizontal -> pos
    | Vertical -> { X = pos.Y; Y = pos.X }

/// swap X & Y coordinats in BB if orientation is vertical
let swapBB (box: BoundingBox) (orientation: Orientation) : BoundingBox =
    match orientation with
    | Horizontal -> box
    | Vertical -> { 
            TopLeft = swapXY box.TopLeft orientation
            W = box.H
            H = box.W }

/// Return new poistion moved the the direction and amount shown.
let updatePos (direction: DirectionToMove) (distanceToShift: float) (pos: XYPos) : XYPos =
    match direction with
    | Up_ -> { pos with Y = pos.Y - distanceToShift }
    | Down_ -> { pos with Y = pos.Y + distanceToShift }
    | Left_ -> { pos with X = pos.X - distanceToShift }
    | Right_ -> { pos with X = pos.X + distanceToShift }


/// Used to capture the 1D coordinates of the two ends of a line. (see Line).
type Bound = { MinB: float; MaxB: float }

/// Protected type for Line IDs 
[<Erase>]
type LineId = LineId of int
with member this.Index = match this with | LineId i -> i

/// Type of Line: note that this is correlated with whether line is a 
/// segment or a symbol edge

[<StringEnum>]
type LType = 
    /// a non-segment fixed (symbol boundary) barrier
    | BARRIER  
    /// a movable line segment
    | NORMSEG 
    /// a segment which is a fixed barrier in clustering but can change after.
    | FIXEDSEG 
    /// a fixed segment which has been manually routed and can never move
    | FIXEDMANUALSEG
    /// a segment linked to another on the same net which is not clustered
    | LINKEDSEG


/// Used to represent a line on the canvas, e.g. a wire segment or symbol edge.
/// The array of lines will all have the same orientation - so optimisation is done in two phases
/// for vertical and horizontal segments. Each phase optimises one half of the segments.
type Line =
    { 
        mutable P: float // the coordinate X or Y perpendicular to the line.
        mutable B: Bound // the two "other" coordinates
        Orientation: Orientation
        Seg1: ASegment option // if the line comes from a wire segment this references the segment and wire
        mutable LType: LType
        mutable SameNetLink: Line list
        Wid: ConnectionId
        PortId: OutputPortId
        mutable Lid: LineId } // index in lines array of this Line.


/// Used to cluster together overlapping and adjacent lines into a group that
/// can be spread out. This is the parameter in a tail recursion used to do the clustering
type Cluster =
    { 
        UpperFix: float option // if clustering is stopped by a barrier
        LowerFix: float option // if clustering is stopped by a barrier
        Segments: int list // list of movable lines found (which will be spread out)
        Bound: Bound } // union of bounds of all segments found so far

let clusterSegments_ = Lens.create (fun (c:Cluster) -> c.Segments) (fun n c -> {c with Segments = n})

/// Controls direction of Cluster search in expandCluster.
/// Search is upwards first and then downwards so downwards search takes a Cluster
/// (generated from upwards search) as parameter.

type LocSearchDir =
    | Upwards
    | Downwards of Cluster

type Extension = {    
    ExtOri: Orientation
    ExtB: Bound
    ExtP: float
    }

/// Defines a wire corner that could be removed.
/// the two removed segments are [StartSeg+1..EndSeg-1]
/// In addition, StartSeg and EndSeg have Length changed.
type WireCorner = {
    /// Wire on which the corner lies
    Wire: Wire
    /// index of segment immediately before the two deleted segments
    StartSeg: int
    /// change in length of StartSeg needed to remove the corner
    StartSegChange: float
    /// change in length of EndSeg needed to remove the corner
    EndSegChange: float // EndSeg = StartSeg + 3
    /// orientation of StartSeg. EndSeg has opposite orientation.
    StartSegOrientation: Orientation
    }

/// Some operations need to work on Horizontal and Vertical lines together.
/// This captures the static information needed to do this.
type LineInfo = {
    /// Vertical lines
    VLines: Line array
    /// Horizontal lines
    HLines: Line array
    /// map from wire IDs to wires
    WireMap: Map<ConnectionId,Wire>
    /// map from segment IDs to lines
    LineMap: Map<int*ConnectionId, LineId>}


//-------------------------------------------------------------------------------------------------//
//--------------------------------HELPERS USED IN CLUSTERING SEGMENTS------------------------------//
//-------------------------------------------------------------------------------------------------//
open Constants

/// Get the horizontal length of the visible segment emerging from a port
let getVisibleNubLength (atEnd: bool) (wire: Wire) =
    let segs = wire.Segments
    let getLength i =
        if atEnd then
            segs.Length - 1 - i
        else
            i
        |> (fun index -> segs[index].Length)
    if (getLength 1) < smallOffset then
        getLength 2 + getLength 0
    else
        getLength 0

/// Return true if the segment extends a wire parallel with a nub (wire end).
let segmentIsNubExtension (wire: Wire) (segIndex: int) : bool =
    let segs = wire.Segments
    let nSegs = segs.Length
    let lastSeg = nSegs-1
    let revSeg n = segs[lastSeg-n]
    match segIndex, lastSeg-segIndex with
    | 0, _ | _, 0 -> true
    | 2, _ when segs[1].IsZero -> true
    |_, 2 when  (revSeg 1).IsZero -> true
    | _ -> false

/// Get the segment indexes within a Cluster (loc)
let inline segPL (lines: Line array) loc =
    loc.Segments |> (List.map (fun n -> lines[n].P))

/// ideal (max) width of segments in loc
let inline widthS (loc: Cluster) =
    float loc.Segments.Length * maxSegmentSeparation

/// ideal upper bound in P direction of segments with P value in pts.
let inline upperS pts =
    (List.min pts + List.max pts) / 2.
    + float pts.Length * maxSegmentSeparation / 2.

/// ideal lower bound in P direction of segments with P value in pts
let inline lowerS pts =
    (List.min pts + List.max pts) / 2.
    - float pts.Length * maxSegmentSeparation / 2.

/// ideal upper bound in P direction of loc including possible fixed constraint
let inline upperB (lines: Line array) (loc: Cluster) =
    let pts = segPL lines loc

    match loc.UpperFix, loc.LowerFix with
    | Some u, _ -> u
    | None, Some l when l > lowerS pts -> l + widthS loc
    | _ -> upperS pts

/// ideal lower bound in P direction of loc including possible fixed constraint
let inline lowerB (lines: Line array) loc =
    let pts = segPL lines loc

    match loc.UpperFix, loc.LowerFix with
    | _, Some l -> l
    | Some u, None when u < upperS pts -> u - widthS loc
    | _ -> lowerS pts


//-------------------------------------------------------------------------------------------------//
//--------------------------------LOW-LEVEL PRINTING (returns strings)-----------------------------//
//-------------------------------------------------------------------------------------------------//

/// Return string to display a wire
let pWire (wire: Wire) =
    let segs = wire.Segments
    let nSegs = segs.Length
    let aSegs = BlockHelpers.getAbsSegments wire
    let pASeg (aSeg:ASegment) =
        let isMan = 
            match aSeg.Segment.Mode with | Manual -> "M" | Auto -> "A"
        let vec = aSeg.End - aSeg.Start
        if aSeg.IsZero then
            isMan + ".S0"
        else
            match getSegmentOrientation aSeg.Start aSeg.End, aSeg.Segment.Length > 0 with
            | Vertical, true -> "Dn"
            | Vertical, false -> "Up"
            | Horizontal,true -> "Rt"
            | Horizontal, false -> "Lt"
            |> (fun s -> isMan + "." + s + $"%.0f{abs aSeg.Segment.Length}")

    let pSegs = aSegs |> List.map pASeg |> String.concat "-"

    sprintf $"W{nSegs}:{wire.InitialOrientation}->{pSegs}"

/// Return string to display an Option
let pOpt (x: 'a option) = match x with | None -> "None" | Some x -> $"^{x}^"

/// Return string to display the type of a Line
let pLineType (line:Line) = $"{line.LType}"

/// Return string to display a Line
let pLine (line:Line) = 
    let ori = match line.Orientation with | Horizontal -> "H" | Vertical -> "V"
    $"|{ori}L{line.Lid.Index}.P=%.0f{line.P}.{pLineType line}:B=%.0f{line.B.MinB}-%.0f{line.B.MaxB}: \
    ({line.SameNetLink |> List.map (fun l -> l.Lid.Index)})|"

/// Return string to display an array of Lines
let pLines (lineA: Line array) =
    $"""{lineA |> Array.map (fun line -> pLine line) |> String.concat "\n"}"""

/// Return string to display a Cluster (compactly).
/// See also pAllCluster.
let pCluster (loc:Cluster) =
    $"Cluster:<{pOpt loc.LowerFix}-{loc.Segments}-{pOpt loc.UpperFix}>"

/// Return string to display a Cluster (long form).
/// See also pCluster.
let pAllCluster (lines: Line array) (loc:Cluster) =
    let oris = match lines[0].Orientation with | Horizontal -> "Horiz" | Vertical -> "Vert"
    $"""Cluster-{oris}:<L={pOpt loc.LowerFix}-{loc.Segments |> List.map (fun n -> pLine lines[n]) |> String.concat ","}-U={pOpt loc.UpperFix}>"""


//-------------------------------------------------------------------------------------------------//
//-----------------------------------------UTILITY FUNCTIONS---------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// Linear search in an array from searchStart in a direction dir = +1/-1 from searchStart.
/// Give up and return None if giveUp is true.
/// Return first location for which predicate is true.
let rec tryFindIndexInArray (searchStart: LineId) (dir: int) (predicate: 'T -> bool) (giveUp: 'T -> bool) (arr: 'T array) =
    if searchStart.Index < 0 || searchStart.Index > arr.Length - 1 then
        None
    else
        match predicate arr[searchStart.Index], giveUp arr[searchStart.Index] with
        | _, true -> None
        | true, _ -> Some searchStart
        | false, _ -> tryFindIndexInArray (LineId(searchStart.Index + dir)) dir predicate giveUp arr
            

/// True if bounds b1 and b2 overlap or are exactly adjacent
let hasOverlap (b1: Bound) (b2: Bound) =
    inMiddleOrEndOf b1.MinB b2.MinB b1.MaxB
    || inMiddleOrEndOf b1.MinB b2.MaxB b1.MinB
    || inMiddleOrEndOf b2.MinB b1.MinB b2.MaxB

/// True if bounds b1 and b2 overlap or are exactly adjacent
let hasNearOverlap (tolerance: float) (b1: Bound) (b2: Bound) =
    inMiddleOf (b1.MinB-tolerance) b2.MinB (b1.MaxB+tolerance)
    || inMiddleOf (b1.MinB-tolerance)  b2.MaxB (b1.MinB+tolerance)
    || inMiddleOf (b2.MinB-tolerance) b1.MinB (b2.MaxB+tolerance)

/// Return union of two bounds b1 and b2. b1 & b2 must overlap or be adjacent.
/// Otherwise the inclusive interval containing b1 and b2 is returned.
let boundUnion (b1: Bound) (b2: Bound) =
    {   MinB = min b1.MinB b2.MinB
        MaxB = max b1.MaxB b2.MaxB }


/// Move segment by amount posDelta in direction perpendicular to segment - + => X or y increases.
/// Movement is by changing lengths of two segments on either side.
/// Will fail if called to change a nub at either end of a wire (nubs cannot move).
let moveSegment (index: int) (posDelta: float) (wire: Wire) =
    let segs = wire.Segments

    if index < 1 || index > segs.Length - 2 then
        failwithf $"What? moveSegment is trying to move segment {index} of a wire length {segs.Length}"

    { wire with
        Segments =
            segs
            |> List.updateAt (index - 1) { segs[index - 1] with Length = segs[index - 1].Length + posDelta }
            |> List.updateAt (index + 1) { segs[index + 1] with Length = segs[index + 1].Length - posDelta } }

/// Change wires to move a wire segment represented by line to the given new value of P coordinate.
/// P is X or Y according to ori.
let moveLine (ori: Orientation) (newP: float) (line: Line) (wires: Map<ConnectionId, Wire>) =
    match line.Seg1 with
    | None -> failwithf "Can't move Line {line} - it is not a segment"
    | Some seg ->
        let oldP =
            match ori with
            | Horizontal -> seg.Start.Y
            | Vertical -> seg.Start.X

        let segIndex = seg.Segment.Index
        let wid = seg.Segment.WireId
        if newP <> oldP then
            let updateWire = Option.map (moveSegment segIndex (newP - oldP))
            Map.change seg.Segment.WireId updateWire wires
        else
            wires
