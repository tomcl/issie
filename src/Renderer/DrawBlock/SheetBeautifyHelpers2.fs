module SheetBeautifyHelpers2

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team

open BlockHelpers
open CommonTypes
open SymbolHelpers
open Optics
open Operators
open Symbol
open RotateScale
open Helpers
open BusWireRoutingHelpers
open DrawModelType



////////// Helper functions //////////

/// Adapted from TestDrawBlock
/// New: includes an option to not consider nubs.
/// The visible segments of a wire, as a list of vectors, from source end to target end.
/// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
/// which if present causes the two segments on either side of it to coalesce into a single visible segment.
/// A wire can have any number of visible segments - even 1.
let visibleSegments
        (wId: ConnectionId)
        (model: SheetT.Model)
        (stripNubs: bool)
        : XYPos list =

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

    /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
    /// wherever this is possible
    let rec coalesce (segVecs: XYPos list)  =
        match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
        | Some zeroVecIndex -> 
            let index = zeroVecIndex + 1 // base index onto full segVecs
            segVecs[0..index-2] @
            [segVecs[index-1] + segVecs[index+1]] @
            segVecs[index+2..segVecs.Length - 1]
            |> coalesce // recurse as long as more coalescing might be possible
        | None -> segVecs // end when there are no inner zero-length segment vectors
     
    match stripNubs with
    | true -> wire.Segments[1..wire.Segments.Length-2]
    | false -> wire.Segments
    |> List.mapi getSegmentVector
    |> coalesce

/// Returns all unique pairs of elements from a list.
let rec uniquePairs
        (lst: 'a list)
        : ('a * 'a) list =
    match lst with
    | [] -> []
    | h::t -> List.map (fun x -> (h, x)) t @ uniquePairs t

/// Converts a Rotation to an integer, useful for comparing rotations.
let rotationToInt
        (rotation: Rotation)
        : int =
    match rotation with
    | Rotation.Degree0 -> 0
    | Rotation.Degree90 -> 1
    | Rotation.Degree180 -> 2
    | Rotation.Degree270 -> 3

/// Finds the difference between two rotations as an integer.
let rotationDiff
        (rotation1: Rotation)
        (rotation2: Rotation)
        : int =
    ((rotationToInt rotation2) - (rotationToInt rotation1)) % 4

/// Returns a list of pairs of vertices representing all visible segments in the sheet.
/// Includes an option to not consider nubs.
let getVisibleSegs
        (sheet: SheetT.Model)
        (stripNubs: bool)
        : (XYPos*XYPos) list =

    // Extract wire vectors for each wire in sheet.
    let wireVectors =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map fst
        |> List.map (fun connId -> visibleSegments connId sheet stripNubs)

    // Extract start pos for each wire in sheet.
    let startPosList =
        sheet.Wire.Wires
        |> Map.toList
        |> List.map snd
        |> List.map (fun x -> x.StartPos)

    // Convert a starting position and a list of segment vectors to a list of vertices.
    let wireVectorsToVertices (vectors: XYPos list) (startPos: XYPos) : XYPos list =
        (startPos, vectors)
        ||> List.scan (fun currPos vector -> currPos + vector)

    List.map2 wireVectorsToVertices wireVectors startPosList
    |> List.map List.pairwise
    |> List.collect id



////////// Requested functions begin here //////////

/// B1RW A lens for the dimensions of a custom component symbol.
let customComponentDims_ =
    let get (symbol: SymbolT.Symbol) : XYPos =
        match symbol.Component.Type with
        | Custom _ -> (getCustomSymCorners symbol)[2]
        | _ -> {X=0; Y=0} // Shouldn't match
    let set (dims: XYPos) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
        setCustomCompHW dims.Y dims.X symbol
        // Another way of doing this is using manualSymbolResize, but that's more complicated.
    Lens.create get set

/// B2W Change the position of a symbol.
let setSymbolPosition
        (symbol: SymbolT.Symbol)
        (newPos: XYPos)
        : SymbolT.Symbol =
    moveSymbol (newPos - symbol.Pos) symbol

/// B3R Returns a list of ordered ports for a given side of a symbol.
let getOrderedPorts
        (model: SymbolT.Model)
        (symbol: SymbolT.Symbol)
        (edge: Edge)
        : Port list =
    symbol.PortMaps.Order[edge]
    |> List.map (fun portId -> model.Ports[portId])

/// B3W Sets the list of ordered ports for a given side of a symbol.
/// The provided list of ports must be a reordering of the existing list of ports
/// (i.e. this function can only change order).
let setOrderedPorts
        (symbol: SymbolT.Symbol)
        (edge: Edge)
        (ports: Port list)
        : SymbolT.Symbol =
    let getId (port: Port) = port.Id // To deal with weird type error
    let portIds = List.map getId ports
    symbol.PortMaps.Order
    |> Map.add edge portIds
    |> fun map -> Optic.set (SymbolT.portMaps_ >-> SymbolT.order_) map symbol

/// B4RW A lens for ReversedInputPorts in a symbol (see alternatives below).
let reversedInputPorts_ =
    let get (symbol: SymbolT.Symbol) : bool option =
        symbol.ReversedInputPorts
    let set (revInputPorts: bool option) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
        {symbol with ReversedInputPorts=revInputPorts}
    Lens.create get set

/// B4R The 'reversed input' state of a MUX.
let isReversedInputs
        (symbol: SymbolT.Symbol)
        : bool option =
    symbol.ReversedInputPorts

/// B4W Toggle the 'reversed input' state of a MUX.
let toggleReversedInputs
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    match symbol.ReversedInputPorts with
    | Some rev -> Optic.set reversedInputPorts_ (Some (not rev)) symbol
    | None -> symbol // Don't change the state of an old MUX

/// B5R Returns the position of a port on the sheet (port must exist).
let getPortSheetPos
        (model: SymbolT.Model)
        (port: Port)
        : XYPos =
    let symbol = model.Symbols[ComponentId port.HostId]
    (getPortPos symbol port) + symbol.Pos

/// B6R Returns the bounding box of a symbol.
// TODO am I missing something here?
let getSymbolBoundingBox
        (symbol: SymbolT.Symbol)
        : BoundingBox =
    getSymbolBoundingBox symbol

// Note: since a function to rotate by a given degree already exists
// (rotateSymbolByDegree), I decided to write a lens to set the rotation
// in absolute reference.
/// B7RW A lens for the rotation state of a symbol.
let rotation_ =
    let get (symbol: SymbolT.Symbol) : Rotation =
        symbol.STransform.Rotation
    let set (rotation: Rotation) (symbol: SymbolT.Symbol) =
        let requiredRotation =
            match rotationDiff symbol.STransform.Rotation rotation with
            | 0 -> Rotation.Degree0
            | 1 -> Rotation.Degree90
            | 2 -> Rotation.Degree180
            | 3 -> Rotation.Degree270
            | _ -> failwithf "Impossible - result is modulo division by 4"
        rotateSymbolInBlock requiredRotation (getRotatedSymbolCentre symbol) symbol
        // TODO this probably doesn't work - see rotateSymbol below.
    Lens.create get set

/// This is probably useful as well.
/// B7W Rotate a symbol.
let rotateSymbol
        (rotation: Rotation)
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    match rotation with
    | Degree0 -> symbol
    | Degree90 | Degree270 -> rotateSymbolInBlock rotation (getRotatedSymbolCentre symbol) symbol
    | Degree180 ->
        symbol
        |> rotateSymbolInBlock Degree90 (getRotatedSymbolCentre symbol)
        |> rotateSymbolInBlock Degree90 (getRotatedSymbolCentre symbol)

/// B8RW A lens for the flip state of a symbol. Note that toggling the flip state
/// while keeping rotation constant implements horizontal flipping.
let flipped_ =
    let get (symbol: SymbolT.Symbol) : bool =
        symbol.STransform.Flipped
    let set (flip: bool) (symbol: SymbolT.Symbol) : SymbolT.Symbol =
        match symbol.STransform.Flipped = flip with
        | true -> symbol
        | false -> flipSymbolInBlock SymbolT.FlipHorizontal (getRotatedSymbolCentre symbol) symbol
    Lens.create get set

// This might be useful as well.
/// B8W Flip a symbol.
let flipSymbol
        (flip: SymbolT.FlipType)
        (symbol: SymbolT.Symbol)
        : SymbolT.Symbol =
    flipSymbolInBlock flip (getRotatedSymbolCentre symbol) symbol

/// T1R Returns the number of pairs of symbols that intersect each other.
let numPairsIntersectingSymbols
        (sheet: SheetT.Model)
        : int =
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
    uniquePairs boxes
    |> List.filter (fun (box1, box2) -> BlockHelpers.overlap2DBox box1 box2)
    |> List.length

/// T2R Returns the number of distinct visible wire segments that intersect one or more symbols.
let numSegmentsIntersectSymbols
        (sheet: SheetT.Model)
        : int =

    let allSymbols =
        sheet.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.toList
        |> List.filter (fun s -> s.Annotation = None)
        |> List.map (fun s -> (s.Component.Type, Symbol.getSymbolBoundingBox s))

    // Adapted from BusWireRoute
    // Returns whether or not the given segments intersects a symbol.
    let segmentIntersectsAnyBox (seg: XYPos*XYPos) : bool =
        let startPos = fst seg
        let endPos = snd seg
        allSymbols
        |> List.map (fun (compType, boundingBox) ->
            (
                compType,
                {
                    W = boundingBox.W + Constants.minWireSeparation * 2.
                    H = boundingBox.H + Constants.minWireSeparation * 2.
                    TopLeft =
                    boundingBox.TopLeft
                    |> updatePos Left_ Constants.minWireSeparation
                    |> updatePos Up_ Constants.minWireSeparation
                }
            ))
        |> List.exists (fun (compType, boundingBox) ->
            // TODO consider dealing with MUX bug later
            match segmentIntersectsBoundingBox boundingBox startPos endPos with // do not consider the symbols that the wire is connected to
            | Some _ -> true // segment intersects bounding box
            | None -> false // no intersection
        )
        //|> List.map (fun (compType, boundingBox) -> boundingBox)

    getVisibleSegs sheet true
    |> List.filter segmentIntersectsAnyBox
    |> List.length

/// Returns the orientation of a segment given by a pair of vertices.
/// Although this shouldn't happen, if given a zero-length segment, return Vertical.
let getVertexPairOrientation
        (seg: XYPos*XYPos)
        : BusWireT.Orientation =
    let deltaX = abs ((fst seg).X - (snd seg).X)
    if deltaX > XYPos.epsilon then BusWireT.Horizontal
    else BusWireT.Vertical

/// T3R Returns the number of segments intersecting at right angles.
/// We look for segments that form a + shape, rather than a T shape.
let numRightAngleSegCrossings
        (sheet: SheetT.Model)
        : int =

    // Check if a given horizontal and vertical segment are crossing.
    // Make sure they actually cross and don't just touch each other.
    let isCrossing (hSeg: XYPos*XYPos) (vSeg: XYPos*XYPos) : bool =
        let top = min (fst vSeg) (snd vSeg)
        let bottom = max (fst vSeg) (snd vSeg)
        let left = min (fst hSeg) (snd hSeg)
        let right = max (fst hSeg) (snd hSeg)

        top.X - left.X > XYPos.epsilon && right.X - top.X > XYPos.epsilon &&
        left.Y - top.Y > XYPos.epsilon && bottom.Y - left.Y > XYPos.epsilon

    getVisibleSegs sheet false
    |> uniquePairs
    |> List.filter (fun (seg1, seg2) ->
        match (getVertexPairOrientation seg1, getVertexPairOrientation seg2) with
        | BusWireT.Horizontal, BusWireT.Vertical -> isCrossing seg1 seg2
        | BusWireT.Vertical, BusWireT.Horizontal -> isCrossing seg2 seg1
        | _, _ -> false
    )
    |> List.length

/// Merge intervals given as pairs of start/end positions in a list.
let mergeIntervals
        (intervals: (float*float) list)
        : (float*float) list =
    let sortedIntervals = List.sortBy fst intervals
    let mergedIntervals =
        ([], sortedIntervals)
        ||> List.fold (fun acc (start, finish) ->
            match acc with
            | [] -> [(start, finish)]
            | (lastStart, lastFinish)::tail when start <= lastFinish ->
                (lastStart, max lastFinish finish)::tail
            | _ ->
                (start, finish)::acc
        )
    List.rev mergedIntervals

// Converts an ASegment to a pair of vertices.
let aSegToVertices
        (seg: BusWireT.ASegment)
        : (XYPos*XYPos) =
    seg.Start, seg.End

// Transform list of vertex pairs to list of 1D invervals.
let get1DIntervals
        (vertexList: (XYPos*XYPos) list)
        : (float*float) list =
    match vertexList with
    | [] -> [] // Shouldn't happen
    | h::_ when getVertexPairOrientation h = BusWireT.Horizontal ->
        vertexList
        |> List.map (fun pair ->
            let left = min (fst pair).X (snd pair).X
            let right = max (fst pair).X (snd pair).X
            (left, right)
        )
    | _ ->
        vertexList
        |> List.map (fun pair ->
            let top = min (fst pair).Y (snd pair).Y
            let bottom = max (fst pair).Y (snd pair).Y
            (top, bottom)
        )

// Sum total length covered by a list of disjoint intervals.
let sumDisjointIntervals
        (intervals: (float*float) list)
        : float =
    (0.0, intervals)
    ||> List.fold (fun currSum interval -> currSum + (snd interval) - (fst interval))

/// T4R Find the total visible wire length on the sheet.
/// Overlapping segments from the same net are only counted once.
let visibleWireLength
        (sheet: SheetT.Model)
        : float =

    // Incorporate a new segment into the existing groups of collinear segments.
    let addNewSegToGroup
            (groups: Map<BusWireT.ASegment, BusWireT.ASegment list>)
            (seg: BusWireT.ASegment)
            : Map<BusWireT.ASegment, BusWireT.ASegment list> =
        groups
        |> Map.tryFindKey (fun leader _ ->
            match (leader.Orientation = seg.Orientation), seg.Orientation with
            | true, BusWireT.Horizontal when abs (leader.Start.Y - seg.Start.Y) < XYPos.epsilon -> true
            | true, BusWireT.Vertical when abs (leader.Start.X - seg.Start.X) < XYPos.epsilon -> true
            | _, _ -> false)
        |> function
           | Some leader -> Map.add leader (groups[leader] @ [seg]) groups
           | None -> Map.add seg [seg] groups

    // Split a list of segments into groups of collinear segments.
    let partitionCollinearSegs (segList: BusWireT.ASegment list) : BusWireT.ASegment list list =
        (Map.empty, segList)
        ||> List.fold addNewSegToGroup
        |> Map.toList
        |> List.map snd

    // Find total visible wire length for a net.
    let getNetVisibleLength (wires: BusWireT.Wire list) : float =
        let intervalsList =
            wires
            |> List.collect getAbsSegments
            |> partitionCollinearSegs
            |> List.map (List.map aSegToVertices)
            |> List.map get1DIntervals
            |> List.map mergeIntervals
        (0.0, intervalsList)
        ||> List.fold (fun currSum intervals -> currSum + sumDisjointIntervals intervals)

    partitionWiresIntoNets sheet.Wire
    |> List.map snd
    |> List.map (List.map snd)
    |> List.map getNetVisibleLength
    |> List.fold (+) 0.0

/// T5R Returns the number of visible wire right-angles in the sheet.
// TODO consider whether we should deal with overlapping same-net right-angles.
let numWireRightAngles
        (sheet: SheetT.Model)
        : int =
    sheet.Wire.Wires
    |> Map.toList
    |> List.map fst
    |> List.map (fun connectionId -> visibleSegments connectionId sheet false)
    |> List.map (fun lst -> lst.Length - 1)
    |> List.fold (+) 0

/// Check if the segment at the given index is a zero-length segment connecting
/// two segments that retrace each other.
/// This function does not check if i is a valid index.
let isRetrace
        (segs: BusWireT.Segment list)
        (i: int)
        : bool =
    abs segs[i].Length < XYPos.epsilon &&
    abs segs[i - 1].Length > XYPos.epsilon &&
    abs segs[i + 1].Length > XYPos.epsilon &&
    ((segs[i - 1].Length < 0 && segs[i + 1].Length > 0) ||
     (segs[i - 1].Length > 0 && segs[i + 1].Length < 0))

/// Returns a list of all pairs of wire segments that retrace themselves, across the whole sheet.
let getAllRetracedSegs
        (sheet: SheetT.Model)
        : (BusWireT.Segment*BusWireT.Segment) list =

    // Returns a list of pairs of segments from the provided segment list that retrace themselves.
    let getWireRetracedSegs (segs: BusWireT.Segment list) : (BusWireT.Segment*BusWireT.Segment) list =
        ([], [1..segs.Length-2])
        ||> List.fold (fun result i ->
            if isRetrace segs i then result @ [(segs[i - 1], segs[i + 1])]
            else result
        )

    sheet.Wire.Wires
    |> Map.toList
    |> List.map snd
    |> List.map (fun wire -> wire.Segments)
    |> List.collect getWireRetracedSegs

/// Returns a list of all visible endpoint segments (not the nubs)
/// that retrace into their connected symbol.
let getSymbolRetraceSegs
        (sheet: SheetT.Model)
        : BusWireT.Segment list =

    // Finds symbol-retraced segments for a given segment list.
    // Returned list can have 0, 1, or 2 segments.
    let findRetraced (segs: BusWireT.Segment list) : BusWireT.Segment list =
        if segs.Length < 4 then []
        else
            let segStart =
                match isRetrace segs 1 with
                | true -> [segs[2]]
                | false -> []
            let segEnd =
                match isRetrace segs (segs.Length-2) with
                | true -> [segs[segs.Length-3]]
                | false -> []
            segStart @ segEnd

    sheet.Wire.Wires
    |> Map.toList
    |> List.map snd
    |> List.map (fun wire -> wire.Segments)
    |> List.collect findRetraced

/// T6R Returns:
/// 1. A list of all the pairs of segments that retrace themselves.
/// 2. A list of all end-of-wire segments that retrace into their symbols.
let getRetraced
        (sheet: SheetT.Model)
        : {|AllRetracedSegs: (BusWireT.Segment*BusWireT.Segment) list;
            SymbolRetraceSegs: BusWireT.Segment list|} =
    // Note there is some repeated work for this implementation,
    // but it is the easiest to understand.
    {|AllRetracedSegs = getAllRetracedSegs sheet;
      SymbolRetraceSegs = getSymbolRetraceSegs sheet|}

/// Returns true if the given symbol does not intersect any other symbol in the sheet.
let noSymbolIntersections
        (sheet: SheetT.Model)
        (symbol: SymbolT.Symbol)
        : bool =
    let symbolBox = Symbol.getSymbolBoundingBox symbol
    sheet.BoundingBoxes
    |> Map.exists (fun compId box -> compId <> symbol.Id && BlockHelpers.overlap2DBox box symbolBox)
    |> not
