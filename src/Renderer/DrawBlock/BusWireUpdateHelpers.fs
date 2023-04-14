module BusWireUpdateHelpers

open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire

open Optics
open Operators


//-------------------------segmentIntersectsBoundingBox---------------------------------//

/// Type used to simplify BoundingBox intersection calculations
type Rectangle = {
    TopLeft: XYPos
    BottomRight: XYPos
}

/// Returns the X-value of an XYPos
let inline toX (pos: XYPos) = pos.X

/// Returns the Y-value of an XYPos
let inline toY (pos: XYPos) = pos.Y

/// Returns the X and Y fields of an XYPos as a pair of floats
let inline getXY (pos: XYPos) = pos.X, pos.Y

/// Returns pos with the X and Y fields scaled by factor (I didn't like the order of parameters for the * operator in XYPos)
let inline scalePos (factor: float) (pos: XYPos) : XYPos =
    { X = factor * pos.X; Y = factor * pos.Y}

/// Returns true if p1 is less than or equal to p2 (has both smaller X and Y values
let inline lThanEqualPos (p1: XYPos) (p2: XYPos) : bool =
    p1.X <= p2.X && p1.Y <= p2.Y

/// Returns the dot product of 2 XYPos
let inline dotProduct (p1: XYPos) (p2: XYPos) : float = 
    p1.X * p2.X + p1.Y * p2.Y

/// Returns the squared distance between 2 points using Pythagoras
let inline squaredDistance (p1: XYPos) (p2: XYPos) = 
    let diff = p1 - p2
    dotProduct diff diff

/// Checks if 2 rectangles intersect
let rectanglesIntersect (rect1: Rectangle) (rect2: Rectangle) =
    /// Checks if there is an intersection in the X or Y dimension
    let intersect1D (xOrY: XYPos -> float): bool =
        let qHi = min (xOrY rect1.BottomRight) (xOrY rect2.BottomRight)
        let qLo = max (xOrY rect1.TopLeft) (xOrY rect2.TopLeft)
        qLo <= qHi

    (intersect1D toX) && (intersect1D toY)

let findPerpendicularDistance (segStart:XYPos) (segEnd:XYPos) (point:XYPos) =
    match abs (segStart.X - segEnd.X) > abs (segStart.Y - segEnd.Y) with
    | true -> abs (segStart.Y - point.Y)
    | false -> abs (segStart.X - point.X)

/// Checks if a segment intersects a bounding box using the segment's start and end XYPos
/// return how close teh segment runs to the box centre, if it intersects
let segmentIntersectsBoundingBox (box: BoundingBox) segStart segEnd =
    let toRect p1 p2 =
        let topLeft, bottomRight =
            if lThanEqualPos p1 p2 then
                p1, p2
            else
                p2, p1

        { TopLeft = topLeft
          BottomRight = bottomRight }

    let bbBottomRight =
        { X = box.TopLeft.X + box.W
          Y = box.TopLeft.Y + box.H }

    let bbRect = toRect box.TopLeft bbBottomRight
    let segRect = toRect segStart segEnd

    if rectanglesIntersect bbRect segRect then
        Some <| findPerpendicularDistance segStart segEnd ((box.TopLeft + bbBottomRight) * 0.5)
    else
        None

//--------------------------------------------------------------------------------//
//---------------------------getClickedSegment-----------------------------------//

/// Returns Some distance between a point and a segment defined by a start and end XYPos, 
/// and None if the segment is of 0 length (can't be clicked)
let distanceBetweenPointAndSegment (segStart : XYPos) (segEnd : XYPos) (point : XYPos) : float option = 
    match squaredDistance segStart segEnd with
    | 0. -> None
    | l2 -> 
        // Extend the segment to line segStart + t (segEnd - segStart)
        // The projection of point on this line falls at tProjection
        let tProjection = dotProduct (point - segStart) (segEnd - segStart) / l2 
        let tBounded = max 0. (min 1. tProjection) // Bound tProjection to be within the segment
        let boundedProjection = 
            segEnd - segStart
            |> scalePos tBounded
            |> (+) segStart
        Some (sqrt (squaredDistance point boundedProjection))

/// Finds the closest non-zero-length segment in a wire to a mouse click using euclidean distance.
/// Return as an ASegment.
let getClickedSegment (model: Model) (wireId: ConnectionId) (mouse: XYPos) : ASegment =
    let closestSegment segStart segEnd state (seg: Segment) =
        let currDist = Option.get <| distanceBetweenPointAndSegment segStart segEnd mouse
        let aSeg = {Start=segStart; End=segEnd; Segment=seg}
        match state with
        | Some (minASeg, minDist) ->
            if currDist < minDist then
                Some (aSeg, currDist)
            else
                Some (minASeg, minDist)
        | None -> Some (aSeg, currDist) // Needed to deal with initial state
    match foldOverNonZeroSegs closestSegment None model.Wires[wireId] with
    | Some (segment, _dist) -> 
        segment
    | None -> failwithf "getClosestSegment was given a wire with no segments" // Should never happen

//--------------------------------------------------------------------------------//

//------------------------Wires Filtering Functions--------------------------------//

/// Returns a list of all the wires in the given model
let getWireList (model: Model) =
    model.Wires
    |> Map.toList
    |> List.map snd

/// Returns the IDs of the wires in the model connected to a list of components given by compIds
let getConnectedWires model compIds =
    let containsPorts wire =
        let inputPorts, outputPorts =
            Symbol.getPortLocations model.Symbol compIds

        Map.containsKey wire.InputPort inputPorts
        || Map.containsKey wire.OutputPort outputPorts

    model
    |> getWireList
    |> List.filter containsPorts

/// Returns the IDs of the wires in the model connected to a list of components given by compIds
let getConnectedWireIds model compIds =
    getConnectedWires model compIds
    |> List.map (fun wire -> wire.WId)

/// Returns a list of wire IDs that meet the given condition
let getFilteredIdList condition wireLst = 
    wireLst
    |> List.filter condition
    |> List.map (fun wire -> wire.WId)

/// Given a model and a list of component Ids, returns an anonymous record
/// containing the id of wires connected to input ports, output ports or both
let filterWiresByCompMoved (model: Model) (compIds: list<ComponentId>) =
    let wireList = getWireList model

    let inputPorts, outputPorts =
        Symbol.getPortLocations model.Symbol compIds

    let containsInputPort wire =
        Map.containsKey wire.InputPort inputPorts

    let containsOutputPort wire =
        Map.containsKey wire.OutputPort outputPorts

    let containsBothPort wire =
        containsInputPort wire && containsOutputPort wire

    let inputWires =
        wireList |> getFilteredIdList containsInputPort

    let outputWires =
        wireList |> getFilteredIdList containsOutputPort

    let fullyConnected =
        wireList |> getFilteredIdList containsBothPort

    {| Inputs = inputWires; Outputs = outputWires; Both = fullyConnected |}

//--------------------------------------------------------------------------------//

//  ====================================================================================================================
//
//                                                  WIRE NOTES
//
// - The first and last segments of a wire (connected to the output and input ports) are called the 'nubs'. These have a minimum 
//   length defined in Constants.nubLength, and are oriented perpendicular to the symbol edge (i.e. A nub for a port on the Right side
//   of a Symbol will be Horizontal). The initial positions and orientations of these nubs are defined in wire. Nubs cannot be dragged
//
// - Additional segments are generated to route between the two nubs. The orientation of one segment will always be the 
//   opposite of the previous.
//
// - To allow for any (non-nub) segments to be draggable, several segments of length 0 are inserted into the initial segment list.
//
// - Both the start and end of the wire is defined to allow the wire to be processed from either direction. This is important because
//   routing is performed from the port that has been moved (i.e. if the input port has been moved we process from the end of the wire)
//
// - Partial autorouting attempts to preserve manually routed segments when moving a port. The fixed point is defined as 
//   the end of the first manually routed segment from the moved port. Partial autorouting can only be applied when
//   the position of the moved port relative to the fixed point is the same as before it was moved (see relativePosition).
//
// ======================================================================================================================

//------------------------------moveSegment--------------------------------------//

/// Returns a distance for a wire move that has been reduced if needed to enforce minimum first/last segment lengths.
/// These prevent the first non-zero segment perpendicular to the nubs
/// to be dragged closer than Constants.nubLength
/// TODO - this can maybe be simplified given we now coalesce segments
let getSafeDistanceForMove (segments: Segment list) (index: int) (distance: float) =
    /// Returns a list of segments up to the first non-zero segment perpendicular to the segment leaving the port
    let findBindingSegments portIndex segList = 
        segList
        |> List.takeWhile (fun seg -> seg.Index % 2 = portIndex % 2 || seg.Length = 0) // Works for both input and output ports

    let findDistanceFromPort boundSegList =
        (0., boundSegList)
        ||> List.fold (fun dist seg -> dist + seg.Length) // Since the segments in perpendicular direction are 0 we can just sum up all the segments as if they are in the same direction
   
    let reduceDistance bindingSegs findBindingIndex distance = 
        if findBindingIndex bindingSegs <> index then 
            distance
        else
            findDistanceFromPort bindingSegs
            |> (fun dist -> 
                    if sign dist = -1 then 
                        max distance (dist + Constants.nubLength)
                    else 
                        min distance (dist - Constants.nubLength))

    let bindingInputSegs = 
        segments
        |> findBindingSegments 0
        |> List.map (fun seg -> { seg with Length = -seg.Length})

    let bindingOutputSegs =
        List.rev segments
        |> findBindingSegments (segments.Length - 1)

    let findInputBindingIndex boundSegList =
        boundSegList
        |> List.length

    let findOutputBindingIndex =
        findInputBindingIndex
        >> (-) (segments.Length - 1)

    distance
    |> reduceDistance bindingInputSegs findInputBindingIndex
    |> reduceDistance bindingOutputSegs findOutputBindingIndex

/// Used when two segments can be coalesced by removing a zero segment separating
/// them
let removeZeroSegment (segs: Segment list) indexToRemove =
    let index = indexToRemove
    let newLength = segs[index+1].Length + segs[index-1].Length
    List.removeManyAt index 2 segs
    |> List.updateAt (index-1) {segs[index-1] with Length = newLength}

/// After coalescing a wire the wire ends may no longer be draggable.
/// This function checks this and adds two segments to correct the problem
/// if necessary. The added segments will not alter wire appearance.
/// The transformation is: 
/// BEFORE: 1st seg length x. AFTER: 1st segment length nubLength -> zero-length seg 
/// -> segment length x - nubLength
let makeEndsDraggable (segments: Segment list): Segment list =
    let addNubIfPossible (segments: Segment list) =
        let seg0 = segments[0]
        if abs segments[0].Length > Constants.nubLength + XYPos.epsilon &&
           (segments.Length = 1 || (not <| segments[1].IsZero()))
        then
            let delta = float (sign segments[0].Length) * Constants.nubLength
            let newSeg0 = {seg0 with Length = delta} 
            let newSeg1 = { seg0 with IntersectOrJumpList = []; Length = 0.; Draggable = true; Mode=Auto}
            let newSeg2 = {newSeg1 with Length = seg0.Length - delta}
            newSeg0 :: newSeg1 :: newSeg2 :: segments[1..]
        else
            segments
    segments
    |> addNubIfPossible
    |> (List.rev >> addNubIfPossible >> List.rev)
    |> List.mapi (fun i seg -> {seg with Index = i})

//-------------------------------Segment processing-------------------------------------------//

// Wires are lists of Horizontal and Vertical segments. The orientation switches each segment and
// the first segment in the list has orientation given by Wire.InitOrientation.
// Two parallel joined together segments can be made by having a zero-length segment in the middle.
// After snapping, this case is quite common (and the zero-length segment will be exactly zero).
// This motovates coalescing segments to make longer ones where possible.
// In addition, to make the ends of a wire draggable the two end segments, if not short, need to be 
// made as a short non-draggable 'nub' connected to the port, followed by a parallel, variable-length segment.
// The end three segments are therefore: nub / 0 length / rest of 1st visible segment.
// Finally, the visible segment from a port must emerge outwards. So coalescing a numb 
// (which is always outwards direction) with an opposite direction segment is not allowed.



/// If as the result of a drag a zero length segment separates two other draggable segments
/// the wire should be simplified by removing the zero-length segment and joining together the
/// draggables.
let coalesceInWire (wId: ConnectionId) (model:Model) =
    let wire = model.Wires[wId]
    let segments = wire.Segments
    //printfn $"Before coalesce, seg lengths: {segments |> List.map (fun seg -> seg.Length)}"
    let segmentsToRemove =
        List.indexed segments
        |> List.filter (fun (i,seg) -> 
            segments[i].IsZero() &&
            i > 1 && i < segments.Length - 2 &&
            segments[i-1].Draggable && segments[i+1].Draggable)
        |> List.map (fun (index,_) -> index)
        |> List.sortDescending // needed if more than one segment can be removed - not sure this can happen!
    let newSegments =
        let opposite seg1 seg2 = 
            match sign seg1.Length, sign seg2.Length with
            | 1, -1 | -1, 1 -> true
            | _ -> false            
        (segments, segmentsToRemove)
        ||> List.fold removeZeroSegment
        |> (fun segments' -> 
            if opposite segments[0] segments'[0] 
                || opposite segments[segments.Length-1] segments'[segments'.Length-1] then
                segments
            else   
                segments')
        |> makeEndsDraggable

    //printfn $"After coalesce, seg lengths: {newSegments |> List.map (fun seg -> seg.Length)}"
    Optic.set (wireOf_ wId >-> segments_) newSegments model


/// Returns a wwireOf_aining the updated list of segments after a segment is moved by 
/// a specified distance. The moved segment is tagged as manual so that it is no longer auto-routed.
/// Throws an error if the index of the segment being moved is not a valid movable segment index.
let moveSegment (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.WireId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then // Should never happen
        printfn $"Trying to move wire segment {seg.Index}:{logSegmentId seg}, out of range in wire length {segments.Length}"
        wire
    else
        let safeDistance = getSafeDistanceForMove segments idx distance
    
        let prevSeg = segments[idx - 1]
        let nextSeg = segments[idx + 1]
        let movedSeg = segments[idx]

        let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
        let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
        let newMovedSeg = { movedSeg with Mode = Manual }
    
        let newSegments = 
            segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

        { wire with Segments = newSegments }

//--------------------------------------------------------------------------------//

//------------------------------autoroute--------------------------------------//

/// Contains geometric information of a port
type PortInfo = {
    Edge: Edge
    Position: XYPos
}

/// Returns a PortInfo object given a port edge and position
let inline genPortInfo edge position =
    { Edge = edge; Position = position }

/// Returns an edge rotated 90 degrees anticlockwise
let inline rotate90Edge (edge: Edge) = 
    match edge with
    | CommonTypes.Top -> CommonTypes.Left
    | CommonTypes.Left -> CommonTypes.Bottom
    | CommonTypes.Bottom -> CommonTypes.Right
    | CommonTypes.Right -> CommonTypes.Top

/// Returns a port rotated 90 degrees anticlockwise about the origin
let inline rotate90Port (port: PortInfo) =
    let newEdge = rotate90Edge port.Edge

    let newPos =
        { X = port.Position.Y
          Y = -port.Position.X }

    genPortInfo newEdge newPos

/// Returns a function to rotate a segment list 90 degrees about the origin, 
/// depending on its initial orientation
let rotateSegments90 initialOrientation =
    let horizontal i =
        match initialOrientation with
        | Horizontal -> i % 2 = 0
        | Vertical -> i % 2 = 1

    let rotateSegment (i, seg) =
        if (horizontal i) then
            { seg with Length = -seg.Length }
        else
            seg

    List.indexed
    >> List.map rotateSegment

/// Returns a version of the start and destination ports rotated until the start edge matches the target edge.
let rec rotateStartDest (target: Edge) ((start, dest): PortInfo * PortInfo) = 
    if start.Edge = target then
        (start, dest)
    else
        rotateStartDest target (rotate90Port start, rotate90Port dest)


/// Gets a wire orientation given a port edge
let inline getOrientationOfEdge (edge: Edge) = 
    match edge with
    | CommonTypes.Top | CommonTypes.Bottom -> Vertical
    | CommonTypes.Left | CommonTypes.Right -> Horizontal

/// Returns an anonymous record containing the starting symbol edge of a wire and its segment list that has been 
/// rotated to a target symbol edge.
let rec rotateSegments (target: Edge) (wire: {| edge: Edge; segments: Segment list |}) =
    if wire.edge = target then
        {| edge = wire.edge; segments = wire.segments |}
    else
        let rotatedSegs =
            rotateSegments90 (getOrientationOfEdge wire.edge) wire.segments
        
        {| edge = rotate90Edge wire.edge; segments = rotatedSegs |}
        |> rotateSegments target 

/// Returns a newly autorouted version of a wire for the given model
let autoroute (model: Model) (wire: Wire) : Wire =
    let destPos, startPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    let destEdge =
        Symbol.getInputPortOrientation model.Symbol wire.InputPort

    let startEdge =
        Symbol.getOutputPortOrientation model.Symbol wire.OutputPort

    let startPort = genPortInfo startEdge startPos
    let destPort = genPortInfo destEdge destPos
    
    // Normalise the routing problem to reduce the number of cases in makeInitialSegmentsList
    let normStart, normEnd = 
        rotateStartDest CommonTypes.Right (startPort, destPort)

    let initialSegments =
        makeInitialSegmentsList wire.WId normStart.Position normEnd.Position normEnd.Edge

    let segments =
        {| edge = CommonTypes.Right
           segments = initialSegments |}
        |> rotateSegments startEdge // Rotate the segments back to original orientation
        |> (fun wire -> wire.segments)

    { wire with
          Segments = segments
          InitialOrientation = getOrientationOfEdge startEdge
          StartPos = startPos
    }

//--------------------------------------------------------------------------------//

//------------------------------partialAutoroute--------------------------------------//

/// Returns an anonymous record indicating the position of pos relative to origin.
/// The isAbove field indicates whether pos is above (true) or below (false) origin.
/// The isLeft field indicates whether pos is to the left (true) or to the right (false) of origin.
let relativePosition (origin: XYPos) (pos:XYPos) = 
    {| isLeft = origin.X > pos.X; isAbove = origin.Y > pos.Y |}

/// Returns the tuple (startPos, endPos) of the segment at the target index in the given wire. 
/// Throws an error if the target index isn't found.
let getAbsoluteSegmentPos (wire: Wire) (target: int) =
    (None, wire)
    ||> foldOverSegs
        (fun startPos endPos state seg ->
            if seg.Index = target then Some (startPos, endPos) else state)
    |> (function
        | None -> failwithf $"Couldn't find index {target} in wire"
        | Some pos -> pos)     

/// Returns the length to change a segment represented by startPos -> endPos 
/// in the appropriate dimension of the difference vector.
let getLengthDiff difference startPos endPos =
    match getSegmentOrientation startPos endPos with
    | Horizontal -> toX difference
    | Vertical -> toY difference

/// Given a segment list, returns the first manual segment index
let getManualIndex segList =
    segList
    |> List.tryFind (fun seg -> seg.Mode = Manual)
    |> Option.map (fun seg -> seg.Index)
    |> Option.bind (fun index ->
        if index < 1 || index >= segList.Length - 1 then
            None
        else
            Some index)

/// Gets the start position for partial routing.
let getPartialRouteStart wire manualIndex =
    wire.Segments
    |> List.tryFind (fun seg -> seg.Index = manualIndex - 1)
    |> Option.map (fun seg -> seg.Index)
    |> Option.map (getAbsoluteSegmentPos wire >> fst)
    |> Option.defaultValue wire.StartPos

/// Partitions a segment list into sections 3 sections for partial autorouting
let partitionSegments segs manualIdx =
    let start, tmp =
        match manualIdx with
        | 1 -> ([], segs)
        | _ -> List.splitAt (manualIdx - 1) segs

    let changed, remaining = List.splitAt 2 tmp
    if (start @ changed @ remaining).Length <> segs.Length then 
        printfn $"Bad partial routing partition: index=\
                    {manualIdx}:{start.Length},{changed.Length},{remaining.Length} ({segs.Length})"
    (start, changed, remaining)

/// Returns None if full autoroute is required or applies partial autorouting
/// from the start of the wire at newPortPos to the first manually routed segment 
/// and returns Some wire with the new segments.
let partialAutoroute (model: Model) (wire: Wire) (newPortPos: XYPos) (reversed: bool)= 
    let segs = wire.Segments
    let newWire = { wire with StartPos = newPortPos }

    /// Returns the manual index and change in port position 
    /// if partial routing can be performend, else none
    let eligibleForPartialRouting manualIdx =
        let oldStartPos = getPartialRouteStart wire manualIdx
        let newStartPos = getPartialRouteStart newWire manualIdx
        let fixedPoint = getAbsoluteSegmentPos wire manualIdx |> snd
        let relativeToFixed = relativePosition fixedPoint
        let portId = 
            match reversed with
            | false -> OutputId wire.OutputPort
            | true -> InputId wire.InputPort
        let portOrientation =
            Symbol.getPortOrientation model.Symbol portId
        if  getWireOutgoingEdge wire = portOrientation &&
            relativeToFixed newStartPos = relativeToFixed oldStartPos then
                Some (manualIdx, newStartPos - oldStartPos, portOrientation)
        else
            None
    
    /// Returns the partially routed segment list
    let updateSegments (manualIdx, diff, portOrientation) =
        /// consistency check not needed (does it work?
        let segsRetracePath (segs: Segment list) =
            [1..segs.Length-2]
            |> List.exists (fun i -> 
                    segs[i].IsZero() 
                    && sign segs[i-1].Length <> sign segs[i+1].Length
                    && not (segs[i-1].IsZero())
                    && not (segs[i+1].IsZero()))
        let start, changed, remaining = partitionSegments segs manualIdx
        let changed' = 
            changed
            |> List.map (fun seg -> 
                let (startPos, endPos) = getAbsoluteSegmentPos wire seg.Index
                { seg with Length = seg.Length - getLengthDiff diff startPos endPos })

        start @ changed' @ remaining
        |> (fun segs -> 
            let wire' = {wire with Segments = segs; StartPos = newPortPos}
            //wire'
            match getWireOutgoingEdge wire' = portOrientation  (*|| not (segsRetracePath segs)*) with
            | true -> Some wire'
            | false -> None)
        
    segs
    |> getManualIndex
    |> Option.bind eligibleForPartialRouting
    |> Option.bind updateSegments
    |> Option.map (fun wire -> {wire with Segments = makeEndsDraggable wire.Segments})

//--------------------------------------------------------------------------------//
//--------------------------------updateWire--------------------------------------//
//--------------------------------------------------------------------------------//

/// Reverses a wire so that it may be processed in the opposite direction. This function is self-inverse.
let reverseWire (wire: Wire) =
    let newSegs =
        List.rev wire.Segments
        |> List.indexed // I don't think we need to reverse the indices, test
        |> List.map (fun (i, seg) -> { seg with Length = -seg.Length; Index = i })

    { wire with
        Segments = newSegs
        StartPos = wire.EndPos
        InitialOrientation = wire.EndOrientation
    }



//--------------------------------------------------------------------------------//

/// Moves a wire by the XY amounts specified by displacement
let moveWire (wire: Wire) (displacement: XYPos) =
    { wire with
          StartPos = wire.StartPos + displacement
    }

/// Returns an updated wireMap with the IntersectOrJumpList of targetSeg 
/// replaced by jumps or modern intersections.
let updateSegmentJumpsOrIntersections targetSeg intersectOrJump wireMap =
    let wId = targetSeg.WireId
    let target = targetSeg.Index

    let changeSegment (segs: Segment List) =
        List.updateAt target {targetSeg with IntersectOrJumpList = intersectOrJump } segs

    wireMap
    |> Map.add wId { wireMap[wId] with Segments = changeSegment wireMap[wId].Segments }

let partitionWiresIntoNets (model:Model) =
    model.Wires
    |> Map.toList
    |> List.groupBy (fun (_,wire) -> wire.OutputPort)

/// type used internally by modern wire circle calculation code
/// For a horizontal segment (x1,y) -> (x2,y):
/// P = y, Qmin = min y1 y2, Qmax = max y1 y2
/// For a vertical segment (x,y1) -> (x,y2) x & y are reversed, so P = x etc.
/// TODO - replace Index by Segment.
type SegInfo = {P: float; Qmin: float; Qmax:float; Index: int; OfWire: Wire}

/// get segments on wire partitioned horizontal and vertical. 
/// small length segments are not included, since this is to determine modern circle placement
let getHVSegs (wire : Wire) =
    let isHorizontal (seg:ASegment) =
        let index = seg.Segment.Index
        match wire.InitialOrientation with
        | Horizontal -> index % 2 = 0
        | Vertical -> index % 2 = 1
        

    let makeInfo p q1 q2 (i:int) (seg:ASegment) =
        let qMin = min q1 q2
        let qMax = max q1 q2
        {P=p; Qmin = qMin; Qmax = qMax; Index = seg.Segment.Index; OfWire=wire}

    wire
    |> getAbsSegments
    |> List.filter (fun (seg)-> abs seg.Segment.Length > Constants.modernCirclePositionTolerance)
    |> List.partition isHorizontal
    |> (fun (hSegs,vSegs) ->
        let hInfo = hSegs |> List.map (fun seg-> makeInfo seg.Start.Y seg.Start.X seg.End.X seg.Segment.Index seg)
        let vInfo = vSegs |> List.map (fun seg -> makeInfo seg.Start.X seg.Start.Y seg.End.Y seg.Segment.Index seg)
        hInfo, vInfo)

type CircleT = float * int * Wire

let resetWireJumpsOrIntersections (wire:Wire) =
    let newSegments = 
        wire.Segments
        |> List.map (fun seg -> 
            {seg with IntersectOrJumpList=[]})
    {wire with Segments = newSegments}

let resetModelJumpsOrIntersections (model: Model) : Model =
    //printfn "resetting jumps or intersections"
    let newWires =
        model.Wires
        |> Map.map (fun _ w -> resetWireJumpsOrIntersections w)
    {model  with Wires = newWires}

let updateCirclesOnSegments 
        (wiresToUpdate: Wire list)
        (circles: CircleT list) 
        (model: Model) =

    (model.Wires, wiresToUpdate)
    ||> List.fold (fun wires wire ->
            let wire = wires[wire.WId]
            let findAllCirclesOnWire circles = 
                List.filter (fun ((_,_,wire'): CircleT) -> wire'.WId = wire.WId) circles
            let newWire =
                (wire, findAllCirclesOnWire circles)
                ||> List.fold (fun wire (cPos,cIndex, _) -> 
                        let seg = wire.Segments[cIndex]
                        let seg' = {seg with IntersectOrJumpList = cPos::seg.IntersectOrJumpList}
                        {wire with Segments = List.updateAt cIndex seg' wire.Segments})
            Map.add wire.WId newWire wires)
    |> (fun wires -> {model with Wires = wires})  

let inline samePos (pos1: XYPos) (pos2: XYPos) =
    max (pos1.X-pos2.X) (pos1.Y - pos2.Y) < Constants.modernCirclePositionTolerance

let inline close (a:float) (b:float) = abs (a-b) < Constants.modernCirclePositionTolerance

/// Update all the modern routing circles on the net of wires: wiresInNet
let updateCirclesOnNet 
        (model: Model)
        (wiresInNet: Wire list) : Model =
    let hsL, vsL =
        List.map getHVSegs wiresInNet
        |> List.unzip
        |> fun (a,b) -> List.concat a, List.concat b
    let e = Constants.modernCirclePositionTolerance
    /// A circle can be on a V segment intersection with the middle of an H segment or vice versa.
    /// Note that at most one of the H or V segments can intersect at its end
    let getIntersection (h,v) =
        if inMiddleOf v.Qmin h.P v.Qmax && inMiddleOrEndOf h.Qmin v.P h.Qmax ||
           inMiddleOrEndOf v.Qmin h.P v.Qmax && inMiddleOf h.Qmin v.P h.Qmax then
            [v.P, h.Index, h.OfWire]
        else []
    /// A join is a point where the ends of two H, or two V segments are coincident (to with a tolerance).
    /// A circle must be placed whenever a V segment end coincides with an H segment join or vice versa.
    /// This function is called twice to find joins of Horizontal and vertical segments.
    let getJoins segs =
        List.allPairs segs segs
        |> List.collect (fun (s1, s2) ->
            if close s1.P s2.P && close s1.Qmax s2.Qmin then
                [{|P=(s1.P+s2.P)/2.;Q=(s1.Qmax+s2.Qmin)/2.; Index=s2.Index; Wire=s2.OfWire|}]
            else 
                [])
    /// get all intersections - circles will be placed here
    let intersectCircles =
        List.allPairs hsL vsL
        |> List.collect getIntersection
    /// all horizontal join circles
    let hJoinCircles =
        getJoins hsL
        |> List.collect (fun join ->
            if List.exists (fun vs -> close vs.P join.Q && (close vs.Qmin join.P || close vs.Qmax join.P)) vsL then
                [join.Q,join.Index,join.Wire]
            else 
                [])
    /// all vertical join circles
    let vJoinCircles =
        getJoins vsL
        |> List.collect (fun join ->
            hsL 
            |> List.tryPick (fun hs -> 
                if close hs.P join.Q && (close hs.Qmin join.P || close hs.Qmax join.P) then
                    Some [hs.Qmin, hs.Index, hs.OfWire]
                else 
                    None)
            |> Option.defaultValue [])
    let circles = intersectCircles  @ vJoinCircles @ hJoinCircles
    model
    |> updateCirclesOnSegments wiresInNet circles
/// Update all the modern routing circles in the model                   
let updateCirclesOnAllNets (model:Model) =
    let cleanModel =
        model
        |> resetModelJumpsOrIntersections
    /// A net is a set of electrically connected wires.
    /// For now this is all wires with given port as source
    /// TODO: join nets which are on same wire label.
    let nets = 
        partitionWiresIntoNets cleanModel
        |> List.map snd
        |> List.map (List.map snd)
    (cleanModel,nets)
    ||> List.fold updateCirclesOnNet

/// Used as a folder in foldOverSegs. Finds all jump offsets in a wire for the segment defined in the state
let inline findJumpIntersects 
        (segStart: XYPos) 
        (segEnd: XYPos) 
        (state: {| Start: XYPos; End: XYPos; JumpsOrIntersections: float list |}) 
        (seg: Segment) =
    if getSegmentOrientation segStart segEnd = Vertical then
        let xVStart, xHStart, xHEnd = segStart.X, state.Start.X, state.End.X
        let yVStart, yVEnd, yHEnd = segStart.Y, segEnd.Y, state.End.Y
        let xhi, xlo = max xHStart xHEnd, min xHStart xHEnd
        let yhi, ylo = max yVStart yVEnd, min yVStart yVEnd

        if xVStart < xhi  && xVStart > xlo  && yHEnd < yhi && yHEnd > ylo then
            {| state with JumpsOrIntersections = abs (xVStart - xHStart) :: state.JumpsOrIntersections |}
        else
            state
    else   
        state

/// Returns a model with all the jumps updated
let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =

    let wires =
        model.Wires
        |> Map.toArray
        |> Array.map snd

    let updateJumpsInWire (segStart: XYPos) (segEnd: XYPos) (wireMap: Map<ConnectionId, Wire>) (seg: Segment) =
        if getSegmentOrientation segStart segEnd = Horizontal then
            ([], wires)
            ||> Array.fold (fun jumpsOrIntersections wire -> 
                if (model.Type = Jump)  then
                    foldOverSegs findJumpIntersects {| Start = segStart; End = segEnd; JumpsOrIntersections = [] |} wire
                    |> (fun res -> res.JumpsOrIntersections)
                    |> List.append jumpsOrIntersections
                else 
                    jumpsOrIntersections)
            |> (fun jumpsOrIntersections -> 
                if jumpsOrIntersections <> seg.IntersectOrJumpList then
                    updateSegmentJumpsOrIntersections seg jumpsOrIntersections wireMap
                else 
                    wireMap)
        else
            wireMap

    match model.Type with
    | Jump ->
        let wiresWithJumps = 
            (model.Wires, wires)
            ||> Array.fold (fun map wire ->
                    foldOverSegs updateJumpsInWire map wire)
    
        { model with Wires = wiresWithJumps }
    | Modern ->
        printfn "Updating modern circles"
        updateCirclesOnAllNets model
    | Radial -> 
        model

let updateWireSegmentJumps (wireList: list<ConnectionId>) (model: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] model
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model

let resetJumpsOrIntersections (wire: Wire) =
    let newSegs =
        wire.Segments
        |> List.map (fun seg -> {seg with IntersectOrJumpList = []})
    {wire with Segments = newSegs}

let resetJumps (model:Model) : Model =
        printfn "Reseting jumps or intersections..."
        (model.Wires, model.Wires)
        ||> Map.fold (fun wires wid wire ->
                Map.add wid (resetJumpsOrIntersections wire) wires)
        |> (fun wires -> {model with Wires = wires})

/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (model : Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    resetJumps model
    |> TimeHelpers.instrumentInterval "ResetJumps" startT


