module BusWireUpdate

open CommonTypes
open Elmish
open DrawHelpers

open BusWire

/// Initialises an empty BusWire Model
let init () = 
    let symbols,_ = Symbol.init()
    {   
        Wires = Map.empty;
        Symbol = symbols; 
        CopiedWires = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Type = Jump
    } , Cmd.none

//-------------------------segmentIntersectsBoundingBox---------------------------------//

/// Type used to simplify BoundingBox intersection calculations
type Rectangle = {
    TopLeft: XYPos
    BottomRight: XYPos
}

/// Returns the X-value of an XYPos
let toX (pos: XYPos) = pos.X

/// Returns the Y-value of an XYPos
let toY (pos: XYPos) = pos.Y

/// Returns the X and Y fields of an XYPos as a pair of floats
let getXY (pos: XYPos) = pos.X, pos.Y

/// Returns pos with the X and Y fields scaled by factor (I didn't like the order of parameters for the * operator in XYPos)
let scalePos (factor: float) (pos: XYPos) : XYPos =
    { X = factor * pos.X; Y = factor * pos.Y}

/// Returns true if p1 is less than or equal to p2 (has both smaller X and Y values
let lThanEqualPos (p1: XYPos) (p2: XYPos) : bool =
    p1.X <= p2.X && p1.Y <= p2.Y

/// Returns the dot product of 2 XYPos
let dotProduct (p1: XYPos) (p2: XYPos) : float = 
    p1.X * p2.X + p1.Y * p2.Y

/// Returns the squared distance between 2 points using Pythagoras
let squaredDistance (p1: XYPos) (p2: XYPos) = 
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

/// Checks if a segment intersects a bounding box using the segment's start and end XYPos
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

    rectanglesIntersect bbRect segRect

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

/// Finds the Id of the closest segment in a wire to a mouse click using euclidean distance
let getClickedSegment (model: Model) (wireId: ConnectionId) (mouse: XYPos) : SegmentId =
    let closestSegment segStart segEnd state (seg: Segment) =
        let currDist = 
            distanceBetweenPointAndSegment segStart segEnd mouse
        match state with
        | Some (minId, minDist) ->
            let dist = Option.defaultValue minDist currDist
            if dist < minDist then
                Some (seg.Id, dist)
            else
                Some (minId, minDist)
        | None -> Option.map (fun dist -> (seg.Id, dist)) currDist // Needed to deal with initial state

    match foldOverSegs closestSegment None model.Wires[wireId] with
    | Some (segmentId, _dist) -> segmentId 
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
    |> List.map (fun wire -> wire.Id)

/// Returns a list of wire IDs that meet the given condition
let getFilteredIdList condition wireLst = 
    wireLst
    |> List.filter condition
    |> List.map (fun wire -> wire.Id)

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

/// Returns a wire containing the updated list of segments after a segment is moved by 
/// a specified distance. The moved segment is tagged as manual so that it is no longer auto-routed.
/// Throws an error if the index of the segment being moved is not a valid movable segment index.
let moveSegment (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.HostId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then // Should never happen
        failwithf $"Trying to move wire segment {seg.Index}:{formatSegmentId seg.Id}, out of range in wire length {segments.Length}"

    let safeDistance = getSafeDistanceForMove segments idx distance
    
    let prevSeg = segments[idx - 1]
    let nextSeg = segments[idx + 1]
    let movedSeg = segments[idx]

    let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
    let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
    let newMovedSeg = { movedSeg with Mode = Manual }
    
    let newSegments = segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

    { wire with Segments = newSegments }

//--------------------------------------------------------------------------------//

//------------------------------autoroute--------------------------------------//

/// Contains geometric information of a port
type PortInfo = {
    Edge: Edge
    Position: XYPos
}

/// Returns a PortInfo object given a port edge and position
let genPortInfo edge position =
    { Edge = edge; Position = position }

/// Returns an edge rotated 90 degrees anticlockwise
let rotate90Edge (edge: Edge) = 
    match edge with
    | CommonTypes.Top -> CommonTypes.Left
    | CommonTypes.Left -> CommonTypes.Bottom
    | CommonTypes.Bottom -> CommonTypes.Right
    | CommonTypes.Right -> CommonTypes.Top

/// Returns a port rotated 90 degrees anticlockwise about the origin
let rotate90Port (port: PortInfo) =
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
let getOrientation (edge: Edge) = 
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
            rotateSegments90 (getOrientation wire.edge) wire.segments
        
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
    let normalisedStart, normalisedEnd = 
        rotateStartDest CommonTypes.Right (startPort, destPort)

    let initialSegments =
        makeInitialSegmentsList wire.Id normalisedStart.Position normalisedEnd.Position normalisedEnd.Edge

    let segments =
        {| edge = CommonTypes.Right
           segments = initialSegments |}
        |> rotateSegments startEdge // Rotate the segments back to original orientation
        |> (fun wire -> wire.segments)

    { wire with
          Segments = segments
          InitialOrientation = getOrientation startEdge
          EndOrientation = getOrientation destEdge
          StartPos = startPos
          EndPos = destPos }

//--------------------------------------------------------------------------------//

//------------------------------partialAutoroute--------------------------------------//

/// Returns an anonymous record indicating the position of pos relative to origin.
/// The isAbove field indicates whether pos is above (true) or below (false) origin.
/// The isLeft field indicates whether pos is to the left (true) or to the right (false) of origin.
let relativePosition (origin: XYPos) (pos:XYPos) = 
    {| isLeft = origin.X > pos.X; isAbove = origin.Y > pos.Y |}

/// Returns the tuple (startPos, endPos) of the segment at the target index in the given wire. Throws an error if the target index isn't found
let getAbsoluteSegmentPos (wire: Wire) (target: int) =
    (None, wire)
    ||> foldOverSegs
        (fun startPos endPos state seg ->
            if seg.Index = target then Some (startPos, endPos) else state)
    |> (function
        | None -> failwithf $"Couldn't find index {target} in wire"
        | Some pos -> pos)     

/// Returns the length to change a segment represented by startPos -> endPos in the appropriate dimension of the difference vector
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
    (start, changed, remaining)

/// Returns None if full autoroute is required or applies partial autorouting
/// from the start of the wire at newPortPos to the first manually routed segment 
/// and returns Some wire with the new segments.
let partialAutoroute (model: Model) (wire: Wire) (newPortPos: XYPos) (reversed: bool)= 
    let segs = wire.Segments
    let newWire = { wire with StartPos = newPortPos }

    /// Returns the manual index and change in port position if partial routing can be performend, else none
    let eligibleForPartialRouting manualIdx =
        let oldStartPos = getPartialRouteStart wire manualIdx
        let newStartPos = getPartialRouteStart newWire manualIdx
        let fixedPoint = getAbsoluteSegmentPos wire manualIdx |> snd
        let relativeToFixed = relativePosition fixedPoint
        let portId = 
            match reversed with
            | false -> Symbol.OutputId wire.OutputPort
            | true -> Symbol.InputId wire.InputPort
        if  getWireOutgoingEdge wire = Symbol.getPortOrientation model.Symbol portId &&
            relativeToFixed newStartPos = relativeToFixed oldStartPos then
            Some (manualIdx, newStartPos - oldStartPos)
        else
            None
    
    /// Returns the partially routed segment list
    let updateSegments (manualIdx, diff) =
        let start, changed, remaining = partitionSegments segs manualIdx
        let changed' = 
            changed
            |> List.map (fun seg -> 
                let (startPos, endPos) = getAbsoluteSegmentPos wire seg.Index
                { seg with Length = seg.Length - getLengthDiff diff startPos endPos })

        start @ changed' @ remaining
        
    segs
    |> getManualIndex
    |> Option.bind eligibleForPartialRouting
    |> Option.map updateSegments
    |> Option.map (fun segs -> { wire with Segments = segs; StartPos = newPortPos })

//--------------------------------------------------------------------------------//

//------------------------------updateWire--------------------------------------//

/// Reverses a wire so that it may be processed in the opposite direction. This function is self-inverse.
let reverseWire (wire: Wire) =
    let newSegs =
        List.rev wire.Segments
        |> List.indexed // I don't think we need to reverse the indices, test
        |> List.map (fun (i, seg) -> { seg with Length = -seg.Length; Index = i })

    { wire with
        Segments = newSegs
        StartPos = wire.EndPos
        EndPos = wire.StartPos
        InitialOrientation = wire.EndOrientation
        EndOrientation = wire.InitialOrientation }

/// Returns a re-routed wire from the given model.
/// First attempts partial autorouting, and defaults to full autorouting if this is not possible.
/// Reverse indicates if the wire should be processed in reverse, 
/// used when an input port (end of wire) is moved.
let updateWire (model : Model) (wire : Wire) (reverse : bool) =
    let newPort = 
        match reverse with
        | true -> Symbol.getInputPortLocation None model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation None model.Symbol wire.OutputPort
    if reverse then
        partialAutoroute model (reverseWire wire) newPort true
        |> Option.map reverseWire
    else 
        partialAutoroute model wire newPort false
    |> Option.defaultValue (autoroute model wire)

//--------------------------------------------------------------------------------//

/// Moves a wire by the XY amounts specified by displacement
let moveWire (wire: Wire) (displacement: XYPos) =
    { wire with
          StartPos = wire.StartPos + displacement
          EndPos = wire.EndPos + displacement }

/// Returns an updated wireMap with the IntersectOrJumpList of targetSeg replaced by jumps or modern intersections
let updateSegmentJumpsOrIntersections targetSeg intersectOrJump wireMap =
    let wId = targetSeg.HostId
    let target = targetSeg.Index

    let changeSegment (segs: Segment List) =
        List.map
            (fun seg ->
                if seg.Index <> target then
                    seg
                else
                    { seg with IntersectOrJumpList = intersectOrJump })
            segs

    wireMap
    |> Map.add wId { wireMap[wId] with Segments = changeSegment wireMap[wId].Segments }

/// Used as a folder in foldOverSegs. Finds all Modern offsets in a wire for the segment defined in the state
let findModernIntersects (segStart: XYPos) (segEnd: XYPos) (state: {| Start: XYPos; End: XYPos; JumpsOrIntersections: (float * SegmentId) list |}) (seg: Segment) =
    let x1Start, x1End, x2Start, x2End = segStart.X, segEnd.X, state.Start.X, state.End.X
    let y1Start, y1End, y2Start, y2End = segStart.Y, segEnd.Y, state.Start.Y, state.End.Y
    let x1hi, x1lo = max x1Start x1End, min x1Start x1End
    let x2hi, x2lo = max x2Start x2End, min x2Start x2End
    let y1hi, y1lo = max y1Start y1End, min y1Start y1End
    let y2hi, y2lo = max y2Start y2End, min y2Start y2End
    if getSegmentOrientation segStart segEnd = Vertical then
        if y2Start < y1hi  && y2Start > y1lo  && (x2Start > x1hi - 0.1 && x2Start < x1hi + 0.1) then
            {| state with JumpsOrIntersections = (abs(0.0), seg.Id) :: state.JumpsOrIntersections |}
        else if y2End < y1hi  && y2End > y1lo  && (x2End > x1hi - 0.1 && x2End < x1hi + 0.1) then
            {| state with JumpsOrIntersections = (abs(x2End - x2Start), seg.Id) :: state.JumpsOrIntersections |}
        else
            state
    else
        if (y2Start > y1hi - 0.1 && y2Start < y1hi + 0.1) && x1hi > x2hi && x1lo < x2lo then
            {| state with JumpsOrIntersections = (abs(0.0), seg.Id) :: (abs(x2End - x2Start), seg.Id) :: state.JumpsOrIntersections |}
        else if (y2Start > y1hi - 0.1 && y2Start < y1hi + 0.1) && x2hi > x1hi && x1hi > x2lo && x2lo > x1lo then
             {| state with JumpsOrIntersections = (abs(0.0), seg.Id) :: (abs(x1hi - x2lo), seg.Id) :: state.JumpsOrIntersections |}
        else
            state

/// Used as a folder in foldOverSegs. Finds all jump offsets in a wire for the segment defined in the state
let inline findJumpIntersects (segStart: XYPos) (segEnd: XYPos) (state: {| Start: XYPos; End: XYPos; JumpsOrIntersections: (float * SegmentId) list |}) (seg: Segment) =
    if getSegmentOrientation segStart segEnd = Vertical then
        let xVStart, xHStart, xHEnd = segStart.X, state.Start.X, state.End.X
        let yVStart, yVEnd, yHEnd = segStart.Y, segEnd.Y, state.End.Y
        let xhi, xlo = max xHStart xHEnd, min xHStart xHEnd
        let yhi, ylo = max yVStart yVEnd, min yVStart yVEnd

        if xVStart < xhi  && xVStart > xlo  && yHEnd < yhi && yHEnd > ylo then
            {| state with JumpsOrIntersections = (abs(xVStart - xHStart), seg.Id) :: state.JumpsOrIntersections |}
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
                else if (model.Type = Modern)  then
                    foldOverSegs findModernIntersects {| Start = segStart; End = segEnd; JumpsOrIntersections = [] |} wire
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

    let wiresWithJumps = 
        (model.Wires, wires)
        ||> Array.fold (fun map wire ->
                foldOverSegs updateJumpsInWire map wire)
    
    { model with Wires = wiresWithJumps }

let updateWireSegmentJumps (wireList: list<ConnectionId>) (model: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] model
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model

/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (model : Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    makeAllJumps wireList model
    |> TimeHelpers.instrumentInterval "ResetJumps" startT


/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let wires = filterWiresByCompMoved model compIdList

    let newWires =
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId wires.Both //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId wires.Inputs //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId wires.Outputs
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList

    { model with Wires = newWires }

let updateSymbolWires (model: Model) (compId: ComponentId) =
    let wires = filterWiresByCompMoved model [compId]
    let newWires =
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) ->
            if List.contains cId wires.Both then // Update wires that are connected on both sides
                cId, (
                    updateWire model wire true 
                    |> fun wire -> updateWire model wire false)
            elif List.contains cId wires.Inputs then 
                cId, updateWire model wire true
            elif List.contains cId wires.Outputs then
                cId, updateWire model wire false
            else cId, wire)
        |> Map.ofList
    { model with Wires = newWires }
            


/// Handles messages
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =

    match msg with
    | Symbol sMsg ->
        // update Symbol model with a Symbol message
        let sm,sCmd = SymbolUpdate.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) ->
        // update wires after moving components in componentIdList by diff
        // wires between components are translated not routed as optimisation
        updateWires model componentIdList diff, Cmd.none

    | UpdateSymbolWires compId ->
        // update all the wires coming from a single symbol
        // useful if the symbol has been flipped or ports have been moved
        // partial routing will be done if this makes sense
        updateSymbolWires model compId, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        // add a newly created wire to the model
        // then send BusWidths message which will re-infer bus widths
        // the new wires (extarcted as connections) are not added back into Issie model. 
        // This happens on save or when starting a simulation (I think)
        let wireId = ConnectionId(JSHelpers.uuid())
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = []
                StartPos = { X = 0; Y = 0 }
                EndPos = { X = 0; Y = 0 }
                InitialOrientation = Horizontal
                EndOrientation = Horizontal
            }
            |> autoroute model
        
        let wireAddedMap = Map.add newWire.Id newWire model.Wires
        let newModel = updateWireSegmentJumps [wireId] {model with Wires = wireAddedMap}
        
        newModel, Cmd.ofMsg BusWidths
    
    | BusWidths ->
        // (1) Call Issie bus inference
        // (2) Add widths to maps on symbols on wires
        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = 
                    if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Component.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with
                        | Some 0 ->
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 ->
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWires = ((Map.empty, model.Wires) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWires) ||> Map.fold addSymbolWidthFolder

            { model with
                Wires = newWires; 
                Notifications = None;
                ErrorWires=[];
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}
            }, Cmd.none

        let canvasState = (SymbolUpdate.extractComponents model.Symbol, extractConnections model)

        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)

    | CopyWires (connIds : list<ConnectionId>) ->
        // add given wires to Copiedwires state (NB, this contains wires at time of copy)
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.Wires
        { model with CopiedWires = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) ->
        // record these wires in model.ErrorWires and highlight them as red.
        // reset the wires that were remobed from model.ErrorWires dark grey 
        // (what if they are supposed to be something else?? Colors carry too muhc state!)
        let newWires =
            model.Wires
            |> Map.map
                (fun id wire ->
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                )

        { model with Wires = newWires ; ErrorWires = connectionIds }, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> 
        // selects all wires in connectionIds, and also deselects all other wires
        let newWires =
            model.Wires
            |> Map.map
                (fun id wire ->
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then
                            {wire with Color = HighLightColor.Brown}
                        else
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple}
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey}
                )

        { model with Wires = newWires }, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) ->
        // deletes wires from model, then runs bus inference
        // Issie model is not affected but will extract connections from wires
        // at some time.
        let newWires =
             model.Wires
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        let model =
            {model with Wires = newWires}
        { model with Wires = newWires }, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let seg = 
                model.Wires[connId].Segments
                |> List.tryFind (fun seg -> model.SelectedSegment = seg.Id) 
                |> function | Some seg -> seg | None -> failwithf $"Segment Id {model.SelectedSegment} not found in segment list"
            if seg.Draggable then
                let distanceToMove = 
                    let (segStart, segEnd) = getAbsoluteSegmentPos model.Wires[connId] seg.Index
                    match getSegmentOrientation segStart segEnd with
                    | Horizontal -> mMsg.Pos.Y - segStart.Y
                    | Vertical -> mMsg.Pos.X - segStart.X

                let newWire = moveSegment model seg distanceToMove 
                let newWires = Map.add seg.HostId newWire model.Wires

                { model with Wires = newWires }, Cmd.none
            else
                model, Cmd.none

        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> 
        // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId ->
                let oldWireOpt = Map.tryFind cId model.Wires
                match oldWireOpt with
                | None ->
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.Wires connIds)
        { model with Wires = newWires }, Cmd.none

    | ResetJumps connIds ->
        // removes wire 'jumps' at start of drag operation for neater component movement 
        // without jump recalculation
        // makejumps at end of a drag operation restores new jumps
        let newModel =
            model
            |> resetWireSegmentJumps connIds

        newModel, Cmd.none

    | MakeJumps connIds ->
        // recalculates (slowly) wire jumps after a drag operation
        let newModel =
            model
            |> updateWireSegmentJumps connIds

        newModel, Cmd.none

    | ResetModel -> 
        // How we start with nothing loaded
        { model with Wires = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none

    | LoadConnections conns -> 
        // we assume components (and hence ports) are loaded before connections
        // Issie connections are loaded as wires
        // vertices on Issie connections contains routing info so wires can be 
        // reconstructed precisely
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (pos.X - (fst vertex)) < epsilon &&
            abs (pos.Y - (snd vertex)) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWires =
            conns
            |> List.map ( fun conn ->
                let inputId = InputPortId conn.Target.Id
                let outputId = OutputPortId conn.Source.Id
                let connId = ConnectionId conn.Id
                let getVertex (x,y,_) = (x,y)
                let segments = issieVerticesToSegments connId conn.Vertices
                let makeWirePosMatchSymbol inOut (wire:Wire) =
                    match inOut with
                    | true -> 
                        posMatchesVertex
                                (Symbol.getInputPortLocation None model.Symbol inputId)
                                (List.last conn.Vertices |> getVertex)
                    | false ->
                        posMatchesVertex
                            (Symbol.getOutputPortLocation None model.Symbol outputId)
                            (List.head conn.Vertices |> getVertex)
                    |> (fun b ->
                        if b then
                            wire
                        else
                            let getS (connId:string) =
                                Map.tryFind connId model.Symbol.Ports
                                |> Option.map (fun port -> port.HostId)
                                |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                |> Option.map (fun sym -> sym.Component.Label)
                            //printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                            updateWire model wire inOut)
                connId,
                { 
                    Id = ConnectionId conn.Id
                    InputPort = inputId
                    OutputPort = outputId
                    Color = HighLightColor.DarkSlateGrey
                    Width = 1
                    Segments = segments
                    StartPos = Symbol.getOutputPortLocation None model.Symbol outputId
                    EndPos = Symbol.getInputPortLocation None model.Symbol inputId
                    InitialOrientation = Symbol.getOutputPortOrientation model.Symbol outputId |> getOrientation
                    EndOrientation = Symbol.getInputPortOrientation model.Symbol inputId |> getOrientation }
                |> makeWirePosMatchSymbol false
                |> makeWirePosMatchSymbol true
            )
            |> Map.ofList

        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)

        { model with Wires = newWires }, Cmd.ofMsg (MakeJumps connIds)

    | UpdateWireDisplayType (style: WireType) ->
        // changes wire display
        { model with Type = style }, Cmd.none

    | UpdateConnectedWires (componentIds: ComponentId list) ->
        // partial or full autoroutes all ends of wires conencted to given symbols
        // typically used after rotating or flipping symbols
        let updatePortIdMessages = 
            componentIds
            |> Symbol.getPortLocations model.Symbol
            |> (fun (m1,m2) -> 
                let inputPorts = Seq.map (fun (InputPortId portId) -> portId) m1.Keys |> Seq.toList
                let outputPorts = Seq.map (fun (OutputPortId portId) -> portId) m2.Keys |> Seq.toList
                inputPorts @ outputPorts
                |> List.map (Msg.RerouteWire >> Cmd.ofMsg))
        model, Cmd.batch updatePortIdMessages

    | RerouteWire (portId: string) ->
        // parially or fully autoroutes wires connected to port
        // typically used after port has moved
        // NB if direction of port has changed wire must be autorouted.
        let portOpt = Map.tryFind portId model.Symbol.Ports 

        let rerouteInputEnd (wire:Wire) = 
            wire.InputPort = InputPortId portId
        
        let wiresToReroute = 
            model.Wires
            |> Map.filter (fun _id wire -> 
                wire.InputPort = InputPortId portId  || wire.OutputPort = OutputPortId portId)
            |> Map.toList

        let newWires =
            (model.Wires, wiresToReroute)
            ||> List.fold (fun wires (wid, wire) ->
                let wire' = updateWire model wire (rerouteInputEnd wire)
                Map.add wid wire' wires)

        {model with Wires = newWires}, Cmd.none

        

//---------------Other interface functions--------------------//

/// Checks if a wire intersects a bounding box by checking if any of its segments intersect
let wireIntersectsBoundingBox (wire : Wire) (box : BoundingBox) =
    let segmentIntersectsBox segStart segEnd state seg =
        match state with
        | true -> true
        | false -> segmentIntersectsBoundingBox box segStart segEnd
    
    foldOverSegs segmentIntersectsBox false wire

/// Returns a list of wire IDs in the model that intersect the given selectBox
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> =
    wModel.Wires
    |> Map.map (fun _id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun _id bool -> bool)
    |> Map.toList
    |> List.map (fun (id, _bool) -> id)

///Searches if the position of the cursor is on a wire in a model,
///where n is 5 pixels adjusted for top level zoom
let getClickedWire (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.TopLeft = {X = pos.X - n; Y = pos.Y - n}; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

/// Updates the model to have new wires between pasted components
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())

            match SymbolUpdate.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId portOnePos portTwoPos (Symbol.getOutputPortOrientation wModel.Symbol (OutputPortId newOutputPort))
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                            StartPos = portOnePos;
                            EndPos = portTwoPos
                    }
                    |> autoroute wModel
                ]
            | None -> []

        wModel.CopiedWires
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList

    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.Wires
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst

    { wModel with Wires = newWireMap }, pastedConnIds


