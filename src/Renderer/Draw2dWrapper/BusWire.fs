module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open EEEHelpers

//Static Vars
let minSegLen = 5.

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

///
type Orientation =  Horizontal | Vertical

///
type SnapPosition = High | Mid | Low

///
type Segment = 
    {
        Id : SegmentId
        Index: int
        Start: XYPos
        End: XYPos
        Dir: Orientation
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: list<float * SegmentId>
        Draggable : bool
    }



///
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
    }

    with static member stickLength = 16.0



///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<(ConnectionId * SegmentId), list<ConnectionId * SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<(ConnectionId * SegmentId), list<ConnectionId * SegmentId>>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration

/// Wire to Connection
let segmentsToVertices (segList:Segment list) = 
    let firstCoord = (segList.[0].Start.X, segList.[0].Start.Y)
    let verticesExceptFirst = List.mapi (fun i seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialWireVerticesList (portCoords : XYPos * XYPos)  = 
    let xs, ys, Xt, Yt = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let adjStick = 
        let d = List.max [ abs (xs - Xt) ; abs (ys - Yt) ; Wire.stickLength / 4.0 ]
        if (Xt - xs > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    // the simple case of a wire travelling from output to input in a left-to-right (positive X) direction
    let leftToRight = 
        [
            {X = xs; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = (xs+Xt)/2.0; Y = ys};
            {X = (xs+Xt)/2.0; Y = Yt};
            {X = Xt-adjStick; Y = Yt}
            {X = Xt-adjStick; Y = Yt}
            {X = Xt; Y = Yt}
        ]
    // the case of a wire travelling from output to input in a right-to-left (negative X) direction. Thus must bend back on itself.
    let rightToLeft =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    // the special case of a wire travelling right-to-left where the two ends are vertically almost identical. 
    // In this case we ad an offset to the main horizontal segment so it is more visible and can be easily re-routed manually.
    let rightToLeftHorizontal =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    if (xs-Xt) <= - (adjStick * 2.0) then 
        leftToRight, true
    elif abs (ys - Yt) < 4.0 then 
        rightToLeftHorizontal, false
    else 
        rightToLeft, false 

let inferDirectionfromVertices (xyVerticesList: XYPos list) =
    if xyVerticesList.Length <> 8 then 
        failwithf $"Can't perform connection type inference except with 8 vertices: here given {xyVerticesList.Length} vertices"
    let getDir (vs:XYPos) (ve:XYPos) =
        match sign ((abs vs.X - abs ve.X)*(abs vs.X - abs ve.X) - (abs vs.Y - abs ve.Y)*(abs vs.Y - abs ve.Y)) with
        | 1 -> Some Horizontal
        | -1 -> Some Vertical
        | _ -> None
    let midS, midE = xyVerticesList.[3], xyVerticesList.[4]
    let first,last = xyVerticesList.[1], xyVerticesList.[5]
    let xDelta = abs last.X - abs first.X
    match getDir midS midE, abs xDelta > 20.0, xDelta > 0.0 with
    | Some Horizontal, _, _ when midE.X < midS.X -> Some Horizontal
    | Some Vertical, _, _ -> Some Vertical 
    | _, true, true -> Some Vertical
    | _, true, false -> Some Horizontal
    | _, false, _ -> None

/// this turns a list of vertices into a list of segments
let xyVerticesToSegments connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =

    let dirs = 
        match isLeftToRight with
        | true -> 
            // for 5 adjustable segments left-to-right
            [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
        | false ->
            // for 3 adjustale segments right-to-left
            [Horizontal;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Horizontal]

    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(uuid())
                Index = i
                Start = {X=startX;Y=startY};
                End = {X=endX;Y=endY};
                Dir = dirs.[i]
                HostId  = connId;
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
            })

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        makeInitialWireVerticesList (xyList.[0], xyList.[xyList.Length - 1])
        |> (fun (vl, isLeftToRight) -> xyVerticesToSegments connId isLeftToRight vl)
        

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
        //vertex lists are one element longer than segment lists
    if xyVerticesList.Length <> 8 then  
        makeSegmentsFromVertices xyVerticesList
    else 
        match inferDirectionfromVertices xyVerticesList with
        | Some Vertical -> 
            printfn "Converting vertical"
            xyVerticesToSegments connId true xyVerticesList
        | Some Horizontal -> 
            printfn "Converting horizontal"
            xyVerticesToSegments connId false xyVerticesList
        | _ ->
            // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeSegmentsFromVertices xyVerticesList
            

    
//----------------------interface to Issie-----------------------//
/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.WX.[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = segmentsToVertices conn.Segments
    } // We don't use vertices

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
/// Given three points p, q, r, the function returns:
/// - 0 if p, q and r are colinear;
/// - 1 if the path that you must follow when you start at p, you visit q and you end at r, is a CLOCKWISE path;
/// - 2 if the path that you must follow when you start at p, you visit q and you end at r, is a COUNTERCLOCKWISE path.
let orientation (p : XYPos) (q : XYPos) (r : XYPos) : int =
    let result = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
  
    if (result = 0.0) then 0 // colinear
    elif (result > 0.0) then 1 // clockwise
    else 2 //counterclockwise

  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : bool =

    // Find the four orientations needed for general and 
    // special cases 
    let o1 = orientation (p1) (q1) (p2)
    let o2 = orientation (p1) (q1) (q2)
    let o3 = orientation (p2) (q2) (p1)
    let o4 = orientation (p2) (q2) (q1)
  
    // General case 
    if (o1 <> o2 && o3 <> o4)
        then true

    // Special Cases 
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1 
    elif (o1 = 0 && onSegment (p1) (p2) (q1))
        then true
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1 
    elif (o2 = 0 && onSegment (p1) (q2) (q1))
        then true
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2 
    elif (o3 = 0 && onSegment (p2) (p1) (q2))
        then true
  
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2 
    elif (o4 = 0 && onSegment (p2) (q1) (q2))
        then true
    else false

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}

///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start
        End = getAbsXY seg.End }

/// Given two coordinates, this function returns the euclidean
/// distance between them.
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    sqrt ( (pos1.X - pos2.X)*(pos1.X - pos2.X) + (pos1.Y - pos2.Y)*(pos1.Y - pos2.Y) )


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) : list<Segment> =
    let xyPairs, isLeftToRight = makeInitialWireVerticesList portCoords
    xyPairs
    |> xyVerticesToSegments hostId isLeftToRight






/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderSegment (segment : Segment) (colour : string) (width : string) : ReactElement = 
    let renderWidth = if width = "1" then 1.5 else 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }

    if segment.Dir = Horizontal then
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

        let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) : list<ReactElement> =
            let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        
        let segmentJumpHorizontalSize = 9.0
        let segmentJumpVerticalSize = 6.0
        
        let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
            let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

            let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
            let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

            makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
            ::
            makeCircle startingPoint.X startingPoint.Y circleParameters
            ::
            [
                makeCircle endingPoint.X endingPoint.Y circleParameters
            ]
        
        let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
            
            match segmentJumpCoordinateList with

            | [] -> []


            | [singleElement] ->
                renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}


            | firstElement :: secondElement :: tailList ->

                if (segment.Start.X > segment.End.X) then
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                
                else
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
            

        let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
            
            let jumpCoordinateList =
                if (segment.Start.X > segment.End.X) then
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sortDescending
                    
                else
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sort
            
            match jumpCoordinateList with
                | [] -> renderWireSubSegment seg.Start seg.End

                | lst ->
                     let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let firstSegmentJumpCoordinate = lst.[0]
                     let lastSegmentJumpCoordinate = lst.[(List.length lst) - 1]

                     if (segment.Start.X > segment.End.X) then
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} seg.End

                     else
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} seg.End
        

        let wireSegmentReactElementList = segment
                                          |> completeWireSegmentRenderFunction

        g[] wireSegmentReactElementList
    
    else
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segment.End.X, segment.End.Y
        let segmentElements = 
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        g[] segmentElements


/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
let segmentIntersectsSegmentCoordinates ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : Option<XYPos> =
    
    if (segmentIntersectsSegment (p1, q1) (p2, q2)) then
        let x1, y1, x2, y2 = p1.X, p1.Y, q1.X, q1.Y
        let x3, y3, x4, y4 = p2.X, p2.Y, q2.X, q2.Y
        let uA = ((x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)) / ((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1))

        let intersectionX = x1 + (uA * (x2-x1)) // if coordinates are wanted, maybe useful later
        let intersectionY = y1 + (uA * (y2-y1))
        Some {X = intersectionX; Y = intersectionY}
    
    else None

/// This funtion is given a bounding box and it returns the coordinates
/// of the top-left and the bottom-right corners of this bounding box.
let getTopLeftAndBottomRightCorner (box : BoundingBox) : XYPos * XYPos = 
    let {BoundingBox.X = x; BoundingBox.Y = y} = box
    let {BoundingBox.H = h; BoundingBox.W = w} = box
    let coords = [(x, y); (x, y+h); (x+w, y); (x+w, y+h)]
    let topLeft = List.min coords
    let bottomRight = List.max coords

    {X = fst(topLeft) ; Y = snd(topLeft)} , {X = fst(bottomRight) ; Y = snd(bottomRight)}

/// This function is given a Segment and a BoundingBox
/// and it returns:
/// - (false, None) if the segment does not intersect the bounding box
/// - (true, None) if the segment is fully included inside the bounding box
/// - (true, Some coordinate)  if the segment intersects the bounding box
let segmentIntersectsBoundingBoxCoordinates (segIn : Segment) (bb : BoundingBox) : bool * Option<XYPos> =
    let seg = makeSegPos segIn
    let ({X = x; Y = y} : XYPos), ({X = a; Y = b} : XYPos) = getTopLeftAndBottomRightCorner bb
    let w , h = (a-x), (b-y) // a = x+w;  b = y+h
    let x1, y1, x2, y2 = seg.Start.X, seg.Start.Y, seg.End.X, seg.End.Y 

    let segPointInBox =
        (
            ( (x1 > x) && (x1 < (x+w)) ) && ( (y1 > y) && (y1 < (y+h)) )
        )
        ||
        (
            ( (x2 > x) && (x2 < (x+w)) ) && ( (y2 > y) && (y2 < (y+h)) )
        )

    let left = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y}, {X=x; Y=y+h})
    let right = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x+w; Y=y}, {X=x+w; Y=y+h})
    let top = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y}, {X=x+w; Y=y})
    let bottom = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y+h}, {X=x+w; Y=y+h})
    
    let (intersectionList : list<XYPos>) = 
        [top; bottom; left; right]
        |> List.choose id

    if intersectionList.Length = 0 then
        if segPointInBox then
            true, None
        else
            false, None
    else
        let intersection = 
            intersectionList
            |> List.head
        true, Some intersection

/// This distance is given a point and a segment
/// and it returns the distance between them.
let distanceFromPointToSegment (point : XYPos) (segment : Segment) : float = 
    let x0, y0 = point.X, point.Y
    let x1, y1, x2, y2 = segment.Start.X, segment.Start.Y, segment.End.X, segment.End.Y

    if (x1 = x2) then abs (x1 - x0)
    elif (y1 = y2) then abs (y1 - y0)
    else
        let numer = abs (  (x2-x1)*(y1-y0) - (x1-x0)*(y2-y1)  )
        let denom = sqrt (  (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)  )
        numer/denom

/// This function takes the current state of the model and the
/// IDs of the wires to be rerouted (i.e. updated) as inputs,
/// it REROUTES ALL THE GIVEN WIRES using the default wire
/// shapes defined and it returns the model updated.
let routeGivenWiresBasedOnPortPositions (wiresToBeRouted : list<ConnectionId>) (model : Model) : Model = 
    let updatedWireMap = 
        wiresToBeRouted
        |> List.map (fun id -> model.WX.[id])
        |> List.map
            (
                fun wire -> 
                    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
                    (wire.Id, {wire with Segments = makeInitialSegmentsList wire.Id posTuple})
            )
        |> Map.ofList
    
    let newWX = 
        model.WX
        |> Map.map (fun id wire -> if Map.containsKey id updatedWireMap then updatedWireMap.[id] else wire)

    {model with WX = newWX}

/// Given the current state of the BusWire model,
/// a ConnectionId and an BoundingBox,
/// this function returns a list of Segments of the
/// wire corresponding to the given id that intersect the bounding box.
let getIntersectingSegments (model:Model) (wireId:ConnectionId) (selectBox:BoundingBox) : list<Segment> =     
    model.WX.[wireId].Segments
    |> List.filter (fun seg -> fst(segmentIntersectsBoundingBoxCoordinates seg selectBox))


//Finds the closest segment in a wire to a point using euclidean distance
let getClosestSegment (model : Model) (wireId : ConnectionId) (pos : XYPos) : Segment =
    model.WX.[wireId].Segments
    |> List.minBy (
        fun seg -> 
            distanceFromPointToSegment pos seg)

/// Function called when a wire has been clicked, so no need to be an option
let getClickedSegment (model:Model) (wireId: ConnectionId) (pos: XYPos) : SegmentId =
    let boundingBox = {X = pos.X - 5.0; Y = pos.Y - 5.0; H = 10.0; W = 10.0}
    let intersectingSegments = getIntersectingSegments model wireId boundingBox

    //getIntersecting segments may not return anything at low resolutions as the mouse was not on any segment, but in range of the wire bbox
    //In this case just return the segment closest to mouse position
    //TODO - should it just do this anyway?
    if List.isEmpty intersectingSegments 
    then (getClosestSegment model wireId pos).Id
    else (List.head intersectingSegments).Id

let checkSegmentAngle (seg:Segment) (name:string) =
    match seg.Dir with
    | Vertical -> abs (abs seg.Start.X - abs seg.End.X) < 0.000001
    | Horizontal -> abs (abs seg.Start.Y - abs seg.End.Y) < 0.000001
    |> (fun ok ->
        if not ok then  
            printfn $"Weird segment '{name}':\n{seg}\n\n fails angle checking")

let segPointsLeft seg =
    abs seg.Start.X > abs seg.End.X && seg.Dir = Horizontal

let segXDelta seg = abs seg.End.X - abs seg.Start.X

/// change the middle X coordinate of the joined ends of two segments (seg0 is LH, seg1 is RH).
/// compensate for negative signs in coordinates using as value but preserving sign
/// xPos is asumed positive
let moveXJoinPos xPos seg0 seg1 =
    let changeXKeepingSign (coord:XYPos) =
        if coord.X < 0.0 then {coord with X = -xPos}
        else {coord with X = xPos}
    [ {seg0 with End = changeXKeepingSign seg0.End}; {seg1 with Start = changeXKeepingSign seg1.Start} ]

let changeLengths isAtEnd seg0 seg1 =
    let outerSeg, innerSeg =
        if isAtEnd then seg1, seg0 else seg0, seg1
    let innerX = segXDelta innerSeg
    let outerX = segXDelta outerSeg

    // should never happen, can't do anything
    if seg0.Dir <> Horizontal || seg1.Dir <> Horizontal || outerX < 0.0 then [seg0 ; seg1]
    elif innerX < 0.0 then  
        // the case where we need to shorten the first or last segment (seg0 here)
        moveXJoinPos (if isAtEnd then seg1.End.X - Wire.stickLength else seg0.Start.X + Wire.stickLength) seg0 seg1
    else [ seg0; seg1]
       

/// Called for segments 1, 2, 3, 4, 5 - if they are vertical and move horizontally.
/// The function returns distance reduced if need be to prevent wires moving into components
/// approx equality test is safer tehn exact equality - but probably not needed.
let getSafeDistanceForMove (seg: Segment) (seg0:Segment) (seg6:Segment) (distance:float) =
    let shrink = match seg.Index with | 1 | 2 | 4 | 5 -> 0.5 | _ -> 1.0
    match seg.Index with
    | _ when seg.Dir = Horizontal ->
        distance
    | 3 when distance < 0.0 && abs (abs seg0.Start.Y - abs seg.Start.Y) > 0.0001 ->
        distance
    | 3 when distance > 0.0 && abs (abs seg6.Start.Y - abs seg.End.Y) > 0.0001 ->
        distance
    | 1 | 2 -> 
        let minDistance = seg0.Start.X + Wire.stickLength * shrink - abs seg.End.X
        max minDistance distance
    | 4 | 5 ->
        let maxDistance = seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        min maxDistance distance
    | 3 ->
        let minDistance = abs seg0.Start.X + Wire.stickLength * shrink - abs seg.Start.X
        let maxDistance = abs seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        distance
        |> max minDistance
        |> min maxDistance        
        
    | _ -> 
        distance

        
/// Adjust wire so that two adjacent horizontal segments that are in opposite directions
/// get eliminated
let removeRedundantSegments  (segs: Segment list) =
    let setAbsX x (pos: XYPos) =
        let x = if pos.X < 0.0 then - abs x else abs x
        {pos with X = x}
    let xDelta seg = abs seg.End.X - abs seg.Start.X
    let setStartX x (seg:Segment) = {seg with Start = setAbsX x seg.Start}
    let setEndX x (seg:Segment) = {seg with End = setAbsX x seg.End}
    let adjust seg1 seg2 =
        let xd1, xd2 = xDelta seg1, xDelta seg2
        if seg1.Dir = Horizontal && 
           seg2.Dir = Horizontal && 
           sign xd1 <> sign xd2 
        then
            if abs xd1 > abs xd2 then
                [setEndX seg2.End.X seg1; setStartX seg2.End.X seg2]
            else
                [setEndX seg1.Start.X seg1; setStartX seg1.End.X seg2]
        else
            [seg1;seg2]
    adjust segs.[0] segs.[1] @  segs.[2..4] @ adjust segs.[5] segs.[6]





        

/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by negating one of its coordinates so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:Segment) (distance:float) (model:Model) = 
    let wire = model.WX.[seg.HostId]
    let index = seg.Index
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wire.Segments.Length}"
    let prevSeg = wire.Segments.[index-1]
    let nextSeg = wire.Segments.[index+1]
    if seg.Dir = prevSeg.Dir || seg.Dir = nextSeg.Dir then
        wire
    else
        //runTestFable()
        distance      
        |> getSafeDistanceForMove seg wire.Segments.[0] wire.Segments.[6]   
        |> (fun distance' ->
            let newPrevEnd, newSegStart, newSegEnd, newNextStart = 
                match seg.Dir with

                | Vertical -> 
                    {prevSeg.End with X = - (abs seg.Start.X + distance')}, 
                    {seg.Start with X = - (abs seg.Start.X + distance')}, 
                    {seg.End with X = - (abs seg.End.X + distance')}, 
                    {nextSeg.Start with X = - (abs seg.End.X + distance')}

                | Horizontal -> 
                    {prevSeg.End with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.Start with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.End with Y = - (abs seg.End.Y + distance')}, 
                    {nextSeg.Start with Y = - (abs seg.End.Y + distance')}

            let newPrevSeg = {prevSeg with End = newPrevEnd}
            let newSeg = {seg with Start = newSegStart;End = newSegEnd}
            let newNextSeg = {nextSeg with Start = newNextStart}
        
            let newSegments =
                wire.Segments.[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wire.Segments.[index+2 ..]
                |> removeRedundantSegments

            {wire with Segments = newSegments})

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }


let mutable cache:Map<string,WireRenderProps*ReactElement> = Map.empty

let memoOf (f: WireRenderProps -> ReactElement, _, _) =
    (fun props ->
        match Map.tryFind props.key cache with
        | None -> 
            let re = f props
            cache <- Map.add props.key (props,re) cache 
            re
        | Some (props',re) ->  
            if props' = props then re else
                let re = f props
                cache <- Map.add props.key (props,re) cache
                re)


let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                props.Segments
                |> List.map
                    (
                        fun (segment : Segment) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP)
                            //call a bunch of render helper functions to render the segment (*** DO NOT FORGET SEGMENT JUMPS ***)
                    )
            
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

///
let MapToSortedList map : Wire list = 
    let listSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Blue) map
        |> Map.toList
        |> List.map snd

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied

let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = Helpers.getTimeMs()
    let wires1 =
        model.WX
        |> MapToSortedList
    let rStart = Helpers.getTimeMs()
    let wires =
        wires1
        |> List.toArray
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getOnePortLocation model.Symbol stringOutId PortType.Output
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeSegPos wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props)
    Helpers.instrumentInterval "WireRender" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> Helpers.instrumentInterval "WireView" start

/// Returns a list of all the intersection coordinates between the given
/// segment and all other segments in the model, along with the HostId and
/// the index of the horizontal intersecting segment inside Wire's Segment list.
/// If no intersection exists then it returns an empty list.
let segmentIntersectionCoordinatesWithAllOtherSegments (wModel : Model) (segmentIn : Segment) : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos> = 
    let segment = makeSegPos segmentIn
    wModel.WX
    |> Map.filter (fun wireId wire -> wireId <> segment.HostId)
    |> Map.toList
    |> List.collect (fun (wireId, wire) -> wire.Segments)
    |> List.filter
        (
            fun segIn ->
                let seg = makeSegPos segIn
                (seg.Dir <> segment.Dir) &&
                (distanceBetweenTwoPoints (seg.Start) (seg.End) >= 0.1) &&
                (wModel.WX.[seg.HostId].OutputPort <> wModel.WX.[segment.HostId].OutputPort) &&
                (wModel.WX.[seg.HostId].InputPort <> wModel.WX.[segment.HostId].InputPort) // Only interested in segments of different orientations
        )
    |> List.map
        (
            fun segIn ->
                let seg = makeSegPos segIn
                if (seg.Dir = Horizontal) then
                    seg.HostId,
                    seg.Id,
                    segment.HostId,
                    segment.Id,
                    segmentIntersectsSegmentCoordinates (segment.Start, segment.End) (seg.Start, seg.End)
                else
                    segment.HostId,
                    segment.Id,
                    seg.HostId,
                    seg.Id,
                    segmentIntersectsSegmentCoordinates (segment.Start, segment.End) (seg.Start, seg.End)
        )
    |> List.distinctBy ( fun (hWid, hSid, _, _, intersectCoords) -> (hWid, hSid, intersectCoords) )    
    |> List.filter
        (
            fun (_, _, _, _, intersectCoords) ->
                match intersectCoords with
                | Some _ -> true
                | None -> false
        )
    |> List.map
        (
            fun (hWid, hSid, vWid, vSid, intersectCoords) ->
                match intersectCoords with
                | Some coord -> (hWid, hSid, vWid, vSid, coord)
                | None -> failwithf "This case should never arise."
        )
    |> List.filter
        (
            fun (hWid, hSid, vWid, vSid, intersectCoords) ->
                if (segment.Dir = Horizontal) then
                    let startX, endX = min segment.Start.X segment.End.X, max segment.Start.X segment.End.X
                    not ( ((startX + 5.0) > intersectCoords.X) || ((endX - 5.0) < intersectCoords.X) )
                else
                    let horizontalSegment =
                        wModel.WX.[hWid].Segments
                        |> List.tryFind (fun seg -> seg.Id = hSid)
                        |> function
                        | Some seg -> seg
                        | None -> failwithf "Couldn't find segId %A in makejumps" hSid
                    let startX, endX = min horizontalSegment.Start.X horizontalSegment.End.X, max horizontalSegment.Start.X horizontalSegment.End.X
                    not ( ((startX + 5.0) > intersectCoords.X) || ((endX - 5.0) < intersectCoords.X) )
        )

///
let updateWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =

    let updateSegment (seg : Segment) : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos> =
        (wModel, seg)
        ||> segmentIntersectionCoordinatesWithAllOtherSegments


    let rec listOfSegmentIntersectionsHelperFunction (wModel : Model) (listOfSegIntersection : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos>) : Model =
        match listOfSegIntersection with

        | [] -> wModel

        | firstElement :: tailList ->
            let hWid, hSid, vWid, vSid, coordIn = firstElement
            let coord = getAbsXY coordIn
            let newWireSegments =
                wModel.WX.[hWid].Segments
                |> List.map
                    (
                        fun seg ->
                            if (seg.Id = hSid) then
                                let jumpCoordinates =
                                    seg.JumpCoordinateList
                                    |> List.map fst

                                if (not (List.contains (coord.X) (jumpCoordinates))) then
                                    let newJumpCoordinateList = (coord.X, vSid) :: seg.JumpCoordinateList
                                    {seg with JumpCoordinateList = newJumpCoordinateList}
                                
                                else seg
                            else seg
                    )
            let newWire = {wModel.WX.[hWid] with Segments = newWireSegments}
            let newWX = Map.add (hWid) (newWire) (wModel.WX)

            let lst1 =
                match ( Map.tryFind (vWid, vSid) (wModel.FromVerticalToHorizontalSegmentIntersections) ) with
                | Some currList -> (hWid, hSid) :: currList
                | None -> [(hWid, hSid)]

            let newFromVerticalToHorizontalSegmentIntersections = Map.add (vWid, vSid) (lst1) (wModel.FromVerticalToHorizontalSegmentIntersections)
            
            let lst2 =
                match ( Map.tryFind (hWid, hSid) (wModel.FromHorizontalToVerticalSegmentIntersections) ) with
                | Some currList -> (vWid, vSid) :: currList
                | None -> [(vWid, vSid)]

            let newFromHorizontalToVerticalSegmentIntersections = Map.add (hWid, hSid) (lst2) (wModel.FromHorizontalToVerticalSegmentIntersections)

            let newModel = 
                {
                    wModel with
                        WX = newWX;
                        FromVerticalToHorizontalSegmentIntersections = newFromVerticalToHorizontalSegmentIntersections;
                        FromHorizontalToVerticalSegmentIntersections = newFromHorizontalToVerticalSegmentIntersections;
                }

            listOfSegmentIntersectionsHelperFunction (newModel) (tailList)


    let rec updateWireModelForJumps (wModel : Model) (wList : list<ConnectionId>) (alreadyFoundIntersections : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos>) : Model =
        match wList with

        | [] -> wModel

        | firstElement :: tailList ->
            let listOfSegmentIntersections =
                wModel.WX.[firstElement].Segments
                |> List.filter
                    (fun segIn -> 
                        let seg = makeSegPos segIn
                        distanceBetweenTwoPoints (seg.Start) (seg.End) >= 0.1)
                |> List.collect updateSegment
            
            let listOfNewIntersections =
                (listOfSegmentIntersections, alreadyFoundIntersections)
                ||> List.fold
                    (
                        fun state segmentIntersection ->
                            let currentNewSegmentIntersections =
                                state
                                |> List.map (fun (hWid, hSid, _, _, intersectCoords) -> (hWid, hSid, intersectCoords))

                            let HWID, HSID, _, _, intersectCoords = segmentIntersection
                            
                            if (List.contains (HWID, HSID, intersectCoords) (currentNewSegmentIntersections)) then
                                state
                                |> List.filter
                                    (
                                        fun (hWid, hSid, _, _, coord) ->
                                            (HWID <> hWid) ||
                                            (HSID <> hSid) ||
                                            (intersectCoords <> coord)
                                    )
                            else state
                    )
            
            let newModel = listOfSegmentIntersectionsHelperFunction wModel listOfNewIntersections

            updateWireModelForJumps (newModel) (tailList) (alreadyFoundIntersections @ listOfNewIntersections)



    updateWireModelForJumps (wModel) (wireList) ([])

/// This function updates the wire model by updating the horizontal wire segments that intersect
/// with other segments on the canvas sheet.
let resetWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =
    
    let resetWireModelForHorizontalSegments (listOfHorizontalSegments : list<ConnectionId * SegmentId>) (listOfVerticalSegments : list<ConnectionId * SegmentId>) (model : Model) : Model =
        (model, listOfHorizontalSegments)
        ||> List.fold
            (
                fun state horizontalSegment ->
                    let hWid, hSid = horizontalSegment
                    let newSegments =
                        state.WX.[hWid].Segments
                        |> List.map
                            (
                                fun seg ->
                                    if (seg.Id = hSid) then
                                        let newJumpCoordinateList =
                                            seg.JumpCoordinateList
                                            |> List.filter
                                                (
                                                    fun (_, sId) ->
                                                        let lst =
                                                            listOfVerticalSegments
                                                            |> List.map snd
                                                        not (List.contains (sId) (lst))
                                                )
                                        {seg with JumpCoordinateList = newJumpCoordinateList}
                                    else seg
                            )
                    let newWire = {state.WX.[hWid] with Segments = newSegments}
                    let newWX = Map.add (hWid) (newWire) (state.WX)

                    {state with WX = newWX}
            )

    let rec resetWireModelForJumps (wModel : Model) (wList : list<ConnectionId>) : Model =
        match wList with

        | [] -> wModel

        | firstElement :: tailList ->

            let listOfVerticalSegmentsOfCurrentWire =
                wModel.WX.[firstElement].Segments
                |> List.filter (fun seg -> seg.Dir = Vertical)
                |> List.map (fun seg -> (firstElement, seg.Id))

            let listOfHorizontalSegmentsOfCurrentWire =
                wModel.WX.[firstElement].Segments
                |> List.filter (fun seg -> seg.Dir = Horizontal)
                |> List.map (fun seg -> (firstElement, seg.Id))
            
            let listOfVerticalSegmentsInOtherWiresToBeResetted =
                ([], listOfHorizontalSegmentsOfCurrentWire)
                ||> List.fold
                    (
                        fun state horizontalSegmentOfCurrentWire ->
                            if (Map.containsKey (horizontalSegmentOfCurrentWire) (wModel.FromHorizontalToVerticalSegmentIntersections)) then
                                state @ wModel.FromHorizontalToVerticalSegmentIntersections.[horizontalSegmentOfCurrentWire]
                            else
                                state
                    )

            let listOfHorizontalSegmentsInOtherWiresToBeResetted =
                ([], listOfVerticalSegmentsOfCurrentWire)
                ||> List.fold
                    (
                        fun state verticalSegmentOfCurrentWire ->
                            if (Map.containsKey (verticalSegmentOfCurrentWire) (wModel.FromVerticalToHorizontalSegmentIntersections)) then
                                state @ wModel.FromVerticalToHorizontalSegmentIntersections.[verticalSegmentOfCurrentWire]
                            else
                                state
                    )
            

            let newModel1 =
                wModel
                |> resetWireModelForHorizontalSegments (listOfHorizontalSegmentsOfCurrentWire) (listOfVerticalSegmentsInOtherWiresToBeResetted)
                |> resetWireModelForHorizontalSegments (listOfHorizontalSegmentsInOtherWiresToBeResetted) (listOfVerticalSegmentsOfCurrentWire)

            
            let newModel2 =
                (newModel1, listOfVerticalSegmentsInOtherWiresToBeResetted)
                ||> List.fold
                    (
                        fun state verticalSegment ->
                            let newListOfHorizontalSegments =
                                state.FromVerticalToHorizontalSegmentIntersections.[verticalSegment]
                                |> List.filter
                                    (
                                        fun horizontalSegment ->
                                            not (List.contains horizontalSegment listOfHorizontalSegmentsOfCurrentWire)
                                    )
                            let newFromVerticalToHorizontalSegmentIntersections =
                                Map.add (verticalSegment) (newListOfHorizontalSegments) (state.FromVerticalToHorizontalSegmentIntersections)

                            {
                                state with
                                    FromVerticalToHorizontalSegmentIntersections = newFromVerticalToHorizontalSegmentIntersections;
                            }
                    )
            
            let newModel3 =
                (newModel2, listOfHorizontalSegmentsInOtherWiresToBeResetted)
                ||> List.fold
                    (
                        fun state horizontalSegment ->
                            let newListOfVerticalSegments =
                                state.FromHorizontalToVerticalSegmentIntersections.[horizontalSegment]
                                |> List.filter
                                    (
                                        fun verticalSegment ->
                                            not (List.contains verticalSegment listOfVerticalSegmentsOfCurrentWire)
                                    )
                            let newFromHorizontalToVerticalSegmentIntersections =
                                Map.add (horizontalSegment) (newListOfVerticalSegments) (state.FromHorizontalToVerticalSegmentIntersections)

                            {
                                state with
                                    FromHorizontalToVerticalSegmentIntersections = newFromHorizontalToVerticalSegmentIntersections;
                            }
                    )
            
            let newModel4 =
                (newModel3, listOfHorizontalSegmentsOfCurrentWire)
                ||> List.fold
                    (
                        fun state horizontalSegment ->
                            let newFromHorizontalToVerticalSegmentIntersections =
                                Map.remove (horizontalSegment) (state.FromHorizontalToVerticalSegmentIntersections)

                            {
                                state with
                                    FromHorizontalToVerticalSegmentIntersections = newFromHorizontalToVerticalSegmentIntersections;
                            }
                    )
            
            let newModel5 =
                (newModel4, listOfVerticalSegmentsOfCurrentWire)
                ||> List.fold
                    (
                        fun state verticalSegment ->
                            let newFromVerticalToHorizontalSegmentIntersections =
                                Map.remove (verticalSegment) (state.FromVerticalToHorizontalSegmentIntersections)

                            {
                                state with
                                    FromVerticalToHorizontalSegmentIntersections = newFromVerticalToHorizontalSegmentIntersections;
                            }
                    )
            

            resetWireModelForJumps (newModel5) (tailList)

    resetWireModelForJumps (wModel) (wireList)

/// Initialisatiton with no wires
let init () =
    let symbols,_ = Symbol.init()
    {   
        WX = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        Symbol = symbols; 
        CopiedWX = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
    } , Cmd.none

///Returns the wires connected to a list of components given by componentIds
let getConnectedWires (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds

    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct

///Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let filterWiresByCompMoved (wModel : Model) (compIds : list<ComponentId>) =
        let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds
        let lst = 
            wModel.WX
            |> Map.toList
            |> List.map snd

        let inputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let outputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let fullyConnected =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        (inputWires, outputWires, fullyConnected)

//Returns a newly autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    {wire with Segments = makeInitialSegmentsList wire.Id posTuple}

/// reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = seg.End; End = seg.Start})

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
// 0 => zero length segment (never used)
//
// segment qualifiers:
// - => decr not incr (segment moves in West or South direction)
// F => fixed length (next to output or input, never changes)
//
// "Simple" case where output.X < input.X
//  S0.FH  S1.0  S2.H  S3.V  S4.H  S5.0 S6.FH
//
// "Complex" case where output.X . input.X
//  S0.FH  S1.H  S2.V  S3.-H  S4.V  S5.H S6.FH
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================


let inline addPosPos (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}


let inline moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with End = mover seg.End} else seg)


let inline moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start} else seg)

let inline moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start; End = mover seg.End} else seg)

let  transformXY tX tY (pos: XYPos) =
    {pos with X = tX pos.X; Y = tY pos.Y}

let transformSeg tX tY (seg: Segment) =
    let trans = transformXY tX tY
    {seg with Start = trans seg.Start; End = trans seg.End }

let topology (pos1: XYPos) (pos2:XYPos) =
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)

/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// ReverseFun must equal not or id. not => the segments go from input to output (reverse of normal).
/// This allows the same code to work on both ends of the wire, with segment reversal done outside this
/// function to implement input -> output direction.
let partialAutoRoute (segs: Segment list) (newPortPos: XYPos) =
    let wirePos = segs.[0].End
    let portPos = segs.[0].Start
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    let lastAutoIndex =
        let isNegative (pos:XYPos) = pos.X < 0.0 || pos.Y < 0.0
        let isAutoSeg seg = 
            not (isNegative seg.Start || isNegative seg.End)
        segs
        |> List.takeWhile isAutoSeg
        |> List.length
        |> (fun n -> if n > 5 then None else Some (n + 1))
    let scaleBeforeSegmentEnd segIndex =
        let seg = segs.[segIndex]
        let fixedPt = getAbsXY seg.End
        let scale x fx nx wx =
            if nx = fx then x else ((abs x - fx)*(nx-fx)/(abs wx - fx) + fx) * float (sign x)
        let startPos = if segIndex = 1 then portPos else wirePos
        let newStartPos = if segIndex = 1 then newPortPos else newWirePos
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y
        match List.splitAt (segIndex+1) segs, segIndex with
        | ((scaledSegs), otherSegs), 1 ->
            Some ((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some ((moveAll (addPosPos diff) 0 [firstSeg] @ List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | _ -> None

    let checkTopology index =
        let finalPt = segs.[6].Start
        let oldTop x = topology (if index = 1 then portPos else wirePos) x
        let newTop x = topology (if index = 1 then newPortPos else newWirePos) x
        if oldTop finalPt <> newTop finalPt then
            // always aandon manual routing
            None 
        else
            let manSegEndPt = segs.[index].End
            let oldT = oldTop manSegEndPt
            let newT = newTop manSegEndPt
            if oldT = newT then
                Some index
            else
                None
    lastAutoIndex
    |> Option.bind checkTopology
    |> Option.bind scaleBeforeSegmentEnd


///Returns the new positions keeping manual coordinates negative, and auto coordinates positive
let negXYPos (pos : XYPos) (diff : XYPos) : XYPos =
    let newPos = Symbol.posAdd (getAbsXY pos) diff
    if pos.X < 0. || pos.Y < 0. then {X = - newPos.X; Y = - newPos.Y}
    else newPos

///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) =    
    {wire with 
        Segments = 
            wire.Segments
            |> List.map (fun seg -> 
                {seg with
                    Start = negXYPos seg.Start diff
                    End = negXYPos seg.End diff
                })
    }

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
let updateWire (model : Model) (wire : Wire) (inOut : bool) =
    let newPort = 
        match inOut with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inOut then
        partialAutoRoute (revSegments wire.Segments) newPort
        |> Option.map revSegments
    else 
        partialAutoRoute wire.Segments newPort
    |> Option.map (fun segs -> {wire with Segments = segs})
    |> Option.defaultValue (autorouteWire model wire)

/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let (inputWires, outputWires, fullyConnected) = filterWiresByCompMoved model compIdList

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId fullyConnected //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId inputWires //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId outputWires
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList
        
    {model with WX = newWires}

///
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(uuid())
        let segmentList = makeInitialSegmentsList wireId (portOnePos, portTwoPos)
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = segmentList
            }
            
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = updateWireSegmentJumps [wireId] {model with WX = wireAddedMap}

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->

        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths.[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports.[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m.[symId]

                    match symbol.Compo.Type with
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

            let newWX = ((Map.empty, model.WX) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    
        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                ) 
        
        {model with WX = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.WX
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
        
        {model with WX = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = resetWireSegmentJumps (connectionIds) (model)
        let newWX =
             newModel.WX
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg model.WX.[connId].Segments
            if seg.Draggable then
                let distanceToMove = 
                    match seg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs seg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs seg.Start.X

                let newWire = moveSegment seg distanceToMove model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.WX
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.WX connIds)
        { model with WX = newWires }, Cmd.none
    
    | ResetJumps connIds ->

        let newModel =
            model
            |> resetWireSegmentJumps connIds
        
        newModel, Cmd.none
    
    | MakeJumps connIds ->

        let newModel =
            model
            |> updateWireSegmentJumps connIds
            
        newModel, Cmd.none
    
    | ResetModel -> { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns ->
        let newWX =
            conns 
            |> List.map ( fun conn ->
                            let inputId = InputPortId conn.Target.Id
                            let outputId = OutputPortId conn.Source.Id
                            let connId = ConnectionId conn.Id
                            let segments = issieVerticesToSegments connId conn.Vertices
                            connId,
                            { Id = ConnectionId conn.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments}
                        )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps connIds)

//---------------Other interface functions--------------------//

///
let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
    let boolList = List.map (fun seg -> fst(segmentIntersectsBoundingBoxCoordinates seg bb)) w.Segments
    List.contains true boolList

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(uuid())
    
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId (portOnePos, portTwoPos)
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                    }
                ]
            | None -> []
        
        wModel.CopiedWX
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList
    
    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.WX
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with WX = newWireMap }, pastedConnIds

///
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX.[connId].InputPort :: inputPorts, model.WX.[connId].OutputPort :: outputPorts))
