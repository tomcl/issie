(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//------------------------------------------------------------------------//
//------------------------------BusWire Constants-------------------------//
//------------------------------------------------------------------------//

module Constants =
    let jumpRadius = 5.
    /// The minimum length of the initial segments (nubs) leaving the ports
    let nubLength = 8.
    /// The standard radius of a radial wire corner
    let cornerRadius = 5. 
    /// The standard radius of a modern wire connect circle
    let modernCircleRadius = 5.


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

/// Represents the orientation of a wire segment
type Orientation =  Horizontal | Vertical

///
type SnapPosition = High | Mid | Low

/// Represents how wires are rendered
type WireType = Radial | Modern | Jump

/// Represents how a wire segment is currently being routed
type RoutingMode = Manual | Auto

/// Used to represent a segment in a wire
type Segment = 
    {
        Id : SegmentId
        Index: int
        Length : float
        HostId: ConnectionId
        /// List of offsets along a segment where jumps or intersects occur. Matches the sign of Length. Only used on horizontal segments.
        IntersectOrJumpList: list<float * SegmentId>
        Draggable : bool
        Mode : RoutingMode
    }

type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
        StartPos : XYPos
        EndPos : XYPos
        InitialOrientation : Orientation
        EndOrientation : Orientation
    }



/// Defines offsets used to render wire width text
type TextOffset =
    static member yOffset = 7.
    static member xOffset = 1.
    static member xLeftOffset = 20.

type Model =
    {
        Symbol: Symbol.Model
        Wires: Map<ConnectionId, Wire>
        CopiedWires: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
        Type : WireType
    }

//----------------------------Message Type-----------------------------------//

type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | UpdateSymbolWires of ComponentId
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | UpdateWireType of WireType
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration
    | Rotate of list<ComponentId>
    | RerouteWire of string






/// Returns an XYPos shifted by length in an X or Y direction defined by orientation.
let inline addLengthToPos (position: XYPos) orientation length =
    match orientation with
    | Horizontal -> { position with X = position.X + length }
    | Vertical -> { position with Y = position.Y + length }

/// Returns the opposite orientation of the input orientation. (i.e. Horizontal becomes Vertical and vice-versa)
let inline switchOrientation orientation =
    match orientation with
    | Horizontal -> Vertical
    | Vertical -> Horizontal

/// <summary> Applies a function which requires the segment start and end positions to the segments in a wire, 
/// threading an accumulator argument through the computation. Essentially a List.fold applied to the list of segments of a wire, but with access to each segment's absolute positions. </summary>
/// <remarks> This is used in cases where absolute segment positions are required. 
/// These positions are computed on the fly and passed to the folder function. </remarks>
/// <param name="folder"> The function to update the state given the segment start and end positions, current state and segment itself.</param>
/// <param name="state"> The initial state.</param>
/// <param name="wire"> The wire containing the segment list we are folding over.</param>
/// <returns> The final state value </returns>
let inline foldOverSegs folder state wire =
    let initPos = wire.StartPos
    let initOrientation = wire.InitialOrientation
    ((state, initPos, initOrientation), wire.Segments)
    ||> List.fold (fun (currState, currPos, currOrientation) seg -> 
        let nextPos = addLengthToPos currPos currOrientation seg.Length
        let nextOrientation = switchOrientation currOrientation
        let nextState = folder currPos nextPos currState seg
        (nextState, nextPos, nextOrientation))
    |> (fun (state, _, _) -> state)

//-------------------------Debugging functions---------------------------------//

/// Formats a SegmentId for logging purposes.
let formatSegmentId (id: SegmentId) =
    id
    |> (fun (SegmentId str) -> str)
    |> (fun str -> str[0..2])

/// Formats a WireId for logging purposes
let formatWireId (id: ConnectionId) =
    id
    |> (fun (ConnectionId str) -> str)
    |> (fun str -> str[0..2])

/// Logs the given SegmentId and returns it unchanged. Used for debugging.
let logSegmentId (id:SegmentId) =
    printfn $"{formatSegmentId id}"; id

/// Logs the given Segment and returns it unchanged. Used for debugging.
let logSegment (seg:Segment) =
    printfn $"|{seg.Index}:{formatSegmentId seg.Id}|-Length: {seg.Length}"; seg

/// Logs the given ConnectionId and returns it unchanged. Used for debugging.
let logConnectionId (id:ConnectionId) =
        id
        |> (fun (ConnectionId str) -> str)
        |> (fun str -> printfn $"{str[0..2]}"; id)

/// Formats an intersection map for logging purposes.
let formatIntersectionMap (m:Map<SegmentId, (ConnectionId * SegmentId) list>) =
    m
    |> Map.toList
    |> List.map (fun (segId, lst) ->
        List.map (snd >> formatSegmentId) lst
        |> (fun segs -> sprintf $"""<{formatSegmentId segId}->[{String.concat ";" segs}]"""))
        |> String.concat ";\n"

/// Logs the intersection maps of a given model and returns it unchanged. Used for debugging
let logIntersectionMaps (model:Model) =
    let intersections =
        let formatSegmentIntersections segments =
            segments
            |> List.collect (fun segment -> 
                segment.IntersectOrJumpList
                |> List.map (fun (_, id) -> formatSegmentId id))

        model.Wires
        |> Map.toList
        |> List.map (fun (wId, wire) -> 
            sprintf $"Wire {formatWireId wId}: {formatSegmentIntersections wire.Segments}")

    printfn $"Intersections"
    printfn $"{intersections}"
    printfn "---- --------------"
    model

/// Formats an XYPos for logging purposes.
let formatXY (xy: XYPos) = sprintf $"{(int(xy.X),int(xy.Y))}"

/// Logs the given wire and returns it unchanged. Used for debugging.
let logWire wire =
    let formatSegments startPos endPos state seg = 
        let entry = sprintf $"|{seg.Index}:{formatSegmentId seg.Id}| Start: {formatXY startPos}, End: {formatXY endPos}"
        String.concat "\n" [state; entry]
    let start = sprintf $"Wire: {formatWireId wire.Id}"
    printfn $"{foldOverSegs formatSegments start wire}"
    wire

/// Given a segment start and end position, finds the orientation of the segment. 
/// Returns None if the segment is neither horizontal nor vertical
let inline getSegmentOrientation (segStart: XYPos) (segEnd: XYPos) =
    if abs (segStart.X - segEnd.X) < XYPos.epsilon then
        Vertical
    else if abs (segStart.Y - segStart.Y) < XYPos.epsilon then
        Horizontal
    else
        failwithf "ERROR: Diagonal wire" // Should never happen

/// Given a segment start and end position, finds the symbol edge of the segment
/// based on its segment direction if it starts an outgoing wire.
let inline getWireOutgoingEdge (wire:Wire) =
    match wire.InitialOrientation, wire.Segments.[0].Length > 0 with
    | Horizontal, true -> Edge.Right
    | Horizontal, false -> Edge.Left
    | Vertical, true -> Edge.Bottom
    | Vertical, false -> Edge.Top
     

/// Tries to find and log a segment identified by segId in a wire identified by wireId in the current model.
/// Assumes wireId can be found in the current model. Returns unit, used for debugging.
let logSegmentInModel model wireId segId  = 
        let wire = model.Wires[wireId]
        let findAndFormatSeg segStart segEnd (_state: string option) (seg: Segment) =
            if seg.Id = segId then 
                let orientation = 
                    match getSegmentOrientation segStart segEnd with
                    | Vertical -> "V"
                    | Horizontal -> "H"
                Some (sprintf $"""[{formatSegmentId seg.Id}: {formatXY segStart}->{formatXY segEnd}]-{orientation}-{seg.Index}""")
            else None

        match foldOverSegs findAndFormatSeg None wire with
        | Some str -> printfn $"{str}"
        | _ -> printfn $"ERROR: Could not find segment {formatSegmentId segId} in wire {formatWireId wireId}"
        


/// Tries to find and log each segment to its corresponding wire identified in wireSegmentIdPairs in the current model.
/// Returns the model unchanged. Used for debugging.
let logSegmentsInModel (model: Model) (wireSegmentIdPairs: (ConnectionId * SegmentId) list)= 
    wireSegmentIdPairs
    |> List.map  ( fun (wireId, segId) -> logSegmentInModel model wireId segId)
    |> ignore
    model

//-------------------------------Implementation code----------------------------//

/// Converts a segment list into a list of vertices
/// Needs wire for starting position
let segmentsToVertices (segList:Segment list) (wire:Wire) = 
    ((wire.StartPos, wire.InitialOrientation),segList)
    ||> List.scan(fun (currPos, currOrientation) seg ->
        let (nextPos, nextOrientation) =
            match currOrientation with
            | Horizontal -> { currPos with X = currPos.X + seg.Length}, Vertical
            | Vertical -> { currPos with Y = currPos.Y + seg.Length}, Horizontal
        let nextState = (nextPos,nextOrientation)
        nextState)
    |> List.map ( fun (pos,_) -> pos.X,pos.Y)

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the final port orientation 
/// this function returns a list of wire vertices
let makeInitialWireVerticesList (wireStartPos : XYPos) (wireEndPos : XYPos) (portOrientation : Edge) = 
    let xStart, yStart, xEnd, yEnd = wireStartPos.X, wireStartPos.Y, wireEndPos.X, wireEndPos.Y

    let nubLength = Constants.nubLength
    /// This is a fixed-length horizontal stick with a zero-length vertical after it.
    /// It starts nearly all the wires
    let rightNub = [
            {X = xStart; Y = yStart};
            {X = xStart+nubLength; Y = yStart}; //Stick horizontal
            {X = xStart+nubLength; Y = yStart}; //Length 0 vertical
        ]
    let rightwards = xStart - xEnd + 20. < 0
    let downwards = yStart - yEnd  < 0
    match rightwards, downwards with //add 20 to prevent issues in the case that the ports are directly on in line with one another
    | true, true ->
            match portOrientation with
            | CommonTypes.Top  ->  rightNub @ [
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd-nubLength}; 
                    {X = xEnd; Y = yEnd-nubLength};// Length 0 horizontal
                    {X = xEnd; Y = yEnd}] // Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+nubLength; Y = yEnd}; 
                    {X = xEnd+nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+nubLength}; 
                    {X = xEnd; Y = yEnd+nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-nubLength; Y = yEnd}; 
                    {X = xEnd-nubLength; Y = yEnd}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
    | true, false -> 
            match portOrientation with
            | CommonTypes.Bottom ->  rightNub @ [
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd+nubLength}; 
                    {X = xEnd; Y = yEnd+nubLength}; //Length 0 hortizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+nubLength; Y = yEnd}; 
                    {X = xEnd+nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-nubLength}; 
                    {X = xEnd; Y = yEnd-nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-nubLength; Y = yEnd}; 
                    {X = xEnd-nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
    | false, true -> 
            match portOrientation with
            | CommonTypes.Bottom ->  rightNub @ [
                    {X = xStart+nubLength+10.; Y = yStart}; //Small horizontal for dragging  
                    {X = xStart+nubLength+10.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+nubLength}; 
                    {X = xEnd; Y = yEnd+nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xStart+nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+nubLength+10.; Y = yEnd};
                    {X = xEnd+nubLength; Y = yEnd}; 
                    {X = xEnd+nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+nubLength; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-nubLength}; 
                    {X = xEnd; Y = yEnd-nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = xStart+nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+nubLength+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-nubLength; Y = yEnd}; 
                    {X = xEnd-nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
        | false, false -> 
            match portOrientation with
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+nubLength; Y = yEnd-10.}; //Length 0 vertical
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-nubLength}; 
                    {X = xEnd; Y = yEnd-nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xStart+nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+nubLength+10.; Y = yEnd};
                    {X = xEnd+nubLength; Y = yEnd}; 
                    {X = xEnd+nubLength; Y = yEnd}; //Lenght 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+nubLength; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd+nubLength}; 
                    {X = xEnd; Y = yEnd+nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = xStart+nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+nubLength+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-nubLength; Y = yEnd}; 
                    {X = xEnd-nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal

/// Converts a list of vertices into a list of segments
let xyVerticesToSegments connId (xyVerticesList: XYPos list) =
    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=xStart; Y=yStart},{X=xEnd; Y=yEnd}) ->    
            let id = SegmentId(JSHelpers.uuid())
            {
                Id = id
                Index = i
                Length = xEnd-xStart+yEnd-yStart
                HostId  = connId;
                IntersectOrJumpList = [] ; // To test jump and modern wire types need to manually insert elements into this list.
                Mode = Auto
                Draggable =
                    if i = 0 || i = xyVerticesList.Length - 2 then //First and Last should not be draggable
                        false
                    else
                        true
            })


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the orientation of the final port
/// this function returns a list of Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (startPos : XYPos) (endPos : XYPos) (portOrientation : Edge) : list<Segment> =
    let xyPairs = makeInitialWireVerticesList startPos endPos portOrientation
    xyPairs
    |> xyVerticesToSegments hostId 


//----------------------interface to Issie-----------------------//

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to a list of segments
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float*bool>) =
    let verticesList' =
        verticesList
        |> List.map (fun (x,y,m) -> 
            let mode = if m then Manual else Auto
            {| Pos = {X=x;Y=y}; Mode = mode |})

    /// Converts a list of vertices into a list of segments
    let verticesToSegments connId (xyVerticesList: {| Pos: XYPos; Mode: RoutingMode |} list) =
        List.pairwise xyVerticesList
        |> List.mapi (
            fun i (startVertex,endVertex) ->    
                let id = SegmentId(JSHelpers.uuid())
                {
                    Id = id
                    Index = i
                    Length = endVertex.Pos.X-startVertex.Pos.X+endVertex.Pos.Y-startVertex.Pos.Y
                    HostId  = connId;
                    IntersectOrJumpList = [] ; // To test jump and modern wire types need to manually insert elements into this list.
                    Mode = endVertex.Mode
                    Draggable =
                        if i = 0 || i = xyVerticesList.Length - 2 then //First and Last should not be draggable
                            false
                        else
                            true
                })
        
    verticesToSegments connId verticesList'

/// Converts a segment list into a list of vertices to store inside Connection
let segmentsToIssieVertices (segList:Segment list) (wire:Wire) = 
    ((wire.StartPos, wire.InitialOrientation, false),segList)
    ||> List.scan(fun (currPos, currOrientation, _) seg ->
        let (nextPos, nextOrientation) =
            match currOrientation with
            | Horizontal -> { currPos with X = currPos.X + seg.Length}, Vertical
            | Vertical -> { currPos with Y = currPos.Y + seg.Length}, Horizontal
        let manual = (seg.Mode = Manual)
        (nextPos,nextOrientation,manual))
    |> List.map ( fun (pos,_,manual) -> pos.X,pos.Y,manual)

/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.Wires[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = segmentsToIssieVertices conn.Segments conn
    }

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connections, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.Wires
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)


//----------------------Rendering Functions----------------------//

/// A pair of vertices as well as any intersections or jumps on that segment
type AbsSegment = 
    {
    Start: XYPos
    End: XYPos
    IntersectCoordinateList: list<float * SegmentId>
    }

/// Type passed to wire renderer functions
type WireRenderProps =
    {
        key: string
        Wire: Wire
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortEdge : Edge
        OutputPortLocation: XYPos
        DisplayType : WireType
    }
let getAbsSegments (wire: Wire) : AbsSegment List= 
    segmentsToVertices wire.Segments wire
    |> List.map (fun x -> {X=fst(x); Y=snd(x)})
    |> List.pairwise
    |> List.zip wire.Segments
    |> List.map (fun x -> fst(snd(x)),snd(snd(x)), fst(x).IntersectOrJumpList)
    |> List.map (fun (startVertex,endVertex,intersectList) -> {Start=startVertex; End=endVertex; IntersectCoordinateList=intersectList})

///Creates the SVG command string required to render the wire 
/// (apart from the final "nub") with a radial display type 
let renderRadialWire (state : (string * Orientation)) (segmentpair : {| First : AbsSegment; Second :AbsSegment|}) =
    
    let startFirstSegment = segmentpair.First.Start
    let endFirstSegment = segmentpair.First.End
    let startSecondSegment = segmentpair.Second.Start
    let endSecondSegment = segmentpair.Second.End
    
    let dist1 = euclideanDistance startFirstSegment endFirstSegment
    let dist2 = euclideanDistance startSecondSegment endSecondSegment
    let rad = System.Math.Floor(min Constants.cornerRadius (max 0.0 (min dist1 dist2)))
    let makeCommandString xStart yStart rad sweepflag xEnd yEnd : string =
        $"L {xStart} {yStart} A {rad} {rad}, 45, 0, {sweepflag}, {xEnd} {yEnd}" 

    //Checking if horizontal followed by length 0 vertical
    if startFirstSegment.X = endFirstSegment.X && 
       startFirstSegment.X = startSecondSegment.X &&
       startFirstSegment.X = endSecondSegment.X then
        let current = sprintf "L %f %f" endFirstSegment.X endFirstSegment.Y
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)
    //Checking if vertical followed by length 0 horizontal
    else if startFirstSegment.Y = endFirstSegment.Y && 
            startFirstSegment.Y = startSecondSegment.Y && 
            startFirstSegment.Y = endSecondSegment.Y then
        let current = sprintf "L %f %f" (endFirstSegment.X) (endFirstSegment.Y)
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)           
    
    else
        if snd(state) = Horizontal then
            if startFirstSegment.X - endFirstSegment.X > 0 then
                if startSecondSegment.Y - endSecondSegment.Y > 0 then
                    let current:string = makeCommandString (endFirstSegment.X+rad) endFirstSegment.Y rad 1 startSecondSegment.X (startSecondSegment.Y-rad)
                    ((fst(state)+current), Vertical)
                else
                    let current:string  =  makeCommandString (endFirstSegment.X+rad) endFirstSegment.Y rad 0 startSecondSegment.X (startSecondSegment.Y+rad)
                    ((fst(state)+current), Vertical)
            else
                if startSecondSegment.Y - endSecondSegment.Y > 0 then
                    let current:string =  makeCommandString (endFirstSegment.X-rad)endFirstSegment.Y rad 0 startSecondSegment.X (startSecondSegment.Y-rad)
                    ((fst(state)+current), Vertical)
                else
                    let current:string = makeCommandString (endFirstSegment.X-rad) endFirstSegment.Y rad 1 startSecondSegment.X (startSecondSegment.Y+rad)
                    ((fst(state)+current), Vertical)
        else
            if startFirstSegment.Y - endFirstSegment.Y > 0 then
                if startSecondSegment.X - endSecondSegment.X > 0 then
                    let current :string =  makeCommandString endFirstSegment.X (endFirstSegment.Y+rad) rad 0 (startSecondSegment.X-rad) startSecondSegment.Y
                    ((fst(state)+current), Horizontal)
                else
                    let current :string =  makeCommandString endFirstSegment.X (endFirstSegment.Y+rad) rad 1 (startSecondSegment.X+rad) startSecondSegment.Y
                    ((fst(state)+current), Horizontal)
            else
                if startSecondSegment.X - endSecondSegment.X > 0 then
                    let current :string =  makeCommandString endFirstSegment.X (endFirstSegment.Y-rad) rad 1 (startSecondSegment.X-rad) startSecondSegment.Y
                    ((fst(state)+current), Horizontal)
                else
                    let current :string =  makeCommandString endFirstSegment.X (endFirstSegment.Y-rad) rad  0 (startSecondSegment.X+rad) startSecondSegment.Y
                    ((fst(state)+current), Horizontal)

///Renders a single segment in the display type of modern
let renderModernSegment (param : {| AbsSegment : AbsSegment; Colour :string; Width : string|}) = 
    let startVertex = param.AbsSegment.Start
    let endVertex = param.AbsSegment.End
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse param.Width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let lineParameters = { defaultLine with Stroke = param.Colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = 2.5; Stroke = param.Colour;  Fill = param.Colour } 
    
    let circles =
        if (startVertex.X - endVertex.X > 0) then //Segment is right to left
            param.AbsSegment.IntersectCoordinateList 
            |> List.map (fun x -> startVertex.X - fst(x))
            |> List.map (fun x -> makeCircle x startVertex.Y circleParameters)
        else                                      //Segment is left to right
            param.AbsSegment.IntersectCoordinateList 
            |> List.map (fun x -> startVertex.X + fst(x))
            |> List.map (fun x -> makeCircle x startVertex.Y circleParameters)
    
    //Only ever render intersections on horizontal segments
    if getSegmentOrientation startVertex endVertex = Horizontal then 
        makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters
        :: circles
    else
        [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
        

let renderSegmentWithJumps (a:AbsSegment) : string list=
    let sPos = a.Start
    let ePos = a.End
    let jR = Constants.jumpRadius
    /// direction of travel for horizontal segments
    let rightTravel = ePos.X > sPos.X
    let dir = if rightTravel then 1.0 else -1.0
    let makePartArc d1 d2 =
        if abs d1 > jR || abs d2 > jR then
            failwithf "d1={d1}, d2={d2}, jR={jR}"
        let h1 = sqrt (max 0. (jR*jR-d1*d1))
        let h2 = sqrt (max 0. (jR*jR-d2*d2))
        makePartArcAttr jR h1 d1 h2 d2
    let rec makeJumpPathAttr jLst xPos =
        match jLst with
        | [] -> 
            [ makeLineAttr (ePos.X - xPos) 0.0 ]
        | (xJ,_):: jLst' when abs (xJ - xPos) > jR ->
            makeLineAttr (xJ - xPos - dir*jR) 0. :: makeJumpPathAttr jLst (xJ - dir*(jR - XYPos.epsilon))
        | [xJ,_] when abs (ePos.X - xJ) < jR ->
            [ makePartArc (xJ - xPos) (ePos.X - xJ) ]
        | [xJ,_] ->
            makePartArc (xJ - xPos) (dir*jR) :: makeJumpPathAttr [] (xJ + dir * jR)
        | (xJ,_) :: (((yJ,_) :: _) as jLst') when abs (yJ - xJ) > 2. * jR ->
            makePartArc (xJ - xPos) (dir*jR) :: makeJumpPathAttr jLst' (xJ + dir*jR)
        | (xJ,_) :: (((yJ,_) :: _) as jLst') ->
            makePartArc (xJ - xPos) ((yJ - xJ) / 2.0) :: makeJumpPathAttr jLst' ((yJ+xJ)/ 2.0)
    let jLst =
        match rightTravel, a.IntersectCoordinateList with
        | true, jL -> jL |> List.sortBy fst
        | false, jL -> jL |> List.sortBy fst
        |> List.map (fun (f,seg) -> f*dir + sPos.X,seg)
    match jLst, abs (sPos.X - ePos.X) < XYPos.epsilon with
    | _, true
    | [], false -> 
        [$"L {ePos.X} {ePos.Y}"]
    | jLst, false -> 
        makeJumpPathAttr jLst sPos.X
       
       


    

///Function used to render a single wire if the display type is jump
let singleWireJumpView props = 
    let absSegments = getAbsSegments props.Wire
    let firstVertex = absSegments.Head.Start
    let secondVertex = absSegments.Head.End
    let lastVertex = (List.last absSegments).Start
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        let pathPars:Path =
            { defaultPath with
                Stroke = colour
                StrokeWidth = string renderWidth 
            }
        absSegments
        |> List.collect renderSegmentWithJumps
        |> String.concat " "
        |> (fun attr -> [makeAnyPath firstVertex attr pathPars])

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
        match props.OutputPortEdge with 
        | CommonTypes.Top -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Bottom -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y+TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Right -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Left -> makeText (props.OutputPortLocation.X-TextOffset.xLeftOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters

    g [] ([ renderWireWidthText ] @ renderedSegmentList)

///Function used to render a single wire if the display type is modern
let singleWireModernView props = 
    let absSegments = getAbsSegments props.Wire
    let firstVertex = absSegments.Head.Start
    let secondVertex = absSegments.Head.End
    let lastVertex = (List.last absSegments).End
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        absSegments
        |> List.map (fun x -> {|AbsSegment = x; Colour = colour; Width = width|})
        |> List.collect renderModernSegment //colour width //(props.ColorP.Text()) (string props.StrokeWidthP)

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
        match props.OutputPortEdge with 
        | CommonTypes.Top -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Bottom -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y+TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Right -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Left -> makeText (props.OutputPortLocation.X-TextOffset.xLeftOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters

    g [] ([ renderWireWidthText ] @ renderedSegmentList)

///Function used to render a single wire if the display type is radial
let singleWireRadialView props =
    let absSegments = getAbsSegments props.Wire
    let firstVertex = absSegments.Head.Start
    let secondVertex = absSegments.Head.End
    let lastVertex = (List.last absSegments).End

    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5

    let pathParameters = { defaultPath with Stroke = props.ColorP.Text(); StrokeWidth = string renderWidth;}
    let initialMoveCommand = sprintf "M %f %f "  firstVertex.X firstVertex.Y
    let initialState = (initialMoveCommand, getSegmentOrientation firstVertex secondVertex )
    
    let radialPathCommands = fst(
        absSegments
        |> List.pairwise
        |> List.map (fun x -> ( {| First = fst(x); Second = snd(x) |}))
        |> List.fold renderRadialWire (initialState) )
    let finalLineCommand = sprintf "L %f %f" lastVertex.X lastVertex.Y
    let fullPathCommand = radialPathCommands + finalLineCommand

    let renderedSVGPath = makePathFromAttr fullPathCommand pathParameters
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
        match props.OutputPortEdge with 
        | CommonTypes.Top -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Bottom -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y+TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Right -> makeText (props.OutputPortLocation.X+TextOffset.xOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters
        | CommonTypes.Left -> makeText (props.OutputPortLocation.X-TextOffset.xLeftOffset) (props.OutputPortLocation.Y-TextOffset.yOffset) (textString) textParameters

    g [] ([ renderWireWidthText ] @ [renderedSVGPath])



/// Function that will render all of the wires within the model, with the display type being set in Model.Type
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires' =
        model.Wires
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires'
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getPortLocation None model.Symbol stringOutId 
                    let outputPortEdge = Symbol.getOutputPortOrientation model.Symbol wire.OutputPort 
                    
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Wire = wire
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width //To test the display of bit width text we can manually change this value, as the function to change it is not correctly implemented in section 3.
                            OutputPortEdge = outputPortEdge
                            OutputPortLocation = outputPortLocation
                            DisplayType = model.Type
                        }
                    match  model.Type with    
                    | Radial -> singleWireRadialView props
                    | Jump -> singleWireJumpView props
                    | Modern -> singleWireModernView props

            )
    let wiresAndTriangle = 
        wires'
        |> Array.map
            (
                fun triangle ->
                    let stringInId =
                            match triangle.InputPort with
                            | InputPortId stringId -> stringId
                    let InputPortLocation = Symbol.getPortLocation None model.Symbol stringInId 
                    let InputPortEdge = Symbol.getInputPortOrientation model.Symbol triangle.InputPort 
                    let str:string = 
                        if InputPortEdge = CommonTypes.Top then
                            sprintf "%f,%f %f,%f %f,%f " InputPortLocation.X InputPortLocation.Y (InputPortLocation.X+2.) (InputPortLocation.Y-4.) (InputPortLocation.X-2.) (InputPortLocation.Y-4.)
                        else if InputPortEdge = CommonTypes.Bottom then
                            sprintf "%f,%f %f,%f %f,%f " InputPortLocation.X InputPortLocation.Y (InputPortLocation.X+2.) (InputPortLocation.Y+4.) (InputPortLocation.X-2.) (InputPortLocation.Y+4.)
                        else if InputPortEdge = CommonTypes.Right then
                            sprintf "%f,%f %f,%f %f,%f " InputPortLocation.X InputPortLocation.Y (InputPortLocation.X+4.) (InputPortLocation.Y+2.) (InputPortLocation.X+4.) (InputPortLocation.Y-2.)
                        else 
                            sprintf "%f,%f %f,%f %f,%f " InputPortLocation.X InputPortLocation.Y (InputPortLocation.X-4.) (InputPortLocation.Y+2.) (InputPortLocation.X-4.) (InputPortLocation.Y-2.)

                    let polygon = {
                        defaultPolygon with
                            Fill = "black"
                            }
                    makePolygon str polygon
            )
    let res = 
        wires 
        |> Array.append wiresAndTriangle

    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] res); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start

