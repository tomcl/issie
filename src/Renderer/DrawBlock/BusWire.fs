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

    /// The minimum length of the initial segments (nubs) leaving the ports
    with static member nubLength = 8.
         /// The standard radius of a radial curve, modern circle or jump width
         static member radius = 5. 
         /// The standard height of a jump
         static member jumpHeight = 7.

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
let addLengthToPos (position: XYPos) orientation length =
    match orientation with
    | Horizontal -> { position with X = position.X + length }
    | Vertical -> { position with Y = position.Y + length }

/// Returns the opposite orientation of the input orientation. (i.e. Horizontal becomes Vertical and vice-versa)
let switchOrientation =
    function
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
let foldOverSegs folder state wire =
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
let getSegmentOrientation (segStart: XYPos) (segEnd: XYPos) =
    if abs (segStart.X - segEnd.X) < XYPos.epsilon then
        Vertical
    else if abs (segStart.Y - segStart.Y) < XYPos.epsilon then
        Horizontal
    else
        failwithf "ERROR: Diagonal wire" // Should never happen

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
    match xStart - xEnd + 20. < 0 with //add 20 to prevent issues in the case that the ports are directly on in line with one another
    | true -> //Right of startpos
        match yStart - yEnd  < 0 with 
        | true -> //Below startpos
            match portOrientation with
            | CommonTypes.Top  ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd-Wire.nubLength}; 
                    {X = xEnd; Y = yEnd-Wire.nubLength};// Length 0 horizontal
                    {X = xEnd; Y = yEnd}] // Stick vertical
            | CommonTypes.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+Wire.nubLength; Y = yEnd}; 
                    {X = xEnd+Wire.nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+Wire.nubLength}; 
                    {X = xEnd; Y = yEnd+Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-Wire.nubLength; Y = yEnd}; 
                    {X = xEnd-Wire.nubLength; Y = yEnd}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
        | false -> //Above startpos
            match portOrientation with
            | CommonTypes.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd+Wire.nubLength}; 
                    {X = xEnd; Y = yEnd+Wire.nubLength}; //Length 0 hortizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+Wire.nubLength; Y = yEnd}; 
                    {X = xEnd+Wire.nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-Wire.nubLength}; 
                    {X = xEnd; Y = yEnd-Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-Wire.nubLength; Y = yEnd}; 
                    {X = xEnd-Wire.nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
    | false-> //left of startpos
        match yStart - yEnd < 0 with
        | true -> //below startpos
            match portOrientation with
            | CommonTypes.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.nubLength+10.; Y = yStart}; //Small horizontal for dragging  
                    {X = xStart+Wire.nubLength+10.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+Wire.nubLength}; 
                    {X = xEnd; Y = yEnd+Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.nubLength+10.; Y = yEnd};
                    {X = xEnd+Wire.nubLength; Y = yEnd}; 
                    {X = xEnd+Wire.nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-Wire.nubLength}; 
                    {X = xEnd; Y = yEnd-Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.nubLength+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-Wire.nubLength; Y = yEnd}; 
                    {X = xEnd-Wire.nubLength; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
        | false -> //above startpos
            match portOrientation with
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yEnd-10.}; //Length 0 vertical
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-Wire.nubLength}; 
                    {X = xEnd; Y = yEnd-Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.nubLength+10.; Y = yEnd};
                    {X = xEnd+Wire.nubLength; Y = yEnd}; 
                    {X = xEnd+Wire.nubLength; Y = yEnd}; //Lenght 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-Wire.nubLength}; 
                    {X = xEnd; Y = yEnd-Wire.nubLength}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.nubLength; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.nubLength; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.nubLength+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.nubLength+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-Wire.nubLength; Y = yEnd}; 
                    {X = xEnd-Wire.nubLength; Y = yEnd}; //Length 0 vertical
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
        AbsSegments: list<AbsSegment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortEdge : Edge
        OutputPortLocation: XYPos
        DisplayType : WireType
    }

///Creates the SVG command string required to render the wire 
/// (apart from the final "nub") with a radial display type 
let renderRadialWire (state : (string * Orientation)) (segmentpair : {| First : AbsSegment; Second :AbsSegment|}) =
    
    let startFirstSegment = segmentpair.First.Start
    let endFirstSegment = segmentpair.First.End
    let startSecondSegment = segmentpair.Second.Start
    let endSecondSegment = segmentpair.Second.End
    
    let dist1 = euclideanDistance startFirstSegment endFirstSegment
    let dist2 = euclideanDistance startSecondSegment endSecondSegment
    let rad = System.Math.Floor(min Wire.radius (max 0.0 (min dist1 dist2)))
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
        
///Renders a single segment in the display type of jump
let renderJumpSegment (param : {| AbsSegment : AbsSegment; Colour :string; Width : string|}) = 
    let startVertex = param.AbsSegment.Start
    let endVertex = param.AbsSegment.End
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse param.Width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let lineParameters = { defaultLine with Stroke = param.Colour; StrokeWidth = string renderWidth }
    let pathParameters = { defaultPath with Stroke = param.Colour; StrokeWidth = string renderWidth;}

    //Generate all sections of a left to right segment that don't have jumps
    let lefttoright (state : (float * ReactElement List)) xPos =
        let element =
            makeLine (fst(state)) startVertex.Y (xPos-Wire.radius) endVertex.Y lineParameters
        (xPos+Wire.radius,List.append (snd(state)) [element])

    //Generate all sections of a right to left segment that don't have jumps
    let righttoleft (state : (float * ReactElement List)) xPos =
        let element =
            makeLine (fst(state)) startVertex.Y (xPos+Wire.radius) endVertex.Y lineParameters
        (xPos-Wire.radius,List.append (snd(state)) [element])
    
    //If no jumps then straight line
    if List.isEmpty param.AbsSegment.IntersectCoordinateList then 
        [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
    else
        if startVertex.X - endVertex.X < 0 then //Segment is left to right
            let jumps =
                param.AbsSegment.IntersectCoordinateList 
                |> List.map (fun x -> startVertex.X + fst(x))
                |> List.map (fun x -> makePath {X = x - Wire.radius; Y = startVertex.Y} {X = x - Wire.radius; Y = startVertex.Y - Wire.jumpHeight} {X = x + Wire.radius; Y = startVertex.Y - Wire.jumpHeight} {X = x + Wire.radius; Y = startVertex.Y} pathParameters)
            let lines =
                param.AbsSegment.IntersectCoordinateList
                |> List.map (fun x -> startVertex.X + fst(x))
                |> List.sort
                |> List.fold lefttoright (startVertex.X,[])
            let finalLines = 
                List.append (snd(lines)) [makeLine (fst(lines)) startVertex.Y endVertex.X endVertex.Y lineParameters]
            
            //Only ever render intersections on horizontal segments
            if getSegmentOrientation startVertex endVertex = Horizontal then
                List.append finalLines jumps
            else
                [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
        
        else                                    //Segment is right to left
            let jumps =
                param.AbsSegment.IntersectCoordinateList 
                |> List.map (fun x -> startVertex.X - fst(x))
                |> List.map (fun x -> makePath {X = x - Wire.radius; Y = startVertex.Y} {X = x - Wire.radius; Y = startVertex.Y - Wire.jumpHeight} {X = x + Wire.radius; Y = startVertex.Y - Wire.jumpHeight} {X = x + Wire.radius; Y = startVertex.Y} pathParameters)
            let lines =
                param.AbsSegment.IntersectCoordinateList
                |> List.map (fun x -> startVertex.X - fst(x))
                |> List.sortDescending
                |> List.fold righttoleft (startVertex.X,[])
            let finalLines = 
                List.append (snd(lines)) [makeLine (fst(lines)) startVertex.Y endVertex.X endVertex.Y lineParameters]
            
            //Only ever render intersections on horizontal segments
            if getSegmentOrientation startVertex endVertex = Horizontal then
                List.append finalLines jumps
            else
                [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]


///Function used to render a single wire if the display type is jump
let singleWireJumpView props = 
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).Start
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        props.AbsSegments
        |> List.map (fun x -> {|AbsSegment = x; Colour = colour; Width = width|})
        |> List.collect renderJumpSegment 

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
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).End
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        props.AbsSegments
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
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).End

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
        props.AbsSegments
        |> List.pairwise
        |> List.map (fun x -> ( {| First = fst(x); Second = snd(x) |}))
        |> List.fold renderRadialWire (initialState) )
    let finalLineCommand = sprintf "L %f %f" lastVertex.X lastVertex.Y
    let fullPathCommand = radialPathCommands + finalLineCommand
    
    let makeCustomPath command (pathParameters: Path) =
        let dAttrribute = command 
        path [
            D dAttrribute
            SVGAttr.Stroke pathParameters.Stroke
            SVGAttr.StrokeWidth pathParameters.StrokeWidth
            SVGAttr.StrokeDasharray pathParameters.StrokeDashArray
            SVGAttr.StrokeLinecap pathParameters.StrokeLinecap
            SVGAttr.Fill pathParameters.Fill
        ]   []

    let renderedSVGPath = makeCustomPath fullPathCommand pathParameters
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
                    let getAbsSegments wire : AbsSegment List= 
                        segmentsToVertices wire.Segments wire
                        |> List.map (fun x -> {X=fst(x); Y=snd(x)})
                        |> List.pairwise
                        |> List.zip wire.Segments
                        |> List.map (fun x -> fst(snd(x)),snd(snd(x)), fst(x).IntersectOrJumpList)
                        |> List.map (fun (startVertex,endVertex,intersectList) -> {Start=startVertex; End=endVertex; IntersectCoordinateList=intersectList})
                    
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            AbsSegments = getAbsSegments wire
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
//   length defined in Wire.nubLength, and are oriented perpendicular to the symbol edge (i.e. A nub for a port on the Right side
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
/// to be dragged closer than Wire.nubLength
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
                        max distance (dist + Wire.nubLength)
                    else 
                        min distance (dist - Wire.nubLength))

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
let partialAutoroute (wire: Wire) (newPortPos: XYPos) = 
    let segs = wire.Segments
    let newWire = { wire with StartPos = newPortPos }

    /// Returns the manual index and change in port position if partial routing can be performend, else none
    let eligibleForPartialRouting manualIdx =
        let oldStartPos = getPartialRouteStart wire manualIdx
        let newStartPos = getPartialRouteStart newWire manualIdx
        let fixedPoint = getAbsoluteSegmentPos wire manualIdx |> snd
        let relativeToFixed = relativePosition fixedPoint
        if relativeToFixed newStartPos = relativeToFixed oldStartPos then
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
        partialAutoroute (reverseWire wire) newPort
        |> Option.map reverseWire
    else 
        partialAutoroute wire newPort
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
let findJumpIntersects (segStart: XYPos) (segEnd: XYPos) (state: {| Start: XYPos; End: XYPos; JumpsOrIntersections: (float * SegmentId) list |}) (seg: Segment) =
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
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps // Arrays are faster to check than lists - is this even worth it?

    let wires =
        model.Wires
        |> Map.toArray
        |> Array.map snd

    let updateJumpsInWire (segStart: XYPos) (segEnd: XYPos) (wireMap: Map<ConnectionId, Wire>) (seg: Segment) =
        if getSegmentOrientation segStart segEnd = Horizontal then
            ([], wires)
            ||> Array.fold (fun jumpsOrIntersections wire -> 
                if (model.Type = Jump) && not (Array.contains wire.Id wiresWithNoJumpsA) then
                    foldOverSegs findJumpIntersects {| Start = segStart; End = segEnd; JumpsOrIntersections = [] |} wire
                    |> (fun res -> res.JumpsOrIntersections)
                    |> List.append jumpsOrIntersections
                else if (model.Type = Modern) && not (Array.contains wire.Id wiresWithNoJumpsA) then
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
    makeAllJumps wireList model


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

/// Handles messages
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =

    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) ->
        updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let wireWidthFromSymbol = WireWidth.Configured 1
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

        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model)

        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)

    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.Wires
        { model with CopiedWires = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) ->
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

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
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
        let newModel = resetWireSegmentJumps (connectionIds) (model)
        let newWires =
             newModel.Wires
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        { newModel with Wires = newWires }, Cmd.ofMsg BusWidths

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
                | _ -> failwithf $"segment Id {segId} not found in segment list"
            let seg = getSeg model.Wires[connId].Segments
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


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
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
        //printfn $"resetting jumps on {connIds.Length} wires"

        let newModel =
            model
            |> resetWireSegmentJumps connIds

        newModel, Cmd.none

    | MakeJumps connIds ->
        //printfn $"making jumps on {connIds.Length} wires"

        let newModel =
            model
            |> updateWireSegmentJumps connIds

        newModel, Cmd.none

    | ResetModel -> { model with Wires = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none

    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
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
                            { Id = ConnectionId conn.Id
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

    | UpdateWireType (style: WireType) ->
        { model with Type = style }, Cmd.none

    | Rotate (componentIds: ComponentId list) ->
        let updatedWireEntries = 
            componentIds
            |> getConnectedWires model
            |> List.map (autoroute model)
            |> List.map (fun wire -> wire.Id, wire)
            |> Map.ofList
        
        let updatedWires = Map.fold (fun merged id wire -> Map.add id wire merged) model.Wires updatedWireEntries

        { model with Wires = updatedWires }, Cmd.none

    | RerouteWire (portId: string) ->
        let reroutedWire = 
            model.Wires
            |> Map.tryPick (fun _id wire -> 
                if wire.InputPort = InputPortId portId  || wire.OutputPort = OutputPortId portId then
                    Some wire
                else
                    None)
            |> Option.map (autoroute model)

        match reroutedWire with
        | Some wire ->
            { model with Wires = Map.add wire.Id wire model.Wires }, Cmd.none
        | None -> model, Cmd.none

        

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

            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
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
