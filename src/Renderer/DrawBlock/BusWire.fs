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
open BlockHelpers

open DrawModelType.SymbolT
open DrawModelType.BusWireT




//------------------------------------------------------------------------//
//------------------------------BusWire Constants-------------------------//
//------------------------------------------------------------------------//

[<AutoOpen>]
module Constants =
    /// default style of routing
    let initialWireType = Radial
    /// default arrow display
    let initialArrowDisplay = true
    let jumpRadius: float = 5.
    /// The minimum length of the initial segments (nubs) leaving the ports.
    /// Must be larger than SmartWire.minWireSeparation
    let nubLength: float = 10.
    /// The standard radius of a radial wire corner
    let cornerRadius: float  = 7. 
    /// The standard radius of a modern wire connect circle
    let modernCircleRadius: float = 3.
    /// How close same net vertices must be before they are joined by modern routing circles
    let modernCirclePositionTolerance : float = 2.

    /// determines how exactly connections must fit components before being autorouted on load
    let vertexLoadMatchTolerance = 0.01

    let busWidthTextStyle =
        {
            TextAnchor = "left";
            FontSize = "12px";
            FontWeight = "Bold";
            FontFamily = "Verdana, Arial, Helvetica, sans-serif";
            Fill = "" // will be filled in later
            UserSelect = UserSelectOptions.None;
            DominantBaseline = "middle";
        }


//-----------------------------------------------------------------------------//
//-------------------------Debugging functions---------------------------------//
//-----------------------------------------------------------------------------//

/// Formats a SegmentId for logging purposes.
let formatSegmentId ((index,wid): SegmentId) =
    let (ConnectionId str) = wid
    $"{index}:{str[0..2]}"

/// Formats a WireId for logging purposes
let formatWireId (id: ConnectionId) =
    id
    |> (fun (ConnectionId str) -> str)
    |> (fun str -> str[0..2])

let logSegmentId (seg:Segment) =
    let (ConnectionId wIdStr) = seg.WireId
    $"{wIdStr[0..3]}:{seg.Index}"

/// Logs the given Segment and returns it unchanged. Used for debugging.
let logSegment (seg:Segment) =
    printfn $"|{logSegmentId seg}|-Length: {seg.Length}"; seg

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
                |> List.map (fun (_) -> logSegmentId segment))

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
        let entry = sprintf $"|{seg.Index}:{logSegmentId seg}| Start: {formatXY startPos}, End: {formatXY endPos}"
        String.concat "\n" [state; entry]
    let start = sprintf $"Wire: {formatWireId wire.WId}"
    printfn $"{foldOverSegs formatSegments start wire}"
    wire

let inline getSegmentFromId (model: Model) (segId:SegmentId) =
    let index,Wid = segId
    model.Wires[Wid].Segments[index]

let inline getASegmentFromId (model: Model) (segId:SegmentId) =
    let index, Wid = segId
    let getASeg startPos endPos state (seg:Segment) =
        if seg.Index = index then Some {Start=startPos; End=endPos;Segment=seg} else state
    foldOverNonZeroSegs  getASeg None model.Wires[Wid]
    |> Option.get

/// Given a segment start and end position, finds the orientation of the segment. 
/// Fails if the segment is neither horizontal nor vertical
let inline getSegmentOrientation (segStart: XYPos) (segEnd: XYPos) =
    if abs (segStart.X - segEnd.X) < XYPos.epsilon then
        Vertical
    else if abs (segStart.Y - segEnd.Y) < XYPos.epsilon then
        Horizontal
    else
        failwithf "ERROR: Diagonal wire" // Should never happen

/// Given a segment start and end position, finds the orientation of the segment. 
/// Returns None if the segment is 0 length
let inline getSegmentOrientationOpt (segStart: XYPos) (segEnd: XYPos) =
    if abs (segStart.X - segEnd.X) < XYPos.epsilon then
        Some Vertical
    else if abs (segStart.Y - segEnd.Y) < XYPos.epsilon then
        Some Horizontal
    else
        None
/// Get the coordinate fixed in an ASegment. NB - ASegments can't be zero length
let inline getFixedCoord (aSeg: ASegment) =
    let ori = getSegmentOrientation aSeg.Start aSeg.End
    match ori with | Vertical -> aSeg.Start.X | Horizontal -> aSeg.Start.Y


/// Given a segment start and end position, finds the symbol edge of the segment
/// based on its segment direction if it starts an outgoing wire.
let inline getWireOutgoingEdge (wire:Wire) =
    match wire.InitialOrientation, wire.Segments.[0].Length > 0 with
    | Horizontal, true -> Edge.Right
    | Horizontal, false -> Edge.Left
    | Vertical, true -> Edge.Bottom
    | Vertical, false -> Edge.Top
     
/// Tries to find and log a segment identified by index in a wire identified by wireId in the current model.
/// Assumes wireId can be found in the current model. Returns unit, used for debugging.
let logSegmentInModel model wireId index  = 
        let wire = model.Wires[wireId]
        let findAndFormatSeg segStart segEnd (_state: string option) (seg: Segment) =
            if seg.Index = index then 
                let orientation = 
                    match getSegmentOrientation segStart segEnd with
                    | Vertical -> "V"
                    | Horizontal -> "H"
                Some (sprintf $"""[{logSegmentId seg}: {formatXY segStart}->{formatXY segEnd}]-{orientation}-{seg.Index}""")
            else None

        match foldOverSegs findAndFormatSeg None wire with
        | Some str -> printfn $"{str}"
        | _ -> printfn $"ERROR: Could not find segment {index} in wire {formatWireId wireId}"
        


/// Tries to find and log each segment to its corresponding wire identified in wireSegmentIdPairs in the current model.
/// Returns the model unchanged. Used for debugging.
let logSegmentsInModel (model: Model) (wireSegmentIdPairs: (int*ConnectionId) list)= 
    wireSegmentIdPairs
    |> List.map  ( fun (index,wireId) -> logSegmentInModel model wireId)
    |> ignore
    model




//-------------------------------Implementation code----------------------------//

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the final port orientation 
/// this function returns a list of wire vertices.
/// The starting segment will always be from a Right Edge (and so in increasing X direction)
let makeInitialWireVerticesList (wireStartPos : XYPos) (wireEndPos : XYPos) (portOrientation : Edge) = 
    let xStart, yStart, xEnd, yEnd = wireStartPos.X, wireStartPos.Y, wireEndPos.X, wireEndPos.Y

    let nubLength =
        let xDelta = xEnd - xStart
        let yDelta = abs (yEnd - yStart)
        match portOrientation with
        | CommonTypes.Edge.Left when xDelta > 0 -> min nubLength (xDelta / 2.)
        | CommonTypes.Edge.Top
        | CommonTypes.Edge.Bottom when xDelta > 0 -> min nubLength xDelta
        | _ -> nubLength
    /// This is a fixed-length horizontal stick with a zero-length vertical after it.
    /// It starts nearly all the wires
    let rightNub = [
            {X = xStart; Y = yStart};
            {X = xStart+nubLength; Y = yStart}; //Stick horizontal
            {X = xStart+nubLength; Y = yStart}; //Length 0 vertical
        ]
    let rightwards = xStart - xEnd  < 0
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
            {
                Index = i
                Length = xEnd - xStart + yEnd - yStart
                WireId  = connId;
                IntersectOrJumpList = [] ; // To test jump and modern wire types need to manually insert elements into this list.
                Mode = Auto
                Draggable = not (i = 0 || i = xyVerticesList.Length - 2) //First and Last should not be draggable
            })


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the orientation of the final port
/// this function returns a list of Segment(s).
/// The starting segment will always be from a Right Edge (and so in increasing X direction)
let makeInitialSegmentsList 
        (hostId : ConnectionId) 
        (startPos : XYPos) 
        (endPos : XYPos) 
        (portOrientation : Edge) 
            : list<Segment> =
    makeInitialWireVerticesList startPos endPos portOrientation
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
        let segT (v1:XYPos) (v2: XYPos) =
            let delta = v1 - v2
            if abs delta.X + abs delta.Y < XYPos.epsilon then
                None
            elif abs delta.Y < XYPos.epsilon then
                Some Horizontal
            elif abs delta.X < XYPos.epsilon then
                Some Vertical
            else 
                failwithf "Diagonal vertices read in Wire"

        let makeCorrectOrientationPairs (verts: {|Mode: RoutingMode; Pos: XYPos|} list) =
            match verts with
            | v1 :: v2 :: lst ->
                (((v2,v1), []), lst)
                ||> List.fold (fun ((v2,v1),vL) v3 ->
                    match segT v1.Pos v2.Pos, segT v2.Pos v3.Pos with
                    | None, _ -> (v3,v2),vL
                    | _, None -> (v2,v1), vL
                    | Some d1, Some d2 when d1 = d2 -> (v3,v1),vL
                    | _ -> (v3,v2), (v2,v1) :: vL)
                |> (fun ((v2,v1),vL) -> 
                    if segT v2.Pos v1.Pos = None then 
                        vL 
                    else
                        (v2,v1) :: vL)
            | _ -> []
            |> List.rev
        xyVerticesList
        |> makeCorrectOrientationPairs
        |> List.mapi (
            fun i (endVertex, startVertex) -> 
                {
                    Index = i
                    Length = endVertex.Pos.X-startVertex.Pos.X+endVertex.Pos.Y-startVertex.Pos.Y
                    WireId  = connId;
                    IntersectOrJumpList = [] ; // To test jump and modern wire types need to manually insert elements into this list.
                    Mode = endVertex.Mode
                    Draggable =
                        if i = 0 || i = xyVerticesList.Length - 2 then //First and Last should not be draggable
                            false
                        else
                            true
                })
        
    verticesToSegments connId verticesList'



/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.Wires[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.WId, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
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

//-----------------------------------------------------------------------------------------//
//----------------------------------Rendering Functions------------------------------------//
//-----------------------------------------------------------------------------------------//



/// Type passed to wire renderer functions.
/// The data here is cached by React and if the same as last time
/// the render function itself is not called.
type WireRenderProps =
    {
        key: string
        Wire: Wire
        ColorP: HighLightColor
        StrokeWidthP: float
        OutputPortEdge : Edge
        OutputPortLocation: XYPos
        DisplayType : WireType
        ArrowDisplay: bool
        TriangleEdge : Edge
        InputPortLocation: XYPos
    }

let renderWireWidthText (props: WireRenderProps): ReactElement =
    let textStyle = 
        { Constants.busWidthTextStyle with Fill = props.ColorP.Text();}

    let text = if props.Wire.Width = 1 then "" else string props.Wire.Width //Only print width > 1
    let outPos = props.OutputPortLocation
    let yOffset = TextOffset.yOffset
    let xOffset = TextOffset.xOffset
    let xLeftOffset = TextOffset.xLeftOffset
    match props.OutputPortEdge with 
    | CommonTypes.Top -> makeText (outPos.X + xOffset) (outPos.Y - yOffset) text textStyle
    | CommonTypes.Bottom -> makeText (outPos.X + xOffset) (outPos.Y + yOffset) text textStyle
    | CommonTypes.Right -> makeText (outPos.X + xOffset) (outPos.Y - yOffset) text textStyle
    | CommonTypes.Left -> makeText (outPos.X - xLeftOffset) (outPos.Y - yOffset) text textStyle

/// Creates the SVG command string required to render the wire
/// (apart from the final "nub") with a radial display type 
let renderRadialWireSVG 
    (state : (string * Orientation)) 
    (segmentpair : {| First : ASegment; Second :ASegment|}) 
    : string * Orientation =
    
    let seg1Start = segmentpair.First.Start
    let seg1End = segmentpair.First.End
    let seg2Start = segmentpair.Second.Start
    let seg2End = segmentpair.Second.End
    
    let dist1 = euclideanDistance seg1Start seg1End
    let dist2 = euclideanDistance seg2Start seg2End
    let rad = System.Math.Floor(min Constants.cornerRadius (max 0.0 (min (dist1/2.) (dist2/2.))))
    let makeCommandString xStart yStart rad sweepflag xEnd yEnd : string =
        $"L {xStart} {yStart} A {rad} {rad}, 45, 0, {sweepflag}, {xEnd} {yEnd}" 

    //Checking if horizontal followed by length 0 vertical
    if seg1Start.X = seg1End.X && 
       seg1Start.X = seg2Start.X &&
       seg1Start.X = seg2End.X then
        let current = sprintf "L %f %f" seg1End.X seg1End.Y
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)
    //Checking if vertical followed by length 0 horizontal
    else if seg1Start.Y = seg1End.Y && 
            seg1Start.Y = seg2Start.Y && 
            seg1Start.Y = seg2End.Y then
        let current = sprintf "L %f %f" (seg1End.X) (seg1End.Y)
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)           
    
    else
        if snd(state) = Horizontal then
            if seg1Start.X - seg1End.X > 0 then
                if seg2Start.Y - seg2End.Y > 0 then
                    let current:string = makeCommandString (seg1End.X+rad) seg1End.Y rad 1 seg2Start.X (seg2Start.Y-rad)
                    ((fst(state)+current), Vertical)
                else
                    let current:string  =  makeCommandString (seg1End.X+rad) seg1End.Y rad 0 seg2Start.X (seg2Start.Y+rad)
                    ((fst(state)+current), Vertical)
            else
                if seg2Start.Y - seg2End.Y > 0 then
                    let current:string =  makeCommandString (seg1End.X-rad)seg1End.Y rad 0 seg2Start.X (seg2Start.Y-rad)
                    ((fst(state)+current), Vertical)
                else
                    let current:string = makeCommandString (seg1End.X-rad) seg1End.Y rad 1 seg2Start.X (seg2Start.Y+rad)
                    ((fst(state)+current), Vertical)
        else
            if seg1Start.Y - seg1End.Y > 0 then
                if seg2Start.X - seg2End.X > 0 then
                    let current :string =  makeCommandString seg1End.X (seg1End.Y+rad) rad 0 (seg2Start.X-rad) seg2Start.Y
                    ((fst(state)+current), Horizontal)
                else
                    let current :string =  makeCommandString seg1End.X (seg1End.Y+rad) rad 1 (seg2Start.X+rad) seg2Start.Y
                    ((fst(state)+current), Horizontal)
            else
                if seg2Start.X - seg2End.X > 0 then
                    let current :string =  makeCommandString seg1End.X (seg1End.Y-rad) rad 1 (seg2Start.X-rad) seg2Start.Y
                    ((fst(state)+current), Horizontal)
                else
                    let current :string =  makeCommandString seg1End.X (seg1End.Y-rad) rad  0 (seg2Start.X+rad) seg2Start.Y
                    ((fst(state)+current), Horizontal)


 
let renderModernWire (props:WireRenderProps) =
    let colour = props.ColorP.Text()

    let segments = getAbsSegments props.Wire

    let lineAttr = 
        segments
        |> List.map (fun seg -> $"L %.2f{seg.End.X} %.2f{seg.End.Y}")
        |> String.concat " "

    let pathPars:Path =
        { defaultPath with
            Stroke = colour
            StrokeWidth = string props.StrokeWidthP
        }

    let circleParameters = { defaultCircle with R = Constants.modernCircleRadius; Stroke = colour;  Fill = colour }

    let circles segments =
        segments
        |> List.collect (fun aseg ->
            let seg = aseg.Segment
            seg.IntersectOrJumpList 
            |> List.map (fun x -> makeCircle x aseg.Start.Y circleParameters))

    g [] (makeAnyPath segments[0].Start lineAttr pathPars :: circles segments)

        

let renderJumpSegment (a:ASegment) : string list=
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
        | xJ:: _ when abs (xJ - xPos) > jR ->
            makeLineAttr (xJ - xPos - dir*jR) 0. :: makeJumpPathAttr jLst (xJ - dir*(jR - XYPos.epsilon))
        | [xJ] when abs (ePos.X - xJ) < jR ->
            [ makePartArc (xJ - xPos) (ePos.X - xJ) ]
        | [xJ] ->
            makePartArc (xJ - xPos) (dir*jR) :: makeJumpPathAttr [] (xJ + dir * jR)
        | xJ :: ((yJ :: _) as jLst') when abs (yJ - xJ) > 2. * jR ->
            makePartArc (xJ - xPos) (dir*jR) :: makeJumpPathAttr jLst' (xJ + dir*jR)
        | xJ :: ((yJ :: _) as jLst') ->
            makePartArc (xJ - xPos) ((yJ - xJ) / 2.0) :: makeJumpPathAttr jLst' ((yJ+xJ)/ 2.0)
    let jLst =
        match rightTravel, a.Segment.IntersectOrJumpList with
        | true, jL -> jL |> List.sort
        | false, jL -> jL |> List.sort
        |> List.map (fun f -> f*dir + sPos.X)
    match jLst, abs (sPos.X - ePos.X) < XYPos.epsilon with
    | _, true
    | [], false -> 
        [$"L {ePos.X} {ePos.Y}"]
    | jLst, false -> 
        makeJumpPathAttr jLst sPos.X
       
///Function used to render a single wire if the display type is jump
let renderJumpWire props = 
    let absSegments = getAbsSegments props.Wire
    let firstVertex = absSegments.Head.Start
    let colour = props.ColorP.Text()

    
    let renderedSegmentList : ReactElement List = 
        let pathPars:Path =
            { defaultPath with
                Stroke = colour
                StrokeWidth = string props.StrokeWidthP
            }
        absSegments
        |> List.collect renderJumpSegment
        |> String.concat " "
        |> (fun attr -> [makeAnyPath firstVertex attr pathPars])

    g [] ([ renderWireWidthText props] @ renderedSegmentList)

///Function used to render a single wire if the display type is radial
let renderRadialWire props =
    let absSegments = getAbsSegments props.Wire
    let firstVertex = absSegments.Head.Start
    let secondVertex = absSegments.Head.End
    let lastVertex = (List.last absSegments).End

    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width

    let pathParameters = { defaultPath with Stroke = props.ColorP.Text(); StrokeWidth = width;}
    let initialMoveCommand = sprintf "M %f %f "  firstVertex.X firstVertex.Y
    let initialState = (initialMoveCommand, getSegmentOrientation firstVertex secondVertex )
    
    let radialPathCommands = fst(
        absSegments
        |> List.pairwise
        |> List.map (fun x -> ( {| First = fst(x); Second = snd(x) |}))
        |> List.fold renderRadialWireSVG (initialState) )
    let finalLineCommand = sprintf "L %f %f" lastVertex.X lastVertex.Y
    let fullPathCommand = radialPathCommands + finalLineCommand

    let renderedSVGPath = makePathFromAttr fullPathCommand pathParameters

    g [] ([ renderWireWidthText props] @ [renderedSVGPath])

/// Function that will render all of the wires within the model, with the display type being set in Model.Type
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wireProps wire =
        let outPortId = getOutputPortIdStr wire.OutputPort
        let outputPortLocation = Symbol.getPortLocation None model.Symbol outPortId
        let outputPortEdge = getOutputPortOrientation model.Symbol wire.OutputPort 
        let stringInId = getInputPortIdStr wire.InputPort
        let inputPortLocation = Symbol.getPortLocation None model.Symbol stringInId 
        let strokeWidthP =
            match wire.Width with
            | 1 -> 1.5
            | n when n < 8 -> 2.5
            | _ -> 3.0
        {
            key = match wire.WId with | ConnectionId s -> s
            Wire = wire
            ColorP = wire.Color
            StrokeWidthP = strokeWidthP 
            OutputPortEdge = outputPortEdge
            OutputPortLocation = outputPortLocation
            DisplayType = model.Type
            ArrowDisplay = model.ArrowDisplay
            TriangleEdge = getInputPortOrientation model.Symbol wire.InputPort
            InputPortLocation = inputPortLocation
        }
        
    let renderWire = 
        FunctionComponent.Of(
            fun (props : WireRenderProps) ->
                let wireReact =
                    match props.DisplayType with    
                    | Radial -> renderRadialWire props
                    | Jump -> renderJumpWire props
                    | Modern -> renderModernWire props
                let polygon = {
                    defaultPolygon with
                        Fill = "black"
                        }
                let x,y = props.InputPortLocation.X, props.InputPortLocation.Y
                let ws = min 2.5 props.StrokeWidthP
                let str:string = 
                    match props.TriangleEdge with
                    | CommonTypes.Top -> $"{x},{y},{x+ws},{y-2.*ws},{x-ws},{y-2.*ws}"
                    | CommonTypes.Bottom -> $"{x},{y},{x+ws},{y+2.*ws},{x-ws},{y+2.*ws}"
                    | CommonTypes.Right -> $"{x},{y},{x+2.*ws},{y+ws},{x+2.*ws},{y-ws}"
                    | CommonTypes.Left -> $"{x},{y},{x-2.*ws},{y+ws},{x-2.*ws},{y-ws}"
                let arrows: ReactElement list =
                    match props.ArrowDisplay with
                    | true -> [makePolygon str polygon]
                    | false -> []
                g [] (arrows @ [wireReact ])          
            , "Wire"
            , equalsButFunctions
        )
    
    let symbols = SymbolView.view model.Symbol (Symbol >> dispatch)
    let wires =
        model.Wires
        |> Map.toList 
        |> List.map (fun (_,wire) -> renderWire (wireProps wire))
    g [] (symbols :: wires)
    //|> TimeHelpers.instrumentInterval "WireView" start

