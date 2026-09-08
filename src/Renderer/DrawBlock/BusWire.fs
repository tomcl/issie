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

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT




//------------------------------------------------------------------------//
//------------------------------BusWire Constants-------------------------//
//------------------------------------------------------------------------//

[<AutoOpen>]
module Constants =
    /// if true display wire segments in colours to aid debugging
    /// See rainbowColours for key
    /// coloured wires are always displayed in modern format without circles
    let debugWireSegments = false
    /// the rainbow colors of the wire segments, when debugWireSegments = true
    /// NB invisible zero-length segments will cause a colour to be skipped
    let rainbowColours = [
        "#FF0000"; // red index=0
        "#FF7F00"; // orange index=1
        "#FFFF00"; // yellow index=2
        "#00FF00"; // green index = 3
        "#00A0A0"; // turquoise index = 4
        "#1010FF"; // blue index = 5
        "#9B50FF"; // violet index = 6
        "#FF00FF"; // magenta index = 7"
        // larger indices are displayed as black
        ]
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

// A shelf of print-and-return-unchanged combinators lived here - logSegment, logWire,
// logIntersectionMaps, logSegmentInModel and the formatters they used - meant to be spliced
// temporarily into a pipeline. Every one of them had been left with no callers. Splice a
// Log.dbg Log.Wire line in when a wire misbehaves; a permanent shelf of them is not needed.

/// Identifies a segment as wire:index, short enough to read in a log line.
let logSegmentId (seg:Segment) =
    let (ConnectionId wId) = seg.WireId
    $"{wId}:{seg.Index}"

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
     
//-------------------------------Implementation code----------------------------//

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the final port orientation 
/// this function returns a list of wire vertices.
/// The starting segment will always be from a Right Edge (and so in increasing X direction)
/// `startInset` and `endInset` are how far each port lies INSIDE its own symbol's bounding box,
/// which is what routing and separation both use as the obstacle. A port normally sits on that
/// box and the inset is zero, leaving the nubs the length they have always been.
///
/// A Mux2's Sel is the case that needs it. The symbol is drawn as a trapezium, so the port is
/// nine units inside the rectangle - the wire's first turn then lands INSIDE the symbol as far
/// as the obstacle model is concerned, and separation, which cannot tell that the constraint is
/// one no wire could satisfy, moves the segment clear by taking it out through the far side.
/// Running straight until the box is behind us makes the situation not arise; the outline model
/// itself is left alone, which is the point - a shape that says WHERE it is thin would have to
/// be understood by routing, separation and every test that measures them.
let makeInitialWireVerticesList (wireStartPos : XYPos) (wireEndPos : XYPos) (portOrientation : Edge)
                                (startInset : float) (endInset : float) = 
    let xStart, yStart, xEnd, yEnd = wireStartPos.X, wireStartPos.Y, wireEndPos.X, wireEndPos.Y

    /// How much of the ordinary nub the space between the ports leaves room for. Close ports get
    /// a shorter nub so that the templates below have somewhere to put their vertices.
    let roomForNub =
        let xDelta = xEnd - xStart
        match portOrientation with
        | CommonTypes.Edge.Left when xDelta > 0 -> min nubLength (xDelta / 2.)
        | CommonTypes.Edge.Top
        | CommonTypes.Edge.Bottom when xDelta > 0 -> min nubLength xDelta
        | _ -> nubLength
    /// The nub at the driving end, and the one at the port this wire ends on.
    ///
    /// The inset is a MINIMUM and not an addition, and that distinction is the whole of it. The
    /// templates below place their detour vertices a nub`s length from the port, so a nub LONGER
    /// than the ordinary one puts two vertices in the wrong order and the wire doubles back - a
    /// spike, which autorouting should never draw. A minimum cannot do that: an inset is smaller
    /// than the nub (nine against ten for a Mux2 SEL), so all this does is stop the shortening
    /// above taking the nub below the length that gets it clear of its own symbol.
    ///
    /// Capped at the ordinary nub for the same reason, in case a symbol ever has a port inset
    /// further than that: the templates are built around a nub of this length and no more.
    let startNub, endNub =
        let atLeast (inset: float) = max roomForNub (min nubLength inset)
        atLeast startInset, atLeast endInset
    /// This is a fixed-length horizontal stick with a zero-length vertical after it.
    /// It starts nearly all the wires
    let rightNub = [
            {X = xStart; Y = yStart};
            {X = xStart+startNub; Y = yStart}; //Stick horizontal
            {X = xStart+startNub; Y = yStart}; //Length 0 vertical
        ]
    let rightwards = xStart - xEnd  < 0
    let downwards = yStart - yEnd  < 0
    match rightwards, downwards with //add 20 to prevent issues in the case that the ports are directly on in line with one another
    | true, true ->
            match portOrientation with
            | CommonTypes.Top  ->  rightNub @ [
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd-endNub}; 
                    {X = xEnd; Y = yEnd-endNub};// Length 0 horizontal
                    {X = xEnd; Y = yEnd}] // Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+endNub; Y = yEnd}; 
                    {X = xEnd+endNub; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+endNub}; 
                    {X = xEnd; Y = yEnd+endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-endNub; Y = yEnd}; 
                    {X = xEnd-endNub; Y = yEnd}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
    | true, false -> 
            match portOrientation with
            | CommonTypes.Bottom ->  rightNub @ [
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd+endNub}; 
                    {X = xEnd; Y = yEnd+endNub}; //Length 0 hortizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xEnd+10.; Y = yStart};
                    {X = xEnd+10.; Y = yEnd};
                    {X = xEnd+endNub; Y = yEnd}; 
                    {X = xEnd+endNub; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-endNub}; 
                    {X = xEnd; Y = yEnd-endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-endNub; Y = yEnd}; 
                    {X = xEnd-endNub; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
    | false, true -> 
            match portOrientation with
            | CommonTypes.Bottom ->  rightNub @ [
                    {X = xStart+startNub+10.; Y = yStart}; //Small horizontal for dragging  
                    {X = xStart+startNub+10.; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+10.};
                    {X = xEnd; Y = yEnd+endNub}; 
                    {X = xEnd; Y = yEnd+endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xStart+startNub+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+startNub+10.; Y = yEnd};
                    {X = xEnd+endNub; Y = yEnd}; 
                    {X = xEnd+endNub; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+startNub; Y = yStart}; //Stick horizontal
                    {X = xStart+startNub; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-endNub}; 
                    {X = xEnd; Y = yEnd-endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = xStart+startNub+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+startNub+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-endNub; Y = yEnd}; 
                    {X = xEnd-endNub; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
        | false, false -> 
            match portOrientation with
            | CommonTypes.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+startNub; Y = yStart}; //Stick horizontal
                    {X = xStart+startNub; Y = yEnd-10.}; //Length 0 vertical
                    {X = xEnd; Y = yEnd-10.};
                    {X = xEnd; Y = yEnd-endNub}; 
                    {X = xEnd; Y = yEnd-endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Right ->  rightNub @ [
                    {X = xStart+startNub+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+startNub+10.; Y = yEnd};
                    {X = xEnd+endNub; Y = yEnd}; 
                    {X = xEnd+endNub; Y = yEnd}; //Lenght 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | CommonTypes.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+startNub; Y = yStart}; //Stick horizontal
                    {X = xStart+startNub; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd+endNub}; 
                    {X = xEnd; Y = yEnd+endNub}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | CommonTypes.Left ->  rightNub @ [
                    {X = xStart+startNub+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+startNub+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-10.; Y = yEnd};
                    {X = xEnd-endNub; Y = yEnd}; 
                    {X = xEnd-endNub; Y = yEnd}; //Length 0 vertical
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
        (startInset : float) 
        (endInset : float) 
            : list<Segment> =
    makeInitialWireVerticesList startPos endPos portOrientation startInset endInset
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
    let InputPortId strInputPort, OutputPortId strOutputPort = conn.InputPort, conn.OutputPort
    {
        Id = conn.WId
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

/// A maximal straight piece of a wire: all of it between two consecutive bends of the shape that
/// is drawn. A wire's segment list is not that shape - it holds zero-length segments, and splits
/// one straight piece over several segments - so radial rendering must work on runs and not on
/// segments, or it rounds a corner where the wire is straight and squares one off where it bends.
type WireRun = {
        RunStart: XYPos
        RunEnd: XYPos
        RunOrientation: Orientation
    }
    with member inline this.Length = euclideanDistance this.RunStart this.RunEnd

/// Collapse a wire's absolute segments into the runs between its bends: drop the zero-length
/// segments, which have no shape, and merge whatever is then adjacent and collinear.
let segmentRuns (absSegments: ASegment list) : WireRun list =
    absSegments
    |> List.filter (fun aSeg -> not aSeg.IsZero)
    |> List.fold (fun runs (aSeg: ASegment) ->
        match runs with
        | run :: earlier when run.RunOrientation = aSeg.Orientation ->
            { run with RunEnd = aSeg.End } :: earlier
        | _ ->
            { RunStart = aSeg.Start; RunEnd = aSeg.End; RunOrientation = aSeg.Orientation } :: runs)
        []
    |> List.rev

/// The bends of a radial wire, each paired with the radius its corner is drawn with. A bend eats
/// `rad` off the end of the run either side of it, so a run with a bend at both ends can give half
/// of itself to each, and one at either end of the wire has a single bend and can give all of
/// itself. The radius is therefore the standard one everywhere except where there is genuinely not
/// the room for it: neighbouring bends never overlap, and a short run is the only thing that
/// squares a corner off.
let wireBends (runs: WireRun list) : (WireRun * WireRun * float) list =
    let lastRun = List.length runs - 1
    let room =
        runs
        |> List.mapi (fun i run -> if i = 0 || i = lastRun then run.Length else run.Length / 2.)
    List.zip (List.pairwise runs) (List.pairwise room)
    |> List.map (fun ((before, after), (roomBefore, roomAfter)) ->
        before, after, List.min [Constants.cornerRadius; roomBefore; roomAfter])

/// The SVG commands drawing one bend of a radial wire: the line along `before` as far as where the
/// corner starts to be rounded off, then the quarter circle of radius `rad` onto `after`.
let renderRadialBend (before: WireRun) (after: WireRun) (rad: float) : string =
    let corner = before.RunEnd
    let directionOf (fromCoord: float) (toCoord: float) = if toCoord > fromCoord then 1. else -1.
    let arc (bendStart: XYPos) (sweep: int) (bendEnd: XYPos) =
        sprintf "L %f %f A %f %f, 45, 0, %d, %f %f"
            bendStart.X bendStart.Y rad rad sweep bendEnd.X bendEnd.Y
    match before.RunOrientation with
    | Horizontal ->
        let xDir = directionOf before.RunStart.X corner.X
        let yDir = directionOf corner.Y after.RunEnd.Y
        let sweep = if xDir * yDir > 0. then 1 else 0
        arc { corner with X = corner.X - xDir * rad } sweep { corner with Y = corner.Y + yDir * rad }
    | Vertical ->
        let yDir = directionOf before.RunStart.Y corner.Y
        let xDir = directionOf corner.X after.RunEnd.X
        let sweep = if xDir * yDir > 0. then 0 else 1
        arc { corner with Y = corner.Y - yDir * rad } sweep { corner with X = corner.X + xDir * rad }

let renderModernWire (props:WireRenderProps) =
    let colour = props.ColorP.Text()

    let segments = getAbsSegments props.Wire

    let lineAttr = 
        segments
        |> List.map (fun seg -> $"L %.2f{seg.End.X} %.2f{seg.End.Y}")
        |> String.concat " "

    let lineSVG (seg: ASegment) =
        let colour =
            List.tryItem seg.Segment.Index Constants.rainbowColours
            |> Option.defaultValue "#000000"
        let p1 = seg.Start
        let p2 = seg.End
        line [
                X1 p1.X
                Y1 p1.Y
                X2 p2.X
                Y2 p2.Y
                SVGAttr.Stroke colour
                SVGAttr.StrokeWidth (string props.StrokeWidthP)
            ] []

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

    if Constants.debugWireSegments then
        g [] (segments |> List.map lineSVG)
    else
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
            failwithf $"d1={d1}, d2={d2}, jR={jR}"
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
    let lastVertex = (List.last absSegments).End

    let width = string props.StrokeWidthP
    let pathParameters = { defaultPath with Stroke = props.ColorP.Text(); StrokeWidth = width;}

    let bendCommands =
        absSegments
        |> segmentRuns
        |> wireBends
        |> List.map (fun (before, after, rad) -> renderRadialBend before after rad)
        |> String.concat " "

    let fullPathCommand =
        sprintf "M %f %f %s L %f %f" firstVertex.X firstVertex.Y bendCommands lastVertex.X lastVertex.Y

    let renderedSVGPath = makePathFromAttr fullPathCommand pathParameters

    g [] ([ renderWireWidthText props] @ [renderedSVGPath])

/// Function that will render all of the wires within the model, with the display type being set in Model.Type
///
/// `highlighted` says what to show picked out for a reason the draw block cannot know - see
/// DrawModelType.Highlighted. It is applied to the render props rather than to the model, so a
/// wire whose highlight changes redraws and the rest are left to React's memoisation.
let view (model : Model) (highlighted: Highlighted) (dispatch : Dispatch<Msg>) =
    // "WirePropsSort" was instrumented on the line after its start time was taken, so it measured
    // nothing - once per render
    let rStart = TimeHelpers.getTimeMs()
    let wireProps wire =
        let outPortId = portIdOfOutput wire.OutputPort
        let outputPortLocation = Symbol.getPortLocation None model.Symbol outPortId
        let outputPortEdge = getOutputPortOrientation model.Symbol wire.OutputPort 
        let inPortId = portIdOfInput wire.InputPort
        let inputPortLocation = Symbol.getPortLocation None model.Symbol inPortId 
        let strokeWidthP =
            match wire.Width with
            | 1 -> 1.5
            | n when n < 8 -> 2.5
            | _ -> 3.0
        {
            key = match wire.WId with | ConnectionId s -> string s
            Wire = wire
            ColorP = if Set.contains wire.WId highlighted.HConns then HighLightColor.SkyBlue else wire.Color
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
                    match props.DisplayType, Constants.debugWireSegments with    
                    | Radial, false -> renderRadialWire props
                    | Jump,   false -> renderJumpWire props
                    | Modern, false 
                    | _,      true -> renderModernWire props
                // The same colour as the wire it ends. The arrow is part of the wire and reads as
                // one thing with it, so drawn black it stayed black while the wire it belonged to
                // went blue for being selected or red for a width error - which said, at the one
                // end of the wire the eye goes to, that something there was not selected.
                let wireColour = props.ColorP.Text()
                let polygon = {
                    defaultPolygon with
                        Fill = wireColour
                        Stroke = wireColour
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
    
    let symbols = SymbolView.view model.Symbol highlighted.HComps (Symbol >> dispatch)
    let wires =
        model.Wires
        |> Map.toList 
        |> List.map (fun (_,wire) -> renderWire (wireProps wire))
    g [] (symbols :: wires)
    //|> TimeHelpers.instrumentInterval "WireView" start

