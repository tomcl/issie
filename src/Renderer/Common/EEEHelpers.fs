module EEEHelpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Electron
open Fable.React

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

type BoundingBox = {
    X: float
    Y: float
    W: float
    H: float
}

type PortLocation = {
    X: float
    Y: float
    R: float
}

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag

type MouseT = {
    Pos: XYPos
    Op: MouseOp}

/// Record to help create SVG lines
type Line = {
    Stroke: string
    StrokeWidth: string
    StrokeDashArray: string
}

/// Record to help create SVG paths (for wire segment jumps ONLY)
type Path = {
    Stroke: string
    StrokeWidth: string
    StrokeDashArray: string
    StrokeLinecap: string
    Fill: string
}

/// Record to help create SVG polygons
type Polygon = {
    Stroke: string
    StrokeWidth: string
    FillOpacity: float
    Fill: string
}

/// Record to help create SVG circles
type Circle = {
    /// Radius of the circle
    R: float 
    Stroke: string
    StrokeWidth: string
    FillOpacity: float
    Fill: string
}

/// Record to help create SVG text
type Text = {
    /// left/right/middle: horizontal algnment vs (X,Y)
    TextAnchor: string
    FontSize: string
    FontWeight: string
    FontFamily: string
    Fill: string
    UserSelect: UserSelectOptions
    /// auto/middle/hanging: vertical alignment vs (X,Y)
    DominantBaseline: string
}


/// Default line, change this one to create new lines
let defaultLine = {
    Stroke = "Black"
    StrokeWidth = "1px"
    StrokeDashArray = "None"
}

/// Default path, change this one to create new paths
let defaultPath = {
    Stroke = "Black"
    StrokeWidth = "1px"
    StrokeDashArray = "None"
    StrokeLinecap = "butt"
    Fill = "transparent"
}

/// Default polygon, change this one to create new polygons
let defaultPolygon = {
    Stroke = "Black"
    StrokeWidth = "1px"
    FillOpacity = 1.0
    Fill = "None"
}

/// Default circle, change this one to create new circles
let defaultCircle = {
    R = 5.0
    Stroke = "Black"
    StrokeWidth = "1px"
    FillOpacity = 1.0
    Fill = "None"
}

/// Default text, change this to create new text types
let defaultText = {
    TextAnchor = "Middle"
    FontSize = "10px"
    FontFamily = "Verdana, Arial, Helvetica, sans-serif" // Change font family to something good
    FontWeight = "Normal"
    Fill = "Black"
    UserSelect = UserSelectOptions.None
    DominantBaseline = "Hanging"
}

/// Port circle, used by both Sheet and Symbol to create ports
let portCircle = { defaultCircle with R = 5.0; Stroke = "Black"; StrokeWidth = "1.0px"; Fill = "Grey"}
//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"

// ----------------------------- SVG Helpers ----------------------------- //

/// Makes a line ReactElement, wildcard inputs as position can be a number or a string 
let makeLine (x1: 'a) (y1: 'b) (x2: 'c) (y2: 'd) (lineParameters: Line) =
    line [
            X1 x1
            Y1 y1
            X2 x2
            Y2 y2
            SVGAttr.Stroke lineParameters.Stroke
            SVGAttr.StrokeWidth lineParameters.StrokeWidth
            SVGAttr.StrokeDasharray lineParameters.StrokeDashArray
    ] []

/// Makes a path ReactElement, points are to be given as an XYPos record element.
/// Please note that this function is designed to create ONLY "Move to - Bézier Curve"
///paths (this is what the "M" and "C" attributes stand for) and NOT a generalized SVG path element.
let makePath (startingPoint: XYPos) (startingControlPoint: XYPos) (endingControlPoint: XYPos) (endingPoint: XYPos) (pathParameters: Path) =
    let x1, y1, x2, y2 = startingPoint.X, startingPoint.Y, endingPoint.X, endingPoint.Y
    let dx1, dy1, dx2, dy2 = startingControlPoint.X, startingControlPoint.Y, endingControlPoint.X, endingControlPoint.Y
    let dAttrribute = sprintf "M %f %f C %f %f, %f %f, %f %f" x1 y1 dx1 dy1 dx2 dy2 x2 y2
    path [
            D dAttrribute
            SVGAttr.Stroke pathParameters.Stroke
            SVGAttr.StrokeWidth pathParameters.StrokeWidth
            SVGAttr.StrokeDasharray pathParameters.StrokeDashArray
            SVGAttr.StrokeLinecap pathParameters.StrokeLinecap
            SVGAttr.Fill pathParameters.Fill
    ] []
    
/// Makes a polygon ReactElement, points are to be given as a correctly formatted SVGAttr.Points string 
let makePolygon (points: string) (polygonParameters: Polygon) =
    polygon [
            SVGAttr.Points points
            SVGAttr.Stroke polygonParameters.Stroke
            SVGAttr.StrokeWidth polygonParameters.StrokeWidth
            SVGAttr.Fill polygonParameters.Fill
            SVGAttr.FillOpacity polygonParameters.FillOpacity
    ] []
    
/// Makes a circle ReactElement
let makeCircle (centreX: float) (centreY: float) (circleParameters: Circle) =
    circle
      [ 
        Cx centreX
        Cy centreY
        R circleParameters.R
        SVGAttr.Fill circleParameters.Fill
        SVGAttr.FillOpacity circleParameters.FillOpacity
        SVGAttr.Stroke circleParameters.Stroke
        SVGAttr.StrokeWidth circleParameters.StrokeWidth
      ] []
      
/// Makes a text ReactElement
let makeText (posX: float) (posY: float) (displayedText: string) (textParameters: Text) =
    text [
            X posX; 
            Y posY; 
            Style [
                TextAnchor textParameters.TextAnchor
                DominantBaseline textParameters.DominantBaseline
                FontWeight textParameters.FontWeight
                FontSize textParameters.FontSize
                Fill textParameters.Fill
                UserSelect textParameters.UserSelect
                
            ]
        ] [str <| sprintf "%s" (displayedText)]

//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS() = 
    System.DateTime.Now.Ticks
    |> (fun t -> t /10L)

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats

//--------------------------------Constants----------------------------------//

/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions : XYPos = 
    {X = 1000. ; Y = 1000.}




    

