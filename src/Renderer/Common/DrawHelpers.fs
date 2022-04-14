(*
  Helper functions for drawing on SVG canvas: mainly used by the draw block.
*)

module DrawHelpers
open Browser.Types
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open CommonTypes


//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//




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
    Movement: XYPos
    ShiftKeyDown: bool
    Op: MouseOp}

/// Record to help draw SVG circles
type Circle = {
    ///  Radius of the circle
    R: float  
    /// color of outline: default => black color
    Stroke: string
    /// width of outline: default => thin
    StrokeWidth: string
    /// Fill: 0.0 => transparent, 1.0 => opaque
    FillOpacity: float // transparent fill
    /// color of fill: default => black color
    Fill: string
}

/// Record tonhelp draw SVG lines
type Line = {
    /// color of outline: default => black color
    Stroke: string
    /// width of outline: default => thin
    StrokeWidth: string
    /// what type of line: default => solid
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

/// Record to help create SVG text
type Text = {
    /// start/end/middle: horizontal algnment vs (X,Y)
    TextAnchor: string
    FontSize: string
    FontWeight: string
    FontFamily: string
    Fill: string
    UserSelect: UserSelectOptions
    /// auto/middle/hanging: vertical alignment vs (X,Y)
    DominantBaseline: string
}

let testCanvas = Browser.Dom.document.createElement("canvas") :?> HTMLCanvasElement
let canvasWidthContext = testCanvas.getContext_2d()

/// To get this to work, note:
/// its seems only to do 10px size - that is compensated in the code below.
/// It is more accurate for some fonts than others.
/// serif fonts are perfectly accurate (try "times").
/// sans serif fonts have varying accuracy. Helvetica is the best I have found and is perfect.
/// note  that many fonts get converted to some standard serif or non-serif font.
/// note that accuracy varies with font weight (normal = 400 usually more accurate than bold = 600).
/// To test this, switch on label corner display in addComponentLabel
let getTextWidthInPixels(txt:string, font:Text) =
   canvasWidthContext?font <- String.concat " " ["10px"; font.FontWeight; font.FontFamily]; // e.g. "16px bold sans-serif";
   let sizeInPx = float ((font.FontSize.ToLower()).Replace("px",""))   
   let ms = sizeInPx * canvasWidthContext.measureText(txt).width / 10.0
   ms

/// this is a hack that works for "monospace" font only
let getMonospaceWidth (font: string) (txt: string) =
    let sizeInPx = float ((font.ToLower()).Replace("px",""))
    float txt.Length * 0.6 *sizeInPx 

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
    TextAnchor = "middle"
    FontSize = "10px"
    FontFamily = "helvetica" // helvetica seems to work for computing widths (most fonts don't)
    FontWeight = "normal"
    Fill = "black"
    UserSelect = UserSelectOptions.None
    DominantBaseline = "hanging"
}

/// Port circle, used by both Sheet and Symbol to create ports
let portCircle = { defaultCircle with R = 5.0; Stroke = "Black"; StrokeWidth = "1.0px"; Fill = "Grey"}


//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//



/// return a v4 (random) universally unique identifier (UUID)
/// works under .NET and FABLE
#if FABLE_COMPILER
let uuid():string = import "v4" "uuid"
#else
let uuid():string = System.Guid.NewGuid.ToString()
#endif

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


/// Makes path attributes for a horizontal upwards-pointing arc radius r
let makeArcAttr r =
    $"a %.2f{r} %.2f{r} 0 0 0 %.3f{2.0*r} 0"

/// Makes a partial arc radius d, heights h1,h2 at ends, distance d1,d2 to centre from ends horizontally
let makePartArcAttr r h1 d1 h2 d2 =
    let rot = -(180.0 / System.Math.PI) * System.Math.Asin (max -0.99999 (min 0.99999 ((h1-h2)/(d1+d2))))
    let flag = if d1 > 0.0 then 1 else 0
    $"a %.2f{r} %.2f{r} %.2f{rot} 0 {flag} %.3f{d1+d2} %.3f{h1-h2}"

/// makes a line segment offset dx,dy
let makeLineAttr dx dy =
    $"l %.3f{dx} %.3f{dy}"

let makePathFromAttr (attr:string) (pathParameters: Path) =
    path [
            D attr
            SVGAttr.Stroke pathParameters.Stroke
            SVGAttr.StrokeWidth pathParameters.StrokeWidth
            SVGAttr.StrokeDasharray pathParameters.StrokeDashArray
            SVGAttr.StrokeLinecap pathParameters.StrokeLinecap
            SVGAttr.Fill pathParameters.Fill
    ] []

/// Makes a path ReactElement, points are to be given as an XYPos record element.
/// Please note that this function is designed to create ONLY "Move to - Bézier Curve"
///paths (this is what the "M" and "C" attributes stand for) and NOT a generalized SVG path element.
let makeAnyPath (startingPoint: XYPos) (pathAttr:string) (pathParameters: Path) =
    let x1, y1 = startingPoint.X, startingPoint.Y
    let dAttr = sprintf "M %f %f %s" x1 y1 pathAttr
    makePathFromAttr dAttr pathParameters

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
                FontFamily textParameters.FontFamily
                Fill textParameters.Fill
                UserSelect textParameters.UserSelect 
            ]
        ] [str <| sprintf "%s" (displayedText)]

/// makes a two-line text ReactElement
/// Dy parameter determines line spacing
let makeTwoLinesOfText (posX: float) (posY: float) (line1: string) (line2: string) (textParameters: Text) =
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
    ] [tspan [] [str line1]; tspan [Dy "1.2em"] [str line2] ]

/// deliver string suitable for HTML color from a HighlightColor type value
let getColorString (col: CommonTypes.HighLightColor) =
    (sprintf "%A" col).ToLower()



//--------------------------------Constants----------------------------------//

/// these determine the size of the draw block canvas relative to the objects on it.
let canvasUnscaledSize = 3500.0




    

