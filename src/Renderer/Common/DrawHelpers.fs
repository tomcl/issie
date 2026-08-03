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
    /// DrawBlock coords (scaled from screen pixels by 1/zoom).
    /// inserted only in update function becaiuse of model dependence
    Pos: XYPos
    /// movement in screen pixel coords
    ScreenMovement: XYPos
    /// position on screen in screen pixel coords
    ScreenPage: XYPos
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

#if FABLE_COMPILER

/// The CSS font shorthand a Text describes, e.g. "500 16px Verdana".
let private fontString (font: Text) =
    String.concat " " [ font.FontWeight; font.FontSize; font.FontFamily ]

/// The off-screen canvas text is measured against. Deferred rather than created when this
/// module loads: `Browser.Dom.document` exists only in the browser, so measuring at module
/// initialisation would make DrawHelpers - and with it the whole draw block, which sizes every
/// symbol from text widths - throw under plain .NET, and so untestable outside Electron.
let private measureContext =
    lazy (
        let testCanvas = Browser.Dom.document.createElement("canvas") :?> HTMLCanvasElement
        testCanvas.getContext_2d())

/// Width of `txt` in pixels, as the browser will draw it in `font`.
/// To get this to work, note the fonts in the playground.fs test which work well.
/// Add fonts there to test if you like.
let getTextWidthInPixels (font: Text) (txt: string) =
    let context = measureContext.Force()
    context.font <- fontString font // e.g. "16px bold sans-serif"
    context.measureText(txt).width

#else

/// Advance width of each printable ASCII character as a fraction of the font size, read out of
/// Chromium's own measureText for "500 100px Verdana" - the weight and family the draw block
/// labels its symbols and ports with, and so the numbers the Fable build works from. (Verdana
/// has no 500 face; the browser picks its regular one, which is what this measured.)
///
/// Summing these reproduces measureText of a whole string to better than 1% for the labels Issie
/// uses. It is exact only because Verdana is barely kerned here: a pair like "To" is 9% narrower
/// measured than summed. That error is in the safe direction - a symbol comes out slightly too
/// wide rather than too narrow - and no Issie label is made of such pairs.
let private asciiCharWidths = [|
    //     !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /
    0.352; 0.394; 0.459; 0.818; 0.636; 1.076; 0.727; 0.269; 0.454; 0.454; 0.636; 0.818; 0.364; 0.454; 0.364; 0.454
    // 0    1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?
    0.636; 0.636; 0.636; 0.636; 0.636; 0.636; 0.636; 0.636; 0.636; 0.636; 0.454; 0.454; 0.818; 0.818; 0.818; 0.545
    // @    A      B      C      D      E      F      G      H      I      J      K      L      M      N      O
    1.000; 0.684; 0.686; 0.698; 0.771; 0.632; 0.575; 0.775; 0.751; 0.421; 0.455; 0.693; 0.557; 0.843; 0.748; 0.787
    // P    Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _
    0.603; 0.787; 0.695; 0.684; 0.616; 0.732; 0.684; 0.989; 0.685; 0.615; 0.685; 0.454; 0.454; 0.454; 0.818; 0.636
    // `    a      b      c      d      e      f      g      h      i      j      k      l      m      n      o
    0.636; 0.601; 0.623; 0.521; 0.623; 0.596; 0.352; 0.623; 0.633; 0.274; 0.344; 0.592; 0.274; 0.973; 0.633; 0.607
    // p    q      r      s      t      u      v      w      x      y      z      {      |      }      ~
    0.623; 0.623; 0.427; 0.521; 0.394; 0.633; 0.592; 0.818; 0.592; 0.592; 0.525; 0.635; 0.454; 0.635; 0.818
|]

/// Verdana Bold runs about 12% wider than its regular face, measured the same way. Nothing whose
/// geometry matters asks for bold - component and port labels are weight 500 - so one factor does.
let private boldWidthRatio = 1.12

/// Anything outside printable ASCII takes a middling width rather than none.
let private relativeCharWidth (c: char) =
    let i = int c - 32
    if i >= 0 && i < asciiCharWidths.Length then asciiCharWidths[i] else 0.6

/// Read the pixel size out of a CSS length. Every FontSize in Issie is written "<n>px".
let private fontSizeInPixels (fontSize: string) =
    let digits = fontSize.Replace("px", "").Trim()
    match System.Double.TryParse(digits, System.Globalization.NumberStyles.Float,
                                 System.Globalization.CultureInfo.InvariantCulture) with
    | true, px -> px
    | _ -> 10.0 // defaultText's size: a size we cannot read is better guessed than thrown on

/// Width of `txt` in pixels, reconstructed from per-character advance widths rather than measured:
/// there is no canvas to measure against outside the browser. It agrees with what the Fable build
/// gets from measureText to better than 1% for Verdana, the draw block's font, and degrades for
/// other families in proportion to how far their letterforms sit from Verdana's.
///
/// This exists so that the draw block can be exercised by `Tests/Issie.Tests` with nothing running
/// - symbols built, wires routed, the separation pass run. Geometry that depends on it is
/// recomputed whenever a sheet is loaded, so being a hair out costs a test nothing; a test should
/// still assert structure - port counts, overlap, orthogonality, ordering - rather than a pixel
/// width, which is only ever as good as the table above.
let getTextWidthInPixels (font: Text) (txt: string) =
    // a CSS font-weight is either a number or a keyword, and the keyword is case-insensitive:
    // both "bold" and "Bold" appear in Issie, and the browser bolds for either
    let boldness =
        match System.Int32.TryParse font.FontWeight with
        | true, weight -> if weight >= 600 then boldWidthRatio else 1.0
        | _ -> if font.FontWeight.ToLowerInvariant().Contains "bold" then boldWidthRatio else 1.0
    let relativeWidth = txt |> Seq.sumBy relativeCharWidth
    boldness * fontSizeInPixels font.FontSize * relativeWidth

#endif

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
    FontFamily = "verdana"
    FontWeight = "normal"
    Fill = "black"
    UserSelect = UserSelectOptions.None
    DominantBaseline = "hanging"
}

/// Port circle, used by both Sheet and Symbol to create ports
let portCircle = { defaultCircle with R = 5.0; Stroke = "Black"; StrokeWidth = "1.0px"; Fill = "Grey"}
let portCircleTarget= { defaultCircle with R = 8.0; Stroke = "DodgerBlue"; StrokeWidth = "2.0px"; Fill = "None"}

/// HLP23 AUTHOR: BRYAN TAN
/// Custom component corner circle
/// Resize handles on a custom component. Blue, matching the draggable ports, so that "you can drag
/// this" is one colour throughout - it used to be red, which elsewhere means something is wrong.
let cornerCircle = { defaultCircle with R = 5.0; Stroke = "Black"; StrokeWidth = "1.0px"; Fill = "DodgerBlue"}


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

//Makes a bezier curve that can now be combined with other curves (for use in makeanypath)
//HLP23: Author Ismagilov
let makePathAttr (startingControlPoint: XYPos) (endingControlPoint: XYPos) (endingPoint: XYPos) =
    let x2, y2 = endingPoint.X, endingPoint.Y
    let dx1, dy1, dx2, dy2 = startingControlPoint.X, startingControlPoint.Y, endingControlPoint.X, endingControlPoint.Y
    let dAttrribute = sprintf "C %f %f, %f %f, %f %f" dx1 dy1 dx2 dy2 x2 y2
    dAttrribute

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


/// Calculates if two bounding boxes intersect by comparing corner coordinates of each box
let boxesIntersect (box1: BoundingBox) (box2: BoundingBox) =
    // Requires min and max since H & W can be negative, i.e. we don't know which corner is which automatically
    // Boxes intersect if there is overlap in both x and y coordinates 
    min box1.TopLeft.X (box1.TopLeft.X + box1.W) < max box2.TopLeft.X (box2.TopLeft.X + box2.W)
    && min box2.TopLeft.X (box2.TopLeft.X + box2.W) < max box1.TopLeft.X (box1.TopLeft.X + box1.W)
    && min box1.TopLeft.Y (box1.TopLeft.Y + box1.H) < max box2.TopLeft.Y (box2.TopLeft.Y + box2.H)
    && min box2.TopLeft.Y (box2.TopLeft.Y + box2.H) < max box1.TopLeft.Y (box1.TopLeft.Y + box1.H)



    

