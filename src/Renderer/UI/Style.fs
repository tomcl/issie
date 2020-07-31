module DiagramStyle

open Fable.React.Props

let private headerHeight = "52px"
let private rightSectionWidthS = "400px" // Small right section.
let private rightSectionWidthL = "650px" // Large right section.

let private leftSectionStyle widthRightSec = Style [
    Width (sprintf "calc(100%s - %s)" "%" widthRightSec)
]

let navbarStyle = Style [
    Width "100%"
    Height headerHeight
]

let private rightSectionStyle width = Style [
    Position PositionOptions.Fixed
    Right "0px"
    Top "0px"
    Height  "100%" //(sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
    Width width
    OverflowX OverflowOptions.Hidden
    OverflowY OverflowOptions.Scroll
    BorderTop "2px solid lightgray"
]

/// Style when right column is expanded.
let rightSectionStyleS = rightSectionStyle rightSectionWidthS
let leftSectionStyleL = leftSectionStyle rightSectionWidthS
/// Style when right column is small.
let rightSectionStyleL = rightSectionStyle rightSectionWidthL
let leftSectionStyleS = leftSectionStyle rightSectionWidthL

let canvasHiddenStyle = Style [
    Display DisplayOptions.None
]

let private canvasVisibleStyle right = Style [
    Display DisplayOptions.Block
    Position PositionOptions.Absolute // Required to work.
    OverflowX OverflowOptions.Scroll
    OverflowY OverflowOptions.Scroll
    Top headerHeight // Placed just under the header.
    Left "0px"
    Bottom "0px"
    Right right
    BorderTop "2px solid lightgray"
]

/// Style when right column is expanded.
let canvasVisibleStyleS = canvasVisibleStyle rightSectionWidthL
/// Style when right column is small.
let canvasVisibleStyleL = canvasVisibleStyle rightSectionWidthS

let canvasSmallMenuStyle = Style [
    Display DisplayOptions.Block
    Position PositionOptions.Absolute // Required to work.
    OverflowX OverflowOptions.Hidden
    OverflowY OverflowOptions.Hidden
    Left "10px"
    Bottom "25px"
    Right (sprintf "calc(100%s - 300px)" "%")
]

let canvasSmallButtonStyle = Style [
    MarginRight "5px"
    BackgroundColor "white"
    BorderRadius "4px"
    BorderStyle "solid"
    Outline "none"
    Padding "4px"
    Opacity 0.7
]

let notificationStyle = Style [
    ZIndex 100 // In front of everything.
    Position PositionOptions.Absolute
    Right "20px"
    Bottom "20px"
]

let simulationNumberStyle = Style [
    Width "320px"
    Height "30px"
]

let simulationBitStyle = Style [
    Width "100px"
    Height "30px"
    PaddingTop "3px"
]

let menuLabelStyle = Style [
    Outline "none"
    MarginTop "10px"
    MarginBottom "10px"
    Color "#7a7a7a"
    FontSize "0.80em"
    LetterSpacing "0.1em"
    TextTransform "uppercase"
]

//Waveform simulator styles

let transLen = 0.1
let vPos = 0.0
let zoomFactor = 1.2
let waveVBextraHeight = 0.5
let maxBusValGap = 3
let busLabelTextSize = 0.6 // multiplied by signal height
let waveBoxPercWidth : float = 73.0
let clkLineWidth = 0.0125
let boxStrokeThck = 0.05

let sigLineStyle: IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.StrokeWidth 0.025
]

let clkLineStyle: IProp list = [
    SVGAttr.Stroke "rgb(200,200,200)"
    SVGAttr.StrokeWidth clkLineWidth
]

let boxLineStyle: IProp list = [ 
    X "0"
    Y vPos
    SVGAttr.Width (waveBoxPercWidth / 10.0)
    SVGAttr.Stroke "black"
    SVGAttr.Fill "white"
    SVGAttr.StrokeWidth boxStrokeThck
]

let cursorRectStyle: IProp list = [
    Y (vPos + boxStrokeThck / 2.0)
    SVGAttr.Width (waveBoxPercWidth / 10.0)
    SVGAttr.Stroke "black"
    SVGAttr.Fill "rgb(220,220,220)"
    SVGAttr.StrokeWidth 0.0
    SVGAttr.FillOpacity 5.0
]

let busValueStyle: IProp list = [
    SVGAttr.Fill "black"
    SVGAttr.TextAnchor "middle"
]

let waveLblStyle: IProp list = [
    X 0.0
    SVGAttr.Fill "black"
    SVGAttr.TextAnchor "start"
]

let cursValLblStyle: IProp list = [
    X 0.0
    SVGAttr.Fill "black"
    SVGAttr.TextAnchor "start"
]

let waveLblDivStyle = Style [
    Float FloatOptions.Left
    Clear ClearOptions.Both
    Width "20%"
]

let waveLblSvgStyle: IProp list = [
    unbox ("width", "100%")
]

let waveContDivStyle = Style [
    Float FloatOptions.Left
    Width (string waveBoxPercWidth + "%")
    Position PositionOptions.Relative 
]

let cursorDivStyle = Style [
    Float FloatOptions.Right
    Width "5%"
    Position PositionOptions.Relative
]

let cursorDivSvgStyle: IProp list = [
    unbox ("width", "100%")
]

let boxSvgStyle: IProp list = [
    Style [ Position PositionOptions.Absolute ]
    unbox ("width", "100%")
    unbox ("y", "0")
]

let waveRightSmallDivStyle = Style [ 
    Width "100%"
    OverflowX OverflowOptions.Scroll
    Position PositionOptions.Absolute 
]

let cursorButtonStyle = Style [ 
    Padding "3% 3%" 
    Width "3%"
    Position PositionOptions.Relative
    Color "red"
    Float FloatOptions.Left
]

let reloadButtonStyle = Style [
    Padding "3% 3%" 
    Width "5%"
    Position PositionOptions.Relative
    Color "red"
    Float FloatOptions.Left
]

let stdButtonStyle = Style [
    Color "gray"
]

let newWaveButtonStyle = Style [
    Padding "0 0" 
    Width "100%"
    Position PositionOptions.Relative
    Color "red"
    Float FloatOptions.Left
    FontSize "5%"
]

let clkNumStyle: IProp list = [
    SVGAttr.FontSize "1%"
    SVGAttr.Y "4%"
    SVGAttr.TextAnchor "middle"
]