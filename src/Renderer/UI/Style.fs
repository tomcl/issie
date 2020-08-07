module DiagramStyle

open DiagramMessageType
open DiagramModelType
open Fable.React.Props

let private headerHeight = "52px"
let private rightSectionWidthS = "400px" // Small right section.
let private rightSectionWidthL = "650px" // Large right section.

let rightSectionWidthViewerDefault = 650

let rightSectionWidth (model:Model) =
    match model.RightTab with
    | DiagramMessageType.RightTab.Properties | DiagramMessageType.RightTab.Catalogue -> rightSectionWidthS
    | DiagramMessageType.RightTab.Simulation ->  rightSectionWidthL
    | DiagramMessageType.RightTab.WaveSim -> sprintf "%dpx" model.ViewerWidth

let leftSectionWidth model = Style [
    Width (sprintf "calc(100%s - %s - 10px)" "%" (rightSectionWidth model))
]

let navbarStyle model = Style [
    Width "100%"
    Height headerHeight
]

let rightSectionStyle model = 
    let widthRightSec = rightSectionWidth model
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "0px"
        Height  "100%" //(sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
        Width widthRightSec
        OverflowX OverflowOptions.Hidden
        OverflowY OverflowOptions.Scroll
        BorderTop "2px solid lightgray"
]

let canvasVisibleStyle model = 
    let widthRightSec = rightSectionWidth model
    Style [
        Display DisplayOptions.Block
        Position PositionOptions.Absolute // Required to work.
        OverflowX OverflowOptions.Scroll
        OverflowY OverflowOptions.Scroll
        Top headerHeight // Placed just under the header.
        Left "0px"
        Bottom "0px"
        Right widthRightSec
        BorderTop "2px solid lightgray"
    ]





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

// Waveform simulator styles

let clkLineWidth = 0.0125
let transLen = 0.1
let vPos = 0.0
let zoomFactor = 1.2
let maxBusValGap = 3
let busLabelTextSize = 0.6 // multiplied by signal height
let sigLineThick = 0.025;

let widthAndVBwave (m : WaveSimModel) : IProp list = [
    let vbwidth = m.posParams.clkWidth * float (Array.length m.waveData)
    Style [Width ((string (vbwidth*10.0) ) + "%")]
    ViewBox ("0 0 " + string vbwidth + " 0.7")
]

let clkRulerStyle m : IProp list = 
    List.append (widthAndVBwave m)
                [ Class "clkRulerSvg"
                  PreserveAspectRatio "none" ]

let cursRectStyle m : IProp list = [
    Class "cursorRectStyle"
    let p = m.posParams
    X (p.clkWidth * float m.cursor + clkLineWidth / 2.0)
    SVGAttr.Width (p.clkWidth - clkLineWidth)
    SVGAttr.Height (p.spacing + p.sigHeight)
]

let cursRectText m i : IProp list = [
    Class "clkNumStyle"
    X (m.posParams.clkWidth * (float i + 0.5)) 
    Y 0.5
]

let inWaveLabel nLabels xInd i model : IProp list = [
    Class "busValueStyle"
    let p = model.posParams
    X (xInd * p.clkWidth)
    Y (p.spacing + p.sigHeight - p.sigHeight * 0.3 + 0.3 * p.sigHeight * (float i - (float nLabels - 1.0) / 2.0))
    SVGAttr.FontSize (busLabelTextSize * p.sigHeight / float nLabels) 
]

let waveCellSvg m last : IProp list = 
    List.append (widthAndVBwave m)
                [ match last with
                  | true -> Class "lastWaveCellSvg"
                  | false -> Class "waveCellSvg"
                  PreserveAspectRatio "none" ]

let waveCell m : IHTMLProp list = [
    Class "rowHeight"
    m.posParams.clkWidth * float (Array.length m.waveData)
    |> (fun x -> Style [Width ((string (x*10.0) ) + "%")])
]
