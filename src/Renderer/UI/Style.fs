module DiagramStyle

open MessageType
open ModelType
open Fable.React.Props

let private headerHeight = "72px"
let private rightSectionWidthS = "400px" // Small right section.
let private rightSectionWidthL = "650px" // Large right section.
let minViewerWidth = 400
let minEditorWidth = 400

let rightSectionWidthViewerDefault = 650

let rightSectionWidth (model:Model) =
    match model.RightTab with
    | MessageType.RightTab.Properties | MessageType.RightTab.Catalogue -> rightSectionWidthS
    | MessageType.RightTab.Simulation ->  rightSectionWidthL
    | MessageType.RightTab.WaveSim -> sprintf "%dpx" model.ViewerWidth

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
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "white"
        //UserSelect UserSelectOptions.None
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
    WhiteSpace WhiteSpaceOptions.Nowrap
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
let maxZoom = 3.0
let minZoom = 0.2
let maxBusValGap = 3
let busLabelTextSize = 0.6 // multiplied by signal height
let sigLineThick = 0.025;
let spacing = 0.4
let sigHeight = 0.3 

let vbWidth m = m.SimParams.ClkWidth * (float m.SimParams.LastClk + 1.0)
let maxWavesColWidthFloat m = vbWidth m * 40.0 + 4.0
let maxWavesColWidth m = string (maxWavesColWidthFloat m) + "px"
let waveCellWidth m = Width (maxWavesColWidth m)

let widthAndVBwave (m : WaveSimModel) : IProp list = [
    Style [waveCellWidth m]
    ViewBox ("0 0 " + string (vbWidth m) + " 0.7")
]

let clkRulerStyle m : IProp list = 
    List.append (widthAndVBwave m)
                [ Class "clkRulerSvg"
                  PreserveAspectRatio "none" ]

let cursorLeftPx m cursor =
    cursor * (m.ClkWidth * 40.0 + 4.0 / (float m.LastClk + 1.0)) 

let cursRectStyle m = Style [
        Left (float m.Cursor |> cursorLeftPx m |> string |> (fun w -> w + "px"))
        Width (40.0 * (m.ClkWidth - clkLineWidth))
]

let cursRectText m i : IProp list = [
    Class "clkNumStyle"
    X (m.SimParams.ClkWidth * (float i + 0.5)) 
    Y 0.5
]

let inWaveLabel nLabels xInd m : IProp list = [
    Class "busValueStyle"
    X (xInd * m.ClkWidth)
    Y (spacing + sigHeight * 0.7 - 0.3 * sigHeight * (float nLabels - 1.0) / 2.0)
    SVGAttr.FontSize (busLabelTextSize * sigHeight / float nLabels) 
]

let waveCellSvg m last : IProp list = 
    List.append (widthAndVBwave m)
                [ match last with
                  | true -> Class "lastWaveCellSvg"
                  | false -> Class "waveCellSvg"
                  PreserveAspectRatio "none" ]

let waveCell m : IHTMLProp list = [
    Class "rowHeight"
    Style [waveCellWidth m]
]

let lwaveCell m : IHTMLProp list = [
    Class "fullHeight"
    Style [waveCellWidth m]
]

let waveDiv= Style [ 
    Width "100%"
    Height "100%"
    Position PositionOptions.Relative
    OverflowX OverflowOptions.Scroll 
]

let wavesTable m : IHTMLProp list = [
    Class "wavesColTableStyle"
    Style [waveCellWidth m]
]
