module DiagramStyle

open CommonTypes
open ModelType
open Fulma
open Fable.React
open Fable.React.Props

let headerHeight = "72px"
let private headerHeightWithBorderOffset = "74px"
let private rightSectionWidthS = "400px" // Small right section.
let private rightSectionWidthL = "650px" // Large right section.
let minViewerWidth = 400
let minEditorWidth = 400

let rightSectionWidthViewerDefault = 650

// Get 
let getHeaderHeight =
    headerHeight
    |> String.filter (fun c -> (int(c) <= 57 && int(c) >= 48))
    |> float
    
let rightSectionWidth (model:Model) =
    match model.RightPaneTabVisible with
    | RightTab.Properties | RightTab.Catalogue -> rightSectionWidthS
    | RightTab.Simulation -> 
        match model.SimSubTabVisible with
        | SimSubTab.StepSim -> rightSectionWidthL
        | SimSubTab.WaveSim | SimSubTab.TruthTable -> sprintf "%dpx" model.WaveSimViewerWidth

let leftSectionWidth model = Style [
    Width (sprintf "calc(100%s - %s - 10px)" "%" (rightSectionWidth model))
]

let navbarStyle model = Style [
    Width "100%"
    Height headerHeight
]

/// For making Sheet contained inside left section (don't want sheet behind right section tabs) NOT USED
let leftSectionStyle model =
    let leftSectionWidth = leftSectionWidth model
    Style [
        Position PositionOptions.Fixed
        Left "0px"
        Top "0px"
        Height  "100%" //(sprintf "calc(100%s - %s)" "%" headerHeight) // WindowSize - headerHeight
        Width leftSectionWidth
        OverflowX OverflowOptions.Hidden
        OverflowY OverflowOptions.Hidden
        BorderTop "2px solid lightgray"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "white"
        //UserSelect UserSelectOptions.None
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
    
// Used by Sheet
let canvasVisibleStyleList model = 
    let widthRightSec = rightSectionWidth model
    [
        Display DisplayOptions.Block
        Position PositionOptions.Absolute // Required to work.
        OverflowX OverflowOptions.Scroll
        OverflowY OverflowOptions.Scroll
        Top headerHeight // Placed under header with offset for the border. // headerHeight // Placed just under the header.
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
    UserSelect UserSelectOptions.None
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

let rowHeightStyle = Style [
    Height "30px"
]

let checkBoxColStyle = Style [
    BorderRight "2px solid #dbdbdb"
    VerticalAlign "bottom"
    Width "5px"
]

let closeWaveSimButtonStyle = Style [
    Height "30px"
    FontSize "16px"
    Float FloatOptions.Left
    Position PositionOptions.Relative
    Margin "0 20px 20px 20px"
]

let clkCycleButtonStyle = Style [
    Float FloatOptions.Right
    Position PositionOptions.Relative
    Height "30px"
    TextAlign TextAlignOptions.Center
    Width "100px"
    Display DisplayOptions.InlineBlock
    FontSize "13px"
    Margin "0 20px 0 20px"
    Resize "vertical"
]

// TODO: Can this be nested inside clkCycleButtonStyle
let clkCycleInputStyle = Style [
    Margin "0 0 0 0"
    Float FloatOptions.Left
    TextAlign TextAlignOptions.Center
    Width "40px"
    Height "30px"
    Display DisplayOptions.InlineBlock
    FontSize "13px"
    Resize "vertical"
    // TODO: -webkit-appearnace: none
    BorderColor "gray"
    BorderWidth "1px 1px 1px 1px"
    BorderRadius 0
]

let clkCycleBut = [
    Margin 0
    Height "30px"
    Padding 0
    Width "30px"
    Position PositionOptions.Relative
    Float FloatOptions.Left
    BorderColor "gray"
    BorderWidth "1px"
]

let clkCycleLeftStyle = Style (
    clkCycleBut @ [
        BorderTopLeftRadius "4px"
        BorderBottomLeftRadius "4px"
        BorderTopRightRadius 0
        BorderBottomRightRadius 0
    ])

let clkCycleRightStyle = Style (
    clkCycleBut @ [
        BorderTopLeftRadius 0
        BorderBottomLeftRadius 0
        BorderTopRightRadius "4px"
        BorderBottomRightRadius "4px"
    ])

let upDownDivStyle = Style [
    Width "100%"
    Position PositionOptions.Relative
    Height "30px"
    Float FloatOptions.Left
]

let upDownButtonStyle = Style [
    Margin 0
    Display DisplayOptions.Block
    Width "100%"
    Height "50%"
    Padding "0 0 0 0"
    Top 0
    FontSize "40%"
    Position PositionOptions.Relative
    BorderColor "gray"
    BorderWidth "1px"
    BorderRadius 0
]

let labelStyle = Style [
    Fill "black"
    Margin "5px"
]

let borderProperties = "2px solid rgb(219,219,219)"

let namesColumnStyle = Style [
    Float FloatOptions.Left
    Height "100%"
    BorderTop borderProperties
    BorderRight borderProperties
    Display DisplayOptions.Grid
    GridAutoRows "30px" 
    VerticalAlign "bottom"
    FontSize "12px"
    // MinWidth "100px"
    Width "100%"
    TextAlign TextAlignOptions.Right
    // GridColumnStart 1
]

let valuesColumnStyle = Style [
    // GridColumnStart 3
    Float FloatOptions.Right
    Height "100%"
    BorderTop borderProperties
    BorderLeft borderProperties
    Display DisplayOptions.Grid
    GridAutoRows "30px" 
    VerticalAlign "bottom"
    FontSize "12px"
    Width "100%"
    MinWidth "50px"
]

let showWaveformsStyle = Style [
    Height "calc(100% - 50px)"
    Width "100%"
    OverflowY OverflowOptions.Auto
    Display DisplayOptions.Grid
    ColumnCount 3
    GridAutoFlow "column"
    GridAutoColumns "auto"
]

let waveformColumnStyle m = Style [
    Height "100%" 
    OverflowX OverflowOptions.Hidden
    // TODO: Remove this magic number
    // MaxWidth (sprintf "%dpx" (m.WaveSimViewerWidth - 125))
    Display DisplayOptions.Grid
    FontSize "12px"
    GridAutoRows "30px"
    BorderTop borderProperties
    Width "100%"
    GridColumnStart 1
    GridRowStart 1
]

let waveViewerPaneStyle m = Style [
    // Width (string (m.WaveSimViewerWidth - 10) + "px")
    MarginLeft "0%"
    MarginTop "0px"
    MarginBottom "100px"
    OverflowX OverflowOptions.Hidden
    OverflowY OverflowOptions.Auto
]

// let clkLineWidth = 0.0125
// let transLen = 0.1
// let vPos = 0.0
// let zoomFactor = 1.3
// let maxZoom = 3.0
// let minZoom = 0.2
// let maxBusValGap = 3
// let busLabelTextSize = 0.6 // multiplied by signal height
// let sigLineThick = 0.025;
// let spacing = 0.4
// let sigHeight = 0.3 

let viewBoxWidth m = m.ClkSVGWidth * (float m.EndCycle + 1.0)
let maxWavesColWidthFloat m = viewBoxWidth m * 40.0 + 4.0
let maxWavesColWidth m = string (maxWavesColWidthFloat m) + "px"

let clkLineWidth = 0.0125

let tmpStyle = Style [
    BorderBottom borderProperties
]

let clkLineStyle = Style [
    Stroke "rgb(200,200,200)"
    StrokeWidth clkLineWidth
]

let clkCycleText m i : IProp list =
    [
        SVGAttr.FontSize "3.5%"
        SVGAttr.TextAnchor "middle"
        X (m.ClkSVGWidth * (float i + 0.5))
        Y 0.65
    ]

let clkCycleSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom borderProperties
]

let viewBoxHeight : float = 1.0

let clkCycleNumberRowProps m : IProp list = [
    // min-x, min-y, width, height
    SVGAttr.Height "30px"
    ViewBox ("0 0 " + string (viewBoxWidth m) + " " + string viewBoxHeight)
    PreserveAspectRatio "none"
    clkCycleSVGStyle
]

let waveRowProps m : IProp list = [
    // min-x, min-y, width, height
    SVGAttr.Height "30px"
    // SVGAttr.Width (viewBoxWidth m)
    ViewBox ("0 0 " + string (viewBoxWidth m) + " " + string viewBoxHeight)
    PreserveAspectRatio "none"
    clkCycleSVGStyle
]

// This controls the background highlighting of which clock cycle is selected
let cursRectStyle (m: WaveSimModel) = Style [
    Left (string (float m.CurrClkCycle * m.ClkSVGWidth * 28.0) + "px")
    Position PositionOptions.Relative
    Width (m.ClkSVGWidth * 28.0)
    BackgroundColor "rgb(230,230,230)"
    StrokeWidth 0
    Opacity 0.4
    ZIndex -1
    Height "100%"
    GridColumnStart 1
    GridRowStart 1
]

let clkCycleHighlightSVG m count = 
    svg [
        Style [
            GridColumnStart 1
            GridRowStart 1
        ]
        SVGAttr.Height (string ((count + 1)* 30) + "px")
        SVGAttr.Width "100%"
        SVGAttr.Fill "rgb(230,230,230)"

        ViewBox ("0 0 " + string (viewBoxWidth m) + " " + string viewBoxHeight)
    ] [
        rect [
            SVGAttr.Width (m.ClkSVGWidth * 5.0)
            SVGAttr.Height "100%"
            X (float m.CurrClkCycle * m.ClkSVGWidth * 5.0)
        ] []
    ]

let lineThickness : float = 0.025

let radixTabsStyle = Style [
    Width "140px"
    Height "30px"
    FontSize "80%"
    Float FloatOptions.Right
    Margin "0 10px 0 10px"
]

let pointsToString (points: XYPos list) : string =
    List.fold (fun str (point: XYPos) ->
        str + string point.X + "," + string point.Y + " "
    ) "" points

let wavePolylineStyle points : IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.Fill "none"
    SVGAttr.StrokeWidth lineThickness

    Points (pointsToString points)
]

// let inWaveLabel nLabels xInd m : IProp list = [
//     Class "busValueStyle"
//     X (xInd * m.ClkSvgWidth)
//     Y (spacing + sigHeight * 0.7 - 0.3 * sigHeight * (float nLabels - 1.0) / 2.0)
//     SVGAttr.FontSize (busLabelTextSize * sigHeight / float nLabels) 
// ]

// let waveCellSvg m last : IProp list = 
//     List.append (widthAndVBwave m)
//                 [ match last with
//                   | true -> Class "lastWaveCellSvg"
//                   | false -> Class "waveCellSvg"
//                   PreserveAspectRatio "none" ]

// let waveCell m = [
//     Class "rowHeight"
//     Style [Width (maxWavesColWidth m)]
// ]

// let waveCellStyle m = Style [
//     Height "30px"
//     Width (maxWavesColWidth m)
// ]

// let lwaveCell m : IHTMLProp list = [
//     Class "fullHeight"
//     Style [Width (maxWavesColWidth m)]
// ]

// let waveDiv= Style [ 
//     Width "100%"
//     Height "100%"
//     Position PositionOptions.Relative
//     OverflowX OverflowOptions.Scroll 
// ]

// let wavesTable m : IHTMLProp list = [
//     Class "wavesColTableStyle"
//     Style [Width (maxWavesColWidth m)]
// ]
