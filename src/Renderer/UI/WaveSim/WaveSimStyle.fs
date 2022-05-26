module WaveSimStyle

open CommonTypes
open ModelType
open DiagramStyle
open Fulma
open Fable.React
open Fable.React.Props

// Waveform simulator styles

let namesColMinWidth = 250
let valuesColMinWidth = 100
// TODO: Explain why: 30*width, width is 1.5, so that's 45. This is 8 cycles (0 to 7)
// This should be divisible by 45
// let initialWaveformColWidth = rightSectionWidthViewerDefault - namesColMinWidth - valuesColMinWidth
let initialWaveformColWidth = int( 1.5 * float (30 * 7)) //rightSectionWidthViewerDefault - namesColMinWidth - valuesColMinWidth

let rowHeight = "30px"

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

let clkCycleInnerStyle = Style (
    clkCycleBut @ [
        BorderRadius 0
    ]
)

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
    Height rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
]

let borderProperties = "2px solid rgb(219,219,219)"

let colWidth width =
    match width with
    | Some x -> string x
    | None -> "100%"

let waveSimColumn width = [
    Height "100%"
    Width "100%" //(colWidth width)
    BorderTop borderProperties
    Display DisplayOptions.Grid
    GridAutoRows "30px" 
    // VerticalAlign "middle"
    FontSize "12px"
    OverflowX OverflowOptions.Scroll
    WhiteSpace WhiteSpaceOptions.Nowrap
    LineHeight "25px"
]

let namesColumnStyle (width: float option) = Style (
    (waveSimColumn width) @ [
        MinWidth "215px"
        Float FloatOptions.Left
        BorderRight borderProperties
        GridColumnStart 1
        TextAlign TextAlignOptions.Right
    ])

let valuesColumnStyle (width: float option) = Style (
    (waveSimColumn width) @ [
        MinWidth "75px"
        Float FloatOptions.Right
        BorderLeft borderProperties
        GridColumnStart 3
    ])

let showWaveformsStyle m = Style [
    Height "calc(100% - 50px)"
    Width "100%"
    OverflowY OverflowOptions.Auto
    Display DisplayOptions.Grid
    ColumnCount 3
    GridAutoFlow "column"
    GridAutoColumns "auto"
]

let waveformColumnStyle m width = Style [
    Height "100%" 
    OverflowX OverflowOptions.Hidden
    // TODO: Remove this magic number
    // MaxWidth (sprintf "%dpx" (m.WaveSimViewerWidth - 125))
    Display DisplayOptions.Grid
    FontSize "12px"
    GridAutoRows "30px"
    BorderTop borderProperties
    Width width
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

let endCycle wsModel = wsModel.StartCycle + wsModel.ShownCycles - 1

let clkLineWidth = 0.0125

let topRowStyle = Style [
    Height rowHeight
    BorderBottom "2px solid rgb(219,219,219)"
]

let tmpStyle = Style [
    Height rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
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

let viewBoxMinX m = string (float m.StartCycle * m.ClkSVGWidth)
let viewBoxWidth m = string (float m.ShownCycles * m.ClkSVGWidth)
let viewBoxHeight : float = 1.0

let clkCycleNumberRowProps m : IProp list =
    [
    SVGAttr.Height "30px"
    SVGAttr.Width (float m.ShownCycles * 30.0 * m.ClkSVGWidth)
    // min-x, min-y, width, height
    ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string viewBoxHeight)
    PreserveAspectRatio "none"
    clkCycleSVGStyle
]

let waveRowSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom "1px solid rgb(219,219,219)"
]

let waveRowProps m : IProp list =
    [
    SVGAttr.Height "30px"
    SVGAttr.Width (float m.ShownCycles * 30.0 * m.ClkSVGWidth)
    // min-x, min-y, width, height
    ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string viewBoxHeight)
    PreserveAspectRatio "none"
    waveRowSVGStyle
]

// This controls the background highlighting of which clock cycle is selected
let clkCycleHighlightSVG m count = 
    svg [
        Style [
            GridColumnStart 1
            GridRowStart 1
        ]
        SVGAttr.Height (string ((count + 1)* 30) + "px")
        SVGAttr.Width (float m.ShownCycles * 30.0 * m.ClkSVGWidth)
        SVGAttr.Fill "rgb(230,230,230)"
        SVGAttr.Opacity 0.4
        ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string (viewBoxHeight * float (count + 1)))
    ] [
        rect [
            SVGAttr.Width (m.ClkSVGWidth)
            SVGAttr.Height "100%"
            X (float m.CurrClkCycle * m.ClkSVGWidth)
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
