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

let zoomOutSVG =
    svg [
            ViewBox "0 0 192.904 192.904"
            SVGAttr.Height "20px"
        ]
        [
            path [
                D "M190.707,180.101l-47.079-47.077c11.702-14.072,18.752-32.142,18.752-51.831C162.381,36.423,125.959,0,81.191,0
                C36.422,0,0,36.423,0,81.193c0,44.767,36.422,81.187,81.191,81.187c19.689,0,37.759-7.049,51.831-18.75l47.079,47.077
                c1.464,1.465,3.384,2.197,5.303,2.197c1.919,0,3.839-0.732,5.303-2.197C193.637,187.778,193.637,183.03,190.707,180.101z
                M15,81.193C15,44.694,44.693,15,81.191,15c36.497,0,66.189,29.694,66.189,66.193c0,36.496-29.692,66.187-66.189,66.187
                C44.693,147.38,15,117.689,15,81.193z"
            ] []
            path [
                D "M118.035,73.689H44.346c-4.142,0-7.5,3.358-7.5,7.5c0,4.142,3.358,7.5,7.5,7.5h73.689c4.142,0,7.5-3.358,7.5-7.5
                    C125.535,77.047,122.177,73.689,118.035,73.689z"
            ] []
        ]

let zoomInSVG =
    svg [
            ViewBox "0 0 192.904 192.904"
            SVGAttr.Height "20px"
        ]
        [
            path [
                D "M190.707,180.101l-47.079-47.077c11.702-14.072,18.752-32.142,18.752-51.831C162.381,36.423,125.959,0,81.191,0
                C36.422,0,0,36.423,0,81.193c0,44.767,36.422,81.187,81.191,81.187c19.689,0,37.759-7.049,51.831-18.75l47.079,47.077
                c1.464,1.465,3.384,2.197,5.303,2.197c1.919,0,3.839-0.732,5.303-2.197C193.637,187.778,193.637,183.03,190.707,180.101z
                M15,81.193C15,44.694,44.693,15,81.191,15c36.497,0,66.189,29.694,66.189,66.193c0,36.496-29.692,66.187-66.189,66.187
                C44.693,147.38,15,117.689,15,81.193z"
            ] []
            path [
                D "M118.035,73.689H88.69V44.345c0-4.142-3.357-7.5-7.5-7.5s-7.5,3.358-7.5,7.5v29.345H44.346c-4.143,0-7.5,3.358-7.5,7.5
                c0,4.142,3.357,7.5,7.5,7.5H73.69v29.346c0,4.142,3.357,7.5,7.5,7.5s7.5-3.358,7.5-7.5V88.689h29.345c4.143,0,7.5-3.358,7.5-7.5
                C125.535,77.047,122.178,73.689,118.035,73.689z"
            ] []
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
        X (m.ZoomLevel * (float i + 0.5))
        Y 0.65
    ]

let clkCycleSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom borderProperties
]

let viewBoxMinX m = string (float m.StartCycle * m.ZoomLevel)
let viewBoxWidth m = string (float m.ShownCycles * m.ZoomLevel)
let viewBoxHeight : float = 1.0

let clkCycleNumberRowProps m : IProp list =
    [
    SVGAttr.Height "30px"
    SVGAttr.Width (float m.ShownCycles * 30.0 * m.ZoomLevel)
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
    SVGAttr.Width (float m.ShownCycles * 30.0 * m.ZoomLevel)
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
        SVGAttr.Width (float m.ShownCycles * 30.0 * m.ZoomLevel)
        SVGAttr.Fill "rgb(230,230,230)"
        SVGAttr.Opacity 0.4
        ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string (viewBoxHeight * float (count + 1)))
    ] [
        rect [
            SVGAttr.Width (m.ZoomLevel)
            SVGAttr.Height "100%"
            X (float m.CurrClkCycle * m.ZoomLevel)
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
