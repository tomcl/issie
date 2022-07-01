module WaveSimStyle

open ModelType
open WaveSimHelpers

open Fulma
open Fable.React
open Fable.React.Props

module Constants =
    let namesColWidth = 200
    let valuesColWidth = 100

    let leftMargin = 50
    let rightMargin = 50

    let rowHeight = 30
    let clkLineWidth = 0.8
    let lineThickness : float = 0.8

    let fontSizeValueOnWave = "10px"
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = fontSizeValueOnWave }
    let valueOnWavePadding = 150.0

    let borderProperties = "2px solid rgb(219,219,219)"

    let labelPadding = 3

let topRowStyle = Style [
    Height Constants.rowHeight
    BorderBottom "2px solid rgb(219,219,219)"
]

/// Empty row used in namesColumn and valuesColumn. Shifts these down by one
/// to allow for the row of clk cycle numbers in waveformsColumn.
let topRow = [ div [ topRowStyle ] [] ]

/// TODO: Tweak these parameters
let errorMessageStyle = Style [
    Width "90%"
    MarginLeft "5%"
    MarginTop "15px"
]

let viewButtonStyle = Style [
    MarginLeft "10px"
    MarginRight "10px"
]

let viewButtonProps = [
    Button.Color IsSuccess
    Button.Props [viewButtonStyle]
]

let viewButtonLight = viewButtonProps @ [ Button.IsLight ]

let checkBoxColStyle = Style [
    BorderRight "2px solid #dbdbdb"
    VerticalAlign "bottom"
    Width "5px"
]

let tableRowStyle = Style [
    VerticalAlign "middle"
    Height Constants.rowHeight
    Width "5px"
]

let selectAllCheckboxProps : IHTMLProp list = [
    tableRowStyle
]

let checkboxStyle = Style [
    Margin "0 5px 0 5px"
    Cursor "pointer"
    Float FloatOptions.Left
]

let checkboxInputProps : IHTMLProp list = [
    Type "checkbox"
    checkboxStyle
]

let boldFontStyle = Style [
    FontWeight "bold"
    FontSize "14px"
]

let normalFontStyle = Style [
    FontWeight "normal"
    FontSize "14px"
]

let noBorderStyle = Style [
    BorderWidth 0
]

let selectWavesStyle = Style [
    Position PositionOptions.Relative
]

let selectRamButtonStyle = Style [
    Height Constants.rowHeight
    FontSize "16px"
    Position PositionOptions.Relative
    MarginRight 0
]

let selectRamButtonProps = [
    Button.Color IsInfo
    Button.Props [selectRamButtonStyle]
]

let selectRamButtonPropsLight = 
    selectRamButtonProps @ [Button.IsLight]

let selectWavesButtonStyle = Style [
    Height Constants.rowHeight
    FontSize "16px"
    Position PositionOptions.Relative
    MarginLeft 0
]

let selectWavesButtonProps = [
    Button.Color IsInfo
    Button.Props [selectWavesButtonStyle]
]

let selectWavesButtonPropsLight = 
    selectWavesButtonProps @ [Button.IsLight]

let ramTableLevelProps : IHTMLProp list = [
    Style [
        Position PositionOptions.Relative
        Display DisplayOptions.InlineBlock
        MarginRight 10
    ]
]

let centerAlignStyle = Style [
    TextAlign TextAlignOptions.Center
]

let ramTablesLevelProps : IHTMLProp list = [
    Style [
        OverflowX OverflowOptions.Scroll
    ]
]

let ramTableRowStyle wenHigh correctAddr =
    if wenHigh && correctAddr then
        Style [
            BackgroundColor "hsl(347, 90%, 96%)"
            Color "hsl(348, 100%, 61%)"
            FontWeight "bold"
        ]
    else if correctAddr then
        Style [
            BackgroundColor "hsl(206, 70%, 96%)"
            Color "hsl(204, 86%, 53%)"
            FontWeight "bold"
        ]
    else Style []

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

let valueOnWaveProps m i start width : IProp list =
    [
        X (float start * (singleWaveWidth m) + Constants.nonBinaryTransLen + float i * width)
        Y (0.6 * Constants.viewBoxHeight)
        Style [
            FontSize Constants.fontSizeValueOnWave
        ]
    ]

let clkCycleButtonStyle = Style [
    Float FloatOptions.Right
    Position PositionOptions.Relative
    Height Constants.rowHeight
    TextAlign TextAlignOptions.Center
    Display DisplayOptions.InlineBlock
    FontSize "13px"
]

// TODO: Can this be nested inside clkCycleButtonStyle
let clkCycleInputStyle = Style [
    Margin "0 0 0 0"
    Float FloatOptions.Left
    TextAlign TextAlignOptions.Center
    Width "40px"
    Height Constants.rowHeight
    Display DisplayOptions.InlineBlock
    FontSize "13px"
    BorderColor "gray"
    BorderWidth "1px 0.5px 1px 0.5px"
    BorderRadius 0
]

let clkCycleInputProps : IHTMLProp list = [
    Min 0
    SpellCheck false
    Step 1
    clkCycleInputStyle
]

let clkCycleBut = [
    Margin 0
    Height Constants.rowHeight
    Padding 0
    Width "30px"
    Position PositionOptions.Relative
    Float FloatOptions.Left
    BorderColor "gray"
    BorderWidth "1px 0.5px 1px 0.5px"
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
        BorderRightWidth "0.5"
    ])

let clkCycleRightStyle = Style (
    clkCycleBut @ [
        BorderTopLeftRadius 0
        BorderBottomLeftRadius 0
        BorderTopRightRadius "4px"
        BorderBottomRightRadius "4px"
        BorderLeftWidth "0.5"
    ])

let nameRowLevelStyle isHovered = Style [
    Height Constants.rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
    if isHovered then
        BackgroundColor "hsl(0, 0%, 96%)"
        Cursor "grab"
]

let nameLabelStyle isHovered = Style [
    if isHovered then
        Cursor "grab"
]

let valueLabelStyle = Style [
    Height Constants.rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
    PaddingLeft Constants.labelPadding
]

let nameRowLevelLeftProps (visibility: string): IHTMLProp list = [
    Style [
        Position PositionOptions.Sticky
        Left 0
        Visibility visibility
    ]
]

let ramRowStyle = Style [
    Height Constants.rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
]

let waveSimColumn = [
    Height "100%"
    Width "100%"
    BorderTop Constants.borderProperties
    Display DisplayOptions.Grid
    GridAutoRows Constants.rowHeight
    FontSize "12px"
    OverflowX OverflowOptions.Scroll
    WhiteSpace WhiteSpaceOptions.Nowrap
    LineHeight "25px"
]

let namesColumnStyle = Style (
    (waveSimColumn) @ [
        MinWidth Constants.namesColWidth
        Float FloatOptions.Left
        BorderRight Constants.borderProperties
        GridColumnStart 1
        TextAlign TextAlignOptions.Right
    ])

let namesColumnProps : IHTMLProp list = [
    Id "namesColumn"
    namesColumnStyle
]

let valuesColumnStyle = Style (
    (waveSimColumn) @ [
        MinWidth Constants.valuesColWidth
        Float FloatOptions.Right
        BorderLeft Constants.borderProperties
        GridColumnStart 3
    ])

let waveformColumnStyle = Style [
    GridColumnStart 2
    Display DisplayOptions.Grid
]

let waveRowsStyle width = Style [
    Height "100%" 
    OverflowX OverflowOptions.Hidden
    Display DisplayOptions.Grid
    FontSize "12px"
    GridAutoRows Constants.rowHeight
    BorderTop Constants.borderProperties
    Width width
    GridColumnStart 1
    GridRowStart 1
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

let waveSelectionPaneStyle = Style [
    MarginLeft Constants.leftMargin
    MarginRight Constants.rightMargin
    MarginTop "15px"
]

let clkLineStyle = Style [
    Stroke "rgb(200,200,200)"
    StrokeWidth Constants.clkLineWidth
]

let clkCycleText m i : IProp list = [
    SVGAttr.FontSize "12px"
    SVGAttr.TextAnchor "middle"
    X (singleWaveWidth m * (float i + 0.5))
    Y (0.6 * Constants.viewBoxHeight)
]

let clkCycleSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom Constants.borderProperties
]

let waveformColumnRowProps m : IProp list = [
    SVGAttr.Height Constants.rowHeight
    SVGAttr.Width (viewBoxWidth m)
    // min-x, min-y, width, height
    ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string Constants.viewBoxHeight)
    PreserveAspectRatio "none"
]

let clkCycleNumberRowProps m : IProp list = 
    waveformColumnRowProps m @ [
    clkCycleSVGStyle
]

let waveRowSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom "1px solid rgb(219,219,219)"
]

let waveRowProps m : IProp list = 
    waveformColumnRowProps m @ [
    waveRowSVGStyle
]

let backgroundSVG (wsModel: WaveSimModel) count : ReactElement list =
    let clkLine x = 
        line [
            clkLineStyle
            X1 x
            Y1 0.0
            X2 x
            Y2 (Constants.viewBoxHeight * float (count + 1))
        ] []
    [ wsModel.StartCycle + 1 .. endCycle wsModel + 1 ] 
    |> List.map (fun x -> clkLine (float x * singleWaveWidth wsModel))

/// Controls the background highlighting of which clock cycle is selected
let clkCycleHighlightSVG m dispatch =
    let count = List.length m.SelectedWaves
    svg [
        Style [
            GridColumnStart 1
            GridRowStart 1
        ]
        SVGAttr.Height (string ((count + 1) * Constants.rowHeight) + "px")
        SVGAttr.Width (viewBoxWidth m)
        SVGAttr.Fill "rgb(230,230,230)"
        SVGAttr.Opacity 0.4
        ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string (Constants.viewBoxHeight * float (count + 1)))
        Id "ClkCycleHighlight"
        OnClick (fun ev ->
            let svgEl = Browser.Dom.document.getElementById "ClkCycleHighlight"
            let bcr = svgEl.getBoundingClientRect ()
            /// Should be the same as singleWaveWidth
            let cycleWidth = bcr.width / float m.ShownCycles
            /// ev.clientX is X-coord of mouse click. bcr.left is x-coord of start of SVG.
            /// getBoundingClientRect only works if ViewBox is 0 0 width height, so
            /// add m.StartCycle to account for when viewBoxMinX is not 0
            let cycle = (int <| (ev.clientX - bcr.left) / singleWaveWidth m) + m.StartCycle
            dispatch <| SetWSModel {m with CurrClkCycle = cycle}
        )
        ]
        (List.append 
            [
                rect [
                    SVGAttr.Width (singleWaveWidth m)
                    SVGAttr.Height "100%"
                    X (float m.CurrClkCycle * (singleWaveWidth m))
                ] []
            ]
            (backgroundSVG m count)
        )


let radixTabProps : IHTMLProp list = [
    Style [
        Width "35px"
        Height Constants.rowHeight
    ]
]

let radixTabAStyle = Style [
    Padding "0 0 0 0"
    Height Constants.rowHeight
]

let radixTabsStyle = Style [
    Height Constants.rowHeight
    FontSize "80%"
    Float FloatOptions.Right
]

let wavePolylineStyle points : IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.Fill "none"
    SVGAttr.StrokeWidth Constants.lineThickness
    Points (pointsToString points)
]

let summaryProps : IHTMLProp list = [
    Style [
        FontSize "20px"
        FontWeight "bold"
    ]
]

/// TODO: Change Open to true
let detailsProps : IHTMLProp list = [
    Open false
]

let topHalfStyle = Style [
    Position PositionOptions.Sticky
    Top 0
    BackgroundColor "white"
    ZIndex 10000
]

let refreshSvg =
    svg [
            ViewBox "0 0 512 512"
            SVGAttr.Height "30"
        ] [
            path [
                D "M496 48V192c0 17.69-14.31 32-32 32H320c-17.69 0-32-14.31-32-32s14.31-32
                32-32h63.39c-29.97-39.7-77.25-63.78-127.6-63.78C167.7 96.22 96 167.9 96 256s71.69
                159.8 159.8 159.8c34.88 0 68.03-11.03 95.88-31.94c14.22-10.53 34.22-7.75 44.81
                6.375c10.59 14.16 7.75 34.22-6.375 44.81c-39.03 29.28-85.36 44.86-134.2 44.86C132.5
                479.9 32 379.4 32 256s100.5-223.9 223.9-223.9c69.15 0 134 32.47 176.1 86.12V48c0-17.69
                14.31-32 32-32S496 30.31 496 48z"
                Style [
                    Fill "black"
                ]
            ] []
        ]
