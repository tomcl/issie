/// Functions to style the DOM elements used in waveform simulator
module WaveSimStyle

//---------------------------------------------------------------------------------------//
//-----------------------CSS DOM Styling for Waveform Simulator--------------------------//
//---------------------------------------------------------------------------------------//

open Fulma
open Fable.React
open Fable.React.Props
open CommonTypes
open ModelType
open ModelHelpers

module Constants =
    // Width of names column - replaced by calcNamesColWidth function

    /// Width of values column
    let valuesColWidth = 100
    let deleteSymbolWidth = 20
    let scrollBarWidth = 15

    /// Width of left margin of waveform simulator
    let leftMargin = 30
    /// Width of right margin of waveform simulator
    let rightMargin = 0

    /// Height of each row in name and value columns.
    /// Same as SVG ViewBox Height.
    let rowHeight = 30

    let colWidth = 120

    /// Width of line that separates each clock cycle.
    let clkLineWidth = 0.8
    /// Width of each waveform line.
    let lineThickness : float = 0.8
    let columnFontSize = "12px"
 
    let columnFontFamily = "Helvetica"
 
    let valueColumnFontSize = "12px"
    let valueColumnFontFamily = "Helvetica"

    let valueColumnText = 
        { DrawHelpers.defaultText with 
            FontSize = valueColumnFontSize
            FontFamily = valueColumnFontFamily}

    let fontSizeValueOnWave = "10px"
    /// Text used to display vlaues on non-binary waves
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = fontSizeValueOnWave }
    /// Whitespace padding between repeated values displayed on non-binary waves.
    let valueOnWavePadding = 75.0
    /// Whitespace padding between non-binary wave values and the edge of transition.
    let valueOnWaveEdgePadding = 8.0

    /// Border between columns and headers of waveform viewer.
    let borderProperties = "2px solid rgb(219,219,219)"

    /// Padding between name label/value label and waveform column.
    let labelPadding = 3
    /// Color for cursor and values column
    let cursorColor = "Lavender"

    /// <summary>Height of scrollbar, in pixels. Affects only the SVG and not the buttons.
    /// Currently set to same height as buttons.</summary>
    let softScrollBarWidth: float = 25.0

    /// <summary>Minimum width of the scrollbar thumb, in pixels.</summary>
    let scrollbarThumbMinWidth: float = 5.0

    /// height of the top half of the wave sim window (including tabs) when waveforms are displayed
    let topHalfHeight = 260.

    // helpers constants
    /// initial time running simulation without spinner to check speed (in ms)
    let initSimulationTime = 100.
    /// max estimated time to run simulation and not need a spinner (in ms)
    let maxSimulationTimeWithoutSpinner = 200.
    /// The horizontal length of a transition cross-hatch for non-binary waveforms
    let nonBinaryTransLen : float = 2.

    /// The height of the viewbox used for a wave's SVG. This is the same as the height
    /// of a label in the name and value columns.
    /// TODO: Combine this with WaveSimStyle.Constants.rowHeight?
    let viewBoxHeight : float = 30.0

    /// Height of a waveform
    let waveHeight : float = 0.8 * viewBoxHeight
    /// Vertical padding between top and bottom of each wave and the row it is in.
    let spacing : float = (viewBoxHeight - waveHeight) / 2.

    /// y-coordinate of the top of a waveform
    let yTop = spacing
    /// y-coordiante of the bottom of a waveform
    let yBot = waveHeight + spacing

    /// minium number of cycles on screen when zooming in
    let minVisibleCycles = 3

    /// Minimum number of visible clock cycles.
    let minCycleWidth = 5

    let zoomChangeFactor = 1.5

    /// If the width of a non-binary waveform is less than this value, display a cross-hatch
    /// to indicate a non-binary wave is rapidly changing value.
    let clkCycleNarrowThreshold = 20

    /// number of extra steps simulated beyond that used in simulation. Is this needed?
    let extraSimulatedSteps = 5 

    let infoMessage = 
        "Find ports by any part of their name. '.' = show all. '*' = show selected. '-' = collapse all"

    let outOfDateMessage = "Use refresh button to update waveforms. 'End' and then 'Start' to simulate a different sheet"

    let infoSignUnicode = "\U0001F6C8"

    let waveLegendMaxChars = 35
    let valueColumnMaxChars = 35
    let multipliers = [1;2;5;10;20;50]

// maybe these should be defined earlier in compile order? Or added as list functions?

let listMaxWithDef defaultValue lst =
    defaultValue :: lst
    |> List.max

let listCollectSomes mapFn lst =
    lst
    |> List.collect (fun x -> match mapFn x with | Some r -> [r] | None -> [])

/// List of selected waves (of type Wave)
let selectedWaves (wsModel: WaveSimModel) : Wave list = 
    wsModel.SelectedWaves
    |> List.map (fun wi -> Map.tryFind wi wsModel.AllWaves |> Option.toList)
    |> List.concat

/// Convert XYPos list to string
let pointsToString (points: XYPos array) : string =
    Array.fold (fun str (point: XYPos) ->
        $"{str} %.1f{point.X},%.1f{point.Y} "
    ) "" points

let screenHeight() = Browser.Dom.document.defaultView.innerHeight


/// Width of one clock cycle.
let singleWaveWidth m = max 5.0 (float m.WaveformColumnWidth / float m.ShownCycles)

/// Left-most coordinate of the SVG viewbox.
let viewBoxMinX m = string (float m.StartCycle * singleWaveWidth m)

/// Total width of the SVG viewbox.
let viewBoxWidth m = string (max 5.0 (m.WaveformColumnWidth))

/// Right-most visible clock cycle.
let endCycle wsModel = wsModel.StartCycle + (wsModel.ShownCycles) - 1

/// Style for top row in wave viewer.
let topRowStyle = Style [
    Height Constants.rowHeight
    BorderBottom "2px solid rgb(219,219,219)"
]

/// Empty row used in namesColumn and valuesColumn. Shifts these down by one
/// to allow for the row of clk cycle numbers in waveformsColumn.
let topRow topRowContent = [ div [ topRowStyle ] topRowContent ]

/// Style for showing error messages in waveform simulator.
let errorMessageStyle = Style [
    Width "90%"
    MarginLeft "5%"
    MarginTop "15px"
]

/// Style of checkboxes
let checkboxStyle = Style [
    Margin "0 5px 0 5px"
    Cursor "pointer"
    Float FloatOptions.Left
]

/// Props for Checkbox.Input
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

/// Style for selectRamButton
let selectRamButtonStyle = Style [
    Height Constants.rowHeight
    FontSize "16px"
    Position PositionOptions.Relative
    MarginRight 0
]

/// Props for selectRamButton

/// Style for selectWavesButton
let selectWavesButtonStyle = Style [
    Height Constants.rowHeight
    FontSize "16px"
    Position PositionOptions.Relative
    MarginLeft 0
]

/// Style for top row of buttons
let topRowButtonStyle isRightSide= Style [
    Height Constants.rowHeight
    Width Constants.colWidth
    FontSize "16px"
    Flex "0 0.5"
    if isRightSide then MarginLeft "auto" else AlignSelf AlignSelfOptions.FlexStart
    MarginRight 5
    MarginLeft 5
]

let infoButtonProps color = [
        Button.Color color
        Button.IsRounded
        Button.Size ISize.IsSmall
        Button.Props [
            Style [
                Height (float Constants.rowHeight)
                Height Constants.rowHeight
                Width Constants.colWidth
                FontSize "16px"
                Position PositionOptions.Relative
                MarginRight 0
                MarginLeft 0
                MarginBottom "5px"
            ]
        ]
    ]

let topHalfButtonProps color buttonId isRightSide = [
    Button.Color color
    Button.Props [HTMLAttr.Id buttonId ; topRowButtonStyle isRightSide]
]

let selectRamButtonProps buttonId = topHalfButtonProps IsInfo buttonId true

/// Props for selectRamButton when no RAMs are selectable
let selectRamButtonPropsLight buttonId =
    selectRamButtonProps buttonId  @ [Button.IsLight]


(*let topHalfButtonPropsWithWidth buttonId color = [
    Button.Color color
    Button.Props [HTMLAttr.Id buttonId ; topRowButtonStyle]
]*)

/// Props for selectWavesButton
let selectWavesButtonProps = topHalfButtonProps IsInfo


/// Props for selectWavesButton when no waves are selectable
let selectWavesButtonPropsLight buttonId =
    selectWavesButtonProps buttonId true @ [Button.IsLight]

let centerAlignStyle = Style [
    TextAlign TextAlignOptions.Center
    FontSize "15px"
]

/// Style for row in ramTable
let ramRowStyle = Style [
    Height Constants.rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
]

type RamRowType = RAMWritten | RAMRead | RAMNormal 

/// Style for each row of ramTable
let ramTableRowStyle (rowType:RamRowType) =
    match rowType with
    // Highlight in red on write
    | RAMWritten ->
        [
            BackgroundColor "hsl(347, 90%, 96%)"
            Color "hsl(348, 100%, 61%)"
            FontWeight "bold"
        ]
    // Highlight in blue on write
    | RAMRead ->
        [
            BackgroundColor "hsl(206, 70%, 96%)"
            Color "hsl(204, 86%, 53%)"
            FontWeight "bold"
        ]

    | RAMNormal ->
        []

/// Props for Bulma Level element for single ramTable
let ramTableLevelProps : IHTMLProp list = [
    Style [
        Font Constants.columnFontFamily
        FontSize Constants.columnFontSize
        Position PositionOptions.Relative
        Display DisplayOptions.InlineBlock
        MarginRight 20
        MarginLeft 20
    ]
]

/// Props for Bulma Level element for ramTables
let ramTablesLevelProps : IHTMLProp list = [
    Style [
        OverflowX OverflowOptions.Auto
        Font Constants.columnFontFamily
        FontSize Constants.columnFontSize
        
    ]
]

let zoomOutSVG =
    svg [
            ViewBox "0 0 192.904 192.904"
            SVGAttr.Height "20px"
        ] [ path [
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
        ] [ path [
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

/// <summary>Props for displaying repeating text elements on non-binary waves.</param>
/// <remarks>Not used any more.</remarks>
/// <param name="m"><c>WaveSim<c> model.</param>
/// <param name="i">Index of text elements to be generated.</param>
/// <param name="start">Starting position of repeating text elements.</param>
/// <param name="width">Width of each text element.</param>
let valueOnWaveProps m i start width : list<IProp> = [
    X (start * (singleWaveWidth m) + Constants.nonBinaryTransLen + float i * width)
    Y (0.6 * Constants.viewBoxHeight)
    Style [ FontSize Constants.fontSizeValueOnWave ]
]

/// <summary>Props for displaying values on non-binary waves by starting position.</summary>
/// <param name="xpos">Starting X-direction position.</param>
let singleValueOnWaveProps xpos: list<IProp> = [
    X xpos
    Y (0.6 * Constants.viewBoxHeight)
    Style [ FontSize Constants.fontSizeValueOnWave ]
]

/// Style for clock cycle buttons
let clkCycleButtonStyle = Style [
    Height Constants.rowHeight
    TextAlign TextAlignOptions.Center
    Display DisplayOptions.Inline
    FontSize "13px"
    WhiteSpace WhiteSpaceOptions.Nowrap
]

/// Style for clock cycle text Input field
let clkCycleInputStyle = Style [
    Margin "0 0 0 0"
    TextAlign TextAlignOptions.Center
    Width "60px"
    Height Constants.rowHeight
    Display DisplayOptions.Inline
    FontSize "13px"
    FontWeight 600
    BorderColor "gray"
    BorderWidth "1px 0.5px 1px 0.5px"
    BorderRadius 0
    WhiteSpace WhiteSpaceOptions.Nowrap
]

/// Props for clock cycle text Input field
let clkCycleInputProps : IHTMLProp list = [
    Min 0
    SpellCheck false
    Step 1
    clkCycleInputStyle
]

/// List of Style properties for clock cycle button
let clkCycleBut height = [
    Margin 0
    Height height
    Padding 0
    Width "30px"
    BorderColor "gray"
    BorderWidth "1px 0.5px 1px 0.5px"
    WhiteSpace WhiteSpaceOptions.Nowrap
]

/// Style for inner clock cycle buttons (buttons to move by one clock cycle)
let clkCycleInnerStyle = Style (
    clkCycleBut Constants.rowHeight @ [
        BorderRadius 0
        WhiteSpace WhiteSpaceOptions.Nowrap
    ]
)

/// Style for left-most clock cycle button
let clkCycleLeftStyle = Style (
    clkCycleBut Constants.rowHeight @ [
        BorderTopLeftRadius "4px"
        BorderBottomLeftRadius "4px"
        BorderTopRightRadius 0
        BorderBottomRightRadius 0
        BorderRightWidth "0.5"
        WhiteSpace WhiteSpaceOptions.Nowrap
    ])

/// Style for left-most clock cycle button
let scrollbarClkCycleLeftStyle = Style (
    clkCycleBut Constants.softScrollBarWidth @ [
        BorderTopLeftRadius "4px"
        BorderBottomLeftRadius "4px"
        BorderTopRightRadius 0
        BorderBottomRightRadius 0
        BorderRightWidth "0.5"
    ])

/// Style for right-most clock cycle button
let clkCycleRightStyle = Style (
    clkCycleBut Constants.rowHeight @ [
        BorderTopLeftRadius 0
        BorderBottomLeftRadius 0
        BorderTopRightRadius "4px"
        BorderBottomRightRadius "4px"
        BorderLeftWidth "0.5"
        WhiteSpace WhiteSpaceOptions.Nowrap
    ])

// FIX: Should be refactored. This is a hack to force button style to NOT float right.
/// <summary>Button style for scrollbar's right button. Left button uses <c>clkCycleLeftStyle</c>.</summary>
let scrollbarClkCycleRightStyle = Style (
    clkCycleBut Constants.softScrollBarWidth @ [
        BorderTopLeftRadius 0
        BorderBottomLeftRadius 0
        BorderTopRightRadius "4px"
        BorderBottomRightRadius "4px"
        BorderLeftWidth "0.5"
    ])

/// Style for Bulma level element in name row
let nameRowLevelStyle isHovered = Style [
    Height Constants.rowHeight
    BorderBottom "1px solid rgb(219,219,219)"
    if isHovered then
        BackgroundColor "hsl(0, 0%, 96%)"
        Cursor "grab"
]

/// Style for name label
let nameLabelStyle isHovered = Style [
    if isHovered then
        Cursor "grab"
]

/// Style for value label
let valueLabelStyle = 
    Style [
        Height Constants.rowHeight
        BorderBottom "1px solid rgb(219,219,219)"
        PaddingLeft Constants.labelPadding
        FontFamily Constants.valueColumnFontFamily
        FontSize Constants.valueColumnFontSize
    ]

/// Prop for Level.left in name row.
let nameRowLevelLeftProps (visibility: string): IHTMLProp list = [
    Style [
        Position PositionOptions.Sticky
        CSSProp.Left 0
        Visibility visibility
    ]
]

       
       


let calcNamesColWidth (ws:WaveSimModel) : int =
    let cWidth =
        DrawHelpers.canvasWidthContext.font <- String.concat " " ["10px"; Constants.columnFontFamily]; // e.g. "16px bold sans-serif";
        let getWidth (txt:string) =
            let sizeInPx = float ((Constants.columnFontSize.ToLower()).Replace("px",""))   
            sizeInPx * DrawHelpers.canvasWidthContext.measureText(txt).width / 10.0
        ws.SelectedWaves
        |> listCollectSomes (
            fun wi -> 
                Map.tryFind wi ws.AllWaves
                |> Option.map (fun wave -> wave.ViewerDisplayName))
        |> (fun lst -> "Dummy" :: lst)
        |> List.map getWidth
        |> List.max
        |> System.Math.Ceiling
        |> int
    cWidth + Constants.deleteSymbolWidth


/// List of Style properties for columns in wave viewer.
let waveSimColumn = [
    Height "100%"
    Width "100%"
    BorderTop Constants.borderProperties
    Display DisplayOptions.Grid
    GridAutoRows Constants.rowHeight
    FontSize Constants.valueColumnFontSize
    FontFamily Constants.valueColumnFontFamily
    OverflowX OverflowOptions.Auto
    WhiteSpace WhiteSpaceOptions.Nowrap
    LineHeight "25px"
]

/// Style properties for names column
let namesColumnStyle (ws:WaveSimModel) = Style (
    (waveSimColumn) @ [
        Width (calcNamesColWidth ws)
        Float FloatOptions.Left
        BackgroundColor Constants.cursorColor
        BorderRight Constants.borderProperties
        GridColumnStart 1
        OverflowX OverflowOptions.Clip
        TextAlign TextAlignOptions.Right
    ])

/// Props for names column
let namesColumnProps (ws:WaveSimModel): IHTMLProp list = [
    Id "namesColumn"
    
    namesColumnStyle ws
]

let valuesColumnSize wsModel =
    let selWaves = selectedWaves wsModel
    let maxValueBusWidth: int =
        selWaves
        |> List.map (fun wave -> wave.Width)
        |> (fun lis -> 0 :: lis)
        |> List.max
    let sampleVals = 
        [maxValueBusWidth; min maxValueBusWidth NumberHelpers.Constants.maxBinaryDisplayWidth]
        |> List.map (fun num ->
                        let num = min (max num 1) (SimulatorTypes.bigIntMaskA.Length - 2)
                        let worstCaseVal, extra =
                            match wsModel.Radix with
                            | CommonTypes.Hex | CommonTypes.Bin -> 0I, 2.
                            | CommonTypes.Dec -> SimulatorTypes.bigIntMask (num+1), 2.
                            | CommonTypes.SDec -> SimulatorTypes.bigIntMask (num-1), 10.
                        let (fd: SimulatorTypes.FastData) = {Dat=SimulatorTypes.BigWord worstCaseVal; Width=num}
                        NumberHelpers.fastDataToPaddedString 10000 wsModel.Radix fd
                        |> (fun s -> s[0..min (s.Length-1) Constants.valueColumnMaxChars])
                        |> (fun v -> extra + DrawHelpers.getTextWidthInPixels Constants.valueColumnText v, v.Length+2))
    sampleVals
    |> List.unzip
    |> (fun (ws,nums) -> List.max ws, List.max nums)
    |> (fun (w,num) -> int w + 20, num)

/// Style properties for values column
let valuesColumnStyle (colWidth:int) = Style (
    (waveSimColumn) @ [
        MinWidth colWidth
        Float FloatOptions.Left
        BorderLeft Constants.borderProperties
        OverflowX OverflowOptions.Auto
        BackgroundColor Constants.cursorColor
        Opacity 1.0
        GridColumnStart 3
    ])



/// Style for waveforms column
let waveformColumnStyle = Style [
    GridColumnStart 2
    Display DisplayOptions.Grid
]

/// Style for rows in waveforms column
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

/// Style for viewWaveSim
let viewWaveSimStyle = Style [
    MarginLeft Constants.leftMargin
    MarginRight Constants.rightMargin
    MarginTop "5px"
]

// style for waveforms and RAM viewer
let showWaveformsAndRamStyle (height:float) = Style [
    Width "100%"
    CSSProp.Custom("overflow", "hidden hidden")
    Height $"{height}px"
    ]

/// Style for waveforms only path of viewer
let showWaveformsStyle = Style [
    //
    Width "100%"
    //OverflowY OverflowOptions.Auto
    Display DisplayOptions.Grid
    ColumnCount 3
    GridAutoFlow "column"
    GridAutoColumns "min-content"
    OverflowX OverflowOptions.Visible
]

let calcWaveformHeight wsModel =
    let rowPixels = Constants.rowHeight * wsModel.SelectedWaves.Length
    let wantedHeight = float rowPixels + 0.6 * Constants.viewBoxHeight + 20.0
    wantedHeight


let calcWaveformAndScrollBarHeight wsModel =
    calcWaveformHeight wsModel + 100. + float Constants.scrollBarWidth
/// Props for text in clock cycle row
let clkCycleText m i : IProp list =
    let props : IProp list =
        [
            SVGAttr.FontSize "12px"
            SVGAttr.TextAnchor "middle"
            X (singleWaveWidth m * (float i + 0.5))
            Y (0.6 * Constants.viewBoxHeight)
        ]
    let cursorExtraProps : IProp list =
        [
            SVGAttr.Custom("fontWeight", "bold")
        ]
    if i* m.CycleMultiplier = m.CurrClkCycle then
        cursorExtraProps @ props
    else
        props

/// Style for clock cycle number row
let clkCycleSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom Constants.borderProperties
]

/// Props for waveform column rows
let waveformColumnRowProps m : IProp list = [
    SVGAttr.Height Constants.rowHeight
    SVGAttr.Width (viewBoxWidth m)
    // min-x, min-y, width, height
    ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string Constants.viewBoxHeight)
    PreserveAspectRatio "none"
]

/// Props for row of clock cycle numbers
let clkCycleNumberRowProps m : IProp list = 
    waveformColumnRowProps m @ [
    clkCycleSVGStyle
]

/// Style for each row in waveform column
let waveRowSVGStyle = Style [
    Display DisplayOptions.Block
    BorderBottom "1px solid rgb(219,219,219)"
]

/// Props for each row in waveform column
let waveRowProps m : IProp list =
    waveformColumnRowProps m @ [
    waveRowSVGStyle
]

/// Style of line separating clock cycles
let clkLineStyle = Style [
    Stroke "rgb(200,200,200)"
    StrokeWidth Constants.clkLineWidth
]

/// Grid lines separating clock cycles
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
        SVGAttr.Fill Constants.cursorColor
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
            dispatch <| UpdateWSModel (fun m -> {m with CurrClkCycle = cycle; CurrClkCycleDetail = cycle * m.CycleMultiplier})
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

/// Props for radix tabs
let radixTabProps : IHTMLProp list = [
    Style [
        Width "35px"
        Height Constants.rowHeight
    ]
]

/// Style for A HTML element in radixTabs
let radixTabAStyle = Style [
    Padding "0 0 0 0"
    Height Constants.rowHeight
]

/// Style for radixTabs
let radixTabsStyle = Style [
    Height Constants.rowHeight
    FontSize "80%"
    OverflowX OverflowOptions.Clip
    Display DisplayOptions.Inline
]

/// Style of polyline used to draw waveforms
let wavePolylineStyle points : IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.Fill "none"
    SVGAttr.StrokeWidth Constants.lineThickness
    Points (pointsToString points)
]

let wavePolyfillStyle points : IProp list = [
    SVGAttr.Stroke "none"
    SVGAttr.Fill "lightgrey"
    SVGAttr.StrokeWidth (Constants.lineThickness+2.)
    Points (pointsToString points)
]

/// Style for top half of waveform simulator (instructions and buttons)
let topHalfStyle = Style [
    Position PositionOptions.Sticky
    CSSProp.Top 0
    BackgroundColor "white"
    ZIndex 10000
]

//---------------------------Code for selector details state----------------------------------//

// It would be better to do this with one subfunction and Optics!

/// Sets or clears a subset of ShowSheetDetail
let setWaveSheetSelectionOpen (wsModel: WaveSimModel) (subSheets: string list list) (show: bool) =
    let setChange = Set.ofList subSheets
    let newSelect =
        match show with
        | false -> Set.difference wsModel.ShowSheetDetail setChange
        | true -> Set.union setChange wsModel.ShowSheetDetail
    {wsModel with ShowSheetDetail = newSelect}   

/// Sets or clears a subset of ShowComponentDetail
let setWaveComponentSelectionOpen (wsModel: WaveSimModel) (fIds: FComponentId list)  (show: bool) =
    let fIdSet = Set.ofList fIds
    let newSelect =
        match show with
        | true -> Set.union fIdSet  wsModel.ShowComponentDetail
        | false -> Set.difference wsModel.ShowComponentDetail fIdSet
    {wsModel with ShowComponentDetail = newSelect}


/// Sets or clears a subset of ShowGroupDetail
let setWaveGroupSelectionOpen (wsModel: WaveSimModel) (grps :(ComponentGroup*string list) list)  (show: bool) =
    let grpSet = Set.ofList grps
    let newSelect =
        match show with
        | true -> Set.union grpSet  wsModel.ShowGroupDetail
        | false -> Set.difference wsModel.ShowGroupDetail grpSet
    {wsModel with ShowGroupDetail = newSelect}

let setSelectionOpen (wsModel: WaveSimModel) (cBox: CheckBoxStyle) (show:bool) =
    match cBox with
    | PortItem _ -> failwithf "What? setselectionopen cannot be called from a Port"
    | ComponentItem fc -> setWaveComponentSelectionOpen wsModel [fc.fId] show
    | GroupItem (grp,subSheet) -> setWaveGroupSelectionOpen wsModel [grp,subSheet] show
    | SheetItem subSheet -> setWaveSheetSelectionOpen wsModel [subSheet] show


/// Props for HTML Summary element
let summaryProps (isSummary:bool) cBox (ws: WaveSimModel) (dispatch: Msg -> Unit): IHTMLProp list = [

    let clickHandler (e:Browser.Types.Event) = 
        if isSummary then
            let show =
                match cBox with
                | PortItem _ -> false
                | ComponentItem fc -> Set.contains fc.fId ws.ShowComponentDetail
                | SheetItem subGroup -> Set.contains subGroup ws.ShowSheetDetail
                | GroupItem (compGrp, subSheet) -> Set.contains (compGrp,subSheet) ws.ShowGroupDetail
            dispatch <| UpdateWSModel (fun ws -> setSelectionOpen ws cBox (not show))
    let size,weight =
        match cBox with 
        | SheetItem _ -> "20px", "bold"
        | ComponentItem _ -> "16px", "bold"
        | GroupItem _ -> "18px", "bold"
        | PortItem _ -> "12px", "normal"
    Style [
        FontSize size
        FontWeight weight
    ]
    OnClick clickHandler
]

/// Props for HTML Details element
let detailsProps showDetails cBox (ws: WaveSimModel) (dispatch: Msg -> Unit): IHTMLProp list = 
    let show =
        match cBox with
        | PortItem _ -> false
        | ComponentItem fc -> Set.contains fc.fId ws.ShowComponentDetail
        | SheetItem subGroup -> Set.contains subGroup ws.ShowSheetDetail
        | GroupItem (compGrp, subSheet) -> Set.contains (compGrp,subSheet) ws.ShowGroupDetail
    [
        Open (show || showDetails)
    ]




