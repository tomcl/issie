module WaveSimStyle

open ModelType
open WaveSimHelpers

open Fulma
open Fable.React
open Fable.React.Props

module Constants =
    // Width of names column - replaced by calcNamesColWidth function

    /// Width of values column
    let valuesColWidth = 100
    let deleteSymbolWidth = 20
    let scrollBarWidth = 15

    /// Width of left margin of waveform simulator
    let leftMargin = 30
    /// Width of right margin of waveform simulator
    let rightMargin = 30

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

    /// Border between columns and headers of waveform viewer.
    let borderProperties = "2px solid rgb(219,219,219)"

    /// Padding between name label/value label and waveform column.
    let labelPadding = 3
    /// Color for cursor and values column
    let cursorColor = "Lavender"



/// Style for top row in wave viewer.
let topRowStyle = Style [
    Height Constants.rowHeight
    BorderBottom "2px solid rgb(219,219,219)"
]

/// Empty row used in namesColumn and valuesColumn. Shifts these down by one
/// to allow for the row of clk cycle numbers in waveformsColumn.
let topRow = [ div [ topRowStyle ] [] ]

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
let topRowButtonStyle = Style [
    Height Constants.rowHeight
    Width Constants.colWidth
    FontSize "16px"
    Position PositionOptions.Relative
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
                FontSize "15px"
                Width "20px"
                Position PositionOptions.Relative
                MarginRight 0
                MarginLeft 0
            ]
        ]
    ]

let topHalfButtonProps color = [
    Button.Color color
    Button.Props [topRowButtonStyle]
]

let selectRamButtonProps = topHalfButtonProps IsInfo

/// Props for selectRamButton when no RAMs are selectable
let selectRamButtonPropsLight =
    selectRamButtonProps @ [Button.IsLight]


let topHalfButtonPropsWithWidth color = [
    Button.Color color
    Button.Props [topRowButtonStyle]
]

/// Props for selectWavesButton
let selectWavesButtonProps = topHalfButtonProps IsInfo


/// Props for selectWavesButton when no waves are selectable
let selectWavesButtonPropsLight =
    selectWavesButtonProps @ [Button.IsLight]

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

/// Props for displaying values on non-binary waves
let valueOnWaveProps m i start width : IProp list = [
    X (start * (singleWaveWidth m) + Constants.nonBinaryTransLen + float i * width)
    Y (0.6 * Constants.viewBoxHeight)
    Style [
        FontSize Constants.fontSizeValueOnWave
    ]
]

/// Style for clock cycle buttons
let clkCycleButtonStyle = Style [
    Float FloatOptions.Right
    Position PositionOptions.Relative
    Height Constants.rowHeight
    TextAlign TextAlignOptions.Center
    Display DisplayOptions.InlineBlock
    FontSize "13px"
]

/// Style for clock cycle text Input field
let clkCycleInputStyle = Style [
    Margin "0 0 0 0"
    Float FloatOptions.Left
    TextAlign TextAlignOptions.Center
    Width "60px"
    Height Constants.rowHeight
    Display DisplayOptions.InlineBlock
    FontSize "13px"
    BorderColor "gray"
    BorderWidth "1px 0.5px 1px 0.5px"
    BorderRadius 0
]

/// Props for clock cycle text Input field
let clkCycleInputProps : IHTMLProp list = [
    Min 0
    SpellCheck false
    Step 1
    clkCycleInputStyle
]

/// List of Style properties for clock cycle button
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

/// Style for inner clock cycle buttons (buttons to move by one clock cycle)
let clkCycleInnerStyle = Style (
    clkCycleBut @ [
        BorderRadius 0
    ]
)

/// Style for left-most clock cycle button
let clkCycleLeftStyle = Style (
    clkCycleBut @ [
        BorderTopLeftRadius "4px"
        BorderBottomLeftRadius "4px"
        BorderTopRightRadius 0
        BorderBottomRightRadius 0
        BorderRightWidth "0.5"
    ])

/// Style for right-most clock cycle button
let clkCycleRightStyle = Style (
    clkCycleBut @ [
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
        Left 0
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
        MinWidth (calcNamesColWidth ws)
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
    MarginTop "15px"
    Height "calc(100% - 72px)"
]

// style for waveforms and RAM viewer
let showWaveformsAndRamStyle = Style [
    OverflowY OverflowOptions.Auto; Height "calc(100% - 250px)"
    ]

/// Style for waveforms only path of viewer
let showWaveformsStyle = Style [
    //Height "calc(100% - 50px)"
    Width "100%"
    //OverflowY OverflowOptions.Auto
    Display DisplayOptions.Grid
    ColumnCount 3
    GridAutoFlow "column"
    GridAutoColumns "min-content"
]



/// Props for text in clock cycle row
let clkCycleText m i : IProp list = [
    SVGAttr.FontSize "12px"
    SVGAttr.TextAnchor "middle"
    X (singleWaveWidth m * (float i + 0.5))
    Y (0.6 * Constants.viewBoxHeight)
]

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
            dispatch <| UpdateWSModel (fun m -> {m with CurrClkCycle = cycle})
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
    Float FloatOptions.Right
]

/// Style of polyline used to draw waveforms
let wavePolylineStyle points : IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.Fill "none"
    SVGAttr.StrokeWidth Constants.lineThickness
    Points (pointsToString points)
]

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

/// Style for top half of waveform simulator (instructions and buttons)
let topHalfStyle = Style [
    Position PositionOptions.Sticky
    Top 0
    BackgroundColor "white"
    ZIndex 10000
]

/// display react of refresh button with color (e.g. white) at given height (e.g. 10px)
let refreshSvg (color:string) (height:string)=
    svg [
            ViewBox "0 0 512 512"
            SVGAttr.Height height
        ] [
            path [
                D "M496 48V192c0 17.69-14.31 32-32 32H320c-17.69 0-32-14.31-32-32s14.31-32
                32-32h63.39c-29.97-39.7-77.25-63.78-127.6-63.78C167.7 96.22 96 167.9 96 256s71.69
                159.8 159.8 159.8c34.88 0 68.03-11.03 95.88-31.94c14.22-10.53 34.22-7.75 44.81
                6.375c10.59 14.16 7.75 34.22-6.375 44.81c-39.03 29.28-85.36 44.86-134.2 44.86C132.5
                479.9 32 379.4 32 256s100.5-223.9 223.9-223.9c69.15 0 134 32.47 176.1 86.12V48c0-17.69
                14.31-32 32-32S496 30.31 496 48z"
                Style [
                    Fill color
                    Stroke color
                    StrokeWidth "5px"
                ]
            ] []
        ]

let emptyRefreshSVG =
    svg [
        SVGAttr.Height "20"
        SVGAttr.Width "20"
    ] []




let inline updateViewerWidthInWaveSim w (model:Model) =
    let wsModel = getWSModel model
    //dispatch <| SetViewerWidth w
    let namesColWidth = calcNamesColWidth wsModel
    printfn $"Updating viewer namesWith = {namesColWidth}"

    /// The +4 is probably because of some unnacounted for padding etc (there is a weird 2px spacer to right of the divider)
    let otherDivWidths = Constants.leftMargin + Constants.rightMargin + DiagramStyle.Constants.dividerBarWidth + Constants.scrollBarWidth + 2

    /// This is what the overall waveform width must be
    let valuesColumnWidth,_ = valuesColumnSize wsModel
    let waveColWidth = w - otherDivWidths - namesColWidth - valuesColumnWidth

    /// Require at least one visible clock cycle: otherwise choose number to get close to correct width of 1 cycle
    let wholeCycles = max 1 (int (float waveColWidth / singleWaveWidth wsModel))


    let singleCycleWidth = float waveColWidth / float wholeCycles

    let viewerWidth = namesColWidth + Constants.valuesColWidth + int (singleCycleWidth * float wholeCycles) + otherDivWidths

    let updateFn wsModel = 
        {
        wsModel with
            ShownCycles = wholeCycles
            WaveformColumnWidth = singleCycleWidth * float wholeCycles
        }
    {model with WaveSimViewerWidth = w}
    |> ModelHelpers.updateWSModel updateFn



let inline setViewerWidthInWaveSim w (model:Model) dispatch =
    dispatch <| UpdateModel (updateViewerWidthInWaveSim w)
    dispatch <| GenerateCurrentWaveforms
