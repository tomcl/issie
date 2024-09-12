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
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = "5px" } // dummy size
    /// Whitespace padding between repeated values displayed on non-binary waves.
    let valueOnWavePadding = 75.0
    /// Whitespace padding between non-binary wave values and the edge of transition.
    let valueOnWaveEdgePadding = 4.0

    /// Border between columns and headers of waveform viewer.
    let borderProperties = "2px solid rgb(219,219,219)"

    /// Padding between name label/value label and waveform column.
    let labelPadding = 3
    /// Color for cursor and values column
    let namesValuesColumnColor = "Lavender"
    let cursorColumnColor = "purple"
    let cursorColumnOpacity = 0.15

    /// <summary>Height of scrollbar, in pixels. Affects only the SVG and not the buttons.
    /// Currently set to same height as buttons.</summary>
    let softScrollBarWidth: float = 25.0

    /// <summary>Minimum width of the scrollbar thumb, in pixels.</summary>
    let scrollbarThumbMinWidth: float = 10.0

    /// height of the top half of the wave sim window (including tabs) when waveforms are displayed
    let topHalfHeight = 260.

    // helpers constants
    /// initial time running simulation without spinner to check speed (in ms)
    let initSimulationTime = 100.
    /// max estimated time to run simulation and not need a spinner (in ms)
    let maxSimulationTimeWithoutSpinner = 300.


    /// initial time making waveforms without spinner to check speed (in ms)
    let initWaveformTime = 50.
        /// max estimated time to generate new waveforms and not need a spinner (in ms)
    let maxWaveCreationTimeWithoutSpinner = 100.



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
    let maxRamRowsDisplayed = 50
    let maxRamLocsWithSparseDisplay = 100



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
let screenWidth() = Browser.Dom.document.defaultView.innerWidth


/// Width of one clock cycle.
let singleWaveWidth m = max 5.0 (float m.WaveformColumnWidth / float m.ShownCycles)

/// Left-most coordinate of the SVG viewbox.
let viewBoxMinX m = string 0

/// Total width of the SVG viewbox.
let viewBoxWidth m = string (max 5.0 (m.WaveformColumnWidth))

/// Right-most visible clock cycle.
let endCycle wsModel = wsModel.StartCycle + (wsModel.ShownCycles) - 1

let zoomOutSVG = DiagramStyle.zoomOutSVG
let zoomInSVG = DiagramStyle.zoomInSVG

let valueTopPadding (ws:WaveSimModel) =
    (float Constants.rowHeight - float ws.WSConfig.FontSize - 8.) / 2.


/// Empty row used in namesColumn and valuesColumn. Shifts these down by one
/// to allow for the row of clk cycle numbers in waveformsColumn.
let topRow (ws:WaveSimModel) topRowContent =
    [ div [ Style [
                Height Constants.rowHeight
                BorderBottom "2px solid rgb(219,219,219)"
                PaddingTop (valueTopPadding ws)
            ]]
          topRowContent ]

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
    Height ModelHelpers.Constants.wsButtonHeight
    Width ModelHelpers.Constants.wsButtonWidth
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

let topHalfButtonPropsLoading isLoading color buttonId isRightSide = [
    Button.Color color
    Button.IsLoading isLoading
    Button.Props [HTMLAttr.Id buttonId ; topRowButtonStyle isRightSide]
]

let topHalfButtonProps = topHalfButtonPropsLoading false

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

/// <summary>Props for displaying values on non-binary waves by starting position.</summary>
/// <param name="xpos">Starting X-direction position.</param>
let singleValueOnWaveProps isStart textFont textWeight xpos: list<IProp> = [
    X xpos
    Y (0.5 * Constants.viewBoxHeight + textFont / 2.)
    Style [ TextAnchor (if isStart then "start" else "end"); FontFamily "Helvetica"; FontSize textFont; FontWeight textWeight ]
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
    Width "80px"
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
    AutoFocus true
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
let valueLabelStyle (ws: WaveSimModel)= 
    Style [
        Height Constants.rowHeight
        BorderBottom "1px solid rgb(219,219,219)"
        PaddingLeft Constants.labelPadding
        PaddingTop (valueTopPadding ws)
        FontFamily Constants.valueColumnFontFamily
        FontSize ws.WSConfig.FontSize
        FontWeight ws.WSConfig.FontWeight
    ]

/// Prop for Level.left in name row.
let nameRowLevelLeftProps (visibility: string): IHTMLProp list = [
    Style [
        Position PositionOptions.Sticky
        CSSProp.Left 0
        Visibility visibility
    ]
]

       
       

/// Calculate the necessary with of the naes column based on the longest name.
let calcNamesColWidth (ws:WaveSimModel) : int =
    let cWidth =
        // technical debt - could be replaced by proper call now?
        DrawHelpers.canvasWidthContext.font <- String.concat " " ["10px"; Constants.columnFontFamily]; // e.g. "16px bold sans-serif";
        let getWidth (txt:string) =
            let sizeInPx = float (ws.WSConfig.FontSize)   
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
    BorderTop Constants.borderProperties
    Height "100%"
    Width "100%"
    Display DisplayOptions.Grid
    GridAutoRows Constants.rowHeight
    //FontSize Constants.valueColumnFontSize
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
        BackgroundColor Constants.namesValuesColumnColor
        FontSize ws.WSConfig.FontSize
        FontWeight ws.WSConfig.FontWeight
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

let valueColumnTextStyle wsModel = {
    DrawHelpers.defaultText with
        FontSize = $"{wsModel.WSConfig.FontSize}px";
        FontWeight = string wsModel.WSConfig.FontWeight
        FontFamily = Constants.valueColumnFontFamily}

let valuesColumnSize wsModel =
    let colText = valueColumnTextStyle wsModel
    let widthOfOneChar = DrawHelpers.getTextWidthInPixels colText "0"
    let selWaves = selectedWaves wsModel
    let maxValueBusWidth: int =
        selWaves
        |> List.map (fun wave -> wave.Width)
        |> (fun lis -> 1 :: lis)
        |> List.max
    let sampleVals = 
        [maxValueBusWidth; min maxValueBusWidth NumberHelpers.Constants.maxBinaryDisplayWidth]
        |> List.map (fun num ->
                        let worstCaseVal, extra =
                            match wsModel.Radix with
                            | CommonTypes.Bin -> (1I <<< num - 1), 10.
                            | CommonTypes.Hex  -> (1I <<< num) - 1I, 10.
                            | CommonTypes.Dec -> (1I <<< num), 2.*widthOfOneChar
                            | CommonTypes.SDec -> (1I <<< (num-1)),  3.*widthOfOneChar
                        let (fd: SimGraphTypes.FastData) = {Dat=SimGraphTypes.BigWord worstCaseVal; Width=num+3}
                        NumberHelpers.fastDataToPaddedString Constants.valueColumnMaxChars wsModel.Radix fd
                        |> (fun v ->
                            let width =  DrawHelpers.getTextWidthInPixels colText v
                            extra + 1.05 * width, v.Length+2))
    sampleVals
    |> List.unzip
    |> (fun (ws,nums) -> List.max ws, List.max nums)
    |> (fun (w,num) ->
        int w, num)

/// Style properties for values column
let valuesColumnStyle (ws: WaveSimModel) (colWidth:int) =
    let size = ws.WSConfig.FontSize
    let weight = ws.WSConfig.FontWeight
    Style (
        (waveSimColumn) @ [
            FontSize size
            FontWeight weight
            MinWidth colWidth
            Float FloatOptions.Left
            BorderLeft Constants.borderProperties
            OverflowX OverflowOptions.Auto
            BackgroundColor Constants.namesValuesColumnColor
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
    //FontSize "13px"
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
            X (singleWaveWidth m * (float (i - m.StartCycle) + 0.5))
            Y (0.6 * Constants.viewBoxHeight)
        ]
    let cursorExtraProps : IProp list =
        [
            SVGAttr.Custom("fontWeight", "bold")
        ]
    if i * m.SamplingZoom = m.CursorDisplayCycle then
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
let cursorCycleHighlightSVG m dispatch =
    let count = List.length m.SelectedWaves
    svg [
        Style [
            GridColumnStart 1
            GridRowStart 1
        ]
        SVGAttr.Height (string ((count + 1) * Constants.rowHeight) + "px")
        SVGAttr.Width (viewBoxWidth m)
        SVGAttr.Fill Constants.cursorColumnColor
        SVGAttr.Opacity Constants.cursorColumnOpacity
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
            dispatch <| UpdateWSModel (fun m -> {m with CursorDisplayCycle = cycle; CursorExactClkCycle = cycle * m.SamplingZoom})
        )
        ]
        (List.append 
            [
                rect [
                    SVGAttr.Width (singleWaveWidth m)
                    SVGAttr.Height "100%"
                    X (float (m.CursorDisplayCycle - m.StartCycle) * (singleWaveWidth m))
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




