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
open Fable.Core
open Fable.Core.JsInterop
open Fulma.Extensions.Wikiki

open WaveSimTypes



/// The waves the viewer draws: those the user has selected that the simulation still holds. All
/// three columns are built from this, so it is also what a row number means anything against.
let selectedWaves (wsModel: WaveSimModel) : Wave list =
    wsModel.SelectedWaves
    |> List.choose (fun wi -> Map.tryFind wi wsModel.AllWaves)

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


let separatorColour = "rgb(219,219,219)"

/// Background for the names column, or for one row of it.
/// The strip at the left, where the view button sits, is painted as pane rather than as column, so
/// that the button reads as standing outside the line the labels start on - which is what it is.
/// The delete cross is inside that line, because it belongs to the label.
/// Any line along the row starts at that same place rather than running out into the strip. It is
/// painted as a second background layer, since a border cannot be inset. The line is given as
/// thickness, colour, and whether it goes along the top of the element rather than the bottom.
let namesColBackground (fill: string) (line: (int * string * bool) option) : CSSProp list =
    let edge = string Constants.viewSymbolWidth + "px"
    let fillLayer = "linear-gradient(to right, white " + edge + ", " + fill + " " + edge + ")"
    match line with
    | None ->
        [ BackgroundImage fillLayer ]
    | Some (thickness, colour, atTop) ->
        [ BackgroundImage
            (fillLayer + ", linear-gradient(to right, transparent " + edge + ", " + colour + " " + edge + ")")
          BackgroundSize ("100% 100%, 100% " + string thickness + "px")
          BackgroundPosition (if atTop then "0 0, 0 0" else "0 0, 0 100%")
          BackgroundRepeat "no-repeat" ]

/// Empty row used in namesColumn and valuesColumn. Shifts these down by one
/// to allow for the row of clk cycle numbers in waveformsColumn.
/// The background carries the line under the row: the names column insets that line past the view
/// button strip, the values column has no strip and draws it as a plain border.
let topRow (ws:WaveSimModel) (background: CSSProp list) topRowContent =
    [ div [ Style (
                [ Height Constants.rowHeight
                  PaddingTop (valueTopPadding ws) ]
                @ background)]
          topRowContent ]

/// The line under the top row of the values column, which has no strip to keep clear of.
let plainTopRowLine = [ BorderBottom ("2px solid " + separatorColour) ]

/// Style for showing error messages in waveform simulator.
let errorMessageStyle = Style [
    Width "90%"
    MarginLeft "5%"
    MarginTop "15px"
]

/// Style of checkboxes
let checkboxStyle = Style [
    //Margin "0 5px 0 5px"
    Cursor "pointer"
    Float FloatOptions.Left
]

/// Props for Checkbox.Input
let checkboxInputProps : IHTMLProp list = [
    Type "checkbox"
    checkboxStyle
]

let boldFontStyle = [
    FontWeight "bold"
    FontSize "14px"
]

let normalFontStyle = [
    FontWeight "normal"
    FontSize "14px"
]

/// Style for top row of buttons
let topRowButtonStyle isRightSide= Style [
    Height ModelHelpers.Constants.wsButtonHeight
    Width ModelHelpers.Constants.wsButtonWidth
    FontSize "16px"
    Flex "0 0.5"
    if isRightSide then MarginLeft "auto" else AlignSelf AlignSelfOptions.FlexStart
    MarginRight Constants.topRowButtonMargin
    MarginLeft Constants.topRowButtonMargin
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

/// SVG group element for tooltip.
/// The props of the tooltip, as well as its text, are set in the function <c>changeToolTip</c>.
/// Initial props make it invisible.
let svgWaveToolTip (ws: WaveSimModel) : ReactElement =
    let iProps = singleValueOnWaveProps true ws.WSConfig.FontSize ws.WSConfig.FontWeight 0.
    EvilHoverCache.evilSvgToolTip "waveTip" ws "" iProps

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
///
/// No AutoFocus. React focuses an autofocused element every time it mounts, so this box took the
/// keyboard whenever the viewer's control row was rebuilt - on Start, and after a cursor move or a
/// zoom. The keyboard then belonged to a text field, and the Left and Right arrows that step the
/// cursor stopped working until something else was clicked. You click into a box to type in it;
/// SelectedComponentView's name box says the same thing for the same reason.
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
let nameRowLevelStyle isHovered = Style (
    [ Height Constants.rowHeight
      if isHovered then Cursor "grab" ]
    // The row's own colour, and the line separating it from the next, both start at the label line
    // rather than at the edge of the pane - the strip to the left of that belongs to the view
    // button. A hovered row must repaint the strip too, or its highlight would cover it.
    @ namesColBackground
        (if isHovered then "hsl(0, 0%, 96%)" else "transparent")
        (Some (1, separatorColour, false)))

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

/// Eye, drawn to the size of the box it is given. Marks the button which shows a waveform's
/// component on the schematic.
let eyeSvg (colour: string) (size: string) =
    svg [
        ViewBox "0 0 576 512"
        SVGAttr.Height size
        SVGAttr.Width size
    ] [
        path [
            SVGAttr.Fill colour
            D "M288 96c-93 0-171 56-224 160 53 104 131 160 224 160s171-56 224-160c-53-104-131-160-224-160zm0
               266c-58 0-106-47-106-106s48-106 106-106 106 47 106 106-48 106-106 106zm0-170c-35 0-64 29-64 64s29
               64 64 64 64-29 64-64-29-64-64-64z"
        ] []
    ]

/// Style of the button which shows a waveform's component on the schematic. It sits in a slot of
/// its own at the outer edge of the name row, beyond the delete icon, so that it reads as an extra
/// beside the label rather than as one of the label's own controls. Green when the component cannot
/// be seen and grey when it is already in front of the user; shown only while the row is hovered.
let viewSymbolButtonStyle (isInView: bool) = Style [
    Width Constants.viewSymbolWidth
    Height Constants.viewSymbolWidth
    MinWidth Constants.viewSymbolWidth
    Display DisplayOptions.Flex
    AlignItems AlignItemsOptions.Center
    JustifyContent "center"
    BorderRadius "4px"
    MarginRight "2px"
    BackgroundColor (if isInView then "hsl(0, 0%, 71%)" else "hsl(141, 53%, 53%)")
    Cursor "pointer"
]

       
       

/// Calculate the necessary with of the naes column based on the longest name.
let calcNamesColWidth (ws:WaveSimModel) : int =
    let cWidth =
        // Measured at 10px and scaled, rather than at the configured size directly, so the column
        // width stays proportional to the font size the user picked. defaultText's weight is
        // "normal", which is what this shorthand meant when it left the weight out.
        let refFont =
            { DrawHelpers.defaultText with
                FontSize = "10px"
                FontFamily = Constants.columnFontFamily }
        let getWidth (txt:string) =
            let sizeInPx = float (ws.WSConfig.FontSize)
            sizeInPx * DrawHelpers.getTextWidthInPixels refFont txt / 10.0
        selectedWaves ws
        |> List.map (fun wave -> wave.ViewerDisplayName)
        |> (fun lst -> "Dummy" :: lst)
        |> List.map getWidth
        |> List.max
        |> System.Math.Ceiling
        |> int
    cWidth + Constants.deleteSymbolWidth + Constants.viewSymbolWidth


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
    (waveSimColumn)
    // The column's top border is drawn as a background layer instead, so that it too stops at the
    // label line rather than running out across the view button strip.
    @ namesColBackground Constants.namesValuesColumnColor (Some (2, separatorColour, true))
    @ [
        BorderTop "none"
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
let waveRowsStyle (wsModel: WaveSimModel) = Style [
    Height "100%"
    OverflowX OverflowOptions.Hidden
    Display DisplayOptions.Grid
    //FontSize "13px"
    GridAutoRows Constants.rowHeight
    BorderTop Constants.borderProperties
    Width wsModel.WaveformColumnWidth
    GridColumnStart 1
    GridRowStart 1
    // Banding behind every other waveform row, so each trace reads against its own row rather
    // than floating in shared white space. Painted here, on the container, so it sits under the
    // wave SVGs (which have no background of their own) and under the cursor-column overlay.
    // The first stripe of the pattern falls on the clock-number row and is left clear; the
    // background is sized to exactly the rows drawn so the band cannot run on into any slack
    // below the last waveform.
    BackgroundImage
        ($"repeating-linear-gradient(to bottom, transparent 0px, transparent {Constants.rowHeight}px, "
         + $"{Constants.rowBandColor} {Constants.rowHeight}px, {Constants.rowBandColor} {2 * Constants.rowHeight}px)")
    BackgroundSize $"100%% {(List.length (selectedWaves wsModel) + 1) * Constants.rowHeight}px"
    BackgroundRepeat "no-repeat"
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

/// The height the waveform table wants: one row per waveform DRAWN, which is a selected wave that
/// AllWaves still holds, plus the row of clock cycle numbers above them.
let calcWaveformHeight wsModel =
    let rowPixels = Constants.rowHeight * (selectedWaves wsModel).Length
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
    // i and CursorDisplayCycle are both sampled (displayed) cycle numbers, so they compare directly:
    // it is the printed label (n, above) that is scaled by the sampling zoom.
    if i = m.CursorDisplayCycle then
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
    Opacity 0.5
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

/// Change Tooltip SVG element based on mouse position.
let setWaveToolTip (m: WaveSimModel) (ev:Browser.Types.MouseEvent) =
    let svgHighlight = Browser.Dom.document.getElementById "ClkCycleHighlight"
    let bcr = svgHighlight.getBoundingClientRect ()
    let cycle = (int <| ((ev.clientX - bcr.left) / singleWaveWidth m)) + m.StartCycle
    let waveNum = (int <| (ev.clientY - bcr.top) / float Constants.rowHeight) - 1
    // The row under the pointer, counted off the rows that are DRAWN. Every column of the viewer
    // is built from selectedWaves, so that is the only list a row number means anything against.
    let numValText =
        match List.tryItem waveNum (selectedWaves m) with
        | None -> ""
        | Some wave -> EvilHoverCache.getWaveToolTip cycle wave m
    let ttXPos = float (cycle - m.StartCycle) * singleWaveWidth m
    let ttYPos = ( float waveNum * float Constants.rowHeight + 16. / 2.)
    // getWaveToolTip labels the value itself, since what it has to say may be a hidden value, the
    // comment against the memory location a ROM is reading, or both
    let ttText = if numValText = "" then "" else $"Cycle:{cycle*m.SamplingZoom}. {numValText}"
    // The tooltip may run on past the right of the waveforms and over the values column, which is
    // drawn under it - so that is where it has to fit, not within the waveforms alone. A ROM's
    // comment is a sentence rather than a number, and cutting it at the edge of the waveforms is
    // what made it unreadable in a narrow window.
    let ttXMaxEdge = float m.ShownCycles * singleWaveWidth m + float (fst (valuesColumnSize m))
    EvilHoverCache.changeToolTip "waveTip" ttText ttXPos ttYPos ttXMaxEdge (numValText <> "")

/// Controls the background highlighting of which clock cycle is selected
let cursorCycleHighlightSVG m dispatch =
    // the rows drawn, as everywhere else: this sizes the SVG the rows are hit-tested in
    let count = List.length (selectedWaves m)
    svg [
        SVGAttr.Fill Constants.cursorColumnColor
        SVGAttr.Opacity 1.0 //Constants.cursorColumnOpacity

        Style [
            GridColumnStart 1
            GridRowStart 1
            ZIndex 33
            // An svg clips whatever falls outside its viewport, and this one is only as wide as the
            // waveforms. The tooltip drawn in it has to be able to run out over the names and values
            // columns to either side, which it is drawn above, or a long one is cut off. What is
            // outside the pane is still clipped, by the scrolling div these all sit in.
            CSSProp.Overflow OverflowOptions.Visible
        ]
        SVGAttr.Height (string ((count + 1) * Constants.rowHeight) + "px")
        SVGAttr.Width (viewBoxWidth m)
        ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string (Constants.viewBoxHeight * float (count + 1)))
        Id "ClkCycleHighlight"
        OnMouseMove (setWaveToolTip m)
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
        (List.concat [

            [
                rect [
                    SVGAttr.Width (singleWaveWidth m)
                    SVGAttr.Height "100%"
                    SVGAttr.Opacity 0.2
                    X (float (m.CursorDisplayCycle - m.StartCycle) * (singleWaveWidth m))
                ] []
            ]
            
            (backgroundSVG m count)

            [ svgWaveToolTip m ] // reactElement for tooltip made visible, text chnaged, and moved as needed

        ]
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
    // Line the right-hand edge of the controls up with the right-hand edge of the waveform
    // table below them, rather than letting them run on to the edge of the window. The
    // buttons and the control row each carry a margin of their own, so only the rest of the
    // table's gap is needed here.
    MarginRight (Constants.waveTableRightGap - Constants.topRowButtonMargin)
    // and the same on the left, against the line the labels start on rather than the edge of the
    // pane: the strip outside that line belongs to the view buttons, which are meant to stand out
    // of it. Padding rather than margin, so the sticky white background still covers the strip.
    PaddingLeft (Constants.viewSymbolWidth - Constants.topRowButtonMargin)
]

//---------------------------Code for selector details state----------------------------------//

// It would be better to do this with one subfunction and Optics!

/// Open or close nodes of the wave selector's hierarchy, each named by its path of design-time
/// sheet names.
///
/// Opening one closes any other node of the same sheet. A sheet that several routes reach appears
/// in several places, and showing more than one of them at once is the multiplying that collapsing
/// the hierarchy is there to prevent. Where only one route reaches a sheet this is vacuous - there
/// is only one such node - so the rule needs no test for which kind of sheet it is.
let setWaveSheetSelectionOpen (wsModel: WaveSimModel) (subSheets: string list list) (show: bool) =
    let setChange = Set.ofList subSheets
    let newSelect =
        match show with
        | false -> Set.difference wsModel.ShowSheetDetail setChange
        | true ->
            let sheetsOpening = subSheets |> List.choose List.tryLast |> Set.ofList
            wsModel.ShowSheetDetail
            |> Set.filter (fun openKey ->
                match List.tryLast openKey with
                | Some sheet -> not (Set.contains sheet sheetsOpening)
                | None -> true)
            |> Set.union setChange
    {wsModel with ShowSheetDetail = newSelect}

/// Sets or clears a subset of ShowGroupDetail
let setWaveGroupSelectionOpen (wsModel: WaveSimModel) (grps :(ComponentGroup*string list) list)  (show: bool) =
    let grpSet = Set.ofList grps
    let newSelect =
        match show with
        | true -> Set.union grpSet  wsModel.ShowGroupDetail
        | false -> Set.difference wsModel.ShowGroupDetail grpSet
    {wsModel with ShowGroupDetail = newSelect}

let setSelectionOpen (cBox: CheckBoxStyle) (show:bool) (wsModel: WaveSimModel)=
    match cBox with
    | GroupItem (grp,subSheet) -> setWaveGroupSelectionOpen wsModel [grp,subSheet] show
    | SheetItem subSheet -> setWaveSheetSelectionOpen wsModel [subSheet] show

/// Props for HTML Summary element
/// <param name="isSummary">True if this is a summary element, false if it is a details element.</param>
/// isSummary is used to determine if the click handler is used.
let summaryProps (isSummary:bool) cBox (ws: WaveSimModel) (dispatch: Msg -> Unit): IHTMLProp list = [

    let summaryOpenCloseClickHandler (e:Browser.Types.Event) =
        if isSummary then
            let show =
                match cBox with
                | SheetItem subGroup -> Set.contains subGroup ws.ShowSheetDetail
                | GroupItem (compGrp, subSheet) -> Set.contains (compGrp,subSheet) ws.ShowGroupDetail
            dispatch <| UpdateWSModel (setSelectionOpen cBox (not show))

    let size,weight =
        match cBox with
        | SheetItem _ -> "20px", "bold"
        | GroupItem _ -> "14px", "bold"
    Style [
        FontSize size
        FontWeight weight
    ]
    OnClick summaryOpenCloseClickHandler
]

/// Props for HTML Details element
let detailsProps showDetails cBox (ws: WaveSimModel) (dispatch: Msg -> Unit): IHTMLProp list =
    let show =
        match cBox with
        | SheetItem subGroup -> Set.contains subGroup ws.ShowSheetDetail
        | GroupItem (compGrp, subSheet) -> Set.contains (compGrp,subSheet) ws.ShowGroupDetail
    [
        Open (show || showDetails)
    ]




