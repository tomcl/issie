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

let noBorderStyle = [
    BorderWidth 0
    BorderCollapse "collapse"
]

/// Style for selectRamButton
let selectRamButtonStyle = [
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

/// SVG group element for tooltip.
/// The props of the tooltip, as well as its text, are set in the function <c>changeToolTip</c>.
/// Initila props make it invisible.
let svgToolTip (ws: WaveSimModel) tipText (xPos:float) (yPos:float) =
    g [Id "toolTipGroup"] [
        rect [
            Id "svgToolTipRect2"
            SVGAttr.Width 50.0
            SVGAttr.Height 20.0
            SVGAttr.Fill "black"
            SVGAttr.Opacity Constants.tooltipShadowOpacity
            Style [ Visibility "hidden" ]
        ] []
        rect [
            Id "svgToolTipRect1"
            SVGAttr.Width 50.0
            SVGAttr.Height 20.0
            SVGAttr.Fill Constants.tooltipBackgroundColor
            SVGAttr.Opacity 1.0
            Style [ Visibility "hidden" ]
        ] []
        text (
            Id "svgToolTipText" ::
            SVGAttr.Fill Constants.tooltipTextColour ::
            SVGAttr.Opacity "1.0" ::
            singleValueOnWaveProps true ws.WSConfig.FontSize ws.WSConfig.FontWeight xPos
        ) [str tipText]

    ]
/// <summary>Change the tooltip text and position.</summary>
/// <param name="tipText">Text to display in the tooltip.</param>
/// <param name="xPos">X-coordinate of the tooltip.</param>
/// <param name="yPos">Y-coordinate of the tooltip.</param>
/// <param name="ttXMaxEdge">Maximum X-coordinate of the tooltip right edge.</param>
/// <param name="isVisible">True if the tooltip is visible, false if it is hidden.</param>
let changeToolTip tipText (xPos:float) (yPos:float) (ttXMaxEdge: float) (isVisible: bool)=
    let svgText = Browser.Dom.document.getElementById "svgToolTipText"
    let tooltipGroup = Browser.Dom.document.getElementById "toolTipGroup"
    let changeShape shapeId w x y show =
        let shape = Browser.Dom.document.getElementById shapeId
        shape.setAttributeNS("", "width", string w)
        shape.setAttributeNS("", "x", string x)
        shape.setAttributeNS("", "y", string y)
        shape.setAttributeNS("", "style", if show then "visibility: visible" else "visibility: hidden")
    if svgText = null then
        printfn "Can't find svgToolTipText in changeToolTip"
    else
        svgText.textContent <- tipText
        let w = svgText?getComputedTextLength()
        let adjXPos = if xPos + w + 10. > ttXMaxEdge then ttXMaxEdge - w - 10. else xPos
        changeShape "svgToolTipRect1" (w+10.) adjXPos yPos isVisible
        changeShape "svgToolTipRect2" (w+10.) (adjXPos + 2.) (yPos + 2.) isVisible
        changeShape "svgToolTipText" w (adjXPos + 5.) (yPos + 16.) isVisible

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

/// Controls the background highlighting of which clock cycle is selected
let cursorCycleHighlightSVG m dispatch =
    let count = List.length m.SelectedWaves
    svg [
        SVGAttr.Fill Constants.cursorColumnColor
        SVGAttr.Opacity 1.0 //Constants.cursorColumnOpacity

        Style [
            GridColumnStart 1
            GridRowStart 1
            ZIndex 33
        ]
        SVGAttr.Height (string ((count + 1) * Constants.rowHeight) + "px")
        SVGAttr.Width (viewBoxWidth m)
        ViewBox (viewBoxMinX m + " 0 " + viewBoxWidth m  + " " + string (Constants.viewBoxHeight * float (count + 1)))
        Id "ClkCycleHighlight"
        OnMouseMove (fun ev ->
            let svgEl = Browser.Dom.document.getElementById "svgToolTip"
            let svgHighlight = Browser.Dom.document.getElementById "ClkCycleHighlight"
            let bcr = svgHighlight.getBoundingClientRect ()
            let cycleWidth = bcr.width / float m.ShownCycles
            let numWaves = List.length m.SelectedWaves
           

            let cycle = (int <| ((ev.clientX - bcr.left) / singleWaveWidth m)) + m.StartCycle
            let waveNum = (int <| (ev.clientY - bcr.top) / float Constants.rowHeight) - 1
            let numValText = EvilHoverCache.getWaveToolTip cycle waveNum m
            let ttXPos = (float cycle * singleWaveWidth m)
            let ttYPos = ( float waveNum * float Constants.rowHeight + 16. / 2.)
            let ttText = if numValText = "" then "" else $"Cycle:{cycle}. Value:{numValText}"
            let ttXMaxEdge = float m.ShownCycles * singleWaveWidth m
            changeToolTip ttText ttXPos ttYPos ttXMaxEdge (numValText <> "")


        )
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

            [ svgToolTip m "" 100. 200.]

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
    | x -> failwithf $"What? setSelectionOpen cannot be called from a '{x}' item"

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
                | _ -> failwithf "Not currently used"
            dispatch <| UpdateWSModel (setSelectionOpen cBox (not show))

    let size,weight =
        match cBox with 
        | SheetItem _ -> "20px", "bold"
        | ComponentItem _ -> "16px", "bold"
        | GroupItem _ -> "14px", "bold"
        | PortItem _ -> "14px", "normal"
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
        | _ -> failwithf "Not currently used"
    [
        Open (show || showDetails)
    ]




