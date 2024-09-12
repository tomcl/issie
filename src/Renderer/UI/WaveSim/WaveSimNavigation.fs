/// All the code that determines which clock cycles are viewed in the simulator.
/// basic functions are zooming and panning (moving + or - in time). Additional function
/// is a sample-based zoom for viewing very long waveforms..
/// Also implement cursor control.
module WaveSimNavigation

//---------------------------------------------------------------------------------------//
//-----------------------Waveform Simulator Navigation-----------------------------------//
//------Scrollbar, zoom buttons, cursor control button, Sampling zoom button-------------//
//---------------------------------------------------------------------------------------//


open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open TopMenuView
open SimGraphTypes
open WaveSimStyle.Constants

/// <summary>Generate scrollbar SVG info based on current <c>WaveSimModel</c>.
/// Called in <c>refreshWaveSim</c> after <c>WaveSimModel</c> has been changed.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <returns>Anonymous record contaning the information to be updated: thumb width,
/// thumb position, and number of cycles the background represents.</returns>
/// <remarks>Note: <c>bkg</c> = background; <c>tb</c> = thumb.</remarks>
let generateScrollbarInfo (wsm: WaveSimModel): {| tbWidth: float; tbPos: float; bkgRep: int |} =
    let mult = wsm.SamplingZoom
    let bkgWidth = wsm.ScrollbarBkgWidth - 60. // 60 = 2x width of buttons

    /// <summary>Return target value when within min and max value, otherwise min or max.</summary>
    let bound (minV: int) (maxV: int) (tarV: int): int = tarV |> max minV |> min maxV
    let currShownMaxCyc = wsm.StartCycle + wsm.ShownCycles
    let newBkgRep = [ wsm.ScrollbarBkgRepCycs; currShownMaxCyc; wsm.ShownCycles*2 ] |> List.max |> bound 0 (wsm.WSConfig.LastClock / mult)

    let tbCalcWidth = bkgWidth / (max 1. (float newBkgRep / float wsm.ShownCycles))
    let tbWidth = max tbCalcWidth Constants.scrollbarThumbMinWidth
    
    let tbMoveWidth = bkgWidth - tbWidth
    let tbPos = (float wsm.StartCycle) / (float newBkgRep - float wsm.ShownCycles) * tbMoveWidth

    // debug statements:
    // printfn "DEBUG:generateScrollbarInfo: Input -"
    // printfn "DEBUG:generateScrollbarInfo: wsm.CurrClkCycle = %d cycles" wsm.CurrClkCycle
    // printfn "DEBUG:generateScrollbarInfo: wsm.StartCycle = %d cycles" wsm.StartCycle
    // printfn "DEBUG:generateScrollbarInfo: wsm.ShownCycles = %d cycles" wsm.ShownCycles
    // printfn "DEBUG:generateScrollbarInfo: wsm.ScrollbarBkgRepCycs = %d cycles" wsm.ScrollbarBkgRepCycs
    // printfn "DEBUG:generateScrollbarInfo: bkgWidth = %.1f cycles" bkgWidth
    // printfn "DEBUG:generateScrollbarInfo: Output -"
    // printfn "DEBUG:generateScrollbarInfo: tbWidth = %.1fpx" tbWidth
    // printfn "DEBUG:generateScrollbarInfo: tbPos = %.1fpx" tbPos
    // printfn "DEBUG:generateScrollbarInfo: newBkgRep = %d cycles" newBkgRep

    {| tbWidth = tbWidth; tbPos = tbPos; bkgRep = newBkgRep |}

/// Make scrollbar parameters consistent with changed zoom. This assumes the scrollbar
/// width has not changed, because that can only be calculated from the viewer width in model.
let validateScrollBarInfo (wsm: WaveSimModel) =
    let scrollInfo = generateScrollbarInfo wsm
    {wsm with ScrollbarTbPos = scrollInfo.tbPos
              ScrollbarTbWidth = scrollInfo.tbWidth
              ScrollbarBkgRepCycs = scrollInfo.bkgRep
    }
     

let inline updateViewerWidthInWaveSim w (model:Model) =
    let wsModel = getWSModel model
    let namesColWidth = calcNamesColWidth wsModel

    /// The extra is probably because of some unnacounted for padding etc (there is a weird 2px spacer to right of the divider)
    /// It also allows space for a scroll bar (about 6 px)
    let otherDivWidths = Constants.leftMargin + Constants.rightMargin + DiagramStyle.Constants.dividerBarWidth + Constants.scrollBarWidth + 8

    /// This is what the overall waveform width must be
    let valuesColumnWidth,_ = valuesColumnSize wsModel
    let waveColWidth = w - otherDivWidths - namesColWidth - valuesColumnWidth

    /// Require at least one visible clock cycle: otherwise choose number to get close to correct width of 1 cycle
    let wholeCycles =
        max 1 (int (float waveColWidth / singleWaveWidth wsModel))
        |> min (wsModel.WSConfig.LastClock / wsModel.SamplingZoom) // make sure there can be no over-run when making viewer larger
        // prevent oscilaltion when number of cycles changes continuously due to width changes in values (rare corner case)
        |> (function | whole when abs (float (whole - wsModel.ShownCycles) / float wsModel.ShownCycles) < 0.1 -> wsModel.ShownCycles
                     | whole -> whole)

    let singleCycleWidth = float waveColWidth / float wholeCycles
    let finalWavesColWidth = singleCycleWidth * float wholeCycles

    /// Estimated length of scrollbar, adding three components together: names col, waveform port, and values col.
    let scrollbarWidth = (float namesColWidth) + finalWavesColWidth + (float valuesColumnWidth)

    // printfn "DEBUG:updateViewerWidthInWaveSim: Names Column Width = %Apx" (float namesColWidth)
    // printfn "DEBUG:updateViewerWidthInWaveSim: Waves Column Width = %Apx" finalWavesColWidth
    // printfn "DEBUG:updateViewerWidthInWaveSim: Values Column Width = %Apx" (float valuesColumnWidth)
    // printfn "DEBUG:updateViewerWidthInWaveSim: Calculated Scrollbar Width = %Apx" scrollbarWidth


    let updateFn wsModel = 
        {
        wsModel with
            ShownCycles = wholeCycles
            StartCycle = min wsModel.StartCycle (wsModel.WSConfig.LastClock - (wholeCycles - 1)*wsModel.SamplingZoom)
            CursorDisplayCycle = min wsModel.CursorDisplayCycle wsModel.WSConfig.LastClock
            WaveformColumnWidth = finalWavesColWidth
            ScrollbarBkgWidth = scrollbarWidth
        }
        // if width has not chnaged don't recalculate scrollbar info since that results in circular loops with
        // scrollbar -> StartCycle -> scrollbar
        |> if w <> model.WaveSimViewerWidth then validateScrollBarInfo else id

    {model with WaveSimViewerWidth = w}
    |> ModelHelpers.updateWSModel updateFn



let inline setViewerWidthInWaveSim w dispatch =
    dispatch <| UpdateModel (updateViewerWidthInWaveSim w)
    dispatch <| GenerateCurrentWaveforms

/// Must be called after any of the wavesim parameters are changed and before
/// they are used to generate waveforms.
let rec validateSimParas (ws: WaveSimModel) =
    if ws.StartCycle < 0 then
        validateSimParas {ws with StartCycle = 0}
    elif ws.CursorExactClkCycle > ws.WSConfig.LastClock then
        validateSimParas {ws with CursorExactClkCycle = ws.WSConfig.LastClock; CursorDisplayCycle = ws.WSConfig.LastClock/ws.SamplingZoom}
    elif (ws.StartCycle +  ws.ShownCycles-1)*ws.SamplingZoom > ws.WSConfig.LastClock then
        if ws.StartCycle = 0 then
            {ws with ShownCycles = ws.WSConfig.LastClock / ws.SamplingZoom + 1}
        else
            validateSimParas {ws with StartCycle = max 0 (ws.WSConfig.LastClock / ws.SamplingZoom - ws.ShownCycles + 1)}
    elif ws.CursorDisplayCycle < ws.StartCycle  then
        {ws with CursorDisplayCycle = ws.StartCycle; CursorExactClkCycle = ws.StartCycle*ws.SamplingZoom}
    elif  ws.CursorDisplayCycle > ws.StartCycle + ws.ShownCycles - 1  then
        let newCurrCycle =  ws.StartCycle + ws.ShownCycles - 1
        {ws with CursorDisplayCycle = newCurrCycle; CursorExactClkCycle = newCurrCycle * ws.SamplingZoom}
    else ws
    //|> (fun ws -> printfn $"currClk={ws.CurrClkCycle} detail = {ws.CurrClkCycleDetail}"; ws)
    |> validateScrollBarInfo

let changeMultiplier newMultiplier (ws: WaveSimModel) =
    let oldM = ws.SamplingZoom
    let sampsHalf = (float ws.ShownCycles - 1.) / 2.
    let newShown = 1 + min ws.ShownCycles (ws.WSConfig.LastClock / newMultiplier)
    let newStart = int ((float ws.StartCycle + sampsHalf) * float oldM / float newMultiplier - (float newShown - 1.) / 2.)
    {ws with ShownCycles = newShown; StartCycle = newStart; SamplingZoom = newMultiplier}
    |> validateSimParas




/// Return a Msg that will set highlighted clock cycle number
let setClkCycleMsg (wsModel: WaveSimModel) (newRealClkCycle: int) : Msg =
    let start = TimeHelpers.getTimeMs ()
    let newDetail  = min (max newRealClkCycle 0) wsModel.WSConfig.LastClock
    let mult = wsModel.SamplingZoom
    let newClkCycle = newRealClkCycle / mult
    let newClkCycle = max 0 newClkCycle
    let startCycle =
        min newClkCycle wsModel.StartCycle
        |> (fun sc -> max sc (newClkCycle - wsModel.ShownCycles + 1))

    if startCycle <> wsModel.StartCycle then
        GenerateWaveforms
            {wsModel with 
                StartCycle = startCycle
                CursorDisplayCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
                CursorExactClkCycle = newDetail
            }
    else
        SetWSModel
            {wsModel with
                CursorDisplayCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
                CursorExactClkCycle = newDetail
            }
 


let setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newRealClkCycle: int) : unit =
    dispatch <| setClkCycleMsg wsModel  newRealClkCycle


/// <summary>Move waveform view window by closest integer number of cycles.
/// Current clock cycle (<c>WaveSimModel.CurrClkCycle</c>) is set to beginning or end depending on direction of movement. 
/// Update is achieved by dispatching a <c>GenerateWaveforms</c> message.
/// Note the side-effect of clearing the <c>ScrollbarQueueIsEmpty</c> counter.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with.</param>
/// <param name="moveByCycs">Number of non-integer cycles to move by.</param>
let setScrollbarTbByCycs (wsm: WaveSimModel) (dispatch: Msg->unit) (moveByCycs: float): unit =
    let moveWindowBy = int (System.Math.Round moveByCycs)
    let mult = wsm.SamplingZoom

    /// <summary>Return target value when within min and max value, otherwise min or max.</summary>
    let bound (minV: int) (maxV: int) (tarV: int): int = tarV |> max minV |> min maxV
    let minSimCyc = 0
    let maxSimCyc = wsm.WSConfig.LastClock / mult

    let newStartCyc = (wsm.StartCycle+moveWindowBy) |> bound minSimCyc (maxSimCyc-wsm.ShownCycles+1)
    let newCurrCyc =
        let newEndCyc = newStartCyc+wsm.ShownCycles-1
        if newStartCyc <= wsm.CursorDisplayCycle && wsm.CursorDisplayCycle <= newEndCyc
        then
            wsm.CursorDisplayCycle
        else
            if abs (wsm.CursorDisplayCycle - newStartCyc) < abs (wsm.CursorDisplayCycle - newEndCyc)
            then newStartCyc
            else newEndCyc
    let detail = wsm.CursorExactClkCycle
    let detail = if newCurrCyc <> detail / mult then  newCurrCyc * mult else detail
    {
        wsm with StartCycle = newStartCyc;
                 CursorDisplayCycle = newCurrCyc;
                 CursorExactClkCycle = detail;
                 ScrollbarQueueIsEmpty = true
    }
    |> validateSimParas
    |> (fun ws -> dispatch <| GenerateWaveforms ws)

/// <summary>Update <c>WaveSimModel</c> with new <c>ScrollbarTbOffset</c>.
/// Used when starting or clearing scrollbar drag mode.
/// Update is achieved by dispatching a <c>GenerateWaveforms</c> message.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with.</param>
/// <param name="offset">Offset option to be written to <c>WaveSimModel.ScrollbarTbOffset</c>.</param>
let setScrollbarOffset (wsm: WaveSimModel) (dispatch: Msg->unit) (offset: float option): unit =
    GenerateWaveforms { wsm with ScrollbarTbOffset = offset; ScrollbarQueueIsEmpty = true } |> dispatch

/// <summary>Update <c>WaveSimModel</c> with new <c>ScrollbarQueueIsEmpty</c>.
/// Used to update is-empty counter to coalesce scrollbar mouse events together.
/// Update is achieved by dispatching a <c>UpdateWSModel</c> message, so as to not clog the queue with <c>GenerateWaveforms</c> messages.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with.</param>
/// <param name="isEmpty">Bool to be written to <c>WaveSimModel.ScrollbarQueueIsEmpty</c>.</param>
let setScrollbarLastX (wsm: WaveSimModel) (dispatch: Msg->unit) (isEmpty: bool): unit =
    UpdateWSModel (fun _ -> { wsm with ScrollbarQueueIsEmpty = isEmpty }) |> dispatch

/// If zoomIn, then increase width of clock cycles (i.e.reduce number of visible cycles).
/// otherwise reduce width. GenerateWaveforms message will reconstitute SVGs after the change.
let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) =
    let start = TimeHelpers.getTimeMs ()
    let shownCycles =
        let wantedCycles = int (float wsModel.ShownCycles / Constants.zoomChangeFactor)
        if zoomIn then
            // try to reduce number of cycles displayed
            wantedCycles
            // If number of cycles after casting to int does not change
            |> (fun nc -> if nc = wsModel.ShownCycles then nc - 1 else nc )
            // Require a minimum of cycles
            |> (fun nc -> 
                    let minVis = min wsModel.ShownCycles Constants.minVisibleCycles
                    max nc minVis)
        else
            let wantedCycles = int (float wsModel.ShownCycles * Constants.zoomChangeFactor)
            // try to increase number of cycles displayed
            wantedCycles
            // If number of cycles after casting to int does not change
            |> (fun nc -> if nc = wsModel.ShownCycles then nc + 1 else nc )
            |> (fun nc -> 
                let maxNc = int (wsModel.WaveformColumnWidth / float Constants.minCycleWidth)
                max wsModel.ShownCycles (min nc maxNc))
    let startCycle =
        // preferred start cycle to keep centre of screen ok
        let sc = (wsModel.StartCycle - (shownCycles - wsModel.ShownCycles)/2)
        let cOffset = wsModel.CursorDisplayCycle - sc
        sc
        // try to keep cursor on screen
        |> (fun sc -> 
            if cOffset > shownCycles - 1 then
                sc + cOffset - shownCycles + 1
            elif cOffset < 0 then
                (sc + cOffset)
            else
                sc)
        // final limits check so no cycle is outside allowed range
        |> min (wsModel.WSConfig.LastClock / wsModel.SamplingZoom - shownCycles + 1)
        |> max 0

        
    dispatch <| GenerateWaveforms {
        wsModel with
            ShownCycles = shownCycles;
            StartCycle = startCycle
        }
    |> TimeHelpers.instrumentInterval "changeZoom" start

/// <summary>Make scrollbar element based on information in <c>WaveSimModel</c>.
/// Called in <c>viewWaveSim</c>, presumably after <c>refreshWaveSim</c> was called.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with. Not used directly, but passed to <c>tbMouseMoveOp</c>.</param>
/// <returns>React element to be placed in to DOM.</returns>
let makeScrollbar (wsm: WaveSimModel) (dispatch: Msg->unit): ReactElement =
    // button props
    let scrollWaveformViewBy (numCycles: float) = setScrollbarTbByCycs wsm dispatch numCycles

    // svg props
    let bkgWidth = wsm.ScrollbarBkgWidth - 60. // 60 = 2x width of buttons

    let tbMouseDownHandler (event: Browser.Types.MouseEvent): unit = // start drag
        ScrollbarMouseMsg (event.clientX, StartScrollbarDrag, dispatch) |> dispatch

    let tbMouseMoveHandler (event: Browser.Types.MouseEvent): unit = // if in drag, drag; otherwise do nothing
        let leftButtonIsdown = (int event.buttons &&& 0x1) <> 0
        let inDrag = Option.isSome wsm.ScrollbarTbOffset
        if inDrag && not leftButtonIsdown then
            // cancel the scroll operation
            ScrollbarMouseMsg (event.clientX, ClearScrollbarDrag, dispatch) |> dispatch
        elif inDrag then
            ScrollbarMouseMsg (event.clientX, InScrollbarDrag, dispatch) |> dispatch
        else ()

    let tbMouseUpHandler (event: Browser.Types.MouseEvent): unit = // if in drag, clear drag; otherwise do nothing
        if Option.isSome wsm.ScrollbarTbOffset
        then ScrollbarMouseMsg (event.clientX, ClearScrollbarDrag, dispatch) |> dispatch
        else ()

    let bkgPropList (width: float): List<IProp> =
        [
            HTMLAttr.Id "scrollbarThumb";
            SVGAttr.X $"0px"; SVGAttr.Y "0.5px";
            SVGAttr.Width $"%.1f{width}px"; SVGAttr.Height $"%.1f{WaveSimStyle.Constants.softScrollBarWidth-1.0}px";
            SVGAttr.Fill "white"; SVGAttr.Stroke "gray"; SVGAttr.StrokeWidth "1px";
        ]

    let tbPropList (pos: float) (width: float): List<IProp> =
        [
            HTMLAttr.Id "scrollbarBkg";
            Style [ Cursor "grab"];
            SVGAttr.X $"%.1f{pos}px"; SVGAttr.Y "0.5px";
            SVGAttr.Width $"%.1f{width}px"; SVGAttr.Height $"%.1f{WaveSimStyle.Constants.softScrollBarWidth-1.0}px";
            SVGAttr.Fill "lightgrey"; SVGAttr.Stroke "gray"; SVGAttr.StrokeWidth "1px";
            OnMouseDown tbMouseDownHandler; OnMouseUp tbMouseUpHandler; OnMouseMove tbMouseMoveHandler;
        ]

    div [ Style [ WhiteSpace WhiteSpaceOptions.Nowrap; MarginTop "5px"; MarginBottom "5px"; Height "25px"; CSSProp.Custom("Overflow", "visible visible")]] [
        button [ Button.Props [scrollbarClkCycleLeftStyle] ]
            (fun _ -> scrollWaveformViewBy -1.0)
            (str "◀")
        svg
            [Style [Width $"{bkgWidth}"; Height $"{WaveSimStyle.Constants.softScrollBarWidth}px"];]
            [
                rect (bkgPropList bkgWidth) []; // background
                rect (tbPropList wsm.ScrollbarTbPos wsm.ScrollbarTbWidth) []; // thumb
            ]
        button [ Button.Props [scrollbarClkCycleRightStyle]]
            (fun _ -> scrollWaveformViewBy 1.0)
            (str "▶")
    ]

/// <summary>Update waveform view information based on mouse postion in the X direction.
/// Called in <c>update</c> when <c>ScrollbarMouseMsg</c> is dispatched.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with, not used directly.</param>
/// <param name="cursor">Cursor postion in relation to the screen, i.e. <c>event.clientX</c>.</param>
/// <param name="action">Scrollbar action to do, see choices for more info, in type of <c>ScrollbarMouseAction</c>.</param>
/// <remarks>Note that <c>screenX</c> does NOT scale with web zoom and will cause weird results!</remarks>
let updateScrollbar (wsm: WaveSimModel) (dispatch: Msg->unit) (cursor: float) (action: ScrollbarMouseAction): unit =
    /// <summary>Translate mouse movements in pixels to number of cycles to move by.
    /// Linear translator aims to allow scrollbar thumb to follow cursor.</summary>
    /// <param name="dx">Number of pixels mouse has moved in X direction, obtained from MouseMove event.</param>
    /// <returns>Number of cycles to move by.</returns>
    /// <remarks>Swap this out with some other mouse-to-cycle translator for better user experience.</remarks>
    let linearMouseToCycleTranslator (dx: float): float = 
        let cycleToPixelRatio = float wsm.ScrollbarBkgRepCycs / (wsm.ScrollbarBkgWidth - wsm.ScrollbarTbWidth)
        dx*cycleToPixelRatio
    match action with
    | StartScrollbarDrag -> // record offset
        let offset = Some (wsm.ScrollbarTbPos-cursor)
        setScrollbarOffset wsm dispatch offset
    | InScrollbarDrag -> // in drag, unknown queue state: update counter, if queue is empty then dispatch ReleaseScrollQueue message
        let canDispatch = wsm.ScrollbarQueueIsEmpty
        setScrollbarLastX wsm dispatch false
        if canDispatch then ScrollbarMouseMsg (cursor, ReleaseScrollQueue, dispatch) |> dispatch
    | ReleaseScrollQueue -> // in drag, and queue is clear: update and set ScrollbarQueueIsEmpty to true
        match wsm.ScrollbarTbOffset with
        | Some puckOffset ->
            let dx = puckOffset + cursor - wsm.ScrollbarTbPos // offset + new cursor = new thumb; dx = new thumb - old thumb
            setScrollbarTbByCycs wsm dispatch (linearMouseToCycleTranslator dx)
        | None -> ()
    | ClearScrollbarDrag -> // clear offset
        setScrollbarOffset wsm dispatch None

        /// Click on these buttons to change the number of visible clock cycles.
let zoomButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ clkCycleButtonStyle ]
        [
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> changeZoom wsModel false dispatch)
                zoomOutSVG
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> changeZoom wsModel true dispatch)
                zoomInSVG
        ]

/// Click on these to change the highlighted clock cycle.
let clkCycleButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    /// Controls the number of cycles moved by the "◀◀" and "▶▶" buttons
    let mult = wsModel.SamplingZoom
    let bigStepSize = max (2*mult) (wsModel.ShownCycles*mult / 2)

    let scrollWaveformsBy (numCycles: int) =
        setClkCycle wsModel dispatch (wsModel.CursorExactClkCycle + numCycles)

    div [ clkCycleButtonStyle ]
        [
            // Move left by bigStepSize cycles
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> scrollWaveformsBy -bigStepSize)
                (str "◀◀")

            // Move left by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy -1)
                (str "◀")

            // Text input box for manual selection of clock cycle
            Input.number [
                Input.Props clkCycleInputProps
                Input.Id "clkCycleInput"
                Input.Value (
                    match wsModel.ClkCycleBoxIsEmpty with
                    | true -> ""
                    | false -> string (wsModel.CursorExactClkCycle)
                )
                // TODO: Test more properly with invalid inputs (including negative numbers)
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n ->
                        setClkCycle wsModel dispatch n
                    | false, _ when c.Value = "" ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = true}
                    | _ ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = false}
                )
            ]

            // Move right by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy 1)
                (str "▶")

            // Move right by bigStepSize cycles
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> scrollWaveformsBy bigStepSize)
                (str "▶▶")
        ]

//-------------------------------------Popup menu for multiplier---------------------------------------------//

let multiplierMenuButton(wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    /// key = 0 .. n-1 where there are n possible multipliers
        let mulTable = Constants.multipliers
        let menuItem (key) =
            let itemLegend = str (match key with | 0 -> "Every Cycle (normal X1)" | _ -> $"Every {mulTable[key]} cycles")
            Menu.Item.li
                [ Menu.Item.IsActive (Constants.multipliers[key] = wsModel.SamplingZoom)
                  Menu.Item.OnClick (fun _ ->
                    dispatch <| ChangeWaveSimMultiplier (key);
                    dispatch ClosePopup)
                ]
                [ itemLegend ]
        let addSpaces n r = span [ Style [ PaddingLeft n; PaddingRight n]] [r]
        let menu =
            div []
                [
                    p [ Style [ Color "darkred"; FontWeight 600; FontSize "18px"]] [str "Warning: zoom greater than X1 will sample the waveform and lose information about fast-changing outputs"]
                    br []
                    p [] [str "Use it this menu to zoom out slow-changing signals when the range of"; addSpaces 5 zoomOutSVG; str "is not enough."]
                    br []
                    Menu.menu [] [ Menu.list [] (List.map menuItem [0 .. mulTable.Length - 1 ])]
                ]

        let buttonClick = Button.OnClick (fun _ ->
            dispatch <| ShowStaticInfoPopup("Zoom Sampling Rate",menu,dispatch ))
        let expectedMaxShown = int (wsModel.WaveformColumnWidth / float Constants.minCycleWidth)
        if wsModel.ShownCycles = expectedMaxShown || wsModel.SamplingZoom > 1 then
            Button.button ( buttonClick :: topHalfButtonProps IColor.IsDanger "ZoomButton" false) [str $"X{wsModel.SamplingZoom}"]
        else str ""
