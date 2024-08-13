module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open TopMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open WaveSimSelect
open DiagramStyle


module Constants =
    /// <summary>Config variable to choose whether to generate the full 1000 cycles of SVG.</summary>
    let generateVisibleOnly = true
    /// <summary>Config variable to choose whether to print performance analysis info to console.</summary>
    let showPerfLogs = false
    let inlineNoWrap = WhiteSpace WhiteSpaceOptions.Nowrap

open Constants


/// <summary>Generates SVG to display non-binary values on waveforms.</summary>
/// <remarks>Should be refactored together with <c>displayBigIntOnWave</c>.</remarks>
let displayUInt32OnWave 
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<uint32>)
    (transitions: array<NonBinaryTransition>)
    : list<ReactElement> =
    // find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> Array.indexed
        |> Array.filter (fun (_, x) -> x = Change)
        |> Array.map (fun (i, _) -> i)

    // find start and length of each gap between a Change transition
    let gaps: array<Gap> =
        if Constants.generateVisibleOnly
        then
            // add dummy change at visible end, but need account for difference in changes:
            // e.g. if we are showing 3 cycles, a wave with a change in each would be 0, 1, 2, 3 and would be fine when
            // 4 is added; however, a wave with no change at all would be 0, and would produce an errorneous gap length
            // of 4 when 4 is added - we therefore add 3
            if changeTransitions[Array.length changeTransitions-1] <> wsModel.ShownCycles
            then Array.append changeTransitions [|wsModel.ShownCycles|]
            else Array.append changeTransitions [|wsModel.ShownCycles+1|]
            |> Array.map (fun loc -> loc+wsModel.StartCycle) // shift cycle to start cycle
        else
            Array.append changeTransitions [|wsModel.StartCycle+transitions.Length-1|] // add dummry change length end
        |> Array.pairwise
        |> Array.map (fun (i1, i2) -> {Start = i1; Length = i2-i1}) // get start and length of gap
    
    // utility functions for SVG generation
    /// <summary>Function to make polygon fill for a gap.</summary>
    /// <param name="points">Array of polyline points to fill.</param>
    let makePolyfill (points: array<XYPos>) = 
        let points = points |> Array.distinct
        polyline (wavePolyfillStyle points) []

    /// <summary>Function to make text element for a gap.</summary>
    /// <param name="start">Starting X location of element.</param>
    let makeTextElement (start: float) (waveValue: string) = 
        text (singleValueOnWaveProps start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        // generate string
        let waveValue = UInt32ToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gap.Start]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints cycleWidth gap
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (singleWidth <= w && w < doubleWidth) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement (float gap.Start * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w when (doubleWidth <= w) -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                if cycleWidth < doubleWidth
                then (cycleWidth - singleWidth) / 2.
                else Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                if singleCycleCenterPadWidth < 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    then 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    else singleCycleCenterPadWidth
            let endPadWidth = (float gap.Length * cycleWidth - startPadWidth - singleWidth)
            let startText = makeTextElement (float gap.Start * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement (float gap.Start * cycleWidth + endPadWidth) waveValue
            [ startText; endText ] 
        | _ -> // catch-all
            failwithf "displayUInt32OnWave: impossible case"
    )
    |> List.concat

/// <summary>Generates SVG to display <c>bigint</c> values on waveforms.</summary>
/// <remarks>Should be refactored together with <c>displayUInt32OnWave</c>.</remarks>
let displayBigIntOnWave
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<bigint>)
    (transitions: array<NonBinaryTransition>)
    : list<ReactElement> =
    // find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> Array.indexed
        |> Array.filter (fun (_, x) -> x = Change)
        |> Array.map (fun (i, _) -> i)

    // find start and length of each gap between a Change transition
    let gaps: array<Gap> =
        if Constants.generateVisibleOnly
        then
            // add dummy change at visible end, but need account for difference in changes:
            // e.g. if we are showing 3 cycles, a wave with a change in each would be 0, 1, 2, 3 and would be fine when
            // 4 is added; however, a wave with no change at all would be 0, and would produce an errorneous gap length
            // of 4 when 4 is added - we therefore add 3
            if changeTransitions[Array.length changeTransitions-1] <> wsModel.ShownCycles
            then Array.append changeTransitions [|wsModel.ShownCycles|]
            else Array.append changeTransitions [|wsModel.ShownCycles+1|]
            |> Array.map (fun loc -> loc+wsModel.StartCycle) // shift cycle to start cycle
        else
            Array.append changeTransitions [|wsModel.StartCycle+transitions.Length-1|] // add dummry change length end
        |> Array.pairwise
        |> Array.map (fun (i1, i2) -> {Start = i1; Length = i2-i1}) // get start and length of gap
    
    // utility functions for SVG generation
    /// <summary>Function to make polygon fill for a gap.</summary>
    /// <param name="points">Array of polyline points to fill.</param>
    let makePolyfill (points: array<XYPos>) = 
        let points = points |> Array.distinct
        polyline (wavePolyfillStyle points) []

    /// <summary>Function to make text element for a gap.</summary>
    /// <param name="start">Starting X location of element.</param>
    let makeTextElement (start: float) (waveValue: string) = 
        text (singleValueOnWaveProps start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        // generate string
        let waveValue = BigIntToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gap.Start]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints cycleWidth gap
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (singleWidth <= w && w < doubleWidth) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement (float gap.Start * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w when (doubleWidth <= w) -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                if cycleWidth < doubleWidth
                then (cycleWidth - singleWidth) / 2.
                else Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                if singleCycleCenterPadWidth < 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    then 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    else singleCycleCenterPadWidth
            let endPadWidth = (float gap.Length * cycleWidth - startPadWidth - singleWidth)
            let startText = makeTextElement (float gap.Start * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement (float gap.Start * cycleWidth + endPadWidth) waveValue
            [ startText; endText ] 
        | _ -> // catch-all
            failwithf "displayUInt32OnWave: impossible case"
    )
    |> List.concat

/// <summary>Check if generated SVG is correct, based on existence, position, 
/// and zoom. Fast simulation data is assumed unchanged. Used to determine if 
/// <c>generateWaveform</c> is run.</summary>
let waveformIsUptodate (ws: WaveSimModel) (wave: Wave): bool =
    wave.SVG <> None &&
    wave.ShownCycles = ws.ShownCycles &&
    wave.StartCycle = ws.StartCycle &&
    wave.CycleWidth = singleWaveWidth ws &&
    wave.Radix = ws.Radix

/// <summary>Called when <c>InitiateWaveSimulation</c> message is dispatched and when wave
/// simulator is refreshed. Generates or updates the SVG for a specific waveform
/// whether needed or not. The SVG depends on cycle width as well as start/stop
/// clocks and design. Assumes that the fast simulation data has not changed and
/// has enough cycles.</summary>
let generateWaveform (ws: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    let makePolyline points = 
        let points = points |> Array.concat |> Array.distinct
        polyline (wavePolylineStyle points) []
    
    let waveform =
        match wave.Width with
        | 0 -> 
            failwithf "Cannot have wave of width 0"

        | 1 -> // binary waveform
            let transitions =
                if Constants.generateVisibleOnly
                then calculateBinaryTransitionsUInt32 wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles
                else calculateBinaryTransitionsUInt32 wave.WaveValues.UInt32Step ws.StartCycle 0
            
            let wavePoints =
                let waveWidth = singleWaveWidth ws
                let startCycle = if Constants.generateVisibleOnly then ws.StartCycle else 0
                Array.mapi (binaryWavePoints waveWidth startCycle) transitions
                |> Array.concat
                |> Array.distinct

            svg (waveRowProps ws) [ polyline (wavePolylineStyle wavePoints) [] ]

        | w when w <= 32 -> // non-binary waveform
            let transitions =
                if Constants.generateVisibleOnly
                then calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles
                else calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle 0

            let fstPoints, sndPoints =
                let waveWidth = singleWaveWidth ws
                let startCycle = if Constants.generateVisibleOnly then ws.StartCycle else 0
                Array.mapi (nonBinaryWavePoints waveWidth startCycle) transitions |> Array.unzip
            
            let valuesSVG = displayUInt32OnWave ws wave.Width wave.WaveValues.UInt32Step transitions
            let polyLines = [makePolyline fstPoints; makePolyline sndPoints]

            svg (waveRowProps ws) (List.append polyLines valuesSVG)

        | _ -> // non-binary waveform with width greather than 32
            let transitions =
                if Constants.generateVisibleOnly
                then calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles
                else calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle 0

            let fstPoints, sndPoints =
                Array.mapi (nonBinaryWavePoints (singleWaveWidth ws) 0) transitions |> Array.unzip

            let valuesSVG = displayBigIntOnWave ws wave.Width wave.WaveValues.BigIntStep transitions

            svg (waveRowProps ws) (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)

    {wave with 
        Radix = ws.Radix
        ShownCycles = ws.ShownCycles
        StartCycle = ws.StartCycle
        CycleWidth = singleWaveWidth ws
        SVG = Some waveform}



/// Set highlighted clock cycle number
let private setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newClkCycle: int) : unit =
    let start = TimeHelpers.getTimeMs ()
    let newClkCycle = min Constants.maxLastClk newClkCycle |> max 0

    if newClkCycle <= endCycle wsModel then
        if newClkCycle < wsModel.StartCycle then
            dispatch <| GenerateWaveforms
                {wsModel with 
                    StartCycle = newClkCycle
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
        else
            dispatch <| SetWSModel
                {wsModel with
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
    else
        dispatch <| GenerateWaveforms
            {wsModel with
                StartCycle = newClkCycle - (wsModel.ShownCycles - 1)
                CurrClkCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
            }
    |> TimeHelpers.instrumentInterval "setClkCycle" start

/// <summary>Move waveform view window by closest integer number of cycles.
/// Current clock cycle (<c>WaveSimModel.CurrClkCycle</c>) is set to beginning or end depending on direction of movement. 
/// Update is achieved by dispatching a <c>GenerateWaveforms</c> message.
/// Note the side-effect of clearing the <c>ScrollbarQueueIsEmpty</c> counter.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <param name="dispatch">Dispatch function to send messages with.</param>
/// <param name="moveByCycs">Number of non-integer cycles to move by.</param>
let setScrollbarTbByCycs (wsm: WaveSimModel) (dispatch: Msg->unit) (moveByCycs: float): unit =
    let moveWindowBy = int (System.Math.Round moveByCycs)

    /// <summary>Return target value when within min and max value, otherwise min or max.</summary>
    let bound (minV: int) (maxV: int) (tarV: int): int = tarV |> max minV |> min maxV
    let minSimCyc = 0
    let maxSimCyc = Constants.maxLastClk

    let newStartCyc = (wsm.StartCycle+moveWindowBy) |> bound minSimCyc (maxSimCyc-wsm.ShownCycles+1)
    let newCurrCyc =
        let newEndCyc = newStartCyc+wsm.ShownCycles-1
        if newStartCyc <= wsm.CurrClkCycle && wsm.CurrClkCycle <= newEndCyc
        then
            wsm.CurrClkCycle
        else
            if abs (wsm.CurrClkCycle - newStartCyc) < abs (wsm.CurrClkCycle - newEndCyc)
            then newStartCyc
            else newEndCyc

    GenerateWaveforms {wsm with StartCycle = newStartCyc; CurrClkCycle = newCurrCyc; ScrollbarQueueIsEmpty = true } |> dispatch

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
        let cOffset = wsModel.CurrClkCycle - sc
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
        |> max 0
        |> min (Constants.maxLastClk - shownCycles)
        
    dispatch <| GenerateWaveforms { wsModel with ShownCycles = shownCycles; StartCycle = startCycle }
    |> TimeHelpers.instrumentInterval "changeZoom" start

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
    let bigStepSize = max 2 (wsModel.ShownCycles / 2)

    let scrollWaveformsBy (numCycles: int) =
        setClkCycle wsModel dispatch (wsModel.CurrClkCycle + numCycles)

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

                Input.Value (
                    match wsModel.ClkCycleBoxIsEmpty with
                    | true -> ""
                    | false -> string wsModel.CurrClkCycle
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

/// ReactElement of the tabs for changing displayed radix
let private radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let radixString = [
        Bin,  "Bin"
        Hex,  "Hex"
        Dec,  "uDec"
        SDec, "sDec"
    ]

    let radixTab (radix, radixStr) =
        Tabs.tab [
            Tabs.Tab.IsActive(wsModel.Radix = radix)
            Tabs.Tab.Props radixTabProps
        ] [ a [
            radixTabAStyle
            OnClick(fun _ -> dispatch <| GenerateWaveforms {wsModel with Radix = radix})
            ] [ str radixStr ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] (List.map (radixTab) radixString)


let highlightCircuit fs comps wave (dispatch: Msg -> Unit) =
    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols comps)))
    // Filter out any non-existent wires
    let conns = connsOfWave fs wave 
    dispatch <| Sheet (SheetT.Msg.SelectWires conns)    

/// Create label of waveform name for each selected wave.
/// Note that this is generated after calling selectedWaves. Any changes to this function
/// must also be made to valueRows and waveRows, as the order of the waves matters here.
/// This is because the wave viewer is comprised of three columns of many rows, rather
/// than many rows of three columns.
let nameRows (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement list =
    selectedWaves wsModel
    |> List.map (fun wave ->
        let visibility =
            if wsModel.HoveredLabel = Some wave.WaveId then
                "visible"
            else "hidden"

        Level.level [
            Level.Level.Option.Props [
                nameRowLevelStyle (wsModel.HoveredLabel = Some wave.WaveId)
                let execWithModel (f: Model -> Unit) = ExecFuncInMessage((fun model _ -> f model), dispatch)
                OnMouseOver (fun _ -> dispatch <| execWithModel (fun model ->
                    if wsModel.DraggedIndex = None then
                        dispatch <| SetWSModel {wsModel with HoveredLabel = Some wave.WaveId}
                        // Check if symbol exists on Canvas
                        let symbols = model.Sheet.Wire.Symbol.Symbols
                        match Map.tryFind (fst wave.WaveId.Id) symbols with
                        | Some {Component={Type=IOLabel;Label=lab}} ->
                            let labelComps =
                                symbols
                                |> Map.toList
                                |> List.map (fun (_,sym) -> sym.Component)
                                |> List.filter (function | {Type=IOLabel;Label = lab'} when lab' = lab -> true |_ -> false)
                                |> List.map (fun comp -> ComponentId comp.Id)
                            highlightCircuit wsModel.FastSim labelComps wave dispatch                            
                        | Some sym ->
                            highlightCircuit wsModel.FastSim [fst wave.WaveId.Id] wave dispatch
                        | None -> ())
                        
                )
                OnMouseOut (fun _ ->
                    dispatch <| SetWSModel {wsModel with HoveredLabel = None}
                    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [])))
                    dispatch <| Sheet (SheetT.Msg.UpdateSelectedWires (connsOfWave wsModel.FastSim wave, false))
                )

                Draggable true

                OnDragStart (fun ev ->
                    ev.dataTransfer.effectAllowed <- "move"
                    ev.dataTransfer.dropEffect <- "move"
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = Some wave.WaveId
                            PrevSelectedWaves = Some wsModel.SelectedWaves
                        }
                )

                OnDrag (fun ev -> 
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()

                    // If the user drags the label outside the bounds of the wave name column
                    if ev.clientX < bcr.left || ev.clientX > bcr.right ||
                        ev.clientY < bcr.top || ev.clientY > bcr.bottom
                    then
                        dispatch <| SetWSModel {
                            wsModel with
                                HoveredLabel = Some wave.WaveId
                                // Use wsModel.SelectedValues if somehow PrevSelectedWaves not set
                                SelectedWaves = Option.defaultValue wsModel.SelectedWaves wsModel.PrevSelectedWaves
                            }
                )

                OnDragOver (fun ev -> ev.preventDefault ())

                OnDragEnter (fun ev ->
                    ev.preventDefault ()
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()
                    let index = int (ev.clientY - bcr.top) / Constants.rowHeight - 1
                    let draggedWave =
                        match wsModel.DraggedIndex with
                        | Some waveId -> [waveId]
                        | None -> []

                    let selectedWaves =
                        wsModel.SelectedWaves
                        |> List.except draggedWave
                        |> List.insertManyAt index draggedWave

                    dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                )

                OnDragEnd (fun _ ->
                    dispatch <| SetWSModel {
                        wsModel with
                            DraggedIndex = None
                            PrevSelectedWaves = None
                        }
                )
            ]
        ] [ Level.left
                [ Props (nameRowLevelLeftProps visibility) ]
                [ Delete.delete [
                    Delete.Option.Size IsSmall
                    Delete.Option.Props [
                        OnClick (fun _ ->
                            let selectedWaves = List.except [wave.WaveId] wsModel.SelectedWaves
                            dispatch <| SetWSModel {wsModel with SelectedWaves = selectedWaves}
                        )
                    ]
                  ] []
                ]
            Level.right
                [ Props [ Style [ PaddingRight Constants.labelPadding ] ] ]
                [ label [ nameLabelStyle (wsModel.HoveredLabel = Some wave.WaveId) ] [ wave.ViewerDisplayName|> str ] ]
        ]
    )

/// Create column of waveform names
let namesColumn model wsModel dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let rows = 
        nameRows model wsModel dispatch
    div (namesColumnProps wsModel)
        (List.concat [ topRow []; rows ])
    |> TimeHelpers.instrumentInterval "namesColumn" start


/// Create label of waveform value for each selected wave at a given clk cycle.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to nameRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
/// Return required width of values column in pixels, and list of cloumn react elements.
let valueRows (wsModel: WaveSimModel) =
    let valueColWidth, valueColNumChars =
        valuesColumnSize wsModel
    selectedWaves wsModel
    |> List.map (fun wave -> getWaveValue wsModel.CurrClkCycle wave wave.Width)
    |> List.map (fun fd ->
        match fd.Width, fd.Dat with
        | 1, Word b -> $" {b}" 
        | _ -> fastDataToPaddedString valueColNumChars wsModel.Radix fd)
    |> List.map (fun value -> label [ valueLabelStyle ] [ str value ])
    |> (fun rows -> valueColWidth, rows)


/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles.
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match singleWaveWidth wsModel with
        | width when width < float Constants.clkCycleNarrowThreshold && i % 5 <> 0 ->
            []
        | width when i >= 1000 && width <  (float Constants.clkCycleNarrowThreshold * 4. / 3.) && i % 10 <> 0 ->
            []
        | _ ->
            [ text (clkCycleText wsModel i) [str (string i)] ]
            

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> svg (clkCycleNumberRowProps wsModel)

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    let width, rows = valueRows wsModel
    let cursorClkNum = wsModel.CurrClkCycle
    let topRowNumber = [ text [Style [FontWeight "bold"; PaddingLeft "2pt"]] [str (string cursorClkNum)] ] 

    div [ HTMLAttr.Id "ValuesCol" ; valuesColumnStyle width]
        (List.concat [ topRow topRowNumber ; rows ])
    |> TimeHelpers.instrumentInterval "valuesColumn" start

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waves = selectedWaves wsModel
    if List.exists (fun wave -> wave.SVG = None) waves then
        dispatch <| GenerateCurrentWaveforms
    let waveRows : ReactElement list =
        waves
        |> List.map (fun wave ->
            match wave.SVG with
            | Some waveform ->
                waveform
            | None ->
                div [] [] // the GenerateCurrentWaveforms message will soon update this
        )

    div [ waveformColumnStyle ]
        [
            clkCycleHighlightSVG wsModel dispatch
            div [ waveRowsStyle <| wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                    waveRows
                )
        ]
    |> TimeHelpers.instrumentInterval "waveformColumn" start

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    if List.isEmpty wsModel.SelectedWaves then
        div [] [] // no waveforms
    else
        let wHeight = calcWaveformHeight wsModel
        let fixedHeight = Constants.softScrollBarWidth + Constants.topHalfHeight
        let cssHeight =
            if wsModel.SelectedRams.Count > 0 then
                $"min( calc(50vh - (0.5 * {fixedHeight}px)) ,  {wHeight}px)"
            else
                $"min( calc(100vh - {fixedHeight}px) ,  {wHeight}px)"

        div [ HTMLAttr.Id "Scroller";  Style [ Height cssHeight; Width "100%"; CSSProp.Custom("overflow", "auto")]] [
            div [ HTMLAttr.Id "WaveCols" ;showWaveformsStyle ]
                [
                    namesColumn model wsModel dispatch
                    waveformColumn wsModel dispatch
                    valuesColumn wsModel
                ]
            ]

/// Table row that shows the address and data of a RAM component.
let ramTableRow ((addr, data,rowType): string * string * RamRowType): ReactElement =

    tr [ Style <| ramTableRowStyle rowType ] [
        td [] [ str addr ]
        td [] [ str data ]
    ]

/// Table showing contents of a RAM component.
let ramTable (wsModel: WaveSimModel) ((ramId, ramLabel): FComponentId * string) : ReactElement =
    let wanted = calcWaveformAndScrollBarHeight wsModel
    let maxHeight = max (screenHeight() - (min wanted (screenHeight()/2.)) - 300.) 30.
    let fs = wsModel.FastSim
    match Map.tryFind ramId wsModel.FastSim.FComps with
    | None -> div [] []
    | Some fc -> 
        let step = wsModel.CurrClkCycle
        FastRun.runFastSimulation None step fs |> ignore // not sure why this is needed

        // in some cases fast sim is run for one cycle less than currClockCycle
        let memData =
            match fc.FType with
            | ROM1 mem
            | AsyncROM1 mem -> mem
            | RAM1 mem
            | AsyncRAM1 mem -> 
                match FastRun.extractFastSimulationState fs wsModel.CurrClkCycle ramId with
                |RamState mem -> mem
                | x -> failwithf $"What? Unexpected state {x} from cycle {wsModel.CurrClkCycle} \
                        in RAM component '{ramLabel}'. FastSim step = {fs.ClockTick}"
            | _ -> failwithf $"Given a component {fc.FType} which is not a vaild RAM"
        let aWidth,dWidth = memData.AddressWidth,memData.WordWidth

        let print w (a:int64) = NumberHelpers.valToPaddedString w wsModel.Radix (((1L <<< w) - 1L) &&& a)

        let lastLocation = int64 ((2 <<< memData.AddressWidth - 1) - 1)

        /// print a single 0 location as one table row
        let print1 (a:int64,b:int64,rw:RamRowType) = $"{print aWidth a}",$"{print dWidth b}",rw
        /// print a range of zero locations as one table row

        let print2 (a1:int64) (a2:int64) (d:int64) = $"{print aWidth (a1+1L)}..{print aWidth (a2-1L)}", $"{print dWidth d}",RAMNormal

        /// output info for one table row filling the given zero memory gap or arbitrary size, or no line if there is no gap.
        let printGap (gStart:int64) (gEnd:int64) =
            match gEnd - gStart with
            | 1L -> []
            | 2L -> [print1 ((gEnd + gStart) / 2L, 0L,RAMNormal)]
            | n when n > 2L ->
                [print2 gStart gEnd 0L]
            | _ ->
                failwithf $"What? gEnd={gEnd},gStart={gStart}: negative or zero gaps are impossible..."

        /// transform Sparse RAM info into strings to print in a table, adding extra lines for zero gaps
        /// line styling is controlled by a RamRowtype value and added later when the table row react is generated
        let addGapLines (items: (int64*int64*RamRowType) list) = 
            let startItem =
                match items[0] with
                | -1L,_,_ -> []
                | gStart,dStart,rw-> [print1 (gStart,dStart,rw)]
            List.pairwise items
            |> List.collect (fun ((gStart,_,_),(gEnd,dEnd,rwe)) -> 
                let thisItem = if gEnd = lastLocation + 1L then [] else [print1 (gEnd,dEnd,rwe)]
                [printGap gStart gEnd; thisItem])
            |> List.concat

        /// Add a RAMNormal RamRowType value to every location in mem.
        /// Add in additional locations for read and/or write if needed.
        /// Set RamRowValue type to RAMWritten or RAMRead for thse locations.
        /// Write is always 1 cycle after WEN=1 and address.
        /// Read is 1 (0) cycles after address for sync (asynch) memories.
        let addReadWrite (fc:FastComponent) (step:int) (mem: Map<int64,int64>) =
            let getInt64 (a: IOArray) step =
                let w = a.Width
                match w with
                | w when w > 32 -> int64 <| convertBigIntToUInt64 w a.BigIntStep[step]
                | _ -> int64 <| a.UInt32Step[step]

            let readStep =
                match fc.FType with
                | AsyncROM1 _ | AsyncRAM1 _ -> step
                | ROM1 _ | RAM1 _ -> step - 1
                | _ -> failwithf $"What? {fc.FullName} should be a memory component"

            let addrSteps step = getInt64 fc.InputLinks[0] step

            let readOpt =
                match step, fc.FType with
                | 0,ROM1 _ | 0, RAM1 _ -> None
                | _ -> 
                    addrSteps readStep
                    |> Some
            let writeOpt =
                match step, fc.FType with
                | _, ROM1 _ 
                | _, AsyncROM1 _
                | 0, _ -> None
                | _, RAM1 _ | _, AsyncRAM1 _ when getInt64 fc.InputLinks[2] (step-1) = 1L -> 
                    addrSteps (step-1)
                    |> Some
                | _ ->  
                    None

            /// Mark addr in memory map as being rType
            /// if addr does not exist - create it
            let addToMap rType addr mem:Map<int64,int64*RamRowType> =
                match Map.tryFind addr mem with
                | Some (d,_) -> Map.add addr (d,rType) mem
                | None  ->  Map.add addr (0L,rType) mem
    

            Map.map (fun k v -> v,RAMNormal) mem
            |> (fun mem ->
                match readOpt with
                | Some addr -> addToMap RAMRead addr mem
                | None -> mem
                |> (fun mem ->
                    match writeOpt with // overwrite RAMRead here is need be
                    | Some addr -> addToMap RAMWritten addr mem
                    | None -> mem))
 

        /// add fake locations beyong normal address range so that
        /// addGapLines fills these (if need be). These locations are then removed
        let addEndPoints (items:(int64*int64*RamRowType) list)  =
            let ad (a,d,rw) = a
            match items.Length with
            | 0 -> [-1L,0L,RAMNormal;  lastLocation,0L,RAMNormal]
            | _ ->
                if ad items[0] < 0L then items else List.insertAt 0 (-1L,-1L,RAMNormal) items
                |> (fun items ->
                    if ad items[items.Length-1] = lastLocation then 
                        items else 
                    List.insertAt items.Length (lastLocation+1L,0L,RAMNormal) items)
    

        let lineItems =
            memData.Data
            |> addReadWrite fc step
            |> Map.toList
            |> List.map (fun (a,(d,rw)) -> a,d,rw)
            |> List.filter (fun (a,d,rw) -> d<>0L || rw <> RAMNormal)
            |> List.sort
            |> addEndPoints 
            |> addGapLines
        


        Level.item [
            Level.Item.Option.Props ramTableLevelProps
            Level.Item.Option.HasTextCentered
        ] [
            Heading.h6 [
                Heading.Option.Props [ centerAlignStyle ]
            ] [ str ramLabel ]
            div [Style [MaxHeight maxHeight;OverflowY OverflowOptions.Auto]] [
            Table.table [
                Table.IsFullWidth
                Table.IsBordered
            ] [ thead [] [
                    tr [] [
                        th [ centerAlignStyle ] [ str "Address"]
                        th [ centerAlignStyle ] [ str "Data"; sub [Style [MarginLeft "2px"; FontSize "10px"]] [str (string wsModel.CurrClkCycle)]]
                    ]
                ]
                tbody []
                    (List.map ramTableRow lineItems) 
            ] ]
            br []
        ]

/// Bulma Level component of tables showing RAM contents.
let ramTables (wsModel: WaveSimModel) : ReactElement =
    let inlineStyle (styles:CSSProp list) = div [Style (Display DisplayOptions.Inline :: styles)]
    let start = TimeHelpers.getTimeMs ()
    let selectedRams = Map.toList wsModel.SelectedRams
    if List.length selectedRams > 0 then
        let tables = 
            let headerRow =
                ["read", RAMRead; "overwritten",RAMWritten]
                |> List.map (fun (op, opStyle) -> inlineStyle [Margin "0px"] [inlineStyle (ramTableRowStyle  opStyle) [str op]])
                |> function 
                    | [a;b] -> [str "Key: Memory location is " ; a; str ", or " ;b; str ". Click waveforms or use cursor control to change current cycle."] 
                    | _ -> failwithf "What? Can't happen!"
            List.map (fun ram -> td [Style [BorderColor "white"]] [ramTable wsModel ram])  selectedRams
            |> (fun tables -> [tbody [] [tr [] [th [ColSpan selectedRams.Length] [inlineStyle [] headerRow]]; tr [Style [Border "10px"]] tables]])
            |> Fulma.Table.table [
                Table.TableOption.Props ramTablesLevelProps;
                Table.IsFullWidth;
                Table.IsBordered;
                ]
        div [HTMLAttr.Id "TablesDiv"] [ hr [ Style [ Margin "5px"]]; br [ Style [ Margin "0px"]]; tables]
    else div [] []
    |> TimeHelpers.instrumentInterval "ramTables" start

/// <summary>This function regenerates all the waveforms listed on <c> wavesToBeMade </c>. 
/// Generation is subject to timeout, so may not complete.</summary>
/// <remarks>This function have been augmented with performance monitoring function, turn <c>Constants.showPerfLogs</c>
/// to print performance information to console.</remarks>
/// <returns>A tuple with the following information:<br/>
/// a) <c>allWaves</c> (with new waveforms),<br/>
/// b) <c>numberDone</c> (no of waveforms made), and<br/>
/// c) <c>timeToDo</c> (<c>Some timeTaken</c> when greater than <c>timeOut</c> or <c>None</c>
/// if completed with no time out).</returns>
let makeWaveformsWithTimeOut
    (timeOut: option<float>)
    (ws: WaveSimModel)
    (allWaves: Map<WaveIndexT,Wave>)
    (wavesToBeMade: list<WaveIndexT>)
    : Map<WaveIndexT,Wave> * int * option<float> =
    let start = TimeHelpers.getTimeMs()
    let allWaves, numberDone, timeToDo =
        ((allWaves, 0, None), wavesToBeMade)
        ||> List.fold (fun (all,n, _) wi ->
                match timeOut, TimeHelpers.getTimeMs() - start with
                | Some timeOut, timeSoFar when timeOut < timeSoFar ->
                    all, n, Some timeSoFar
                | _ -> 
                    (Map.change wi (Option.map (generateWaveform ws wi)) all), n+1, None)
    let finish = TimeHelpers.getTimeMs()
    
    if Constants.showPerfLogs then
        let countWavesWithWidthRange lowerLim upperLim =
            wavesToBeMade
            |> List.map (fun wi -> (Map.find wi allWaves).Width)
            |> List.filter (fun width -> lowerLim <= width && width <= upperLim)
            |> List.length
        
        printfn "PERF:makeWaveformsWithTimeOut: generating visible only: %b" Constants.generateVisibleOnly
        printfn "PERF:makeWaveformsWithTimeOut: making %d/%d waveforms" (List.length wavesToBeMade) (Map.count allWaves)
        printfn "PERF:makeWaveformsWithTimeOut: binary = %d" (countWavesWithWidthRange 1 1)
        printfn "PERF:makeWaveformsWithTimeOut: int32 = %d" (countWavesWithWidthRange 2 32)
        printfn "PERF:makeWaveformsWithTimeOut: process took %.2fms" (finish-start)

    allWaves, numberDone, timeToDo

/// <summary>Generate scrollbar SVG info based on current <c>WaveSimModel</c>.
/// Called in <c>refreshWaveSim</c> after <c>WaveSimModel</c> has been changed.</summary>
/// <param name="wsm">Target <c>WaveSimModel</c>.</param>
/// <returns>Anonymous record contaning the information to be updated: thumb width,
/// thumb position, and number of cycles the background represents.</returns>
/// <remarks>Note: <c>bkg</c> = background; <c>tb</c> = thumb.</remarks>
let generateScrollbarInfo (wsm: WaveSimModel): {| tbWidth: float; tbPos: float; bkgRep: int |} =
    let bkgWidth = wsm.ScrollbarBkgWidth - 60. // 60 = 2x width of buttons

    /// <summary>Return target value when within min and max value, otherwise min or max.</summary>
    let bound (minV: int) (maxV: int) (tarV: int): int = tarV |> max minV |> min maxV
    let currShownMaxCyc = wsm.StartCycle + wsm.ShownCycles
    let newBkgRep = [ wsm.ScrollbarBkgRepCycs; currShownMaxCyc; wsm.ShownCycles*2 ] |> List.max |> bound 0 Constants.maxLastClk

    let tbCalcWidth = bkgWidth / (1. + (float newBkgRep / float wsm.ShownCycles))
    let tbWidth = max tbCalcWidth WaveSimStyle.Constants.scrollbarThumbMinWidth
    
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
        if Option.isSome wsm.ScrollbarTbOffset
        then ScrollbarMouseMsg (event.clientX, InScrollbarDrag, dispatch) |> dispatch
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

    div [ Style [ MarginTop "5px"; MarginBottom "5px"; Height "25px"]] [
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

/// Start or update a spinner popup
let updateSpinner (name:string) payload (numToDo:int) (model: Model) =
    match model.SpinnerPayload with
    | Some sp when sp.Name = name ->
        {model with SpinnerPayload = Some {Name = name; Payload = payload; ToDo = numToDo; Total = sp.Total}}
    | _ ->
        {model with SpinnerPayload = Some {Name = name; Payload = payload; ToDo = numToDo; Total = numToDo + 1}}

    


/// remove the spinner popup
let cancelSpinner (model:Model) =
    {model with SpinnerPayload = None}
    

/// Major function called after changes to extend simulation and/or redo waveforms.
/// Note that after design change simulation muts be redonne externally, and function called with
/// newSimulation = true.
/// First extend simulation, if needed, with timeout and callback from Spinner if needed.
/// Then remake any waveforms which have changed and not yet been remade. Again if needed with
/// timeOut and callback from Spinner.
/// Spinner (in reality a progress bar) is used if the estimated time to completion is longer than
/// a constant. To get the estimate some initial execution must be completed (1 clock cycle and one waveform).
let rec refreshWaveSim (newSimulation: bool) (wsModel: WaveSimModel) (model: Model): Model * Elmish.Cmd<Msg> = 
    let isSameWave (wi:WaveIndexT) (wi': WaveIndexT) =
        wi.Id = wi'.Id && wi.PortNumber = wi'.PortNumber && wi.PortType = wi'.PortType
    // use given (more uptodate) wsModel
    let model = updateWSModel (fun _ -> wsModel) model
    let start = TimeHelpers.getTimeMs ()
    let fs = wsModel.FastSim
    if fs.NumStepArrays = 0 then
        model, Elmish.Cmd.none
    else
    // starting runSimulation
        //printfn "Starting refresh"
        let lastCycleNeeded = wsModel.StartCycle + wsModel.ShownCycles + 1

        FastRun.runFastSimulation (Some Constants.initSimulationTime) lastCycleNeeded fs
        |> (fun speedOpt ->
            let cyclesToDo = lastCycleNeeded - wsModel.FastSim.ClockTick
            match speedOpt with
            | Some speed when  float cyclesToDo / speed + Constants.initSimulationTime > Constants.maxSimulationTimeWithoutSpinner  &&
                               Option.isNone model.Spinner ->
                // long simulation, set spinner on and dispatch another refresh 
                let spinnerFunc = fun model -> fst (refreshWaveSim newSimulation wsModel model)
                let model = model |> updateSpinner "Waveforms simulation..." spinnerFunc cyclesToDo
                //printfn "ending refresh with continuation..."
                model, Elmish.Cmd.none
                |> TimeHelpers.instrumentInterval "refreshWaveSim" start
            | _ ->
                if speedOpt <> None then 
                    //printfn "Force running simulation"
                    // force simulation to finish now
                    FastRun.runFastSimulation None lastCycleNeeded fs |> ignore                
                // simulation has finished so can generate waves

                printfn $"Ending refresh now at Tick {fs.ClockTick}..."
                let allWavesStart = TimeHelpers.getTimeMs ()    
                    //printfn "starting getwaves"
                // redo waves based on new simulation
                let allWaves = 
                    if newSimulation then
                        //printfn "making new waves..."
                        getWaves wsModel fs 
                    else wsModel.AllWaves
                let model = updateWSModel (fun ws -> {ws with AllWaves = allWaves}) model
                // redo viewer width (and therefore shown cycles etc) based on selected waves names
                // which are currently only calculatable after getwaves has generated waves
                let model = updateViewerWidthInWaveSim model.WaveSimViewerWidth model 
                // extract wsModel from updated model for processing below
                let wsModel = getWSModel model

                let simulationIsUptodate = wsModel.FastSim.ClockTick > wsModel.ShownCycles + wsModel.StartCycle

                match simulationIsUptodate with
                | falae ->
                    
                printfn $"Simulationuptodate: {simulationIsUptodate}"
                // need to use isSameWave here becasue sarray index may have changed
                let wavesToBeMade =
                    allWaves
                    |> Map.filter (fun wi wave ->
                        // Only generate waveforms for selected waves.
                        // Regenerate waveforms whenever they have changed
                        let hasChanged = not <| waveformIsUptodate wsModel wave
                        //if List.contains index ws.SelectedWaves then 
                        List.exists (fun wi' -> isSameWave wi wi') wsModel.SelectedWaves && hasChanged && simulationIsUptodate)
                    |> Map.toList                   
                    |> List.map fst

                let model, allWaves, spinnerPayload, numToDo =
                    //printfn $"{wavesToBeMade.Length} waves to make."
                    let numToDo = wavesToBeMade.Length
                    makeWaveformsWithTimeOut (Some Constants.initSimulationTime) wsModel allWaves wavesToBeMade
                    |> (fun (allWaves, numDone, timeOpt) ->
                            match wavesToBeMade.Length - numDone, timeOpt with
                            | n, None -> 
                                model, allWaves, None, n // finished
                            | _ when numDone = 0 -> 
                                failwithf "What? makewaveformsWithTimeOut must make at least one waveform"
                            | numToDo, Some t when 
                                    float wavesToBeMade.Length * t / float numDone < Constants.maxSimulationTimeWithoutSpinner ->
                                let (allWaves, numDone, timeOpt) = makeWaveformsWithTimeOut None wsModel allWaves wavesToBeMade
                                model, allWaves, None, numToDo - numDone
                            | numToDo, _ ->
                                let payload = Some ("Making waves", refreshWaveSim false {wsModel with AllWaves = allWaves} >> fst)
                                model,  allWaves, payload, numToDo)

                let scrollbarInfo = generateScrollbarInfo wsModel

                let ramComps =
                    let isRAMOrROM fcid (fc: FastComponent) =
                        match fc.FType with
                        | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ ->
                            true
                        | _ -> false
                    Map.filter isRAMOrROM fs.FComps
                    |> Map.toList
                    |> List.map (fun (fcid,fc) -> fc)
                    |> List.sortBy (fun fc -> fc.FullName)

                let ramCompIds = List.map (fun (fc: FastComponent) -> fc.fId) ramComps
                let allWaveA = Map.keys allWaves |> Seq.toArray
                // arrayIndex may have changed, so we have to use new arrayIndex
                // if we cannot find it, then the selected wave no longer exists and is dropped
                let selectedWaves = 
                    wsModel.SelectedWaves
                    |> List.collect (fun wi -> match Array.tryFind (isSameWave wi) allWaveA with Some w -> [w] | None -> [])

                let selectedRams = Map.filter (fun ramfId _ -> List.contains ramfId ramCompIds) wsModel.SelectedRams

                let ws =  
                    {
                        wsModel with
                            State = Success
                            AllWaves = allWaves
                            SelectedWaves = selectedWaves
                            RamComps = ramComps
                            SelectedRams = selectedRams
                            FastSim = fs
                            ScrollbarTbWidth = scrollbarInfo.tbWidth
                            ScrollbarTbPos = scrollbarInfo.tbPos
                            ScrollbarBkgRepCycs = scrollbarInfo.bkgRep
                    }

                let model = 
                    match spinnerPayload with
                    | None -> cancelSpinner model
                    | Some sp -> 
                        updateSpinner (fst sp) (snd sp) numToDo model
                    |> updateWSModel (fun _ -> ws)
                model, Elmish.Cmd.none
                |> TimeHelpers.instrumentInterval "refreshWaveSim" start)
//}

/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let model = MemoryEditorView.updateAllMemoryComps model
    let wsSheet = 
        match model.WaveSimSheet with
        | None ->
            printfn "Sheet was none"
            Option.get (getCurrFile model)
        | Some sheet ->
            printfn "sheet already existing"
            sheet
    printfn $"Refresh Button with width = {model.WaveSimViewerWidth}"
    let model = 
        model
        |> removeAllSimulationsFromModel
        |> fun model -> {model with WaveSimSheet = Some wsSheet}
    let wsModel = getWSModel model
    //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
    match SimulationView.simulateModel model.WaveSimSheet (ModelHelpers.Constants.maxLastClk + ModelHelpers.Constants.maxStepsOverflow)  canvasState model with
    //| None ->
    //    dispatch <| SetWSModel { wsModel with State = NoProject; FastSim = FastCreate.emptyFastSimulation "" }
    | (Error e, _) ->
        dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
    | (Ok simData, canvState) ->
        if simData.IsSynchronous then
            SimulationView.setFastSimInputsToDefault simData.FastSim
            let wsModel = { wsModel with State = Loading ; FastSim = simData.FastSim }
            dispatch <| SetWSModelAndSheet (wsModel, wsSheet)
            dispatch <| RefreshWaveSim wsModel 
        else
            dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet)
           
/// ReactElement showing instructions and wave sim buttons
let topHalf canvasState (model: Model) dispatch : ReactElement * bool =
    let title =
        match model.WaveSimSheet with
        | None -> str "Waveform Viewer"
        | Some sheet -> div [Style [WhiteSpace WhiteSpaceOptions.Nowrap]] [str "Simulating:"  ; span [Style [Color "#3e8ed0"; MarginLeft "5px"]] [str $"{sheet}"]]
    let wsModel = getWSModel model
    //printfn $"Active wsModel sheet={model.WaveSimSheet}, state = {wsModel.State}"
    //printfn $"""Wavesim states: {model.WaveSim |> Map.toList |> List.map (fun (sh, ws) -> sh, ws.State.ToString(),ws.Sheets)}"""
    let loading =
        match wsModel.State with
        | Loading -> true
        | _ -> false

    let titleLine() =       
        div [ Style [
                inlineNoWrap;
                MarginBottom "0px"
                FontSize "24px"
                MarginBottom "0px"
                LineHeight "24px"
                FontWeight 600
                OverflowX OverflowOptions.Hidden ;
                Display DisplayOptions.Inline;
                MarginRight "10px"];

              Id "WaveSimHelp"] [title]

    let refreshStartEndButton() =
        let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg "white" "20px"
        let startOrRenew model = refreshButtonAction canvasState model dispatch
        let waveEnd model = endButtonAction canvasState model dispatch
        let wbo = getWaveSimButtonOptions canvasState model wsModel
        let startEndButton =
            button 
                (topHalfButtonProps wbo.StartEndColor "startEndButton" false) 
                (fun ev -> dispatch <| ExecFuncInMessage(
                    (fun model _ -> if wbo.IsRunning then waveEnd model ev  else startOrRenew model ev),dispatch))
                (str wbo.StartEndMsg)
        let needsRefresh = wbo.IsDirty && wbo.IsRunning
        div 
            [Style [inlineNoWrap]]                     
            (if not wbo.IsRunning then [
                startEndButton
            ] 
            else [
                if needsRefresh then
                    button
                        (topHalfButtonProps IsSuccess "RefreshButton" false)
                        (fun ev -> dispatch <| ExecFuncInMessage((fun model _ -> startOrRenew model ev), dispatch))
                        refreshButtonSvg
                startEndButton
            ])

    let messageOrControlButtons =
        let simError e =
            SimulationView.setSimErrorFeedback e model dispatch
            div [ errorMessageStyle ]
                [ SimulationView.viewSimulationError canvasState e model WaveSim dispatch ]

        let notRunning = 
            false, div [ errorMessageStyle ] [ str "Start the waveform viewer by pressing the Start button." ]

        match model.WaveSimSheet, wsModel.State with
        | Some sheet as sheetOpt, SimError e when sheetOpt <> getCurrFile model ->
            dispatch <| UpdateModel( fun model -> {model with WaveSimSheet = None})
            dispatch <| UpdateModel( updateWSModelOfSheet sheet (fun ws -> {ws with State = Ended}))
            notRunning

        | None, SimError e  ->
            notRunning

        | _,SimError e ->
            false, simError e
            
        | _,NonSequential ->
            false, div [ errorMessageStyle ] [ str "There is no clocked logic in this circuit. Add clocked logic to simulate waveforms." ]

        | _,Empty | _,Ended | None,_ | Some "", _->
            notRunning

        | Some sheet, _ when wsModel.FastSim.SimulatedTopSheet = "" ->
            notRunning
        
        | _,NoProject ->
            false, div [ errorMessageStyle ] [ str "Please open a project to use the waveform viewer." ]

        | _, (Loading | Success) when List.isEmpty wsModel.SelectedWaves && Map.isEmpty wsModel.SelectedRams->
            false, div [Id "WaveSimHelp"] [str "Use 'Select Waves' to add waves for simulation. Right-click for help."]

        | _, Success ->
            true, div [Style [Height Constants.rowHeight; Display DisplayOptions.Flex; JustifyContent "space-between"; Margin "5px"; MarginTop "30px" ; MarginBottom "15px"]]  [

                        zoomButtons wsModel dispatch
                        
                        radixButtons wsModel dispatch
  
                        clkCycleButtons wsModel dispatch
                    ]
        | _ -> notRunning

    let needsBottomHalf, messageOrControlLine = messageOrControlButtons

    div [ topHalfStyle ] [
        titleLine()
 
        div [Style [Display DisplayOptions.Flex; JustifyContent "space-between"]] [
            refreshStartEndButton()
            div [Style [inlineNoWrap; Flex "0 1"]] [selectWavesButton wsModel dispatch; selectRamButton wsModel dispatch]
            ]
        
        messageOrControlLine], needsBottomHalf
        
        

/// Entry point to the waveform simulator.
let viewWaveSim canvasState (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model

    let top, needsBottomHalf = topHalf canvasState model dispatch
    let needsRAMs = not <| Map.isEmpty wsModel.SelectedRams
    let height = calcWaveformAndScrollBarHeight wsModel
    let bottomHalf = // this has fixed height
        div [HTMLAttr.Id "BottomHalf" ; showWaveformsAndRamStyle (if needsRAMs then screenHeight() else height)] (
            if wsModel.SelectedWaves.Length > 0 then [
                showWaveforms model wsModel dispatch
                makeScrollbar wsModel dispatch ]
            else []
            @
            [ramTables wsModel]
        )
    printfn "got bottomhalf"   

    div [] [
        selectRamModal wsModel dispatch
        selectWavesModal wsModel dispatch
        div [ viewWaveSimStyle ]
            [
                //printfn $"WSmodel state: {wsModel.State}"
                top
                //hr [ Style [ MarginBottom "0px";  MarginTop "0px"]]
                
                if needsBottomHalf then bottomHalf else div [] []
                //hr [ Style [ MarginBottom "0px"; MarginTop "0px" ]]
            ]
        
    ]

