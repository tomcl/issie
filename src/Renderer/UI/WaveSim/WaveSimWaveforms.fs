/// showWaveforms and subfunctions to display in DOM the waveforms
/// Waveform SVGs themselves are generated from simulation in WaveSimWaves module
module WaveSimWaveforms

//---------------------------------------------------------------------------------------//
//-----------------------Waveform display DOM Generation---------------------------------//
//---------------------------------------------------------------------------------------//

(*
    This file contains the functions to generate the DOM elements for the waveforms
    in the WaveSimulator. The waveforms are displayed in three columns: the leftmost
    column contains the names of the waveforms, the middle column contains the waveforms
    themselves, and the rightmost column contains the values of the waveforms at the
    current clock cycle. The waveforms are generated as SVGs in the WaveSimSVGs module.
    The functions in this file are responsible for generating the DOM elements that
    display the waveforms in the browser.
*)

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open TopMenuView
open SimGraphTypes
open SimTypes
open NumberHelpers
open DrawModelType
open WaveSimTypes
open WaveSimNavigation
open WaveSimSVGs
open WaveSimSelect
open DiagramStyle

/// ReactElement of the tabs for changing displayed radix
let radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
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


//---------------------------------------------------------------------------------------//
//------------Showing on the schematic the component a waveform comes from---------------//
//---------------------------------------------------------------------------------------//

// Hovering a wave's name highlights the component the wave comes from, which is no help when that
// component is scrolled out of view, or is on a sheet other than the one open. The button at the
// outer edge of each name goes there: it changes sheet if it has to, then scrolls the component to
// the middle of the canvas.

let highlightCircuit (model: Model) comps wave (dispatch: Msg -> Unit) =
    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols comps)))
    // Only wires the open sheet still has. The connections come from the simulation, which holds
    // the design as it was when it was built, so an edit since then leaves ids naming nothing -
    // and they would otherwise be added to the sheet's selection, where a later Delete would act
    // on them. The wire colouring ignores them, which is why this went unnoticed.
    let conns =
        connsOfWave (Simulator.getFastSim()) wave
        |> List.filter (fun cid -> Map.containsKey cid model.Sheet.Wire.Wires)
    // ColourSelection, which records the wires as selected and paints them, in one message.
    //
    // Not SheetT.SelectWires: that is the message a CLICK on a wire sends, and it toggles against
    // PrevWireSelection - the selection left by the last click on the canvas - so hovering a
    // waveform whose wire the user had clicked deselected it instead of highlighting it. It reached
    // this colour by dispatching ColourSelection itself, as a command, which is why it arrived
    // AFTER the purple that BusWireT.SelectWires paints and won. Asking for the colour directly
    // does not depend on which of two queued messages lands last.
    //
    // Sky blue rather than the purple of a selected wire: this is a wire the pointer is passing
    // over, not one the user has chosen. The mouse-out handler's UpdateSelectedWires takes these
    // back out of the selection, which repaints every wire and so clears it.
    dispatch <| Sheet (SheetT.Msg.ColourSelection ([], conns, HighLightColor.SkyBlue))

/// Highlight on the schematic the component a wave comes from, and the wires carrying that wave.
/// An IOLabel is highlighted wherever it appears on the sheet, since every copy of it is the one
/// signal. Does nothing when the component is not on the sheet now open.
let highlightWaveComps (model: Model) (wave: Wave) (dispatch: Msg -> Unit) =
    let symbols = model.Sheet.Wire.Symbol.Symbols
    match Map.tryFind (fst wave.WaveId.Id) symbols with
    | Some {Component={Type=IOLabel;Label=lab}} ->
        symbols
        |> Map.toList
        |> List.map (fun (_,sym) -> sym.Component)
        |> List.filter (function | {Type=IOLabel;Label = lab'} when lab' = lab -> true |_ -> false)
        |> List.map (fun comp -> ComponentId comp.Id)
        |> fun labelComps -> highlightCircuit model labelComps wave dispatch
    | Some _ ->
        highlightCircuit model [fst wave.WaveId.Id] wave dispatch
    | None -> ()

/// The visible part of the schematic, in sheet coordinates, or None when it is not being shown.
/// Sheet coordinates are canvas pixels divided by the zoom, as getVisibleScreenCentre has it.
let private visibleSheetArea (model: Model) : BoundingBox option =
    let canvas = Browser.Dom.document.getElementById "Canvas"
    if canvas = null || model.Sheet.Zoom = 0. then
        None
    else
        Some {
            TopLeft = {X = canvas.scrollLeft / model.Sheet.Zoom; Y = canvas.scrollTop / model.Sheet.Zoom}
            W = canvas.clientWidth / model.Sheet.Zoom
            H = canvas.clientHeight / model.Sheet.Zoom
        }

/// Where on the open sheet the component a wave comes from is, if it is on that sheet at all.
let private symbolBoxOnOpenSheet (model: Model) (wave: Wave) : BoundingBox option =
    Map.tryFind (fst wave.WaveId.Id) model.Sheet.BoundingBoxes

let private centreOf (box: BoundingBox) =
    {X = box.TopLeft.X + box.W / 2.; Y = box.TopLeft.Y + box.H / 2.}

/// True when the component is on the sheet now open and within the visible part of it. Its centre
/// is what has to be visible: a component half off the edge is one the user still has to go to.
let private symbolIsInView (model: Model) (wave: Wave) : bool =
    match symbolBoxOnOpenSheet model wave, visibleSheetArea model with
    | Some box, Some view ->
        let centre = centreOf box
        centre.X > view.TopLeft.X && centre.X < view.TopLeft.X + view.W &&
        centre.Y > view.TopLeft.Y && centre.Y < view.TopLeft.Y + view.H
    | _ -> false

/// Scroll the open sheet to put the component in the middle of the canvas. Does nothing when the
/// component is not on the sheet now open, which after a sheet change is true until it has loaded.
let private scrollToSymbol (wave: Wave) (model: Model) (dispatch: Msg -> unit) =
    match symbolBoxOnOpenSheet model wave, visibleSheetArea model with
    | Some box, Some view ->
        let centre = centreOf box
        // UpdateScrollPos is in canvas pixels, which are sheet coordinates scaled by the zoom
        dispatch <| Sheet (SheetT.UpdateScrollPos {
            X = max 0. ((centre.X - view.W / 2.) * model.Sheet.Zoom)
            Y = max 0. ((centre.Y - view.H / 2.) * model.Sheet.Zoom) })
    | _ -> ()

/// The sheet holding a component, found by the id it has on that sheet's canvas.
let private sheetOfComponent (model: Model) (wave: Wave) : string option =
    let compId = fst wave.WaveId.Id
    model.CurrentProj
    |> Option.bind (fun project ->
        project.LoadedComponents
        |> List.tryFind (fun ldc ->
            fst ldc.CanvasState |> List.exists (fun comp -> ComponentId comp.Id = compId))
        |> Option.map (fun ldc -> ldc.Name))

/// Put the component in the middle of the canvas and highlight it, once the sheet holding it has
/// loaded. Opening a sheet is asynchronous - doBatchOfMsgsAsynch delivers its load messages in one
/// batch 300ms later, and that batch ends with the Ctrl-W that fits the sheet to the window - so
/// this waits for the component to appear rather than running after the next render, which would
/// be too early and would then be undone by that Ctrl-W. Gives up rather than waiting for ever:
/// the sheet may be one which cannot be opened at all.
let rec private showWhenSheetLoaded (triesLeft: int) (wave: Wave) (dispatch: Msg -> unit) =
    let attempt model dispatch =
        match symbolBoxOnOpenSheet model wave with
        | Some _ ->
            scrollToSymbol wave model dispatch
            highlightWaveComps model wave dispatch
        | None when triesLeft > 0 ->
            showWhenSheetLoaded (triesLeft - 1) wave dispatch
        | None -> ()
    dispatch <| DispatchDelayed(Constants.sheetLoadPollMs, ExecFuncInMessage(attempt, dispatch))

/// Show on the schematic the component a wave comes from: change to its sheet if it is on another
/// one, then scroll it into the middle of the canvas and highlight it.
/// The highlight is applied here as well as on hover because it does not survive the journey: a
/// sheet change loads a new set of symbols, taking any selection with it, and the highlight the
/// hover had applied was to a component the user could not see anyway.
let private showSymbolOfWave (wave: Wave) (model: Model) (dispatch: Msg -> unit) =
    match symbolBoxOnOpenSheet model wave with
    | Some _ ->
        scrollToSymbol wave model dispatch
        highlightWaveComps model wave dispatch
    | None ->
        match sheetOfComponent model wave, model.CurrentProj with
        | Some sheet, Some project ->
            MenuHelpers.openFileInProject sheet project model dispatch
            showWhenSheetLoaded Constants.sheetLoadPollTries wave dispatch
        | _ -> ()

/// Button at the outer edge of a wave's name which shows that wave's component on the schematic.
/// Green when the component cannot be seen - on another sheet, or scrolled out of view - and grey
/// when it is already in front of the user, so that the colour says whether pressing it will move
/// anything without the button itself coming and going under the mouse.
let private viewSymbolButton (model: Model) (wave: Wave) (dispatch: Msg -> unit) : ReactElement =
    let isInView = symbolIsInView model wave
    div [
        viewSymbolButtonStyle isInView
        Draggable false
        HTMLAttr.Title (
            if isInView then "Centre this component on the schematic"
            else "Show this component: change sheet and scroll to it")
        OnClick (fun ev ->
            // Stop the click reaching the row, which selects and drags
            ev.stopPropagation()
            // Run against the model as it is when this is handled, not as it was when the row was
            // drawn: the canvas may have been scrolled since
            dispatch <| ExecFuncInMessage(showSymbolOfWave wave, dispatch))
    ] [ eyeSvg "white" "12px" ]

/// Create label of waveform name for each selected wave.
/// Note that this is generated after calling selectedWaves. Any changes to this function
/// must also be made to valueRows and waveRows, as the order of the waves matters here.
/// This is because the wave viewer is comprised of three columns of many rows, rather
/// than many rows of three columns.
let nameRows (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement list =
    let shown = selectedWaves wsModel
    /// Which names more than one of the waveforms on show carries.
    ///
    /// A waveform is called sheet.component.port, which does not say which INSTANCE of the sheet it
    /// came from - deliberately, since naming the instance meant naming it as a path of labels in
    /// every name whether it was needed or not. Two instances can therefore reach this column under
    /// one name, and only then is there anything to disambiguate.
    let ambiguous =
        shown
        |> List.countBy (fun wave -> wave.ViewerDisplayName)
        |> List.choose (fun (name, count) -> if count > 1 then Some name else None)
        |> Set.ofList
    shown
    |> List.map (fun wave ->
        let visibility =
            if wsModel.HoveredLabel = Some wave.WaveId then
                "visible"
            else "hidden"

        Level.level [
            Level.Level.Option.Props [
                nameRowLevelStyle (wsModel.HoveredLabel = Some wave.WaveId)
                // Every handler below updates the wave sim model rather than REPLACING it with the
                // one this row was drawn from. A row is drawn once and its handlers run for as
                // long as it is on screen, so a replacement puts back whatever the rest of the
                // column has changed since - which is how a drag that reordered the waveforms was
                // undone by the mouse leaving the row it started on. Same reason as
                // WaveSimSelect.toggleRamSelection.
                let execWithModel (f: Model -> Unit) = ExecFuncInMessage((fun model _ -> f model), dispatch)
                OnMouseOver (fun _ -> dispatch <| execWithModel (fun model ->
                    if (getWSModel model).DraggedIndex = None then
                        dispatch <| UpdateWSModel (fun ws -> {ws with HoveredLabel = Some wave.WaveId})
                        highlightWaveComps model wave dispatch)
                )
                OnMouseOut (fun _ ->
                    dispatch <| UpdateWSModel (fun ws ->
                        {ws with HoveredLabel = None; DraggedIndex = None; PrevSelectedWaves = None })
                    dispatch <| Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [])))
                    dispatch <| Sheet (SheetT.Msg.UpdateSelectedWires (connsOfWave (Simulator.getFastSim()) wave, false))
                )

                Draggable true

                OnDragStart (fun ev ->
                    ev.dataTransfer.effectAllowed <- "move"
                    ev.dataTransfer.dropEffect <- "move"
                    dispatch <| UpdateWSModel (fun ws ->
                        { ws with
                            DraggedIndex = Some wave.WaveId
                            PrevSelectedWaves = Some ws.SelectedWaves })
                )

                OnDrag (fun ev ->
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()

                    // If the user drags the label outside the bounds of the wave name column
                    if ev.clientX < bcr.left || ev.clientX > bcr.right ||
                        ev.clientY < bcr.top || ev.clientY > bcr.bottom
                    then
                        dispatch <| UpdateWSModel (fun ws ->
                            { ws with
                                DraggedIndex = None
                                HoveredLabel = Some wave.WaveId
                                // the order before the drag started, or the current one if somehow
                                // PrevSelectedWaves was not set
                                SelectedWaves = Option.defaultValue ws.SelectedWaves ws.PrevSelectedWaves })
                )

                OnDragOver (fun ev -> ev.preventDefault ())

                OnDragEnter (fun ev ->
                    ev.preventDefault ()
                    ev.dataTransfer.dropEffect <- "move"
                    let nameColEl = Browser.Dom.document.getElementById "namesColumn"
                    let bcr = nameColEl.getBoundingClientRect ()
                    // Read from the event now: the update below runs after this handler has
                    // returned, by which time the event object is no longer ours to read.
                    let offsetY = ev.clientY - bcr.top
                    dispatch <| UpdateWSModel (fun ws ->
                        let draggedWave = Option.toList ws.DraggedIndex
                        let others = ws.SelectedWaves |> List.except draggedWave
                        // Bounded by what is left after the dragged row is taken out, which is what
                        // it is being put back into. Bounding it by the whole list allowed an index
                        // one past the end of that, which insertManyAt refuses.
                        let index =
                            int offsetY / Constants.rowHeight - 1 |> max 0 |> min (List.length others)
                        {ws with SelectedWaves = others |> List.insertManyAt index draggedWave})
                )

                OnDragEnd (fun _ ->
                    dispatch <| UpdateWSModel (fun ws ->
                        {ws with DraggedIndex = None; PrevSelectedWaves = None})
                )
            ]
        ] [ Level.left
                [ Props (nameRowLevelLeftProps visibility) ]
                // The view button sits outboard of the delete icon: delete belongs to the label,
                // the view button is an extra beside it, and this is the order the eye ends up in
                // the slot taken from the margin.
                [ viewSymbolButton model wave dispatch
                  Delete.delete [
                    Delete.Option.Size IsSmall
                    Delete.Option.Props [
                        OnClick (fun _ ->
                            dispatch <| UpdateWSModel (fun ws ->
                                {ws with SelectedWaves = List.except [wave.WaveId] ws.SelectedWaves})
                        )
                    ]
                  ] []
                ]
            Level.right
                [ Props [ Style [ PaddingRight Constants.labelPadding ] ] ]
                [ label
                    [ nameLabelStyle (wsModel.HoveredLabel = Some wave.WaveId)
                      // Only where the name alone cannot say which waveform this is. A tooltip on
                      // every name would fire while the mouse was on its way somewhere else, which
                      // is most of the time it is over this column.
                      if Set.contains wave.ViewerDisplayName ambiguous then
                          HTMLAttr.Title (
                              String.concat "." (wave.SubSheet @ [wave.CompLabel; wave.PortLabel])) ]
                    [ wave.ViewerDisplayName |> str ] ]
        ]
    )

/// Create column of waveform names
let namesColumn model wsModel dispatch : ReactElement =
    let rows =
        nameRows model wsModel dispatch
    div (namesColumnProps wsModel)
        (List.concat [ topRow wsModel (namesColBackground "transparent" (Some (2, separatorColour, false))) []; rows ])
    //|> TimeHelpers.instrumentInterval "namesColumn" start


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
    |> List.map (getWaveValue wsModel)
    |> List.map (fun fd ->
        match fd with
        // the viewer does not have this value: say so rather than print a number that is not one
        | None -> " ?"
        | Some fd ->
            match fd.Width, fd.Dat with
            | 1, Word b -> $" {b}"
            | _ -> fastDataToPaddedString valueColNumChars wsModel.Radix fd)
    |> List.map (fun value -> label [ valueLabelStyle wsModel] [ str value ])
    |> (fun rows -> valueColWidth, rows)


/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles multiplied by the current multiplier
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        let n = i * wsModel.SamplingZoom
        match singleWaveWidth wsModel with
        | width when width < float Constants.clkCycleNarrowThreshold && i % 5 <> 0 ->
            []
        | width when n >= 1000 && width <  (float Constants.clkCycleNarrowThreshold * 5. / 3.) && i % 10 <> 0 ->
            []
        | width when n >= 10000 && width <  (float Constants.clkCycleNarrowThreshold * 6. / 3.) && i % 10 <> 0 ->
            []
        | width when n >= 100000 && width <  (float Constants.clkCycleNarrowThreshold * 7. / 3.) && i % 10 <> 0 ->
            []
        | _ ->
            [ text (clkCycleText wsModel i) [str (string n)] ]
            

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> svg (clkCycleNumberRowProps wsModel)

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let width, rows = valueRows wsModel
    let cursorClkNum = wsModel.CursorExactClkCycle
    let topRowNumber = [ div [Style [
            FontSize wsModel.WSConfig.FontSize ;
            VerticalAlign "bottom";
            FontWeight wsModel.WSConfig.FontWeight;
            PaddingLeft "4pt"]] [str (string <| cursorClkNum)] ]
    div [ HTMLAttr.Id "ValuesCol" ; valuesColumnStyle wsModel width]
        (List.concat [ topRow wsModel plainTopRowLine topRowNumber ; rows ])
    //|> TimeHelpers.instrumentInterval "valuesColumn" start

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) dispatch : ReactElement =
    let start = TimeHelpers.getTimeMs ()
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waves = selectedWaves wsModel

    // Each row is drawn from the data as it is now: the view the controls ask for where that has
    // arrived, and the view the row is already showing where it has not. Making the SVG is a pure
    // function of the two, memoised on exactly its inputs, so a render in which nothing changed
    // costs a map lookup per row - which matters, because Issie renders on every message.
    //
    // Nothing is dispatched from here. This used to ask for a regeneration whenever a row had no
    // SVG, which is a view deciding what the update should do next, from a model field that existed
    // to be looked at by the view.
    let waveRows : ReactElement list =
        waves
        |> List.map (fun wave ->
            match WaveSimSVGs.drawnFor wsModel wave with
            | Some drawn -> drawn.Svg
            // nothing has ever been drawn for this wave - it has just been selected, and its first
            // data is on its way
            | None -> div [] [])

    
    div [ waveformColumnStyle ]
        [
            cursorCycleHighlightSVG wsModel dispatch
            div [ waveRowsStyle wsModel ]
                (  [ clkCycleNumberRow wsModel ] @
                   waveRows)
        ]
    |> TimeHelpers.instrumentInterval "waveformColumn" start

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    try
        if List.isEmpty wsModel.SelectedWaves then
            div [] [] // no waveforms
        else
            /// Said when the waveforms have been showing a view other than this one for longer
            /// than any fetch takes, and nothing is claiming to be working on it.
            ///
            /// Waveforms lagging by a frame or two is how a viewer whose data comes over a wire is
            /// meant to behave, and blanking them instead is worse. The cost of that is that a
            /// waveform which is simply WRONG looks exactly like one that is about to be right,
            /// and nothing in the viewer would ever have said otherwise. This is the difference,
            /// and it is deliberately loud: it is here to catch our own bugs, in front of whoever
            /// hits one.
            ///
            /// A pure function of the model, the cache and the clock, worked out here on every
            /// render rather than tracked. The only thing recorded is when the view changed;
            /// whether the data for it is here is a question about the cache, and how long that has
            /// been true is the clock's business.
            ///
            /// Asked of the DATA and not of what is drawn. The drawn waveforms are filled in by the
            /// render itself, further down this same function, so asking them here would be asking
            /// before the answer is made: every view change would show this banner for one frame,
            /// which is the opposite of a warning that means something.
            let staleWarning =
                let behindMs = TimeHelpers.getTimeMs () - wsModel.ViewSetAtMs

                let anyWaveBehind () =
                    let window: WaveSlice.Window =
                        { StartSample = wsModel.StartCycle
                          Multiplier = wsModel.SamplingZoom
                          SampleCount = wsModel.ShownCycles }

                    wsModel.SelectedWaves
                    |> List.map (fun wi -> SignalHandle wi.SimArrayIndex)
                    |> fun handles -> WaveData.needFetching handles window
                    |> List.isEmpty
                    |> not

                if
                    model.SpinnerPayload = None
                    && behindMs > Constants.staleWaveformWarningMs
                    && anyWaveBehind ()
                then
                    // Which of the two it is matters more than the fact: a fetch in flight is slow,
                    // and no fetch at all is broken.
                    let waiting =
                        if ModelHelpers.sidecarFetchInFlight model then
                            "Data for it has been asked for and has not arrived."
                        else
                            "Nothing is fetching it, so this is a fault rather than a wait."

                    [ div
                          [ Style
                                [ Background "#ffe0b2"
                                  BorderBottom "1px solid #e69138"
                                  Color "#7f4a00"
                                  Padding "4px 8px"
                                  FontSize "12px" ] ]
                          [ str
                                $"These waveforms have been showing an older view for %.0f{behindMs / 1000.0} seconds. What is drawn is not what the cycle numbers above it say. {waiting}" ] ]
                else
                    []

            div [ HTMLAttr.Id "Scroller"; waveformScrollerStyle ] [
                yield! staleWarning
                div [ HTMLAttr.Id "WaveCols" ;showWaveformsStyle ]
                    [
                        namesColumn model wsModel dispatch 
                        waveformColumn wsModel dispatch
                        valuesColumn wsModel
                    ]
                ]
    with
        // Catch any exceptions during waveform view calculation and display an error message.
        // an error here is probably because the simulation has finished
        // and is not fatal. this error boundary ignores the error
        // and displays a blank div.
        | ex ->
            Log.dbg Log.Wave $"waveforms drawn after the simulation ended: {ex.Message}"
            div [] []
