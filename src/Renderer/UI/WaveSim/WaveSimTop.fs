
/// Top-level functions for Waveform Simulator
module WaveSimTop

//---------------------------------------------------------------------------------------//
//-----------------------Top-level functions for Waveform Simulator---------------------//
//---------------------------------------------------------------------------------------//

// refreshWaveSim - updates the simulation and/or waveforms to match current WaveSimModel parameters
// viewWaveSim - creates the DOM of the waveform simulator

open Fulma
open Fable.React
open Fable.React.Props
open EEExtensions
open CommonTypes
open ModelType
open ModelHelpers
open Sheet.SheetInterface
open WaveSimStyle
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open WaveSimTypes
open WaveSimNavigation
open DiagramStyle
open WaveSimSelectHelpers

open WaveSimSVGs.Constants
open Optics
open Optics.Operators

/// Start or update a spinner popup
let setProgressBar (name:string) payload (numToDo:int) (model: Model) =
    match model.SpinnerPayload with
    | Some sp when sp.Name = name -> // continuation of an existing progress bar
        {model with SpinnerPayload = Some {sp with ToDo = numToDo}}
    | _ -> // A new progress bar is needed
        {model with SpinnerPayload = Some {UseProgressBar = true; Name = name; ToDo = numToDo; Total = numToDo + 1}}
    |> (fun model -> {model with RunAfterRenderWithSpinner = Some {FnToRun=payload; ButtonSpinnerOn=true}})

let setButtonSpinner payload (model: Model) =
    {model with SpinnerPayload = Some {UseProgressBar = false; Name = ""; ToDo = 0; Total = 0}}
    |> (fun model -> {model with RunAfterRenderWithSpinner = Some {FnToRun = payload; ButtonSpinnerOn = true}})

/// remove the spinner popup
let cancelSpinner (model:Model) =
    {model with SpinnerPayload = None}



/// Bring a WaveSimModel's selection, and what is known about each selected wave, into step with
/// the simulation.
///
/// Two things have to be true before anything reads the model. Every selected wave must name a port
/// of the simulation as it is NOW: a wave index says where its data lies as well as which port it
/// is, and the first is true only of the build it came from, so a selection made before a rebuild
/// has to be resolved against the new one and a wave the rebuilt design no longer offers dropped.
/// And every selected wave must have its Wave record - its name, its width, where its data lies -
/// since that is what the viewer draws from and what the selection dialog reads back.
///
/// Records already held are kept: they carry the SVG last drawn, which is what makes redrawing a
/// view cheap.
///
/// This is called at the top of every refresh, and it has to be, not once the data for the view has
/// arrived. Selecting a wave in .NET mode is exactly the case where the view is NOT held: the
/// refresh asks for it and returns, and anything conditional on that answer never runs. Leaving the
/// model unreconciled that long emptied the selection - the dialog reads it back through
/// consistentSelectedWaves, which drops what has no record, so ticking one more box replaced the
/// selection with the box just ticked.
let private reconcileWaves (fs: FastSimulation) (ws: WaveSimModel) : WaveSimModel =
    let selected = ws.SelectedWaves |> List.choose (WaveSimHelpers.reResolveWave fs)

    let details =
        selected
        |> List.map (fun wi ->
            wi,
            match Map.tryFind wi ws.WaveDetails with
            | Some wave -> wave
            | None -> WaveSimHelpers.makeWave ws fs wi)
        |> WaveSimHelpers.makeWaveMap

    { ws with
        SelectedWaves = selected
        WaveDetails = details }

/// The RAM and ROM components of a simulation, in the order the RAM selector lists them.
///
/// A fact about the simulation and nothing else, but it was rebuilt on every GenerateWaveforms -
/// which is every tick of a checkbox - by filtering and sorting every FastComponent in the design.
/// main6 of largeTest has about 480,000 of them.
/// How the cache reads the renderer's own simulation: a handle is a driver index into whatever
/// simulation is current.
///
/// Looked up at read time rather than closed over, so installing this pins nothing - a closure
/// holding a FastSimulation would keep its step arrays alive for as long as the cache lived,
/// which is the leak ModelHelpers.releaseWaveSimData exists to prevent.
let private localDriverData (SignalHandle i) =
    match Array.tryItem i (Simulator.getFastSim ()).Drivers with
    | Some(Some driver) -> Some driver.DriverData
    | _ -> None

let private ramCompIdsOf: FastSimulation -> FComponentId list =
    Helpers.memoizeByIdentity (fun fs ->
        fs.FComps
        |> Map.filter (fun _ (fc: FastComponent) ->
            match fc.FType with
            | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ -> true
            | _ -> false)
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun fc -> fc.FullName)
        |> List.map (fun fc -> fc.fId))

/// Major function called after changes to extend simulation and/or redo waveforms.
/// Note that after design change simulation must be recreated externally, and the function called with
/// newSimulation = true. That is because this function has no way to know that the simulation has changed.
/// This function performs (as required) three actions.
/// 1. Extend the simulation to the current cycle (if not already done).
/// 2. Remake the Wave headers, one for each selected waveform.
/// 3. Remake (or make for first time) the saved waveform SVGs for all selected waveforms.
let rec refreshWaveSim (newSimulation: bool) (wsModel: WaveSimModel) (model: Model): Model * Elmish.Cmd<Msg> =
    // The function performs immediately the first part of the main long functions to determine their time and as needed splits
    // the rest of the work into multiple function calls using a spinner to alert the user to the delay.
    // The Spinner (in reality a progress bar) is used if the estimated time to completion is longer than
    // a constant. To get the estimate some initial execution must be completed (1 clock cycle and one waveform).

    /// Give the cursor-control box its focus back after the next render, but only if it had it
    /// when this refresh began.
    ///
    /// The point of this has always been that a progress bar shown mid-refresh takes focus away
    /// from the box the user is typing a clock cycle into. Restoring it unconditionally, which is
    /// what it used to do, meant every refresh - a click on a waveform, a zoom, a cursor step -
    /// ended with the box focused whether the user had gone near it or not. The keyboard then
    /// belonged to a text field, so the Left and Right arrows that step the cursor stopped
    /// working until something else was clicked. Only put back what was actually taken.
    let dispatchFocusAfterRender model =
        let hadFocus =
            match Browser.Dom.document.activeElement with
            | null -> false
            | el -> el.id = "clkCycleInput"
        match hadFocus with
        | false -> model
        | true ->
            let focusCurrClk1 _ model =
                let el = Browser.Dom.document.getElementById "clkCycleInput"
                if el <> null then el.focus()
                model
            { model with RunAfterRenderWithSpinner = Some <| Option.defaultValue {FnToRun=focusCurrClk1;ButtonSpinnerOn=false} model.RunAfterRenderWithSpinner }

           
    /// Make sure we always have consistent parameters. They will be written back to model after this function terminates.
    /// The validation may be done more than once because this function is recursive, but that is OK.
    /// validateSimparas is idempotent unless model changes.
    let wsModel =
        let chooseDefaultWaves (wsModel: WaveSimModel) =
            // a viewer with nothing in it is never what the user wants: give a first start the top
            // sheet's own ports. Does nothing once anything at all has been selected.
            //
            // This is where every wave the simulation offers used to be described, one record per
            // viewable port - more records than a large flat design has components. What each
            // SELECTED wave is, is worked out below.
            let t0, m0 = TimeHelpers.getTimeMs (), TimeHelpers.usedHeapBytes ()
            WaveSimSelect.withDefaultSelectionIfEmpty (Simulator.getFastSim()) wsModel
            |> fun ws ->
                if Log.isOn Log.Perf then
                    let dt, dm = TimeHelpers.getTimeMs () - t0, TimeHelpers.usedHeapBytes () - m0
                    Log.dbg Log.Perf $"defaultWaves {ws.SelectedWaves.Length} selected %8.0f{dt}ms  %+6.0f{dm / 1.0e6}MB"
                ws
        validateSimParas wsModel
        |> if newSimulation then chooseDefaultWaves else id
        |> reconcileWaves (Simulator.getFastSim())

    // Use the given (more uptodate) wsModel. This ensures it is returned from this function.
    //
    // Settling how many cycles are shown belongs here, before anything is worked out from it. It
    // depends on how wide the names column is, which is as wide as the longest selected wave's
    // name - so selecting a wave with a longer name than any before it takes cycles off the view.
    // That was only done after the render below, by which point this refresh had already decided
    // what data to ask the simulator for, and asked for the wrong view: what the sidecar sends back
    // is one exact window and covers no other, so the waveforms drew blank. The viewer width itself
    // is measured from the DOM, but it is already in the model - a ResizeObserver puts it there -
    // so nothing here needs a render to have happened. The TODO this answers is as old as the
    // function.
    /// The view a WaveSimModel is asking for: ShownCycles samples, one every SamplingZoom cycles,
    /// from StartCycle - which counts SAMPLES, not clock cycles (see WaveSlice.Window).
    let windowOf (ws: WaveSimModel) : WaveSlice.Window =
        { StartSample = ws.StartCycle
          Multiplier = ws.SamplingZoom
          SampleCount = ws.ShownCycles }

    /// The view as it was before this refresh, to tell a refresh that changes it from one that
    /// does not - which is the difference between a wait that starts now and one already running.
    let previousWindow = windowOf (getWSModel model)

    let model = updateWSModel (fun _ -> wsModel) model |> updateViewerWidthInWaveSim model.WaveSimViewerWidth

    // The one write of this timestamp. Everything that asks how long the waveforms have been
    // behind is a pure function of it and the clock, worked out where it is asked.
    let model =
        model
        |> updateWSModel (fun ws ->
            if windowOf ws = previousWindow then
                ws
            else
                { ws with ViewSetAtMs = TimeHelpers.getTimeMs () })

    let wsModel = getWSModel model

    // local containing the current fast simulation to be examined and extended if need be.
    let fs = Simulator.getFastSim()

    /// This is the highest simulation cycle that might be required in this simulation
    /// as determined by the current WSConfig. The limit for this refresh will be the minimum
    /// of this and what is required for the current view.
    let cycleLimit = 
        (wsModel.ShownCycles + wsModel.StartCycle)*wsModel.SamplingZoom //last shown cycle + 1, to get transitions
        |> min (wsModel.WSConfig.LastClock + Constants.maxStepsOverflow - 1 + wsModel.SamplingZoom) // cannot go beyond the array

    if cycleLimit >= fs.MaxArraySize then
        failwithf $"Sanity check failed: lastCycleNeeded = {cycleLimit} >= fs.MaxArraySize = {fs.MaxArraySize}"

    if fs.NumStepArrays = 0 then
        // There is no simulation to draw from. That is not a quiet case to skip: the viewer is
        // showing waveforms, this refresh has just recorded that the view changed, and returning
        // here leaves those waveforms drawn for a view that will now never be fetched - silently,
        // and for as long as the user keeps moving the cursor rather than the window, since only a
        // window move refreshes again.
        //
        // It happens when a build failed after a simulation was already on screen. The commonest
        // reason is running out of memory for the step arrays, which the renderer still allocates
        // in full even when the sidecar is doing the simulating - four million cycles of 3cpu is
        // 7.1GB - and which fails once the heap is fragmented even where it succeeded before.
        Log.error
            "the waveform viewer has no simulation to draw from - what is on screen is whatever was drawn last, and will not update"

        model, Elmish.Cmd.none
            
    else
        // The simulation must be run to the last cycle needed for the current view.
        // This may require no work, in which case runFastSimulation will return immediately.
        // NB during waveform simulation the simulation buffer is NOT used as a circular buffer. Simulation
        // length is therefore limited to the size of the buffer.
        // All date from time = 0 is stored.

        /// This function calculates the last cycle needed for the simulation for the current view.
        let lastCycleNeeded wsModel =
            (wsModel.ShownCycles + wsModel.StartCycle)*wsModel.SamplingZoom + 1
            |> min cycleLimit
        /// This function is called when the simulation is running and the spinner is needed.
        /// It dispatches a continuation which will recursively call refreshWaveSim
        let runSimulationWithSpinner cyclesToDo model =
            let spinnerFunc = fun _dispatch model ->
                let wsModel = getWSModel model
                // A RunData callback can only return a model, so this DISCARDS whatever command the
                // refresh wanted to issue - see the note on GenerateCurrentWaveforms below for what
                // that costs. It is harmless here only because this path exists for the renderer's
                // own simulator, which needs no command to fetch anything. Anything reached from
                // here that does will need RunData to carry a command.
                fst (refreshWaveSim false wsModel model)
            let model =
                model
                |> setProgressBar $"Extending Circuit Simulation..." spinnerFunc cyclesToDo
            model, Elmish.Cmd.none
 
        /// The view this refresh is for.
        let window = windowOf wsModel

        /// the drivers the shown waves read. Taken from the selection as it stands: after a
        /// rebuild it may name indices that no longer exist, which the re-resolution below
        /// corrects - and the fetch then misses its cover check and is simply made again.
        let shownDrivers =
            wsModel.SelectedWaves
            |> List.map (fun wi -> wi.SimArrayIndex)
            |> List.filter (fun i -> i >= 0 && i < fs.Drivers.Length)

        // Which simulator is answering, decided once. Nothing below asks again: the refresh has
        // one question - is the data for this view here yet - and only the answer differs.
        // The lookup and the clock are read at call time rather than closed over, so installing
        // them pins nothing: a closure holding a FastSimulation would keep its step arrays alive
        // for as long as the cache lived.
        WaveProvider.selectSimulator
            model.SimulateInRenderer
            newSimulation
            localDriverData
            (fun () -> (Simulator.getFastSim()).ClockTick)

        /// Ask the simulator for this view, and come back to draw when it answers. The waves on
        /// screen stay as they are meanwhile; they are redrawn on re-entry, by which point
        /// WaveData holds the window and every read below finds it there.
        let fetchThisView () =
            match model.CurrentProj with
            | None -> Elmish.Cmd.none
            | Some project ->
                let design =
                    ModelHelpers.designOf project (model.Sheet.GetCanvasState())
                    |> CanvasExtractor.simpleDesignOfLoadedComponents
                    |> fun d -> { d with TopSheet = fs.SimulatedTopSheet }

                let failed (why: string) =
                    UpdateModel(fun m ->
                        Log.error $"the .NET simulator could not answer for this view: {why}"
                        cancelSpinner m)

                Elmish.Cmd.OfPromise.either
                    (fun () ->
                        WaveProvider.fillFor
                            design
                            fs.MaxArraySize
                            fs
                            shownDrivers
                            window
                            wsModel.CursorExactClkCycle
                            ignore)
                    ()
                    (function
                        | Ok() ->
                            // Just refresh: the waves on screen are the ones drawn for whatever
                            // view they were last drawn for, and each carries that view's stamp, so
                            // the refresh below finds them out of date and redraws them from what
                            // has just arrived.
                            //
                            // This used to throw every SVG away first, because a wave that had been
                            // regenerated while the data was in flight carried the CURRENT view's
                            // stamp with nothing drawn under it, and so counted as up to date. A
                            // wave with no data now keeps what it is showing and does not take the
                            // stamp, which removes the reason - and with it a flash of white across
                            // every waveform on every scroll.
                            // GenerateCurrentWaveforms rather than UpdateModel, because a refresh
                            // returns a model AND A COMMAND, and UpdateModel can only carry the
                            // model. This was `UpdateModel(fun m -> fst (refreshWaveSim ...))`, and
                            // that `fst` threw the command away.
                            //
                            // The command it threw away was the next fetch. When the view moved
                            // while a fetch was in flight - two zoom clicks, a scroll, anything -
                            // this refresh was the one thing that would ask for the view the user
                            // had ended up on, and it computed the request and dropped it. The
                            // waveforms then showed the older view for ever, with nothing running
                            // and nothing to say so.
                            GenerateCurrentWaveforms
                        | Error e -> failed e)
                    (fun exn -> failed exn.Message)

        let viewIsHeld =
            WaveProvider.covers
                model.SimulateInRenderer
                window
                (List.map SignalHandle shownDrivers)
                wsModel.CursorExactClkCycle

        // What this refresh is working from, in one line: the view, what is selected, what is known
        // about it, and whether the data for it is already here. The waveform viewer's failures are
        // almost always a disagreement between two of those.
        let heldOrNot =
            if viewIsHeld then "held"
            elif WaveProvider.fetchInProgress () then "waiting on a fetch already running"
            else "fetching"

        Log.dbg
            Log.Wave
            $"refresh: view {window.StartSample}+{window.SampleCount}x{window.Multiplier} cursor {wsModel.CursorExactClkCycle}, {wsModel.SelectedWaves.Length} selected, {wsModel.WaveDetails.Count} detailed, {shownDrivers.Length} drivers - {heldOrNot}"

        if not viewIsHeld && WaveProvider.fetchInProgress () then
            // A fetch is already running against this session, and a second would interleave with
            // it. Do nothing at all: when the one in progress lands it refreshes, and that refresh
            // asks for whatever view is current by then - which is this one, or a later one, but
            // never one the user has already scrolled past.
            model, Elmish.Cmd.none
        elif not viewIsHeld then
            model, fetchThisView ()
        else

            // Run the renderer's own simulator to the last cycle this view needs - if it is the one
            // answering. The sidecar is run to that same cycle as part of fetching the view, so
            // when it is simulating this ran the whole thing a SECOND time, in the process that is
            // drawing, and threw every cycle of it away. With the cursor at 1.26M on 3cpu that is a
            // renderer locked up for as long as the simulation takes, starting the moment the
            // waveforms became correct - and it is the renderer's run, not the sidecar's, that the
            // progress bar below is reporting.
            //
            // What still reads the renderer's own step arrays in .NET mode - the schematic probe
            // and the RAM tables - therefore reads a simulation that has not been run. Those move
            // to the sidecar next; showing them stale is better than running a 4,000,000 cycle
            // simulation twice to keep them right.
            (if model.SimulateInRenderer then
                 FastRun.runFastSimulation (Some Constants.initSimulationTime) (lastCycleNeeded wsModel) fs
             else
                 RunCompleted)
            |> (fun outcome ->
                    match outcome with
                    | RunStoppedAt clock ->
                        // One budget was not enough, so this run is long enough to be worth saying
                        // so - and that budget IS the delay before a progress bar appears. Nothing
                        // is estimated: this used to divide the cycles left by a measured rate to
                        // guess how much longer, which is inferring elapsed time from work done,
                        // and is what collapses when a machine sleeps mid-run.
                        runSimulationWithSpinner (lastCycleNeeded wsModel - clock) model
                    | RunCompleted ->
                        // Completed means completed. A second forced run "in case it is not
                        // finished" used to follow, from when the outcome could not say which.

                        // Simulation has now always finished so we can generate the waves
                        // this again may need to be done in a spinner if it takes too long.
                        // That decision is made below with the help of makeWaveformsWithTimeOut.

                        // Validate and update all parameters affecting waveforms.
                        let model =
                            updateViewerWidthInWaveSim model.WaveSimViewerWidth model
                            // cancel any spinner so that when a new one is started
                            // it will have teh correct total number of steps to do.
                            //|> (fun model -> {model with SpinnerPayload = None})
                        let wsModel =
                            getWSModel model
                        
                        // Has the simulator been run far enough for what is now being drawn?
                        //
                        // Asked of the simulator that is RUNNING. The renderer's own answers with its
                        // clock tick; the sidecar has been run to the last cycle this view needs as
                        // part of fetching it, and `covers` above is what says that fetch has landed.
                        // Reading the renderer's clock tick while the sidecar simulates asks a
                        // simulation that is never run whether it has run - always no, and the branch
                        // below calls this function again, which is an unbounded recursion inside one
                        // message and locks the renderer up with no way back.
                        let simulationIsStillUptodate =
                            not model.SimulateInRenderer
                            || Simulator.getFastSim().ClockTick >= lastCycleNeeded wsModel

                        // Rendering can change the viewer's width - the panel divider moves, the
                        // window is resized - and the width decides how many cycles are shown. When
                        // it does, this refresh asked the simulator for a view that is no longer the
                        // one being drawn, and what the sidecar sends back is one exact window that
                        // covers no other. Everything a SELECTION changes is settled before the ask,
                        // above, so this is left for what only a render can tell us.
                        //
                        // It cannot loop: it re-enters only when the view has ALREADY changed, and
                        // the next pass asks for the view it now has.
                        let viewIsUnchanged = windowOf wsModel = window

                        if not simulationIsStillUptodate || not viewIsUnchanged then
                            // The simulation or the view has changed under this refresh. Do it again
                            // for what we now have; we come back here when both are current.
                            refreshWaveSim newSimulation wsModel model
                        else
                            // Read again rather than reusing what refreshWaveSim bound: this runs
                            // after a render, and re-resolving a selection against the simulation is
                            // exactly the case where it may not be the one we started with.
                            let fs = Simulator.getFastSim()

                            // Again, because this runs after a render: the viewer width check just
                            // above can change the view, and a rebuild can land between the two.
                            let wsModel = reconcileWaves fs wsModel
                            let selectedWaves = wsModel.SelectedWaves

                            // Only generate waveforms for selected waves, and only where the SVG they
                            // hold is not the one the current view calls for.
                            let wavesToBeMade =
                                selectedWaves
                                |> List.filter (fun wi ->
                                    match Map.tryFind wi wsModel.WaveDetails with
                                    | Some wave -> not <| WaveSimSVGs.waveformIsUptodate wsModel wave
                                    | None -> false)
                            if wsModel.StartCycle < 0 then
                                failwithf $"Sanity check failed: wsModel.StartCycle = {wsModel.StartCycle}"
                            let spinnerInfo =  
                                let numToDo = wavesToBeMade.Length
                                WaveSimSVGs.makeWaveformsWithTimeOut (Some <| Constants.initWaveformTime ) wsModel  wavesToBeMade
                                |> (fun res ->
                                        match wavesToBeMade.Length - res.NumberDone, res.TimeTaken with
                                        | _, None | 0, _-> 
                                            {| WSM=res.WSM; SpinnerPayload=None; NumToDo=numToDo|} // finished
                                        | numToDo, Some t when float numToDo * t / float res.NumberDone < Constants.maxWaveCreationTimeWithoutSpinner ->
                                            let res2 = WaveSimSVGs.makeWaveformsWithTimeOut None res.WSM wavesToBeMade
                                            {| WSM= res2.WSM; SpinnerPayload=None; NumToDo = numToDo - res2.NumberDone|}
                                        | numToDo, _ ->
                                            if res.NumberDone = 0 && numToDo > 0 then
                                                Log.warn $"no waves completed when {numToDo} are required - retrying refreshWaveSim"
                                            let payload = Some ("Updating Waveform Display", refreshWaveSim false res.WSM >> fst)
                                            {| WSM=res.WSM; SpinnerPayload=payload; NumToDo=numToDo|})

                            let ramCompIds = ramCompIdsOf fs
                            let ramCompIdSet = Set.ofList ramCompIds
                            let selectedRams = Map.filter (fun ramfId _ -> Set.contains ramfId ramCompIdSet) wsModel.SelectedRams

                            let ws =  
                                {
                                    wsModel with
                                        State = Success
                                        WaveDetails = spinnerInfo.WSM.WaveDetails
                                        SelectedWaves = selectedWaves
                                        RamComps = ramCompIds
                                        SelectedRams = selectedRams
                                }

                            let model = putWaveSim ws model

                            match spinnerInfo.SpinnerPayload with
                            | None ->
                                cancelSpinner model
                                |> dispatchFocusAfterRender
                            | Some (spinnerName, spinnerAction) -> 
                                setButtonSpinner (fun _dispatch model -> spinnerAction model)  model
                            |> updateWSModel (fun _ -> {ws with DefaultCursor = Default})
                            |> (fun model -> model, Elmish.Cmd.none))

/// Refresh the state of the wave simulator according to the model and canvas state.
/// Redo a new simulation. Set inputs to default values. Then call refreshWaveSim via RefreshWaveSim message.
/// 1st parameter ofrefreshWaveSin will be set true which causes all waves to be necessarily regenerated.
let refreshButtonAction canvasState model dispatch = fun _ ->
    let startWaveSimulation dispatch model =
        /// update the model memories to match any updated linked initial contents files
        let model = MemoryEditorView.updateAllMemoryComps model
        // the simulation is about to use these contents, so say if a linked file did not load
        MemoryEditorView.notifyMemoryFileErrors dispatch model
        let wsSheet =
            match model.WaveSimSheet with
            | None ->
                Option.get (getCurrFile model)
            | Some sheet ->
                sheet
        let model = 
            model
            |> removeAllSimulationsFromModel
            |> fun model -> {model with WaveSimSheet = Some wsSheet}
        let wsModel =
            getWSModel model
            |> fun wsModel -> {wsModel with ScrollbarBkgRepCycs= Constants.scrollbarBkgRepCyclesInit}
            // A simulation being started at the untouched default clock count is given one its
            // design can be started in - an explicitly configured count is honoured exactly, see
            // startingLastClock. Only on starting: a Refresh of a running simulation keeps what
            // the configuration says.
            |> fun wsModel ->
                match wsModel.State with
                | Success -> wsModel
                | _ ->
                    let estimate = ModelHelpers.simulationHeapEstimate model.WaveSimSheet canvasState model
                    let lastClock = ModelHelpers.startingLastClock wsModel.WSConfig.LastClock estimate
                    if lastClock = wsModel.WSConfig.LastClock then
                        wsModel
                    else
                        let message =
                            $"This design is large, so its waveform simulation starts at {lastClock} clock cycles \
                              rather than the default {wsModel.WSConfig.LastClock}. Set the cycle count in \
                              Configure to simulate more."
                        Log.warn message
                        // in the UI as well as the log: the console is not where the user who
                        // wonders why the viewer stops at 200 cycles will be looking
                        dispatch <| SetSimulationNotification (Notifications.warningSimNotification message)
                        Optic.set (wSConfig_ >-> lastClock_) lastClock wsModel
        let simRes =
            // Here is where the new fast simulation is created
            ModelHelpers.simulateModel
                true
                model.WaveSimSheet
                (Constants.waveSimRequiredArraySize wsModel)
                canvasState model

        match simRes with
        | (Error e, _) ->
            dispatch <| SetWSModelAndSheet ({ wsModel with State = SimError e }, wsSheet)
        | (Ok simData, canvState) ->
            if simData.IsSynchronous then
                SimulationView.setFastSimInputsToDefault simData.FastSim
                let wsModel = { wsModel with State = Loading}
                dispatch <| SetWSModelAndSheet (wsModel, wsSheet)
                dispatch <| RefreshWaveSim wsModel
                dispatch <| UpdateWSModel (fun wsModel -> {wsModel with  DefaultCursor = Default})
            else
                dispatch <| SetWSModelAndSheet ({ wsModel with State = NonSequential}, wsSheet)
    dispatch <| RunAfterRender(true, fun dispatch model -> startWaveSimulation dispatch model; model)
    





/// ReactElement showing instructions and wave sim buttons
let topHalf canvasState (model: Model) dispatch : ReactElement * bool =
    let title =
        match model.WaveSimSheet with
        | None -> "Waveform Viewer for:", model.WaveSimOrCurrentSheet
        | Some sheet -> "Simulating:", sheet
        |> fun (text,sheet) ->
            div [Style [WhiteSpace WhiteSpaceOptions.Nowrap]]
                [str text  ; span [Style [Color "#3e8ed0"; MarginLeft "5px"]] [str $"{sheet}"]]
    let wsModel = getWSModel model
    let loading =
        match wsModel.State with
        | Loading -> true
        | _ -> false

    let titleLine() =       
        div [ Style [
                inlineNoWrap;
                MarginBottom (if model.WaveSimSheet = None then "50px" else "10px")
                FontSize "24px"
                LineHeight "24px"
                FontWeight 600
                OverflowX OverflowOptions.Clip ;
                Display DisplayOptions.Inline;
                Height Constants.rowHeight
                Flex "0 0.5"
                AlignSelf AlignSelfOptions.FlexStart
                MarginRight 5
                MarginLeft 5
                ];

              Id "WaveSimHelp"]
              [
                title
              ]

    let refreshStartEndButton() =
        let refreshButtonSvg = if loading then emptyRefreshSVG else refreshSvg "white" "20px"
        /// This is the only action for creating a new (or changed) waveform simulator fast simulation
        /// Once a simulation is created is maxClkCycle is fixed and cannot be changed. However the length
        /// of the simulation can be changed by extending the simulation to any value less than its maxClkCycle.
        let startOrRenew model =
            refreshButtonAction canvasState model dispatch

        let waveEnd model = endButtonAction canvasState model dispatch
        let wbo = getWaveSimButtonOptions canvasState model wsModel dispatch
        let isLoading =
            match model.RunAfterRenderWithSpinner, model.SpinnerPayload with
            | Some {ButtonSpinnerOn = true}, _ -> true
            | _ , Some _ -> true
            | _ -> false
        let startEndButton =
            button 
                (topHalfButtonPropsLoading isLoading wbo.StartEndColor "startEndButton" false) 
                (fun ev -> dispatch <| ExecFuncInMessage((fun model _ ->
                                if wbo.IsRunning then waveEnd model ev  else startOrRenew model ev),dispatch)
                           dispatch <| ExecFuncInMessage ((fun model _ -> ()), dispatch))
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

    let needsBottomHalf, messageOrControlLine =
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

        | Some sheet, _ when Simulator.getFastSim().SimulatedTopSheet = "" ->
            notRunning
        
        | _,NoProject ->
            false, div [ errorMessageStyle ] [ str "Please open a project to use the waveform viewer." ]

        | _, (Loading | Success) when List.isEmpty wsModel.SelectedWaves && Map.isEmpty wsModel.SelectedRams->
            false, div [Id "WaveSimHelp"] [str "Use 'Select Waves' to add waves for simulation. Right-click for help."]

        | _, Success ->
            true, div [Style [Height Constants.rowHeight; Display DisplayOptions.Flex; JustifyContent "space-between"; Margin "5px"; MarginTop "30px" ; MarginBottom "15px"]]  [

                        zoomButtons wsModel dispatch

                        multiplierMenuButton wsModel dispatch
                        
                        WaveSimWaveforms.radixButtons wsModel dispatch
  
                        clkCycleButtons wsModel dispatch
                    ]
        | _ -> notRunning


    div [ topHalfStyle ] [
        div [Style [MarginTop 20.; Display DisplayOptions.Flex; JustifyContent "space-between"]] [
            titleLine()
            UIPopups.makeWSConfigButton dispatch model
            waveInfoButton (match wsModel.State with | Success -> "Instructions" | _ ->"Getting Started") dispatch
        ]
 
        div [Style [MarginTop 15.; Display DisplayOptions.Flex; JustifyContent "space-between"]] [
            refreshStartEndButton()
            div [Style [inlineNoWrap; Flex "0 1"]] [
                WaveSimSelect.selectWavesButton wsModel dispatch
                WaveSimSelect.selectRamButton wsModel dispatch]
            ]
        
        messageOrControlLine], needsBottomHalf
        
        

/// Entry point to the waveform simulator.
let viewWaveSim canvasState (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model

    let top, needsBottomHalf = topHalf canvasState model dispatch
    let needsRAMs = not <| Map.isEmpty wsModel.SelectedRams
    let height = calcWaveformAndScrollBarHeight wsModel
    let bottomHalf() = // this has fixed height
        div [HTMLAttr.Id "BottomHalf" ; showWaveformsAndRamStyle (if needsRAMs then screenHeight() else height)] (
            if wsModel.SelectedWaves.Length > 0 then [
                WaveSimWaveforms.showWaveforms model wsModel dispatch               
                makeScrollbar wsModel dispatch ]
            else []
            @
            [WaveSimRams.ramTables dispatch wsModel model] 
        )
    div [
        Style [
            OverflowX OverflowOptions.Clip;
            Cursor <| wsModel.DefaultCursor.Text()
        ]
    ] [
        WaveSimSelect.selectRamModal wsModel dispatch
        WaveSimSelect.selectCompWavesModal wsModel dispatch
        WaveSimSelectHelpers.selectWavesModal wsModel dispatch model
        div [ viewWaveSimStyle ]
            [
                top
                if needsBottomHalf &&
                   (match model.SpinnerPayload with | Some {UseProgressBar=true  } -> false | _ -> true)
                then bottomHalf()
                else div [] []
            ]
        
    ]

