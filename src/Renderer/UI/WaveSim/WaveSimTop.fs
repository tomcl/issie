
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

    // For one simulation the circuit is frozen, so resolution can only change at a build
    // boundary - which is when a wave in an instance the sidecar has not yet described comes
    // through UNRESOLVED (SimArrayIndex = -1) rather than dropped. Such a wave gets no record
    // built: there is nothing to build one from until the slice lands, and the next refresh
    // after it does resolves the wave and builds it then.
    let details =
        selected
        |> List.choose (fun wi ->
            match Map.tryFind wi ws.WaveDetails with
            | Some wave -> Some(wi, wave)
            | None when wi.SimArrayIndex >= 0 -> Some(wi, WaveSimHelpers.makeWave ws fs wi)
            | None -> None)
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

let private isMemory (comp: Component) =
    match comp.Type with
    | RAM1 _ | ROM1 _ | AsyncRAM1 _ | AsyncROM1 _ -> true
    | _ -> false

/// Every memory instance, with the name the selector lists it under, in that order.
///
/// Which memories a design has is a fact about the DESIGN - a memory drawn on a sheet is a memory
/// in every instance of that sheet - so it is worked out from the design, following only the
/// subtrees that hold one. It used to filter and sort every FastComponent in the expansion, which
/// on main6 of largeTest is about 480,000 records, to find a handful.
///
/// The name comes with it. It used to be looked up per row, in the expansion-sized map, by the
/// modal drawing the list - which is the only reason that map was reachable from the selector at
/// all - and it is the key the saved RAM selection uses, so it has to be the same string
/// FastComponent.FullName held.
let private ramCompIdsOf: FastSimulation -> (FComponentId * string) list =
    Helpers.memoizeByIdentity (fun (fs: FastSimulation) ->
        fs.Design.InstancesOfComponents isMemory
        |> List.map (fun (comp, InstancePath ap as pair) ->
            (ComponentId comp.Id, ap), fs.Design.FullNameOf pair)
        |> List.sortBy snd)

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
        // Reconciled BEFORE the default selection is chosen, not after. A selection saved with the
        // sheet can name ports the design no longer has - it was saved by an older version of the
        // design, or of Issie - and reconciling drops those. Choosing defaults first meant a
        // selection that resolved to nothing still counted as a selection, so nothing replaced it
        // and the viewer opened empty: the one thing chooseDefaultWaves exists to prevent, in
        // exactly the case where the user has no way to know why.
        validateSimParas wsModel
        |> reconcileWaves (Simulator.getFastSim())
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

    // A RAM's rows are of a cycle of a session; a new simulation is a new session, and rows kept
    // across it would be drawn under the new one's clock. Cleared here, where the new wsModel is
    // put into the model, rather than beside WaveDrawn.forget below - these live in the model.
    // A new simulation is a new session: rows kept across it would be drawn under the new one's
    // clock, and a read still out belongs to the session that has gone - its reply is dropped, so
    // clear the flag here rather than wait for one that will not count.
    let wsModel = if newSimulation then { wsModel with RamRows = Map.empty } else wsModel
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

    // Only of the simulator that is RUNNING. The renderer's own arrays are sized for the run when it
    // is simulating, and for nothing at all when the sidecar is - so past the first few hundred
    // cycles this would refuse every view of a .NET simulation, which is exactly the case the small
    // arrays exist for.
    if model.SimulateInRenderer && cycleLimit >= fs.MaxArraySize then
        failwithf $"Sanity check failed: lastCycleNeeded = {cycleLimit} >= fs.MaxArraySize = {fs.MaxArraySize}"

    // Whether there is a simulation to draw from. The renderer's own build answers with its
    // arrays; the sidecar's answers with the design the carrier holds, because in that mode the
    // local array count is zero by construction, however real the simulation.
    let nothingBuilt =
        if model.SimulateInRenderer then fs.NumStepArrays = 0 else fs.SimulatedTopSheet = ""

    if nothingBuilt then
        // There is no simulation to draw from. That is not a quiet case to skip: the viewer is
        // showing waveforms, this refresh has just recorded that the view changed, and returning
        // here leaves those waveforms drawn for a view that will now never be fetched - silently,
        // and for as long as the user keeps moving the cursor rather than the window, since only a
        // window move refreshes again.
        //
        // It happens when a build failed after a simulation was already on screen, which in
        // practice means running out of memory for the step arrays. The renderer's own are sized
        // for what it will read - a few hundred cycles when the sidecar is simulating - so what
        // remains is a design too big to hold at the cycle count it is configured for, in the mode
        // that holds it here.
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
            let spinnerFunc = fun dispatch model ->
                let wsModel = getWSModel model
                refreshAndIssue dispatch false wsModel model
            let model =
                model
                |> setProgressBar $"Extending Circuit Simulation..." spinnerFunc cyclesToDo
            model, Elmish.Cmd.none
 
        /// The view this refresh is for.
        let window = windowOf wsModel

        /// The drivers the shown waves read. Taken from the selection as it stands: after a rebuild
        /// it may name indices that no longer exist, which the re-resolution below corrects - and
        /// the waves then have no data under their new indices and are simply asked for.
        let driversOf (ws: WaveSimModel) =
            ws.SelectedWaves
            |> List.map (fun wi -> wi.SimArrayIndex)
            // Negative means unresolved - nothing to read. The upper bound is only the renderer's
            // own driver table; a handle in sidecar mode is valid because the build's slices
            // issued it, and the local table is empty by construction.
            |> List.filter (fun i -> i >= 0 && (not model.SimulateInRenderer || i < fs.Drivers.Length))

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

        // What is drawn was drawn from the simulation that has just been replaced. A waveform is
        // remembered against its driver index, and a driver index names a different signal in the
        // next build, so every one of them is now a picture of something else - which would be
        // drawn, under the new signal's name, for as long as the view did not change.
        if newSimulation then
            WaveDrawn.forget ()


        // What this refresh is working from, in one line: the view, what is selected, what is known
        // about it, and how much of it is here. The waveform viewer's failures are almost always a
        // disagreement between two of those.
        let missing =
            WaveProvider.wavesToFetch model.SimulateInRenderer (List.map SignalHandle (driversOf wsModel)) window

        let heldOrNot =
            match missing, ModelHelpers.sidecarFetchInFlight model with
            | [], _ -> "held"
            | m, true -> $"{m.Length} waves missing, a fetch is already running"
            | m, false -> $"{m.Length} waves to fetch"

        Log.dbg
            Log.Wave
            $"refresh: view {window.StartSample}+{window.SampleCount}x{window.Multiplier} cursor {wsModel.CursorExactClkCycle}, {wsModel.SelectedWaves.Length} selected, {wsModel.WaveDetails.Count} detailed - {heldOrNot}"

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

                        if wsModel.StartCycle < 0 then
                            failwithf $"Sanity check failed: wsModel.StartCycle = {wsModel.StartCycle}"

                        // Waveforms are not made here. They are made in the view, from the data as
                        // it is at the moment of drawing, and memoised on exactly what they are a
                        // function of - so there is nothing for this refresh to bring up to date
                        // and nothing to time out part way through. What used to be here was a pass
                        // over the selection asking each wave whether its SVG matched the current
                        // view, remaking those that did not, and a spinner for when that took too
                        // long: an update deciding what the screen should look like, from a copy of
                        // the screen kept in the model.
                        //
                        // Keeping the memo to the selection is this refresh's business, because
                        // this is where the selection settles.
                        WaveDrawn.keepOnly (selectedWaves |> List.map (fun wi -> wi.SimArrayIndex) |> Set.ofList)

                        let ramCompIds = ramCompIdsOf fs
                        let ramCompIdSet = ramCompIds |> List.map fst |> Set.ofList
                        let selectedRams = Map.filter (fun ramfId _ -> Set.contains ramfId ramCompIdSet) wsModel.SelectedRams

                        let ws =  
                            {
                                wsModel with
                                    State = Success
                                    SelectedWaves = selectedWaves
                                    RamComps = ramCompIds
                                    SelectedRams = selectedRams
                            }

                        let model = putWaveSim ws model

                        cancelSpinner model
                        |> dispatchFocusAfterRender
                        |> updateWSModel (fun _ -> {ws with DefaultCursor = Default})
                        |> (fun model -> model, Elmish.Cmd.none))

/// Refresh, and issue the command the refresh returns, using a dispatch we already have.
///
/// Spinner continuations can only return a model, and a refresh returns a model AND a command -
/// which is how the next fetch is asked for, and which the model records as in flight before it is
/// issued. Dropping it would leave the viewer waiting for a fetch that was never made. A command is
/// a list of functions of dispatch, and these continuations are given one, so run it here.
and private refreshAndIssue (dispatch: Msg -> unit) (newSimulation: bool) (ws: WaveSimModel) (model: Model) =
    let model, cmd = refreshWaveSim newSimulation ws model
    cmd |> List.iter (fun sub -> sub dispatch)
    model

/// What the view being drawn wants from the .NET simulator and has not got.
///
/// Everything in one record because everything goes in one operation: whichever of these a pass
/// finds missing, it asks for together, in order, in one promise, under one entry in the in-flight
/// table. One round trip and one message however many of them there are.
type private Missing =
    { /// the cycle the session must have reached before any of it can be read
      MissCycle: int
      /// the cycle a run should aim FOR - MissCycle plus prefetch. The waveform viewer runs one
      /// window ahead of the one being read, so that stepping forward finds the next window
      /// already simulated: one longer run instead of an annoying gap on every step. A user
      /// affordance, not a correctness need, and deliberately one expression to adjust. Reads do
      /// not wait for it - a chunk that reaches MissCycle unblocks the fetch, and whatever the
      /// chunk ran beyond it is the prefetch banked.
      MissRunTo: int
      /// how many cycles of arrays the sidecar's build must hold
      MissArraySize: int
      /// the design to build, if it is not built
      MissDesign: SimpleDesign
      /// driver indices of waveforms that have not got the window they are drawn over
      MissWaves: WaveIndexT list
      MissWindow: WaveSlice.Window
      /// at most one RAM's rows. ONE, because the next pass takes the next: a round trip is a
      /// fifth of a millisecond, so this is not slower in any way a user could see
      MissRam: (FComponentId * RamView.RamKey) option
      /// the step panel's signals, when it is showing a cycle it has no values for
      MissPanel: (int * StepPanelData.PanelSignal list) option
      /// the wire the pointer is resting on, when the model has no value for exactly it
      MissProbe: (WaveIndexT * int) option }

/// What the waveform viewer is missing, or None when it is not the thing on screen.
let private missingForWaves (model: Model) (project: Project) : Missing option =
    let ws = model.WaveSimSheet |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim)

    /// Long enough after a failure to be worth trying again, rather than as fast as the message
    /// queue will carry it.
    let backedOff (ws: WaveSimModel) =
        ws.FetchFailedAtMs > 0.0
        && TimeHelpers.getTimeMs () - ws.FetchFailedAtMs < Constants.fetchRetryAfterMs

    match ws with
    | Some ws when ws.State = Success && not (backedOff ws) ->
        let fs = Simulator.getFastSim ()

        let window: WaveSlice.Window =
            { StartSample = ws.StartCycle
              Multiplier = ws.SamplingZoom
              SampleCount = ws.ShownCycles }

        /// What each memory on screen is reading, cycle by cycle: the address input of every
        /// selected memory, and of every ROM whose data output is drawn and whose .ram file has
        /// comments.
        ///
        /// Fetched with the waveforms rather than asked for on demand, because both the things
        /// that want it happen where nothing can wait - the read marker on a ROM's rows, drawn in
        /// a render, and the .ram comment tooltip, written into the DOM by a mouse handler. One
        /// signal per memory, in the request that is going out anyway.
        ///
        /// It is also what lets a ROM's rows be fetched once and shown at every cycle: the marker
        /// moves without them, so scrolling and stepping cost no memory fetch at all.
        let addressWaves =
            let selectedMemories =
                ws.SelectedRams
                |> Map.toList
                |> List.choose (fun (ramId, _) -> EvilHoverCache.addressWaveOf fs ramId)

            let drawnRoms =
                WaveSimStyle.selectedWaves ws
                |> List.choose (fun wave -> EvilHoverCache.romAddressOf fs wave |> Option.map fst)

            selectedMemories @ drawnRoms

        // The waves themselves and not their array indices: an index says where a wave's data
        // lies in the build that answered, which is what the CACHE is keyed by, but naming a port
        // is what lets a simulator holding the data find it. Both are wanted, so the wave - which
        // carries both - is what travels.
        let toFetch =
            let candidates =
                ws.SelectedWaves
                |> List.append addressWaves
                // negative is unresolved; the local driver table bounds nothing here, because in
                // this mode the handles were issued by the sidecar's own slices
                |> List.filter (fun wi -> wi.SimArrayIndex >= 0)
                |> List.distinctBy (fun wi -> wi.SimArrayIndex)

            let missing =
                candidates
                |> List.map (fun wi -> SignalHandle wi.SimArrayIndex)
                |> fun handles -> WaveProvider.wavesToFetch model.SimulateInRenderer handles window
                |> List.map (fun (SignalHandle i) -> i)
                |> Set.ofList

            candidates |> List.filter (fun wi -> Set.contains wi.SimArrayIndex missing)

        let ram =
            ws.SelectedRams
            |> Map.toList
            |> List.filter (fun (ramId, _) -> RamData.needed model ramId)
            |> List.tryHead
            |> Option.map (fun (ramId, _) -> ramId, RamData.keyOf model ramId)

        if fs.SimulatedTopSheet = "" then
            None
        else
            // What the SIDECAR allocates, from the configuration - not from the renderer's own
            // arrays, which in this mode are sized for their structure and hold two cycles
            // whatever the configuration says.
            let arraySize = ModelHelpers.Constants.waveSimRequiredArraySize ws

            let readableAt =
                ram
                |> Option.map (fun (_, key) -> key.Cycle)
                |> Option.defaultValue 0
                |> max (window.LastCycle + 1)

            Some
                { MissCycle = readableAt
                  // never below MissCycle: at the end of the configured run readableAt can pass
                  // LastClock, and a run target short of the gate would run forever reaching it
                  MissRunTo = max readableAt (min ws.WSConfig.LastClock (readableAt + window.SampleCount * window.Multiplier))
                  MissArraySize = arraySize
                  MissDesign =
                    ModelHelpers.designOf project (model.Sheet.GetCanvasState())
                    |> CanvasExtractor.simpleDesignOfLoadedComponents
                    |> fun d -> { d with TopSheet = fs.SimulatedTopSheet }
                  MissWaves = toFetch
                  MissWindow = window
                  MissRam = ram
                  MissPanel = None
                  MissProbe = None }
    | _ -> None

/// What the step simulator's panel is missing, or None when it is not the thing on screen.
///
/// The panel shows the cycle the model says it is showing, and holds values for whatever cycle it
/// last read. When those differ it needs a read - which is the same shape as "this wave has not got
/// the window it is drawn over", derived the same way, and now asked for by the same mechanism. It
/// used to be a promise of its own inside advanceTo, which gave the step simulator a second way of
/// talking to the sidecar that had to be kept in step with this one.
let private missingForPanel (model: Model) (project: Project) : Missing option =
    match model.CurrentStepSimulationStep with
    | Some(Ok simData) when simData.FastSim.SimulatedTopSheet <> "" ->
        let cycle = simData.ClockTickNumber

        if StepPanelData.cycleHeld () = Some cycle then
            None
        else
            let signals = SimulationView.panelSignals simData

            if List.isEmpty signals then
                None
            else
                Some
                    { MissCycle = cycle
                      // no prefetch: the step simulator's next cycle is one click away and cheap
                      MissRunTo = cycle
                      MissArraySize =
                        SimulationView.stepSimArraySize model
                        |> Result.defaultValue SimulationView.Constants.maxArraySize
                      MissDesign =
                        ModelHelpers.designOf project (model.Sheet.GetCanvasState())
                        |> CanvasExtractor.simpleDesignOfLoadedComponents
                        |> fun d -> { d with TopSheet = simData.FastSim.SimulatedTopSheet }
                      MissWaves = []
                      MissWindow =
                        { StartSample = cycle
                          Multiplier = 1
                          SampleCount = 1 }
                      MissRam = None
                      MissPanel = Some(cycle, signals)
                      MissProbe = None }
    | _ -> None

/// Make what the view is drawing true, one operation at a time.
///
/// **The one place a request to the .NET simulator is decided**, and it decides from the model and
/// the caches alone: what the view being drawn has not got, and whether something is already in the
/// air. Nothing records that a fetch is owed and no message has to remember to ask for one - which
/// is why this is at the end of `update` rather than at the end of a refresh. A refresh is not the
/// only thing that can leave a view short of its data: a cursor move inside the window does not
/// refresh, a step moves the panel's clock, and a fetch that failed while the sidecar was still
/// starting leaves everything missing.
///
/// **Build, then run a chunk at a time, then read** - each an operation, each answered by a
/// message, and the next decided here by looking at the model. Nothing is ever interrupted: what a
/// cancel changes is what this asks for next. Cancelling a run puts the cursor back on the cycle
/// the session has reached (Update.CancelWaveSimulation), so the next pass finds it already far
/// enough and stops asking for chunks. There is no cancellation on the wire because none is needed.
///
/// Cheap enough to run on every message: a map lookup per drawn wave, of which there are at most a
/// hundred, and a comparison per other thing.
let fetchWhatIsMissing (model: Model, cmd: Elmish.Cmd<Msg>) : Model * Elmish.Cmd<Msg> =
    match model.CurrentProj with
    | None -> model, cmd
    | _ when model.SimulateInRenderer -> model, cmd
    // something is already outstanding; its answer brings us back here
    | _ when ModelHelpers.sidecarIsBusy model -> model, cmd
    | Some project ->
        // The two simulators are mutually exclusive, so at most one of these is ever Some - but
        // they are asked in one place and answered by one mechanism, which is the point.
        /// The wire the pointer is resting on, if the model has no value for exactly it. Asked
        /// with whatever else is missing rather than separately: hovering is occasional and the
        /// question does not change while the pointer sits still, so this is one more thing in the
        /// promise on the pass that first notices it.
        let probe =
            WaveSimSelect.probeQuestion model
            |> Option.filter (fun (_, wi, cycle, _) ->
                (WaveSimSelect.heldProbeValue model wi cycle).IsNone)
            |> Option.map (fun (_, wi, cycle, _) -> wi, cycle)

        match
            missingForWaves model project
            |> Option.orElseWith (fun () -> missingForPanel model project)
            |> Option.map (fun miss -> { miss with MissProbe = probe })
        with
        | None -> model, cmd
        | Some miss ->

        /// Nothing fetchable right now. That must not skip the BUILD below: with the sidecar
        /// simulating, a freshly loaded selection is entirely unresolved until the build's slices
        /// arrive, so at the very moment a build is most needed there is nothing to fetch - the
        /// early-out that stood here kept the session unbuilt for ever, quietly showing nothing.
        let nothingToRead =
            List.isEmpty miss.MissWaves
            && miss.MissRam.IsNone
            && miss.MissPanel.IsNone
            && miss.MissProbe.IsNone

        /// Everything this pass wants read, in order, in one promise. The session has already been
        /// run far enough - that is a separate operation, sequenced below - so nothing here
        /// commands it.
        let readAll epoch () =
            promise {
                let! waves =
                    if List.isEmpty miss.MissWaves then
                        Promise.lift (Ok())
                    else
                        WaveProvider.fetchWavesFor epoch (Simulator.getFastSim ()) miss.MissWaves miss.MissWindow

                let! rows =
                    match miss.MissRam with
                    | None -> Promise.lift None
                    | Some(ramId, key) -> RamData.fetch epoch ramId key WaveSimTypes.Constants.maxRamRowsDisplayed

                match miss.MissPanel with
                | None -> ()
                | Some(cycle, signals) ->
                    match! StepPanelData.fill epoch cycle signals with
                    | Error e -> Log.warn $"reading the step panel at cycle {cycle}: {e}"
                    | Ok() -> ()

                let! probed =
                    match miss.MissProbe with
                    | None -> Promise.lift None
                    | Some(wi, cycle) ->
                        WaveProvider.fetchProbeValue epoch (Simulator.getFastSim ()) wi cycle
                        |> Promise.map (Option.map (fun value -> wi, cycle, value))

                return waves, rows, probed
            }

        match model.SidecarSession with
        | session when not (session.Holds(miss.MissDesign.TopSheet, miss.MissArraySize)) ->
            Log.dbg
                Log.Wave
                $"building {miss.MissDesign.TopSheet} on the .NET simulator for {miss.MissArraySize} cycles"

            let seq = ModelHelpers.newSeq ()
            let startBuild () = SidecarSession.build miss.MissDesign miss.MissArraySize

            model
            |> Optic.map sidecarInFlight_ (Map.add seq (OpBuild(miss.MissDesign.TopSheet, miss.MissArraySize))),
            Elmish.Cmd.batch
                [ cmd
                  Elmish.Cmd.OfPromise.either
                      startBuild
                      ()
                      (fun result -> SidecarReply(seq, AnsBuilt result))
                      (fun exn -> SidecarReply(seq, AnsBuilt(Error exn.Message))) ]

        | session when session.Clock < miss.MissCycle ->
            let seq = ModelHelpers.newSeq ()
            let epoch = Option.get session.Epoch
            let runOne () = SidecarSession.runChunk epoch miss.MissRunTo

            model |> Optic.map sidecarInFlight_ (Map.add seq (OpRunForWaves miss.MissCycle)),
            Elmish.Cmd.batch
                [ cmd
                  Elmish.Cmd.OfPromise.either
                      runOne
                      ()
                      (fun result -> SidecarReply(seq, AnsRan result))
                      (fun exn -> SidecarReply(seq, AnsRan(Error exn.Message))) ]

        | _ when nothingToRead -> model, cmd

        | session ->
            let alsoRam =
                match miss.MissRam with
                | None -> ""
                | Some(_, key) -> $", one RAM at cycle {key.Cycle}"

            let alsoPanel =
                match miss.MissPanel with
                | None -> ""
                | Some(cycle, signals) -> $", {signals.Length} panel signals at cycle {cycle}"

            let alsoProbe =
                match miss.MissProbe with
                | None -> ""
                | Some(_, cycle) -> $", the probe's wire at cycle {cycle}"

            let view =
                $"{miss.MissWindow.StartSample}+{miss.MissWindow.SampleCount}x{miss.MissWindow.Multiplier}"

            Log.dbg Log.Wave $"fetching {miss.MissWaves.Length} waves over {view}{alsoRam}{alsoPanel}{alsoProbe}"

            let seq = ModelHelpers.newSeq ()

            let fetch =
                Elmish.Cmd.OfPromise.either
                    (readAll (Option.get session.Epoch))
                    ()
                    (fun (waves, rows, probed) -> SidecarReply(seq, AnsFetched(waves, rows, probed)))
                    (fun exn -> SidecarReply(seq, AnsFetched(Error exn.Message, None, None)))

            model
            |> Optic.map
                sidecarInFlight_
                (Map.add seq (OpFetch(miss.MissWaves.Length, miss.MissRam |> Option.map fst))),
            Elmish.Cmd.batch [ cmd; fetch ]

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
        // The two simulations are mutually exclusive, so starting this one ends the step
        // simulation. As a MESSAGE, because the model built below is a local copy used to work out
        // the new WaveSimModel and is never dispatched - removeAllSimulationsFromModel applied to
        // it cleared a step simulation that stayed in the real model, which is how one could
        // survive the start of a waveform simulation and leave two of them live at once.
        dispatch EndSimulation

        let model =
            model
            |> removeAllSimulationsFromModel
            |> fun model -> {model with WaveSimSheet = Some wsSheet}
        let wsModel =
            getWSModel model
            |> fun wsModel -> {wsModel with ScrollbarBkgRepCycs= Constants.scrollbarBkgRepCyclesInit}
            // A simulation that is being STARTED starts at the beginning. The WaveSimModel outlives
            // the simulation it was made for - ending one leaves it in the map, marked Ended, so
            // that the selection and the configuration survive - and the cursor and the scroll
            // position were surviving with them, which put a brand new simulation's cursor wherever
            // the last one happened to be left. A Refresh keeps its place, which is the point of a
            // Refresh.
            |> fun wsModel ->
                match wsModel.State with
                | Success -> wsModel
                | _ ->
                    { wsModel with
                        StartCycle = 0
                        CursorDisplayCycle = 0
                        CursorExactClkCycle = 0 }
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
            // Here is where the new fast simulation is created.
            //
            // Sized for what THIS process will read from it, which where the .NET simulator is
            // simulating is none of it: the waveforms come off the wire, and the renderer's copy is
            // built for its structure. What the sidecar allocates is a separate decision, made from
            // the configuration where the fetch is issued.
            let arraySize =
                if model.SimulateInRenderer then
                    Constants.waveSimRequiredArraySize wsModel
                else
                    min (Constants.waveSimRequiredArraySize wsModel) Constants.rendererArraySizeWhenSidecarSimulates

            ModelHelpers.simulateModel model.SimulateInRenderer true model.WaveSimSheet arraySize canvasState model

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
    let bottomHalf () = // the space under the controls, filled by what is shown in it
        div [HTMLAttr.Id "BottomHalf" ; showWaveformsAndRamStyle] (
            if wsModel.SelectedWaves.Length > 0 then [
                WaveSimWaveforms.showWaveforms model wsModel dispatch               
                makeScrollbar wsModel dispatch ]
            else []
            @
            [ div
                  [ HTMLAttr.Id "RamTables"
                    ramTablesStyle (wsModel.SelectedWaves.Length > 0) ]
                  [ WaveSimRams.ramTables dispatch wsModel model ] ]
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

/// Ask the .NET simulator to describe the instances the model references and it has not yet
/// described - the STRUCTURE pass, beside the data pass above and deliberately not part of it.
///
/// Data is convergence-driven: what the view lacks changes with every scroll and cursor move, and
/// depends on how far the simulation has run. Structure is event-driven and run-independent: for
/// one build the circuit is frozen, so a slice is fetched once per instance and never again, and
/// the set worth having changes only when a build completes or the model starts referencing an
/// instance it did not - a wave selected, a RAM ticked, a selector node expanded. All of those are
/// messages, so running after every message IS the event trigger, and the check is a handful of
/// dictionary lookups.
///
/// One operation at a time, like everything on this connection: at most one OpPorts in flight, and
/// the answer brings us back here for whatever is still missing.
let describeWhatIsShown (model: Model, cmd: Elmish.Cmd<Msg>) : Model * Elmish.Cmd<Msg> =
    match model.SidecarSession.Epoch with
    | _ when model.SimulateInRenderer -> model, cmd
    | None -> model, cmd
    | Some epoch when PortData.epochHeld () = Some epoch ->
        let inFlight =
            model.SidecarInFlight
            |> Map.exists (fun _ op ->
                match op with
                | OpPorts _ -> true
                | _ -> false)

        if inFlight then
            model, cmd
        else
            let fs = Simulator.getFastSim ()

            let ws =
                model.WaveSimSheet
                |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim)

            /// what the open selector dialog is showing - the same derivation its view uses
            let shownInSelector =
                match ws with
                | Some ws when ws.WaveModalActive && fs.SimulatedTopSheet <> "" ->
                    (WaveSimHierarchy.getSelectorHierarchy fs ws).HierOrder
                    |> List.choose (fun node -> node.NodeInstance)
                | _ -> []

            let referenced =
                match ws with
                | Some ws ->
                    (ws.SelectedWaves |> List.map (fun wi -> InstancePath(snd wi.Id)))
                    @ (ws.SelectedRams |> Map.toList |> List.map (fun ((_, ap), _) -> InstancePath ap))
                | None -> []

            /// instances holding a Viewer, for the step panel's rows - design-pruned, so a design
            /// whose viewers sit in a handful of subsheets costs that handful
            let viewerInstances =
                fs.Design.InstancesOfComponents (fun c ->
                    match c.Type with
                    | Viewer _ -> true
                    | _ -> false)
                |> List.map snd

            match PortData.missingOf (InstancePath [] :: referenced @ viewerInstances @ shownInSelector) with
            | [] -> model, cmd
            | missing ->
                let seq = ModelHelpers.newSeq ()

                model |> Optic.map sidecarInFlight_ (Map.add seq (OpPorts(List.length missing))),
                Elmish.Cmd.batch
                    [ cmd
                      Elmish.Cmd.OfPromise.either
                          (PortData.fetch epoch)
                          missing
                          (fun result -> SidecarReply(seq, AnsPorts result))
                          (fun exn -> SidecarReply(seq, AnsPorts(Error exn.Message))) ]
    | Some _ -> model, cmd
