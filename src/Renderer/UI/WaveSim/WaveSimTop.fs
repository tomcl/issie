
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
    // through UNRESOLVED (SimArrayIndex = DriverIndex -1) rather than dropped. Such a wave gets no record
    // built: there is nothing to build one from until the slice lands, and the next refresh
    // after it does resolves the wave and builds it then.
    let details =
        selected
        |> List.choose (fun wi ->
            match Map.tryFind wi ws.WaveDetails with
            | Some wave -> Some(wi, wave)
            | None when driverIndexValue wi.SimArrayIndex >= 0 -> Some(wi, WaveSimHelpers.makeWave ws fs wi)
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
let private localDriverData (SignalHandle(DriverIndex i)) =
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
            (comp.Id, ap), fs.Design.FullNameOf pair)
        |> List.sortBy snd)

/// Major function called after changes to extend simulation and/or redo waveforms.
/// Note that after design change simulation must be recreated externally, and the function called with
/// newSimulation = true. That is because this function has no way to know that the simulation has changed.
/// This function performs (as required) three actions.
/// 1. Extend the simulation to the current cycle (if not already done).
/// 2. Remake the Wave headers, one for each selected waveform.
/// 3. Remake (or make for first time) the saved waveform SVGs for all selected waveforms.
let rec refreshWaveSim (newSimulation: bool) (model: Model): Model * Elmish.Cmd<Msg> =
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

           
    // The refresh works on the wave sim state as it IS. Taking it as a parameter is what let a
    // dispatch made from a render inject a snapshot here, which the end of this function then
    // wrote back over everything dispatched since - the viewer oscillated between two pasts.
    // Nothing hands this function state any more; it reads it.
    let wsModel = getWSModel model

    /// Make sure we always have consistent parameters. They will be written back to model after this function terminates.
    /// The validation may be done more than once because this function is recursive, but that is OK.
    /// validateSimparas is idempotent unless model changes.
    let wsModel =
        /// A viewer with nothing in it is never what the user wants: give a simulation started with
        /// nothing chosen the Viewers of the design. Does nothing once anything at all has been
        /// selected.
        ///
        /// Tried on every refresh while it is still owed, not once at the start, because what it
        /// reads is the top instance's PORTS - and while the .NET simulator is simulating those
        /// arrive with its first slice, some refreshes after the start. Choosing once meant
        /// choosing from nothing there, and the viewer opened empty in the mode that ships. The
        /// flag is what stops that becoming a viewer nobody can empty: see
        /// WaveSimModel.DefaultSelectionPending.
        ///
        /// This is where every wave the simulation offers used to be described, one record per
        /// viewable port - more records than a large flat design has components. What each
        /// SELECTED wave is, is worked out below.
        let chooseDefaultWaves (wsModel: WaveSimModel) =
            if not wsModel.DefaultSelectionPending then
                wsModel
            else
                let t0, m0 = TimeHelpers.getTimeMs (), TimeHelpers.usedHeapBytes ()
                WaveSimSelect.withDefaultSelectionIfPending (Simulator.getFastSim()) wsModel
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
        // a START is what owes a default selection; every refresh after it is where the debt may
        // finally be payable, since only then are the ports it chooses from known
        |> (fun ws -> if newSimulation then { ws with DefaultSelectionPending = true } else ws)
        |> chooseDefaultWaves
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


    // A RAM's rows are of a cycle of a session; a new simulation is a new session, and rows kept
    // across it would be drawn under the new one's clock. Cleared here, where the new wsModel is
    // put into the model, rather than beside WaveDrawn.forget below - these live in the model.
    // A new simulation is a new session: rows kept across it would be drawn under the new one's
    // clock, and a read still out belongs to the session that has gone - its reply is dropped, so
    // clear the flag here rather than wait for one that will not count.
    let wsModel = if newSimulation then { wsModel with RamRows = Map.empty } else wsModel
    let model = updateWSModel (fun _ -> wsModel) model |> updateViewerWidthInWaveSim model.WaveSimViewerWidth

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
                refreshAndIssue dispatch false model
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
            |> List.filter (fun (DriverIndex i) ->
                i >= 0 && (not model.SimulateInRenderer || i < fs.Drivers.Length))

        // Which simulator is answering, decided once. Nothing below asks again: the refresh has
        // one question - is the data for this view here yet - and only the answer differs.
        // The lookup and the clock are read at call time rather than closed over, so installing
        // them pins nothing: a closure holding a FastSimulation would keep its step arrays alive
        // for as long as the cache lived.
        WaveProvider.selectSimulator
            model.SimulateInRenderer
            // the session the cache must be of; 0 - a number no build issues - when there is none
            (model.SidecarSession.Epoch |> Option.defaultValue 0)
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
                        refreshWaveSim newSimulation model
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
and private refreshAndIssue (dispatch: Msg -> unit) (newSimulation: bool) (model: Model) =
    let model, cmd = refreshWaveSim newSimulation model
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
                |> List.filter (fun wi -> driverIndexValue wi.SimArrayIndex >= 0)
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

/// The DATA the view needs from the .NET simulation, as ONE value - the single derivation of
/// ModelType.DataViewport, whose field list is the contract. Pure in the model (plus the
/// simulation the model implicitly holds, named by `epoch`); consults no caches.
let private dataViewportOf (model: Model) (epoch: int) : DataViewport =
    let ws = model.WaveSimSheet |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim)

    let window, waves, rams =
        match ws with
        | Some ws when ws.State = Success ->
            let fs = Simulator.getFastSim ()

            let window: WaveSlice.Window =
                { StartSample = ws.StartCycle
                  Multiplier = ws.SamplingZoom
                  SampleCount = ws.ShownCycles }

            /// What each memory on screen is reading, cycle by cycle - one signal per memory,
            /// in the viewport so it travels with the waveforms (see EvilHoverCache for why the
            /// two consumers cannot wait).
            let addressWaves =
                let selectedMemories =
                    ws.SelectedRams
                    |> Map.toList
                    |> List.choose (fun (ramId, _) -> EvilHoverCache.addressWaveOf fs ramId)

                let drawnRoms =
                    WaveSimStyle.selectedWaves ws
                    |> List.choose (fun wave -> EvilHoverCache.romAddressOf fs wave |> Option.map fst)

                selectedMemories @ drawnRoms

            let waves =
                ws.SelectedWaves
                |> List.append addressWaves
                // negative is unresolved - nothing to read for it yet; its slice arriving
                // resolves it, which changes this list, which is a new viewport and a fetch
                |> List.filter (fun wi -> driverIndexValue wi.SimArrayIndex >= 0)
                |> List.distinctBy (fun wi -> wi.SimArrayIndex)
                |> List.sortBy (fun wi -> wi.SimArrayIndex)

            let rams =
                ws.SelectedRams
                |> Map.toList
                |> List.map (fun (ramId, _) -> ramId, RamData.keyOf model ramId)

            window, waves, rams
        | _ -> { StartSample = 0; Multiplier = 1; SampleCount = 0 }, [], []

    let panelCycle =
        match model.CurrentStepSimulationStep with
        | Some(Ok simData) when
            simData.FastSim.SimulatedTopSheet <> ""
            && not (List.isEmpty (SimulationView.panelSignals simData))
            ->
            Some simData.ClockTickNumber
        | _ -> None

    { VpEpoch = epoch
      VpWindow = window
      VpWaves = waves
      VpRams = rams
      VpPanelCycle = panelCycle
      VpProbe =
        WaveSimSelect.probeQuestion model
        |> Option.map (fun (_, wi, cycle, _) -> wi, cycle)
      VpStimulus = model.StimulusGeneration }

/// The instances whose port slices the model references - ModelType.StructureViewport's one
/// derivation: the selection, the RAM ticks, the Viewer components (design-pruned), the sheet on
/// the draw block and whatever the open selector dialog is showing, always including the top.
let private structureViewportOf (model: Model) (epoch: int) : StructureViewport =
    let fs = Simulator.getFastSim ()
    let ws = model.WaveSimSheet |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim)

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

    let viewerInstances =
        fs.Design.InstancesOfComponents (fun c ->
            match c.Type with
            | Viewer _ -> true
            | _ -> false)
        |> List.map snd

    /// The instance of the sheet the draw block is SHOWING, where the design holds exactly one of
    /// it.
    ///
    /// That is the whole of what the schematic can ask about: the probe and the wire and component
    /// right-click menus all resolve a component through `copiesOfCanvasComp`, which answers only
    /// for a sheet with a sole instance - and answers with this one. A sheet placed twice has two
    /// runtime signals per wire and is declined whatever is held (invariant H1).
    ///
    /// It has to be derived from the DESIGN, and it is, because the deadlock is otherwise
    /// unbreakable: with no slice for this instance, `PortView.ofInstanceCached` answers with an
    /// empty view, so the probe resolves no wave, so nothing names the instance, so no slice is
    /// ever asked for. The probe worked on the top sheet and on whatever sheet happened to hold a
    /// selected wave, a ticked RAM or a viewer, and silently did nothing everywhere else.
    let openInstance =
        ModelHelpers.getCurrFile model
        |> Option.bind fs.Design.SoleInstanceOfSheet
        |> Option.toList

    { SvEpoch = epoch
      SvInstances =
        InstancePath [] :: openInstance @ referenced @ viewerInstances @ shownInSelector
        |> List.distinct
        |> List.sort }

/// The cycle the session must have reached before everything in `vp` can be read.
let private neededCycleOf (vp: DataViewport) : int =
    [ if vp.VpWindow.SampleCount > 0 then
          yield vp.VpWindow.LastCycle + 1
      for _, key in vp.VpRams do
          yield key.Cycle
      match vp.VpPanelCycle with
      | Some cycle -> yield cycle
      | None -> ()
      match vp.VpProbe with
      | Some(_, cycle) -> yield cycle
      | None -> () ]
    |> function
        | [] -> 0
        | needs -> List.max needs

/// Read everything the snapshot covers, in order, in one promise: fetch = a sequence of reads,
/// chained at wire speed with no Elmish round trip between them, under ONE in-flight entry and
/// answered by ONE message. `panelSignals` is passed in because it is derived from the step
/// simulation, which the caller has in hand.
///
/// **Every read's failure reaches the result.** The bundle used to answer with the WAVE read's
/// result alone: a memory, the step panel and the port slices each logged a warning and were
/// dropped, so a fetch whose RAM read failed reported `Ok`, which set `FetchedData` to the
/// snapshot, which is the claim of section J that the caches now hold everything the snapshot
/// needs. They did not, and the comparison that decides fetches is against that record - so
/// nothing asked again until the view moved for some other reason, and a table stayed blank or a
/// selector stayed empty with only a console line to say why.
///
/// Reporting them costs no retry storm: a failed fetch latches its snapshot in `Model.FailedFetch`,
/// so an unchanged viewport is not asked again and any change to it is a different snapshot that
/// is. What it buys is that the viewer's stale banner tells the truth.
///
/// What DID arrive is returned regardless. A memory whose rows came back is worth showing whether
/// or not the one beside it failed.
let private readBundle
    (epoch: int)
    (snapshot: FetchSnapshot)
    (panelSignals: StepPanelData.PanelSignal list)
    (ramsToRead: (FComponentId * RamView.RamKey) list)
    ()
    =
    promise {
        let! waves =
            match snapshot.SnapData with
            | Some vp when vp.VpWindow.SampleCount > 0 && not (List.isEmpty vp.VpWaves) ->
                WaveProvider.fetchWavesFor epoch (Simulator.getFastSim ()) vp.VpWaves vp.VpWindow
            | _ -> Promise.lift (Ok())

        let! ramReads =
            // Only the memories whose rows are not already held under exactly the key their table
            // is asking under - `ramsToRead`, worked out by the caller, which has the model the
            // rows live in. Reading the whole of `VpRams` meant a scroll, which changes the
            // waveform window and nothing about any memory, re-read every one of them.
            (Promise.lift [], ramsToRead)
            ||> List.fold (fun acc (ramId, key) ->
                acc
                |> Promise.bind (fun collected ->
                    RamData.fetch epoch ramId key WaveSimTypes.Constants.maxRamRowsDisplayed
                    |> Promise.map (fun read -> read :: collected)))

        let! panel =
            match snapshot.SnapData |> Option.bind (fun vp -> vp.VpPanelCycle) with
            | Some cycle when not (List.isEmpty panelSignals) ->
                StepPanelData.fill epoch cycle panelSignals
                |> Promise.map (Result.mapError (fun e -> $"reading the step panel at cycle {cycle}: {e}"))
            | _ -> Promise.lift (Ok())

        let! probed =
            match snapshot.SnapData |> Option.bind (fun vp -> vp.VpProbe) with
            | None -> Promise.lift None
            | Some(wi, cycle) ->
                WaveProvider.fetchProbeValue epoch (Simulator.getFastSim ()) wi cycle
                |> Promise.map (Option.map (fun value -> wi, cycle, value))

        let! ports =
            match snapshot.SnapStructure with
            | Some sv ->
                // Only the ones this build has not been asked about. A slice never goes stale
                // within a build - PortData holds them BY the build and a new one starts an empty
                // store - so re-describing an instance already held is a round trip that can only
                // produce the answer already in hand. It costs nothing while the list barely
                // moves, and the list now grows by an entry for every sheet the user visits.
                PortData.fetch epoch (PortData.missingOf sv.SvInstances)
                |> Promise.map (function
                    | Ok _ -> Ok()
                    | Error e -> Error $"describing instances for the selector: {e}")
            | None -> Promise.lift (Ok())

        let rows =
            ramReads |> List.choose (function Ok row -> Some row | Error _ -> None)

        /// Every read that failed, in one message. All of them, not the first: they are
        /// independent reads and a fetch that lost two of them should say so.
        let failures =
            (ramReads |> List.choose (function Error e -> Some e | Ok _ -> None))
            @ ([ waves; panel; ports ] |> List.choose (function Error e -> Some e | Ok() -> None))

        return (if List.isEmpty failures then Ok() else Error(String.concat "; " failures)), rows, probed
    }

/// Suppress the run banner while the waveform viewer is being scrolled horizontally, and arm the
/// delayed message that lets it show again once the scrolling has stopped.
///
/// Here, once, rather than at each of the things that scroll - the scrollbar, its arrow buttons, a
/// zoom, a cursor move that pulls the window along - because what they have in common is what they
/// did to StartCycle, and none of them should have to know that there is a banner. Every one of
/// them reaches this on the way out of the update.
let noteWaveScroll (oldModel: Model) (model: Model, cmd: Elmish.Cmd<Msg>) : Model * Elmish.Cmd<Msg> =
    if (getWSModel model).StartCycle = (getWSModel oldModel).StartCycle then
        model, cmd
    else
        // Each scroll re-arms: the serial it stamps is the one the message must still find when it
        // lands, so a drag's earlier timers expire into nothing and the wait is measured from the
        // scroll that turns out to be the last.
        let serial = model.WaveScrollSerial + 1

        let model =
            model
            |> Optic.set waveScrollSerial_ serial
            |> Optic.set waveScrollSettling_ true

        let settled =
            Elmish.Cmd.ofMsg (DispatchDelayed(Constants.runBannerAfterScrollMs, WaveScrollSettled serial))

        model, Elmish.Cmd.batch [ cmd; settled ]

/// The end-of-update checks that keep the .NET simulation serving the view. Decisions are STATE
/// COMPARISONS, never cache interrogations and never event tracking: is a session there (the
/// start paths' business, not this one's - nothing here ever builds), has it run far enough for
/// the viewport (no: one chunk), and is the current viewport the one the last completed fetch
/// was for (no: one bundle). One operation in flight; every reply is a message; this runs after
/// every message, so a completion or a viewport change is acted on immediately and anything
/// else finds nothing to do in a handful of comparisons. See docs/dev/sidecarInvariants.md.
let sidecarChecks (model: Model, cmd: Elmish.Cmd<Msg>) : Model * Elmish.Cmd<Msg> =
    match model.CurrentProj with
    | None -> model, cmd
    | _ when model.SimulateInRenderer -> model, cmd
    | Some project ->
        match model.SidecarSession with
        | Session(_, _, epoch, clock) ->
            let dataVp = dataViewportOf model epoch

            // The viewport's change is stamped whether or not anything can be issued right
            // now: the stale banner's clock is "how long has THIS viewport been waiting",
            // which starts when it appears, not when the wire happens to be free.
            let model =
                if Some dataVp <> model.CurrentViewport then
                    model
                    |> Optic.set currentViewport_ (Some dataVp)
                    |> Optic.set viewportChangedAtMs_ (TimeHelpers.getTimeMs ())
                else
                    model

            let needed = neededCycleOf dataVp

            /// something is already outstanding; its answer brings us back here
            let busy = ModelHelpers.sidecarIsBusy model

            /// Long enough after a failed run or read to be worth trying again, rather than as
            /// fast as the message queue will carry it. Gates the RUN branch as well as the
            /// read: a chunk that errors frees the wire, and an unpaced re-issue is a spin.
            let backedOff =
                let ws = getWSModel model

                ws.FetchFailedAtMs > 0.0
                && TimeHelpers.getTimeMs () - ws.FetchFailedAtMs < Constants.fetchRetryAfterMs

            if busy || backedOff then
                model, cmd
            elif needed > clock then
                // Run before read. The target beyond `needed` is the prefetch - one window
                // ahead, a user affordance - clamped to the configured run so a need at the end
                // of it cannot chase an unreachable target.
                let ws = getWSModel model

                let runTarget =
                    let span = dataVp.VpWindow.SampleCount * dataVp.VpWindow.Multiplier
                    let cap = if span > 0 then ws.WSConfig.LastClock else needed
                    max needed (min cap (needed + span))

                let seq = ModelHelpers.newSeq ()

                model |> Optic.map sidecarInFlight_ (Map.add seq (OpRunForWaves needed)),
                Elmish.Cmd.batch
                    [ cmd
                      Elmish.Cmd.OfPromise.either
                          (fun () -> SidecarSession.runChunk epoch runTarget)
                          ()
                          (fun result -> SidecarReply(seq, AnsRan result))
                          (fun exn -> SidecarReply(seq, AnsRan(Error exn.Message))) ]
            else
                let structVp = structureViewportOf model epoch
                let dataDiff = Some dataVp <> model.FetchedData
                let structDiff = Some structVp <> model.FetchedStructure

                let snapshot =
                    { SnapData = (if dataDiff then Some dataVp else None)
                      SnapStructure = (if structDiff then Some structVp else None) }

                if (dataDiff || structDiff) && model.FailedFetch <> Some snapshot then
                    let panelSignals =
                        match dataVp.VpPanelCycle, model.CurrentStepSimulationStep with
                        | Some _, Some(Ok simData) -> SimulationView.panelSignals simData
                        | _ -> []

                    // Worked out here rather than inside the promise, because the rows already
                    // held are in the model and the promise has no model - the same reason
                    // panelSignals is passed in.
                    let ramsToRead =
                        if dataDiff then RamData.notHeld model dataVp.VpRams else []

                    let structNote =
                        if structDiff then $", {structVp.SvInstances.Length} instances" else ""

                    Log.dbg
                        Log.Wave
                        $"fetching viewport: {dataVp.VpWaves.Length} waves over                           {dataVp.VpWindow.StartSample}+{dataVp.VpWindow.SampleCount}x{dataVp.VpWindow.Multiplier},                           {dataVp.VpRams.Length} RAMs, panel {dataVp.VpPanelCycle}{structNote}"

                    let seq = ModelHelpers.newSeq ()

                    model |> Optic.map sidecarInFlight_ (Map.add seq (OpFetch snapshot)),
                    Elmish.Cmd.batch
                        [ cmd
                          Elmish.Cmd.OfPromise.either
                              (readBundle epoch snapshot panelSignals ramsToRead)
                              ()
                              (fun (waves, rows, probed) -> SidecarReply(seq, AnsFetched(waves, rows, probed)))
                              (fun exn -> SidecarReply(seq, AnsFetched(Error exn.Message, [], None))) ]
                else
                    // Nothing to do - and during migration, the OLD derivation must agree.
                    // Logged in the dangerous direction only: equality saying "held" while the
                    // cache interrogation finds something missing is a viewport input-list
                    // omission, the one silent-staleness risk of this design. Debug builds with
                    // the wave log on; deleted with the old derivation once validated.
                    if not (dataDiff || structDiff) && Log.isOn Log.Wave then
                        match
                            missingForWaves model project
                            |> Option.orElseWith (fun () -> missingForPanel model project)
                        with
                        | Some miss when
                            not (List.isEmpty miss.MissWaves)
                            || miss.MissRam.IsSome
                            || miss.MissPanel.IsSome
                            ->
                            Log.dbg
                                Log.Wave
                                $"VIEWPORT DIVERGENCE: equality says nothing to fetch, the old                                   derivation finds {miss.MissWaves.Length} waves,                                   ram {miss.MissRam.IsSome}, panel {miss.MissPanel.IsSome}"
                        | _ -> ()

                    model, cmd
        | _ ->
            // No session, or a failed one: the start and refresh paths own that - nothing here
            // builds, which is what deleted the build-retry-storm class outright.
            model, cmd

/// Start - or restart - the waveform simulation, on the model as it IS: the update branch of the
/// StartWaveSim message, which is the ONE way a waveform simulation begins. Start means first do
/// stop: the step and waveform simulations are mutually exclusive, so whatever is running is
/// ended here, synchronously, before the new build - sequencing that used to be spread across
/// dispatched snapshots and a post-render closure, where it raced the renders it spanned.
let startWaveSimulation (model: Model) : Model * Elmish.Cmd<Msg> =
    if model.IsLoading then
        // A project or sheet is part-way through loading: the draw block still holds the OLD
        // canvas while the project already names the new sheets, and a simulation built from
        // that pairing is a simulation of a circuit that never existed. Anything can dispatch
        // this message - a queued script, a stale continuation - so the guard is here, not at
        // the senders. Ignored rather than requeued: whoever wanted it can press Start.
        Log.warn "cannot start a waveform simulation while a project is loading"
        model, Elmish.Cmd.none
    else

    /// update the model memories to match any updated linked initial contents files
    let model = MemoryEditorView.updateAllMemoryComps model

    // the simulation is about to use these contents, so say if a linked file did not load
    let notifyBadMemories =
        Elmish.Cmd.ofEffect (fun dispatch -> MemoryEditorView.notifyMemoryFileErrors dispatch model)

    let canvasState = model.Sheet.GetCanvasState()

    let wsSheet =
        match model.WaveSimSheet with
        | None -> Option.get (getCurrFile model)
        | Some sheet -> sheet

    // First do stop. The simulation slot is released as EndSimulation releases it: only when it
    // holds the STEP simulation being ended. On a Refresh it holds the waveform build being
    // refreshed, which prepareSimulationMemoized reuses when the design has not changed - the
    // reuse that makes Refresh cheaper than Start.
    (match model.CurrentStepSimulationStep with
     | Some(Ok sd) when System.Object.ReferenceEquals(sd.FastSim, Simulator.simCache.FastSim) ->
         Simulator.simCache <- Simulator.simCacheInit ()
         PortData.forget ()
     | _ -> ())
    // the indexes memoised over whatever simulation there was would otherwise hold it alive
    Helpers.clearIdentityMemos ()

    let model =
        model
        |> removeAllSimulationsFromModel
        |> fun model -> { model with WaveSimSheet = Some wsSheet }

    let wsModel, startNotification =
        let ws =
            getWSModel model
            |> fun ws -> { ws with ScrollbarBkgRepCycs = Constants.scrollbarBkgRepCyclesInit }

        match ws.State with
        // a Refresh keeps its place, which is the point of a Refresh
        | Success -> ws, Elmish.Cmd.none
        | _ ->
            // A simulation that is being STARTED starts at the beginning. The WaveSimModel
            // outlives the simulation it was made for - ending one leaves it in the map, marked
            // Ended, so that the selection and the configuration survive - and the cursor and the
            // scroll position were surviving with them, which put a brand new simulation's cursor
            // wherever the last one happened to be left.
            let ws =
                { ws with
                    StartCycle = 0
                    CursorDisplayCycle = 0
                    CursorExactClkCycle = 0 }

            // A simulation being started at the untouched default clock count is given one its
            // design can be started in - an explicitly configured count is honoured exactly, see
            // startingLastClock.
            let estimate = ModelHelpers.simulationHeapEstimate model.WaveSimSheet canvasState model
            let lastClock = ModelHelpers.startingLastClock ws.WSConfig.LastClock estimate

            if lastClock = ws.WSConfig.LastClock then
                ws, Elmish.Cmd.none
            else
                let message =
                    $"This design is large, so its waveform simulation starts at {lastClock} clock cycles \
                      rather than the default {ws.WSConfig.LastClock}. Set the cycle count in \
                      Configure to simulate more."
                Log.warn message
                // in the UI as well as the log: the console is not where the user who wonders
                // why the viewer stops at 200 cycles will be looking
                Optic.set (wSConfig_ >-> lastClock_) lastClock ws,
                Elmish.Cmd.ofMsg (SetSimulationNotification(Notifications.warningSimNotification message))

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
    | Error e, _ ->
        // the same feedback a failed step simulation start gives: the error pane, and the
        // offending components highlighted on the sheet
        let model = setWSModel { wsModel with State = SimError e } model

        model,
        Elmish.Cmd.batch
            [ notifyBadMemories
              startNotification
              yield! SimulationView.getSimErrFeedbackMessages e model |> List.map Elmish.Cmd.ofMsg ]
    | Ok simData, _ ->
        if simData.IsSynchronous then
            SimulationView.setFastSimInputsToDefault simData.FastSim

            // The build is the START's to issue: the fetch checks read, and only read, so a
            // session they find missing is a stopped simulation, never a build they forgot.
            // AnsBuilt creates the session those checks require.
            let model, buildCmd =
                match model.CurrentProj with
                | Some project when not model.SimulateInRenderer ->
                    let design =
                        ModelHelpers.designOf project canvasState
                        |> CanvasExtractor.simpleDesignOfLoadedComponents
                        |> fun d -> { d with TopSheet = wsSheet }

                    // what the SIDECAR allocates, from the configuration - the renderer's own
                    // carrier holds no arrays whatever this says
                    let arraySize = Constants.waveSimRequiredArraySize wsModel
                    let seq = ModelHelpers.newSeq ()

                    model
                    |> Optic.map sidecarInFlight_ (Map.add seq (OpBuild(wsSheet, arraySize))),
                    Elmish.Cmd.OfPromise.either
                        (fun () -> SidecarSession.build design arraySize)
                        ()
                        (fun result -> SidecarReply(seq, AnsBuilt result))
                        (fun exn -> SidecarReply(seq, AnsBuilt(Error exn.Message)))
                | _ -> model, Elmish.Cmd.none

            setWSModel { wsModel with State = Loading } model,
            Elmish.Cmd.batch
                [ notifyBadMemories
                  startNotification
                  buildCmd
                  Elmish.Cmd.ofMsg RefreshWaveSim
                  Elmish.Cmd.ofMsg (UpdateWSModel(fun ws -> { ws with DefaultCursor = Default })) ]
        else
            setWSModel { wsModel with State = NonSequential } model,
            Elmish.Cmd.batch [ notifyBadMemories; startNotification ]

/// What the waveform viewer's Start and Refresh buttons do. The click itself is only a paint
/// fence: the button's spinner must be ON SCREEN before StartWaveSimulation's update branch -
/// which does the work, on the model as it then is - blocks the renderer for the build.
///
/// The start is NOT sent as the RunAfterRender slot's continuation. That slot is one deep and a
/// competing ask - a sheet open finishing, a progress bar re-arming - REPLACES what is pending,
/// and the loser is dropped without a trace, which is how a Start click could do nothing. The
/// slot is used only for what it draws (the spinner); the message goes through its own two
/// animation frames - the same paint guarantee runWhenPainted documents - and then the ordinary
/// queue, where nothing can swallow it.
let refreshButtonAction canvasState model dispatch = fun _ ->
    dispatch <| RunAfterRender(true, fun _ model -> model)

    Browser.Dom.window.requestAnimationFrame (fun _ ->
        Browser.Dom.window.requestAnimationFrame (fun _ -> dispatch StartWaveSimulation) |> ignore)
    |> ignore
    





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
            // the .NET simulator building is the same state the local spinner covers: a start
            // that has not finished. Derived from the ops table, like everything about it.
            | _ -> ModelHelpers.sidecarIsBuilding model
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
