/// Driving Issie from outside it, for development and for automated checking.
///
/// Published as `window.issieDev` in a debug build, and reached from a terminal through
/// `scripts/drive.js`. It exists because the alternative - synthesising DOM events and reading
/// rendered text back - is both slow and wrong often enough to mislead: a click has to find the
/// right element, a wait has to guess how long a render takes, and text scraped from the DOM can
/// be a frame out of date while looking authoritative.
///
/// Four things, which is what that experience says are needed:
///
///   onNextRender   when the update has been applied AND the view has run, so a caller can wait
///                  for the app rather than for a stopwatch
///   send           a named command, dispatched as the message the UI would have sent
///   state          what the app currently is, as data rather than as rendered text
///   simRefs        what is holding a simulation, which is the question the DOM cannot answer
///
/// `send` takes a NAME from a fixed table rather than a serialised Msg. A Msg is an F# union
/// carrying models, canvases and functions; nothing useful survives a round trip through JSON, and
/// a general dispatch-anything surface in a debug build is a hazard for the sake of messages
/// nobody wanted to send. Add a row to `commands` when a new one is needed - that is the point.
module DevHarness

open Fable.Core.JsInterop
open CommonTypes
open ModelType
open ModelHelpers
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson // Json.serialize, the renderer's wire encoder (an extension member, so the open is required)

/// The most recent model and dispatch, kept so that the harness can answer questions and send
/// messages between renders. Not model state - this is the outside world's handle on the app, in
/// the same way that KeyBindings.modelContext is the DOM's (docs/mutableState.md).
let mutable private latestModel: Model option = None
let mutable private latestDispatch: (Msg -> unit) option = None

/// Callbacks waiting for the next completed render.
let mutable private waitingForRender: (unit -> unit) list = []

/// Choose which simulator runs, throwing away anything currently simulating.
///
/// The two backends share nothing - not a step array, not a built simulation - so a switch
/// cannot leave one running and hand it to the other. Everything that holds a simulation is
/// therefore ended first: the step simulator, the waveform simulator and the truth table, which
/// between them are what `ModelHelpers.simulationIsOpen` reports. The flag is then set, and the
/// next simulation the user starts is built by whichever backend is now chosen.
///
/// Here rather than in the menu because both the Development menu and the `simulateIn` harness
/// command need it, and a switch that stopped simulations in one path and not the other would be
/// a good way to end up with a waveform viewer reading a simulation nobody is running.
let setSimulateInRenderer (inRenderer: bool) (dispatch: Msg -> unit) =
    dispatch EndSimulation
    dispatch EndWaveSim
    dispatch (TruthTableMsg CloseTruthTable)
    dispatch <| UpdateModel(fun m -> { m with SimulateInRenderer = inRenderer })

    Log.out (
        if inRenderer then
            "simulation will now run IN THE RENDERER (deprecated - for development only)"
        else
            "simulation will now run in the .NET sidecar"
    )

/// The simulation the last `benchmark` built, so that `rerun` can time the run loop on its own.
/// Building a large design costs many times what running it does, and a profile of the two
/// together says almost nothing about either.
///
/// This RETAINS a whole simulation until the next benchmark replaces it or `endSimulation` drops
/// it, which on a large design is gigabytes. That is worth knowing while measuring: a heap left
/// near its limit slows everything that follows, so a measurement taken after a big one is not
/// comparable with the same measurement taken before it.
let mutable private lastBenchmarkSim: SimTypes.FastSimulation option = None

/// Called from the view wrapper, before the view runs.
let recordModel (model: Model) = latestModel <- Some model

/// Called from the view wrapper, after the view has produced its elements.
///
/// The callbacks run on the next animation frame rather than immediately: the view returning means
/// React has been given the new elements, not that the DOM holds them, and a caller waiting for a
/// render wants the state it can then read to be the state it was waiting for.
let renderDone () =
    match waitingForRender with
    | [] -> ()
    | callbacks ->
        waitingForRender <- []
        Browser.Dom.window.requestAnimationFrame (fun _ -> callbacks |> List.iter (fun f -> f ()))
        |> ignore

let private onNextRender (callback: unit -> unit) = waitingForRender <- callback :: waitingForRender

//------------------------------------------------------------------------------------------------//
//--------------------------------------- What the app is ---------------------------------------//
//------------------------------------------------------------------------------------------------//

let private sheetNames (model: Model) =
    match model.CurrentProj with
    | Some p -> p.LoadedComponents |> List.map (fun ldc -> ldc.Name) |> Array.ofList
    | None -> [||]

/// The simulation status the Simulation tab is showing, as a word.
let private circuitCheckOf (model: Model) =
    match model.CircuitCheck.Verdict with
    | None -> "notYetChecked"
    | Some(Ok true, _) -> "buildsAndIsSynchronous"
    | Some(Ok false, _) -> "buildsAndIsCombinational"
    | Some(Error _, _) -> "doesNotBuild"

let private state () =
    match latestModel with
    | None -> box {| ready = false |}
    | Some model ->
        box
            {| ready = true
               project = model.CurrentProj |> Option.map (fun p -> p.ProjectPath) |> Option.defaultValue ""
               openSheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
               sheets = sheetNames model
               rightTab = string model.RightPaneTabVisible
               simSubTab = string model.SimSubTabVisible
               simulateIn = (if model.SimulateInRenderer then "renderer" else "sidecar")
               circuitCheck = circuitCheckOf model
               stepSimulationOpen = model.CurrentStepSimulationStep <> None
               truthTableOpen = model.CurrentTruthTable <> None
               waveSimSheet = model.WaveSimSheet |> Option.defaultValue ""
               popupOpen = Option.isSome model.PopupViewFunc |}

/// What the waveform viewer is showing, what it has, and what it is waiting for.
///
/// The three are separate questions and the viewer's failures are almost always a disagreement
/// between two of them: the view says one window, the cache holds another, and a fetch either is or
/// is not on its way to close the gap. None of it can be read off the DOM - a waveform drawn for an
/// older window looks exactly like one drawn for this one.
let private waveState () =
    match latestModel with
    | None -> box {| open_ = false |}
    | Some model ->
        match model.WaveSimSheet |> Option.bind (fun sheet -> Map.tryFind sheet model.WaveSim) with
        | None -> box {| open_ = false |}
        | Some ws ->
            let window: WaveSlice.Window =
                { StartSample = ws.StartCycle
                  Multiplier = ws.SamplingZoom
                  SampleCount = ws.ShownCycles }

            let handles =
                ws.SelectedWaves |> List.map (fun wi -> SignalHandle wi.SimArrayIndex)

            box
                {| open_ = true
                   sheet = model.WaveSimSheet |> Option.defaultValue ""
                   state = string ws.State
                   inRenderer = model.SimulateInRenderer
                   startCycle = ws.StartCycle
                   shownCycles = ws.ShownCycles
                   samplingZoom = ws.SamplingZoom
                   cursor = ws.CursorExactClkCycle
                   lastClock = ws.WSConfig.LastClock
                   selected = List.length ws.SelectedWaves
                   detailed = Map.count ws.WaveDetails
                   // waves whose data is not the window they are about to be drawn over. Zero is
                   // the resting state; anything else should be accompanied by fetchInProgress, or
                   // by a fetch on the way out of the update that is running now
                   missing = List.length (WaveData.needFetching handles window)
                   // waves with something on screen, and how many of those are showing a view
                   // other than the one the controls ask for. Stale is the fallback working, not a
                   // fault - unless it stays that way
                   drawn =
                    ws.SelectedWaves
                    |> List.filter (fun wi -> Option.isSome (WaveDrawn.tryDrawn wi.SimArrayIndex))
                    |> List.length
                   stale = WaveDrawn.staleCount ws (WaveSimStyle.selectedWaves ws)
                   // where the first drawn waveform actually starts, which while data is arriving
                   // is behind `startCycle` and should be seen to follow it rather than to stick
                   drawnStart =
                    ws.SelectedWaves
                    |> List.tryPick (fun wi ->
                        WaveDrawn.tryDrawn wi.SimArrayIndex |> Option.map (fun d -> d.Spec.Window.StartSample))
                    |> Option.defaultValue -1
                   fetchInProgress = ModelHelpers.sidecarFetchInFlight model
                   spinner = Option.isSome model.SpinnerPayload |}

/// Whether the .NET sidecar is reachable, and how much is in flight to it.
///
/// The app spawns the sidecar as it starts, and after a rebuild that process has to be published
/// before it can listen - tens of seconds, during which every fetch fails with "not running yet".
/// Anything driving the app therefore has to be able to WAIT for it rather than sleep and hope,
/// which is what `drive.js wait` uses this for.
let private sidecar () =
    let connected, pending = SidecarClient.connectionState ()

    box
        {| connected = connected
           pending = pending
           // which build the fetched port slices are keyed to, and whether that is the build the
           // views are asking about - the disagreement every "selector shows nothing" comes down to
           ports = PortData.describeHeld (Simulator.getFastSim ())
           // what the selector's own read path answers for the TOP instance: -1 not described
           // yet, otherwise how many ports its slice resolved to
           topPorts =
               match PortView.tryOfInstanceCached (Simulator.getFastSim ()) (InstancePath []) with
               | None -> -1
               | Some v -> v.ViewPorts.Length |}

/// What is currently holding a simulation, and how large each one is.
///
/// The question a heap snapshot was needed for once, which is once more than it should be. A
/// FastSimulation is reachable from several places and only one of them is the model, so "is it
/// still in memory" cannot be answered from the model alone - and answering it wrongly is how a
/// leak gets attributed to whichever holder was looked at first.
let private simRefs () =
    let comps (fs: SimTypes.FastSimulation) = fs.FComps.Count + fs.FCustomComps.Count
    let inModel =
        match latestModel with
        | Some { CurrentStepSimulationStep = Some(Ok sd) } -> comps sd.FastSim
        | _ -> 0
    box
        {| cached = comps (Simulator.simCache.FastSim)
           inModel = inModel
           truthTable =
            match latestModel with
            | Some { CurrentTruthTable = Some(Ok tt) } -> comps tt.TableSimData.FastSim
            | _ -> 0
           waveSimSheets = latestModel |> Option.map (fun m -> m.WaveSim |> Map.toArray |> Array.map fst) |> Option.defaultValue [||]
           usedHeapMB = JSHelpers.usedHeap () / 1048576
           heapLimitMB = JSHelpers.heapLimit () / 1048576 |}

/// Exact structure counts for whichever simulation is live, for checking the memory model in
/// SimulationBudget against reality. The budget multiplies components by an estimated bytes-each;
/// this is the census that says what "each" divides by: how many components, how many step
/// arrays of which kind, how many of them synchronous. Counting from the built simulation is
/// exact where estimating from the design would repeat the estimate being checked.
let private simStats () =
    let fs =
        [ Simulator.simCache.FastSim ]
        |> List.tryFind (fun fs -> fs.FComps.Count > 0)
    match fs with
    | None -> box {| live = false |}
    | Some fs ->
        let outputArrays =
            fs.FComps
            |> Map.toSeq
            |> Seq.collect (fun (_, fc) -> fc.Outputs |> Seq.map (fun io -> io.Width))
        let widths = outputArrays |> Seq.toArray
        box
            {| live = true
               sheet = fs.SimulatedTopSheet
               comps = fs.FComps.Count
               customComps = fs.FCustomComps.Count
               maxArraySize = fs.MaxArraySize
               numStepArrays = fs.NumStepArrays
               outputPorts = widths.Length
               outputPortsLe32 = widths |> Array.filter (fun w -> w <= 32) |> Array.length
               outputPortsWide = widths |> Array.filter (fun w -> w > 32) |> Array.length
               syncComps = fs.FClockedComps.Length
               waves =
                latestModel
                |> Option.map (fun m ->
                    m.WaveSim |> Map.toSeq |> Seq.sumBy (fun (_, ws) -> ws.WaveDetails.Count))
                |> Option.defaultValue 0
               typedArrayMB = float fs.StepCost.TypedArrayBytes * float fs.MaxArraySize / 1.0e6
               heapStepMB = float fs.StepCost.HeapBytes * float fs.MaxArraySize / 1.0e6 |}

//------------------------------------------------------------------------------------------------//
//------------------------------------- Sending a message ---------------------------------------//
//------------------------------------------------------------------------------------------------//

/// Run `fs` for `steps` cycles, repeatedly, and report the median with what it was spread over.
///
/// Cycles are re-run from tick 0 over arrays that already hold data, which is what makes every
/// repetition the same work. Nothing reads the results, so the values in them do not matter.
/// Median of the repetitions after a warm-up, since the distribution is a tight cluster with
/// occasional fast outliers (docs/dev/simulatorStructure.md).
let private timeRuns (fs: SimTypes.FastSimulation) (steps: int) =
    let once () =
        fs.ClockTick <- 0
        let t0 = TimeHelpers.getTimeMs ()
        FastRun.runFastSimulation None steps fs |> ignore
        TimeHelpers.getTimeMs () - t0
    let warmUp = 3
    let repeats = 7
    // Every repetition is reported, in order, as well as the median of the ones after the warm-up.
    // Whether the warm-up was long enough is not something to assume: if the first repetitions are
    // slower than the rest, the run is still being tiered up by the JIT and the median is measuring
    // that rather than the simulation.
    let all = [ 1 .. warmUp + repeats ] |> List.map (fun _ -> once ())
    let times = all |> List.skip warmUp |> List.sort
    let median = times[times.Length / 2]
    let series = all |> List.map (sprintf "%.1f") |> String.concat ", "
    let comps = fs.FComps.Count + fs.FCustomComps.Count
    let heapMB = float (JSHelpers.usedHeap ()) / 1048576.0
    $"""{{"sheet": "{fs.SimulatedTopSheet}", "comps": {comps}, "syncComps": {fs.FClockedComps.Length}, """
    + $""""ordered": {fs.FOrderedComps.Length}, "stepArrays": {fs.NumStepArrays}, """
    + $""""maxArraySize": {fs.MaxArraySize}, "typedArrayMB": %.1f{float fs.StepCost.TypedArrayBytes * float fs.MaxArraySize / 1.0e6}, """
    + $""""heapStepMB": %.1f{float fs.StepCost.HeapBytes * float fs.MaxArraySize / 1.0e6}, "usedHeapMB": %.0f{heapMB}, """
    + $""""steps": {steps}, "medianMs": %.2f{median}, "compStepPerMs": %.0f{float (comps * steps) / median}, """
    + $""""seriesMs": [{series}]}}"""

/// Send the whole current design to the dotnet sidecar as SimpleSheets, timing every stage: the
/// journey a design will make when the sidecar simulates - fresh canvas for the open sheet, id
/// reduction (on the copy; the model is untouched), conversion to the Simple wire types, one
/// JSON per sheet so the sidecar can reuse unchanged ones from its cache, the wire, and the
/// sidecar's own deserialisation. Asynchronous, so the timings land in the log rather than in
/// any reply. Reached from Development > Play > Send Design To Sidecar and from the harness's
/// `sendDesign` command; it lives here rather than in Playground because Playground compiles
/// after this file.
/// The open design, fresh: the draw block's canvas for the open sheet, the project's for the
/// rest, and its Simple form. None when no project is open.
let private currentDesign (model: Model) =
    model.CurrentProj
    |> Option.map (fun project ->
        let ldcs = ModelHelpers.designOf project (model.Sheet.GetCanvasState())
        ldcs, CanvasExtractor.simpleDesignOfLoadedComponents ldcs)

let sendDesignToSidecar (model: Model) (_dispatch: Msg -> unit) =
    let t0 = TimeHelpers.getTimeMs ()

    match currentDesign model with
    | None -> Log.error "sidecar design test: open a project first"
    | Some (_, design) ->
        let t1 = TimeHelpers.getTimeMs ()

        // one JSON per sheet: an unchanged sheet serialises to the identical string, which is
        // what lets the sidecar answer it from cache instead of decoding it again
        let sheetJsons =
            design.Sheets |> List.map Json.serialize<SimpleSheet>

        let totalChars = sheetJsons |> List.sumBy String.length
        let t2 = TimeHelpers.getTimeMs ()

        promise {
            do! SidecarClient.connect ()
            let! reply = SidecarClient.sendDesign design.TopSheet sheetJsons
            let t3 = TimeHelpers.getTimeMs ()

            Log.out (
                $"design -> sidecar ({design.Sheets.Length} sheets): "
                + $"reduce+convert %.2f{t1 - t0}ms, "
                + $"serialise %.2f{t2 - t1}ms ({totalChars} chars), "
                + $"round trip incl dotnet deserialise %.2f{t3 - t2}ms, "
                + $"total %.2f{t3 - t0}ms"
            )

            Log.out $"sidecar replied: {reply}"
        }
        |> Promise.catch (fun e -> Log.error $"sidecar design test: {e.Message}")
        |> ignore

/// The name the sidecar run's progress bar carries. Also its ownership tag: the run loop keeps
/// going only while Model.SpinnerPayload holds a payload with this name, so the progress bar's
/// own Cancel button - which clears SpinnerPayload via CancelWaveSimulation, exactly as for a
/// local run - is what stops it. Cancellation by not sending the next chunk, as designed.
let private sidecarRunName = "Running simulation on the .NET sidecar..."

/// A long simulation run ON THE SIDECAR, reported through the same progress bar as a local run.
/// The loop is chunked SimRun requests with a time budget each; after every reply it updates
/// the bar and re-checks ownership on the model, so Cancel takes effect within one chunk.
/// The wave-cursor nudge in CancelWaveSimulation touches only the LOCAL simulation and is a
/// no-op when none exists.
let runOnSidecarWithProgress (cycles: int) (arraySize: int) (topSheet: string option) (model: Model) (dispatch: Msg -> unit) =
    match currentDesign model with
    | None -> Log.error "sidecarRun: open a project first"
    | Some (_, wholeDesign) ->
        // Which sheet to simulate. The design's own TopSheet is the one nothing instantiates,
        // which is what a user simulating "the design" means; naming a sheet instead measures
        // that subtree, and a hierarchy where each level instantiates several of the last is how
        // a big expanded design is reached from small files (largeTest's main1..main6).
        let design =
            match topSheet with
            | Some name when wholeDesign.Sheets |> List.exists (fun sh -> sh.SheetName = name) ->
                { wholeDesign with TopSheet = name }
            | Some name ->
                Log.warn $"sidecarRun: no sheet called {name}, simulating {wholeDesign.TopSheet}"
                wholeDesign
            | None -> wholeDesign

        let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>
        let started = TimeHelpers.getTimeMs ()

        let setBar (toDo: int) =
            dispatch
            <| UpdateModel (fun m ->
                { m with
                    SpinnerPayload =
                        Some { UseProgressBar = true; Name = sidecarRunName; ToDo = max 0 toDo; Total = cycles } })

        let clearBarIfOurs () =
            dispatch
            <| UpdateModel (fun m ->
                match m.SpinnerPayload with
                | Some p when p.Name = sidecarRunName -> { m with SpinnerPayload = None }
                | _ -> m)

        // the epoch the build issued, carried by every command that follows it: the sidecar
        // refuses one that names a session other than the one it holds
        let finishSession (epoch: int) =
            SidecarClient.simEnd epoch |> Promise.map ignore |> ignore

        let rec chunk (epoch: int) (chunkCount: int) : unit =
            SidecarClient.simRun epoch cycles 100
            |> Promise.map (fun reply ->
                if reply.StartsWith "{\"error" then
                    clearBarIfOurs ()
                    Log.error $"sidecarRun: {reply}"
                else
                    let tick = int (unbox<float> ((Fable.Core.JS.JSON.parse reply)?clockTick))
                    let finished = reply.Contains "\"done\":true"

                    // decide on the MODEL, where cancellation is visible
                    dispatch
                    <| ExecFuncInMessage(
                        (fun model _ ->
                            match model.SpinnerPayload with
                            | Some p when p.Name = sidecarRunName ->
                                if finished then
                                    clearBarIfOurs ()
                                    let ms = TimeHelpers.getTimeMs () - started

                                    Log.out (
                                        $"sidecarRun: {cycles} cycles in %.0f{ms}ms over {chunkCount} chunks "
                                        + $"(%.2f{float cycles / ms} cycles/ms) - see simLog/sidecarSimLog for the per-chunk records"
                                    )

                                    finishSession epoch
                                else
                                    setBar (cycles - tick)
                                    chunk epoch (chunkCount + 1)
                            | _ ->
                                // the progress bar's Cancel fired: stop by sending no more chunks
                                Log.out $"sidecarRun: cancelled at cycle {tick} after {chunkCount} chunks"
                                finishSession epoch),
                        dispatch
                    ))
            |> Promise.catch (fun e ->
                clearBarIfOurs ()
                Log.error $"sidecarRun: {e.Message}")
            |> ignore

        promise {
            do! SidecarClient.connect ()
            let! _ = SidecarClient.sendDesign design.TopSheet sheetJsons
            let! built = SidecarClient.simBuild arraySize

            if built.Contains "error" then
                Log.error $"sidecarRun: {built}"
            else
                setBar cycles
                chunk (SidecarClient.epochOf built) 1
        }
        |> Promise.catch (fun e -> Log.error $"sidecarRun: {e.Message}")
        |> ignore

/// The commands `send` accepts. Each takes the argument string - "" when there is none - and the
/// model and dispatch it is being sent into, and returns what to report back.
///
/// These are the messages the corresponding UI element sends, reached the same way, so that driving
/// the app from here and driving it by hand cannot diverge.
let private commands: (string * (string -> Model -> (Msg -> unit) -> string)) list =
    [ "endSimulation",
      fun _ _ dispatch ->
          dispatch EndSimulation
          // and whatever benchmark was holding, so that there is a way to get the heap back down
          // between measurements without restarting the app
          lastBenchmarkSim <- None
          "ended the step simulation"

      "simulateIn",
      // Which simulator runs: "renderer" for the deprecated in-app one, anything else for the
      // .NET sidecar. Stops whatever is simulating, since the two share no state; with no
      // argument it reports the current setting rather than changing it.
      fun arg model dispatch ->
          match arg.Trim().ToLower() with
          | "" -> sprintf """{"simulateIn": "%s"}""" (if model.SimulateInRenderer then "renderer" else "sidecar")
          | "renderer" ->
              setSimulateInRenderer true dispatch
              """{"simulateIn": "renderer"}"""
          | "sidecar" | "dotnet" | "net" ->
              setSimulateInRenderer false dispatch
              """{"simulateIn": "sidecar"}"""
          | other -> sprintf """{"error": "no simulator called '%s' - say renderer or sidecar"}""" other

      "endWaveSim",
      fun _ _ dispatch ->
          dispatch EndWaveSim
          "ended the waveform simulation"

      "startWaveSim",
      // What the waveform viewer's Start button does. `startSimulation` is the STEP simulator's
      // button and does not open this one, which is a difference that costs an hour to notice from
      // a screenshot.
      fun _ model dispatch ->
          WaveSimTop.refreshButtonAction (model.Sheet.GetCanvasState()) model dispatch ()
          "waveform simulation started"

      "waveConfig",
      // "<lastClock>" - the cycle count the Configure dialog sets, without the dialog. The
      // simulation has to be started again for it to take effect, which is what the dialog's OK
      // does; this only records the intent, so that a long simulation can be driven from a script.
      fun arg _ dispatch ->
          match System.Int32.TryParse arg with
          | false, _ -> $"waveConfig needs a number of clock cycles, not '{arg}'"
          | true, n ->
              dispatch (UpdateWSModel(fun ws -> { ws with WSConfig = { ws.WSConfig with LastClock = n } }))
              $"last clock set to {n} - start the simulation for it to take effect"

      "ramSelect",
      // "<n>" - show the contents of the first n RAMs the simulation offers, as ticking them in the
      // Select RAM dialog would. The RAM tables share the pane with the waveforms, so how many
      // there are is what the layout has to cope with.
      fun arg model dispatch ->
          match System.Int32.TryParse arg with
          | false, _ -> $"ramSelect needs a number, not '{arg}'"
          | true, n ->
              let ws = getWSModel model
              let rams = ws.RamComps |> List.truncate n

              dispatch (UpdateWSModel(fun ws -> { ws with SelectedRams = Map.ofList rams }))
              $"showing {List.length rams} of {List.length ws.RamComps} RAMs"

      "waveSelect",
      // "<n>" - show the first n waves the design offers, as picking them in the selector would.
      // Enumerated from the DESIGN, unresolved, in one deterministic order - so the same command
      // selects the same waves whichever simulator is running, and reconciliation resolves them
      // against whichever it is. It used to read the local build's wave index, which a renderer
      // whose simulator is in another process does not have.
      fun arg _ dispatch ->
          match System.Int32.TryParse arg with
          | false, _ -> $"waveSelect needs a number, not '{arg}'"
          | true, n ->
              let design = (Simulator.getFastSim ()).Design

              let rec instances (InstancePath ap as inst) sheet =
                  inst
                  :: (design.SubSheetsOf sheet
                      |> List.collect (fun (cid, child) -> instances (InstancePath(ap @ [ cid ])) child))

              let waves =
                  instances (InstancePath []) design.DesignTopSheet
                  |> List.collect (PortView.waveIndicesOfDesign design)
                  |> List.truncate n

              dispatch (UpdateWSModel(fun ws -> { ws with SelectedWaves = waves }))
              dispatch GenerateCurrentWaveforms
              $"selected {List.length waves} waves"

      "waveView",
      // "<startCycle> [shownCycles] [samplingZoom]" - move the window, as scrolling and zooming do.
      // Set directly rather than through the navigation helpers so that a view can be asked for
      // exactly, including ones the buttons would round.
      fun arg _ dispatch ->
          let parts = arg.Split(' ') |> Array.filter (fun p -> p <> "") |> Array.map int

          match parts with
          | [||] -> "waveView needs '<startCycle> [shownCycles] [samplingZoom]'"
          | _ ->
              dispatch (
                  UpdateWSModel(fun ws ->
                      { ws with
                          StartCycle = parts[0]
                          ShownCycles = (if parts.Length > 1 then parts[1] else ws.ShownCycles)
                          SamplingZoom = (if parts.Length > 2 then parts[2] else ws.SamplingZoom) })
              )

              dispatch GenerateCurrentWaveforms
              $"view set to {arg}"

      "waveCursor",
      // "<cycle>" - move the cursor, which is what the value column and the tooltips read.
      fun arg model dispatch ->
          match System.Int32.TryParse arg with
          | false, _ -> $"waveCursor needs a number, not '{arg}'"
          | true, n ->
              let ws = getWSModel model
              dispatch (WaveSimNavigation.setClkCycleMsg ws n)
              $"cursor to {n}"

      "rightTab",
      fun arg _ dispatch ->
          match arg with
          | "Catalogue" -> dispatch (ChangeRightTab Catalogue); "Catalogue"
          | "Properties" -> dispatch (ChangeRightTab Properties); "Properties"
          | "Simulation" -> dispatch (ChangeRightTab Simulation); "Simulation"
          | "Build" -> dispatch (ChangeRightTab Build); "Build"
          | other -> $"unknown tab '{other}': Catalogue | Properties | Simulation | Build"

      "simSubTab",
      fun arg _ dispatch ->
          match arg with
          | "StepSim" -> dispatch (ChangeSimSubTab StepSim); "StepSim"
          | "TruthTable" -> dispatch (ChangeSimSubTab TruthTable); "TruthTable"
          | "WaveSim" -> dispatch (ChangeSimSubTab WaveSim); "WaveSim"
          | other -> $"unknown sub-tab '{other}': StepSim | TruthTable | WaveSim"

      "openProject",
      // The path of the project folder. Reached the same way the recent-projects list reaches it,
      // which also settles the race that made opening a project from outside unreliable: the reply
      // does not arrive until the open has been dispatched.
      fun arg model dispatch ->
          if arg = "" then
              "openProject needs the path of a project folder"
          else
              MenuHelpers.openProjectFromPath arg model dispatch
              $"opening {arg}"

      "openSheet",
      fun arg model dispatch ->
          match model.CurrentProj with
          | None -> "no project is open"
          | Some project when not (project.LoadedComponents |> List.exists (fun l -> l.Name = arg)) ->
              $"no sheet '{arg}' in this project"
          | Some project ->
              MenuHelpers.openFileInProject arg project model dispatch
              $"opened {arg}"

      "startSimulation",
      // What the Step Simulation tab's button does: build the simulation from the canvas as it is
      // now, and report the result the same way, so a failure arrives as the error the user sees.
      fun _ model dispatch ->
          let canvasState = model.Sheet.GetCanvasState()
          SimulationView.tryGetSimData false canvasState model
          |> function
              | Ok simData ->
                  dispatch (StartSimulation(Ok simData))
                  "simulation started"
              | Error simError ->
                  dispatch (StartSimulation(Error simError))
                  $"simulation failed: {simError.ErrType}"

      "checkCircuit",
      fun _ _ dispatch ->
          dispatch RunCircuitCheck
          "circuit check requested"

      "copyAll",
      // Select the whole sheet and copy it, which is what fills the clipboard the paste commands
      // work from. Two key messages rather than one, because that is what the user does.
      fun _ _ dispatch ->
          dispatch (Sheet(SheetT.KeyPress SheetT.KeyboardMsg.CtrlA))
          dispatch (Sheet(SheetT.KeyPress SheetT.KeyboardMsg.CtrlC))
          "selected all and copied"

      "pasteArray",
      // "<vertical|horizontal> <copies> [firstSuffix]" - what the Paste array dialog sends when its
      // button is pressed, so that the paste itself can be driven without going through the dialog.
      //
      // The dialog is where a suffix that clashes with a label already on the sheet is caught, so
      // this can make duplicate labels that the dialog would have refused. That is the point of it
      // being here rather than in the app: it drives the paste, not the checks in front of it.
      fun arg _ dispatch ->
          let parts = arg.Split(' ') |> Array.filter (fun s -> s <> "")
          let intAt i dflt least =
              match Array.tryItem i parts with
              | None -> Some dflt
              | Some s ->
                  match System.Int32.TryParse s with
                  | true, n when n >= least -> Some n
                  | _ -> None
          let direction =
              match Array.tryItem 0 parts with
              | Some "horizontal" -> Some SheetT.ArrayHorizontal
              | Some "vertical" | None -> Some SheetT.ArrayVertical
              | Some _ -> None
          match direction, intAt 1 2 2, intAt 2 0 0 with
          | Some dir, Some copies, Some firstSuffix ->
              let way = if dir = SheetT.ArrayVertical then "vertical" else "horizontal"
              dispatch (Sheet(SheetT.PasteArray(dir, copies, firstSuffix)))
              $"pasting {copies} copies as a {way} array, suffixes from {firstSuffix}"
          | _ ->
              "pasteArray takes <vertical|horizontal> <copies> [firstSuffix], "
              + "copies being 2 or more and firstSuffix 0 or more"

      "benchmark",
      // What a clock cycle of the open sheet costs, and what it is spread over. The argument is the
      // number of cycles per repetition, default 100.
      //
      // The BUILD is deliberately outside the timing, and the same built simulation is run over and
      // over: the question is what the run loop costs per component, which is the number that falls
      // away as a design gets bigger. Median of the repetitions after a warm-up, since the
      // distribution is a tight cluster with occasional fast outliers (docs/dev/simulatorStructure).
      //
      // Cycles are re-run from tick 0 over arrays that already hold data, which is what makes every
      // repetition the same work. Nothing reads the results, so the values in them do not matter.
      fun arg model _ ->
          // "<steps>" or "<steps> <stepArraySize>". The array size is variable because it is the
          // one thing that changes the DISTANCE between the words a clock cycle touches without
          // changing anything about the work: same components, same reducers, same allocation.
          let parts = arg.Split(' ') |> Array.filter (fun s -> s <> "")
          let intAt i dflt =
              match Array.tryItem i parts |> Option.map System.Int32.TryParse with
              | Some(true, n) when n > 0 -> n
              | _ -> dflt
          let steps = intAt 0 100
          let arraySize = intAt 1 SimulationView.Constants.maxArraySize
          let canvasState = model.Sheet.GetCanvasState()
          let sheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
          let ldcs = model.CurrentProj |> Option.map (fun p -> p.LoadedComponents) |> Option.defaultValue []
          match Simulator.startCircuitSimulation arraySize sheet canvasState ldcs with
          | Error e -> $"""{{"error": "{e.ErrType}"}}"""
          | Ok simData ->
              lastBenchmarkSim <- Some simData.FastSim
              timeRuns simData.FastSim steps

      "rerun",
      // Time the run loop again on whatever `benchmark` last built, without rebuilding it. The
      // argument is the number of cycles per repetition. This is the one to put a profiler round:
      // building a large design costs many times what running it does, so a profile of the two
      // together is a profile of the build.
      fun arg _ _ ->
          let steps =
              match System.Int32.TryParse arg with
              | true, n when n > 0 -> n
              | _ -> 100
          match lastBenchmarkSim with
          | None -> """{"error": "nothing built yet - send benchmark first"}"""
          | Some fs -> timeRuns fs steps

      "sendDesign",
      // Send the current design to the dotnet sidecar, per-sheet with caching. Asynchronous -
      // the reply cannot wait on the round trip - so the timings and the sidecar's report land
      // in the log: node scripts/inspect-canvas.js log
      fun _ model dispatch ->
          sendDesignToSidecar model dispatch
          """{"status": "sending - timings appear in the log"}"""

      "simLog",
      // This runtime's simulation invocation records - one per build, one per run chunk (which
      // in the app means one per progress-bar update) - as JSON. The sidecar answers its own
      // SimLog protocol command with the identical shape, which is what makes any user-driven
      // session's costs directly comparable across the two runtimes.
      fun _ _ _ -> SimLog.recentJson ()

      "sidecarSimLog",
      // The SIDECAR's simulation invocation records, fetched over the renderer's own connection
      // (the sidecar serves one client at a time, so a second socket would queue behind the
      // app's). Asynchronous: the JSON lands in the log.
      fun _ _ _ ->
          promise {
              do! SidecarClient.connect ()
              let! log = SidecarClient.simLog ()
              Log.out $"sidecar simLog: {log}"
          }
          |> Promise.catch (fun e -> Log.error $"sidecarSimLog: {e.Message}")
          |> ignore

          """{"status": "fetching - records appear in the log"}"""

      "sidecarRun",
      // A long run on the sidecar behind the app's own progress bar, Cancel included.
      // Arguments: cycle count (default one million) and optionally the step-array size - pass
      // cycles+margin to reproduce the waveform simulator's non-circular full-array workload
      // rather than the default small circular buffer. Report lands in the log.
      fun arg model dispatch ->
          let parts = arg.Split ' ' |> Array.filter (fun s -> s <> "")

          let intAt i dflt =
              match Array.tryItem i parts |> Option.map System.Int32.TryParse with
              | Some(true, n) when n > 0 -> n
              | _ -> dflt

          runOnSidecarWithProgress (intAt 0 1_000_000) (intAt 1 250) (Array.tryItem 2 parts) model dispatch
          """{"status": "running - progress bar up, report lands in the log"}"""

      "localRun",
      // The LOCAL half of the backend comparison: the same chunked long run as sidecarRun, on
      // this runtime's FastSim, with the same arguments - so SimLog carries directly comparable
      // per-chunk records for both. The simulation is standalone (not the step or wave sim) and
      // is retained like benchmark's, freed by endSimulation.
      fun arg model dispatch ->
          let parts = arg.Split ' ' |> Array.filter (fun s -> s <> "")

          let intAt i dflt =
              match Array.tryItem i parts |> Option.map System.Int32.TryParse with
              | Some(true, n) when n > 0 -> n
              | _ -> dflt

          let cycles = intAt 0 1_000_000
          let arraySize = intAt 1 250

          match currentDesign model with
          | None -> """{"error": "no project open"}"""
          | Some (ldcs, wholeDesign) ->
              // an optional third argument names the sheet to simulate - see
              // runOnSidecarWithProgress, which takes the same one, so the two halves of a
              // comparison can be pointed at the same subtree
              let design =
                  match Array.tryItem 2 parts with
                  | Some name when ldcs |> List.exists (fun ldc -> ldc.Name = name) ->
                      { wholeDesign with TopSheet = name }
                  | _ -> wholeDesign

              let top = ldcs |> List.find (fun ldc -> ldc.Name = design.TopSheet)
              let started = TimeHelpers.getTimeMs ()

              match Simulator.startCircuitSimulation arraySize design.TopSheet top.CanvasState ldcs with
              | Error e -> sprintf """{"error": "local build failed: %A"}""" e.ErrType
              | Ok simData ->
                  let fs = simData.FastSim
                  lastBenchmarkSim <- Some fs

                  let rec chunk (chunkCount: int) =
                      promise {
                          FastRun.runFastSimulation (Some 100.0) cycles fs |> ignore

                          if fs.ClockTick < cycles then
                              // yield to the event loop so renders and SimLog reads stay live
                              do! Promise.sleep 1
                              return! chunk (chunkCount + 1)
                          else
                              let ms = TimeHelpers.getTimeMs () - started

                              Log.out (
                                  $"localRun: {design.TopSheet} {cycles} cycles in %.0f{ms}ms over {chunkCount} chunks "
                                  + $"(%.2f{float cycles / ms} cycles/ms) - see simLog for the per-chunk records"
                              )
                      }

                  chunk 1 |> Promise.catch (fun e -> Log.error $"localRun: {e.Message}") |> ignore
                  """{"status": "running locally - report lands in the log"}"""

      "sidecarProbe",
      // The binary read path, end to end: build the open design on BOTH sides, run N cycles
      // (argument, default 20), SimRead a window of the first few top-sheet outputs from the
      // sidecar - values arriving as a zero-copy Uint32Array view over the response frame, the
      // point of the 8-byte header - and compare word-for-word with local extraction.
      // Asynchronous: the verdict lands in the log.
      fun arg model _ ->
          let cycles =
              match System.Int32.TryParse arg with
              | true, n when n > 1 -> n
              | _ -> 20

          match currentDesign model with
          | None -> """{"error": "no project open"}"""
          | Some (ldcs, design) ->
              let top = ldcs |> List.find (fun ldc -> ldc.Name = design.TopSheet)

              match Simulator.startCircuitSimulation 250 design.TopSheet top.CanvasState ldcs with
              | Error e -> sprintf """{"error": "local build failed: %A"}""" e.ErrType
              | Ok simData ->
                  let localFs = simData.FastSim
                  FastRun.runFastSimulation None (cycles - 1) localFs |> ignore

                  // the first few top-level components with a ≤32-bit output
                  let items =
                      localFs.FComps
                      |> Map.toList
                      |> List.choose (fun ((ComponentId cid, path), fc) ->
                          if List.isEmpty path && fc.Outputs.Length > 0 && fc.Outputs[0].Width <= 32 then
                              Some(cid, 0, [])
                          else
                              None)
                      |> List.truncate 4

                  let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>

                  promise {
                      do! SidecarClient.connect ()
                      let! _ = SidecarClient.sendDesign design.TopSheet sheetJsons
                      let! built = SidecarClient.simBuild 250
                      let epoch = SidecarClient.epochOf built
                      let! _ = SidecarClient.simRun epoch (cycles - 1) 0
                      let! frame = SidecarClient.simRead epoch 0 1 cycles items

                      let asText = SidecarClient.decodeText frame

                      if asText.StartsWith "{" then
                          Log.error $"sidecarProbe: SimRead failed: {asText} (build: {built})"
                      else
                          let count = List.length items * cycles
                          let view = SidecarClient.viewSimReadData frame count

                          let mismatches =
                              [ for itemIndex in 0 .. List.length items - 1 do
                                  let cid, _, _ = items[itemIndex]

                                  for c in 0 .. cycles - 1 do
                                      let wire = SidecarClient.uint32At view (itemIndex * cycles + c)

                                      let local =
                                          match FastExtract.extractFastSimulationOutput localFs c (ComponentId cid, []) (OutputPortNumber 0) with
                                          | SimGraphTypes.IData fd -> float (uint32 fd.GetBigInt)
                                          | _ -> -1.0

                                      if wire <> local then
                                          yield $"comp {cid} cycle {c}: dotnet {wire} vs electron {local}" ]

                          match mismatches with
                          | [] ->
                              Log.out (
                                  $"sidecarProbe: IDENTICAL step data - {List.length items} signals x {cycles} cycles "
                                  + "read through a zero-copy Uint32Array view over the 8-aligned frame"
                              )
                          | first :: _ ->
                              Log.error $"sidecarProbe: {List.length mismatches} mismatches, first: {first}"
                  }
                  |> Promise.catch (fun e -> Log.error $"sidecarProbe: {e.Message}")
                  |> ignore

                  """{"status": "probing - verdict appears in the log"}"""

      "sidecarPorts",
      // The port-slice path, end to end: build the open design on both sides, ask the sidecar
      // for the slice of every instance over the wire, and compare against the shared function
      // run locally. Equality also says the two processes allocate identical driver indices for
      // identical input. Asynchronous: the verdict lands in the log.
      fun _ model _ ->
          match currentDesign model with
          | None -> """{"error": "no project open"}"""
          | Some(ldcs, design) ->
              let top = ldcs |> List.find (fun ldc -> ldc.Name = design.TopSheet)

              match Simulator.startCircuitSimulation 250 design.TopSheet top.CanvasState ldcs with
              | Error e -> sprintf """{"error": "local build failed: %A"}""" e.ErrType
              | Ok simData ->
                  let localFs = simData.FastSim

                  let rec instances (InstancePath ap as inst) sheet =
                      inst
                      :: (localFs.Design.SubSheetsOf sheet
                          |> List.collect (fun (cid, child) ->
                              instances (InstancePath(ap @ [ cid ])) child))

                  let all = instances (InstancePath []) design.TopSheet
                  let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>

                  promise {
                      do! SidecarClient.connect ()
                      let! _ = SidecarClient.sendDesign design.TopSheet sheetJsons
                      let! built = SidecarClient.simBuild 250
                      let epoch = SidecarClient.epochOf built
                      let mutable ports = 0
                      let mutable failures = []
                      let t0 = TimeHelpers.getTimeMs ()

                      for (InstancePath ap) as inst in all do
                          let! slice = SidecarClient.simPorts epoch (ap |> List.map (fun (ComponentId c) -> c))

                          match slice with
                          | Error e -> failures <- $"{inst}: {e}" :: failures
                          | Ok overTheWire ->
                              let local = PortView.sheetSliceOf localFs inst

                              ports <-
                                  ports
                                  + (overTheWire
                                     |> List.sumBy (fun c -> c.SlotsIns.Length + c.SlotsOuts.Length))

                              if overTheWire <> local then
                                  failures <- $"{inst}: slice differs" :: failures

                      let ms = TimeHelpers.getTimeMs () - t0

                      match failures with
                      | [] ->
                          Log.out
                              $"sidecarPorts: IDENTICAL port slices - {List.length all} instances, {ports} ports, %.1f{ms}ms over the wire"
                      | first :: _ -> Log.error $"sidecarPorts: {List.length failures} failures, first: {first}"
                  }
                  |> Promise.catch (fun e -> Log.error $"sidecarPorts: {e.Message}")
                  |> ignore

                  """{"status": "comparing - verdict appears in the log"}"""

      "simCompare",
      // The cross-runtime correctness check: compute the deterministic-stimulus digest of the
      // open design locally, send the design to the sidecar and ask for ITS digest of the same
      // thing, and diff. The argument is the cycle count, default 30. Asynchronous: the verdict
      // lands in the log.
      fun arg model _ ->
          let ticks =
              match System.Int32.TryParse arg with
              | true, n when n > 0 -> n
              | _ -> 30

          match currentDesign model with
          | None -> """{"error": "no project open"}"""
          | Some (ldcs, design) ->
              match SimDigest.render ldcs design.TopSheet ticks with
              | Error e -> sprintf """{"error": "local digest failed: %s"}""" e
              | Ok localText ->
                  let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>

                  promise {
                      do! SidecarClient.connect ()
                      let! sent = SidecarClient.sendDesign design.TopSheet sheetJsons
                      let t0 = TimeHelpers.getTimeMs ()
                      let! remote = SidecarClient.simDigest ticks
                      let ms = TimeHelpers.getTimeMs () - t0

                      if remote.StartsWith "{" then
                          Log.error $"simCompare: sidecar digest failed: {remote} (design sent: {sent})"
                      elif remote = localText then
                          Log.out (
                              $"simCompare: IDENTICAL behaviour over {ticks} cycles "
                              + $"({localText.Length} chars of digest; sidecar build+run+render %.1f{ms}ms)"
                          )
                      else
                          let localLines = localText.Split '\n'
                          let remoteLines = remote.Split '\n'

                          let firstDiff =
                              Seq.append (Seq.zip localLines remoteLines |> Seq.indexed |> Seq.filter (fun (_, (a, b)) -> a <> b) |> Seq.map fst)
                                         (Seq.singleton (min localLines.Length remoteLines.Length))
                              |> Seq.head

                          let at (lines: string array) i = if i < lines.Length then lines[i] else "<missing>"

                          Log.error (
                              $"simCompare: DIVERGED over {ticks} cycles at digest line {firstDiff}: "
                              + $"electron '{at localLines firstDiff}' vs dotnet '{at remoteLines firstDiff}'"
                          )
                  }
                  |> Promise.catch (fun e -> Log.error $"simCompare: {e.Message}")
                  |> ignore

                  """{"status": "comparing - verdict appears in the log"}""" ]

let private send (name: string) (arg: string) =
    match latestModel, latestDispatch with
    | Some model, Some dispatch ->
        match commands |> List.tryFind (fun (n, _) -> n = name) with
        | Some(_, run) -> run arg model dispatch
        | None ->
            let known = commands |> List.map fst |> String.concat ", "
            $"unknown command '{name}': try one of {known}"
    | _ -> "the app has not finished starting"

//------------------------------------------------------------------------------------------------//

/// Publish the harness. Debug builds only, by the same test the Development menu uses: this can
/// start simulations and open sheets, which is not something a shipped build should offer a page.
let publish (dispatch: Msg -> unit) =
    latestDispatch <- Some dispatch
#if FABLE_COMPILER
    if JSHelpers.debugLevel > 0 then
        Browser.Dom.window?issieDev <-
            {| state = state
               simRefs = simRefs
               waveState = waveState
               sidecar = sidecar
               simStats = simStats
               send = send
               commands = fun () -> commands |> List.map fst |> Array.ofList
               onNextRender = onNextRender |}
#endif
    ()
