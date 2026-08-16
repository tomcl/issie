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

/// The most recent model and dispatch, kept so that the harness can answer questions and send
/// messages between renders. Not model state - this is the outside world's handle on the app, in
/// the same way that KeyBindings.modelContext is the DOM's (docs/mutableState.md).
let mutable private latestModel: Model option = None
let mutable private latestDispatch: (Msg -> unit) option = None

/// Callbacks waiting for the next completed render.
let mutable private waitingForRender: (unit -> unit) list = []

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
               circuitCheck = circuitCheckOf model
               stepSimulationOpen = model.CurrentStepSimulationStep <> None
               truthTableOpen = model.CurrentTruthTable <> None
               waveSimSheet = model.WaveSimSheet |> Option.defaultValue ""
               popupOpen = Option.isSome model.PopupViewFunc |}

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
        {| stepCache = comps (Simulator.simCache.FastSim)
           waveCache = comps (Simulator.simCacheWS.FastSim)
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
        [ Simulator.simCacheWS.FastSim; Simulator.simCache.FastSim ]
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
                    m.WaveSim |> Map.toSeq |> Seq.sumBy (fun (_, ws) -> ws.AllWaves.Count))
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

      "endWaveSim",
      fun _ _ dispatch ->
          dispatch EndWaveSim
          "ended the waveform simulation"

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
          | Some fs -> timeRuns fs steps ]

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
               simStats = simStats
               send = send
               commands = fun () -> commands |> List.map fst |> Array.ofList
               onNextRender = onNextRender |}
#endif
    ()
