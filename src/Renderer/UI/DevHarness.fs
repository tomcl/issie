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

/// The most recent model and dispatch, kept so that the harness can answer questions and send
/// messages between renders. Not model state - this is the outside world's handle on the app, in
/// the same way that KeyBindings.modelContext is the DOM's (docs/mutableState.md).
let mutable private latestModel: Model option = None
let mutable private latestDispatch: (Msg -> unit) option = None

/// Callbacks waiting for the next completed render.
let mutable private waitingForRender: (unit -> unit) list = []

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

//------------------------------------------------------------------------------------------------//
//------------------------------------- Sending a message ---------------------------------------//
//------------------------------------------------------------------------------------------------//

/// The commands `send` accepts. Each takes the argument string - "" when there is none - and the
/// model and dispatch it is being sent into, and returns what to report back.
///
/// These are the messages the corresponding UI element sends, reached the same way, so that driving
/// the app from here and driving it by hand cannot diverge.
let private commands: (string * (string -> Model -> (Msg -> unit) -> string)) list =
    [ "endSimulation",
      fun _ _ dispatch ->
          dispatch EndSimulation
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
          "circuit check requested" ]

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
               send = send
               commands = fun () -> commands |> List.map fst |> Array.ofList
               onNextRender = onNextRender |}
#endif
    ()
