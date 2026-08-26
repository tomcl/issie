module Update

open Elmish
open Fable.React
open Fable.React.Props
open ModelType
open ElectronAPI
open FilesIO
open SimGraphTypes
open SimTypes
open ModelHelpers
open CommonTypes
open CatalogueView
open Sheet.SheetInterface
open DrawModelType
open UpdateHelpers
open Optics
open Optics.Optic
open Optics.Operators


//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//


let mutable uiStartTime: float = 0.

// EvilUIState was a mutable global holding a three-case approximation of the UI context, read
// only to decide whether to swallow the space bar. KeyBindings derives the real context from the
// model in the same place, and space is now an ordinary row in the shortcut table.

    


/// End the waveform simulation, releasing everything it holds.
///
/// **The two simulations are mutually exclusive**, and this is what makes it so from the step
/// simulator's side: starting one ends the other, ending one leaves nothing of it, and whichever
/// is started next builds from scratch. Nothing in Issie runs two simulations at once, and the
/// .NET simulator depends on that - it holds ONE session, and there is no arbitration between two
/// things driving it because there are never two.
///
/// Safe to call when there is no waveform simulation: it is then the releases, which are already
/// empty, and a model that already says so.
let private endWaveSimulation (model: Model) =
    // The simulation slot is NOT cleared here. This runs on two occasions: the wave simulation
    // ending for good (the EndWaveSim handler, which releases the slot itself), and a STEP
    // simulation having just been built (the StartSimulation handler) - where the slot already
    // holds the new build, and clearing it would destroy the simulation being started.
    //
    // the waveforms on screen are about to stop being on screen, and they are the only thing
    // this holds
    WaveDrawn.forget ()
    // As EndSimulation: the wave selector's and the RAM list's indexes are memoised on the
    // simulation, so they have to be emptied or they retain it for the rest of the session.
    // PortData is NOT forgotten here: like the simulation slot, this runs when the OTHER
    // simulator has just started, and its build has already installed the slice source it needs
    // - tearing it down would make the next render compute an empty slice locally on the carrier
    // and memoise the poison. The End handlers forget it under the same identity guard that
    // releases the slot.
    Helpers.clearIdentityMemos ()
    let model = removeAllSimulationsFromModel model

    match model.WaveSimSheet with
    | None
    | Some "" -> model
    | Some sheet ->
        { model with
            WaveSimSheet = None
            WaveSim =
                Map.change sheet (Option.map (fun ws -> { ws with State = Ended; WaveModalActive = false })) model.WaveSim }

//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function, before a read-only sheet is held at its pinned state.
/// Call `update`, not this.
let updateUnpinned (msg : Msg) oldModel =

    let withNoMsg (model: Model) = model, Cmd.none

    let withMsg (msg: Msg) (model : Model)  = model,Cmd.ofMsg msg

    let withMsgs (msgs: Msg list) (model : Model) = model, Cmd.batch (List.map Cmd.ofMsg msgs)

    let startOfUpdateTime = TimeHelpers.getTimeMs()   

    //Add the message to the pending queue if it is a mouse drag message
    let model =
        if matchMouseMsg (fun op -> op = DrawHelpers.Drag) msg then
            {oldModel with Pending = msg :: oldModel.Pending}
        else
            oldModel
    
    //Check if the current message is stored as pending, if so execute all pending messages currently in the queue
    let testMsg, cmd =
        List.tryFind (fun x -> isSameMsg x msg) model.Pending
        |> function
        | Some _ ->
            //Add any message recieved to the pending message queue
            DoNothing, Cmd.ofMsg (ExecutePendingMessages (List.length model.Pending))
        | None ->
            msg, Cmd.none
    let model = updateAllMemoryCompsIfNeeded model
    //-------------------------------------------------------------------------------//
    //------------------------------MAIN MESSAGE DISPATCH----------------------------//
    //-------------------------------------------------------------------------------//

    match testMsg with
    // global message on any key press includimng control keys etc.
    | AnyKeyPress key ->
            match  model.CodeEditorState with
            | Some _ -> Editor.updateEditorOnKeyPress key model
            | None -> model, Cmd.none

    | RunAfterRender( withSpinner, fn) ->
        {model with RunAfterRenderWithSpinner = Some {FnToRun=fn; ButtonSpinnerOn = withSpinner}}, Cmd.none

    | ChangeWaveSimMultiplier key ->
        let table = Constants.multipliers
        if key < 0 || key >= table.Length then
            Log.warn $"cannot change the waveform multiplier to key {key}"
            model, Cmd.none   
        else
           match model.WaveSimSheet with
           | None ->
                Log.warn "cannot change the waveform multiplier: no waveform simulator sheet"
                model, Cmd.none
           | Some sheet ->
                model
                |> Optic.map waveSim_ (fun ws ->
                    let wsModel = ws[sheet]
                    Map.add sheet (WaveSimNavigation.changeMultiplier (table[key]) wsModel) ws)
                |> WaveSimTop.refreshWaveSim false
                

    | CheckMemory ->
        if JSHelpers.loggingMemory then
            let heapInBytes = JSHelpers.getProcessPrivateMemory()
            let hint = [$"{heapInBytes} MB"]
            Log.dbg Log.Perf $"heap size {hint}"
            {model with Sheet.Wire.Symbol.HintPane = Some hint}, Cmd.none
        else
            model, Cmd.none
    | SaveModel ->
        model, Cmd.none
    | FileCommand(fc,dispatch) ->
        FileUpdate.fileCommand fc dispatch model 
    | StartUICmd uiCmd ->
        uiStartTime <- TimeHelpers.getTimeMs()
        match model.UIState with
        | None -> //if nothing is currently being processed, allow the ui command operation to take place
            match uiCmd with
            | CloseProject ->
                {model with CurrentProj = None; UIState = Some uiCmd}
                |> withNoMsg
            | _ -> 
                {model with UIState = Some uiCmd}
                |> withMsg (Sheet (SheetT.SetSpinner true))
        | _ -> model, Cmd.none //otherwise discard the message
    | FinishUICmd ->
        let popup = CustomCompPorts.optCurrentSheetDependentsPopup model
        {model with UIState = None; PopupViewFunc = popup}
        |> withMsg (Sheet (SheetT.SetSpinner false))

    | CloseApp ->
        exitApp model
        model, Cmd.none

    | Sheet sMsg ->
        // There used to be two gates here. One dropped every KeyPress while a popup was open,
        // which covered only KeyPress and so let Rotate, Flip, Arrangement and RotateLabels
        // through into popups. The other stripped the arrow keys out and redirected them to the
        // waveform simulator, in every context. Both are gone: a shortcut now states the contexts
        // it is live in, and a second guard saying the same thing is how the two come to disagree.
        match sMsg with
        | SheetT.ToggleNet canvas ->
            model, Cmd.none
        | _ ->
            // Adding or deleting a custom component - or pasting one, or undoing either - changes
            // what the design sets its subsheets' parameters to, so the values have to be worked
            // out again. Detected by comparing the instances rather than by listing the messages
            // that can do it: paste and undo reach the same state by their own routes, and a list
            // of messages is the kind of thing that goes stale. Ordinary canvas edits leave this
            // untouched and cost only the comparison.
            let instancesOf (m: Model) =
                m.Sheet.Wire.Symbol.Symbols
                |> Map.toList
                |> List.choose (fun (_, sym) ->
                    match sym.Component.Type with
                    | Custom cc -> Some (sym.Component.Id, cc.Name, cc.ParameterBindings)
                    | _ -> None)
            let before = instancesOf model
            let newModel, cmd = sheetMsg sMsg model
            match instancesOf newModel = before with
            | true -> newModel, cmd
            | false -> newModel, Cmd.batch [cmd; Cmd.ofMsg PropagateParameters]

    | WaveSimKeyPress s ->
        // Navigation in the waveform simulator. Only reachable from the WaveSim key context, which
        // already establishes that the wave simulator has the keyboard and is running.
        let wsModel = getWSModel model
        let moveCursorMsg num = WaveSimNavigation.setClkCycleMsg wsModel (wsModel.CursorExactClkCycle + num)
        match s with
        | "ArrowLeft" -> model, Cmd.ofMsg (moveCursorMsg -1)
        | "ArrowRight" -> model, Cmd.ofMsg (moveCursorMsg 1)
        // changeZoom dispatches for itself rather than returning a message, as the viewer's own
        // zoom buttons call it, so it needs a dispatch that update does not have: Cmd.ofEffect
        // is given one.
        | "ZoomIn" -> model, Cmd.ofEffect (WaveSimNavigation.changeZoom wsModel true)
        | "ZoomOut" -> model, Cmd.ofEffect (WaveSimNavigation.changeZoom wsModel false)
        | _ -> model, Cmd.none


    | SynchroniseCanvas ->
        // used after drawblock components are centred on load to enusre that Issie CanvasState is updated
        // This may be needed if Ctrl/w on load moves the whole draw block sheet circuit to centre it.
        // in this case we do not want the save button to be active, because moving the circuit is not a "real" change
        // updating loaded component CanvasState to equal draw bloack canvasstate will ensure the button stays inactive.
        let canvas = model.Sheet.GetCanvasState ()
        // this should disable the saev button by making loadedcomponent and draw blokc canvas the same
        model
        |> map openLoadedComponentOfModel_ (fun ldc -> {ldc with CanvasState = canvas})
        |> withNoMsg

    | PinReadOnlyCanvas ->
        // Sent as the last message of a sheet load. Loading a sheet legitimately changes its
        // canvas - symbol sizes are recomputed, wires whose ports have moved are rerouted,
        // inferred widths are written back, the circuit is centred, and parameterised sheets are
        // redrawn at the values computed for the top sheet - so this is the first moment at which
        // "unchanged" means anything. Armed any earlier, the pin would undo the load itself.
        // Always sent, so that opening an ordinary sheet clears the previous sheet's pin.
        let baseline =
            if openSheetIsReadOnly model then Some (pinnedCanvasOf model.Sheet) else None
        set readOnlyBaseline_ baseline model
        |> withNoMsg
        
    // special messages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode ->
        {model with DividerDragMode= mode}
        |> withNoMsg

    | StartDragPlacement (ghost, cursorPos) ->
        model
        |> set dragPlacement_ (Some (Dragging (ghost, cursorPos)))
        |> withNoMsg

    | MoveDragPlacement cursorPos ->
        // Only while still carrying something: a stray move must not resurrect a gesture that
        // has already been dropped, which would put the ghost back and lose the drop position.
        match model.DragPlacement with
        | Some (Dragging (ghost, _)) ->
            model |> set dragPlacement_ (Some (Dragging (ghost, cursorPos))) |> withNoMsg
        | _ -> model |> withNoMsg

    | DropDragPlacement pos ->
        model
        |> set dragPlacement_ (Some (DroppedAt pos))
        |> withNoMsg

    | EndDragPlacement ->
        model
        |> set dragPlacement_ None
        |> withNoMsg

    | SetProjectBrowserFolder folder ->
        // One refresh chain at a time. Reopening the browser while the last chain is still in
        // flight must not start a second one, or the folder would be read twice a second, then
        // four times, and so on.
        let chainAlreadyRunning = model.ProjectBrowser.IsSome
        let model =
            model |> set projectBrowser_ (Some (ModelHelpers.readProjectBrowserFolder folder 0))
        if chainAlreadyRunning then
            model |> withNoMsg
        else
            model, Cmd.ofMsg (DispatchDelayed (Constants.projectBrowserRefreshMs, TickProjectBrowser))

    | TickProjectBrowser ->
        match model.ProjectBrowser with
        // the browser has closed, so the chain ends here rather than ticking on unseen
        | None -> model |> withNoMsg
        | Some browser ->
            // the selection is kept where it was, but the folder may have shrunk under it
            model
            |> set projectBrowser_
                (Some (ModelHelpers.readProjectBrowserFolder browser.Folder browser.Selected)),
            Cmd.ofMsg (DispatchDelayed (Constants.projectBrowserRefreshMs, TickProjectBrowser))

    | MoveProjectBrowserSelection delta ->
        model
        |> Optic.map projectBrowser_ (Option.map (fun browser ->
            match browser.Listing with
            | Error _ -> browser
            | Ok entries ->
                { browser with
                    Selected = ModelHelpers.clampSelection (browser.Selected + delta) entries }))
        |> withNoMsg

    | GoToProjectBrowserParent ->
        match model.ProjectBrowser with
        | Some browser when not (isFilesystemRoot browser.Folder) ->
            model |> withMsg (SetProjectBrowserFolder (dirName browser.Folder))
        | _ -> model |> withNoMsg

    | OpenProjectBrowserSelection dispatch ->
        FileUpdate.activateBrowserSelection model dispatch
        model |> withNoMsg

    | SetViewerWidth w ->
        {model with WaveSimViewerWidth = w}
        |> withNoMsg

    | SheetBackAction dispatch ->
        processSheetBackAction dispatch model
        |> withNoMsg        

    | UpdateUISheetTrail updateFun ->
        model
        |> map uISheetTrail_ (updateFun >> List.filter (filterByOKSheets model))
        |> withNoMsg

    | ReloadSelectedComponent width ->
        {model with LastUsedDialogWidth=width}
        |> withNoMsg

    | Benchmark ->
        let step = 2000
        let warmup = 5
        let simulationRound = 10
        let benchmarkRound = 20

        let geometricMean (values: float list) = (values |> List.reduce (*)) ** (1.0 / (float values.Length))

        let benchmark i =
            match model.CurrentProj with
            | Some p ->
                Log.out $"benchmarking {dirName p.ProjectPath}, stepArraySize {SimulationView.Constants.maxArraySize}, step {step}, warmup {warmup}, repeat {simulationRound}"

                p.LoadedComponents
                |> List.map (fun c ->
                    let simData = Simulator.startCircuitSimulation SimulationView.Constants.maxArraySize c.Name c.CanvasState p.LoadedComponents

                    match simData with
                    | Error err -> failwithf "Error occured when running startCircuitSimulation on %A, %A" c.Name err
                    | Ok simData ->
                        let comps = simData.FastSim.FComps.Values |> Seq.filter (fun fc -> match fc.FType with | IOLabel -> false | _ -> true) |> Seq.length
                        Log.out $"benchmarking with component {c.Name}"

                        [ 1 .. (warmup + simulationRound) ]
                        |> List.map (fun _ ->
                            simData.FastSim.ClockTick <- 0
                            let start = TimeHelpers.getTimeMs ()
                            // for _ in 0..(step-1) do FastRun.stepSimulation simData.FastSim
                            FastRun.runFastSimulation None step simData.FastSim |> ignore
                            TimeHelpers.getTimeMs () - start)
                        |> List.skip warmup
                        |> List.average
                        |> (fun time ->
                            // as floats BEFORE multiplying: int multiply wraps at 2^31 under Fable,
                            // and comps * step passes that on any large design
                            let speed = float comps * float step / time
                            Log.out $"simulated {c.Name} for {step} steps with {comps} effective components in %.3f{time}ms, average speed %.3f{speed} comp*step/ms"
                            speed))
                |> geometricMean

            | None -> failwith "No project loaded, please load a project to benchmark"

        [ 1..benchmarkRound ]
        |> List.map (fun i -> benchmark i)
        |> fun mean -> Log.out $"geometric mean of simulation speed on this project: {mean}"

        model, Cmd.none

    | StartSimulation simData ->
        // The two simulations are mutually exclusive, and this is the direction that was missing.
        // Starting a WAVEFORM simulation already ends this one - startWaveSimulation goes through
        // removeAllSimulationsFromModel - and so does ending it, but a step simulation started
        // while a waveform one was live left both in the model. With the .NET simulator that is
        // two things driving one session: the step simulator running and reading it, and
        // fetchWhatIsMissing still fetching waves and RAM rows for a view nobody is looking at.
        let model =
            model
            |> endWaveSimulation
            |> set currentStepSimulationStep_ (Some simData)

        // the build is the START's to issue - see SimulationView.issueStepBuild
        match simData, model.SimulateInRenderer with
        | Ok sd, false -> SimulationView.issueStepBuild model sd
        | _ -> model |> withNoMsg

    | SetWSModel wsModel ->
        setWSModel wsModel model
        |> withNoMsg

    | UpdateWSModel updateFn ->
        updateWSModel updateFn model
        |> withNoMsg

    | UpdateModel( updateFn: Model -> Model) ->
        updateFn model, Cmd.none

    | DispatchDelayed (timeInMs, msg) ->
          let delayedCmd (dispatch: Msg -> unit) : unit =
              let delayedDispatch = async {
                  do! Async.Sleep timeInMs
                  dispatch msg
              }

              Async.StartImmediate delayedDispatch

          model, Cmd.ofEffect delayedCmd

    | RequestCircuitCheck ->
        // The view asks whenever it sees a stale verdict, which is on every render until a new one
        // arrives - so an outstanding check absorbs all of them and the burst costs one flatten.
        if model.CircuitCheck.CheckPending then
            model, Cmd.none
        else
            model
            |> Optic.set (circuitCheck_ >-> checkPending_) true
            |> (fun model -> model, Cmd.ofMsg (DispatchDelayed(Constants.circuitCheckDelayMs, RunCircuitCheck)))

    | RunCircuitCheck ->
        // Whatever the design is NOW, not what it was when the check was asked for: edits made
        // during the delay are what the delay is for. If it has changed again since, the view sees
        // a stale verdict on the next render and asks once more.
        model
        |> Optic.set circuitCheck_ (runCircuitCheck model)
        |> withNoMsg

    | UpdateImportDecisions importDecisions' ->
        let updatedModel = 
            model
            |> set (popupDialogData_ >-> importDecisions_) importDecisions'
       
        updatedModel, Cmd.none

    | StartWaveSimulation ->
        // The waveform viewer's Start and Refresh buttons, and the restart after an error fix:
        // the whole stop-then-start sequence, in this update, on the model as it is now.
        WaveSimTop.startWaveSimulation model

    | RefreshWaveSim ->
        // restart the wave simulator after design change etc that invalidates all waves. No
        // payload: the refresh reads the model as it IS, which is what keeps a queued refresh
        // harmless - each acts on current state, so a superseded one changes nothing.
        WaveSimTop.refreshWaveSim true model

    | AddWSModel (sheet, wsModel) ->
        model
        |> map waveSim_ (Map.add sheet wsModel)
        |> withNoMsg

    | GenerateWaveforms transform ->
        // change the wave viewer's state, then refresh its waveforms to match
        updateWSModel transform model
        |> WaveSimTop.refreshWaveSim false

    | GenerateCurrentWaveforms ->
        // Update the wave simulator with new waveforms based on current WsModel
        WaveSimTop.refreshWaveSim false model

    | CancelWaveSimulation ->
        // The simulation-extension loop re-arms itself through RunAfterRenderWithSpinner, so
        // cancelling means clearing that continuation along with the spinner - nothing else
        // stops it, which is why the Cancel button that merely closed the popup did not work.
        // Everything already simulated is kept: the cursor is put on the last simulated cycle,
        // which also scrolls the view to end there, and regenerates the waveforms for exactly
        // the cycles that exist.
        let model = { model with SpinnerPayload = None; RunAfterRenderWithSpinner = None }
        let fs = Simulator.getFastSim ()

        if fs.SimulatedTopSheet = "" then
            model |> withNoMsg
        else
            // The clock of whichever simulator is running. fs.ClockTick is the renderer's own, and
            // is zero however far the sidecar has run, so cancelling threw the cursor back to
            // cycle 0 rather than leaving it on the last cycle that exists.
            model
            |> withMsg (WaveSimNavigation.setClkCycleMsg (getWSModel model) (WaveProvider.cyclesSimulated model.SimulateInRenderer model.SidecarSession.Clock fs))

    | SetWaveGroupSelectionOpen (fIdL, show) ->
        model
        |> updateWSModel (fun ws -> WaveSimStyle.setWaveGroupSelectionOpen ws fIdL show)
        |> withNoMsg

        
    | SetWaveSheetSelectionOpen (fIdL, show) ->       
        model
        |> updateWSModel (fun ws -> WaveSimStyle.setWaveSheetSelectionOpen ws fIdL show)
        |> withNoMsg    

    | TryStartSimulationAfterErrorFix simType ->
        StepSimulationTop.tryStartSimulationAfterErrorFix simType model

    | SetSimulationGraph (graph, fastSim) ->
        let simData =
            getSimulationDataOrFail model "SetSimulationGraph"
            |> (set graph_ graph >> set fastSim_ fastSim)
            |> Ok |> Some
        model
        |> set currentStepSimulationStep_ simData
        |> withNoMsg

    | SetSimulationBase numBase ->
        let simData =
            getSimulationDataOrFail model "SetSimulationBase"
            |> set numberBase_ numBase
        model
        |> set currentStepSimulationStep_ (simData |> Ok |> Some)
        |> withNoMsg

    | IncrementSimulationClockTick n ->
        let simData =
            getSimulationDataOrFail model "IncrementSimulationClockTick"
            |> map clockTickNumber_ (fun x -> x+n)
        model
        |> set currentStepSimulationStep_ (simData |> Ok |> Some )
        |> withNoMsg

    | EndSimulation ->
        // Releasing the slot matters: it is a module-level mutable holding a whole FastSimulation,
        // only otherwise replaced when the NEXT simulation is built, so an unreleased design costs
        // roughly a second per gigabyte retained on every major garbage collection - which is what
        // made editing feel slow after simulating a large design, with nothing on screen to say
        // why.
        //
        // Released only when the slot holds the STEP simulation this message ends - which the
        // model says, by identity. There is one slot now, and this message is also dispatched by
        // startWaveSimulation for mutual exclusion, arriving AFTER the wave build has taken the
        // slot: clearing unconditionally would destroy the simulation being started. Under mutual
        // exclusion the identity test is exact - the slot holds this step sim's build, or
        // something newer that must be kept.
        (match model.CurrentStepSimulationStep with
         | Some(Ok sd) when System.Object.ReferenceEquals(sd.FastSim, Simulator.simCache.FastSim) ->
             Simulator.simCache <- Simulator.simCacheInit ()
             PortData.forget ()
         | _ -> ())
        // The indexes built over a simulation are memoised on the simulation itself, so an
        // unemptied memo holds the whole of it - step arrays and all - after this has let go.
        Helpers.clearIdentityMemos()
        model
        |> set currentStepSimulationStep_ None
        // as removeAllSimulationsFromModel: the in-flight operations were for the simulation
        // this ends, and emptying the table is what discards their replies
        |> set sidecarInFlight_ Map.empty
        |> withNoMsg

    | EndWaveSim ->
        // The mirror of EndSimulation's release: let the slot go unless it holds a live STEP
        // simulation's build - which it can, since ending the waveform state is also part of
        // starting a step simulation, and by then the slot already holds the new build.
        (match model.CurrentStepSimulationStep with
         | Some(Ok sd) when System.Object.ReferenceEquals(sd.FastSim, Simulator.simCache.FastSim) -> ()
         | _ ->
             Simulator.simCache <- Simulator.simCacheInit ()
             PortData.forget ())

        match model.WaveSimSheet with
        | None
        | Some "" ->
            Log.warn "cannot end the waveform simulation: it has already ended"
            endWaveSimulation model, Cmd.none
        | Some _ -> endWaveSimulation model, Cmd.none

    | SidecarOpStarted(seq, op) ->
        // Into the table, where anything asking whether the wire is free can see it. This is the
        // one state a synchronous question about the simulator cannot be answered in, and the
        // reason it is in the model rather than beside it: the UI must draw it, and a belief the
        // view can only reach through a side channel is one the view cannot draw from.
        model |> Optic.map sidecarInFlight_ (Map.add seq op) |> withNoMsg

    | SidecarReply(seq, answer) ->
        // The number says which operation, the table says what it was, and what to do follows from
        // the two - which is why there is one of these rather than a message per feature.
        let op = Map.tryFind seq model.SidecarInFlight
        let model = model |> Optic.map sidecarInFlight_ (Map.remove seq)

        match op, answer with
        | None, _ ->
            // an answer to an operation nothing is waiting for: the simulation it was asked for
            // ended while it was in flight, and the table was emptied with it
            model |> withNoMsg

        | Some(OpBuild(top, arraySize)), AnsBuilt(Ok epoch) ->
            // From this moment instance views answer from the slices the sidecar has been asked
            // for. WHICH instances need asking is derived from the model by describeWhatIsShown,
            // which runs at the end of this update like it runs after every message - so nothing
            // here has to know what the selection or the selector currently references.
            PortData.startEpoch (Simulator.getFastSim ()) epoch

            model
            |> set sidecarSession_ (Session(top, arraySize, epoch, 0))
            |> set sidecarBuildEndedMs_ (TimeHelpers.getTimeMs ())
            // a step run that started before there was a session issued this build; now there is
            // one, its first chunk goes out. Does nothing when no run is wanted.
            |> SimulationView.continueStepRun

        | Some(OpPorts _), AnsPorts(Ok described) ->
            Log.dbg Log.Wave $"described {described} instances for the wave selector"
            // whatever was waiting on a description - an unresolved wave, an empty selector row -
            // resolves on the next refresh, so ask for one if a waveform view is up
            model
            |> withNoMsg
            |> fun (model, cmd) ->
                match model.WaveSimSheet with
                | Some _ -> model, Cmd.batch [ cmd; Cmd.ofMsg GenerateCurrentWaveforms ]
                | None -> model, cmd

        | Some(OpPorts _), AnsPorts(Error e) ->
            Log.error $"the .NET simulator could not describe the design's instances: {e}"
            model |> withNoMsg

        | Some(OpRunForWaves _), AnsRan(Ok(clock, _)) ->
            // The clock it reached, and nothing else. Whether another chunk is needed is worked
            // out where every other decision about this session is - fetchWhatIsMissing, which
            // runs after this message like it runs after every other, compares this clock with
            // what the view needs and issues the next chunk or the read. So cancelling a run is
            // not a thing that has to reach into anything: it is this function no longer being
            // asked for another chunk.
            model
            |> Optic.map sidecarSession_ (fun session ->
                match session with
                | Session(top, size, epoch, _) -> Session(top, size, epoch, clock)
                | other -> other)
            |> set sidecarRunEndedMs_ (TimeHelpers.getTimeMs ())
            |> withNoMsg

        | Some(OpRunForWaves _), AnsRan(Error e) ->
            Log.error $"the .NET simulator could not run the design: {e}"
            model |> set sidecarRunEndedMs_ (TimeHelpers.getTimeMs ()) |> withNoMsg

        | Some(OpBuild _), AnsBuilt(Error e) ->
            Log.error $"the .NET simulator could not build the design: {e}"

            // The refusal is written for the user - a design too large to simulate says what to
            // set the cycle count to - so it goes where a simulation error is shown, not only to
            // the log. Silently blank waveforms over a refusal the user never sees are what this
            // used to be.
            let shown: SimulationError =
                { ErrType = SimGraphTypes.GenericSimError e
                  InDependency = None
                  ComponentsAffected = []
                  ConnectionsAffected = [] }

            model
            |> set sidecarSession_ (SessionFailed e)
            |> set sidecarBuildEndedMs_ (TimeHelpers.getTimeMs ())
            |> updateWSModel (fun ws -> { ws with State = SimError shown })
            |> withNoMsg

        | Some(OpFetch snapshot), AnsFetched(result, ramRows, probed) ->
            // The entry has already left the table above, whether the fetch worked or not, so the
            // refresh below is free to ask for whatever is still missing. After a view that moved
            // while this fetch was in the air, that is the view the user is now looking at.
            let model =
                model
                // What the probe asked for and what came back, together, so the label is drawn
                // only where the value on screen is the value that was asked for. None where the
                // simulation could not give one, which draws no label rather than a stale one.
                |> Optic.set probeRead_ (Option.orElse model.ProbeRead probed)
                |> updateWSModel (fun ws ->
                    { ws with
                        // whatever RAM rows came back with the waves. Held in the model rather than
                        // beside it because the pane is memoised on the model: rows arriving anywhere
                        // else would not redraw the table they belong to.
                        RamRows =
                            (ws.RamRows, ramRows)
                            ||> List.fold (fun rows (ram, held) -> Map.add ram held rows)
                        // a fetch that worked clears the backoff, so the next failure gets the full
                        // wait rather than whatever is left of an older one
                        FetchFailedAtMs =
                            match result with
                            | Ok() -> 0.0
                            | Error _ -> TimeHelpers.getTimeMs () })

            // a step run parked while this read held the wire continues now (nothing, otherwise)
            let model, continueRun = SimulationView.continueStepRun model

            match result with
            | Ok() ->
                // What arrived is in the caches, and the records say WHAT the fetch was for -
                // the snapshot it carried, never what is current by then. If the view moved
                // while it flew, the next end-of-update comparison sees the difference and
                // fetches again; nothing diffs, nothing is tracked.
                let model =
                    model
                    |> (match snapshot.SnapData with
                        | Some vp -> set fetchedData_ (Some vp)
                        | None -> id)
                    |> (match snapshot.SnapStructure with
                        | Some sv -> set fetchedStructure_ (Some sv)
                        | None -> id)
                    |> set failedFetch_ None

                WaveSimTop.refreshWaveSim false model
                |> fun (model, cmd) -> model, Cmd.batch [ cmd; continueRun ]
            | Error e ->
                // A fetch that fails now means a fault rather than a wait: the transport waits for a
                // sidecar that is still starting, so what is left is a session that no longer exists, a
                // design that would not build, or a sidecar that has died. Say so and stop - a fault
                // asked again is a fault again, and the viewer's banner is what tells the user that
                // what is on screen is not what the numbers above it say.
                Log.error $"the .NET simulator could not answer for this view: {e}"
                // the failed snapshot is remembered so an UNCHANGED viewport does not retry at
                // wire speed; any change to it is a different snapshot and tries again
                WaveSimTop.cancelSpinner (set failedFetch_ (Some snapshot) model), continueRun

        | Some(OpStep _), AnsStepped -> model |> withNoMsg

        | Some(OpStep _), AnsSteppedTo(before, t1, result) ->
            match result, model.CurrentStepSimulationStep with
            | Ok(reached, _), Some(Ok simData) ->
                // The clock is the simulator's FACT, written as reported - never incremented
                // towards. The accumulation this replaces is how the model's belief could drift
                // past the simulator's clock and show a negative speed.
                let dt = TimeHelpers.getTimeMs () - t1
                let nComps = float simData.FastSim.Design.ExpandedComponentCount
                let speed = if dt <= 0.0 then 0.0 else (float reached - float before) * nComps / dt

                model
                |> set currentStepSimulationStep_ (Some(Ok { simData with ClockTickNumber = reached }))
                |> Optic.map sidecarSession_ (fun session ->
                    match session with
                    | Session(top, size, epoch, _) -> Session(top, size, epoch, reached)
                    | other -> other)
                |> map
                    (popupDialogData_ >-> progress_)
                    (Option.map (fun bar ->
                        let sinceStart =
                            model.StepRunTarget
                            |> Option.map (fun p -> max 0 (reached - p.InitialClock))
                            |> Option.defaultValue bar.Value

                        { bar with Value = sinceStart; Speed = speed }))
                // the fact is recorded; the next chunk - or the finish - is issued from here,
                // which with StartStepRun and the other two reply handlers is the WHOLE set of
                // places the run continues from
                |> SimulationView.continueStepRun
            | Error e, _ ->
                Log.error $"the .NET simulator could not run the step simulation: {e}"
                model |> set stepRunTarget_ None |> withNoMsg
            | _ -> model |> withNoMsg

        | Some op, answer ->
            Log.warn $"the .NET simulator answered %A{op} with %A{answer}, which does not belong to it"
            model |> withNoMsg

    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editMsgs = [inferMsg; ClosePropertiesNotification]

        model
        |> set rightPaneTabVisible_ newTab
        |> withMsgs
                (match newTab with 
                | Properties 
                | Catalogue 
                | Simulation 
                | Build -> editMsgs
                | Transition -> [])

    | ChangeSimSubTab subTab ->
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editMsgs = [inferMsg; ClosePropertiesNotification] 
        model
        |> set simSubTabVisible_ subTab
        |> withMsgs editMsgs


    | ChangeBuildTabVisibility ->
        model
        |> map buildVisible_ not
        |> withNoMsg

    | SetHighlighted (componentIds, connectionIds) ->
        SheetUpdate.update (SheetT.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model

    | SetSelWavesHighlighted connIds ->
        SheetUpdate.update (SheetT.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model

    | SetClipboard components ->
        { model with Clipboard = components }
        |> withNoMsg

    | SetCreateComponent pos ->
        { model with LastCreatedComponent = Some pos}
        |> withNoMsg

    | SetProject project ->
        // Which library components have been opened for viewing is deliberately forgotten when
        // the project changes: looking inside one is meant to last no longer than the session
        // that asked for it, so a project reopened is a project with its abstractions intact.
        let leavingProject =
            match model.CurrentProj with
            | Some p -> p.ProjectPath <> project.ProjectPath
            | None -> true
        model
        |> set currentProj_ (Some project)
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath
        |> (fun model ->
                if leavingProject then
                    model
                    |> set openedLibrarySheets_ Set.empty
                    |> set readOnlyBaseline_ None
                else model)
        |> withNoMsg

    | UpdateProject update ->
        CustomCompPorts.updateProjectFiles true update model
        |> withNoMsg

    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model
        |> withNoMsg

    | CheckTopSheetChoice ->
        match model.PopupViewFunc with
        | Some _ -> model |> withNoMsg
        | None ->
            match ParameterView.topSheetChoiceCheck model with
            | None -> model |> withNoMsg
            | Some popup ->
                model
                |> set popupViewFunc_ (Some popup)
                |> withNoMsg

    | PropagateParameters ->
        // the push works by dispatching symbol-change messages, so it needs a dispatch of its own
        model, Cmd.ofEffect (fun dispatch -> ParameterView.propagateParameters model dispatch)

    | ShowPopup popup ->
        model
        |> set popupViewFunc_ (Some popup)
        |> withNoMsg

    | ShowStaticInfoPopup(title, body, dispatch) ->
        let foot = div [] []
        PopupHelpers.closablePopup title body foot [Width 800] dispatch
        model
        |> withNoMsg

    | ClosePopup ->
        // A drop position outlives the drop precisely so that a creation popup can stand between
        // the two, so it must die with that popup. Cancelling would otherwise leave it to be
        // picked up by the NEXT placement, which would land where this one was abandoned. On the
        // accepting path it has already been consumed and cleared, and clearing again costs
        // nothing.
        { model with
            DragPlacement = None
            // closing the project browser is what stops its refresh timer: the next tick finds
            // this None and does not schedule another
            ProjectBrowser = None
            PopupViewFunc = None;
            CodeEditorState = None
            PopupDialogData =
                    { model.PopupDialogData with
                        Text = None;
                        ImportDecisions = Map.empty;
                        Int = None;
                        Int2 = None;
                        DialogState = None;
                        MemorySetup = None;
                        MemoryEditorData = None;
                        VerilogCode = None;
                        VerilogErrors = [];
                    }}
        |> withNoMsg

    | SetPopupDialogText text ->
        model
        |> set (popupDialogData_ >-> text_) text
        |> withNoMsg

    | SetPopupDialogText2 text ->
        model
        |> set (popupDialogData_ >-> text2_) text
        |> withNoMsg

    | SetPopupDialogBadLabel isBad ->
        model
        |> set (popupDialogData_ >-> badLabel_) isBad
        |> withNoMsg

    | SetPopupDialogCode code ->
        model
        |> set (popupDialogData_ >-> verilogCode_) code
        |> withNoMsg

    | SetPopupDialogVerilogErrors errorList ->
        model
        |> set (popupDialogData_ >-> verilogErrors_) errorList
        |> withNoMsg

    | SetPopupDialogInt int ->
        model
        |> set (popupDialogData_ >-> int_) int
        |> withNoMsg

    | SetPopupDialogInt2 int ->
        set (popupDialogData_ >-> int2_) int model, Cmd.none

    | SetPopupDialogInt3 i -> set (popupDialogData_ >-> int3_) i model, Cmd.none
   

    | SetPopupDialogTwoInts (n, select, optText)->
        model
        |> map popupDialogData_
                    (match select with
                     | FirstInt -> set int_ (Option.map int32 n)
                     | SecondInt -> set int2_ n
                     | ThirdInt -> set int3_ n)
        |> withNoMsg
    
    | SetPopupDialogIntList intlist->
        model
        |> set (popupDialogData_ >-> intlist_) intlist
        |> withNoMsg

    | SetPopupDialogIntList2 intlist2->
        model
        |> set (popupDialogData_ >-> intlist2_) intlist2
        |> withNoMsg

    | AddPopupDialogParamSpec (slot, boxState) ->
        let paramInputs_ = popupDialogData_ >-> paramCompSpec_
        let newInputs = 
            model
            |> get paramInputs_
            |> Option.defaultValue Map.empty
            |> Map.add slot boxState
        model
        |> set paramInputs_ (Some newInputs)
        |> withNoMsg

    | ClearPopupDialogParamSpec slot ->
        let paramInputs_ = popupDialogData_ >-> paramCompSpec_
        let newInputs = 
            model
            |> get paramInputs_
            |> Option.defaultValue Map.empty
            |> Map.remove slot
        model
        |> set paramInputs_ (Some newInputs)
        |> withNoMsg

    | SetPopupDialogMemorySetup m ->
        model
        |> set (popupDialogData_ >-> memorySetup_) m
        |> withNoMsg

    | SetPopupMemoryEditorData m ->
        model
        |> set (popupDialogData_ >-> memoryEditorData_) m
        |> withNoMsg

    | SetPopupProgress progOpt ->
        let model = set (popupDialogData_ >-> progress_) progOpt model

        match progOpt with
        // Closing the bar IS cancelling the run: the pipeline issues chunks only while the bar
        // is up, so there is nothing to reach into and stop - it is simply no longer asked.
        | None -> set stepRunTarget_ None model, Cmd.none
        | Some _ -> model, Cmd.none

    | StartStepRun prog ->
        // The command that STARTS the run's cascade: target into the model, the clock to the
        // run's start - a backward jump is a restart from the beginning, of the panel's clock
        // here and of the simulator when the first chunk asks for a cycle behind its own - and
        // the first piece of work issued. From here each completion's handler issues the next.
        model
        |> set stepRunTarget_ (Some prog)
        |> (fun model ->
            match model.CurrentStepSimulationStep with
            | Some(Ok _) ->
                let simData = getSimulationDataOrFail model "StartStepRun"
                set currentStepSimulationStep_ (Some(Ok { simData with ClockTickNumber = prog.InitialClock })) model
            | _ -> model)
        |> SimulationView.continueStepRun

    | UpdatePopupProgress updateFn ->
        model
        |> map (popupDialogData_ >-> progress_) (Option.map updateFn)
        |> withNoMsg

    | SimulateWithProgressBar simPars ->
        SimulationView.simulateWithProgressBar simPars model

    | SetSelectedComponentMemoryLocation (addr,data) ->
        model
        |> map selectedComponent_ (updateComponentMemory addr data)
        |> withNoMsg

    | CloseDiagramNotification ->
        model
        |> set (notifications_ >-> fromDiagram_) None
        |> withNoMsg

    | SetSimulationNotification n ->
        model
        |> set (notifications_ >-> fromSimulation_) (Some n)
        |> withNoMsg
    | CloseSimulationNotification ->
        model
        |> set (notifications_ >-> fromSimulation_) None
        |> withNoMsg

    | CloseWaveSimNotification ->
        model
        |> set (notifications_ >-> fromWaveSim_) None
        |> withNoMsg

    | SetFilesNotification n ->
        model
        |> set (notifications_ >-> fromFiles_) (Some n)
        |> withNoMsg

    | CloseFilesNotification ->
        model
        |> set (notifications_ >-> fromFiles_) None
        |> withNoMsg

    | SetMemoryEditorNotification n ->
        model
        |> set (notifications_ >-> fromMemoryEditor_) (Some n)
        |> withNoMsg

    | CloseMemoryEditorNotification ->
        model
        |> set (notifications_ >-> fromMemoryEditor_) None
        |> withNoMsg

    | SetPropertiesNotification n ->
        model
        |> set (notifications_ >-> fromProperties_) (Some n)
        |> withNoMsg

    | ClosePropertiesNotification ->
        model
        |> set (notifications_ >-> fromProperties_) None
        |> withNoMsg        

    | SetTopMenu t ->
        { model with TopMenuOpenState = t}
        |> withNoMsg

    | SetSheetMenuPinned pinned ->
        model
        |> set sheetMenuPinned_ pinned
        |> withNoMsg

    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model)
        |> withNoMsg

    | ExecCmd cmd ->
        model, cmd

    | ExecFuncAsynch func ->
             let cmd' =
                // Elmish 4 dropped OfAsyncImmediate.result: perform with id maps the returned
                // message through unchanged, which is the same thing
                Elmish.Cmd.OfAsyncImmediate.perform (fun () -> async {
                //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
                //this number only seems to affect the wavesim spinner cursor, it does not help with open project/change sheet spinner cursor
                    do! (Async.Sleep 100)
                    let cmd = func ()
                    return (ExecCmd cmd)}) () id
             model, cmd'

    | ExecCmdAsynch cmd ->
        let cmd' =
            Elmish.Cmd.OfAsyncImmediate.perform (fun () -> async {
            //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
            //this number only seems to affect the wavesim spinner cursor.
                do! (Async.Sleep 300)
                return (ExecCmd cmd)}) () id
        model, cmd'

    | SendSeqMsgAsynch msgs ->
        model, SimulationView.doBatchOfMsgsAsynch msgs

    | MenuAction(act,dispatch) ->
        match act with 
        | MenuSaveFile -> getMenuView act model dispatch, Cmd.ofMsg (Sheet SheetT.SaveSymbols)
        | MenuSaveProjectInNewFormat -> getMenuView act model dispatch, Cmd.ofMsg (Sheet SheetT.SaveSymbols)
        | _ -> getMenuView act model dispatch, Cmd.none

    | ContextMenuAction e ->
        let menuType = getContextMenu e model
        Bridge.showContextMenu menuType
        model, Cmd.none

    | ContextMenuItemClick(menuType, item, dispatch) ->
        processContextMenuClick menuType item dispatch model


    | DiagramMouseEvent ->
        model, Cmd.none // this now does nothing and should be removed

    | SelectionHasChanged -> 
        { model with ConnsOfSelectedWavesAreHighlighted = true }
        |> withNoMsg

    | SetIsLoading b ->
        let cmd = if b then Cmd.none else Cmd.ofMsg (Sheet (SheetT.SetSpinner false)) //Turn off spinner after project/sheet is loaded
        {model with IsLoading = b}, cmd

    | ReadUserData userAppDir ->
        Log.dbg Log.Files $"user app directory is {userAppDir}"
        let model,cmd = readUserData userAppDir model        
        model,cmd

    | SetUserData (data: UserData) ->
        model
        |> set userData_  data
        |> userDataToDrawBlockModel
        |> writeUserData
        |> withNoMsg

    | SetThemeUserData (theme: DrawModelType.SymbolT.ThemeType) ->
        let model =
            {model with UserData = {model.UserData with Theme=theme}}
            |> userDataToDrawBlockModel
        model, Cmd.none

    | ExecutePendingMessages n ->
        executePendingMessagesF n model

    | TruthTableMsg ttMsg ->
        TruthTableUpdate.truthTableUpdate model ttMsg

    | ScrollbarMouseMsg (cursor: float, action: ScrollbarMouseAction, dispatch: Msg->unit) ->
        let wsm = Map.find (Option.get model.WaveSimSheet) model.WaveSim
        WaveSimNavigation.updateScrollbar wsm dispatch cursor action
        model, Cmd.none

    | CodeEditorMsg codeMsg ->
        Editor.update codeMsg model

    // Various messages here that are not implemented as yet, or are no longer used
    // should be sorted out
    | LockTabsToWaveSim | UnlockTabsFromWaveSim | SetExitDialog _ 
    | SetPropertiesExtraDialogText _ | SetRouterInteractive _ 
    | ShowExitDialog -> model, Cmd.none
    | DoNothing -> //Acts as a placeholder to propagate the ExecutePendingMessages message in a Cmd
        model, cmd

    | JSDiagramMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none

    // post-processing of update function (Model * Cmd<Msg>)
    |> map fst_ (fun model' -> resetDialogIfSelectionHasChanged model' oldModel)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible

/// Start a function the model has asked to run after the render it has just caused - see
/// `Model.RunAfterRenderWithSpinner` - once that render is ON SCREEN.
///
/// What runs here blocks the thread for as long as it takes, and building a wave simulation is
/// seconds, so the render that has just switched a spinner on has to have been painted before it
/// starts or the spinner only ever reaches the DOM. Being rendered does not mean being painted:
/// Elmish calls React's `root.render` from an animation frame, but a `createRoot` render commits in
/// a task of React's own afterwards, and a timer started from there can be the very next task, with
/// no frame - and so no paint - in between.
///
/// `requestAnimationFrame` fires at the start of a frame's rendering steps. The first lands in the
/// frame Elmish rendered from and the second in the frame after React's commit, so a timer started
/// from inside the second runs once the frame carrying that commit has been painted. Waiting on the
/// browser rather than on a millisecond count is what makes this hold on a slow machine as well as
/// a fast one.
///
/// This is why starting a waveform simulation showed its spinner the first time and never again: a
/// first start commits a far larger change than a restart, and only that took long enough to be
/// painted before the build began.
let private runWhenPainted (request: RunData) : Cmd<Msg> =
    Cmd.ofEffect (fun dispatch ->
        let run () =
            DispatchDelayed(0, UpdateModel(fun model ->
                // What is pending may have been replaced since this was scheduled, by a later
                // request or by the previous one finishing. Only the request this was scheduled for
                // may run, and it may run once.
                match model.RunAfterRenderWithSpinner with
                | Some pending when System.Object.ReferenceEquals(pending, request) ->
                    { model with RunAfterRenderWithSpinner = None } |> request.FnToRun dispatch
                | _ -> model))
            |> dispatch
        Browser.Dom.window.requestAnimationFrame (fun _ ->
            Browser.Dom.window.requestAnimationFrame (fun _ -> run ()) |> ignore)
        |> ignore)

/// Schedule whatever after-render function this update has newly asked to run.
///
/// One place rather than each of the five that ask, because what they need is not "call me back"
/// but "call me back when the user can see this", and none of them should have to know how a model
/// change reaches the screen.
let private scheduleAfterRender (oldModel: Model) (model: Model, cmd: Cmd<Msg>) =
    match oldModel.RunAfterRenderWithSpinner, model.RunAfterRenderWithSpinner with
    | _, None -> model, cmd
    // the same request as before is already scheduled: an update that leaves it alone is not a new
    // ask, and scheduling it again would run it twice
    | Some before, Some after when System.Object.ReferenceEquals(before, after) -> model, cmd
    | _, Some request -> model, Cmd.batch [ cmd; runWhenPainted request ]

/// Main MVU model update function.
///
/// Wraps the dispatch so that a library sheet opened for viewing is held at the state it loaded
/// with - see ModelHelpers.pinDrawBlock. It has to be done here rather than inside Sheet.update:
/// the three draw block update functions nest, so gating there would catch every message, but a
/// handful of places edit model.Sheet directly with Optic.map and reach none of them - the
/// separate/reroute wires actions in Renderer.fs, KeyBindings.fs and TopMenuView.fs, two context
/// menu items in UpdateHelpers.fs, the simulator's "save input values as default", and the
/// memory refresh that runs on every message. All of those pass through here.
let update (msg : Msg) oldModel =
    updateUnpinned msg oldModel
    |> map fst_ (ModelHelpers.pinIfReadOnly msg)
    |> scheduleAfterRender oldModel
    |> WaveSimTop.sidecarChecks
