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


    


//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update (msg : Msg) oldModel =

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
    | RunAfterRender( withSpinner, fn) ->
        {model with RunAfterRenderWithSpinner = Some {FnToRun=fn; ButtonSpinnerOn = withSpinner}}, Cmd.none

    | ChangeWaveSimMultiplier key ->
        let table = Constants.multipliers
        if key < 0 || key >= table.Length then
            printf $"Warning: Can't change multiplier to key = {key}"
            model, Cmd.none   
        else
           match model.WaveSimSheet with
           | None ->
                printfn "Warning: can't change multiplier when there is no WaveSim sheet!"
                model, Cmd.none
           | Some sheet ->
                model
                |> Optic.map waveSim_ (fun ws ->
                    let wsModel = ws[sheet]
                    Map.add sheet (WaveSimNavigation.changeMultiplier (table[key]) wsModel) ws)
                |> (fun model -> WaveSimTop.refreshWaveSim false (getWSModel model) model)
                

    | CheckMemory ->
        if JSHelpers.loggingMemory then
            let heapInBytes = JSHelpers.getProcessPrivateMemory()
            let hint = [$"{heapInBytes} MB"]
            printfn $"Heap size {hint}"
            {model with Sheet.Wire.Symbol.HintPane = Some hint}, Cmd.none
        else
            model, Cmd.none
    | SaveModel ->
        model, Cmd.none
    | FileCommand(fc,dispatch) ->
        FileUpdate.fileCommand fc dispatch model 
    | StartUICmd uiCmd ->
        //printfn $"starting UI command '{uiCmd}"
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
        //printfn $"ending UI command '{model.UIState}"
        //printf $"***UI Command: %.2f{TimeHelpers.getTimeMs() - uiStartTime} ***"
        let popup = CustomCompPorts.optCurrentSheetDependentsPopup model
        {model with UIState = None; PopupViewFunc = popup}
        |> withMsg (Sheet (SheetT.SetSpinner false))

    | CloseApp ->
        exitApp model
        model, Cmd.none

    | Sheet sMsg ->
        match sMsg, model.PopupViewFunc with
        | SheetT.ToggleNet canvas, _ ->
            model, Cmd.none
        | SheetT.KeyPress _, Some _ -> 
            // do not allow keys to affect Sheet when popup is on.
            model, Cmd.none
        | SheetT.ManualKeyDown s, _ when s = "ArrowLeft" || s = "ArrowRight" ->
            // intercept these keys and send them to the wave simulator
            // needed because electron does not propagate onkeydown events through the DOM
            // so we can't use the normal Elmish keydown handling
            model, Cmd.ofMsg (WaveSimKeyPress s)
        | _ -> sheetMsg sMsg model

    | WaveSimKeyPress s ->
        // These keys implement navigation in teh Waveform simulator
        let wsModel = getWSModel model
        let moveCursorMsg num  = WaveSimNavigation.setClkCycleMsg wsModel (wsModel.CursorExactClkCycle + num)
        if model.MousePointerIsOnRightSection then
            let cmd =
                match wsModel.State, s with
                |Success, "ArrowLeft" -> Cmd.ofMsg (moveCursorMsg -1)
                |Success, "ArrowRight" -> Cmd.ofMsg (moveCursorMsg 1)
                | _ -> Cmd.none
            model, cmd
        else
            model, Cmd.none


    | SynchroniseCanvas ->
        // used after drawblock components are centred on load to enusre that Issie CanvasState is updated
        // This may be needed if Ctrl/w on load moves the whole draw block sheet circuit to centre it.
        // in this case we do not want the save button to be active, because moving the circuit is not a "real" change
        // updating loaded component CanvasState to equal draw bloack canvasstate will ensure the button stays inactive.
        let canvas = model.Sheet.GetCanvasState ()
        //printf "synchronising canvas..."
        // this should disable the saev button by making loadedcomponent and draw blokc canvas the same
        model
        |> map openLoadedComponentOfModel_ (fun ldc -> {ldc with CanvasState = canvas})
        |> withNoMsg
        
    // special messages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode ->
        {model with DividerDragMode= mode}
        |> withNoMsg

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
                printfn "Benchmarking on %20s, stepArraySize %8d, step %8d, warmup %3d, repeat %3d" (dirName p.ProjectPath) SimulationView.Constants.maxArraySize step warmup simulationRound

                p.LoadedComponents
                |> List.map (fun c ->
                    let simData = Simulator.startCircuitSimulation SimulationView.Constants.maxArraySize c.Name c.CanvasState p.LoadedComponents

                    match simData with
                    | Error err -> failwithf "Error occured when running startCircuitSimulation on %A, %A" c.Name err
                    | Ok simData ->
                        let comps = simData.FastSim.FComps.Values |> Seq.filter (fun fc -> match fc.FType with | IOLabel -> false | _ -> true) |> Seq.length
                        printfn "Benchmarking with component: %s" c.Name

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
                            let speed = float (comps * step) / time
                            printfn "simulated %20s for %5d steps with %4d effective components, simulation finished in %8.3fms, average simulation speed: %10.3f (comp * step / ms)" c.Name step comps time speed
                            speed))
                |> geometricMean

            | None -> failwith "No project loaded, please load a project to benchmark"

        [ 1..benchmarkRound ]
        |> List.map (fun i -> benchmark i)
        |> printfn "Geometric mean of simulation speed of ISSIE on current project: %A"

        model, Cmd.none

    | StartSimulation simData -> 
        {model with CurrentStepSimulationStep = Some simData }
        |> withNoMsg

    | SetWSModel wsModel ->
        setWSModel wsModel model
        |> withNoMsg

    | UpdateWSModel updateFn ->
        updateWSModel updateFn model
        |> withNoMsg

    | SetWSModelAndSheet (wsModel, wsSheet) ->
        model
        |> set waveSimSheet_ (if wsSheet = "" then None else Some wsSheet)
        |> setWSModel wsModel
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

          model, Cmd.ofSub delayedCmd

    | UpdateImportDecisions importDecisions' ->
        let updatedModel = 
            model
            |> set (popupDialogData_ >-> importDecisions_) importDecisions'
       
        updatedModel, Cmd.none

    | RefreshWaveSim ws ->
        // restart the wave simulator after design change etc that invalidates all waves
        WaveSimTop.refreshWaveSim true ws model

    | AddWSModel (sheet, wsModel) ->
        model
        |> map waveSim_ (Map.add sheet wsModel)
        |> withNoMsg

    | GenerateWaveforms ws ->
        // Update the wave simulator with new waveforms
        // Is called whenever any waveform might need to be changed
        WaveSimTop.refreshWaveSim false ws model

    | GenerateCurrentWaveforms ->
        // Update the wave simulator with new waveforms based on current WsModel
        let ws = getWSModel model
        WaveSimTop.refreshWaveSim false ws model

    | SetWaveComponentSelectionOpen (fIdL, show) ->       
        model
        |> updateWSModel (fun ws -> WaveSimStyle.setWaveComponentSelectionOpen ws fIdL show)
        |> withNoMsg

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
        model
        |> set currentStepSimulationStep_ None
        |> withNoMsg

    | EndWaveSim -> 
        let model =
            Simulator.simCacheWS <- Simulator.simCacheInit()
            let model = removeAllSimulationsFromModel model
            match model.WaveSimSheet with
            | None | Some "" -> 
                printfn "What? can't end WaveSim when it is already ended"
                model
            | Some sheet -> 
                { model with 
                    WaveSimSheet = None; 
                    WaveSim = Map.change sheet (Option.map (fun ws ->
                        {ws with  State = Ended ; WaveModalActive = false})) model.WaveSim
                }
        model, Cmd.none

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
        printf $"Setting project with component: '{project.OpenFileName}'"
        model
        |> set currentProj_ (Some project) 
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath
        |> withNoMsg

    | UpdateProject update ->
        CustomCompPorts.updateProjectFiles true update model
        |> withNoMsg

    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model
        |> withNoMsg

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
        { model with
            PopupViewFunc = None;
            PopupDialogData =
                    { model.PopupDialogData with
                        Text = None;
                        ImportDecisions = Map.empty;
                        Int = None;
                        Int2 = None;
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

    | SetPopupDialogMemorySetup m ->
        model
        |> set (popupDialogData_ >-> memorySetup_) m
        |> withNoMsg

    | SetPopupMemoryEditorData m ->
        model
        |> set (popupDialogData_ >-> memoryEditorData_) m
        |> withNoMsg

    | SetPopupProgress progOpt ->
        set (popupDialogData_ >-> progress_) progOpt model, Cmd.none

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

    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model)
        |> withNoMsg

    | ExecCmd cmd ->
        model, cmd

    | ExecFuncAsynch func ->
             let cmd' = 
                Elmish.Cmd.OfAsyncImmediate.result (async { 
                //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
                //this number only seems to affect the wavesim spinner cursor, it does not help with open project/change sheet spinner cursor
                    do! (Async.Sleep 100) 
                    if Set.contains "update" JSHelpers.debugTraceUI then
                        printfn "Starting ExecFuncAsynch payload"
                    let cmd = func ()                    
                    return (ExecCmd cmd)})
             model, cmd'

    | ExecCmdAsynch cmd ->
        let cmd' = 
            Elmish.Cmd.OfAsyncImmediate.result (async { 
            //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
            //this number only seems to affect the wavesim spinner cursor.
                do! (Async.Sleep 300)
                return (ExecCmd cmd)})
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
        renderer.ipcRenderer.send("show-context-menu", [|unbox menuType|])
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
        printfn $"Got user app dir of {userAppDir}"
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

    // Various messages here that are not implemented as yet, or are no longer used
    // should be sorted out
    | LockTabsToWaveSim | UnlockTabsFromWaveSim | SetExitDialog _ 
    | SetPropertiesExtraDialogText _ | SetRouterInteractive _ 
    | ShowExitDialog -> model, Cmd.none
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd

    | JSDiagramMsg _ | KeyboardShortcutMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none

    // post-processing of update function (Model * Cmd<Msg>)
    |> map fst_ (fun model' -> resetDialogIfSelectionHasChanged model' oldModel)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible
