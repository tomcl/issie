module Update

open Elmish
open Fable.React
open Fable.React.Props
open ModelType
open ElectronAPI
open FilesIO
open SimulatorTypes
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

    let cmdOfTTMsg (ttMsg: TTMsg) = Cmd.ofMsg(TruthTableMsg ttMsg)
    let withCmdTTMsg (ttMsg: TTMsg) (model:Model) = model, cmdOfTTMsg ttMsg

    let withCmdNone (model: Model) = model, Cmd.none

    let withMsg (msg: Msg) (model : Model)  = model,Cmd.ofMsg msg

    let withMsgs (msgs: Msg list) (model : Model) = model, Cmd.batch (List.map Cmd.ofMsg msgs)

    let startOfUpdateTime = TimeHelpers.getTimeMs()   

    //Add the message to the pending queue if it is a mouse drag message
    let model =
        if matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag) msg then
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
    | StartUICmd uiCmd ->
        //printfn $"starting UI command '{uiCmd}"
        uiStartTime <- TimeHelpers.getTimeMs()
        match model.UIState with
        | None -> //if nothing is currently being processed, allow the ui command operation to take place
            match uiCmd with
            | CloseProject ->
                {model with CurrentProj = None; UIState = Some uiCmd}
                |> withCmdNone
            | _ -> 
                {model with UIState = Some uiCmd}
                |> withMsg (Sheet (SheetT.SetSpinner true))
        | _ -> model, Cmd.none //otherwise discard the message
    | FinishUICmd _->
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
        | _ -> sheetMsg sMsg model

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
        |> withCmdNone
        
    // special messages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode ->
        {model with DividerDragMode= mode}
        |> withCmdNone

    | SetViewerWidth w ->
        {model with WaveSimViewerWidth = w}
        |> withCmdNone

    | SheetBackAction dispatch ->
        processSheetBackAction dispatch model
        |> withCmdNone        

    | UpdateUISheetTrail updateFun ->
        model
        |> map uISheetTrail_ (updateFun >> List.filter (filterByOKSheets model))
        |> withCmdNone

    | ReloadSelectedComponent width ->
        {model with LastUsedDialogWidth=width}
        |> withCmdNone

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
        |> withCmdNone

    | SetWSModel wsModel ->
        setWSModel wsModel model
        |> withCmdNone

    | UpdateWSModel updateFn ->
        updateWSModel updateFn model
        |> withCmdNone

    | SetWSModelAndSheet (wsModel, wsSheet) ->
        model
        |> set waveSimSheet_ (if wsSheet = "" then None else Some wsSheet)
        |> setWSModel wsModel
        |> withCmdNone

    | UpdateModel( updateFn: Model -> Model) ->
        updateFn model, Cmd.none

    | UpdateImportDecisions updateFn ->
        model
        |> map (popupDialogData_ >-> importDecisions_) updateFn
        |> withCmdNone

    | RefreshWaveSim ws ->
        // restart the wave simulator after design change etc that invalidates all waves
        WaveSim.refreshWaveSim true ws model

    | AddWSModel (sheet, wsModel) ->
        model
        |> map waveSim_ (Map.add sheet wsModel)
        |> withCmdNone

    | GenerateWaveforms ws ->
        // Update the wave simulator with new waveforms
        // Is called whenever any waveform might need to be changed
        WaveSim.refreshWaveSim false ws model

    | GenerateCurrentWaveforms ->
        // Update the wave simulator with new waveforms based on current WsModel
        let ws = getWSModel model
        WaveSim.refreshWaveSim false ws model

    | SetWaveComponentSelectionOpen (fIdL, show) ->       
        model
        |> updateWSModel (fun ws -> WaveSimHelpers.setWaveComponentSelectionOpen ws fIdL show)
        |> withCmdNone

    | SetWaveGroupSelectionOpen (fIdL, show) -> 
        model
        |> updateWSModel (fun ws -> WaveSimHelpers.setWaveGroupSelectionOpen ws fIdL show)
        |> withCmdNone

        
    | SetWaveSheetSelectionOpen (fIdL, show) ->       
        model
        |> updateWSModel (fun ws -> WaveSimHelpers.setWaveSheetSelectionOpen ws fIdL show)
        |> withCmdNone    

    | TryStartSimulationAfterErrorFix simType ->
        SimulationView.tryStartSimulationAfterErrorFix simType model

    | SetSimulationGraph (graph, fastSim) ->
        let simData =
            getSimulationDataOrFail model "SetSimulationGraph"
            |> (set graph_ graph >> set fastSim_ fastSim)
            |> Ok |> Some
        model
        |> set currentStepSimulationStep_ simData
        |> withCmdNone

    | SetSimulationBase numBase ->
        let simData =
            getSimulationDataOrFail model "SetSimulationBase"
            |> set numberBase_ numBase
        model
        |> set currentStepSimulationStep_ (simData |> Ok |> Some)
        |> withCmdNone

    | IncrementSimulationClockTick n ->
        let simData =
            getSimulationDataOrFail model "IncrementSimulationClockTick"
            |> map clockTickNumber_ (fun n -> n+1)
        model
        |> set currentStepSimulationStep_ (simData |> Ok |> Some )
        |> withCmdNone

    | EndSimulation ->
        model
        |> set currentStepSimulationStep_ None
        |> withCmdNone

    | EndWaveSim -> 
        let model =
            let model = removeAllSimulationsFromModel model
            match model.WaveSimSheet with
            | None | Some "" -> 
                printfn "What? can't end WaveSim when it is already ended"
                model
            | Some sheet -> 
                { model with 
                    WaveSimSheet = None; 
                    WaveSim = Map.change sheet (Option.map (fun ws -> 
                        {ws with State = Ended ; WaveModalActive = false})) model.WaveSim
                }
        model, Cmd.none

    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editMsgs = [inferMsg; ClosePropertiesNotification]
        firstTip <- true

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
        |> withCmdNone

    | SetHighlighted (componentIds, connectionIds) ->
        SheetUpdate.update (SheetT.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model

    | SetSelWavesHighlighted connIds ->
        SheetUpdate.update (SheetT.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model

    | SetClipboard components ->
        { model with Clipboard = components }
        |> withCmdNone

    | SetCreateComponent pos ->
        { model with LastCreatedComponent = Some pos}
        |> withCmdNone

    | SetProject project ->
        printf $"Setting project with component: '{project.OpenFileName}'"
        model
        |> set currentProj_ (Some project) 
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath
        |> withCmdNone

    | UpdateProject update ->
        CustomCompPorts.updateProjectFiles true update model
        |> withCmdNone

    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model
        |> withCmdNone

    | ShowPopup popup ->
        model
        |> set popupViewFunc_ (Some popup)
        |> withCmdNone

    | ShowStaticInfoPopup(title, body, dispatch) ->
        let foot = div [] []
        PopupHelpers.closablePopup title body foot [Width 800] dispatch
        model
        |> withCmdNone

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
        |> withCmdNone

    | SetPopupDialogText text ->
        model
        |> set (popupDialogData_ >-> text_) text
        |> withCmdNone

    | SetPopupDialogBadLabel isBad ->
        model
        |> set (popupDialogData_ >-> badLabel_) isBad
        |> withCmdNone

    | SetPopupDialogCode code ->
        model
        |> set (popupDialogData_ >-> verilogCode_) code
        |> withCmdNone

    | SetPopupDialogVerilogErrors errorList ->
        model
        |> set (popupDialogData_ >-> verilogErrors_) errorList
        |> withCmdNone

    | SetPopupDialogInt int ->
        model
        |> set (popupDialogData_ >-> int_) int
        |> withCmdNone

    | SetPopupDialogInt2 int ->
        set (popupDialogData_ >-> int2_) int model, Cmd.none

    | SetPopupDialogTwoInts (n, select, optText)->
        model
        |> map popupDialogData_
                    (match select with
                     | FirstInt -> set int_ (Option.map int32 n)
                     | SecondInt -> set int2_ n)
        |> withCmdNone

    | SetPopupDialogMemorySetup m ->
        model
        |> set (popupDialogData_ >-> memorySetup_) m
        |> withCmdNone

    | SetPopupMemoryEditorData m ->
        model
        |> set (popupDialogData_ >-> memoryEditorData_) m
        |> withCmdNone

    | SetPopupProgress progOpt ->
        set (popupDialogData_ >-> progress_) progOpt model, Cmd.none

    | UpdatePopupProgress updateFn ->
        model
        |> map (popupDialogData_ >-> progress_) (Option.map updateFn)
        |> withCmdNone

    | SimulateWithProgressBar simPars ->
        SimulationView.simulateWithProgressBar simPars model

    | SetSelectedComponentMemoryLocation (addr,data) ->
        model
        |> map selectedComponent_ (updateComponentMemory addr data)
        |> withCmdNone

    | CloseDiagramNotification ->
        model
        |> set (notifications_ >-> fromDiagram_) None
        |> withCmdNone

    | SetSimulationNotification n ->
        model
        |> set (notifications_ >-> fromSimulation_) (Some n)
        |> withCmdNone
    | CloseSimulationNotification ->
        model
        |> set (notifications_ >-> fromSimulation_) None
        |> withCmdNone

    | CloseWaveSimNotification ->
        model
        |> set (notifications_ >-> fromWaveSim_) None
        |> withCmdNone

    | SetFilesNotification n ->
        model
        |> set (notifications_ >-> fromFiles_) (Some n)
        |> withCmdNone

    | CloseFilesNotification ->
        model
        |> set (notifications_ >-> fromFiles_) None
        |> withCmdNone

    | SetMemoryEditorNotification n ->
        model
        |> set (notifications_ >-> fromMemoryEditor_) (Some n)
        |> withCmdNone

    | CloseMemoryEditorNotification ->
        model
        |> set (notifications_ >-> fromMemoryEditor_) None
        |> withCmdNone

    | SetPropertiesNotification n ->
        model
        |> set (notifications_ >-> fromProperties_) (Some n)
        |> withCmdNone

    | ClosePropertiesNotification ->
        model
        |> set (notifications_ >-> fromProperties_) None
        |> withCmdNone        

    | SetTopMenu t ->
        { model with TopMenuOpenState = t}
        |> withCmdNone

    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model)
        |> withCmdNone

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
        |> withCmdNone

    | DiagramMouseEvent ->
        model, Cmd.none // this now does nothing and should be removed

    | SelectionHasChanged -> 
        { model with ConnsOfSelectedWavesAreHighlighted = true }
        |> withCmdNone

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
        |> withCmdNone

    | SetThemeUserData (theme: DrawModelType.SymbolT.ThemeType) ->
        let model =
            {model with UserData = {model.UserData with Theme=theme}}
            |> userDataToDrawBlockModel
        model, Cmd.none

    | ExecutePendingMessages n ->
        executePendingMessagesF n model

    | TruthTableMsg ttMsg ->
        TruthTableUpdate.truthTableUpdate model ttMsg

    // Various messages here that are not implemented as yet, or are no longer used
    // should be sorted out
    | LockTabsToWaveSim | UnlockTabsFromWaveSim | SetExitDialog _ 
    | SetPropertiesExtraDialogText _ | SetRouterInteractive _ 
    | ShowExitDialog _ -> model, Cmd.none
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd

    | JSDiagramMsg _ | KeyboardShortcutMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none

    // post-processing of update function (Model * Cmd<Msg>)
    |> map fst_ (fun model' -> resetDialogIfSelectionHasChanged model' oldModel)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible
