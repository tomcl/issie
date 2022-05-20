module Update

open Elmish

open Fulma
open Fable.React
open Fable.React.Props
open ElectronAPI
open FilesIO
open SimulatorTypes
open ModelType
open CommonTypes
open Extractor
open CatalogueView
open PopupView
open FileMenuView
open WaveSimHelpers
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson


//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//

///Used to filter specific mouse messages based on mouse data.
let matchMouseMsg (msgSelect: DrawHelpers.MouseT -> bool) (msg : Msg) : bool =
    match msg with
    | Sheet sMsg ->
        match sMsg with
        | SheetT.MouseMsg mMsg ->
            msgSelect mMsg
        | _ -> false
    | _ -> false

/// If debugTrace is on print out human readable info on message.
/// Be careful not to do this on mouse moves (there are too many).
/// be careful not to try to ptint simulation result arrays (that would crash the renderer!).
/// optimise for very quick return in the case that debugLevel = 0 (production version)
/// optimise for quick return if nothing is printed.
let getMessageTraceString (msg: Msg) =
    let noDisplayMouseOp (mMsg:DrawHelpers.MouseT) = 
        mMsg.Op = DrawHelpers.Drag || mMsg.Op = DrawHelpers.Move
    let noDisplayMessage = function
        | Sheet (SheetT.Msg.Wire(BusWireT.Msg.Symbol(SymbolT.MouseMsg _ | SymbolT.ShowPorts _ ))) -> true
        | _ -> false
    let shortDisplayMsg = function 
        | SetWSMod _ -> Some "U(SetWSMod)"
        | SetWaveSimModel _ -> Some "U(SetWaveSimModel)"
        | SetWSModAndSheet _ -> Some "U(SetWsModAndSheet)"
        | StartSimulation _ -> Some "U(StartSimulation)"
        | SetSimulationGraph _ -> Some "U(SetSimulationGraph)"
        | SetPopupMemoryEditorData _ -> Some "U(SetPopupmemoryEditorData)"
        | _ -> None 
    if JSHelpers.debugLevel = 0 ||
       not (Set.contains "update" JSHelpers.debugTraceUI) ||
       matchMouseMsg noDisplayMouseOp msg ||
       noDisplayMessage msg then
        ""
    else 
        match shortDisplayMsg msg with
        | Some shortName -> shortName
        | None ->
            Helpers.sprintInitial 70 $"{msg}"




/// Read persistent user data from file in userAppDir.
/// Store in Model UserData.
let private readUserData (userAppDir: string) (model: Model) : Model * Cmd<Msg> =
    let addAppDirToUserData model = 
        {model with UserData = {model.UserData with UserAppDir = Some userAppDir}}

    let modelOpt =
        try
            let jsonRes = tryReadFileSync <| pathJoin [|userAppDir;"IssieSettings.json"|]
            jsonRes
            |> Result.bind (fun json -> Json.tryParseAs<UserData> json)
            |> Result.bind (fun (data: UserData) -> Ok {model with UserData = data})
            |> (function | Ok model -> model | Error _ -> printfn "Error reading user data" ; model)
            |> addAppDirToUserData 
            |> userDataToDrawBlockModel
            |> Some
        with
        | e -> None
    match modelOpt with
    | Some model -> model, Cmd.none
    | None -> addAppDirToUserData model, Cmd.none

let private writeUserData (model:Model) =
    model.UserData.UserAppDir
    |> Option.map (fun userAppDir ->
        try
            let data = drawBlockModelToUserData model model.UserData
            Json.serialize<UserData> data |> Ok
        with
        | e -> Error "Can't write settings on this PC because userAppDir does not exist"
        |> Result.bind (fun json -> writeFile (pathJoin [|userAppDir;"IssieSettings.json"|]) json)
        |> Result.mapError (fun mess -> $"Write error on directory {userAppDir}: %s{mess}")
        |> function | Error mess -> printfn "%s" mess | _ -> ())
    |> ignore
    
        
        

        
    
    

/// subfunction used in model update function
let private getSimulationDataOrFail model msg =
    match model.CurrentStepSimulationStep with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData



let verilogOutputPage sheet fPath  =
    div [] [
        str $"You can write sheet '{sheet}' (and its subsheets) in either simulation or synthesis format. The output will be written to:"
        Text.div [ 
            Modifiers [ Modifier.TextWeight TextWeight.Bold]
            Props [Style [TextAlign TextAlignOptions.Center; Padding "10px"; FontFamily "monospace"; FontSize "15px"]]] [str $"%s{Helpers.cropToLength 55 false fPath}.v"]
        Columns.columns [ ]
            [ Column.column [ ]
                [ Panel.panel [ Panel.Color IsInfo ]
                    [ Panel.heading [ ] [ str "Simulation output"]
                      Panel.Block.div [] [ str "Simulation output will run on an online synthesis tool such as Icarus v10 to check that Issie's Verilog output is working"]
                      Panel.Block.div [] 
                        [ Button.button 
                            [   Button.Color IsSuccess
                               
                                Button.IsFullWidth
                                Button.OnClick <| openInBrowser "https://www.tutorialspoint.com/compile_verilog_online.php"
                            ]
                            [ str "Icarus v10 Verilog simulator"]
                        ]
                    ]
                ]
              Column.column [ ]
                [ Panel.panel [ Panel.Color IsInfo ]
                    [ Panel.heading [ ] [ str "Synthesis output"]
                      Panel.Block.div [] [str "Synthesis output can be used as input to FPGA synthesis tools." ]
                      Panel.Block.div [] 
                        [ Button.button 
                            [   Button.Color IsSuccess                          
                                Button.IsFullWidth
                                Button.OnClick <| openInBrowser "https://github.com/edstott/issie-synth"
                            ]
                            [ str "Instructions for synthesis work-flow"] 
                        ]
                      
                         ] ] ] ] 


/// handle Menu actions that may need Model data
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuSaveFile -> 
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuExit ->
        FileMenuView.doActionWithSaveFileDialog "Exit ISSIE" CloseApp model dispatch ()
    | MenuVerilogOutput ->
        mapOverProject () model (fun p ->
            let sheet = p.OpenFileName
            let fPath = FilesIO.pathJoin [|p.ProjectPath ; sheet|]
            PopupView.choicePopup
                "Verilog Output"
                (verilogOutputPage sheet fPath)
                "Write Synthesis Verilog"
                "Write Simulation Verilog"
                (fun forSim _ -> 
                    match forSim with
                    | true -> SimulationView.verilogOutput Verilog.ForSynthesis model dispatch
                    | false -> SimulationView.verilogOutput Verilog.ForSimulation model dispatch
                    dispatch ClosePopup)
                dispatch)
            
    | _ -> ()
    model

/// get timestamp of current loaded component.
/// is this ever used? No.
let getCurrentTimeStamp model =
    match model.CurrentProj with
    | None -> System.DateTime.MinValue
    | Some p ->
        p.LoadedComponents
        |> List.tryFind (fun lc -> lc.Name = p.OpenFileName)
        |> function | Some lc -> lc.TimeStamp
                    | None -> failwithf "Project inconsistency: can't find component %s in %A"
                                p.OpenFileName ( p.LoadedComponents |> List.map (fun lc -> lc.Name))

/// Replace timestamp of current loaded component in model project by current time
/// Used in update function
let updateTimeStamp model =
    let setTimeStamp (lc:LoadedComponent) = {lc with TimeStamp = System.DateTime.Now}
    match model.CurrentProj with
    | None -> model
    | Some p ->
        p.LoadedComponents
        |> List.map (fun lc -> if lc.Name = p.OpenFileName then setTimeStamp lc else lc)
        |> fun lcs -> { model with CurrentProj=Some {p with LoadedComponents = lcs}}

//Finds if the current canvas is different from the saved canvas
// waits 50ms from last check

let findChange (model : Model) : bool = 
    let last = model.LastChangeCheckTime // NB no check to reduce total findChange time implemented yet - TODO if needed
    let start = TimeHelpers.getTimeMs()

    match model.CurrentProj with
    | None -> false
    | Some prj ->
        //For better efficiency just check if the save button
        let savedComponent = 
            prj.LoadedComponents
            |> List.find (fun lc -> lc.Name = prj.OpenFileName)
        let canv = savedComponent.CanvasState
        let canv' = model.Sheet.GetCanvasState ()
        (canv <> canv') && not (compareCanvas 100. canv canv')
        |> TimeHelpers.instrumentInterval "findChange" start

/// Needed so that constant properties selection will work
/// Maybe good idea for other things too?
let resetDialogIfSelectionHasChanged newModel oldModel =
    let newSelected = newModel.Sheet.SelectedComponents
    if newSelected.Length = 1 && newSelected <> oldModel.Sheet.SelectedComponents then
        {newModel with PopupDialogData = {newModel.PopupDialogData with Text = None ; Int = None}}
    else newModel

let updateComponentMemory (addr:int64) (data:int64) (compOpt: Component option) =
    match compOpt with
    | None -> None
    | Some ({Type= (AsyncROM1 mem as ct)} as comp)
    | Some ({Type = (ROM1 mem as ct)} as comp)
    | Some ({Type= (AsyncRAM1 mem as ct)} as comp)
    | Some ({Type= (RAM1 mem as ct)} as comp) -> 
        let update mem ct =
            match ct with
            | AsyncROM1 _ -> AsyncROM1 mem
            | ROM1 _ -> ROM1 mem
            | RAM1 _ -> RAM1 mem
            | AsyncRAM1 _ -> AsyncRAM1 mem
            | _ -> ct
        let mem' = {mem with Data = mem.Data |> Map.add addr data}
        Some {comp with Type= update mem' ct}
    | _ -> compOpt
   
let exitApp (model:Model) =
    // send message to main process to initiate window close and app shutdown
    writeUserData model
    renderer.ipcRenderer.send("exit-the-app",[||])

///Tests physical equality on two objects
///Used because Msg type does not support structural equality
let isSameMsg = LanguagePrimitives.PhysicalEquality 



///Returns None if no mouse drag message found, returns Some (lastMouseMsg, msgQueueWithoutMouseMsgs) if a drag message was found
let getLastMouseMsg msgQueue =
    msgQueue
    |> List.filter (matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag))
    |> function
    | [] -> None
    | lst -> Some lst.Head //First item in the list was the last to be added (most recent)

let sheetMsg sMsg model = 
    let sModel, sCmd = SheetUpdate.update sMsg model.Sheet
    let newModel = { model with Sheet = sModel} 
    {newModel with SavedSheetIsOutOfDate = findChange newModel}, Cmd.map Sheet sCmd


//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update (msg : Msg) oldModel =
    let startUpdate = TimeHelpers.getTimeMs()
    // number of top-level components in graph
    // mostly, we only operate on top-level components
    let getGraphSize g =
        g
        |> Option.map (fun sd -> sd.Graph |> Map.toList |> List.length)
        |> Option.defaultValue -1
   
    let sdlen = 
        getCurrentWSMod oldModel 
        |> Option.bind (fun ws -> ws.InitWaveSimGraph) 
        |> getGraphSize
    
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

    // main message dispatch match expression
    match testMsg with
    | StartUICmd uiCmd ->
        match model.UIState with
        | None -> //if nothing is currently being processed, allow the ui command operation to take place
            match uiCmd with
            | CloseProject ->
                {model with CurrentProj = None; UIState = Some uiCmd}, Cmd.none
            | _ -> 
                {model with UIState = Some uiCmd}, Cmd.ofMsg (Sheet (SheetT.SetSpinner true))
        | _ -> model, Cmd.none //otherwise discard the message
    | FinishUICmd _ ->
        let popup = CustomCompPorts.optCurrentSheetDependentsPopup model
        {model with UIState = None; PopupViewFunc = popup}, Cmd.ofMsg (Sheet (SheetT.SetSpinner false))
    (*| ShowExitDialog ->
        match model.CurrentProj with
        | Some p when model.SavedSheetIsOutOfDate ->
            {model with ExitDialog = true}, Cmd.none
        | _ -> // exit immediately since nothing to save
            exitApp()
            model, Cmd.none*)
    | CloseApp ->
        exitApp model
        model, Cmd.none
    (*| SetExitDialog status ->
        {model with ExitDialog = status}, Cmd.none*)
    | Sheet sMsg ->
        match sMsg, model.PopupViewFunc with
        | SheetT.ToggleNet canvas, _ -> 
            model, Cmd.ofMsg (Sheet (SheetT.SelectWires (getNetSelection canvas model)))
        | SheetT.KeyPress _, Some _ -> 
            // do not allow keys to affect Sheet when popup is on.
            model, Cmd.none
        | _ -> sheetMsg sMsg model
    // special mesages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode -> {model with DividerDragMode= mode}, Cmd.none
    | SetViewerWidth w -> {model with WaveSimViewerWidth = w}, Cmd.none
    | ReloadSelectedComponent width -> {model with LastUsedDialogWidth=width}, Cmd.none
    | StartSimulation simData -> 
        { model with CurrentStepSimulationStep = Some simData }, 
        Cmd.batch [
            Cmd.ofMsg (Sheet (SheetT.SetWaveSimMode false)); 
            // hack to make sure wavesim highlighting is reset - but step simulation error highlighting is not.
            // the state here clearly needs refactoring
            if model.Sheet.IsWaveSim then Cmd.ofMsg (Sheet(SheetT.ResetSelection)) else Cmd.none
            ] //Close wavesim
    | SetWSMod wSMod -> 
        setWSMod wSMod model, Cmd.none
    | UpdateWSModel updateFn ->
        updateCurrentWSMod updateFn model, Cmd.none
    | SetWSModAndSheet(ws,sheet) ->
        match model.CurrentProj with
        | None -> failwithf "What? SetWSModAndSheet: Can't set wavesim if no project is loaded"
        | Some p ->
            let sheets = p.LoadedComponents |> List.map (fun lc -> lc.Name)
            match List.contains sheet sheets with
            | false -> 
                failwithf "What? sheet %A can't be used in wavesim because it does not exist in project sheets %A" sheet sheets
            | true ->
                let model =
                    {model with WaveSimSheet = sheet}
                    |> setWSMod ws
                model,Cmd.none
    | SetWSError err -> 
        { model with WaveSim = fst model.WaveSim, err}, Cmd.none
    | AddWaveSimFile (fileName, wSMod') ->
        { model with WaveSim = Map.add fileName wSMod' (fst model.WaveSim), snd model.WaveSim}, Cmd.none
    | SetSimulationGraph (graph, fastSim) ->
        let simData = getSimulationDataOrFail model "SetSimulationGraph"
        { model with CurrentStepSimulationStep = { simData with Graph = graph ; FastSim = fastSim} |> Ok |> Some }, Cmd.none
    | SetSimulationBase numBase ->
        let simData = getSimulationDataOrFail model "SetSimulationBase"
        { model with CurrentStepSimulationStep = { simData with NumberBase = numBase } |> Ok |> Some }, Cmd.none
    | IncrementSimulationClockTick n ->
        let simData = getSimulationDataOrFail model "IncrementSimulationClockTick"
        { model with CurrentStepSimulationStep = { simData with ClockTickNumber = simData.ClockTickNumber + n } |> Ok |> Some }, Cmd.none
    | EndSimulation -> { model with CurrentStepSimulationStep = None }, Cmd.none
    | EndWaveSim -> { model with WaveSim = (Map.empty, None) }, Cmd.none
    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        firstTip <- true
        { model with RightPaneTabVisible = newTab }, 
        match newTab with 
        | Properties -> Cmd.batch <| editCmds
        | Catalogue -> Cmd.batch  <| editCmds
        | Simulation -> Cmd.batch <| editCmds
        | WaveSim -> Cmd.ofMsg (Sheet (SheetT.SetWaveSimMode true))
 
    | SetHighlighted (componentIds, connectionIds) ->
        let sModel, sCmd = SheetUpdate.update (SheetT.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model.Sheet
        {model with Sheet = sModel}, Cmd.map Sheet sCmd
    | SetSelWavesHighlighted connIds ->
        let wModel, wCmd = SheetUpdate.update (SheetT.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model.Sheet
        {model with Sheet = wModel}, Cmd.map Sheet wCmd
    | SetClipboard components -> { model with Clipboard = components }, Cmd.none
    | SetCreateComponent pos -> { model with LastCreatedComponent = Some pos }, Cmd.none
    | SetProject project -> 
        { model with 
            CurrentProj = Some project
            PopupDialogData = {model.PopupDialogData with ProjectPath = project.ProjectPath}
        }, Cmd.none
    | UpdateProject update -> 
        CustomCompPorts.updateProjectFiles true update model, Cmd.none
    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model,Cmd.none
    | ShowPopup popup -> { model with PopupViewFunc = Some popup }, Cmd.none
    | ShowStaticInfoPopup(title, body, dispatch) ->
        let foot = div [] []
        PopupView.closablePopup title body foot [Width 800] dispatch
        model, Cmd.none
    | ClosePopup ->
        let model' =
            match getSheetWaveSimOpt model, model.PopupDialogData.WaveSetup with
            | Some wsMod, Some(sheetWaves, paths) -> 
                setWSMod (setSimParams (fun sp -> {sp with MoreWaves = Set.toList paths}) wsMod) model
            | _ -> model
        { model' with 
            PopupViewFunc = None;
            PopupDialogData =
                    { model.PopupDialogData with
                        Text = None; 
                        Int = None; 
                        Int2 = None; 
                        MemorySetup = None; 
                        MemoryEditorData = None; 
                    }}, Cmd.none
    | SetPopupDialogText text ->
        { model with PopupDialogData = {model.PopupDialogData with Text = text} }, Cmd.none
    | SetPopupDialogInt int ->
        { model with PopupDialogData = {model.PopupDialogData with Int = int} }, Cmd.none
    | SetPopupDialogTwoInts data ->
        { model with PopupDialogData = 
                        match data with
                        | n, FirstInt,_ ->  {model.PopupDialogData with Int  = Option.map int32 n}
                        | n, SecondInt, optText -> {model.PopupDialogData with Int2 = n}
        }, Cmd.none
    | SetPopupDialogMemorySetup m ->
        { model with PopupDialogData = {model.PopupDialogData with MemorySetup = m} }, Cmd.none
    | SetPopupWaveSetup m ->
        { model with PopupDialogData = {model.PopupDialogData with WaveSetup = Some m} }, Cmd.none
    | SetPopupMemoryEditorData m ->
        { model with PopupDialogData = {model.PopupDialogData with MemoryEditorData = m} }, Cmd.none
    | SetPopupProgress progOpt ->
        { model with PopupDialogData = {model.PopupDialogData with Progress = progOpt} }, Cmd.none
    | UpdatePopupProgress updateFn ->
        { model with PopupDialogData = {model.PopupDialogData with Progress = Option.map updateFn model.PopupDialogData.Progress} }, Cmd.none

    | SimulateWithProgressBar simPars ->
        SimulationView.simulateWithProgressBar simPars model
    | SetSelectedComponentMemoryLocation (addr,data) ->
        {model with SelectedComponent = updateComponentMemory addr data model.SelectedComponent}, Cmd.none
    | CloseDiagramNotification ->
        { model with Notifications = {model.Notifications with FromDiagram = None} }, Cmd.none
    | SetSimulationNotification n ->
        { model with Notifications =
                        { model.Notifications with FromSimulation = Some n} }, Cmd.none
    | CloseSimulationNotification ->
        { model with Notifications = {model.Notifications with FromSimulation = None} }, Cmd.none
    | CloseWaveSimNotification ->
        { model with Notifications = {model.Notifications with FromWaveSim = None} }, Cmd.none
    | SetFilesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromFiles = Some n} }, Cmd.none
    | CloseFilesNotification ->
        { model with Notifications = {model.Notifications with FromFiles = None} }, Cmd.none
    | SetMemoryEditorNotification n ->
        { model with Notifications =
                        { model.Notifications with FromMemoryEditor = Some n} }, Cmd.none
    | CloseMemoryEditorNotification ->
        { model with Notifications = { model.Notifications with FromMemoryEditor = None} }, Cmd.none
    | SetPropertiesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromProperties = Some n} }, Cmd.none
    | ClosePropertiesNotification ->
        { model with Notifications = { model.Notifications with FromProperties = None} }, Cmd.none
    | SetTopMenu t ->
        { model with TopMenuOpenState = t}, Cmd.none
    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model), Cmd.none
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
        | _ -> getMenuView act model dispatch, Cmd.none
        
    | DiagramMouseEvent -> model, Cmd.none
    | SelectionHasChanged -> 
        match currWaveSimModel model with
        | None | Some {WSViewState=WSClosed} -> model
        | Some _ ->
            { model with ConnsOfSelectedWavesAreHighlighted = true }
        |> (fun m -> m, Cmd.none)
    | SetWaveSimIsOutOfDate b -> 
        changeSimulationIsStale b model, Cmd.none
    | SetIsLoading b ->
        let cmd = if b then Cmd.none else Cmd.ofMsg (Sheet (SheetT.SetSpinner false)) //Turn off spinner after project/sheet is loaded
        {model with IsLoading = b}, cmd
    | InitiateWaveSimulation (view, paras)  -> 
        updateCurrentWSMod (fun ws -> setEditorNextView view paras ws) model, Cmd.ofMsg FinishUICmd
    //TODO
    | WaveSimulateNow ->
        // do the simulation for WaveSim and generate new SVGs
        printfn "Starting...!"
        match getCurrentWSMod model, getCurrentWSModNextView model  with
        | Some wsMod, Some (pars, nView) -> 
            let checkCursor = wsMod.SimParams.CursorTime <> pars.CursorTime
            let pars' = adjustPars wsMod pars wsMod.SimParams.LastScrollPos
            // does the actual simulation and SVG generation, if needed
            let wsMod' = 
                simulateAndMakeWaves model wsMod pars'
                |> (fun ws -> {ws with WSViewState=nView; WSTransition = None})
                |> setEditorView nView
            model
            |> setWSMod wsMod'
            |> (fun model -> 
                {model with CheckWaveformScrollPosition=checkCursor}, 
                Cmd.ofMsg (Sheet(SheetT.SetSpinner false))) //turn off spinner after wavesim is loaded
        | Some _, None -> 
            // This case may happen if WaveSimulateNow commands are stacked up due to 
            // repeated view function calls before the WaveSimNow trigger message is processed
            // Only the first one will actually do anything. TODO: eliminate extra calls?
            model, Cmd.none
        | _ -> 
            failwith "SetSimInProgress dispatched when getCurrFileWSMod is None"

    | SetLastSimulatedCanvasState cS ->
        { model with LastSimulatedCanvasState = cS }, Cmd.none
    | UpdateScrollPos b ->
        { model with CheckWaveformScrollPosition = b}, Cmd.none
    | SetLastScrollPos posOpt ->
        let updateParas (sp:SimParamsT) = {sp with LastScrollPos = posOpt}
        updateCurrentWSMod (fun (ws:WaveSimModel) -> setSimParams updateParas ws) model, Cmd.none
    | SetWaveSimModel( sheetName, wSModel) -> 
        let updateWaveSim sheetName wSModel model =
            let sims,err = model.WaveSim
            sims.Add(sheetName, wSModel), err
        {model with WaveSim = updateWaveSim sheetName wSModel model}, Cmd.none
    | ReadUserData userAppDir ->
        printfn $"Got user app dir of {userAppDir}"
        let model,cmd = readUserData userAppDir model        
        model,cmd
    | SetUserData (data: UserData) ->
        let model =
            {model with UserData = data}
            |> userDataToDrawBlockModel
        model, Cmd.none
    | ExecutePendingMessages n ->
        if n = (List.length model.Pending)
        then 
            getLastMouseMsg model.Pending
            |> function
            | None -> failwithf "shouldn't happen"
            | Some mMsg -> 
                match mMsg with
                | Sheet sMsg -> sheetMsg sMsg model
                | _ -> failwithf "shouldn't happen "
        
        //ignore the exectue message
        else 
            model, Cmd.none
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd
    | msg ->
        model, Cmd.none
    |> (fun (newModel,cmd) -> resetDialogIfSelectionHasChanged newModel oldModel,cmd)
    |> (fun (model,cmdL) -> 
            if JSHelpers.debugLevel > 0 then
                let str = getMessageTraceString msg
                let rootOfMsg = 
                    match str.Split [|' ';'('|] with
                    | ss when ss.Length > 0 -> ss.[0]
                    | _ -> ""
                TimeHelpers.instrumentInterval rootOfMsg startUpdate |> ignore
                if str <> "" then printfn "**Upd:%s" str
                Cmd.map (fun msg -> printfn ">>Cmd:%s" (getMessageTraceString msg)) |> ignore
            model,cmdL)




