module Update

open Elmish

open Fulma
open Fable.React
open Fable.React.Props
open Electron
open BusWidthInferer
open SimulatorTypes
open ModelType
open CommonTypes
open Extractor
open CatalogueView
open PopupView
open FileMenuView
open WaveSimHelpers

open Fable.Core
open Fable.Core.JsInterop



//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// subfunction used in model update function
let private getSimulationDataOrFail model msg =
    match model.CurrentStepSimulationStep with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

/// handle menus (never used?)
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuSaveFile -> 
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuVerilogOutput ->
        SimulationView.verilogOutput model dispatch
        failwithf "try to break here..."
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

//Finds if a change has occured
let findChange (model : Model) : bool = 
    match model.CurrentProj with
    | None -> false
    | Some prj ->
        let savedComponent = 
            prj.LoadedComponents
            |> List.find (fun lc -> lc.Name = prj.OpenFileName)
        printf "DEBUG in findChange \n savedCanvas: \n %A \n\n currentCanvas: \n %A" savedComponent.CanvasState (getReducedCanvState model)
        savedComponent.CanvasState <> (model.Sheet.GetCanvasState ())
    

let updateComponentMemory (addr:int64) (data:int64) (compOpt: Component option) =
    match compOpt with
    | None -> None
    | Some ({Type= (AsyncROM mem as ct)} as comp)
    | Some ({Type = (ROM mem as ct)} as comp)
    | Some ({Type= (RAM mem as ct)} as comp) -> 
        let update mem ct =
            match ct with
            | AsyncROM _ -> AsyncROM mem
            | ROM _ -> ROM mem
            | RAM _ -> RAM mem
            | _ -> ct
        let mem' = {mem with Data = mem.Data |> Map.add addr data}
        Some {comp with Type= update mem' ct}
    | _ -> compOpt
   
let exitApp() =
    // send message to main process to initiate window close and app shutdown
    Electron.electron.ipcRenderer.send("exit-the-app",[||])

//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update msg model =
    let startUpdate = Helpers.getTimeMs()
    // number of top-level components in graph
    // mostly, we only operate on top-level components
    let getGraphSize g =
        g
        |> Option.map (fun sd -> sd.Graph |> Map.toList |> List.length)
        |> Option.defaultValue -1
   
    let sdlen = 
        getCurrentWSMod model 
        |> Option.bind (fun ws -> ws.InitWaveSimGraph) 
        |> getGraphSize

    if Set.contains "update" JSHelpers.debugTraceUI then
        let msgS = (sprintf "%A..." msg) |> Seq.truncate 60 |> Seq.map (fun c -> string c) |> String.concat ""
        printfn "%d %s" sdlen msgS
    // main message dispatch match expression
    match msg with
    | ShowExitDialog ->
        match model.CurrentProj with
        | Some p when model.SavedSheetIsOutOfDate ->
            {model with ExitDialog = true}, Cmd.none
        | _ -> // exit immediately since nothing to save
            exitApp()
            model, Cmd.none
    | CloseApp ->
        exitApp()
        model, Cmd.none
    | SetExitDialog status ->
        {model with ExitDialog = status}, Cmd.none
    | Sheet sMsg ->
        let sModel, sCmd = Sheet.update sMsg model.Sheet
        let newModel = { model with Sheet = sModel} 
        {newModel with SavedSheetIsOutOfDate = findChange newModel}, Cmd.map Sheet sCmd 
    // special mesages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode -> {model with DividerDragMode= mode}, Cmd.none
    | SetViewerWidth w -> {model with WaveSimViewerWidth = w}, Cmd.none

    // Messages triggered by the "classic" Elmish UI (e.g. buttons and so on).
    | SetLastSavedCanvas(name,state) -> 
        // this field is (potentially) used to determine when a new autosave is taken. Now maybe not used?
        setActivity (fun a -> {a with LastSavedCanvasState= Map.add name state a.LastSavedCanvasState}) model, Cmd.none
    | StartSimulation simData -> { model with CurrentStepSimulationStep = Some simData }, Cmd.none
    | SetWSMod wSMod -> 
        setWSMod wSMod model, Cmd.none
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
    | IncrementSimulationClockTick ->
        let simData = getSimulationDataOrFail model "IncrementSimulationClockTick"
        { model with CurrentStepSimulationStep = { simData with ClockTickNumber = simData.ClockTickNumber+1 } |> Ok |> Some }, Cmd.none
    | EndSimulation -> { model with CurrentStepSimulationStep = None }, Cmd.none
    | EndWaveSim -> { model with WaveSim = (Map.empty, None) }, Cmd.none
    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        firstTip <- true
// TODO: Why BusInference on tab change?
//        if newTab <> WaveSim && model.RightPaneTabVisible = WaveSim then 
//            //printfn "Running inference"
//            model.Diagram.ResetSelected()
//            model
//        else
//            model
//        |>  runBusWidthInference
//        |> (fun model -> 
//            { model with RightPaneTabVisible = newTab }),
        { model with RightPaneTabVisible = newTab }, 
        match newTab with 
        | Properties -> Cmd.batch <| editCmds
        | Catalogue -> Cmd.batch  <| editCmds
        | Simulation -> Cmd.batch <| editCmds
        | WaveSim -> Cmd.none
 
    | SetHighlighted (componentIds, connectionIds) ->
        let sModel, sCmd = Sheet.update (Sheet.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model.Sheet
        {model with Sheet = sModel}, Cmd.map Sheet sCmd
    | SetSelWavesHighlighted connIds ->
        let wModel, wCmd = Sheet.update (Sheet.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model.Sheet
        {model with Sheet = wModel}, Cmd.map Sheet wCmd
    | SetClipboard components -> { model with Clipboard = components }, Cmd.none
    | SetCreateComponent pos -> { model with LastCreatedComponent = Some pos }, Cmd.none
    | SetProject project -> 
        { model with CurrentProj = Some project}, Cmd.none
    | CloseProject -> { model with CurrentProj = None }, Cmd.none
    | ShowPopup popup -> { model with PopupViewFunc = Some popup }, Cmd.none
    | ClosePopup ->
        let model' =
            match getSheetWaveSimOpt model, model.PopupDialogData.WaveSetup with
            | Some wsMod, Some(sheetWaves, paths) -> 
                setWSMod (setSimParams (fun sp -> {sp with MoreWaves = Set.toList paths}) wsMod) model
            | _ -> model
        { model' with 
            PopupViewFunc = None;
            PopupDialogData =
                    { 
                        Text = None; 
                        Int = None; 
                        Int2 = None; 
                        MemorySetup = None; 
                        MemoryEditorData = None; 
                        WaveSetup = model.PopupDialogData.WaveSetup} 
                    }, Cmd.none
    | SetPopupDialogText text ->
        { model with PopupDialogData = {model.PopupDialogData with Text = text} }, Cmd.none
    | SetPopupDialogInt int ->
        { model with PopupDialogData = {model.PopupDialogData with Int = int} }, Cmd.none
    | SetPopupDialogTwoInts data ->
        { model with PopupDialogData = 
                        match data with
                        | n, FirstInt ->  {model.PopupDialogData with Int  = n}
                        | n, SecondInt -> {model.PopupDialogData with Int2 = n}
        }, Cmd.none
    | SetPopupDialogMemorySetup m ->
        { model with PopupDialogData = {model.PopupDialogData with MemorySetup = m} }, Cmd.none
    | SetPopupWaveSetup m ->
        { model with PopupDialogData = {model.PopupDialogData with WaveSetup = Some m} }, Cmd.none
    | SetPopupMemoryEditorData m ->
        { model with PopupDialogData = {model.PopupDialogData with MemoryEditorData = m} }, Cmd.none
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
// TODO
//    | ReloadSelectedComponent width ->
//        match model.SelectedComponent with
//        | None -> {model with LastUsedDialogWidth = width}
//        | Some comp ->
//            match model.Diagram.GetComponentById comp.Id with
//            | Error err -> {model with LastUsedDialogWidth=width}
//            | Ok jsComp -> { model with SelectedComponent = Some <| extractComponent jsComp ; LastUsedDialogWidth=width}
//        |> (fun x -> x, Cmd.none)
    | MenuAction(act,dispatch) ->
        getMenuView act model dispatch, Cmd.none
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
        {model with IsLoading = b}, Cmd.none
    | InitiateWaveSimulation (view, paras)  -> 
        updateCurrentWSMod (fun ws -> setEditorNextView view paras ws) model, Cmd.none
    //TODO
    | WaveSimulateNow ->
        // do the simulation for WaveSim and generate new SVGs
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
            |> (fun model -> {model with CheckWaveformScrollPosition=checkCursor}, Cmd.none)
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
// TODO
    | ReleaseFileActivity a ->
        releaseFileActivityImplementation a
        model, Cmd.none
//    // post-update check always done which deals with regular tasks like updating connections and 
//    // auto-saving files
//    | SetRouterInteractive isInteractive ->
//        model.Diagram.SetRouterInteractive isInteractive
//        model, Cmd.none
//    |> checkForAutoSaveOrSelectionChanged msg
    | msg ->
        printfn $"DEBUG: Leftover Message needs to be deleted: {msg}" // TODO
        model, Cmd.none
    |> Helpers.instrumentInterval (Helpers.sprintInitial 20 $"Update: %A{msg}") startUpdate



