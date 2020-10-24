module Update

open Elmish

open Fulma
open Fable.React
open Fable.React.Props

open BusWidthInferer
open BusTypes

open DiagramStyle
open SimulatorTypes
open MessageType
open ModelType
open CommonTypes
open Draw2dWrapper
open Extractor
open OnDiagramButtonsView
open CatalogueView
open SelectedComponentView
open SimulationView
open PopupView
open FileMenuView
open WaveSimHelpers
open WaveformSimulationView

open Fable.Core
open Fable.Core.JsInterop



//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// Repaint each connection according to the new inferred width.
let private repaintConnections model connsWidth =
    connsWidth
    |> Map.map (fun (ConnectionId connId) width ->
        match width with
        | None -> () // Could not infer.
        | Some w -> model.Diagram.PaintConnection connId w None
    )
    |> ignore

/// subfunction in bus inference painting
let private mapPortIdsToConnWidth (conns:Connection list) connsWidth = 
    (Map.empty, conns) ||> List.fold (fun map conn ->
        let width = getConnectionWidth connsWidth <| ConnectionId conn.Id
        let map = map.Add (conn.Source.Id, width)
        map.Add (conn.Target.Id, width)
    )

/// Repaint the components that change due to bus inferred width
let private repaintBusComponents model connsWidth state =
    let comps, conns = state
    let portIdsToConnWidth = mapPortIdsToConnWidth conns connsWidth
    let lookupWidthOnPort portId =
        match portIdsToConnWidth.TryFind portId with
        | None -> None // Unconnected.
        | Some w -> w // Connected, may be inferred or not.
    comps |> List.map (fun (comp:Component) ->
        match comp.Type with
        | MergeWires ->
            (
                comp.InputPorts.[0].Id |> lookupWidthOnPort, // Top left input wire.
                comp.InputPorts.[1].Id |> lookupWidthOnPort, // Bottom left input wire.
                comp.OutputPorts.[0].Id |> lookupWidthOnPort // Output wire.
            ) |||> model.Diagram.UpdateMergeWiresLabels comp.Id
        | SplitWire topOutputWidth ->
            (
                comp.InputPorts.[0].Id |> lookupWidthOnPort,  // Input wire.
                Some topOutputWidth, // Top right input wire.
                comp.OutputPorts.[1].Id |> lookupWidthOnPort  // Bottom right input wire.
            ) |||> model.Diagram.UpdateSplitWireLabels comp.Id
        | _ -> () // Ignore other components.
    ) |> ignore



/// Alter diagram with highlighting of busses. This runs the width inference algorithm and then 
/// colors Diagram but does NOT set model.Highlighted. This needs to be done separately.
let private runBusWidthInference model =
    match model.Diagram.GetCanvasState () with
    | None -> model
    | Some jsState ->
        let state = extractState jsState
        state |> inferConnectionsWidth |> function
        | Error e ->
            // TODO: this makes the content of the model.Higlighted inconsistent.
            // Need to dispatch SetHighlighted (can do by using mkProgram).
            e.ConnectionsAffected
            |> List.map (fun (ConnectionId c) -> model.Diagram.HighlightConnection c)
            |> ignore
            // Display notification with error message.
            { model with Notifications =
                            { model.Notifications with FromDiagram =
                                                        Some <| errorNotification e.Msg CloseDiagramNotification} }
        | Ok connsWidth ->
            repaintConnections model connsWidth
            repaintBusComponents model connsWidth state
            // Close the notification if all is good.
            { model with Notifications = {model.Notifications with FromDiagram = None} }

/// subfunction used in model update function
let private getSimulationDataOrFail model msg =
    match model.Simulation with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

/// Handle messages triggered by the JS diagram.
let private handleJSDiagramMsg msg model =
    match msg with
    | InitCanvas canvas -> // Should be triggered only once.
        model.Diagram.InitCanvas canvas
        model
    | SelectComponent jsComponent ->
        { model with SelectedComponent = Some <| extractComponent jsComponent }
    | UnselectComponent () ->
        { model with SelectedComponent = None }
    | InferWidths () ->
        runBusWidthInference model
    | SetHasUnsavedChanges s -> 
        let isNotEquiv (comps1,conns1) (comps2,conns2) =
            Set comps1 <> Set comps2 || Set conns1 <> Set conns2           
        let nState = getDetailedState model
        if s && isNotEquiv model.LastDetailedState nState && not model.IsLoading then
            printfn "Setting Changed %A" s
            { model with 
                HasUnsavedChanges = s 
            }
        elif not s then
            printfn "Setting Changed %A" false
            { model with 
                LastDetailedState = nState
                HasUnsavedChanges = false }
        else model

/// Handle messages triggered by keyboard shortcuts.
let private handleKeyboardShortcutMsg msg model =
    match msg with
    | CtrlS ->
        let opt = 
            saveOpenFileAction false model 
        let ldcOpt = Option.map fst opt
        let redState = Option.map snd opt
        let proj' =
            match model.CurrProject with
            | Some p -> 
                let lcs = updateLdCompsWithCompOpt ldcOpt p.LoadedComponents
                Some {p with LoadedComponents=lcs}
            | None -> None
            
        { model with 
            HasUnsavedChanges = false
            CurrProject = proj'
            AsyncActivity = {
                model.AsyncActivity with 
                    LastSavedCanvasState = 
                        model.AsyncActivity.LastSavedCanvasState
                        |>  match opt with
                            | None -> id
                            | Some (ldc, redState) -> (fun aState ->                                
                                   aState.Add(ldc.Name, redState))
            }
        }
    | AltC ->
        // Similar to the body of OnDiagramButtonsView.copyAction but without
        // dispatching the SetClipboard message.
        match model.Diagram.GetSelected () with
        | None -> model
        | Some jsState -> { model with Clipboard = extractState jsState }
    | AltV ->
        pasteAction model
        model
    | AltZ ->
        model.Diagram.Undo ()
        model
    | AltShiftZ ->
        model.Diagram.Redo ()
        model

/// handle menus (never used?)
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuPrint ->
        match model.CurrProject with
        | None -> ()
        | Some p ->
            model.Diagram.printCanvas( fun (fn:string) (png:string) -> 
                printfn "PNG is %d bytes" png.Length
                FilesIO.savePngFile p.ProjectPath p.OpenFileName  png)
    | MenuSaveFile -> 
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuZoom z -> 
        zoomDiagram z model
    model

/// get timestamp of current loaded component.
/// is this ever used? No.
let getCurrentTimeStamp model =
    match model.CurrProject with
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
    match model.CurrProject with
    | None -> model
    | Some p ->
        p.LoadedComponents
        |> List.map (fun lc -> if lc.Name = p.OpenFileName then setTimeStamp lc else lc)
        |> fun lcs -> { model with CurrProject=Some {p with LoadedComponents = lcs}}

/// Check whether current selection is identical to previous selection and 
/// emit SelectionHasChanged if not, return model updated with new selection.
/// Uses LastSelectedIDs, updates CurrentSelection, LasSelectedIds
let checkSelection model cmd =
    let extractIds (jsComps,jsConns) =
        let compIds = jsComps |> List.map (fun comp -> JSHelpers.getFailIfNull comp ["id"] : string)
        let connIds = jsConns |> List.map (fun conn -> JSHelpers.getFailIfNull conn ["id"] : string)
        compIds,connIds
    match model.Diagram.GetSelected() with
    | None -> model,cmd // can't do anything yet
    | Some newSelection ->
        let newSelectedIds =  extractIds newSelection
        if newSelectedIds = model.LastSelectedIds then
            {model with CurrentSelected = extractState newSelection; LastSelectedIds = newSelectedIds}, cmd
        else
            model, Cmd.batch [cmd; Cmd.ofMsg SelectionHasChanged]

/// Check whether current message could mark a change in Diagram worth saving.
/// If so, check whether Diagram has a significant circuit change (don't count layout).
/// If so, do an autosave. TODO: make the autosave asynchronous
/// similarly, check for chnage in selection and send message if there is one
let checkForAutoSaveOrSelectionChanged msg (model, cmd) =
    let simIsStale (newState:CanvasState option) (simState:CanvasState option) =
        match newState,simState with
        | None, _  -> false
        | Some (ncomps,nconns), Some (comps,conns) -> 
            Set ncomps <> Set comps || Set nconns <> Set conns
        | _ -> true

    let needsAutoSave (proj: Project) (newState:CanvasState option) (state:Map<string,CanvasState>) =
        match newState, Map.tryFind proj.OpenFileName state with
        | None, _ | _, None -> 
            false
        | Some (ncomps,nconns), Some (comps,conns) -> 
            Set ncomps <> Set comps || Set nconns <> Set conns
    match msg with 
    | DiagramMouseEvent -> checkSelection model cmd
    | _ -> 
        if System.DateTime.Now < (model.AsyncActivity.LastAutoSaveCheck).AddSeconds 0.1 then
            model, cmd
        else
            let model,cmd = checkSelection model cmd
            let model = setActivity (fun a -> {a with LastAutoSaveCheck=System.DateTime.Now}) model
            match model.CurrProject with
            | None -> 
                model, cmd // do nothing
            | Some proj ->
                let newReducedState = getReducedState model
                let update = not model.IsLoading && needsAutoSave proj newReducedState  model.AsyncActivity.LastSavedCanvasState
                let simUpdate = simIsStale newReducedState model.LastSimulatedCanvasState
                //printfn "SimUpdate=%A" simUpdate
                if update
                then
                    printfn "AutoSaving at '%s'" (System.DateTime.Now.ToString("mm:ss"))
                    {model with HasUnsavedChanges=true}
                    |> updateTimeStamp
                    |> setActivity (fun a -> {a with LastSavedCanvasState = addReducedState a proj.OpenFileName model})
                    |> (fun model' -> 
                        saveOpenFileAction true model' |> ignore
                        setActivity (fun a -> {a with LastAutoSave = a.LastAutoSave.Add(proj.OpenFileName,System.DateTime.Now)}) model')
                else
                    model
                |> setActivity (fun a ->
                                match update || Map.tryFind proj.OpenFileName a.LastSavedCanvasState = None with
                                | true -> {a with LastSavedCanvasState = addReducedState a proj.OpenFileName model}
                                | false -> a)
                |> (fun model -> changeSimulationIsStale simUpdate model, cmd)
       
/// Uses model.Hilighted to track what is currently highlighted (green or red)
/// Changes green highlighting as determined by connIds
let private setSelWavesHighlighted model connIds =
    let (_, errConnIds), oldConnIds = model.Hilighted
    let currentConnIds = 
        model.Diagram.GetCanvasState()
        |> Option.map extractState
        |> Option.map (snd >> List.map (fun conn -> conn.Id))
        |> Option.defaultValue []
        |> Set
    let isCurrent cId = Set.contains cId currentConnIds
 
    oldConnIds
    |> List.map (fun (ConnectionId c) -> 
        match List.contains (ConnectionId c) errConnIds with
        | false when isCurrent c -> model.Diagram.UnHighlightConnection c 
        | _ -> ())
    |> ignore
    connIds
    |> List.map (fun (ConnectionId c) -> 
        match List.contains (ConnectionId c) errConnIds with
        | false when isCurrent c -> model.Diagram.HighlightConnection c "green"
        | _ -> ())
    |> ignore
    List.fold (fun st cId -> if List.contains cId errConnIds 
                                then st
                                else cId :: st ) [] connIds

//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update msg model =
    // initial (short0 code used for debug printout
    (*
    match msg with
    | SelectionHasChanged | SetHighlighted _ -> ()
    | _ ->          
        printfn "\n--------------------------\n%s\n" (spMess msg)
        //pp model
    // main dispatch based on message *)
    match msg with
    | SetDragMode mode -> {model with DragMode= mode}, Cmd.none
    | SetViewerWidth w -> {model with ViewerWidth = w}, Cmd.none
    | JSDiagramMsg msg' -> handleJSDiagramMsg msg' model, Cmd.none
    | KeyboardShortcutMsg msg' -> handleKeyboardShortcutMsg msg' model, Cmd.none
    // Messages triggered by the "classic" Elmish UI (e.g. buttons and so on).
    | SetLastSavedCanvas(name,state) -> 
        setActivity (fun a -> {a with LastSavedCanvasState= Map.add name state a.LastSavedCanvasState}) model, Cmd.none
    | StartSimulation simData -> { model with Simulation = Some simData }, Cmd.none
    | SetCurrFileWSMod wSMod' -> 
        match FileMenuView.getCurrFile model with
        | Some fileName ->
            { model with WaveSim = Map.add fileName wSMod' (fst model.WaveSim), 
                                   snd model.WaveSim }
        | None -> model 
        |> (fun x -> x, Cmd.none)
    | SetWSError err -> { model with WaveSim = fst model.WaveSim, err }, Cmd.none
    | AddWaveSimFile (fileName, wSMod') ->
        { model with WaveSim = Map.add fileName wSMod' (fst model.WaveSim), snd model.WaveSim}, Cmd.none
    | SetSimulationGraph graph ->
        let simData = getSimulationDataOrFail model "SetSimulationGraph"
        { model with Simulation = { simData with Graph = graph } |> Ok |> Some }, Cmd.none
    | SetSimulationBase numBase ->
        let simData = getSimulationDataOrFail model "SetSimulationBase"
        { model with Simulation = { simData with NumberBase = numBase } |> Ok |> Some }, Cmd.none
    | IncrementSimulationClockTick ->
        let simData = getSimulationDataOrFail model "IncrementSimulationClockTick"
        { model with Simulation = { simData with ClockTickNumber = simData.ClockTickNumber+1 } |> Ok |> Some }, Cmd.none
    | EndSimulation -> { model with Simulation = None }, Cmd.none
    | EndWaveSim -> { model with WaveSim = (Map.empty, None) }, Cmd.none
    | ChangeRightTab newTab -> 
        { model with RightTab = newTab }, 
        match newTab with 
        | Properties -> Cmd.ofMsg <| SetSelWavesHighlighted [||]
        | Catalogue -> Cmd.ofMsg <| SetSelWavesHighlighted [||]
        | Simulation -> Cmd.ofMsg <| SetSelWavesHighlighted [||]
        | WaveSim -> Cmd.none
    | SetHighlighted (componentIds, connectionIds) ->
        let oldComponentIds, oldConnectionIds = fst model.Hilighted
        oldComponentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.UnHighlightComponent c)
        |> ignore
        componentIds
        |> List.map (fun (ComponentId c) -> model.Diagram.HighlightComponent Red c)
        |> ignore
        oldConnectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.UnHighlightConnection c)
        |> ignore
        connectionIds
        |> List.map (fun (ConnectionId c) -> model.Diagram.HighlightConnection c "red")
        |> ignore
        { model with Hilighted = (componentIds, connectionIds), snd model.Hilighted }, Cmd.none
    | SetSelWavesHighlighted connIds ->
        setSelWavesHighlighted model (Array.toList connIds)
        |> (fun lst -> { model with Hilighted = fst model.Hilighted, lst
                                    ConnsToBeHighlighted = false }, Cmd.none)
    | SetClipboard components -> { model with Clipboard = components }, Cmd.none
    | SetCreateComponent pos -> { model with CreateComponent = Some pos }, Cmd.none
    | SetProject project -> 
        { model with CurrProject = Some project}, Cmd.none
    | CloseProject -> { model with CurrProject = None }, Cmd.none
    | ShowPopup popup -> { model with Popup = Some popup }, Cmd.none
    | ClosePopup ->
        { model with Popup = None; PopupDialogData =
                    { Text = None; Int = None; Int2 = None; MemorySetup = None; MemoryEditorData = None} }, Cmd.none
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
    | SetPopupMemoryEditorData m ->
        { model with PopupDialogData = {model.PopupDialogData with MemoryEditorData = m} }, Cmd.none
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
        { model with TopMenu = t}, Cmd.none
    | ReloadSelectedComponent width ->
        match model.SelectedComponent with
        | None -> {model with LastUsedDialogWidth = width}
        | Some comp ->
            match model.Diagram.GetComponentById comp.Id with
            | Error err -> {model with LastUsedDialogWidth=width}
            | Ok jsComp -> { model with SelectedComponent = Some <| extractComponent jsComp ; LastUsedDialogWidth=width}
        |> (fun x -> x, Cmd.none)
    | MenuAction(act,dispatch) ->
        getMenuView act model dispatch, Cmd.none
    | DiagramMouseEvent -> model, Cmd.none
    | SelectionHasChanged -> 
        match currWaveSimModel model with
        | Some _ ->
            { model with ConnsToBeHighlighted = true }
        | None -> model
        |> (fun m -> m, Cmd.none)
    | SetSimIsStale b -> 
        changeSimulationIsStale b model, Cmd.none
    | SetIsLoading b ->
        {model with IsLoading = b}, Cmd.none
    | SetSimInProgress waveInfo -> 
        { model with SimulationInProgress = Some waveInfo }, Cmd.none
    | SimulateWhenInProgress waveInfo ->
        // do the simulation for WaveSim and generate new SVGs
        match FileMenuView.getCurrFile model with
        | Some fileName ->
            match currWaveSimModel model, waveInfo with
            | Some wSMod, Ok ports -> 
                // does the actual simulation and SVG generation, if needed
                let wsMod' = waveGen model waveSvg clkRulerSvg wSMod ports
                { model with Hilighted = fst model.Hilighted, setSelWavesHighlighted model [] 
                             WaveSim = Map.add fileName wsMod' (fst model.WaveSim), 
                                       snd model.WaveSim
                             SimulationInProgress = None }, Cmd.ofMsg SetSimNotInProgress
            | Some wSMod, Error par -> 
                // in this case 
                { model with WaveSim = Map.add fileName (updateWSMod waveSvg clkRulerSvg model wSMod par) (fst model.WaveSim), 
                                       snd model.WaveSim 
                             SimulationInProgress = None }, Cmd.ofMsg SetSimNotInProgress
            | _ -> 
                failwith "SetSimInProgress dispatched when currWS model is None"
        | None -> failwith "SetSimInProgress dispatched when getCurrFile model is None"
    | SetSimNotInProgress ->
        model, Cmd.none
    | SetLastSimulatedCanvasState cS ->
        { model with LastSimulatedCanvasState = cS }, Cmd.none
    | UpdateScrollPos b ->
        { model with CheckScrollPos = b}, Cmd.none
    | SetWaveSimModel( sheetName, wSModel) -> 
        let updateWaveSim sheetName wSModel model =
            let sims,err = model.WaveSim
            sims.Add(sheetName, wSModel), err
        {model with WaveSim = updateWaveSim sheetName wSModel model}, Cmd.none
    // post-update check always done which deals with regular tasks like updating connections and 
    // auto-saving files
    |> checkForAutoSaveOrSelectionChanged msg



