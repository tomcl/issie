module DiagramMainView

open Elmish

open Fulma
open Fable.React
open Fable.React.Props

open BusWidthInferer
open BusTypes

open DiagramStyle
open SimulatorTypes
open DiagramMessageType
open DiagramModelType
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

// -- Init Model

let initActivity = {
    AutoSave = Inactive
    LastSavedCanvasState = Map.empty
    LastAutoSaveCheck = System.DateTime.MinValue
    LastAutoSave = Map.empty
    RunningSimulation = false
    }

let init() = {
    AsyncActivity = initActivity
    Diagram = new Draw2dWrapper()
    SimulationIsStale = true
    LastSimulatedCanvasState = None
    LastSelectedIds = [],[]
    CurrentSelected = [],[]
    SelectedComponent = None
    LastUsedDialogWidth = 1
    Simulation = None
    WaveSim = Map.empty, None
    RightTab = Catalogue
    CurrProject = None
    Hilighted = ([], []), []
    Clipboard = [], []
    CreateComponent = None
    HasUnsavedChanges = false
    Popup = None
    PopupDialogData = {
        Text = None
        Int = None
        Int2 = None
        MemorySetup = None
        MemoryEditorData = None
    }
    Notifications = {
        FromDiagram = None
        FromSimulation = None
        FromWaveSim = None
        FromFiles = None
        FromMemoryEditor = None
        FromProperties = None
    }
    TopMenu = Closed
    DragMode = DragModeOff
    ViewerWidth = rightSectionWidthViewerDefault
    SimulationInProgress = None
    ConnsToBeHighlighted= false
    CheckScrollPos = false
}

/// Repaint each connection according to the new inferred width.
let private repaintConnections model connsWidth =
    connsWidth
    |> Map.map (fun (ConnectionId connId) width ->
        match width with
        | None -> () // Could not infer.
        | Some w -> model.Diagram.PaintConnection connId w None
    )
    |> ignore

let private mapPortIdsToConnWidth (conns:Connection list) connsWidth = 
    (Map.empty, conns) ||> List.fold (fun map conn ->
        let width = getConnectionWidth connsWidth <| ConnectionId conn.Id
        let map = map.Add (conn.Source.Id, width)
        map.Add (conn.Target.Id, width)
    )

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

let private makeSelectionChangeMsg (model:Model) (dispatch: Msg -> Unit) (ev: 'a) =
    dispatch SelectionHasChanged

// -- Create View

/// Display the content of the right tab.
let private viewRightTab model dispatch =
    match model.RightTab with
    | Catalogue ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Click on a component to add it to the diagram. Hover on components for details." ]
            viewCatalogue model dispatch
        ]
    | Properties ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Component properties" ]
            viewSelectedComponent model dispatch
        ]
    | Simulation ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Simulation" ]
            viewSimulation model dispatch
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; Height "calc(100% - 48px)"; MarginTop "15px" ] ]
            ( viewWaveSim model dispatch )

let setDragMode (modeIsOn:bool) (model:Model) dispatch =
    fun (ev: Browser.Types.MouseEvent) ->        
        makeSelectionChangeMsg model dispatch ev
        //printfn "START X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        match modeIsOn, model.DragMode with
        | true, DragModeOff ->  SetDragMode (DragModeOn (int ev.clientX)) |> dispatch
        | false, DragModeOn _ -> SetDragMode DragModeOff |> dispatch
        | _ -> ()


let dividerbar (model:Model) dispatch =
    let isDraggable = model.RightTab = WaveSim
    let variableStyle = 
        if isDraggable then [
            BackgroundColor "grey"
            Cursor "grab" 
            Width "10px"

        ] else [
            BackgroundColor "lightgrey"
            Width "2px"

        ]
    let commonStyle = [
            Height "100%"
            Float FloatOptions.Left
        ]
    div [
            Style <| commonStyle @ variableStyle
            OnMouseDown (setDragMode true model dispatch)       
        ] []

let displayView model dispatch =
    let windowX,windowY =
        int Browser.Dom.self.innerWidth, int Browser.Dom.self.innerHeight
    let selectedComps, selectedconns = 
        model.Diagram.GetSelected()
        |> Option.map extractState
        |> Option.defaultValue ([],[])
    let sd = scrollData model
    let x' = sd.SheetLeft+sd.SheetX
    let y' = sd.SheetTop+sd.SheetY
    //selectedComps
    //|> List.map (fun comp -> sprintf "(%d,%d)" comp.X  comp.Y )
    //|> String.concat ","
    //|> (fun comps -> printfn "W=(%d,%d) Top=(%d,%d) Bot=(%d,%d)Comps=[%s]\n%A\n\n"  windowX windowY sd.SheetLeft sd.SheetTop x' y' comps sd)
        


    let processMouseMove (ev: Browser.Types.MouseEvent) =
        //printfn "X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        if ev.buttons = 1. then dispatch SelectionHasChanged
        match model.DragMode, ev.buttons with
        | DragModeOn pos , 1.-> 
            let newWidth = model.ViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth)
            SetViewerWidth w |> dispatch
            match currWS model with
            | Some wSMod when w > maxWidth wSMod && not wSMod.WaveAdderOpen ->
                {| LastClk = wSMod.LastClk + 10u
                   ClkW = wSMod.ClkWidth
                   Curs = wSMod.Cursor |}
                |> Error |> SetSimInProgress |> dispatch
            | _ -> ()
            SetDragMode (DragModeOn (int ev.clientX)) |> dispatch
        | DragModeOn _, _ ->  SetDragMode DragModeOff |> dispatch
        | DragModeOff, _-> ()

    div [ OnMouseUp (fun ev -> setDragMode false model dispatch ev; dispatch SelectionHasChanged);
          OnMouseDown (makeSelectionChangeMsg model dispatch)
          OnMouseMove processMouseMove
          Style [ BorderTop "2px solid lightgray"; BorderBottom "2px solid lightgray" ] ] [
        viewNoProjectMenu model dispatch
        viewPopup model
        viewTopMenu model fileMenuViewActions simulateButtonFunc dispatch
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) (canvasVisibleStyle model |> DispMode ) 
        viewNotifications model dispatch
        viewOnDiagramButtons model dispatch
        div [ rightSectionStyle model ]
            [ dividerbar model dispatch
              div [ Style [ Height "100%" ] ] 
                  [ Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.CustomClass "rightSectionTabs"
                                Tabs.Props [Style [Margin 0] ] ]
                              [ Tabs.tab
                                    [ Tabs.Tab.IsActive (model.RightTab = Catalogue) ]
                                    [ a [ OnClick (fun _ -> ChangeRightTab Catalogue |> dispatch ) ] 
                                    [ JSHelpers.tipStr "bottom" "Catalogue" "List of components and custom components from other design sheets to add to this sheet"] ]
                                Tabs.tab
                                    [ Tabs.Tab.IsActive (model.RightTab = Properties) ]
                                    [ a [ OnClick (fun _ -> ChangeRightTab Properties |> dispatch ) ] 
                                    [ JSHelpers.tipStr "bottom" "Properties" "View or change component name, width, etc"] ]

                                Tabs.tab
                                    [ Tabs.Tab.IsActive (model.RightTab = Simulation) ]
                                    [ a [ OnClick (fun _ -> ChangeRightTab Simulation |> dispatch ) ] 
                                    [ JSHelpers.tipStr "bottom" "Simulation" "Simple simulation for combinational logic which allows inputs to be changed manually" ] ]

                                match currWS model with
                                | Some wSMod ->
                                    match wSMod.WaveAdder with
                                    | Some {SimData = Some _}-> 
                                        Tabs.tab
                                            [ Tabs.Tab.IsActive (model.RightTab = WaveSim) ]
                                            [ a [ OnClick (fun _ -> ChangeRightTab WaveSim |> dispatch) ] 
                                            [ str "WaveSim" ] ] 
                                    | Some {SimData=None} -> failwithf "Unexpected WaveAdder with SimData=None"
                                    | None -> div [] []
                                | _ -> div [] []
                              ]
                    viewRightTab model dispatch ] ] ]

    
// -- Update Model

let private getSimulationDataOrFail model msg =
    match model.Simulation with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

// Handle messages triggered by the JS diagram.
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
        { model with HasUnsavedChanges = s }

// Handle messages triggered by keyboard shortcuts.
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
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuZoom z -> 
        zoomDiagram z model
    model

/// get timestamp of current loaded component.
/// is this ever used?
let getCurrentTimeStamp model =
    match model.CurrProject with
    | None -> System.DateTime.MinValue
    | Some p ->
        p.LoadedComponents
        |> List.tryFind (fun lc -> lc.Name = p.OpenFileName)
        |> function | Some lc -> lc.TimeStamp
                    | None -> failwithf "Project inconsistency: can't find component %s in %A"
                                p.OpenFileName ( p.LoadedComponents |> List.map (fun lc -> lc.Name))

/// replace timestamp of current loaded component in model project by current time
let updateTimeStamp model =
    let setTimeStamp (lc:LoadedComponent) = {lc with TimeStamp = System.DateTime.Now}
    match model.CurrProject with
    | None -> model
    | Some p ->
        p.LoadedComponents
        |> List.map (fun lc -> if lc.Name = p.OpenFileName then setTimeStamp lc else lc)
        |> fun lcs -> { model with CurrProject=Some {p with LoadedComponents = lcs}}

/// Check whether current selection is identical to previous selection and 
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
                let newReducedState = 
                    model.Diagram.GetCanvasState()
                    |> Option.map extractReducedState 
                let addReducedState a  =
                    let lastState = a.LastSavedCanvasState
                    match newReducedState with
                    | None -> lastState
                    | Some state -> lastState.Add(proj.OpenFileName, state)

                let update = needsAutoSave proj newReducedState  model.AsyncActivity.LastSavedCanvasState
                let simUpdate = simIsStale newReducedState model.LastSimulatedCanvasState
                printfn "SimUpdate=%A" simUpdate
                if update
                then
                    printfn "AutoSaving at '%s'" (System.DateTime.Now.ToString("mm:ss"))
                    {model with HasUnsavedChanges=true}
                    |> updateTimeStamp
                    |> setActivity (fun a -> {a with LastSavedCanvasState = addReducedState a})
                    |> (fun model' -> 
                        saveOpenFileAction true model' |> ignore
                        setActivity (fun a -> {a with LastAutoSave = a.LastAutoSave.Add(proj.OpenFileName,System.DateTime.Now)}) model')
                else
                    model
                |> setActivity (fun a ->
                                match update || Map.tryFind proj.OpenFileName a.LastSavedCanvasState = None with
                                | true -> {a with LastSavedCanvasState = addReducedState a}
                                | false -> a)
                |> (fun model -> changeSimulationIsStale simUpdate model, cmd)
       
          
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


let update msg model =
    match msg with
    | SelectionHasChanged | SetHighlighted _ -> ()
    | _ ->          
        printfn "\n--------------------------\n%s\n" (spMess msg)
        pp model
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
        match currWS model with
        | Some _ ->
            { model with ConnsToBeHighlighted = true }
        | None -> model
        |> (fun m -> m, Cmd.none)
    | SetSimIsStale b -> 
        changeSimulationIsStale b model, Cmd.none
    | SetSimInProgress par -> 
        { model with SimulationInProgress = Some par }, Cmd.none
    | SimulateWhenInProgress par ->
        // do the simulation for WaveSim and generate new SVGs
        match FileMenuView.getCurrFile model with
        | Some fileName ->
            match currWS model, par with
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
    |> checkForAutoSaveOrSelectionChanged msg



