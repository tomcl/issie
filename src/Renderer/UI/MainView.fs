module DiagramMainView
open Fulma

open Fable.React
open Fable.React.Props

open DiagramStyle
open ModelType
open FileMenuView
open WaveSimHelpers
open WaveSimStyle
open WaveSim
open Sheet.SheetInterface
open DrawModelType

open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom

//------------------Buttons overlaid on Draw2D Diagram----------------------------------//
//--------------------------------------------------------------------------------------//

let viewOnDiagramButtons model dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let dispatch = SheetT.KeyPress >> sheetDispatch

    div [ canvasSmallMenuStyle ] [
        let canvasBut func label = 
            Button.button [ 
                Button.Props [ canvasSmallButtonStyle; OnClick func ] 
                Button.Modifiers [
                    //Modifier.TextWeight TextWeight.Bold
                    Modifier.TextColor IsLight
                    Modifier.BackgroundColor IsSuccess
                    ]
                ] 
                [ str label ]
        canvasBut (fun _ -> dispatch SheetT.KeyboardMsg.CtrlZ ) "< undo" 
        canvasBut (fun _ -> dispatch SheetT.KeyboardMsg.CtrlY ) "redo >" 
        canvasBut (fun _ -> dispatch SheetT.KeyboardMsg.CtrlC ) "copy" 
        canvasBut (fun _ -> dispatch SheetT.KeyboardMsg.CtrlV ) "paste" 

    ]

// -- Init Model

/// Initial value of model
let init() = {
    UserData = {
        WireType = BusWireT.Radial
        ArrowDisplay = true
        UserAppDir = None
        LastUsedDirectory = None
        RecentProjects = None
        }
    LastChangeCheckTime = 0.
    // Diagram = new Draw2dWrapper()
    Sheet = fst (SheetUpdate.init())
    WaveSimulationIsOutOfDate = true
    WaveSimulationInProgress = false
    IsLoading = false
    LastDetailedSavedState = ([],[])
    LastSimulatedCanvasState = None
    LastSelectedIds = [],[]
    CurrentSelected = [],[]
    SelectedComponent = None
    LastUsedDialogWidth = 1
    CurrentStepSimulationStep = None
    WaveSim = Map.empty
    WaveSimSheet = ""
    RightPaneTabVisible = Catalogue
    SimSubTabVisible = StepSim
    CurrentProj = None
    Hilighted = ([], []), []
    Clipboard = [], []
    LastCreatedComponent = None
    SavedSheetIsOutOfDate = false
    PopupViewFunc = None
    PopupDialogData = {
        ProjectPath = ""
        Text = None
        Int = None
        Int2 = None
        MemorySetup = None
        MemoryEditorData = None
        WaveSetup = None
        Progress = None
    }
    Notifications = {
        FromDiagram = None
        FromSimulation = None
        FromWaveSim = None
        FromFiles = None
        FromMemoryEditor = None
        FromProperties = None
    }
    TopMenuOpenState = Closed
    DividerDragMode = DragModeOff
    WaveSimViewerWidth = rightSectionWidthViewerDefault
    SimulationInProgress = None
    ConnsOfSelectedWavesAreHighlighted= false
    CheckWaveformScrollPosition = false
    Pending = []
    UIState = None
}



let makeSelectionChangeMsg (model:Model) (dispatch: Msg -> Unit) (ev: 'a) =
    dispatch SelectionHasChanged

// -- Create View

let viewSimSubTab model dispatch =
    match model.SimSubTabVisible with
    | StepSim -> 
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Step Simulation" ]
            SimulationView.viewSimulation model dispatch
        ]
    | TruthTable ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Truth Table" ]
            div [] [str "Placeholder for Truth Table"]
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; Height "calc(100% - 72px)"; MarginTop "15px" ] ]
            [ viewWaveSim model dispatch ]
            // ( WaveformSimulationView.viewWaveSim model dispatch )

/// Display the content of the right tab.
let private viewRightTab model dispatch =
    match model.RightPaneTabVisible with
    | Catalogue ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Click on a component to add it to the diagram. Hover on components for details." ]
            CatalogueView.viewCatalogue model dispatch
        ]
    | Properties ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Component properties" ]
            SelectedComponentView.viewSelectedComponent model dispatch
        ]

    | Simulation ->
        let subtabs = 
            Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.CustomClass "rightSectionTabs";
                        Tabs.Props [Style [Margin 0] ] ]  
                    [                 
                    Tabs.tab // step simulation subtab
                        [ Tabs.Tab.IsActive (model.SimSubTabVisible = StepSim) ]
                        [ a [  OnClick (fun _ -> 
                            if not model.WaveSimulationInProgress
                            then
                                dispatch <| ChangeSimSubTab StepSim ) 
                            ] [str "Step Simulation"] ] 

                    (Tabs.tab // truth table tab to display truth table for combinational logic
                    [ Tabs.Tab.IsActive (model.SimSubTabVisible = TruthTable) ]
                    [ a [  OnClick (fun _ -> 
                        if not model.WaveSimulationInProgress 
                        then
                            dispatch <| ChangeSimSubTab TruthTable ) 
                        ] [str "Truth Table"] ] )

                    (Tabs.tab // wavesim tab
                    [ Tabs.Tab.IsActive (model.SimSubTabVisible = WaveSim) ]
                    [ a [  OnClick (fun _ -> 
                        // if not model.WaveSimulationInProgress
                        // then
                            dispatch <| ChangeSimSubTab WaveSim
                        ) 
                        ] [str "Wave Simulation"] ])
                    ]
        div [ HTMLAttr.Id "RightSelection"; Style [ Height "100%" ]] 
            [
                //br [] // Should there be a gap between tabs and subtabs for clarity?
                subtabs
                viewSimSubTab model dispatch
            ]

/// determine whether moving the mouse drags the bar or not
let inline setDragMode (modeIsOn:bool) (model:Model) dispatch =
    fun (ev: Browser.Types.MouseEvent) ->        
        makeSelectionChangeMsg model dispatch ev
        //printfn "START X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        match modeIsOn, model.DividerDragMode with
        | true, DragModeOff ->  
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | false, DragModeOn _ -> 
            dispatch <| SetDragMode DragModeOff
        | _ -> ()

/// Draggable vertivcal bar used to divide Wavesim window from Diagram window
let dividerbar (model:Model) dispatch =
    let isDraggable = 
        model.RightPaneTabVisible = Simulation 
        && (model.SimSubTabVisible = WaveSim 
        || model.SimSubTabVisible = TruthTable)
    let heightAttr = 
        let rightSection = document.getElementById "RightSection"
        if (isNull rightSection) then Height "100%"
        else Height rightSection.scrollHeight
    let variableStyle = 
        if isDraggable then [
            BackgroundColor "grey"
            Cursor "ew-resize" 
            Width "10px"

        ] else [
            BackgroundColor "lightgray"
            Width "2px"

        ]
    let commonStyle = [
            heightAttr
            Float FloatOptions.Left
        ]
    div [
            Style <| commonStyle @ variableStyle
            OnMouseDown (setDragMode true model dispatch)       
        ] []

//---------------------------------------------------------------------------------------------------------//
//------------------------------------------VIEW FUNCTION--------------------------------------------------//
//---------------------------------------------------------------------------------------------------------//
/// Top-level application view: as react components that create a react virtual-DOM
let displayView model dispatch =
    JSHelpers.traceIf "view" (fun _ -> "View Function...")
    let windowX,windowY =
        int Browser.Dom.self.innerWidth, int Browser.Dom.self.innerHeight
    //let selectedComps, selectedconns = 
    //    model.Diagram.GetSelected()
    //    |> Option.map extractState
    //    |> Option.defaultValue ([],[])
    
    // TODO
//    let sd = scrollData model
//    let x' = sd.SheetLeft+sd.SheetX
//    let y' = sd.SheetTop+sd.SheetY
    // let wsModelOpt = getCurrentWSMod model

    /// Feed changed viewer width from draggable bar back to Viewer parameters TODO
    let inline setViewerWidthInWaveSim w =
        printf "w: %A" w
        let wsModel = getWSModel model
        if wsModel.State = WaveViewer then
            dispatch <| SetViewerWidth w

            let waveColWidth = w - Constants.namesColMinWidth - Constants.valuesColMinWidth
            let wholeCycles = waveColWidth / int (singleWaveWidth wsModel)
            let wholeCycleWidth = wholeCycles * int (singleWaveWidth wsModel)
            printf "wholeCycles: %A" wholeCycles
            let viewerWidth = Constants.namesColMinWidth + Constants.valuesColMinWidth + wholeCycleWidth

            let wsModel = {
                wsModel with
                    ShownCycles = wholeCycles
                    WaveformColumnWidth = wholeCycleWidth
                }
            dispatch <| SetWSModel wsModel
            // printf "dispatch in setViewerWidthInWaveSim"
            printf "viewerWidth: %A" viewerWidth
            dispatch <| SetViewerWidth viewerWidth
            // dispatch <| SetViewerWidth w

        else
            dispatch <| SetViewerWidth w


    let inline processAppClick topMenu dispatch (ev: Browser.Types.MouseEvent) =
        if topMenu <> Closed then 
            dispatch <| Msg.SetTopMenu Closed

    /// used only to make the divider bar draggable
    let inline processMouseMove (ev: Browser.Types.MouseEvent) =
        //printfn "X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        if ev.buttons = 1. then 
            dispatch SelectionHasChanged
        match model.DividerDragMode, ev.buttons with
        | DragModeOn pos , 1.-> 
            let newWidth = model.WaveSimViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth)
            dispatch <| SetViewerWidth w 
            // setViewerWidthInWaveSim w
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | DragModeOn pos, _ ->
            printf "this case"
            let newWidth = model.WaveSimViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth)
            // dispatch <| SetViewerWidth w 
            setViewerWidthInWaveSim w
            dispatch <| SetDragMode DragModeOff
        | DragModeOff, _-> ()

    let headerHeight = getHeaderHeight
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    // the whole app window
    let cursorText = model.Sheet.CursorType.Text()
    let topCursorText = match model.Sheet.CursorType with | SheetT.Spinner -> "wait" | _ -> ""

    div [ HTMLAttr.Id "WholeApp"
          Key cursorText
          OnMouseMove processMouseMove
          OnClick (processAppClick model.TopMenuOpenState dispatch)
          Style [ 
            //CSSProp.Cursor cursorText
            UserSelect UserSelectOptions.None
            BorderTop "2px solid lightgray"
            BorderBottom "2px solid lightgray"
            Cursor topCursorText ] ] [
        // transient
        FileMenuView.viewNoProjectMenu model dispatch
        
        PopupView.viewPopup model dispatch 
        // Top bar with buttons and menus: some subfunctions are fed in here as parameters because the
        // main top bar function is early in compile order
        FileMenuView.viewTopMenu model dispatch

        if model.PopupDialogData.Progress = None then
            Sheet.view model.Sheet headerHeight (canvasVisibleStyleList model) sheetDispatch
        
        // transient pop-ups
        Notifications.viewNotifications model dispatch
        // editing buttons overlaid bottom-left on canvas
        if model.PopupDialogData.Progress <> None  then
            div [] []
        else
            viewOnDiagramButtons model dispatch

            //--------------------------------------------------------------------------------------//
            //------------------------ left section for Sheet (NOT USED) ---------------------------//
            // div [ leftSectionStyle model ] [ div [ Style [ Height "100%" ] ] [ Sheet.view model.Sheet sheetDispatch ] ]

            //--------------------------------------------------------------------------------------//
            //---------------------------------right section----------------------------------------//
            // right section has horizontal divider bar and tabs
            div [ HTMLAttr.Id "RightSection"; rightSectionStyle model ]
                  // vertical and draggable divider bar
                [ dividerbar model dispatch
                  // tabs for different functions
                  div [ 
                        HTMLAttr.Id "RightSelection"
                        Style [ Height "100%" ] 
                      ] 
                      [ Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.CustomClass "rightSectionTabs"
                                    Tabs.Props [Style [Margin 0] ] ]  
                                    
                                  [ Tabs.tab // catalogue tab to add components
                                        [   Tabs.Tab.IsActive (model.RightPaneTabVisible = Catalogue) ]
                                        [ a [ OnClick (fun _ ->
                                                if not model.WaveSimulationInProgress 
                                                then 
                                                    dispatch <| ChangeRightTab Catalogue ) ] [str "Catalogue" ] ] 
                                                                  
                                    Tabs.tab // Properties tab to view/change component properties
                                        [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Properties) ]                                   
                                        [ a [ OnClick (fun _ -> 
                                                if not model.WaveSimulationInProgress  
                                                then 
                                                    dispatch <| ChangeRightTab Properties )] [str "Properties"  ] ]
                                                    
                                    Tabs.tab // simulation tab to view all simulators
                                        [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Simulation) ]
                                        [ a [  OnClick (fun _ -> 
                                                if not model.WaveSimulationInProgress 
                                                then
                                                    dispatch <| ChangeRightTab Simulation ) 
                                            ] [str "Simulations"] ]
                                  ]
                        viewRightTab model dispatch  ] ] ]
