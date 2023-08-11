﻿module DiagramMainView
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
open CommonTypes

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



let init() = {
    SpinnerPayload = None
    Spinner = None
    UserData = {
        WireType = BusWireT.Radial
        ArrowDisplay = true
        UserAppDir = None
        LastUsedDirectory = None
        RecentProjects = None
        Theme = SymbolT.ThemeType.Colourful
        }
    LastChangeCheckTime = 0.
    // Diagram = new Draw2dWrapper()
    Sheet = fst (SheetUpdate.init())
    IsLoading = false
    LastDetailedSavedState = ([],[])
    LastSimulatedCanvasState = None
    LastSelectedIds = [],[]
    CurrentSelected = [],[]
    SelectedComponent = None
    LastUsedDialogWidth = 1
    CurrentStepSimulationStep = None
    CurrentTruthTable = None
    TTConfig = TruthTableUpdate.tTTypeInit
    WaveSim = Map.empty
    WaveSimSheet = None
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
        Progress = None
        ConstraintTypeSel = None
        ConstraintIOSel = None
        ConstraintErrorMsg = None
        NewConstraint = None
        AlgebraInputs = None
        AlgebraError = None
        VerilogCode = None
        VerilogErrors = []
        BadLabel = false
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
    ConnsOfSelectedWavesAreHighlighted= false
    Pending = []
    UIState = None
    BuildVisible = false
}



let makeSelectionChangeMsg (model:Model) (dispatch: Msg -> Unit) (ev: 'a) =
    dispatch SelectionHasChanged

// -- Create View

let viewSimSubTab canvasState model dispatch =
    match model.SimSubTabVisible with
    | StepSim -> 
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Step Simulation" ]
            SimulationView.viewSimulation canvasState model dispatch
        ]
    | TruthTable ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Truth Tables" ]
            TruthTableView.viewTruthTable canvasState model dispatch
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; Height "calc(100% - 72px)"; MarginTop "15px" ] ]
            [ viewWaveSim canvasState model dispatch ]

/// Display the content of the right tab.
let private  viewRightTab canvasState model dispatch =
    let pane = model.RightPaneTabVisible
    match pane with
    | Catalogue | Transition ->
        
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ; Height "calc(100%-100px)"] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ; Height "100%"; OverflowY OverflowOptions.Auto] ] 
                [ str "Click on a component to add it to the diagram. Hover on components for details." ]
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
                        [ a [  OnClick (fun _ -> dispatch <| ChangeSimSubTab StepSim ) ] [str "Step Simulation"] ]  

                    (Tabs.tab // truth table tab to display truth table for combinational logic
                    [ Tabs.Tab.IsActive (model.SimSubTabVisible = TruthTable) ]
                    [ a [  OnClick (fun _ -> dispatch <| ChangeSimSubTab TruthTable ) ] [str "Truth Tables"] ])

                    (Tabs.tab // wavesim tab
                    [ Tabs.Tab.IsActive (model.SimSubTabVisible = WaveSim) ]
                    [ a [  OnClick (fun _ -> dispatch <| ChangeSimSubTab WaveSim) ] [str "Wave Simulation"] ])
                    ]
        div [ HTMLAttr.Id "RightSelection2"; Style [Height "100%"]] 
            [
                //br [] // Should there be a gap between tabs and subtabs for clarity?
                subtabs
                viewSimSubTab canvasState model dispatch
            ]
    | Build ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Build" ]
            div [ Style [ MarginBottom "15px" ] ] [ str "Compile your design and upload it to one of the supported devices" ]
            BuildView.viewBuild model dispatch
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
        else Height "100%" //rightSection.scrollHeight
    let variableStyle = 
        if isDraggable then [
            BackgroundColor "grey"
            Cursor "ew-resize" 
            Width Constants.dividerBarWidth

        ] else [
            BackgroundColor "lightgray"
            Width "2px"
            Height "100%"

        ]
    let commonStyle = [
            heightAttr
            Float FloatOptions.Left
        ]
    div [
            Style <| commonStyle @ variableStyle
            OnMouseDown (setDragMode true model dispatch)       
        ] []

let viewRightTabs canvasState model dispatch =
    /// Hack to avoid scrollbar artifact changing from Simulation to Catalog
    /// The problem is that the HTML is bistable - with Y scrollbar on the catalog <aside> 
    /// moves below the tab body div due to reduced available width, keeping scrollbar on. 
    /// Not fully understood.
    /// This code temporarily switches the scrollbar off during the transition.
    let scrollType = 
        if model.RightPaneTabVisible = Transition then 
            dispatch <| ChangeRightTab Catalogue // after one view in transition it is OK to go to Catalogue
            OverflowOptions.Clip // ensure no scrollbar temporarily after the transition
        else 
            OverflowOptions.Auto
    
    let buildTab =
        if model.BuildVisible then
            Tabs.tab
                [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Build)]
                [ a [  OnClick (fun _ -> 
                        if model.RightPaneTabVisible <> Simulation 
                        then
                            dispatch <| ChangeRightTab Build ) 
                    ] [str "Build"] ]
        else
            null
    
    div [HTMLAttr.Id "RightSelection";Style [ Height "100%"; OverflowY OverflowOptions.Auto]] [
        Tabs.tabs [ 
            Tabs.IsFullWidth; 
            Tabs.IsBoxed; 
            Tabs.CustomClass "rightSectionTabs"
            Tabs.Props [Style [Margin 0]] ; 
            
        ] [
            Tabs.tab // catalogue tab to add components
                [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Catalogue) ]
                [ a [ OnClick (fun _ -> 
                        let target = 
                            if model.RightPaneTabVisible = Simulation then
                                Transition else
                                Catalogue
                        dispatch <| ChangeRightTab target ) ] [str "Catalogue" ] ]
            Tabs.tab // Properties tab to view/change component properties
                [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Properties) ]                                   
                [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab Properties )] [str "Properties"  ] ]
            Tabs.tab // simulation tab to view all simulators
                [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Simulation) ]
                [ a [  OnClick (fun _ -> dispatch <| ChangeRightTab Simulation ) ] [str "Simulations"] ]
            buildTab
        ]
        div [HTMLAttr.Id "TabBody"; belowHeaderStyle "36px"; Style [OverflowY scrollType]] [viewRightTab canvasState model dispatch]

    ]
let mutable testState:CanvasState = [],[]
let mutable lastDragModeOn = false

//---------------------------------------------------------------------------------------------------------//
//------------------------------------------VIEW FUNCTION--------------------------------------------------//
//---------------------------------------------------------------------------------------------------------//
/// Top-level application view: as react components that create a react virtual-DOM
let displayView model dispatch =
    let time = int(TimeHelpers.getTimeMs()) % 10000
    //JSHelpers.traceIf "view" (fun _ -> $"View Function... ({time}ms)")
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


    let inline processAppClick topMenu dispatch (ev: Browser.Types.MouseEvent) =
        if topMenu <> Closed then 
            dispatch <| Msg.SetTopMenu Closed
    /// used only to make the divider bar draggable
    let inline processMouseMove (keyUp: bool) (ev: Browser.Types.MouseEvent) =
        //printfn "X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        if ev.buttons = 1. then 
            dispatch SelectionHasChanged
        match model.DividerDragMode, ev.buttons, keyUp with
        | DragModeOn pos , 1., false-> 
            let newWidth = model.WaveSimViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth())
            dispatch <| SetDragMode (DragModeOn (int ev.clientX - w + newWidth))
            dispatch <| SetViewerWidth w 
        | DragModeOn pos, _, true ->
            let newWidth = model.WaveSimViewerWidth - int ev.clientX + pos
            let w =
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth())
            setViewerWidthInWaveSim w model dispatch
            dispatch <| SetDragMode DragModeOff
            dispatch <| SetViewerWidth w 
        | _ -> ()

    let headerHeight = getHeaderHeight
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    // the whole app window
    let cursorText = model.Sheet.CursorType.Text()
    let topCursorText = match model.Sheet.CursorType with | SheetT.Spinner -> "wait" | _ -> ""

    let conns = BusWire.extractConnections model.Sheet.Wire
    let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
    let canvasState = comps,conns   
    match model.Spinner with
    | Some fn -> 
        dispatch <| UpdateModel fn
    | None -> ()
    div [ HTMLAttr.Id "WholeApp"
          Key cursorText
          OnMouseMove (processMouseMove false)
          OnClick (processAppClick model.TopMenuOpenState dispatch)
          OnMouseUp (processMouseMove true)
          Style [ 
            //CSSProp.Cursor cursorText
            UserSelect UserSelectOptions.None
            BorderTop "2px solid lightgray"
            BorderBottom "2px solid lightgray"
            OverflowX OverflowOptions.Auto
            Height "calc(100%-4px)"
            Cursor topCursorText ] ] [
        // transient
        
        FileMenuView.viewNoProjectMenu model dispatch
        
        
        PopupHelpers.viewPopup model dispatch 
        // Top bar with buttons and menus: some subfunctions are fed in here as parameters because the
        // main top bar function is early in compile order
        FileMenuView.viewTopMenu model dispatch

        if model.PopupDialogData.Progress = None then
            SheetDisplay.view model.Sheet headerHeight (canvasVisibleStyleList model) sheetDispatch
        
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
                  viewRightTabs canvasState model dispatch ] ]
