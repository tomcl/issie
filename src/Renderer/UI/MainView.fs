module DiagramMainView
open Fulma

open Fable.React
open Fable.React.Props

open DiagramStyle
open ModelType
open TopMenuView
open WaveSimHelpers
open WaveSimStyle
open WaveSimTop
open Sheet.SheetInterface
open DrawModelType
open CommonTypes
open PopupHelpers

open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom

module Constants =
    let memoryCheckMinTime = 500.


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
    MousePointerIsOnRightSection = false
    RunAfterRenderWithSpinner = None
    SpinnerPayload = None
    Spinner = None
    UISheetTrail = []
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
        ImportDecisions = Map.empty
        Int = None
        Int2 = None
        Int3 = None
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
        IntList = None
        IntList2 = None
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




// -- Create View

let viewSimSubTab canvasState model dispatch =
    match model.SimSubTabVisible with
    | StepSim -> 
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Step Simulation" ]
            StepSimulationTop.viewSimulation canvasState model dispatch
        ]
    | TruthTable ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Truth Tables" ]
            TruthTableView.viewTruthTable canvasState model dispatch
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; MarginTop "15px" ;Height "calc(100% - 72px)"; ] ]
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
let inline setDragMode (modeIsOn:bool) (dividerDragMode: DragMode) dispatch =
    fun (ev: Browser.Types.MouseEvent) ->        
        dispatch SelectionHasChanged
        //printfn "START X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        match modeIsOn, dividerDragMode with
        | true, DragModeOff ->  
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | false, DragModeOn _ -> 
            dispatch <| SetDragMode DragModeOff
        | _ -> ()

/// Draggable vertivcal bar used to divide Wavesim window from Diagram window
let dividerbar (model:Model) dispatch =
    let dragMode = model.DividerDragMode
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
            OnMouseDown (setDragMode true dragMode dispatch)       
        ] []

let viewRightTabs canvasState model dispatch =

    let rightPanelVisible = model.RightPaneTabVisible
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
                [ Tabs.Tab.IsActive (rightPanelVisible = Build)]
                [ a [  OnClick (fun _ -> 
                        if rightPanelVisible <> Simulation 
                        then
                            dispatch <| ChangeRightTab Build ) 
                    ] [str "Build"] ]
        else
            null
    
    div [HTMLAttr.Id "RightSelection";Style [ Height "100%"; OverflowY OverflowOptions.Visible]] [
        Tabs.tabs [ 
            Tabs.IsFullWidth; 
            Tabs.IsBoxed; 
            Tabs.CustomClass "rightSectionTabs"
            Tabs.Props [Style [Margin 0]] ; 
            
        ] [
            Tabs.tab // catalogue tab to add components
                [ Tabs.Tab.IsActive (rightPanelVisible = Catalogue) ]
                [ a [ OnClick (fun _ -> 
                        let target = 
                            if model.RightPaneTabVisible = Simulation then
                                Transition else
                                Catalogue
                        dispatch <| ChangeRightTab target ) ] [str "Catalogue" ] ]
            Tabs.tab // Properties tab to view/change component properties
                [ Tabs.Tab.IsActive (rightPanelVisible = Properties) ]                                   
                [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab Properties )] [str "Properties"  ] ]
            Tabs.tab // simulation tab to view all simulators
                [ Tabs.Tab.IsActive (rightPanelVisible = Simulation) ]
                [ a [  OnClick (fun _ -> dispatch <| ChangeRightTab Simulation ) ] [str "Simulations"] ]
            buildTab
        ]
        div [HTMLAttr.Id "TabBody"; belowHeaderStyle "36px"; Style [OverflowY scrollType]] [viewRightTab canvasState model dispatch]

    ]

let mutable lastDragModeOn = false
let mutable lastMemoryCheckTime: float option = None

//---------------------------------------------------------------------------------------------------------//
//------------------------------------------VIEW FUNCTION--------------------------------------------------//
//---------------------------------------------------------------------------------------------------------//
/// Top-level application view: as react components that create a react virtual-DOM
let displayView model dispatch =
    let time = TimeHelpers.getTimeMs()
    if time - Option.defaultValue 0. lastMemoryCheckTime > float Constants.memoryCheckMinTime then
        lastMemoryCheckTime <- Some time
        dispatch CheckMemory
    //JSHelpers.traceIf "view" (fun _ -> $"View Function... ({time}ms)")
    let windowX,windowY =
        int Browser.Dom.self.innerWidth, int Browser.Dom.self.innerHeight

    let inline processAppClick topMenu dispatch (ev: Browser.Types.MouseEvent) =
        if topMenu = Project then
            printf "Setting Top menu closed from processappclick"
            dispatch <| Msg.SetTopMenu Closed
    /// used only to make the divider bar draggable
    let dividerDragMode = model.DividerDragMode
    let wsViewerWidth = model.WaveSimViewerWidth
    if float wsViewerWidth > screenWidth() - 10. then
        dispatch <| SetViewerWidth (int (screenWidth()) - 10)
    let inline processMouseMove (keyUp: bool) (ev: Browser.Types.MouseEvent) =
        //printfn "X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        if ev.buttons = 1. then 
            dispatch SelectionHasChanged
        match dividerDragMode, ev.buttons, keyUp with
        | DragModeOn pos , 1., false-> 
            let newWidth = wsViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth())
            dispatch <| SetDragMode (DragModeOn (int ev.clientX - w + newWidth))
            dispatch <| SetViewerWidth w 
        | DragModeOn pos, _, true ->
            let newWidth = wsViewerWidth - int ev.clientX + pos
            let w =
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth())
            WaveSimNavigation.setViewerWidthInWaveSim w dispatch
            dispatch <| SetDragMode DragModeOff
            dispatch <| SetViewerWidth w 
        | _ -> ()
        if ev.buttons = 0 then
            dispatch <| UpdateModel (fun model -> {model with MousePointerIsOnRightSection = ev.clientX > float (windowX - wsViewerWidth)})

    let headerHeight = getHeaderHeight
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    // the whole app window
    let cursorText = model.Sheet.CursorType.Text()

    let conns = BusWire.extractConnections model.Sheet.Wire
    let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
    let canvasState = comps,conns
    let offsetOpt =
        model.WaveSimSheet
        |> Option.bind (fun wsSheet ->
            Map.tryFind wsSheet model.WaveSim
            |> Option.bind _.ScrollbarTbOffset)

    // mouse ops for wavesim scrollbar
    let wavesimSbMouseMoveHandler (event: Browser.Types.MouseEvent): unit = // if in drag, update scrollbar; otherwise do nothing
        let leftButtonIsdown = (int event.buttons &&& 0x1) <> 0
        let inDrag = Option.isSome offsetOpt
        if inDrag && not leftButtonIsdown then
            // cancel the scroll operation
            ScrollbarMouseMsg (event.clientX, ClearScrollbarDrag, dispatch) |> dispatch
        elif inDrag then 
            ScrollbarMouseMsg (event.clientX, InScrollbarDrag, dispatch) |> dispatch


    let wavesimSbMouseUpHandler (event: Browser.Types.MouseEvent): unit = // if in drag clear drag; otherwise do nothing
        if  Option.isSome offsetOpt
        then
            ScrollbarMouseMsg (event.clientX, ClearScrollbarDrag, dispatch) |> dispatch

    let afterRenderHook: IHTMLProp list =
        match model.RunAfterRenderWithSpinner with
        | Some {FnToRun=fn} ->
            [Ref (fun element ->
                    if element <> null then
                        dispatch <| DispatchDelayed (0, UpdateModel ((fun model -> { model with RunAfterRenderWithSpinner = None }) >> fn dispatch)))]
        | None -> []

    match model.Spinner with
    | Some fn -> 
        dispatch <| UpdateModel fn
    | None -> ()
    if model.CurrentProj = None  (* (((purgeTime - lastPurgeTime) > 10000.) && (JSHelpers.Memory.getProcessMemory() > 500))*) then
        div [HTMLAttr.Id "OpenProject"] [
                TopMenuView.viewNoProjectMenu model dispatch
                UIPopups.viewPopup model dispatch ]
    elif model.TopMenuOpenState = TransientClosed then
        JSHelpers.delayedDispatch dispatch 1000 (SetTopMenu Closed) |> ignore
        div [] []
    else
        div (afterRenderHook @ [
                HTMLAttr.Id "WholeApp"
                OnMouseMove (processMouseMove false)
                OnClick (processAppClick model.TopMenuOpenState dispatch)
                OnMouseUp (processMouseMove true)
                Style [ 
                    Cursor cursorText
                    UserSelect UserSelectOptions.None
                    BorderTop "2px solid lightgray"
                    BorderBottom "2px solid lightgray"
                    CSSProp.Custom("Overflow", "clip clip")
                    Height "calc(100%-4px)"
                    ]
                ]) [
            // transient popups
            UIPopups.viewPopup model dispatch  

            if model.PopupDialogData.Progress = None then
                SheetDisplay.view model.Sheet headerHeight (canvasVisibleStyleList model) sheetDispatch

            Notifications.viewNotifications model dispatch
            let wsModel = ModelHelpers.getWSModel model

            // main window
            if model.PopupDialogData.Progress <> None  then
                // blank it if there is a progress popup
                div [] []
            else
                // Top bar with buttons and menus: some subfunctions are fed in here as parameters because the
                // main top bar function is early in compile order
                TopMenuView.viewTopMenu model dispatch

                // editing buttons overlaid bottom-left on canvas
                viewOnDiagramButtons model dispatch

                //---------------------------------right section----------------------------------------//
                // right section has horizontal divider bar and tabs
                div [
                    HTMLAttr.Id "RightSection";
                    rightSectionStyle model;
                    OnMouseMove wavesimSbMouseMoveHandler;
                    OnMouseUp wavesimSbMouseUpHandler ]
                      // vertical and draggable divider bar
                    [
                        dividerbar model dispatch
                        // tabs for different functions
                        viewRightTabs canvasState model dispatch
                        //div [Id "RightSelection"] [str "selection"]
                    ]
            ]
