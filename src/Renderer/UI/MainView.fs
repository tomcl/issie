module DiagramMainView

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

// -- Init Model

/// Initial value of activity subrecord in model
let initActivity = {
    AutoSave = Inactive
    LastSavedCanvasState = Map.empty
    LastAutoSaveCheck = System.DateTime.MinValue
    LastAutoSave = Map.empty
    RunningSimulation = false
    }

/// Initial value of model
let init() = {
    AsyncActivity = initActivity
    Diagram = new Draw2dWrapper()
    SimulationIsStale = true
    IsLoading = false
    LastDetailedState = ([],[])
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
            CatalogueView.viewCatalogue model dispatch
        ]
    | Properties ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Component properties" ]
            SelectedComponentView.viewSelectedComponent model dispatch
        ]
    | Simulation ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Simulation" ]
            SimulationView.viewSimulation model dispatch
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; Height "calc(100% - 48px)"; MarginTop "15px" ] ]
            ( WaveformSimulationView.viewWaveSim model dispatch )

/// determine whether moving the mouse drags the bar or not
let setDragMode (modeIsOn:bool) (model:Model) dispatch =
    fun (ev: Browser.Types.MouseEvent) ->        
        makeSelectionChangeMsg model dispatch ev
        //printfn "START X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        match modeIsOn, model.DragMode with
        | true, DragModeOff ->  
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | false, DragModeOn _ -> 
            dispatch <| SetDragMode DragModeOff
        | _ -> ()

/// Draggable vertivcal bar used to divide Wavesim window from Diagram window
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

//---------------------------------------------------------------------------------------------------------//
//------------------------------------------VIEW FUNCTION--------------------------------------------------//
//---------------------------------------------------------------------------------------------------------//
/// Top-level application view: as react components that create a react virtual-DOM
let displayView model dispatch =
    let windowX,windowY =
        int Browser.Dom.self.innerWidth, int Browser.Dom.self.innerHeight
    //let selectedComps, selectedconns = 
    //    model.Diagram.GetSelected()
    //    |> Option.map extractState
    //    |> Option.defaultValue ([],[])
    let sd = scrollData model
    let x' = sd.SheetLeft+sd.SheetX
    let y' = sd.SheetTop+sd.SheetY
 
    /// used only to make the divider bar draggable
    let processMouseMove (ev: Browser.Types.MouseEvent) =
        //printfn "X=%d, buttons=%d, mode=%A, width=%A, " (int ev.clientX) (int ev.buttons) model.DragMode model.ViewerWidth
        if ev.buttons = 1. then 
            dispatch SelectionHasChanged
        match model.DragMode, ev.buttons with
        | DragModeOn pos , 1.-> 
            let newWidth = model.ViewerWidth - int ev.clientX + pos
            let w = 
                newWidth
                |> max minViewerWidth
                |> min (windowX - minEditorWidth)
            dispatch <| SetViewerWidth w 
            match currWaveSimModel model with
            | Some wSMod when w > maxWidth wSMod && wSMod.WaveSimEditorOpen <> WSEditorOpen ->
                let simProgressState = 
                    {|  LastClk = wSMod.LastClk + 10u
                        ClkW = wSMod.ClkWidth
                        Curs = wSMod.Cursor |}
                dispatch <| SetSimInProgress (ChangeParameters simProgressState)
            | _ -> ()
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | DragModeOn _, _ ->  
            dispatch <| SetDragMode DragModeOff
        | DragModeOff, _-> ()

    // the whole app window
    div [ OnMouseUp (fun ev -> 
            setDragMode false model dispatch ev; 
            dispatch SelectionHasChanged);
          OnMouseDown (makeSelectionChangeMsg model dispatch)
          OnMouseMove processMouseMove
          Style [ BorderTop "2px solid lightgray"; BorderBottom "2px solid lightgray" ] ] [
        // transient
        FileMenuView.viewNoProjectMenu model dispatch
        // transient overlay
        PopupView.viewPopup model
        // Top bar with buttons and menus: some subfunctions are fed in here as parameters because the
        // main top bar function is early in compile order
        FileMenuView.viewTopMenu model WaveSimHelpers.fileMenuViewActions WaveformSimulationView.WaveformButtonFunc dispatch
        // Draw2D editor Diagram component with canvas of components and connections
        model.Diagram.CanvasReactElement (JSDiagramMsg >> dispatch) (canvasVisibleStyle model |> DispMode ) 
        // transient pop-ups
        PopupView.viewNotifications model dispatch
        // editing buttons overlaid bottom-left on canvas
        OnDiagramButtonsView.viewOnDiagramButtons model dispatch
        //--------------------------------------------------------------------------------------//
        //---------------------------------right section----------------------------------------//
        // right section has horizontal divider bar and tabs
        div [ rightSectionStyle model ]
              // vertical and draggable divider bar
            [ dividerbar model dispatch
              // tabs for different functions
              div [ Style [ Height "100%" ] ] 
                  [ Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed; Tabs.CustomClass "rightSectionTabs"
                                Tabs.Props [Style [Margin 0] ] ]                              
                              [ Tabs.tab // catalogue tab to add components
                                    [ Tabs.Tab.IsActive (model.RightTab = Catalogue) ]
                                    [ a [ OnClick (fun _ -> CatalogueView.firstTip <- true; ChangeRightTab Catalogue |> dispatch ) ] 
                                    [ JSHelpers.tipStr "bottom" "Catalogue" "List of components and custom components from other design sheets to add to this sheet"] ]                                
                                Tabs.tab // Properties tab to view/chnage component properties
                                    [ Tabs.Tab.IsActive (model.RightTab = Properties) ]
                                    [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab Properties ) ] 
                                    [ JSHelpers.tipStr "bottom" "Properties" "View or change component name, width, etc"] ]

                                Tabs.tab // simulation tab to do combinational simulation
                                    [ Tabs.Tab.IsActive (model.RightTab = Simulation) ]
                                    [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab Simulation ) ] 
                                    [ JSHelpers.tipStr "bottom" "Simulation" "Simple simulation for combinational logic which allows inputs to be changed manually" ] ]
                                /// Optional wavesim tab. If present contains waveforms or waveform aditor window
                                match currWaveSimModel model with
                                | Some {InitWaveSimGraph =  Some _} ->
                                    Tabs.tab // WaveSim tab - if wavesim exists
                                        [ Tabs.Tab.IsActive (model.RightTab = WaveSim) ]
                                        [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab WaveSim ) ] 
                                        [ str "WaveSim" ] ] 
                                | _ -> div [] []
                              ]
                    viewRightTab model dispatch ] ] ]

