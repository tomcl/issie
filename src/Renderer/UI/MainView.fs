module DiagramMainView

open Elmish

open Fulma
open Fulma.Extensions.Wikiki

open Fable.React
open Fable.React.Props

open DiagramStyle
open ModelType
open CommonTypes
open Extractor
open CatalogueView
open System
open FileMenuView
open WaveformSimulationView
open Helpers

open Fable.Core
open Fable.Core.JsInterop

//------------------Buttons overlaid on Draw2D Diagram----------------------------------//
//--------------------------------------------------------------------------------------//

let viewOnDiagramButtons model dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let dispatch = Sheet.KeyPress >> sheetDispatch

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
        canvasBut (fun _ -> dispatch Sheet.KeyboardMsg.CtrlZ ) "< undo" 
        canvasBut (fun _ -> dispatch Sheet.KeyboardMsg.CtrlY ) "redo >" 
        canvasBut (fun _ -> dispatch Sheet.KeyboardMsg.CtrlC ) "copy" 
        canvasBut (fun _ -> dispatch Sheet.KeyboardMsg.CtrlV ) "paste" 

    ]

// -- Init Model



/// Initial value of model
let init() = {
    LastChangeCheckTime = 0.
    // Diagram = new Draw2dWrapper()
    Sheet = fst (Sheet.init())
    WaveSimulationIsOutOfDate = true
    ExitDialog = false
    IsLoading = false
    LastDetailedSavedState = ([],[])
    LastSimulatedCanvasState = None
    LastSelectedIds = [],[]
    CurrentSelected = [],[]
    SelectedComponent = None
    LastUsedDialogWidth = 1
    CurrentStepSimulationStep = None
    WaveSim = Map.empty, None
    WaveSimSheet = ""
    RightPaneTabVisible = Catalogue
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
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            Heading.h4 [] [ str "Simulation" ]
            SimulationView.viewSimulation model dispatch
        ]
    | WaveSim -> 
        div [ Style [Width "100%"; Height "calc(100% - 48px)"; MarginTop "15px" ] ]
            ( WaveformSimulationView.viewWaveSim model dispatch )

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
    let isDraggable = model.RightPaneTabVisible = WaveSim
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
    let wsModelOpt = getCurrentWSMod model

    /// Feed changed viewer width from draggable bar back to Viewer parameters TODO
    let inline setViewerWidthInWaveSim w =
        match currWaveSimModel model with
        | Some wSMod when w > maxUsedViewerWidth wSMod && wSMod.WSViewState = WSViewerOpen ->
            match wsModelOpt with
            | Some ws ->
                let simProgressState = 
                    {ws.SimParams with LastClkTime = ws.SimParams.LastClkTime + 10u}
                dispatch <| InitiateWaveSimulation(WSViewerOpen, simProgressState)
            | _ -> ()
        | _ -> ()

 
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
            setViewerWidthInWaveSim w
            dispatch <| SetDragMode (DragModeOn (int ev.clientX))
        | DragModeOn _, _ ->  
            dispatch <| SetDragMode DragModeOff
        | DragModeOff, _-> ()

    let headerHeight = getHeaderHeight
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    // the whole app window

    div [ HTMLAttr.Id "WholeApp"
          Style [UserSelect UserSelectOptions.None]
//          OnMouseUp (fun ev -> 
//          setDragMode false model dispatch ev; 
//          dispatch SelectionHasChanged);
//          OnMouseDown (makeSelectionChangeMsg model dispatch)
          OnMouseMove processMouseMove
          Style [ BorderTop "2px solid lightgray"; BorderBottom "2px solid lightgray" ] ] [
        // transient
        FileMenuView.viewNoProjectMenu model dispatch
        
        FileMenuView.viewExitDialog model dispatch
        
        PopupView.viewPopup model dispatch
        // Top bar with buttons and menus: some subfunctions are fed in here as parameters because the
        // main top bar function is early in compile order
        FileMenuView.viewTopMenu model WaveSimHelpers.fileMenuViewActions WaveformSimulationView.WaveformButtonFunc dispatch

        Sheet.view model.Sheet headerHeight (canvasVisibleStyleList model) sheetDispatch
        
        // transient pop-ups
        PopupView.viewNotifications model dispatch
        // editing buttons overlaid bottom-left on canvas
        viewOnDiagramButtons model dispatch
        
        
        //--------------------------------------------------------------------------------------//
        //------------------------ left section for Sheet (NOT USED) ---------------------------//
        // div [ leftSectionStyle model ] [ div [ Style [ Height "100%" ] ] [ Sheet.view model.Sheet sheetDispatch ] ]

        //--------------------------------------------------------------------------------------//
        //---------------------------------right section----------------------------------------//
        // right section has horizontal divider bar and tabs
        div [ rightSectionStyle model ]
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
                                    [ a [ OnClick (fun _ -> ChangeRightTab Catalogue |> dispatch ) ] [str "Catalogue" ] ] 
                                                                  
                                Tabs.tab // Properties tab to view/change component properties
                                    [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Properties) ]                                   
                                    [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab Properties )] [str "Properties"  ] ] 

                     
                                (Tabs.tab // simulation tab to do combinational simulation
                                    [ Tabs.Tab.IsActive (model.RightPaneTabVisible = Simulation) ]
                                    [ a [  OnClick (fun _ -> dispatch <| ChangeRightTab Simulation ) 
                                        ] [str "Simulation"] ] )
                            
                                /// Optional wavesim tab. If present contains waveforms or waveform editor window
                                (match currWaveSimModel model with
                                | Some {WSViewState=WSClosed} -> 
                                    div [] []
                                | _ ->
                                    Tabs.tab // WaveSim tab - if wavesim exists
                                        [ Tabs.Tab.IsActive (model.RightPaneTabVisible = WaveSim) ]
                                        [ a [ OnClick (fun _ -> dispatch <| ChangeRightTab WaveSim ) ] 
                                        [ str "WaveSim" ] ] ) 
                              ]
                    viewRightTab model dispatch  ] ] ]

