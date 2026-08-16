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


//--------------------------------------------------------------------------------------//
//----------------The value on the wire under the cursor, during a simulation-----------//
//--------------------------------------------------------------------------------------//

(*
    Reading a value off the schematic itself is the thing users of every schematic simulator ask
    for first, and until now Issie could only answer it in the waveform table or the step
    simulator's panel - both of which mean looking away from the circuit and finding the signal
    again by name.

    This is the join between the two halves and belongs at the seam: the draw block knows which
    wire the mouse is resting on (SheetT.Model.HoveredWire) and nothing about simulation; a
    simulator knows every value and nothing about the canvas. Neither can do this alone, and giving
    either one the other's knowledge would be worse than the small function here.

    Both simulators are answered, by the same code. All the probe needs is a FastSimulation, a
    cycle and a radix, and each simulator has all three; WaveSimSelect.probeLabelForWire does the
    rest. The waveform simulator is preferred when both are running, because its cursor is a
    deliberate choice of cycle whereas the step simulator's is just wherever the clock has got to.
*)

module Probe =
    open DrawModelType

    /// The simulation to read, the cycle to read it at, and the radix to write it in.
    ///
    /// The waveform simulator first: when both are running, its cursor is where the user has
    /// deliberately put it, and it is the one they are looking at. The step simulator's clock tick
    /// is simply how far it has been stepped, which is still the right answer when it is the only
    /// simulation there is.
    let private source (model: Model) : (SimTypes.FastSimulation * int * NumberBase) option =
        let ws = ModelHelpers.getWSModel model
        match ws.State with
        | WaveSimState.Success ->
            Some(Simulator.getFastSim (), ws.CursorExactClkCycle, ws.Radix)
        | _ ->
            match model.CurrentStepSimulationStep with
            // ClockTickNumber, not fs.ClockTick: it is the tick the step simulator is showing and
            // has written on its own button, so the probe and the panel cannot disagree
            | Some(Ok simData) -> Some(simData.FastSim, simData.ClockTickNumber, simData.NumberBase)
            | _ -> None

    /// The font the label is written in. Named once because its width has to be measured with the
    /// same one it is drawn with, or the box and the text inside it disagree.
    let private labelFont =
        { DrawHelpers.defaultText with
            TextAnchor = "start"
            FontSize = "11px"
            FontFamily = "helvetica"
            Fill = "#443300" }

    /// The horizontal span of the schematic the user can actually see, in draw block coordinates.
    ///
    /// Read from the DOM rather than the model, because only half of it is in the model: the
    /// scroll position is copied there, but the width of the window being scrolled inside is not,
    /// and it changes with the divider bar and the window. `getVisibleScreenCentre` reads the same
    /// element for the same reason.
    let private visibleXSpan (zoom: float) : (float * float) option =
        match Browser.Dom.document.getElementById "Canvas" with
        | null -> None
        | canvas when canvas.clientWidth <= 0.0 -> None
        | canvas -> Some(canvas.scrollLeft / zoom, (canvas.scrollLeft + canvas.clientWidth) / zoom)

    /// A label drawn beside the cursor giving the value on the wire under it. Empty unless a
    /// simulation of this sheet's design is running and the mouse is resting on a wire whose value
    /// that simulation knows.
    let view (model: Model) : ReactElement list =
        match model.Sheet.HoveredWire, source model, model.Sheet.Action with
        // only while nothing is being dragged: during a gesture the cursor is doing something else
        // and a label following it is in the way
        | Some cid, Some(fs, cycle, radix), SheetT.CurrentAction.Idle ->
            match WaveSimSelect.probeLabelForWire fs cycle radix model.Sheet.Wire cid with
            | None -> []
            | Some text ->
                let pos = model.Sheet.LastMousePos
                // measured, not counted: the width decides where the label is allowed to sit, so a
                // per-character guess would clip the very case this is here to prevent
                let padding = 5.0
                let w = DrawHelpers.getTextWidthInPixels labelFont text + 2.0 * padding
                let h = 18.0
                let y = pos.Y - h - 6.0
                // Beside the cursor, but never off the side of the window. Slid rather than
                // flipped to the other side of the pointer: the label stays where the eye already
                // is, and only the last few pixels of travel differ. Vertical needs no such care -
                // the label sits above the cursor, and a wire at the very top of the window is
                // reached by scrolling, which moves the label with it.
                let x =
                    let preferred = pos.X + 12.0
                    match visibleXSpan model.Sheet.Zoom with
                    | None -> preferred
                    | Some(visibleLeft, visibleRight) ->
                        let margin = 4.0
                        // min first, then max: a label too wide for the window keeps its left edge
                        // on screen, which is the readable end of "NAME = value"
                        preferred
                        |> min (visibleRight - w - margin)
                        |> max (visibleLeft + margin)
                // the label must never be what the mouse is over: it follows the cursor, so a
                // label that took the pointer would take it away from the wire it is describing
                // and the two would chase each other
                [ g [ Style [ CSSProp.PointerEvents "none" ] ] [
                    rect [
                        X x; Y y
                        SVGAttr.Width w; SVGAttr.Height h
                        SVGAttr.Rx 3.0
                        SVGAttr.Fill "#fffbe6"
                        SVGAttr.Stroke "#b0a060"
                        SVGAttr.StrokeWidth 0.8
                    ] []
                    DrawHelpers.makeText (x + padding) (y + 4.0) text labelFont
                  ] ]
        | _ -> []

//------------------Banner over a library component's sheet-----------------------------//
//--------------------------------------------------------------------------------------//

/// Says, for as long as it applies, that the sheet on screen is a library component being looked
/// at rather than one of the user's own.
///
/// A banner and not a confirmation before opening: nothing here is destructive or hard to undo, so
/// there is nothing to confirm, and the question would come at the one moment the user has not yet
/// seen what they are being asked about. This answers the question they will actually ask, which
/// is why they cannot edit what is in front of them.
///
/// Named by the library and component their author gave them, not by the L<n>_ sheet the project
/// keeps them in - that name is a detail of how the component is stored.
let viewReadOnlyBanner (model: Model) =
    match model.CurrentProj with
    | Some p when ModelHelpers.openSheetIsReadOnly model ->
        let origin =
            p.LoadedComponents
            |> List.tryFind (fun ldc -> ldc.Name = p.OpenFileName)
            |> Option.bind (fun ldc ->
                match ldc.Form with
                | Some (Library (libName, compName)) -> Some $"{compName} · {libName} library"
                | _ -> None)
            |> Option.defaultValue p.OpenFileName
        div [ canvasReadOnlyBannerStyle model ] [
            str $"👁 {origin} — read-only, and open only until this project is closed"
        ]
    | _ -> null

//------------------Buttons overlaid on Draw2D Diagram----------------------------------//
//--------------------------------------------------------------------------------------//

let viewOnDiagramButtons model dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    /// the whole app's dispatch, which the shadowing below hides from everything after it
    let appDispatch = dispatch
    let dispatch = SheetT.KeyPress >> sheetDispatch

    // All four edit the sheet, so on a library component being looked at they are shown disabled
    // rather than hidden: the buttons are a fixed landmark on the canvas, and one that came and
    // went would be more startling than one that is plainly unavailable.
    let readOnly = ModelHelpers.openSheetIsReadOnly model

    div [ canvasSmallMenuStyle ] [
        let canvasBut func label =
            Button.button [
                Button.Props [ canvasSmallButtonStyle; OnClick (fun ev -> if not readOnly then func ev) ]
                Button.Disabled readOnly
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

        // Only between a copy and the paste that follows it - see SheetT.Model.OfferPasteArray.
        // Repeating a fragment is worth putting in front of someone at the moment they have just
        // copied one, and is not worth a button that is always there; the Edit menu has it either
        // way. A fifth permanent button would also crowd the row, which sits over the schematic.
        if model.Sheet.OfferPasteArray && not readOnly then
            canvasBut (fun _ -> UIPopups.PasteArray.pasteArrayPopup model appDispatch) "paste array"
    ]

// -- Init Model




let init() = {
    KeyFocusPane = LeftPane
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
    CircuitCheck = { Verdict = None; CheckPending = false }
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
    PendingDragAddition = None
    DragPlacement = None
    ProjectBrowser = None
    TopSheetChoiceDeclined = Set.empty
    ComponentLibraries = ComponentLibraries.findLibraries ()
    OpenLibrary = None
    CatalogueSearch = ""
    ShowLibrarySheets = false
    OpenedLibrarySheets = Set.empty
    ReadOnlyBaseline = None
    PopupViewFunc = None
    PopupDialogData = {
        DialogState= None
        ProjectPath = ""
        Text = None
        Text2 = None
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
    SheetMenuPinned = false
    DividerDragMode = DragModeOff
    WaveSimViewerWidth = rightSectionWidthViewerDefault
    ConnsOfSelectedWavesAreHighlighted= false
    Pending = []
    UIState = None
    BuildVisible = false
    CodeEditorState = None
}




// -- Create View

let viewSimSubTab canvasState model dispatch =
    match model.SimSubTabVisible with
    | StepSim ->
        // A flex column of bounded height, so that the pane below can keep its controls in place
        // and scroll only the signals - see StepSimulationTop.viewSimulation. Taken off the height
        // are the sub-tab bar above this div, at the same 36px the tab bar above that is allowed,
        // and its own top margin.
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px"
                     Height "calc(100% - 51px)"
                     Display DisplayOptions.Flex; FlexDirection "column" ] ] [
            Heading.h4 [] [ str "Step Simulation" ]
            StepSimulationTop.viewSimulation canvasState model dispatch
        ]
    | TruthTable ->
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ] [
            //Heading.h4 [] [ str "Truth Tables" ]
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
        
        // The height is the pane less this div's own top margin, and the line under the heading is
        // sized by its text. Both used to say otherwise - an unparseable calc, and a one-line blurb
        // asking for the full height - and neither meant anything while the tab body had no height
        // for a percentage to be of. Give it one and the blurb fills the pane, pushing the
        // catalogue itself out of sight.
        div [ Style [Width "90%"; MarginLeft "5%"; MarginTop "15px" ; Height "calc(100% - 15px)"] ] [
            Heading.h4 [] [ str "Catalogue" ]
            div [ Style [ MarginBottom "15px" ] ]
                [ str "Drag components to sheet, or click and click to drop. Hover for details." ]
            CatalogueView.viewCatalogue model dispatch
        ]
        
    | Properties ->
        // This pane shows one of three different things, and used to head all of them "Component
        // properties" - including the case where nothing is selected and what is shown is the open
        // SHEET. The heading and the line under it say which of the three it is, so that a sheet
        // and an instance of a sheet cannot be mistaken for one another.
        let heading, blurb =
            match model.Sheet.SelectedComponents, model.CurrentProj with
            | [], Some proj ->
                $"Sheet properties — {proj.OpenFileName}",
                Some "Properties of the open sheet. Select a component for its own properties."
            | [], None -> "Sheet properties", None
            | [compId], _ ->
                let comp = SymbolUpdate.displayedComponent model.Sheet.Wire.Symbol compId
                let blurb =
                    match comp.Type with
                    // A library component is not presented as an instance of a sheet: that sheet
                    // is not part of the design the user navigates, and naming it here would put
                    // back the L<n>_ name the header deliberately leaves out.
                    | Custom {Form = Some (Library _)} -> None
                    | Custom custom ->
                        Some $"One instance of sheet {custom.Name}. These values apply to this instance only."
                    | _ -> None
                "Component properties", blurb
            | _ -> "Component properties", None
        // A flex column, so that the heading and the line under it keep their place and the fields
        // below them scroll. The height counts this div's own top margin, as the catalogue's does,
        // so that a pane whose contents just fit does not scroll by the height of its margin.
        div [ HTMLAttr.Id "PropertiesPane";
              Style [Width "90%"; MarginLeft "5%"; MarginTop "15px"; Height "calc(100% - 15px)"
                     Display DisplayOptions.Flex; FlexDirection "column" ] ] [
            Heading.h4 [] [ str heading ]
            (match blurb with
             | None -> null
             | Some text -> div [ Style [MarginBottom "15px"; FontSize "12px"] ] [ str text ])
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
                    // singular, as its two neighbours are and as the panel inside it is: the tab
                    // read "Truth Tables" and then headed itself "Truth Table"
                    [ a [  OnClick (fun _ -> dispatch <| ChangeSimSubTab TruthTable ) ] [str "Truth Table"] ])

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
        div [HTMLAttr.Id "TabBody"; belowHeaderStyle "36px" scrollType] [viewRightTab canvasState model dispatch]

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
    // Debug builds publish what the draw block is showing as window.issie, for
    // scripts/inspect-canvas.js to read over the DevTools protocol - see ModelHelpers.
    // A function rather than the data, so that nothing is computed unless something asks: all
    // this costs per render is one closure and one object.
    if JSHelpers.debugLevel > 0 then
        window?issie <- createObj [
            "canvas" ==> (fun () -> ModelHelpers.canvasInspection model)
            "raw" ==> (fun () -> ModelHelpers.canvasRaw model)
        ]
    let windowX,windowY =
        int Browser.Dom.self.innerWidth, int Browser.Dom.self.innerHeight

    let inline processAppClick topMenu dispatch (ev: Browser.Types.MouseEvent) =
        // A click anywhere else - the schematic, most of all - dismisses an open menu. The Sheet
        // menu is the exception when pinned: the point of the pin is that the hierarchy stays in
        // view while sheets are opened from it and the schematic is worked on.
        match topMenu with
        | Files when model.SheetMenuPinned -> ()
        | Project | Edit | View | Files ->
            dispatch <| Msg.SetTopMenu Closed
        | Closed | TransientClosed -> ()
    /// used only to make the divider bar draggable
    let dividerDragMode = model.DividerDragMode
    let wsViewerWidth = model.WaveSimViewerWidth
    if float wsViewerWidth > screenWidth() - 10. then
        dispatch <| SetViewerWidth (int (screenWidth()) - 10)
    let inline processMouseMove (keyUp: bool) (ev: Browser.Types.MouseEvent) =
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

    // Model.RunAfterRenderWithSpinner used to be run from a Ref on the div below, which fires in
    // React's commit. That is not the same moment as the browser painting what was committed, so
    // the spinner a function had just switched on could still be unpainted when the function
    // blocked the thread for the length of a simulation build. Update.runWhenPainted schedules it
    // instead, from the update that asks for it.

    match model.Spinner with
    | Some fn -> 
        dispatch <| UpdateModel fn
    | None -> ()
    if model.CurrentProj = None  (* (((purgeTime - lastPurgeTime) > 10000.) && (JSHelpers.Memory.getProcessMemory() > 500))*) then
        div [HTMLAttr.Id "OpenProject"] [
                TopMenuView.viewNoProjectMenu model dispatch
                UIPopups.viewPopup model dispatch
                // Notifications were drawn only once a project was open, so anything that went
                // wrong on the way to opening one - the screen where opening is all there is to
                // do - set a notification that nothing rendered, and looked like silence.
                Notifications.viewNotifications model dispatch ]
    elif model.TopMenuOpenState = TransientClosed then
        JSHelpers.delayedDispatch dispatch 1000 (SetTopMenu Closed) |> ignore
        div [] []
    else
        div [
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
                ] [
            // transient popups
            UIPopups.viewPopup model dispatch

            // A component being dragged out of the catalogue, drawn at the cursor. It belongs to
            // the whole window rather than to either pane, because the gesture crosses between
            // them: the drag starts over the catalogue and ends over the canvas.
            CatalogueView.viewDragGhost model

            /// Which pane the keyboard follows, changed only by clicking in one.
            let claimKeyFocus pane =
                if model.KeyFocusPane <> pane then
                    dispatch <| UpdateModel(fun m -> { m with KeyFocusPane = pane })

            if model.PopupDialogData.Progress = None then
                div [
                    OnMouseDown (fun _ -> claimKeyFocus LeftPane)
                    OnWheel (fun (e: Browser.Types.WheelEvent) ->
                        if e.ctrlKey || e.metaKey then
                            e.preventDefault()
                            dispatch (Sheet (
                                if e.deltaY > 0. then
                                    SheetT.KeyPress SheetT.KeyboardMsg.ZoomOut
                                else
                                    SheetT.KeyPress SheetT.KeyboardMsg.ZoomIn
                                ))
                        else
                            ()
                        );
                ] [
                    SheetDisplay.view model.Sheet headerHeight (canvasVisibleStyleList model)
                        (Probe.view model) sheetDispatch
                ]

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

                // says that the sheet below belongs to a library component and cannot be changed
                viewReadOnlyBanner model

                // editing buttons overlaid bottom-left on canvas
                viewOnDiagramButtons model dispatch

                //---------------------------------right section----------------------------------------//
                // right section has horizontal divider bar and tabs
                div [
                    HTMLAttr.Id "RightSection";
                    rightSectionStyle model;
                    OnMouseDown (fun _ -> claimKeyFocus RightPane);
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
