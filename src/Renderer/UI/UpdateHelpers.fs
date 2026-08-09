module UpdateHelpers

open Elmish
open Fulma
open Fable.React
open Fable.React.Props
open ElectronAPI
open FilesIO
open SimGraphTypes
open SimTypes
open ModelType
open ModelHelpers
open CommonTypes
open CanvasExtractor
open MenuHelpers
open TopMenuView
open Sheet.SheetInterface
open BusWireUpdateHelpers
open DrawModelType
open Fable.SimpleJson
open NumberHelpers
open DiagramStyle
open Browser
open PopupHelpers
open Optics.Optic
open Optics.Operators
open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open EEExtensions

module Constants =
    /// time between checks in ms
    let memoryUpdateCheckTime = 500

    /// how long the "copied the project path" confirmation stays up, in ms
    let copiedPathNotificationTime = 2500

//-------------------------------------------------------------------------------------------------//
//-------------------------------------MESSAGE TRACING---------------------------------------------//
//-------------------------------------------------------------------------------------------------//


///Used to filter specific mouse messages based on mouse data.
let matchMouseMsg (msgSelect: DrawHelpers.MouseOp -> bool) (msg : Msg) : bool =
    match msg with
    | Sheet sMsg ->
        match sMsg with
        | SheetT.MouseMsgOrig (_,op,_) ->
            msgSelect op
        | _ -> false
    | _ -> false

/// short summary used where Sheet messages are too complex to print
let shortDSheetMsg (sMsg: SheetT.Msg) =
    match sMsg with
    | SheetT.Msg.MouseMsgOrig(ev,op,_) -> Some $"Mouse {op}"
    | _ -> Some $"Sheet %10A{sMsg}"

/// short summary of wavesim message which has a lot of data
let shortDWSM (ws: WaveSimModel) =
    let fs = Simulator.getFastSim()
    Some <| sprintf $"WS<{fs.SimulatedTopSheet}->{ws.StartCycle}-{ws.CursorDisplayCycle}-\
            {ws.ShownCycles} Waves:{ws.AllWaves.Count} ({ws.SelectedWaves.Length})>"

/// Function returning a short but usually informative display of message
/// used when message tracing (see Sheet menu to which on or off).
/// Parameters that might be very large (like fastsimulation, or Model, or Symbols) should not be
/// displayed using printf "%A".
let shortDisplayMsg (msg:Msg) =
    match msg with
    | AnyKeyPress code -> Some $"AnyKeyPress %A{code}"
    | WaveSimKeyPress _ -> None
    | ChangeWaveSimMultiplier n ->
        List.tryItem n Constants.multipliers
        |> Option.map (fun n -> $"Set WS multiplier to {n}")
        |> Option.defaultValue $"Invalid Ws mult key of {n}"
        |> Some
    | CheckMemory
    | DispatchDelayed _
    | RunAfterRender _
    | SaveModel -> None
    | SheetBackAction _ -> Some "SheetBackAction"
    | FileCommand(fc,_) -> Some $"{fc}"
    | UpdateUISheetTrail _
    | ShowExitDialog
    | PinReadOnlyCanvas
    | SynchroniseCanvas -> None
    | Sheet sheetMsg -> shortDSheetMsg sheetMsg
    | JSDiagramMsg (InitCanvas _ )-> Some "JSDiagramMsg.InitCanvas"
    | JSDiagramMsg _ -> None
    | StartSimulation x -> Some $"""StartSimulation({match x with | Ok _ -> "OK" | Error x -> "Error"})"""
    | AddWSModel (s,ws) -> Some $"AddWSModel:{s}->{shortDWSM ws}"
    | SetWSModel ws -> Some $"SetWSModel:{Simulator.getFastSim().SimulatedTopSheet}->{shortDWSM ws}"
    | UpdateWSModel _ -> Some "Updating WS model"
    | SetWSModelAndSheet (ws,s)-> Some $"SetWSModelAndSheet:{s}->{shortDWSM ws}"
    | GenerateWaveforms ws -> Some $"GenerateWaveforms:{shortDWSM ws}"
    | GenerateCurrentWaveforms -> Some $"Generate Current Waveforms"
    | RefreshWaveSim ws -> Some "RefreshWaveSim"
    | SetWaveSheetSelectionOpen _
    | SetWaveComponentSelectionOpen _-> Some "SetWaveComponentSelectionOpen"
    | SetWaveGroupSelectionOpen _
    | LockTabsToWaveSim 
    | UnlockTabsFromWaveSim -> None
    | TryStartSimulationAfterErrorFix _ -> Some "TryStartSimulationAfterErrorFix"
    | SetSimulationGraph _ -> Some "SetSimulationGraph"
    | SetSimulationBase _
    | IncrementSimulationClockTick _
    | EndSimulation
    | EndWaveSim -> None
    | TruthTableMsg ttMsg ->
        match ttMsg with
        | GenerateTruthTable _ -> Some "GenerateTruthTable"
        | RegenerateTruthTable
        | FilterTruthTable
        | SortTruthTable
        | DCReduceTruthTable
        | HideTTColumns
        | CloseTruthTable
        | ClearInputConstraints
        | ClearOutputConstraints
        | AddInputConstraint _
        | AddOutputConstraint _
        | DeleteInputConstraint _
        | DeleteOutputConstraint _
        | ToggleHideTTColumn _
        | ClearHiddenTTColumns
        | ClearDCMap
        | SetTTSortType _
        | MoveColumn _ -> None
        | SetIOOrder _ -> Some "SetIOOrder"
        | SetTTAlgebraInputs _ -> None
        | SetTTBase _ -> None
        | SetTTGridCache _ -> Some "SetTTGridCache"
        | TogglePopupAlgebraInput _ -> Some  "TogglePopupAlgebraInput"
        | SetPopupInputConstraints _ 
        | SetPopupOutputConstraints _ 
        | SetPopupConstraintTypeSel _ 
        | SetPopupConstraintIOSel _ 
        | SetPopupConstraintErrorMsg _ 
        | SetPopupNewConstraint _ 
        | SetPopupAlgebraInputs _ 
        | SetPopupAlgebraError _
        | StartDraggingColumn _
        | DragColumnEnter _
        | EndDraggingColumn
        | CancelDraggingColumn -> None
    | ScrollbarMouseMsg _

    | ChangeRightTab _ -> None
    | ChangeSimSubTab _ -> None
    | SetHighlighted (comps,conns) -> Some $"SetHighlighted: {comps.Length} comps, {conns.Length} conns"
    | SetSelWavesHighlighted x -> Some $"SetSelWavesHighlighted{x.Length}"
    | SetClipboard _ -> Some "SetClipboard"
    | SetCreateComponent _ -> Some "SetCreateComponent"
    | SetProject _ -> Some "SetProject"
    | UpdateProject _ 
    | UpdateModel _
    | UpdateImportDecisions _
    | UpdateProjectWithoutSyncing _ 
    | ShowPopup _ 
    | ShowStaticInfoPopup _ 
    | ClosePopup 
    | SetPopupDialogBadLabel _ 
    | SetPopupDialogText _ 
    | SetPopupDialogText2 _
    | SetPopupDialogCode _ 
    | SetPopupDialogVerilogErrors _ 
    | SetPopupDialogInt _ 
    | SetPopupDialogInt2 _
    | SetPopupDialogInt3 _
    | SetPopupDialogTwoInts _ 
    | SetPopupDialogIntList _
    | SetPopupDialogIntList2 _
    | AddPopupDialogParamSpec _
    | ClearPopupDialogParamSpec _
    | SetPropertiesExtraDialogText _ 
    | SetPopupDialogBadLabel _ 
    | SetPopupDialogMemorySetup _  
    | SetPopupMemoryEditorData _ 
    | SetPopupProgress _ 
    | UpdatePopupProgress _ 
    | SimulateWithProgressBar _ -> None
    | SetSelectedComponentMemoryLocation _ -> Some "SetSelectedComponentMemoryLocation"
    | CloseDiagramNotification
    | SetSimulationNotification _ 
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification _ 
    | CloseFilesNotification
    | SetMemoryEditorNotification _ 
    | CloseMemoryEditorNotification
    | SetPropertiesNotification _ 
    | ClosePropertiesNotification
    | SetTopMenu _ 
    | ReloadSelectedComponent _ 
    | SetDragMode _
    // one per mouse move for as long as a catalogue item is being carried: far too many to trace,
    // and each says only where the cursor now is
    | MoveDragPlacement _
    // one a second for as long as the project browser is open, saying only "look again"
    | TickProjectBrowser
    // Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth _
    | MenuAction _ 
    | DiagramMouseEvent
    | ContextMenuAction _ -> None
    | ContextMenuItemClick _
    | SelectionHasChanged -> Some "Selection has changed"
    | StartDragPlacement _ -> Some "StartDragPlacement"
    | DropDragPlacement pos -> Some $"DropDragPlacement at {pos}"
    | EndDragPlacement -> Some "EndDragPlacement"
    | SetProjectBrowserFolder folder -> Some $"Browsing {folder}"
    | MoveProjectBrowserSelection n -> Some $"Browser selection moves by {n}"
    | GoToProjectBrowserParent -> Some "Browser up"
    | OpenProjectBrowserSelection _ -> Some "Browser open selection"
    | SetIsLoading _
    | SetRouterInteractive _
    | CloseApp
    | SetExitDialog _
    | ExecutePendingMessages _ 
    | DoNothing
    | StartUICmd _
    | FinishUICmd
    | ChangeBuildTabVisibility
    | ReadUserData _
    | SetUserData _
    | ChangeBuildTabVisibility
    | Benchmark
    | SetThemeUserData _ -> None
    | ExecCmd _ -> Some "ExecCmd"
    | ExecFuncInMessage _ -> Some "ExecFuncInMessage"
    | ExecFuncAsynch _ -> Some "ExecFuncAsync"
    | ExecCmdAsynch _ -> Some "ExecCmdAsynch"
    | SendSeqMsgAsynch _ -> Some "SendSeqMsgAsynch"
    | CodeEditorMsg _ -> Some "CodeEditorMsg"
    | CheckTopSheetChoice -> Some "CheckTopSheetChoice"
    | ApplyComputedDisplayValues -> Some "ApplyComputedDisplayValues"




/// Human-readable info on a message, or "" for one not worth showing.
///
/// Never `%A` on the message itself: a Msg can carry a whole FastSimulation, a Model or a canvas,
/// and printing one of those crashes the renderer. shortDisplayMsg names every case by hand for
/// exactly that reason. Mouse drags and moves are suppressed unless Log.Mouse is on, since they
/// are the majority of all messages and would bury everything else.
let getMessageTraceString (msg: Msg) =
    let noDisplayMouseOp (op: DrawHelpers.MouseOp) =
        (op = DrawHelpers.Drag || op = DrawHelpers.Move) && not (Log.isOn Log.Mouse)
    let noDisplayMessage = function
        | Sheet (SheetT.Msg.Wire(BusWireT.Msg.Symbol(SymbolT.MouseMsg _ | SymbolT.ShowPorts _ ))) -> true
        | _ -> false

    if matchMouseMsg noDisplayMouseOp msg || noDisplayMessage msg then
        ""
    else
        match shortDisplayMsg msg with
        | Some shortName -> shortName
        | None -> Helpers.sprintInitial 70 $"{msg}"

/// Count the message just handled, and trace it if Log.Update is on.
///
/// The counting is unconditional and costs an add and a compare: the message name is only built
/// when this message was the slowest so far, which is what makes the periodic summary affordable.
let traceMessage startOfUpdateTime (msg: Msg) ((model, cmdL): Model * Cmd<Msg>) =
    let updateTime = TimeHelpers.getTimeMs() - startOfUpdateTime
    Log.countMessage updateTime (fun () -> getMessageTraceString msg)
    if Log.isOn Log.Update then
        match getMessageTraceString msg with
        | "" -> ()
        | str -> Log.dbg Log.Update $"%6.1f{updateTime}ms {str}"
    model, cmdL

let mutable lastMemoryUpdateCheck = 0.

let updateAllMemoryCompsIfNeeded (model:Model) =
    let time = TimeHelpers.getTimeMs()
    if time - lastMemoryUpdateCheck > Constants.memoryUpdateCheckTime && (getWSModel model).State = Success then
        lastMemoryUpdateCheck <- time
        model
        |> MemoryEditorView.updateAllMemoryComps
    else
        model


let verilogOutputPage sheet fPath  =
    div [] [
        str $"You can write sheet '{sheet}' (and its subsheets) in either simulation or synthesis format. The output will be written to:"
        Text.div [ 
            Modifiers [ Modifier.TextWeight TextWeight.Bold]
            Props [Style [TextAlign TextAlignOptions.Center; CSSProp.Padding "10px"; FontFamily "monospace"; FontSize "15px"]]] [str $"%s{Helpers.cropToLength 55 false fPath}.v"]
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

/// Ask which Verilog flavour to write for a sheet, then write it.
/// Reached from the sheet's pill in the sheet menu, so it names the sheet rather than assuming
/// whichever one happens to be open.
let verilogOutputPopup (sheetName: string) (model: Model) (dispatch: Msg -> Unit) =
    mapOverProject () model (fun p ->
        let fPath = FilesIO.pathJoin [| p.ProjectPath; sheetName |]
        choicePopup
            "Verilog Output"
            (verilogOutputPage sheetName fPath)
            "Write Synthesis Verilog"
            "Write Simulation Verilog"
            (fun forSim _ ->
                let vType = if forSim then Verilog.ForSynthesis else Verilog.ForSimulation
                SimulationView.verilogOutputForSheet sheetName vType model dispatch
                dispatch ClosePopup)
            dispatch)

//-------------------------------------------------------------------------------------------------//
//-------------------------------------CONTEXT MENUS-----------------------------------------------//
//-------------------------------------------------------------------------------------------------//

(*

        Implement right-click context menus throughout Issie:

        getContextMenu - detemines menu items for a given context

        processContextMenuClick - determines action (typically a single message) for each menu item.

        Common/ContextMenus.contextMenus - names and item names for each menu.

*)

type RightClickElement =
    | DBCustomComp of SymbolT.Symbol * CustomComponentType
    | DBScalingBox of list<ComponentId>
    | DBComp of SymbolT.Symbol
    | DBWire of Wire: BusWireT.Wire * ASeg: BusWireT.ASegment list
    | DBCanvas of XYPos
    | DBInputPort of string
    | DBOutputPort of string
    | IssieElement of string
    | SheetMenuBreadcrumb of Sheet: SheetTree * IsSubSheet: bool
    | ProjectPathBreadcrumb of Path: string
    | WaveSimHelp
    | NoMenu
    

let mutable rightClickElement: RightClickElement = NoMenu

/// Function that works out from the right-click event and model
/// what the current context menu should be.
/// output should be a menu name as defined in ContextMenus.contextMenus, or "" for no menu.
let getContextMenu (e: Browser.Types.MouseEvent) (model: Model) : string =
    //--------- the sample code below shows how useful info can be extracted from e --------------//
    // calculate equivalent sheet XY coordinates - valid if mouse is over schematic.
    let symbols = model.Sheet.Wire.Symbol.Symbols
    let bwModel = model.Sheet.Wire
    let sheetXYPos = SheetDisplay.getDrawBlockPos e DiagramStyle.getHeaderHeight model.Sheet
    let element:Types.Element = unbox e.target
    let htmlId = try element.id with | e -> "invalid"
    let elType = try element.nodeName with | e -> "invalid"
    let drawOn = Sheet.mouseOn model.Sheet sheetXYPos
    let mouseInScalingBox = 
        let insideBox (pos: XYPos) boundingBox =
            let {BoundingBox.TopLeft={X = xBox; Y= yBox}; H=hBox; W=wBox} = boundingBox
            pos.X >= xBox - 50.0 && pos.X <= xBox + wBox + 50.0 && pos.Y >= yBox - 50.0 && pos.Y <= yBox + hBox + 50.0
        match model.Sheet.ScalingBox with
        | None -> false
        | Some b -> insideBox sheetXYPos b.ScalingBoxBound
            //insideBox (model.Sheet.LastMousePos) b.ScalingBoxBound

    rightClickElement <- // mutable so that we have this info also in the callback from main
        match drawOn, htmlId, elType with
        | _, "refreshButton", _
        | _, "selectButton", _
        | _, "selectRamButton", _
        | _, "startEndButton", _ ->
            WaveSimHelp
        | _, "ProjectPath", _ ->
            // no project means the bar is showing placeholder text, which is not worth copying
            model.CurrentProj
            |> Option.map (fun p -> ProjectPathBreadcrumb p.ProjectPath)
            |> Option.defaultValue NoMenu
        | _, elId, _ when String.startsWith "SheetMenuBreadcrumb:" elId ->
            let nameParts = elId.Split(":",System.StringSplitOptions.RemoveEmptyEntries)
            model.CurrentProj
            |> Option.map (fun p ->
                Map.tryFind nameParts[1] (getSheetTrees false p) 
                |> Option.map ( fun sheet ->
                    SheetMenuBreadcrumb (sheet, nameParts.Length > 2)))
            |> Option.flatten
            |> Option.defaultValue NoMenu

        | SheetT.MouseOn.Canvas, _ , "path"
        | _, "WaveSimHelp", _ ->
            WaveSimHelp
        | SheetT.MouseOn.Canvas, "DrawBlockSVGTop", _ ->
            if mouseInScalingBox then  
                DBScalingBox model.Sheet.SelectedComponents
            else 
                DBCanvas sheetXYPos

        | SheetT.MouseOn.Canvas, x, _ ->
            IssieElement (element.ToString())

        | SheetT.MouseOn.Component compId, _, _->
            if mouseInScalingBox then  
                DBScalingBox model.Sheet.SelectedComponents
            else 
                match Map.tryFind compId symbols with
                | Some {Component = {Type = Custom ct}} ->
                    DBCustomComp (symbols[compId], ct)
                | Some sym when sym.Annotation = None ->
                    DBComp sym
                | _ -> NoMenu

        | SheetT.MouseOn.Connection connId, _, _ ->
            Map.tryFind connId bwModel.Wires
            |> function | None ->
                            NoMenu
                        | Some wire ->
                            let segs = getClickedSegment  bwModel connId sheetXYPos
                            match segs with
                            | [] ->
                                NoMenu
                            | segs ->
                                DBWire(wire, segs)

        | SheetT.MouseOn.InputPort (InputPortId s, _),_ , _ ->
            DBInputPort s
        | SheetT.MouseOn.OutputPort (OutputPortId s, _),_ , _ ->
            DBOutputPort s
        | _ -> NoMenu
            
    // return the desired menu
    /// The menus for a component come in pairs: the one with the extra item to add waveforms from
    /// the component is offered only when there are waveforms to add. ContextMenus.fs is compiled
    /// into the main process too and cannot see the model, so the choice has to be made here.
    let ifWavesToOffer (sym: SymbolT.Symbol) (waveSimMenu: string) (menu: string) =
        if WaveSimSelect.compWavesToOffer model sym.Id <> [] then waveSimMenu else menu

    /// Whether the sheet a custom component instantiates came from a component library, and so is
    /// reachable only by asking to view it.
    let instantiatesLibrarySheet (ct: CustomComponentType) =
        model.CurrentProj
        |> Option.bind (getFileInProject ct.Name)
        |> Option.map ComponentLibraries.isLibrarySheet
        |> Option.defaultValue false

    /// The sheet on screen cannot be edited, so every menu on it is a reduced one.
    let readOnly = openSheetIsReadOnly model

    match rightClickElement with
    | SheetMenuBreadcrumb (sheet, _) when Set.contains sheet.SheetName model.OpenedLibrarySheets ->
        "SheetMenuBreadcrumbLibrary"
    | SheetMenuBreadcrumb _ ->
        if JSHelpers.debugLevel > 0 then "SheetMenuBreadcrumbDev" else "SheetMenuBreadcrumb"
    | ProjectPathBreadcrumb _ ->
        "ProjectPath"
    | DBScalingBox _ ->
        // every item on it rotates, flips, deletes, copies or moves
        if readOnly then "" else "ScalingBox"
    | DBCustomComp (sym, ct) when instantiatesLibrarySheet ct ->
        let viewed = Set.contains ct.Name model.OpenedLibrarySheets
        match readOnly, viewed with
        | true, true -> "LibraryInstanceOpenReadOnly"
        | true, false -> "LibraryInstanceReadOnly"
        | false, true -> ifWavesToOffer sym "LibraryInstanceOpenWaveSim" "LibraryInstanceOpen"
        | false, false -> ifWavesToOffer sym "LibraryInstanceWaveSim" "LibraryInstance"
    | DBCustomComp (sym, _) ->
        if readOnly then "ComponentReadOnly"
        else ifWavesToOffer sym "CustomComponentWaveSim" "CustomComponent"
    | DBComp sym ->
        if readOnly then "ComponentReadOnly"
        else ifWavesToOffer sym "ComponentWaveSim" "Component"
    | DBCanvas _ ->
        if readOnly then "CanvasReadOnly" else "Canvas"
    | DBWire _ ->
        // its only item unfixes the wire's routing
        if readOnly then "" else "Wire"
    | WaveSimHelp ->
        "WaveSimHelp"
    | _ ->
        Log.dbg Log.Sheet $"right-clicked on '{drawOn.ToString()}', which has no context menu"
        "" // default is no menu
            


/// Open a library component's sheet to be looked at, and go to it.
///
/// The sheet becomes visible in the Sheets menu and stays reachable until the project is closed,
/// but is read-only throughout: it is not the user's to change, and a library component that
/// differed from the library it came from would not be one. Only this sheet is opened - a
/// component built from other library components keeps them shut, each needing the same
/// deliberate click of its own.
let viewLibrarySheet (name: string) (model: Model) (dispatch: Msg -> unit) =
    let p = Option.get model.CurrentProj
    let model = map openedLibrarySheets_ (Set.add name) model
    openFileInProject name p model dispatch
    map uISheetTrail_ (fun trail -> p.OpenFileName :: trail) model

/// Put a viewed library component's sheet away again.
///
/// If it is the sheet on screen the user has to be taken off it first, since it is about to
/// become unreachable: back the way they came, or failing that to any sheet of their own. With
/// nowhere at all to go the sheet stays open, rather than stranding the user on a sheet the rest
/// of Issie has stopped believing in.
///
/// The order matters. Leaving a sheet writes it back - to the loaded components, and to a backup
/// file - and what stops that for a library sheet is its being in OpenedLibrarySheets. So the
/// sheet is left while it is still marked, and put away only in the model this returns.
let hideLibrarySheet (name: string) (model: Model) (dispatch: Msg -> unit) =
    let p = Option.get model.CurrentProj
    let putAway model = map openedLibrarySheets_ (Set.remove name) model
    if p.OpenFileName <> name then
        putAway model
    else
        let goTo =
            model.UISheetTrail
            |> List.filter (fun sheet ->
                sheet <> name && List.exists (fun ldc -> ldc.Name = sheet) p.LoadedComponents)
            |> List.tryHead
            |> Option.orElseWith (fun () ->
                p.LoadedComponents
                |> List.tryFind (fun ldc -> ldc.Name <> name && not (ComponentLibraries.isLibrarySheet ldc))
                |> Option.map (fun ldc -> ldc.Name))
        match goTo with
        | None ->
            Log.warn $"there is no sheet to return to, so '{name}' has been left open"
            model
        | Some sheet ->
            openFileInProject sheet p model dispatch
            model
            |> putAway
            |> map uISheetTrail_ (List.filter (fun s -> s <> sheet))

/// Function that implement action based on context menu item click.
/// menuType is the menu from chooseContextMenu.
/// item will be one of the possible items in this menu.
let processContextMenuClick
        (menuType: string) // name of menu
        (item: string) // name of menu item clicked
        (dispatch: Msg -> unit) // dispatch function
        (model: Model)
            : Model * Cmd<Msg> = // can change state directly (Model) or via a message wrapped in Cmd.ofMsg.

    let withNoCmd (model: Model) = model, Cmd.none
    let withMsg (msg: Msg) (model : Model)  = model,Cmd.ofMsg msg
    let withMsgs (msgs: Msg list) (model : Model)  = model, Cmd.batch ( msgs |> List.map Cmd.ofMsg)
    let withWireMsg msg = withMsg (Msg.Sheet (SheetT.Msg.Wire msg))
    let sheetDispatch = Sheet >> dispatch
    let keyDispatch = SheetT.KeyPress >> sheetDispatch
    let rotateDispatch = SheetT.Rotate >> sheetDispatch
    let flipDispatch = SheetT.Flip >> sheetDispatch
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)

    match rightClickElement,item with
    | SheetMenuBreadcrumb(sheet,_), "Rename" ->
        renameFileInProject sheet.SheetName p model dispatch
        withNoCmd model
    | SheetMenuBreadcrumb(sheet,_), "Duplicate" ->
        MiscMenuView.duplicateSheet sheet.SheetName model dispatch
        withNoCmd model
    | SheetMenuBreadcrumb(sheet,_), "Delete" ->
        deleteFileConfirmationPopup sheet.SheetName model dispatch
        withNoCmd model

    | SheetMenuBreadcrumb(sheet,_), "Save as library component" ->
        MiscMenuView.saveAsLibraryComponent sheet.SheetName model dispatch
        withNoCmd model

    | SheetMenuBreadcrumb(sheet,_), "Set as top" ->
        // changing the top changes the values every sheet displays with, so the open one is
        // redrawn. Which ancestor parameters exist above a sheet changes too, but that is now
        // surfaced by the bind button in an instance's properties rather than by a popup.
        model
        |> setTopSheetState sheet.SheetName
        |> withMsg ApplyComputedDisplayValues

    | SheetMenuBreadcrumb(sheet,_), "Write design as Verilog" ->
        verilogOutputPopup sheet.SheetName model dispatch
        withNoCmd model

    | SheetMenuBreadcrumb(sheet,isSubSheet), "Lock" ->
        model
        |> changeLockState isSubSheet sheet (fun _ -> Locked)
        |> withNoCmd

    | SheetMenuBreadcrumb(sheet,isSubSheet), "Unlock" ->
        model
        |> changeLockState isSubSheet sheet (fun _ -> Unlocked)
        |> withNoCmd

    | SheetMenuBreadcrumb(sheet,isSubSheet), "Lock Subtree" ->
        model
        |> changeSubtreeLockState isSubSheet sheet (fun _ -> Locked) 
        |> withNoCmd 

    | SheetMenuBreadcrumb(sheet,isSubSheet), "Unlock Subtree" ->
        model
        |> changeSubtreeLockState isSubSheet sheet (fun _ -> Unlocked)
        |> withNoCmd 

    | ProjectPathBreadcrumb path, "Copy path" ->
        // the bar shows a cropped path, so confirm what actually reached the clipboard
        electron.clipboard.writeText path
        model
        |> withMsgs
            [ SetFilesNotification (Notifications.successNotification $"Copied {path}" CloseFilesNotification)
              DispatchDelayed (Constants.copiedPathNotificationTime, CloseFilesNotification) ]

    | ProjectPathBreadcrumb path, "Open directory" ->
        let openDirectory (dispatch: Msg -> unit) =
            FilesIO.openFolderInFileManager path (fun reason ->
                dispatch <| SetFilesNotification
                    (Notifications.errorFilesNotification $"Could not open {path}: {reason}"))
        model, Cmd.ofEffect openDirectory

    | DBCustomComp(sym,_), "Move ports"
    | DBCustomComp(sym,_), "Resize symbol" ->
        // Selecting the component is part of entering the mode, not a nicety: a selected symbol is
        // drawn lightgreen, which is what the blue of the draggable ports and corners reads
        // against. Without it the mode would be invisible on a clocked custom component, which is
        // lightblue itself.
        let mode = if item = "Move ports" then SheetT.EditPorts else SheetT.EditSize
        // Set the whole appearance, not just the part being turned on: switching straight from one
        // mode to the other would otherwise leave the previous mode's affordance showing until the
        // next mouse move.
        let show =
            match mode with
            | SheetT.EditPorts ->
                [ SymbolT.ShowCustomOnlyPorts [sym.Id]; SymbolT.HideCustomCorners [sym.Id] ]
            | SheetT.EditSize ->
                [ SymbolT.ShowPorts []; SymbolT.ShowCustomCorners [sym.Id] ]
        model
        |> set (sheet_ >-> SheetT.symbolEdit_) (Some(sym.Id, mode))
        |> set (sheet_ >-> SheetT.selectedWires_) []
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> withMsgs
            (Sheet(SheetT.Msg.Wire(BusWireT.Msg.Symbol(SymbolT.SelectSymbols [sym.Id])))
             :: (show |> List.map (fun m -> Sheet(SheetT.Msg.Wire(BusWireT.Msg.Symbol m)))))

    | DBCustomComp(_,ct), item when item = ContextMenus.viewLibraryItem ->
        viewLibrarySheet ct.Name model dispatch
        |> withNoCmd

    | DBCustomComp(_,ct), item when item = ContextMenus.hideLibraryItem ->
        hideLibrarySheet ct.Name model dispatch
        |> withNoCmd

    | SheetMenuBreadcrumb(sheet,_), item when item = ContextMenus.hideLibraryItem ->
        hideLibrarySheet sheet.SheetName model dispatch
        |> withNoCmd

    | DBCustomComp(_,ct), "Go to sheet" ->
        let p = Option.get model.CurrentProj
        openFileInProject ct.Name p model dispatch
        model
        |> map uISheetTrail_ (fun trail -> p.OpenFileName :: trail)
        |> withNoCmd

    | DBComp sym, "Rotate Clockwise (Ctrl+Right)" ->
        rotateDispatch Degree90
        model
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> withNoCmd

    | DBComp sym, "Rotate AntiClockwise (Ctrl+Left)" ->
        rotateDispatch Degree270
        model
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> withNoCmd
    
    | DBComp sym, "Flip Vertical (Ctrl+Up)" ->
        flipDispatch SymbolT.FlipVertical
        model
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> withNoCmd
    
     | DBComp sym, "Flip Horizontal (Ctrl+Down)" ->
        flipDispatch SymbolT.FlipHorizontal
        model
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> withNoCmd
    
    | DBComp sym, "Properties" | DBCustomComp(sym, _), "Properties" ->
         model
        |> set selectedComponent_ (Some sym.Component)
        |> set (sheet_ >-> SheetT.selectedWires_) []
        |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
        |> set rightPaneTabVisible_ Properties
        |> withWireMsg (BusWireT.Msg.Symbol (SymbolT.SelectSymbols [sym.Id]))
    
    | DBComp sym, "Add waveforms to viewer"
    | DBCustomComp (sym, _), "Add waveforms to viewer" ->
        // Show the wave simulator as well as the dialog, so that the waveforms being added can be
        // seen going in - and because the dialog is drawn by the wave simulator's own view.
        model
        |> set rightPaneTabVisible_ Simulation
        |> set simSubTabVisible_ WaveSim
        |> withMsg (UpdateWSModel (fun ws -> {ws with PortSelectComp = Some sym.Id}))

    | DBComp _, "Delete (DEL)" ->
        keyDispatch SheetT.KeyboardMsg.DEL
        model  
        |> withNoCmd
    
    | DBComp sym, "Copy (Ctrl+C)" ->
        let model =
            if model.Sheet.SelectedComponents = [] then // make sure at least one symbol is selected for copy
                model
                |> map (sheet_ >-> SheetT.wire_ >-> BusWireT.symbol_) (fun model -> SymbolUpdate.selectSymbols model [sym.Id])
                |> set (sheet_ >-> SheetT.selectedComponents_) [sym.Id]
            else model
        model  
        |> withMsg (Sheet (SheetT.KeyPress SheetT.KeyboardMsg.CtrlC))
    
    | DBWire (wire, aSeg), "Unfix Wire" ->
        let changeManualSegToAuto : BusWireT.Segment -> BusWireT.Segment =
            map BusWireT.mode_ (function | BusWireT.Manual -> BusWireT.Auto | m -> m)
        model
        |> map (sheet_ >-> SheetT.wireOf_ wire.WId >-> BusWireT.segments_)  (List.map changeManualSegToAuto)
        |> map (sheet_ >-> SheetT.wire_) (BusWireSeparate.separateAndOrderModelSegments [wire.WId])
        |> withNoCmd
    
    | DBScalingBox selectedcomps, "Rotate Clockwise (Ctrl+Right)"->
        rotateDispatch Degree90
        model 
        |> withWireMsg (BusWireT.Msg.UpdateConnectedWires selectedcomps)

    | DBScalingBox selectedcomps, "Rotate AntiClockwise (Ctrl+Left)"->
        rotateDispatch Degree270
        model 
        |> withWireMsg (BusWireT.Msg.UpdateConnectedWires selectedcomps)
    
    | DBScalingBox selectedcomps, "Flip Vertical (Ctrl+Up)"->
        flipDispatch SymbolT.FlipVertical
        model 
        |> withWireMsg (BusWireT.Msg.UpdateConnectedWires selectedcomps)
    
    | DBScalingBox selectedcomps, "Flip Horizontal (Ctrl+Down)" ->
        flipDispatch SymbolT.FlipHorizontal
        model 
        |> withWireMsg (BusWireT.Msg.UpdateConnectedWires selectedcomps)
    
    | DBScalingBox _, "Delete Box (DEL)" ->
        keyDispatch SheetT.KeyboardMsg.DEL
        model  
        |> withNoCmd
    
    | DBScalingBox _, "Copy Box (Ctrl+C)" ->
        keyDispatch SheetT.KeyboardMsg.CtrlC
        model  
        |> withNoCmd
    
    | DBCanvas pos, "Zoom-in (Ctrl+plus) and centre"  ->
        model
        |> map (sheet_ >-> SheetT.zoom_)  (fun zoom -> min Sheet.Constants.maxMagnification (zoom*Sheet.Constants.zoomIncrement))
        |> withMsg (Sheet (SheetT.Msg.KeepZoomCentered pos))

    | DBCanvas pos, "Zoom-out (Ctrl+minus)" ->
        keyDispatch SheetT.KeyboardMsg.ZoomOut
        model
        |> withNoCmd

    | DBCanvas _, "Fit to window (Ctrl+0)" ->
        keyDispatch SheetT.KeyboardMsg.CtrlW
        model
        |> withNoCmd
    
    | DBCanvas pos, "Paste (Ctrl+V)" ->
        keyDispatch SheetT.KeyboardMsg.CtrlV
        model
        |> withNoCmd

    | DBCanvas _, "Reroute all wires" ->
        keyDispatch SheetT.KeyboardMsg.CtrlW
        model
        |> Optics.Optic.map
                (sheet_ >-> SheetT.wire_)
                (model.Sheet.Wire.Wires.Keys |> Seq.toList |> BusWireSeparate.updateWireSegmentJumpsAndSeparations)
        |> withNoCmd

    | WaveSimHelp, feature ->
        UIPopups.viewWaveInfoPopup dispatch feature
        withNoCmd model

    | DBCanvas _, "Properties" ->
        model
        |> set selectedComponent_ None
        |> set (sheet_ >-> SheetT.selectedComponents_) []
        |> set (sheet_ >-> SheetT.selectedWires_) []
        |> set rightPaneTabVisible_ Properties
        |> withNoCmd

    | _ ->
        Log.warn $"context menu item not implemented: {rightClickElement} -> {item}"
        model
        |> withNoCmd

let filterByOKSheets (model: Model) (sheet: string) =
    match model.CurrentProj with
    | Some p when p.OpenFileName = sheet -> false
    | Some p when p.LoadedComponents |> List.forall (fun ldc -> ldc.Name <> sheet) -> false
    | _ -> true   

//-------------------------------------------------------------------------------------------------//
//-------------------------------------UPDATE FUNCTIONS--------------------------------------------//
//-------------------------------------------------------------------------------------------------//

(* a message Msg.DoSomething will have an equivalent update function doSomethingF of type

  : DoSomething -> Model -> Model

  Update functions can thus be used in Model -> Model pipelines to implement operations

  Move update.fs code to this file as an update function if it is long, or if it needs to be called
  as a function as well as from a message.

*)

/// Adapter function to pipeline adding a default "Cmd.none" command to a model as returned
/// in update function.
let withNoMsg (model: Model) : Model * Cmd<Msg> =
    model, Cmd.none

/// Implement action of top bar 'Back' button using the UISheetTrail
let processSheetBackAction (dispatch: Msg -> unit) (model: Model)  =
    let goodSheets = // filter trail to remove no-longer-valid sheets
        model.UISheetTrail
        |> List.filter (filterByOKSheets model) // make sure trail still exists!
    let trail =
        match goodSheets with
        | [] ->
            []
        | (sheet :: others) ->
            let p = Option.get model.CurrentProj
            openFileInProject sheet p model dispatch
            others
    model
    |> set uISheetTrail_ trail


/// Read persistent user data from file in userAppDir.
/// Store in Model UserData.
let readUserData (userAppDir: string) (model: Model) : Model * Cmd<Msg> =
    let addAppDirToUserData model = 
        {model with UserData = {model.UserData with UserAppDir = Some userAppDir}}

    let modelOpt =
        try
            let jsonRes = tryReadFileSync <| pathJoin [|userAppDir;"IssieSettings.json"|]
            jsonRes
            |> Result.bind (fun json -> Json.tryParseNativeAs<UserData> json)
            |> Result.bind (fun (data: UserData) -> Ok {model with UserData = data})
            |> (function | Ok model -> model | Error _ -> Log.warn "could not read the saved user settings"; model)
            |> addAppDirToUserData 
            |> userDataToDrawBlockModel
            |> Some
        with
        | e -> None
    match modelOpt with
    | Some model -> model, Cmd.none
    | None -> addAppDirToUserData model, Cmd.none

let writeUserData (model:Model) =
    model.UserData.UserAppDir
    |> Option.map (fun userAppDir ->
        try
            let data = drawBlockModelToUserData model model.UserData
            Json.serialize<UserData> data |> Ok
        with
        | e -> Error "Can't write settings on this PC because userAppDir does not exist"
        |> Result.bind (fun json -> writeFile (pathJoin [|userAppDir;"IssieSettings.json"|]) json)
        |> Result.mapError (fun mess -> $"Write error on directory {userAppDir}: %s{mess}")
        |> function | Error mess -> Log.error mess | _ -> ())
    |> ignore
    model


/// subfunction used in model update function
let getSimulationDataOrFail model msg =
    match model.CurrentStepSimulationStep with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

/// handle Menu actions that may need Model data
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuSaveFile -> 
        MenuHelpers.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuSaveProjectInNewFormat ->
        MenuHelpers.saveOpenProjectInNewFormat model |> ignore
    | MenuNewFile -> 
        TopMenuView.addFileToProject model dispatch
    | MenuLostFocus ->
        ()
        
    | MenuExit ->
        FileUpdate.doActionWithSaveFileDialog "Exit ISSIE" CloseApp model dispatch ()
    // Verilog output is no longer a menu command: it is per-sheet, and reached by right-clicking
    // that sheet's pill in the sheet menu. See verilogOutputPopup above.
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

let currentSheetIsOutOfDate (model : Model) : bool = 
    let last = model.LastChangeCheckTime // NB no check to reduce total findChange time implemented yet - TODO if needed

    match model.CurrentProj with
    | None -> false
    | Some prj ->
        //For better efficiency just check if the save button
        let savedComponent = 
            prj.LoadedComponents
            |> List.find (fun lc -> lc.Name = prj.OpenFileName)
        let canv = savedComponent.CanvasState
        let canv' = model.Sheet.GetCanvasState ()
        savedComponent.LoadedComponentIsOutOfDate ||
        ((canv <> canv') && not (CanvasExtractor.compareCanvas 100. canv canv'))
        //|> TimeHelpers.instrumentInterval "findChange" start

/// Needed so that constant properties selection will work
/// Maybe good idea for other things too?
let resetDialogIfSelectionHasChanged newModel oldModel : Model =
    let newSelected = newModel.Sheet.SelectedComponents
    if newSelected.Length = 1 && newSelected <> oldModel.Sheet.SelectedComponents then
        newModel
        |> map popupDialogData_ (
            set text_ None >>
            set int_ None
        )
    else newModel

let updateComponentMemory (addr:bigint) (data:bigint) (compOpt: Component option) =
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
    writeUserData model |> ignore
    renderer.ipcRenderer.send("exit-the-app",[||])

/// Tests physical equality on two objects.
/// Used because Msg type does not support structural equality.
/// **DANGER** will only work for messages which are physically the the same.
/// In this use case that is fine.
let isSameMsg = LanguagePrimitives.PhysicalEquality 



///Returns None if no mouse drag message found, returns Some (lastMouseMsg, msgQueueWithoutMouseMsgs) if a drag message was found
let getLastMouseMsg msgQueue =
    msgQueue
    |> List.filter (matchMouseMsg (fun op -> op = DrawHelpers.Drag))
    |> function
    | [] -> None
    | lst -> Some lst.Head //First item in the list was the last to be added (most recent)

let sheetMsg sMsg model =
    let sModel, sCmd = SheetUpdate.update sMsg model
    let model' = {sModel with SavedSheetIsOutOfDate = currentSheetIsOutOfDate sModel}
    // A placement or paste is committed when the drag-and-drop action settles back to idle:
    // the one funnel through which every way of adding components passes. Custom component
    // instances added this way may have unbound parameters worth offering to bind to the top,
    // and pasted ones must inherit the parameter slots of the components they were copied from.
    // What is in flight is remembered in Model.PendingDragAddition - see DragAddition.
    let model'', placementCmd =
        match model.Sheet.Action, model'.Sheet.Action with
        | SheetT.InitialisedCreateComponent _, SheetT.DragAndDrop ->
            // a catalogue component has just been created and is following the mouse
            model' |> set pendingDragAddition_ (Some (PlacedFromCatalogue model'.Sheet.SelectedComponents)),
            Cmd.none
        | previous, SheetT.DragAndDrop when (match previous with | SheetT.DragAndDrop -> false | _ -> true) ->
            let pending =
                match sMsg with
                | SheetT.KeyPress SheetT.KeyboardMsg.CtrlV ->
                    Some (PastedFromClipboard model'.Sheet.SelectedComponents)
                | _ ->
                    // an existing selection re-entered drag-and-drop (error revert, undo
                    // snapshot): whatever settles from here was not added
                    None
            model' |> set pendingDragAddition_ pending, Cmd.none
        | SheetT.DragAndDrop, SheetT.Idle ->
            // A pasted component keeps the parameterisation of the one it was copied from.
            // pasteSymbols creates its new symbols from copiedSymbolsInPasteOrder and returns
            // their ids in that same order, so the two lists pair up positionally; if that ever
            // stops holding, the length check leaves the copies unparameterised rather than
            // giving them the wrong expressions.
            // A catalogue placement needs nothing here: its parameters were asked for before the
            // component was created.
            let pasteCmd =
                match model'.PendingDragAddition with
                | Some (PastedFromClipboard pastedIds) ->
                    let sourceIds = BlockHelpers.getCopiedSymbols model'.Sheet.Wire.Symbol
                    match List.length sourceIds = List.length pastedIds with
                    | false -> Cmd.none
                    | true ->
                        List.zip sourceIds pastedIds
                        |> List.map (fun (ComponentId src, ComponentId pasted) -> src, pasted)
                        |> ParameterView.copyParamSlotsToPastedComponents
                        |> UpdateModel
                        |> Cmd.ofMsg
                | _ -> Cmd.none
            model' |> set pendingDragAddition_ None, pasteCmd
        | _ -> model', Cmd.none
    // A parameter box keys its error text by slot name alone, not by the component the slot
    // belongs to, so an error left behind by one component is shown against the same slot of the
    // next one selected - two Registers share Buswidth, two instances of a sheet share each of its
    // CustomCompParam slots. Selection belongs to the draw block and changes here, which is the
    // only place that knows it happened; the pane itself cannot clear it without dispatching from
    // a render.
    let model''' =
        match model.Sheet.SelectedComponents = model''.Sheet.SelectedComponents with
        | true -> model''
        | false -> model'' |> set (popupDialogData_ >-> paramCompSpec_) None
    model''', Cmd.batch [sCmd; placementCmd]

let executePendingMessagesF n model =
    if n = (List.length model.Pending)
    then 
        getLastMouseMsg model.Pending
        |> function
        | None -> failwithf "shouldn't happen"
        | Some mMsg -> 
            match mMsg with
            | Sheet sMsg -> sheetMsg sMsg {model with Pending = []}
            | _ -> failwithf "shouldn't happen "
        
    //ignore the exectue message
    else 
        model, Cmd.none


    
