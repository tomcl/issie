module UpdateHelpers

open Elmish

open Fulma
open Fable.React
open Fable.React.Props
open ElectronAPI
open FilesIO
open SimulatorTypes
open TruthTableTypes
open TruthTableCreate
open TruthTableReduce
open ModelType
open ModelHelpers
open CommonTypes
open Extractor
open CatalogueView
open PopupHelpers
open FileMenuView
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson
open Helpers
open NumberHelpers
open DiagramStyle
open Fable.Core.JsInterop
open Browser

module Constants =
    let memoryUpdateCheckTime = 300.


///Used to filter specific mouse messages based on mouse data.
let matchMouseMsg (msgSelect: DrawHelpers.MouseT -> bool) (msg : Msg) : bool =
    match msg with
    | Sheet sMsg ->
        match sMsg with
        | SheetT.MouseMsg mMsg ->
            msgSelect mMsg
        | _ -> false
    | _ -> false


let shortDSheetMsg msg = Some "Sheet message"

let shortDWSM (ws: WaveSimModel) =
    Some <| sprintf $"WS<{ws.FastSim.SimulatedTopSheet}->{ws.StartCycle}-{ws.CurrClkCycle}-\
            {ws.ShownCycles} Waves:{ws.AllWaves.Count} ({ws.SelectedWaves.Length})>"

let shortDisplayMsg (msg:Msg) =
    match msg with
    | ShowExitDialog -> None
    | SynchroniseCanvas -> None
    | Sheet sheetMsg -> shortDSheetMsg sheetMsg
    | JSDiagramMsg (InitCanvas _ )-> Some "JSDiagramMsg.InitCanvas"
    | JSDiagramMsg _ -> None
    | KeyboardShortcutMsg _ -> None
    | StartSimulation x -> Some $"""StartSimulation({match x with | Ok _ -> "OK" | Error x -> "Error"})"""
    | AddWSModel (s,ws) -> Some $"AddWSModel:{s}->{shortDWSM ws}"
    | SetWSModel ws -> Some $"SetWSModel:{ws.FastSim.SimulatedTopSheet}->{shortDWSM ws}"
    | UpdateWSModel _ -> None
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
    | MoveColumn _
    | SetIOOrder _ -> Some "SetIOOrder"
    | SetTTAlgebraInputs _ -> None
    | SetTTBase _ -> None
    | SetTTGridCache _ -> Some "SetTTGridCache"
    | ChangeRightTab _ -> None
    | ChangeSimSubTab _ -> None
    | SetHighlighted (comps,conns) -> Some $"SetHighlighted: {comps.Length} comps, {conns.Length} conns"
    | SetSelWavesHighlighted x -> Some $"SetSelWavesHighlighted{x.Length}"
    | SetClipboard _ -> Some "SetClipboard"
    | SetCreateComponent _ -> Some "SetCreateComponent"
    | SetProject _ -> Some "SetProject"
    | UpdateProject _ 
    | UpdateModel _ 
    | UpdateProjectWithoutSyncing _ 
    | ShowPopup _ 
    | ShowStaticInfoPopup _ 
    | ClosePopup 
    | SetPopupDialogBadLabel _ 
    | SetPopupDialogText _ 
    | SetPopupDialogCode _ 
    | SetPopupDialogVerilogErrors _ 
    | SetPopupDialogInt _ 
    | SetPopupDialogInt2 _ 
    | SetPopupDialogTwoInts _ 
    | SetPropertiesExtraDialogText _ 
    | SetPopupDialogBadLabel _ 
    | SetPopupDialogMemorySetup _  
    | SetPopupMemoryEditorData _ 
    | SetPopupProgress _ 
    | UpdatePopupProgress _ 
    | SetPopupInputConstraints _ 
    | SetPopupOutputConstraints _ 
    | SetPopupConstraintTypeSel _ 
    | SetPopupConstraintIOSel _ 
    | SetPopupConstraintErrorMsg _ 
    | SetPopupNewConstraint _ 
    | SetPopupAlgebraInputs _ 
    | SetPopupAlgebraError _ -> None
    | TogglePopupAlgebraInput _ -> Some  "TogglePopupAlgebraInput"
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
    // Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth _ 
    | MenuAction _ 
    | DiagramMouseEvent
    | ContextMenuAction _ -> None
    | ContextMenuItemClick _
    | SelectionHasChanged -> Some "Selection has changed"
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




/// If debugTrace is on print out human readable info on message.
/// Be careful not to do this on mouse moves (there are too many).
/// be careful not to try to ptint simulation result arrays (that would crash the renderer!).
/// optimise for very quick return in the case that debugLevel = 0 (production version)
/// optimise for quick return if nothing is printed.
let getMessageTraceString (msg: Msg) =
    let noDisplayMouseOp (mMsg:DrawHelpers.MouseT) = 
        mMsg.Op = DrawHelpers.Drag || mMsg.Op = DrawHelpers.Move
    let noDisplayMessage = function
        | Sheet (SheetT.Msg.Wire(BusWireT.Msg.Symbol(SymbolT.MouseMsg _ | SymbolT.ShowPorts _ ))) -> true
        | _ -> false

    if JSHelpers.debugLevel = 0 ||
       not (Set.contains "update" JSHelpers.debugTraceUI) ||
       matchMouseMsg noDisplayMouseOp msg ||
       noDisplayMessage msg then
        ""
    else 
        match shortDisplayMsg msg with
        | Some shortName -> shortName
        | None ->
            Helpers.sprintInitial 70 $"{msg}"


let traceMessage startOfUpdateTime (msg:Msg) ((model,cmdL): Model*Cmd<Msg>) =
    if JSHelpers.debugLevel > 0 then
        let str = getMessageTraceString msg
        let rootOfMsg = 
            match str.Split [|' ';'('|] with
            | ss when ss.Length > 0 -> ss.[0]
            | _ -> ""
        TimeHelpers.instrumentInterval rootOfMsg startOfUpdateTime |> ignore
        let updateTime = TimeHelpers.getTimeMs() - startOfUpdateTime
        //if str <> "" then printfn "%s" $"**Upd:{str} %.1f{updateTime}ms ({int startOfUpdateTime % 10000}ms)"
        Cmd.map (fun msg -> printfn ">>Cmd:%s" (getMessageTraceString msg)) |> ignore
    model,cmdL

let mutable lastMemoryUpdateCheck = 0.

let updateAllMemoryCompsIfNeeded (model:Model) =
    let time = TimeHelpers.getTimeMs()
    if time - lastMemoryUpdateCheck > Constants.memoryUpdateCheckTime && (WaveSimHelpers.getWSModel model).State = Success then
        printfn "checking update of memories"
        lastMemoryUpdateCheck <- time
        MemoryEditorView.updateAllMemoryComps model
    else
        model

//-------------------------------------------------------------------------------------------------//
//-------------------------------------CONTEXT MENUS-----------------------------------------------//
//-------------------------------------------------------------------------------------------------//

/// Function that works out from the right-click event and model
/// what the current context menu should be.
let getContextMenu (e: Browser.Types.MouseEvent) (model: Model) =
    //--------- the sample code below shows how useful info can be extracted from e --------------//
    // calculate equivalent sheet XY coordinates - valid if mouse is over schematic.
    let sheetXYPos = SheetDisplay.getDrawBlockPos e DiagramStyle.getHeaderHeight model.Sheet
    let element:Types.Element = unbox e.target
    let htmlId = try element.id with | e -> "invalid"
    let drawOn = Sheet.mouseOn model.Sheet sheetXYPos
    let clickType =
        match drawOn, htmlId with
        | SheetT.MouseOn.Canvas, "DrawBlockSVGTop" ->
            printfn "Draw block sheet canvas"
            "canvas"
        | SheetT.MouseOn.Canvas, x ->
            printfn "Other issie element"
            element.ToString()
        | drawOn, _ ->
            printfn "Draw block element: %A" drawOn
            drawOn.ToString()
    printfn "--------"
    "Menu1" // send a string so contextMenu.fs code can bring up the correct menu

/// Function that implement action based on context menu item click.
/// menuType is the menu from chooseContextMenu.
/// item will be one of the possible items in this menu.
let processContextMenuClick menuType item dispatch model =
    match menuType,item with
    | _ ->
        printfn "%s" $"Context menu item not implemented: {menuType} -> {item}"
    model, Cmd.none




    
    
