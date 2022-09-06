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
open PopupView
open FileMenuView
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson
open Helpers
open NumberHelpers
open DiagramStyle


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
    | Sheet sheetMsg -> shortDSheetMsg sheetMsg
    | JSDiagramMsg (InitCanvas _ )-> Some "JSDiagramMsg.InitCanvas"
    | JSDiagramMsg _ -> None
    | KeyboardShortcutMsg _ -> None
    | StartSimulation x -> Some $"""StartSimulation({match x with | Ok _ -> "OK" | Error x -> "Error"})"""
    | AddWSModel (s,ws) -> Some $"AddWSModel:{s}->{shortDWSM}"
    | SetWSModel ws -> Some $"SetWSModel:{ws.FastSim.SimulatedTopSheet}->{shortDWSM}"
    | UpdateWSModel _ -> None
    | SetWSModelAndSheet (ws,s)-> Some $"SetWSModelAndSheet:{s}->{shortDWSM}"
    | GenerateWaveforms ws -> Some $"GenerateWaveforms:{shortDWSM ws}"
    | RefreshWaveSim (ws,sd,cs) -> Some "RRefreshWaveSim"
    | SetWaveSheetSelectionOpen _
    | SetWaveComponentSelectionOpen _-> Some "SetWaveComponentSelectionOpen"
    | SetWaveGroupSelectionOpen _
    | LockTabsToWaveSim 
    | UnlockTabsFromWaveSim -> None
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
    | UpdateProject _ -> None
    | UpdateModel _ -> None
    | UpdateProjectWithoutSyncing _ -> None
    | ShowPopup _ -> None
    | ShowStaticInfoPopup _ -> None
    | ClosePopup -> None
    | SetPopupDialogText _ -> None
    | SetPopupDialogCode _ -> None
    | SetPopupDialogVerilogErrors _ -> None
    | SetPopupDialogInt _ -> None
    | SetPopupDialogInt2 _ -> None
    | SetPopupDialogTwoInts _ -> None
    | SetPropertiesExtraDialogText _ -> None
    | SetPopupDialogMemorySetup _  -> None
    | SetPopupMemoryEditorData _ -> None
    | SetPopupProgress _ -> None
    | UpdatePopupProgress _ -> None
    | SetPopupInputConstraints _ -> None
    | SetPopupOutputConstraints _ -> None
    | SetPopupConstraintTypeSel _ -> None
    | SetPopupConstraintIOSel _ -> None
    | SetPopupConstraintErrorMsg _ -> None
    | SetPopupNewConstraint _ -> None
    | SetPopupAlgebraInputs _ -> None
    | SetPopupAlgebraError _ -> None
    | TogglePopupAlgebraInput _ -> Some  "TogglePopupAlgebraInput"
    | SimulateWithProgressBar _ -> None
    | SetSelectedComponentMemoryLocation _ -> None
    | CloseDiagramNotification
    | SetSimulationNotification _ -> None
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification _ -> None
    | CloseFilesNotification
    | SetMemoryEditorNotification _ -> None
    | CloseMemoryEditorNotification
    | SetPropertiesNotification _ -> None
    | ClosePropertiesNotification
    | SetTopMenu _ -> None
    | ReloadSelectedComponent _ -> None
    | SetDragMode _ -> None
    /// Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth _ -> None
    | MenuAction _ -> None
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetIsLoading _
    | SetRouterInteractive _
    | CloseApp
    | SetExitDialog _
    | ExecutePendingMessages _ 
    | DoNothing
    | StartUICmd _
    | FinishUICmd
    | ReadUserData _
    | SetUserData _
    | ExecCmd _
    | ExecFuncInMessage _
    | ExecFuncAsynch _
    | ExecCmdAsynch _
    | SendSeqMsgAsynch _ -> None



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
        if str <> "" then printfn "**Upd:%s" str
        Cmd.map (fun msg -> printfn ">>Cmd:%s" (getMessageTraceString msg)) |> ignore
    model,cmdL
    