﻿module UpdateHelpers

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
open FileMenuView
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson
open Helpers
open NumberHelpers
open DiagramStyle
open Fable.Core.JsInterop
open Browser
open PopupHelpers
open Optics.Operators
open Optics.Optic

module Constants =
    let memoryUpdateCheckTime = 300.

//-------------------------------------------------------------------------------------------------//
//-------------------------------------MESSAGE TRACING---------------------------------------------//
//-------------------------------------------------------------------------------------------------//


///Used to filter specific mouse messages based on mouse data.
let matchMouseMsg (msgSelect: DrawHelpers.MouseT -> bool) (msg : Msg) : bool =
    match msg with
    | Sheet sMsg ->
        match sMsg with
        | SheetT.MouseMsg mMsg ->
            msgSelect mMsg
        | _ -> false
    | _ -> false

/// short summary used where Sheet messages are too complex to print
let shortDSheetMsg msg = Some "Sheet message"

/// short summary of wavesim message which has a lot of data
let shortDWSM (ws: WaveSimModel) =
    Some <| sprintf $"WS<{ws.FastSim.SimulatedTopSheet}->{ws.StartCycle}-{ws.CurrClkCycle}-\
            {ws.ShownCycles} Waves:{ws.AllWaves.Count} ({ws.SelectedWaves.Length})>"

/// Function returning a short but usually informative display of message
/// used when message tracing (see Sheet menu to which on or off).
/// Parameters that might be very large (like fastsimulation, or Model, or Symbols) should not be
/// displayed using printf "%A".
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
        | SetPopupAlgebraError _ -> None

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

(*

        Implement right-click context menus throughout Issie:

        getContextMenu - detemines menu items for a given context

        processContextMenuClick - determines action (typically a single message) for each menu item.

        Common/ContextMenus.contextMenus - names and item names for each menu.

*)

/// Function that works out from the right-click event and model
/// what the current context menu should be.
/// output should be a menu name as defined in ContextMenus.contextMenus, or "" for no menu.
let getContextMenu (e: Browser.Types.MouseEvent) (model: Model) : string =
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
let processContextMenuClick
        (menuType: string) // name of menu
        (item: string) // name of menu item clicked
        (dispatch: Msg -> unit) // disapatch function
        (model: Model)
            : Model * Cmd<Msg> = // can change state directly (Model) or via a message wrapped in Cmd.ofMsg.
    match menuType,item with
    | _ ->
        printfn "%s" $"Context menu item not implemented: {menuType} -> {item}"
    model, Cmd.none



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
let withNoCmd (model: Model) : Model * Cmd<Msg> =
    model, Cmd.none


/// Read persistent user data from file in userAppDir.
/// Store in Model UserData.
let readUserData (userAppDir: string) (model: Model) : Model * Cmd<Msg> =
    let addAppDirToUserData model = 
        {model with UserData = {model.UserData with UserAppDir = Some userAppDir}}

    let modelOpt =
        try
            let jsonRes = tryReadFileSync <| pathJoin [|userAppDir;"IssieSettings.json"|]
            jsonRes
            |> Result.bind (fun json -> Json.tryParseAs<UserData> json)
            |> Result.bind (fun (data: UserData) -> Ok {model with UserData = data})
            |> (function | Ok model -> model | Error _ -> printfn "Error reading user data" ; model)
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
        |> function | Error mess -> printfn "%s" mess | _ -> ())
    |> ignore
/// subfunction used in model update function
let getSimulationDataOrFail model msg =
    match model.CurrentStepSimulationStep with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

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

/// handle Menu actions that may need Model data
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuSaveFile -> 
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuSaveProjectInNewFormat ->
        FileMenuView.saveOpenProjectInNewFormat model |> ignore
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuExit ->
        FileMenuView.doActionWithSaveFileDialog "Exit ISSIE" CloseApp model dispatch ()
    | MenuVerilogOutput ->
        mapOverProject () model (fun p ->
            let sheet = p.OpenFileName
            let fPath = FilesIO.pathJoin [|p.ProjectPath ; sheet|]
            PopupHelpers.choicePopup
                "Verilog Output"
                (verilogOutputPage sheet fPath)
                "Write Synthesis Verilog"
                "Write Simulation Verilog"
                (fun forSim _ -> 
                    match forSim with
                    | true -> SimulationView.verilogOutput Verilog.ForSynthesis model dispatch
                    | false -> SimulationView.verilogOutput Verilog.ForSimulation model dispatch
                    dispatch ClosePopup)
                dispatch)
            
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

let findChange (model : Model) : bool = 
    let last = model.LastChangeCheckTime // NB no check to reduce total findChange time implemented yet - TODO if needed
    let start = TimeHelpers.getTimeMs()

    match model.CurrentProj with
    | None -> false
    | Some prj ->
        //For better efficiency just check if the save button
        let savedComponent = 
            prj.LoadedComponents
            |> List.find (fun lc -> lc.Name = prj.OpenFileName)
        let canv = savedComponent.CanvasState
        let canv' = model.Sheet.GetCanvasState ()
        (canv <> canv') && not (compareCanvas 100. canv canv')
        |> TimeHelpers.instrumentInterval "findChange" start

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

let updateComponentMemory (addr:int64) (data:int64) (compOpt: Component option) =
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
    writeUserData model
    renderer.ipcRenderer.send("exit-the-app",[||])

///Tests physical equality on two objects
///Used because Msg type does not support structural equality
let isSameMsg = LanguagePrimitives.PhysicalEquality 



///Returns None if no mouse drag message found, returns Some (lastMouseMsg, msgQueueWithoutMouseMsgs) if a drag message was found
let getLastMouseMsg msgQueue =
    msgQueue
    |> List.filter (matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag))
    |> function
    | [] -> None
    | lst -> Some lst.Head //First item in the list was the last to be added (most recent)

let sheetMsg sMsg model = 
    let sModel, sCmd = SheetUpdate.update sMsg model
    {sModel with SavedSheetIsOutOfDate = findChange sModel}, sCmd

let executePendingMessagesF n model =
    if n = (List.length model.Pending)
    then 
        getLastMouseMsg model.Pending
        |> function
        | None -> failwithf "shouldn't happen"
        | Some mMsg -> 
            match mMsg with
            | Sheet sMsg -> sheetMsg sMsg model
            | _ -> failwithf "shouldn't happen "
        
    //ignore the exectue message
    else 
        model, Cmd.none

    
    
