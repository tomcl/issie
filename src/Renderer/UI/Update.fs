﻿module Update

open Elmish

open Fulma
open Fable.React
open Fable.React.Props
open ModelType
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
open UpdateHelpers
open Optics
open Optic

open Operators

//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//


let mutable uiStartTime: float = 0.



/// Read persistent user data from file in userAppDir.
/// Store in Model UserData.
let private readUserData (userAppDir: string) (model: Model) : Model * Cmd<Msg> =
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

let private writeUserData (model:Model) =
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
let private getSimulationDataOrFail model msg =
    match model.CurrentStepSimulationStep with
    | None -> failwithf "what? Getting simulation data when no simulation is running: %s" msg
    | Some sim ->
        match sim with
        | Error _ -> failwithf "what? Getting simulation data when could not start because of error: %s" msg
        | Ok simData -> simData

let private getTruthTableOrFail model msg =
    match model.CurrentTruthTable with
    | None -> failwithf "what? Getting truth table when no table has been generated: %s" msg
    | Some res ->
        match res with
        | Error _ -> failwithf "what? Getting truth table when there is error in generation: %s" msg
        | Ok tt -> tt

let truncationWarning table =
    $"The Truth Table has been truncated to {table.TableMap.Count} input combinations.
    Not all rows may be shown."

/// Apply a single numerical output constraint to a Truth Table Map
let applyNumericalOutputConstraint (table: Map<TruthTableRow,TruthTableRow>) (con: Constraint) =
    table
    |> Map.filter (fun _ right ->
        right
        |> List.exists (fun cell ->
            match con with
            | Equality e ->
                if e.IO <> cell.IO then
                    false
                else
                    match cell.Data with
                    | Algebra _ -> false
                    | DC -> true
                    | Bits wd ->
                        let cellVal = convertWireDataToInt wd
                        cellVal = e.Value
            | Inequality i ->
                if i.IO <> cell.IO then
                    false
                else
                    match cell.Data with
                    | Algebra _ -> false
                    | DC -> true
                    | Bits wd ->
                        let cellVal = convertWireDataToInt wd
                        i.LowerBound <= int cellVal && cellVal <= i.UpperBound
                        ))

/// Comparison function for CellData values
let compareCellData (cd1: CellData) (cd2: CellData) =
    match cd1, cd2 with
    | DC, DC -> 0 // X = X
    | DC, _ -> 1 // DC is considered larger than anything
    | _, DC -> -1
    | Algebra _, Bits _ -> 1 // Algebra is considered larger than Bits
    | Bits _, Algebra _ -> -1
    | Algebra a1, Algebra a2 -> 
        compare a1 a2 // Algebra compared by alphabetical order
    | Bits wd1, Bits wd2 -> // Bits compared by which is largest
        (convertWireDataToInt wd1, convertWireDataToInt wd2)
        ||> compare

let sortByIO (io: CellIO) (lst: TruthTableRow list) = 
    let idx = 
        lst.Head
        |> List.tryFindIndex (fun c ->
            c.IO = io)
        |> function
            | Some i -> i
            | None -> failwithf "what? Failed to find IO: %s while sorting TT" io.getLabel
    
    lst
    |> List.sortWith (fun r1 r2 ->
        let cd1 = r1[idx].Data
        let cd2 = r2[idx].Data
        compareCellData cd1 cd2)

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
let resetDialogIfSelectionHasChanged newModel oldModel =
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

//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update (msg : Msg) oldModel =
    let startOfUpdateTime = TimeHelpers.getTimeMs()
    

    //Add the message to the pending queue if it is a mouse drag message
    let model =
        if matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag) msg then
            {oldModel with Pending = msg :: oldModel.Pending}
        else
            oldModel
    
    //Check if the current message is stored as pending, if so execute all pending messages currently in the queue
    let testMsg, cmd =
        List.tryFind (fun x -> isSameMsg x msg) model.Pending
        |> function
        | Some _ ->
            //Add any message recieved to the pending message queue
            DoNothing, Cmd.ofMsg (ExecutePendingMessages (List.length model.Pending))
        | None ->
            msg, Cmd.none
    // main message dispatch match expression
    let model = updateAllMemoryCompsIfNeeded model
    match testMsg with
    | StartUICmd uiCmd ->
        //printfn $"starting UI command '{uiCmd}"
        uiStartTime <- TimeHelpers.getTimeMs()
        match model.UIState with
        | None -> //if nothing is currently being processed, allow the ui command operation to take place
            match uiCmd with
            | CloseProject ->
                {model with CurrentProj = None; UIState = Some uiCmd}, Cmd.none
            | _ -> 
                {model with UIState = Some uiCmd}, Cmd.ofMsg (Sheet (SheetT.SetSpinner true))
        | _ -> model, Cmd.none //otherwise discard the message
    | FinishUICmd _->
        //printfn $"ending UI command '{model.UIState}"
        printf $"***UI Command: %.2f{TimeHelpers.getTimeMs() - uiStartTime} ***"
        let popup = CustomCompPorts.optCurrentSheetDependentsPopup model
        {model with UIState = None; PopupViewFunc = popup}, Cmd.ofMsg (Sheet (SheetT.SetSpinner false))

    (*| ShowExitDialog ->
        match model.CurrentProj with
        | Some p when model.SavedSheetIsOutOfDate ->
            {model with ExitDialog = true}, Cmd.none
        | _ -> // exit immediately since nothing to save
            exitApp()
            model, Cmd.none*)
    | CloseApp ->
        exitApp model
        model, Cmd.none
    (*| SetExitDialog status ->
        {model with ExitDialog = status}, Cmd.none*)
    | Sheet sMsg ->
        match sMsg, model.PopupViewFunc with
        | SheetT.ToggleNet canvas, _ ->
            model, Cmd.none
        | SheetT.KeyPress _, Some _ -> 
            // do not allow keys to affect Sheet when popup is on.
            model, Cmd.none
        | _ -> sheetMsg sMsg model
    | SynchroniseCanvas ->
        // used after drawblock components are centred on load to enusre that Issie CanvasState is updated
        // This may be needed if Ctrl/w on load moves the whole draw block sheet circuit to centre it.
        // in this case we do not want the save button to be active, because moving the circuit is not a "real" change
        // updating loaded component CanvasState to equal draw bloack canvasstate will ensure the button stays inactive.
        let canvas = model.Sheet.GetCanvasState ()
        printf "synchronising canvas..."
        // this should disable the saev button by making loadedcomponent and draw blokc canvas the same
        model
        |> Optic.map openLoadedComponentOfModel_ (fun ldc -> {ldc with CanvasState = canvas})
        |> (fun model -> 
            printf $"findChange model after sycnh = '{findChange model}'"       
            model, Cmd.none)
        
    // special messages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode -> {model with DividerDragMode= mode}, Cmd.none
    | SetViewerWidth w ->
        {model with WaveSimViewerWidth = w}, Cmd.none
    | ReloadSelectedComponent width ->
        {model with LastUsedDialogWidth=width}, Cmd.none
    | Benchmark ->
        let step = 2000
        let warmup = 5
        let simulationRound = 10
        let benchmarkRound = 20

        let geometricMean (values: float list) = (values |> List.reduce (*)) ** (1.0 / (float values.Length))

        let benchmark i =
            match model.CurrentProj with
            | Some p ->
                printfn "Benchmarking on %20s, stepArraySize %8d, step %8d, warmup %3d, repeat %3d" (dirName p.ProjectPath) SimulationView.Constants.maxArraySize step warmup simulationRound

                p.LoadedComponents
                |> List.map (fun c ->
                    let simData = Simulator.startCircuitSimulation SimulationView.Constants.maxArraySize c.Name c.CanvasState p.LoadedComponents

                    match simData with
                    | Error err -> failwithf "Error occured when running startCircuitSimulation on %A, %A" c.Name err
                    | Ok simData ->
                        let comps = simData.FastSim.FComps.Values |> Seq.filter (fun fc -> match fc.FType with | IOLabel -> false | _ -> true) |> Seq.length
                        printfn "Benchmarking with component: %s" c.Name

                        [ 1 .. (warmup + simulationRound) ]
                        |> List.map (fun _ ->
                            simData.FastSim.ClockTick <- 0
                            let start = TimeHelpers.getTimeMs ()
                            // for _ in 0..(step-1) do FastRun.stepSimulation simData.FastSim
                            FastRun.runFastSimulation None step simData.FastSim |> ignore
                            TimeHelpers.getTimeMs () - start)
                        |> List.skip warmup
                        |> List.average
                        |> (fun time ->
                            let speed = float (comps * step) / time
                            printfn "simulated %20s for %5d steps with %4d effective components, simulation finished in %8.3fms, average simulation speed: %10.3f (comp * step / ms)" c.Name step comps time speed
                            speed))
                |> geometricMean

            | None -> failwith "No project loaded, please load a project to benchmark"

        [ 1..benchmarkRound ]
        |> List.map (fun i -> benchmark i)
        |> printfn "Geometric mean of simulation speed of ISSIE on current project: %A"

        model, Cmd.none
    | StartSimulation simData -> 
        {model with CurrentStepSimulationStep = Some simData }, Cmd.none
    | SetWSModel wsModel ->
        setWSModel wsModel model, Cmd.none
    | UpdateWSModel updateFn ->
        updateWSModel updateFn model, Cmd.none
    | SetWSModelAndSheet (wsModel, wsSheet) ->
        let newModel =
            {model with WaveSimSheet = if wsSheet = "" then None else Some wsSheet}
            |> setWSModel wsModel
        newModel, Cmd.none
    | UpdateModel( updateFn: Model -> Model) ->
        updateFn model, Cmd.none
    | RefreshWaveSim ws ->
        // restart the wave simulator after design change etc that invalidates all waves
        WaveSim.refreshWaveSim true ws model
    | AddWSModel (sheet, wsModel) ->
        { model with 
            WaveSim = Map.add sheet wsModel model.WaveSim
        }, Cmd.none
    | GenerateWaveforms ws ->
        // Update the wave simulator with new waveforms
        // Is called whenever any waveform might need to be changed
        WaveSim.refreshWaveSim false ws model
    | GenerateCurrentWaveforms ->
        // Update the wave simulator with new waveforms based on current WsModel
        let ws = WaveSimHelpers.getWSModel model
        WaveSim.refreshWaveSim false ws model
    | SetWaveComponentSelectionOpen (fIdL, show) ->       
        let model = 
            model
            |> updateWSModel (fun ws -> WaveSimHelpers.setWaveComponentSelectionOpen ws fIdL show)
        model, cmd
    | SetWaveGroupSelectionOpen (fIdL, show) -> 
        let model = 
            model
            |> updateWSModel (fun ws -> WaveSimHelpers.setWaveGroupSelectionOpen ws fIdL show)
        model, cmd

        
    | SetWaveSheetSelectionOpen (fIdL, show) ->       
        let model = 
            model
            |> updateWSModel (fun ws -> WaveSimHelpers.setWaveSheetSelectionOpen ws fIdL show)
        model, cmd
    

    | TryStartSimulationAfterErrorFix simType ->
        let conns = BusWire.extractConnections model.Sheet.Wire
        let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
        let canvasState = comps,conns
        let simErrFeedback simErr otherMsg =
            Cmd.batch
                ((SimulationView.getSimErrFeedbackMessages simErr model) @ [otherMsg]
                |> List.map Cmd.ofMsg)
        match simType with
            | StepSim ->
                SimulationView.tryGetSimData canvasState model
                |> function
                    | Ok (simData) -> 
                        { model with CurrentStepSimulationStep = simData |> Ok |> Some }, Cmd.ofMsg (StartSimulation (Ok simData))
                    | Error simError ->
                        { model with CurrentStepSimulationStep = simError |> Error |> Some }, simErrFeedback simError (StartSimulation (Error simError))
            | TruthTable ->
                SimulationView.simulateModel None 2 canvasState model
                |> function
                    | Ok (simData), state ->
                        if simData.IsSynchronous = false then
                            { model with CurrentStepSimulationStep = simData |> Ok |> Some }, Cmd.ofMsg (GenerateTruthTable (Some (Ok simData, state)))
                        else
                            { model with CurrentStepSimulationStep = None }, Cmd.ofMsg CloseTruthTable
                    | Error simError, state ->
                        { model with CurrentStepSimulationStep = simError |> Error |> Some }, simErrFeedback simError (GenerateTruthTable (Some (Error simError, state)))
            | WaveSim ->
                let model = MemoryEditorView.updateAllMemoryComps model
                let wsSheet = 
                    match model.WaveSimSheet with
                    | None -> Option.get (getCurrFile model)
                    | Some sheet -> sheet
                let model = 
                    model
                    |> removeAllSimulationsFromModel
                    |> fun model -> {model with WaveSimSheet = Some wsSheet}
                let wsModel = WaveSimHelpers.getWSModel model
                //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
                match SimulationView.simulateModel model.WaveSimSheet (WaveSimHelpers.Constants.maxLastClk + WaveSimHelpers.Constants.maxStepsOverflow)  canvasState model with
                //| None ->
                //    dispatch <| SetWSModel { wsModel with State = NoProject; FastSim = FastCreate.emptyFastSimulation "" }
                | (Error simError, _) ->
                    { model with CurrentStepSimulationStep = simError |> Error |> Some }, simErrFeedback simError (SetWSModelAndSheet ({ wsModel with State = SimError simError }, wsSheet))
                | (Ok simData, canvState) ->
                    if simData.IsSynchronous then
                        SimulationView.setFastSimInputsToDefault simData.FastSim
                        let wsModel = { wsModel with State = Loading ; FastSim = simData.FastSim }
                        { model with CurrentStepSimulationStep = simData |> Ok |> Some }, Cmd.batch [Cmd.ofMsg (SetWSModelAndSheet (wsModel, wsSheet));
                                                                                                            Cmd.ofMsg (RefreshWaveSim wsModel)]
                    else
                        { model with CurrentStepSimulationStep = simData |> Ok |> Some }, Cmd.ofMsg (SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet))
    | SetSimulationGraph (graph, fastSim) ->
        let simData = getSimulationDataOrFail model "SetSimulationGraph"
        { model with CurrentStepSimulationStep = { simData with Graph = graph ; FastSim = fastSim} |> Ok |> Some }, Cmd.none
    | SetSimulationBase numBase ->
        let simData = getSimulationDataOrFail model "SetSimulationBase"
        { model with CurrentStepSimulationStep = { simData with NumberBase = numBase } |> Ok |> Some }, Cmd.none
    | IncrementSimulationClockTick n ->
        let simData = getSimulationDataOrFail model "IncrementSimulationClockTick"
        { model with CurrentStepSimulationStep = { simData with ClockTickNumber = simData.ClockTickNumber + n } |> Ok |> Some }, Cmd.none
    | EndSimulation -> { model with CurrentStepSimulationStep = None }, Cmd.none
    | EndWaveSim -> 
        let model =
            let model = removeAllSimulationsFromModel model
            match model.WaveSimSheet with
            | None | Some "" -> 
                printfn "What? can't end WaveSim when it is already ended"
                model
            | Some sheet -> 
                { model with 
                    WaveSimSheet = None; 
                    WaveSim = Map.change sheet (Option.map (fun ws -> 
                        {ws with State = Ended ; WaveModalActive = false})) model.WaveSim
                }
        model, Cmd.none
    | GenerateTruthTable simRes ->
        match simRes with
        | Some (Ok sd,_) ->
            // delete any old simulations
            let model = ModelHelpers.removeAllSimulationsFromModel model
            // Generate the Truth Table
            let tt = 
                truthTable 
                    sd 
                    model.TTConfig
                    false
            // Styles for the grid
            let colStyles = 
                tt.IOOrder
                |> List.mapi (fun i io -> (io,ttGridColumnProps i))
                |> Map.ofList 
            // List of messages
            let commands = 
                [
                    // Set the IO Order for the Truth Table
                    tt.IOOrder 
                    |> List.toArray 
                    |> SetIOOrder
                    // Set the popup Algebra inputs to empty list
                    SetPopupAlgebraInputs (Some [])
                    // Truncation warning
                    if tt.IsTruncated then
                        Notifications.warningPropsNotification (truncationWarning tt)
                        |> SetPropertiesNotification
                ]
                |> List.map Cmd.ofMsg
            model
            |> Optic.set currentTruthTable_ (Some (Ok tt))
            |> Optic.set (tTType_ >-> gridStyles_) colStyles,
                Cmd.batch <| commands
        | Some (Error e, _) ->
            model
            |> Optic.set currentTruthTable_ (Some (Error e))
            |> Optic.set (tTType_ >-> gridStyles_) Map.empty,
                Cmd.none
        | None -> model, Cmd.none
    | RegenerateTruthTable ->
        let table = getTruthTableOrFail model "Regeneration"
        let ttRes, commands =
            try
                let tt =
                    truthTable
                        table.TableSimData
                        model.TTConfig
                        true
                let comms = 
                    [
                        // If table is truncated, issue truncation warning. 
                        if tt.IsTruncated then
                            Notifications.warningPropsNotification (truncationWarning tt)
                            |> SetPropertiesNotification
                        // Else, clear any prior truncation warning.
                        else
                            ClosePropertiesNotification
                        // Filter using output constraints
                        FilterTruthTable
                    ]
                    |> List.map Cmd.ofMsg
                Ok tt, comms
            with
            // Protections when setting algebraic inputs should mean this never occurs,
            // but leaving this in as a fallback.
            | AlgebraNotImplemented err -> Error err, []
        {model with CurrentTruthTable = Some ttRes}, Cmd.batch <| commands
    | FilterTruthTable ->
        let table = getTruthTableOrFail model "Refilter"
        let tMap = 
            match table.DCMap with
            | Some m -> m
            | None -> table.TableMap
        let allOutputConstraints =
            (model.TTConfig.OutputConstraints.Equalities
            |> List.map Equality)
            @
            (model.TTConfig.OutputConstraints.Inequalities
            |> List.map Inequality)
        let filteredMap =
            (tMap, allOutputConstraints)
            ||> List.fold applyNumericalOutputConstraint
        let newTable = {table with FilteredMap = filteredMap} |> Ok |> Some
        {model with CurrentTruthTable = newTable}, Cmd.ofMsg SortTruthTable
    | SortTruthTable ->
        let start = TimeHelpers.getTimeMs()
        let table = getTruthTableOrFail model "Sorting"
        let sortedTable =
            match model.TTConfig.SortType, tableAsList table.FilteredMap with 
            | _, [] -> 
                {table with SortedListRep = []}
            | None, lst ->
                {table with SortedListRep = lst}
            | Some (io, Ascending), lst ->
                let sortedLst = sortByIO io lst
                {table with SortedListRep = sortedLst}
            | Some (io, Descending), lst ->
                let sortedLst = 
                    sortByIO io lst
                    |> List.rev
                {table with SortedListRep = sortedLst}
            |> TimeHelpers.instrumentInterval "Sorting Truth Table" start
            |> Ok
            |> Some
        {model with CurrentTruthTable = sortedTable}, Cmd.ofMsg HideTTColumns
    | DCReduceTruthTable ->
        let table = getTruthTableOrFail model "DC Reduction"
        let start = TimeHelpers.getTimeMs() 
        let reducedTable = 
            reduceTruthTable table None
            |> TimeHelpers.instrumentInterval "DC Reduction" start
            |> Ok
            |> Some
        {model with CurrentTruthTable = reducedTable}, Cmd.ofMsg FilterTruthTable
    | HideTTColumns ->
        let start = TimeHelpers.getTimeMs()
        /// Recursive function to hide columns and adjust the positions of the remaining
        /// visible columns.
        let rec correctProps (index: int) (acc: list<CellIO*list<CSSProp>>) (lst: CellIO list):  list<CellIO*list<CSSProp>>=
            let hiddenProps = ttGridHiddenColumnProps model.TTConfig.IOOrder.Length
            match lst with
            | [] -> acc
            | io::tl ->
                if List.contains io model.TTConfig.HiddenColumns then
                    correctProps (index) ((io,hiddenProps)::acc) tl
                else
                    correctProps (index+1) ((io,ttGridColumnProps index)::acc) tl
        let newStyles =
            correctProps 0 [] (Array.toList model.TTConfig.IOOrder)
            |> Map.ofList
            |> TimeHelpers.instrumentInterval "Hiding Columns" start
        model
        |> Optic.set (tTType_ >-> gridStyles_)  newStyles, Cmd.ofMsg (SetTTGridCache None)
    | CloseTruthTable -> 
        let newPopupData =
            {model.PopupDialogData with 
                AlgebraInputs = None
                AlgebraError = None
                ConstraintErrorMsg = None}    
            
        { model with
            TTConfig = DiagramMainView.tTTypeInit
            CurrentTruthTable = None
            PopupDialogData = newPopupData}, Cmd.none
    | ClearInputConstraints ->
        model
        |> Optic.set (tTType_ >-> inputConstraints_) emptyConstraintSet, Cmd.ofMsg RegenerateTruthTable
    | ClearOutputConstraints ->
        model
        |> Optic.set (tTType_ >-> outputConstraints_) emptyConstraintSet, Cmd.ofMsg RegenerateTruthTable
    | AddInputConstraint con ->
        match con with
        | Equality e -> 
            model
            |> Optic.map (tTType_ >-> inputConstraints_ >-> equalities_) (fun eqs -> e :: eqs), Cmd.ofMsg RegenerateTruthTable
        | Inequality i ->
            model
            |> Optic.map (tTType_ >-> inputConstraints_ >-> inequalities_) (fun inEqs -> i :: inEqs), Cmd.ofMsg RegenerateTruthTable
    | DeleteInputConstraint con ->
        match con with
        | Equality e ->
            model
            |> Optic.map (tTType_ >-> inputConstraints_ >-> equalities_) (List.except [e]), Cmd.ofMsg RegenerateTruthTable
        | Inequality i ->
            model
            |> Optic.map (tTType_ >-> inputConstraints_ >-> inequalities_) (List.except [i]), Cmd.ofMsg RegenerateTruthTable
    | AddOutputConstraint con ->
        match con with
        | Equality e ->
            model
            |> Optic.map (tTType_ >-> outputConstraints_ >-> equalities_) (fun eqs -> e :: eqs), Cmd.ofMsg FilterTruthTable
        | Inequality i ->
            model
            |> Optic.map (tTType_ >-> outputConstraints_ >-> inequalities_) (fun inEqs -> i :: inEqs), Cmd.ofMsg FilterTruthTable
    | DeleteOutputConstraint con ->
        match con with
        | Equality e ->
            model
            |> Optic.map (tTType_ >-> outputConstraints_ >-> equalities_) (List.except [e]), Cmd.ofMsg FilterTruthTable
        | Inequality i ->
            model
            |> Optic.map (tTType_ >-> outputConstraints_ >-> inequalities_) (List.except [i]), Cmd.ofMsg FilterTruthTable

    | ToggleHideTTColumn io ->
        // Column is currently hidden, so we unhide
        if List.contains io model.TTConfig.HiddenColumns then
            model
            |> Optic.map (tTType_ >-> hiddenColumns_) (List.except [io]), Cmd.ofMsg HideTTColumns
        else
            let newSort =
                match model.TTConfig.SortType with
                | None -> None
                | Some (cIO,st) ->
                    if cIO = io then None
                    else Some (cIO,st)
            model
            |> Optic.map (tTType_ >-> hiddenColumns_) (fun cols -> io :: cols)
            |> Optic.set (tTType_ >-> sortType_) newSort, Cmd.ofMsg HideTTColumns
    | ClearHiddenTTColumns -> 
        model
        |> Optic.set (tTType_ >-> hiddenColumns_) [], Cmd.ofMsg HideTTColumns
    | ClearDCMap ->
        let newTT = 
            match model.CurrentTruthTable with
            | None -> None
            | Some tableopt ->
                match tableopt with
                | Error _ -> failwithf "what? Trying to clear DC Map in TT with error"
                | Ok table ->
                    {table with DCMap = None}
                    |> Ok
                    |> Some
        {model with CurrentTruthTable = newTT}, Cmd.ofMsg FilterTruthTable
    | SetTTSortType stOpt ->
        model
        |> Optic.set (tTType_ >-> sortType_) stOpt, Cmd.ofMsg SortTruthTable
    | MoveColumn (io, dir) ->
        let oldOrder = model.TTConfig.IOOrder
        let idx = 
            oldOrder
            |> Array.tryFindIndex (fun cIO -> cIO = io)
            |> function
                | Some i -> i
                | None -> failwithf "what? IO: %A not found in TTIOOrder" io
        let newOrder =
            match dir, idx with
            | MLeft, 0 -> oldOrder
            | MLeft, i -> swapArrayEls (i) (i-1) oldOrder
            | MRight, i -> 
                if i = (oldOrder.Length-1) then
                    oldOrder
                else
                    swapArrayEls (idx) (idx+1) oldOrder
        let newStyles =
            newOrder
            |> Array.mapi (fun i io -> (io,ttGridColumnProps i))
            |> Map.ofArray
        model
        |> Optic.set (tTType_ >-> ioOrder_) newOrder
        |> Optic.set (tTType_ >-> gridStyles_) newStyles, Cmd.ofMsg (SetTTGridCache None)
    | SetIOOrder x -> 
        model
        |> Optic.set (tTType_ >-> ioOrder_) x, Cmd.none
    | SetTTAlgebraInputs lst ->
        model
        |> Optic.set (tTType_ >-> algebraIns_) lst, Cmd.ofMsg RegenerateTruthTable
    | SetTTBase numBase ->
        let table = getTruthTableOrFail model "SetTTBase"
        let updatedTT = 
            {table with TableSimData = {table.TableSimData with NumberBase = numBase}}
            |> Ok
            |> Some
        {model with CurrentTruthTable = updatedTT}, Cmd.ofMsg (SetTTGridCache None)
    | SetTTGridCache gridopt ->
        model
        |> Optic.set (tTType_ >-> gridCache_) gridopt, Cmd.none
    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        firstTip <- true
        { model with RightPaneTabVisible = newTab }, 
        match newTab with 
        | Properties -> Cmd.batch <| editCmds
        | Catalogue -> Cmd.batch  <| editCmds
        | Simulation -> Cmd.batch <| editCmds
        | Build -> Cmd.batch  <| editCmds
        //| TruthTable -> Cmd.batch <| editCmds
        | Transition -> Cmd.none
    | ChangeSimSubTab subTab ->
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        { model with SimSubTabVisible = subTab},
        match subTab with
        | StepSim -> Cmd.batch <| editCmds
        | TruthTable -> Cmd.batch <| editCmds
        | WaveSim -> Cmd.batch <| editCmds
    | ChangeBuildTabVisibility ->
        {model with BuildVisible = (not <| model.BuildVisible)}, Cmd.none
    | SetHighlighted (componentIds, connectionIds) ->
        SheetUpdate.update (SheetT.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model
    | SetSelWavesHighlighted connIds ->
        SheetUpdate.update (SheetT.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model
    | SetClipboard components -> { model with Clipboard = components }, Cmd.none
    | SetCreateComponent pos -> { model with LastCreatedComponent = Some pos }, Cmd.none
    | SetProject project ->
        printf $"Setting project with component: '{project.OpenFileName}'"
        model
        |> set currentProj_ (Some project) 
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath, Cmd.none
    | UpdateProject update ->
        CustomCompPorts.updateProjectFiles true update model, Cmd.none
    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model,Cmd.none
    | ShowPopup popup -> { model with PopupViewFunc = Some popup }, Cmd.none
    | ShowStaticInfoPopup(title, body, dispatch) ->
        let foot = div [] []
        PopupHelpers.closablePopup title body foot [Width 800] dispatch
        model, Cmd.none
    | ClosePopup ->
        { model with
            PopupViewFunc = None;
            PopupDialogData =
                    { model.PopupDialogData with
                        Text = None;
                        Int = None;
                        Int2 = None;
                        MemorySetup = None;
                        MemoryEditorData = None;
                        VerilogCode = None;
                        VerilogErrors = [];
                    }}, Cmd.none
    | SetPopupDialogText text ->
        set (popupDialogData_ >-> text_) text model, Cmd.none
    | SetPopupDialogBadLabel isBad ->
        set (popupDialogData_ >-> badLabel_) isBad model, Cmd.none
    | SetPopupDialogCode code ->
        set (popupDialogData_ >-> verilogCode_) code model, Cmd.none
    | SetPopupDialogVerilogErrors errorList ->
        set (popupDialogData_ >-> verilogErrors_) errorList model, Cmd.none
    | SetPopupDialogInt int ->
        set (popupDialogData_ >-> int_) int model, Cmd.none
    | SetPopupDialogInt2 int ->
        set (popupDialogData_ >-> int2_) int model, Cmd.none
    | SetPopupDialogTwoInts data ->
        { model with PopupDialogData =
                        match data with
                        | n, FirstInt,_ ->  {model.PopupDialogData with Int  = Option.map int32 n}
                        | n, SecondInt, optText -> {model.PopupDialogData with Int2 = n}
        }, Cmd.none
    | SetPopupDialogMemorySetup m ->
        set (popupDialogData_ >-> memorySetup_) m model, Cmd.none
    | SetPopupMemoryEditorData m ->
        set (popupDialogData_ >-> memoryEditorData_) m model, Cmd.none
    | SetPopupProgress progOpt ->
        set (popupDialogData_ >-> progress_) progOpt model, Cmd.none
    | UpdatePopupProgress updateFn ->
        { model with PopupDialogData = {model.PopupDialogData with Progress = Option.map updateFn model.PopupDialogData.Progress} }, Cmd.none
    | SetPopupConstraintTypeSel ct ->
        set (popupDialogData_ >-> constraintTypeSel_) ct model, Cmd.none
    | SetPopupConstraintIOSel io ->
        set (popupDialogData_ >-> constraintIOSel_) io model, Cmd.none
    | SetPopupConstraintErrorMsg msg ->
        set (popupDialogData_ >-> constraintErrorMsg_) msg model, Cmd.none
    | SetPopupNewConstraint con ->
        set (popupDialogData_ >-> newConstraint_) con model, Cmd.none
    | TogglePopupAlgebraInput (io,sd) ->
        let (_,_,w) = io
        let oldLst =
            match model.PopupDialogData.AlgebraInputs with
            | Some l -> l
            | None -> failwithf  "what? PopupDialogData.AlgebraInputs is None when trying to toggle"
        if List.contains io oldLst then // Algebra -> Values
            let zero = IData <| convertIntToFastData w 0u
            match ConstraintReduceView.validateAlgebraInput io zero sd with
            | Ok _ ->
                let newLst = List.except [io] oldLst
                model
                |> map popupDialogData_ (
                    set algebraInputs_ (Some newLst) >> 
                    set algebraError_ None
                ), Cmd.none
                
            | Error err ->
                let newLst = List.except [io] oldLst
                model
                |> map popupDialogData_ (
                    set algebraInputs_ (Some newLst) >> 
                    set algebraError_ (Some err)
                ), Cmd.none

        else // Values -> Algebra
            let alg = IAlg <| SingleTerm io
            match ConstraintReduceView.validateAlgebraInput io alg sd with
            | Ok _ ->
                let newLst = io::oldLst
                model
                |> map popupDialogData_ (
                    set algebraInputs_ (Some newLst) >> 
                    set algebraError_ None
                ), Cmd.none
            | Error err ->
                let newLst = io::oldLst
                model
                |> map popupDialogData_ (
                    set algebraInputs_ (Some newLst) >> 
                    set algebraError_ (Some err)
                ), Cmd.none

    | SetPopupAlgebraInputs opt ->
        set (popupDialogData_ >-> algebraInputs_) opt model, Cmd.none
    | SetPopupAlgebraError opt ->
        set (popupDialogData_ >-> algebraError_) opt model, Cmd.none
    | SimulateWithProgressBar simPars ->
        SimulationView.simulateWithProgressBar simPars model
    | SetSelectedComponentMemoryLocation (addr,data) ->
        {model with SelectedComponent = updateComponentMemory addr data model.SelectedComponent}, Cmd.none
    | CloseDiagramNotification ->
        { model with Notifications = {model.Notifications with FromDiagram = None} }, Cmd.none
    | SetSimulationNotification n ->
        { model with Notifications =
                        { model.Notifications with FromSimulation = Some n} }, Cmd.none
    | CloseSimulationNotification ->
        { model with Notifications = {model.Notifications with FromSimulation = None} }, Cmd.none
    | CloseWaveSimNotification ->
        { model with Notifications = {model.Notifications with FromWaveSim = None} }, Cmd.none
    | SetFilesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromFiles = Some n} }, Cmd.none
    | CloseFilesNotification ->
        { model with Notifications = {model.Notifications with FromFiles = None} }, Cmd.none
    | SetMemoryEditorNotification n ->
        { model with Notifications =
                        { model.Notifications with FromMemoryEditor = Some n} }, Cmd.none
    | CloseMemoryEditorNotification ->
        { model with Notifications = { model.Notifications with FromMemoryEditor = None} }, Cmd.none
    | SetPropertiesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromProperties = Some n} }, Cmd.none
    | ClosePropertiesNotification ->
        { model with Notifications = { model.Notifications with FromProperties = None} }, Cmd.none
    | SetTopMenu t ->
        { model with TopMenuOpenState = t}, Cmd.none
    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model), Cmd.none
    | ExecCmd cmd ->
        model, cmd
    | ExecFuncAsynch func ->
             let cmd' = 
                Elmish.Cmd.OfAsyncImmediate.result (async { 
                //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
                //this number only seems to affect the wavesim spinner cursor, it does not help with open project/change sheet spinner cursor
                    do! (Async.Sleep 100) 
                    if Set.contains "update" JSHelpers.debugTraceUI then
                        printfn "Starting ExecFuncAsynch payload"
                    let cmd = func ()                    
                    return (ExecCmd cmd)})
             model, cmd'
    | ExecCmdAsynch cmd ->
        let cmd' = 
            Elmish.Cmd.OfAsyncImmediate.result (async { 
            //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
            //this number only seems to affect the wavesim spinner cursor.
                do! (Async.Sleep 300)
                return (ExecCmd cmd)})
        model, cmd'
    | SendSeqMsgAsynch msgs ->
        model, SimulationView.doBatchOfMsgsAsynch msgs
    | MenuAction(act,dispatch) ->
        match act with 
        | MenuSaveFile -> getMenuView act model dispatch, Cmd.ofMsg (Sheet SheetT.SaveSymbols)
        | MenuSaveProjectInNewFormat -> getMenuView act model dispatch, Cmd.ofMsg (Sheet SheetT.SaveSymbols)
        | _ -> getMenuView act model dispatch, Cmd.none
    | ContextMenuAction e ->
        let menuType = getContextMenu e model
        renderer.ipcRenderer.send("show-context-menu", [|unbox menuType|])
        model, Cmd.none
    | ContextMenuItemClick(menuType, item, dispatch) ->
        processContextMenuClick menuType item dispatch model
    | DiagramMouseEvent ->
        model, Cmd.none
    | SelectionHasChanged -> 
        { model with ConnsOfSelectedWavesAreHighlighted = true }
        |> (fun m -> m, Cmd.none)
    | SetIsLoading b ->
        let cmd = if b then Cmd.none else Cmd.ofMsg (Sheet (SheetT.SetSpinner false)) //Turn off spinner after project/sheet is loaded
        {model with IsLoading = b}, cmd
    | ReadUserData userAppDir ->
        printfn $"Got user app dir of {userAppDir}"
        let model,cmd = readUserData userAppDir model        
        model,cmd
    | SetUserData (data: UserData) ->
        let model =
            {model with UserData = data}
            |> userDataToDrawBlockModel
        model, Cmd.none
    | SetThemeUserData (theme: DrawModelType.SymbolT.ThemeType) ->
        let model =
            {model with UserData = {model.UserData with Theme=theme}}
            |> userDataToDrawBlockModel
        model, Cmd.none
    | ExecutePendingMessages n ->
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
    // Various messages here that are not implemented as yet, or are no longer used
    // should be sorted out
    | LockTabsToWaveSim | UnlockTabsFromWaveSim | SetExitDialog _ 
    | SetPopupInputConstraints _ | SetPopupOutputConstraints _ 
    | SetPropertiesExtraDialogText _ | SetRouterInteractive _ 
    | ShowExitDialog _ -> model, Cmd.none
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd
    | JSDiagramMsg _ | KeyboardShortcutMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none
    |> (fun (newModel,cmd) -> resetDialogIfSelectionHasChanged newModel oldModel,cmd)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible
