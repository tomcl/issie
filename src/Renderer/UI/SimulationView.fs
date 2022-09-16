//(*
//    SimulationView.fs
//
//    View for simulation in the right tab.
//*)
//
module SimulationView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupView
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open Sheet.SheetInterface
open DrawModelType

open Optics
open Optics.Operators

module Constants =
    let maxArraySize = 550

/// save verilog file
/// TODO: the simulation error display here is shared with step simulation and also waveform simulation -
/// maybe it should be a subfunction.
let verilogOutput (vType: Verilog.VMode) (model: Model) (dispatch: Msg -> Unit) =
    printfn "Verilog output"
    match FileMenuView.updateProjectFromCanvas model dispatch, model.Sheet.GetCanvasState() with
        | Some proj, state ->
            match model.UIState with  //TODO should this be its own UI operation?
            | Some _ ->
                () // do nothing if in middle of I/O operation
            | None ->
                prepareSimulation 2 proj.OpenFileName (state) proj.LoadedComponents
                |> (function 
                    | Ok sim -> 
                        let path = FilesIO.pathJoin [| proj.ProjectPath; proj.OpenFileName + ".v" |]
                        printfn "writing %s" proj.ProjectPath
                        try 
                            let code = (Verilog.getVerilog vType sim.FastSim Verilog.CompilationProfile.Release)
                            FilesIO.writeFile path code
                        with
                        | e -> 
                            printfn $"Error in Verilog output: {e.Message}"
                            Error e.Message
                        |> Notifications.displayAlertOnError dispatch
                        dispatch <| ChangeRightTab Simulation
                        let note = successSimulationNotification $"verilog output written to file {path}"
                        dispatch  <| SetSimulationNotification note
                    | Error simError ->
                       printfn $"Error in simulation prevents verilog output {simError.Msg}"
                       dispatch <| ChangeRightTab Simulation
                       if simError.InDependency.IsNone then
                           // Highlight the affected components and connection only if
                           // the error is in the current diagram and not in a
                           // dependency.
                           (simError.ComponentsAffected, simError.ConnectionsAffected)
                           |> SetHighlighted |> dispatch
                       Error simError
                       |> StartSimulation
                       |> dispatch)
        | _ -> () // do nothing if no project is loaded

let setFastSimInputsToDefault (fs:FastSimulation) =
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun cid fc -> fst cid, match fc.FType with | Input1 (w,defVal) -> (w,defVal) | _ -> failwithf "What? Impossible")
    |> Map.toList
    |> List.map (fun ( _, (cid, (w,defaultVal ))) -> 
        match w,defaultVal with
        | _, Some defaultVal -> cid, convertIntToWireData w (int64 defaultVal)
        | _, None -> cid, convertIntToWireData w 0L)
    |> List.iter (fun (cid, wire) -> FastRun.changeInput cid (FSInterface.IData wire) 0 fs)

let InputDefaultsEqualInputs fs (model:Model) =
    let setInputDefault (newDefault: int) (sym: SymbolT.Symbol) =
        let comp = sym.Component
        let comp' = 
            let ct =
                match comp.Type with 
                | Input1(w,defVal) -> Input1(w,Some newDefault)
                | x -> x
            {comp with Type = ct}
        {sym with Component = comp'}
    let tick = fs.ClockTick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun cid fc -> fst cid, fc.Outputs[0].Step[tick % fs.MaxArraySize])
    |> Map.values
    |> Seq.forall (fun (cid, currentValue) -> 
            match currentValue with
            | Data fd when Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) -> 
                let newDefault = int (convertFastDataToInt fd)
                let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type
                match typ with | Input1 (_, Some d) -> d = newDefault | _ -> newDefault = 0
            | _ -> true)
            

let setInputDefaultsFromInputs fs (dispatch: Msg -> Unit) =
    let setInputDefault (newDefault: int) (sym: SymbolT.Symbol) =
        let comp = sym.Component
        let comp' = 
            let ct =
                match comp.Type with 
                | Input1(w,defVal) -> Input1(w,Some newDefault)
                | x -> x
            {comp with Type = ct}
        {sym with Component = comp'}
    let tick = fs.ClockTick
    fs.FComps
    |> Map.filter (fun cid fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Map.map (fun cid fc -> fst cid, fc.Outputs[0].Step[tick % fs.MaxArraySize])
    |> Map.values
    |> Seq.iter (fun (cid, currentValue) -> 
            match currentValue with
            | Data fd  -> 
                let newDefault = convertFastDataToInt fd
                SymbolUpdate.updateSymbol (setInputDefault (int newDefault)) cid 
                |> Optic.map DrawModelType.SheetT.symbol_ 
                |> Optic.map ModelType.sheet_
                |> UpdateModel
                |> dispatch
            | _ -> () // should never happen
        )
    |> ignore
        
       
    
//----------------------------View level simulation helpers------------------------------------//


type SimCache = {
    Name: string
    StoredState: LoadedComponent list
    StoredResult: Result<SimulationData, SimulationError>
    }



let simCacheInit () = {
    Name = ""; 
    StoredState = []
    StoredResult = Ok {
        FastSim = 
            printfn "Creating cache"
            FastCreate.simulationPlaceholder
        Graph = Map.empty 
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }
        
/// Used to store last canvas state and its simulation
let mutable simCache: SimCache = simCacheInit ()

let cacheIsEqual (cache: SimCache) (ldcs: LoadedComponent list ) : bool=
    match cache.StoredResult with
    | Error _ -> false
    | Ok {FastSim =fs} -> 
        fs.SimulatedCanvasState
        |> List.forall (fun ldc' ->
            ldcs
            |> List.tryFind (fun ldc'' -> ldc''.Name = ldc'.Name)
            |> Option.map (loadedComponentIsEqual ldc')
            |> (=) (Some true))
            

/// Start up a simulation, doing all necessary checks and generating simulation errors
/// if necesary. The code to do this is quite long so results are memoized. this is complicated because
/// we want the comparison (in the case nothing has chnaged) to be fast.
/// 1. If the current sheet changes we redo the simulation. 
/// 2. While current sheet does not change we assume the other sheets
/// ( and so subsheet content) cannot change. 
/// 3. Therefore we need only compare current sheet canvasState with its
/// initial value. This is compared using extractReducedState to make a copy that has geometry info removed 
/// from components and connections.
let prepareSimulationMemoized
        (simulationArraySize: int)
        (openFileName: string)
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    //printfn $"Diagram{diagramName}, open={openFileName}, deps = {loadedDependencies |> List.map (fun dp -> dp.Name)}"
    let storedArraySize =
        match simCache.StoredResult with
        | Ok sd -> sd.FastSim.MaxArraySize
        | _ -> 0
    let ldcs = addStateToLoadedComponents openFileName canvasState loadedDependencies
    let isSame = 
            storedArraySize = simulationArraySize &&
            diagramName = simCache.Name &&
            cacheIsEqual simCache ldcs
    if  isSame then
        simCache.StoredResult, canvasState
    else
        printfn "New simulation"
        let name, state, ldcs = getStateAndDependencies diagramName ldcs
        let simResult = prepareSimulation simulationArraySize diagramName state ldcs 
        simCache <- {
            Name = diagramName
            StoredState = ldcs
            StoredResult = simResult
            }
        simResult, canvasState
   

/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
let makeSimData (simulatedSheet: string option) (simulationArraySize: int) canvasState model =
    let start = TimeHelpers.getTimeMs()
    match canvasState, model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        let simSheet = Option.defaultValue project.OpenFileName simulatedSheet
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> prepareSimulationMemoized simulationArraySize project.OpenFileName simSheet 
        |> Some
        |> TimeHelpers.instrumentInterval "MakeSimData" start

let changeBase dispatch numBase = numBase |> SetSimulationBase |> dispatch

/// A line that can be used for an input, an output, or a state.
let private splittedLine leftContent rightConent =
    Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
        Level.left [] [
            Level.item [] [ leftContent ]
        ]
        Level.right [] [
            Level.item [] [ rightConent ]
        ]
    ]

/// Pretty print a label with its width.
let makeIOLabel label width =
    let label = cropToLength 15 true label
    match width with
    | 1 -> label
    | w -> sprintf "%s (%d bits)" label w

let private viewSimulationInputs
        (numberBase : NumberBase)
        (simulationData : SimulationData)
        (inputs : (SimulationIO * FSInterface) list)
        dispatch =

    let simulationGraph = simulationData.Graph
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData [] -> failwith "what? Empty wireData while creating a line in simulation inputs."
            | IData [bit] ->
                // For simple bits, just have a Zero/One button.
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    //Button.Color IsPrimary
                    (match bit with Zero -> Button.Color Color.IsGreyLighter | One -> Button.Color IsPrimary)
                    Button.IsHovered false
                    Button.OnClick (fun _ ->
                        let newBit = match bit with
                                     | Zero -> One
                                     | One -> Zero
                        let graph = simulationGraph
                        FastRun.changeInput (ComponentId inputId) (IData [newBit]) simulationData.ClockTickNumber simulationData.FastSim
                        dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                    )
                ] [ str <| bitToString bit ]
            | IData bits ->
                let defValue = viewNum numberBase <| convertWireDataToInt bits
                Input.text [
                    Input.Key (numberBase.ToString())
                    Input.DefaultValue defValue
                    Input.Props [
                        simulationNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match strToIntCheckWidth width text with
                            | Error err ->
                                let note = errorPropsNotification err
                                dispatch  <| SetSimulationNotification note
                            | Ok num ->
                                let bits = convertIntToWireData width num
                                // Close simulation notifications.
                                CloseSimulationNotification |> dispatch
                                // Feed input.
                                let graph = simulationGraph
                                FastRun.changeInput (ComponentId inputId) (IData bits) simulationData.ClockTickNumber simulationData.FastSim
                                dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                        ))
                    ]
                ]
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel inputLabel width) valueHandle
    div [] <| List.map makeInputLine inputs

let private staticBitButton bit =
    Button.button [
        Button.Props [ simulationBitStyle ]
        //Button.Color IsPrimary
        (match bit with Zero -> Button.Color IsGreyLighter | One -> Button.Color IsPrimary)
        Button.IsHovered false
        Button.Disabled true
    ] [ str <| bitToString bit ]

let private staticNumberBox numBase bits =
    let width = List.length bits
    let value = viewFilledNum width numBase <| convertWireDataToInt bits
    Input.text [
        Input.IsReadOnly true
        Input.Value value
        Input.Props [simulationNumberStyle]
    ]

let private viewSimulationOutputs numBase (simOutputs : (SimulationIO * FSInterface) list) =
    let makeOutputLine ((ComponentId _, ComponentLabel outputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | IData [bit] -> staticBitButton bit
            | IData bits -> staticNumberBox numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel outputLabel width) valueHandle
    div [] <| List.map makeOutputLine simOutputs

let private viewViewers numBase (simViewers : ((string*string) * int * FSInterface) list) =
    let makeViewerOutputLine ((label,fullName), width, inputVals) =
        let valueHandle =
            match inputVals with
            | IData [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | IData[bit] -> staticBitButton bit
            | IData bits -> staticNumberBox numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        let addToolTip tip react = 
            div [ 
                HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-right"
                Tooltip.dataTooltip tip
            ] [react]
        let line = 
            str <| makeIOLabel label width
            |> (fun r -> if fullName <> "" then addToolTip fullName r else r)
        splittedLine line valueHandle
    div [] <| List.map makeViewerOutputLine simViewers

let private viewStatefulComponents step comps numBase model dispatch =
    let getWithDefault (lab:string) = if lab = "" then "no-label" else lab
    let makeStateLine ((fc,state) : FastComponent*SimulationComponentState) =
        let label = getWithDefault fc.FullName
        match state with
        | RegisterState fd when fd.Width = 1 ->
            let bit = if fd = SimulatorTypes.fastDataZero then Zero else One
            let label = sprintf "DFF: %s" <| label
            [ splittedLine (str label) (staticBitButton bit) ]
        | RegisterState bits ->
            let label = sprintf "Register: %s (%d bits)" label bits.Width
            [ splittedLine (str label) (staticNumberBox numBase (bits |> convertFastDataToWireData)) ]
        | RamState mem ->
            let label = sprintf "RAM: %s" <| label
            let initialMem compType = match compType with RAM1 m | AsyncRAM1 m -> m | _ -> failwithf "what? viewStatefulComponents expected RAM component but got: %A" compType
            let viewDiffBtn =
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    Button.OnClick (fun _ ->
                        openMemoryDiffViewer (initialMem fc.FType) mem model dispatch
                    )
                ] [ str "View" ]
            [ splittedLine (str label) viewDiffBtn ]
        | _ -> []
    div [] (List.collect makeStateLine comps )

let viewSimulationError (simError : SimulationError) =
    let error = 
        match simError.InDependency with
        | None ->
            div [] [
                str simError.Msg
                br []
                str <| "Please fix the error and retry."
            ]
        | Some dep ->
            div [] [
                str <| "Error found in sheet '" + dep + "' which is a dependency:"
                br []
                str simError.Msg
                br []
                str <| "Please fix the error in this sheet and retry."
            ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]

let private simulationClockChangePopup (simData: SimulationData) (dispatch: Msg -> Unit) (dialog:PopupDialogData) =
    let step = simData.ClockTickNumber
    div [] 
        [
            h6 [] [str $"This simulation contains {simData.FastSim.FComps.Count} components"]
            (match dialog.Int with 
            | Some n when n > step -> 
                Text.p [
                    Modifiers [Modifier.TextWeight TextWeight.Bold] 
                  ] [str "Goto Tick:"]
            | _ -> Text.p [
                            Modifiers [
                                Modifier.TextWeight TextWeight.Bold
                                Modifier.TextColor IsDanger] 
                          ] [str $"The clock tick must be > {step}"])
            br []
            Input.number [
                Input.Props [AutoFocus true;Style [Width "100px"]]
                Input.DefaultValue <| sprintf "%d" step
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]

        ]

let simulateWithTime timeOut steps simData =
    let startTime = getTimeMs()
    FastRun.runFastSimulation None (steps + simData.ClockTickNumber) simData.FastSim |> ignore
    getTimeMs() - startTime

let cmd block =
    Elmish.Cmd.OfAsyncWith.perform block

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg



let simulateWithProgressBar (simProg: SimulationProgress) (model:Model) =
    match model.CurrentStepSimulationStep, model.PopupDialogData.Progress with
    | Some (Ok simData), Some barData ->
        let nComps = float simData.FastSim.FComps.Count
        let oldClock = simData.FastSim.ClockTick
        let clock = min simProg.FinalClock (simProg.ClocksPerChunk + oldClock)
        let t1 = getTimeMs()
        FastRun.runFastSimulation None clock simData.FastSim |> ignore
        printfn $"clokctick after runFastSim{clock} from {oldClock} is {simData.FastSim.ClockTick}"
        let t2 = getTimeMs()
        let speed = if t2 = t1 then 0. else (float clock - float oldClock) * nComps / (t2 - t1)
        let messages =
            if clock - oldClock < simProg.ClocksPerChunk then [   
                SetSimulationGraph(simData.Graph, simData.FastSim)
                IncrementSimulationClockTick (clock - oldClock); 
                SetPopupProgress None ]
            else [
                SetSimulationGraph(simData.Graph, simData.FastSim)
                IncrementSimulationClockTick simProg.ClocksPerChunk
                UpdatePopupProgress (fun barData -> {barData with Value = clock - simProg.InitialClock; Speed = speed})
                SimulateWithProgressBar simProg ]
        model, doBatchOfMsgsAsynch messages       
    | _ -> 
        model, Elmish.Cmd.ofMsg (SetPopupProgress None)
    
    

let simulationClockChangeAction dispatch simData (dialog:PopupDialogData) =
    let clock = 
        match dialog.Int with
        | None -> failwithf "What - must have some number from dialog"
        | Some clock -> clock
    let initClock = simData.ClockTickNumber
    let steps = clock - initClock
    let numComps = simData.FastSim.FComps.Count
    let initChunk = min steps (20000/(numComps + 1))
    let initTime = getTimeMs()
    let estimatedTime = (float steps / float initChunk) * (simulateWithTime None initChunk simData + 0.0000001)
    let chunkTime = min 2000. (estimatedTime / 5.)
    let chunk = int <| float steps * chunkTime / estimatedTime
    if steps > 2*initChunk && estimatedTime > 500. then 
        printfn "test1"
        dispatch <| SetPopupProgress 
            (Some {
                Speed = float (numComps * steps) / estimatedTime
                Value=initChunk; 
                Max=steps; 
                Title= "running simulation..."
                })
        [
            SetSimulationGraph(simData.Graph, simData.FastSim)
            IncrementSimulationClockTick (initChunk)
            ClosePopup
            SimulateWithProgressBar {
                FinalClock = clock; 
                InitialClock = initChunk + initClock; 
                ClocksPerChunk = chunk 
                }
        ]
        |> Seq.map Elmish.Cmd.ofMsg 
        |> Elmish.Cmd.batch
        |> ExecCmdAsynch
        |> dispatch
    else
        FastRun.runFastSimulation None clock simData.FastSim |> ignore
        printfn $"test2 clock={clock}, clokcticknumber= {simData.ClockTickNumber}, {simData.FastSim.ClockTick}"
        [
            SetSimulationGraph(simData.Graph, simData.FastSim)
            IncrementSimulationClockTick (clock - simData.ClockTickNumber)
            ClosePopup
        ]
        |> Seq.map Elmish.Cmd.ofMsg 
        |> Elmish.Cmd.batch
        |> ExecCmdAsynch
        |> dispatch



let private viewSimulationData (step: int) (simData : SimulationData) model dispatch =
    let hasMultiBitOutputs =
        simData.Outputs |> List.filter (fun (_,_,w) -> w > 1) |> List.isEmpty |> not
    let maybeBaseSelector =
        match hasMultiBitOutputs with
        | false -> div [] []
        | true -> baseSelector simData.NumberBase (changeBase dispatch)
    let maybeClockTickBtn =
        let step = simData.ClockTickNumber
        match simData.IsSynchronous with
        | false -> div [] []
        | true ->
            div [] [
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        let isDisabled (dialogData:PopupDialogData) =
                            match dialogData.Int with
                            | Some n -> n <= step
                            | None -> true
                        dialogPopup 
                            "Advance Simulation"
                            (simulationClockChangePopup simData dispatch)
                            "Goto Tick"
                            (simulationClockChangeAction dispatch simData)
                            isDisabled
                            []
                            dispatch)
                        ] [ str "Goto" ]
                str " "
                str " "
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        if SimulationRunner.simTrace <> None then
                            printfn "*********************Incrementing clock from simulator button******************************"
                            printfn "-------------------------------------------------------------------------------------------"
                        //let graph = feedClockTick simData.Graph
                        FastRun.runFastSimulation None (simData.ClockTickNumber+1) simData.FastSim |> ignore
                        dispatch <| SetSimulationGraph(simData.Graph, simData.FastSim)                    
                        if SimulationRunner.simTrace <> None then
                            printfn "-------------------------------------------------------------------------------------------"
                            printfn "*******************************************************************************************"
                        IncrementSimulationClockTick 1 |> dispatch
                    )
                ] [ str <| sprintf "Clock Tick %d" simData.ClockTickNumber ]
            ]
    let maybeStatefulComponents() =
        let stateful = 
            FastRun.extractStatefulComponents simData.ClockTickNumber simData.FastSim
            |> Array.toList
        match List.isEmpty stateful with
        | true -> div [] []
        | false -> div [] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Stateful components" ]
            viewStatefulComponents step stateful simData.NumberBase model dispatch
        ]
    let questionIcon = str "\u003F"

    let tip tipTxt txt =
        span [
                // Style [Float FloatOptions.Left]
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                Tooltip.dataTooltip tipTxt
            ]
            [
                Text.span [
                    Modifiers [
                        Modifier.TextColor IsPrimary
                    ]
                    Props [
                        Style [
                            Display DisplayOptions.InlineBlock
                            Width "80px"
                            TextAlign TextAlignOptions.Center]]
            ] [str txt] ]
    div [] [
        splittedLine maybeBaseSelector maybeClockTickBtn

        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewSimulationInputs
            simData.NumberBase
            simData
            (FastRun.extractFastSimulationIOs simData.Inputs simData)
            dispatch


        Heading.h5 [ 
            Heading.Props [ Style [ MarginTop "15px" ] ] 
            ] [ 
                str "Outputs &" 
                tip "Add Viewer components to any sheet in the simulation" "Viewers"
            ]
        viewViewers simData.NumberBase <| List.sort (FastRun.extractViewers simData)
        viewSimulationOutputs simData.NumberBase
        <| FastRun.extractFastSimulationIOs simData.Outputs simData

        maybeStatefulComponents()
    ]

let setSimErrorFeedback (simError:SimulatorTypes.SimulationError) (model:Model) (dispatch: Msg -> Unit) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch = SheetT.KeyPress >> sheetDispatch
    if simError.InDependency.IsNone then
        // Highlight the affected components and connection only if
        // the error is in the current diagram and not in a
        // dependency.
        let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
        dispatch <| SetHighlighted (badComps,badConns)
        if not (Sheet.isAllVisible model.Sheet badConns badComps) then
            // make whole diagram visible if any of the errors are not visible
            keyDispatch <| SheetT.KeyboardMsg.CtrlW

let viewSimulation canvasState model dispatch =
    printf "Viewing Simulation"
    // let JSState = model.Diagram.GetCanvasState ()
    let startSimulation () =
        match canvasState, model.CurrentProj with
        | _, None -> failwith "what? Cannot start a simulation without a project"
        | canvasState, Some project ->
            let otherComponents =
                project.LoadedComponents
                |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            simCache <- simCacheInit ()
            (canvasState, otherComponents)
            ||> prepareSimulationMemoized Constants.maxArraySize project.OpenFileName project.OpenFileName
            |> function
               | Ok (simData), state -> 
                if simData.FastSim.ClockTick = 0 then 
                    setFastSimInputsToDefault simData.FastSim
                Ok simData
               | Error simError, state ->
                  printfn $"ERROR:{simError}"
                  setSimErrorFeedback simError model dispatch
                  Error simError
            |> StartSimulation
            |> dispatch

    match model.CurrentStepSimulationStep with
    | None ->
        let simRes = makeSimData None Constants.maxArraySize canvasState model
        let isSync = match simRes with | Some( Ok {IsSynchronous=true},_) | _ -> false
        let buttonColor, buttonText = 
            match simRes with
            | None -> IColor.IsWhite, ""
            | Some (Ok _, _) -> IsSuccess, "Start Simulation"
            | Some (Error _, _) -> IsWarning, "See Problems"
        div [] [
            str "Simulate simple logic using this tab."
            br []
            str (if isSync then "You can also use the Wave Simulation tab to view waveforms" else "")
            br []; br []
            Button.button
                [ 
                    Button.Color buttonColor; 
                    Button.OnClick (fun _ -> startSimulation()) ; 
                ]
                [ str buttonText ]
        ]
    | Some sim ->
        let body = match sim with
                   | Error simError -> viewSimulationError simError
                   | Ok simData -> viewSimulationData simData.ClockTickNumber simData model dispatch
        let setDefaultButton =
            match sim with
            | Error _ -> div [] []
            | Ok simData ->
            div [] [
                Button.button
                    [ 
                        Button.Color IsInfo; 
                        Button.Disabled (InputDefaultsEqualInputs simData.FastSim model)
                        Button.OnClick (fun _ -> setInputDefaultsFromInputs simData.FastSim dispatch) ; 
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ]
                    [ str "Save current input values as default" ]
            ]
        let endSimulation _ =
            dispatch CloseSimulationNotification // Close error notifications.
            dispatch <| Sheet (SheetT.ResetSelection) // Remove highlights.
            dispatch EndSimulation // End simulation.
            dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.
        div [Style [Height "100%"]] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endSimulation ; Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Left ]]]
                [ str "End simulation" ]
            setDefaultButton
            br []; br []
            str "The simulation uses the diagram as it was at the moment of
                 pressing the \"Start simulation\" button."
            hr []
            body
        ]
