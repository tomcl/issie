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
                prepareSimulation proj.OpenFileName (state) proj.LoadedComponents 
                |> (function 
                    | Ok sim -> 
                        let path = FilesIO.pathJoin [| proj.ProjectPath; proj.OpenFileName + ".v" |]
                        printfn "writing %s" proj.ProjectPath
                        try 
                            FilesIO.writeFile path (Verilog.getVerilog vType sim.FastSim)
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

//----------------------------View level simulation helpers------------------------------------//

type SimCache = {
    Name: string
    StoredState: CanvasState
    StoredResult: Result<SimulationData, SimulationError>
    }



let simCacheInit name = {
    Name = name; 
    StoredState = ([],[]) // reduced canvas state from extractReducedState
    StoredResult = Ok {
        FastSim = FastCreate.emptyFastSimulation()
        Graph = Map.empty 
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }
        
/// Used to store last canvas state and its simulation
let mutable simCache: SimCache = simCacheInit ""

/// Start up a simulation, doing all necessary checks and generating simulation errors
/// if necesary. The code to do this is quite long so results are memoized. this is complicated because
/// we want the comparison (in the case nothing has chnaged) to be fast.
/// 1. If the current sheet changes we redo the simulation. 
/// 2. While current sheet does not change we assume the other sheets
/// ( and so subsheet content) cannot change. 
/// 3. Therefore we need only compare current sheet canvasState with its
/// initial value. This is compared using extractReducedState to make a copy that has geometry info removed 
/// from components and connections.
let rec prepareSimulationMemoized
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    let rState = extractReducedState canvasState
    if diagramName <> simCache.Name then
        simCache <- simCacheInit diagramName
        // recursive call having initialised the cache state on sheet change
        prepareSimulationMemoized diagramName canvasState loadedDependencies
    else
        let isSame = rState = simCache.StoredState
        if  isSame then
            simCache.StoredResult, rState
        else
            printfn "New simulation"
            let simResult = prepareSimulation diagramName rState loadedDependencies
            simCache <- {
                Name = diagramName
                StoredState = rState
                StoredResult = simResult
                }
            simResult, rState
   

/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
let makeSimData model =
    let start = TimeHelpers.getTimeMs()
    match model.Sheet.GetCanvasState(), model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> prepareSimulationMemoized project.OpenFileName
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
let private makeIOLabel label width =
    let label = cropToLength 15 true label
    match width with
    | 1 -> label
    | w -> sprintf "%s (%d bits)" label w

let private viewSimulationInputs
        (numberBase : NumberBase)
        (simulationData : SimulationData)
        (inputs : (SimulationIO * WireData) list)
        dispatch =
    let simulationGraph = simulationData.Graph
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), wireData) =
#if ASSERTS
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewSimulationInput for %s: expected %d but got %A" inputLabel width wireData.Length
#endif
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation inputs."
            | [bit] ->
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
                        FastRun.changeInput (ComponentId inputId) [newBit] simulationData.ClockTickNumber simulationData.FastSim
                        dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                    )
                ] [ str <| bitToString bit ]
            | bits ->
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
                                FastRun.changeInput (ComponentId inputId) bits simulationData.ClockTickNumber simulationData.FastSim
                                dispatch <| SetSimulationGraph(graph, simulationData.FastSim)
                        ))
                    ]
                ]
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

let private viewSimulationOutputs numBase (simOutputs : (SimulationIO * WireData) list) =
    let makeOutputLine ((ComponentId _, ComponentLabel outputLabel, width), wireData) =
#if ASSERTS
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewSimulationOutput for %s: expcted %d but got %d" outputLabel width wireData.Length
#endif
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | [bit] -> staticBitButton bit
            | bits -> staticNumberBox numBase bits
        splittedLine (str <| makeIOLabel outputLabel width) valueHandle
    div [] <| List.map makeOutputLine simOutputs

let private viewViewers numBase (simViewers : ((string*string) * int * WireData) list) =
    let makeViewerOutputLine ((label,fullName), width, wireData) =
#if ASSERTS
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewViewer for %s: expcted %d but got %d" label width wireData.Length
#endif
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | [bit] -> staticBitButton bit
            | bits -> staticNumberBox numBase bits
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
                str <| "Error found in dependency \"" + dep + "\":"
                br []
                str simError.Msg
                br []
                str <| "Please fix the error in the dependency and retry."
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

let simulateWithTime steps simData =
    let startTime = getTimeMs()
    FastRun.runFastSimulation (steps + simData.ClockTickNumber) simData.FastSim 
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
        FastRun.runFastSimulation clock simData.FastSim 
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
    let estimatedTime = (float steps / float initChunk) * (simulateWithTime initChunk simData + 0.0000001)
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
        FastRun.runFastSimulation clock simData.FastSim 
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
                        FastRun.runFastSimulation (simData.ClockTickNumber+1) simData.FastSim 
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

let SetSimErrorFeedback (simError:SimulatorTypes.SimulationError) (model:Model) (dispatch: Msg -> Unit) =
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
        dispatch <| Sheet(SheetT.SetWaveSimMode false)


let viewSimulation model dispatch =
    let state = model.Sheet.GetCanvasState ()
    // let JSState = model.Diagram.GetCanvasState ()
    let startSimulation () =
        match state, model.CurrentProj with
        | _, None -> failwith "what? Cannot start a simulation without a project"
        | canvasState, Some project ->
            let otherComponents =
                project.LoadedComponents
                |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            simCache <- simCacheInit ""
            (canvasState, otherComponents)
            ||> prepareSimulationMemoized project.OpenFileName
            |> function
               | Ok (simData), state -> Ok simData
               | Error simError, state ->
                  printfn $"ERROR:{simError}"
                  SetSimErrorFeedback simError model dispatch
                  Error simError
            |> StartSimulation
            |> dispatch
            // let sheetName = project.OpenFileName
            // match Map.tryFind sheetName (fst model.WaveSim) with
            // | Some wSModel ->
            //     printfn "Closing wavesim..."
            //     dispatch <| SetWSModel {wSModel with InitWaveSimGraph=None; WSViewState=WSClosed; WSTransition = None}
            //     dispatch <| SetWaveSimIsOutOfDate true
            // | None -> ()
    match model.CurrentStepSimulationStep with
    | None ->
        let simRes = makeSimData model
        let isSync = match simRes with | Some( Ok {IsSynchronous=true},_) | _ -> false
        let buttonColor, buttonText = 
            match simRes with
            | None -> IColor.IsWhite, ""
            | Some (Ok _, _) -> IsSuccess, "Start Simulation"
            | Some (Error _, _) -> IsWarning, "See Problems"
        div [] [
            str "Simulate simple logic using this tab."
            br []
            str (if isSync then "You can also use the Waveforms >> button to view waveforms" else "")
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
        let endSimulation _ =
            dispatch CloseSimulationNotification // Close error notifications.
            dispatch <| Sheet (SheetT.ResetSelection) // Remove highlights.
            dispatch EndSimulation // End simulation.
            dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endSimulation ]
                [ str "End simulation" ]
            br []; br []
            str "The simulation uses the diagram as it was at the moment of
                 pressing the \"Start simulation\" button."
            hr []
            body
        ]
