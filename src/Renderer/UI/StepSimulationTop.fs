module StepSimulationTop

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open Elmish

open DiagramStyle
open Notifications
open PopupHelpers
open ModelType
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasExtractor
open Simulator
open ModelHelpers
open Optics
open Optics.Optic
open Optics.Operators
open SimulationView

let viewSimulation canvasState model dispatch =
    /// This is the top-level function that creates a new fast simulation for the step simulator.
    let startSimulation model _ =
        // make sure simulation runs with uptodate memory contents based on linked files.
        // memory in model is not updated because the model update will be lost. This does
        // matter because if memory contents are ever viewed the viewer will update them then.
        let model = MemoryEditorView.updateAllMemoryComps model
        tryGetSimData false canvasState model
        |> function
            | Ok simData -> 
                Ok simData
            | Error simError ->
                setSimErrorFeedback simError model dispatch
                Error simError
        |> StartSimulation
        |> dispatch
        match model.CurrentProj with
        | Some project ->
            let loadedDependencies = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            let ldcs = addStateToLoadedComponents simCache.Name canvasState loadedDependencies
            simCache <- {simCache with StoredState = ldcs}
        | None -> ()

    let hasCanvasChanged
        (currentCanvasState)
        (simCache)
        (model)
        : bool = 
        match model.CurrentProj with
        | Some project ->
            let loadedDependencies = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            let ldcs = addStateToLoadedComponents simCache.Name currentCanvasState loadedDependencies
            let isSame = storedstateisEqual simCache ldcs
            not isSame
        | _ -> false
    
    let simRes = simulateModel false None Constants.maxArraySize canvasState model
    // let JSState = model.Diagram.GetCanvasState ()
    match model.CurrentStepSimulationStep with
    | None ->
        let isSync = match simRes with | Ok {IsSynchronous=true},_ -> true | _ -> false
        let buttonColor, buttonText = 
            match simRes with
            | Ok _, _ -> IsSuccess, "Start Simulation"
            | Error _, _ -> IsWarning, "See Problems"
        div [] [
            str "Simulate simple logic using this tab."
            br []
            str (if isSync then "You can also use the Wave Simulation tab to view waveforms" else "")
            br []; br []
            Button.button
                [ 
                    Button.Color buttonColor; 
                    Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(startSimulation, dispatch)) ; 
                ]
                [ str buttonText ]
        ]
    | Some sim ->
        let canvasStateChange = hasCanvasChanged canvasState simCache model
        let body = match sim with
                    | Error simError -> viewSimulationError canvasState simError model StepSim dispatch
                    | Ok simData -> 
                        if simCache.RestartSim then
                            let clock = simData.ClockTickNumber
                            startSimulation model ()
                            simCache <- {simCache with RestartSim = false}
                            simCache <- {simCache with ClockTickRefresh = clock}
                        if (simData.ClockTickNumber = 0 && not (simCache.ClockTickRefresh = 0)) then
                            IncrementSimulationClockTick simCache.ClockTickRefresh |> dispatch
                            FastRun.runFastSimulation None simCache.ClockTickRefresh simData.FastSim |> ignore
                            simCache <- {simCache with ClockTickRefresh = 0}
                        viewSimulationData simData.ClockTickNumber simData model dispatch
        let setDefaultButton =
            match sim with
            | Error _ -> div [] []
            | Ok simData ->
                Button.button
                    [ 
                        Button.Color IsInfo;
                        Button.Disabled (InputDefaultsEqualInputs simData.FastSim model simData.ClockTickNumber)
                        Button.OnClick (fun _ -> setInputDefaultsFromInputs simData.FastSim dispatch simData.ClockTickNumber) ; 
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ]
                    [ str "Save current input values as default" ]

        let confirmRefreshPopup (model:Model) dispatch (simData: SimulationData) =
            fun (model:Model) ->
                div [] 
                    [
                    div [Style [Height "60px"; Display DisplayOptions.Block; MarginBottom "5px"]] [
                    h6 [Style [Width "80%"; Float FloatOptions.Left;]] [str $"Refresh the simulation using current values of the inputs and the latest design? 
                    The current values will be used as default for future simulations."]
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ ->
                            setInputDefaultsFromInputs simData.FastSim dispatch simData.ClockTickNumber
                            simCache <- {simCache with RestartSim = true}
                            ClosePopup |> dispatch
                        )
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right; MarginTop "10px";]]
                    ] [ str "Ok" ]]
                    hr [Style [Width "100%"; Float FloatOptions.Left;]]
                    div [Style [Height "50px"; Display DisplayOptions.Block;]] [
                    h6 [Style [Width "80%"; Float FloatOptions.Left ]] [str $"Refresh the simulation using default values of inputs, current values will be lost."]
                    Button.button [
                        Button.Color IsInfo
                        Button.OnClick (fun _ ->
                            let clock = simData.ClockTickNumber
                            startSimulation model dispatch
                            simCache <- {simCache with ClockTickRefresh = clock}
                            ClosePopup |> dispatch
                        )
                        Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Right ]]
                    ] [ str "Reset" ]]
                ]

        let buttonColor, buttonIcon = 
            match simRes with
            | Ok _, _ -> IsSuccess, refreshSvg "white" "20px"
            | Error _, _ -> IsWarning, str "See Problems"

        let createRefreshButton buttonColor buttonIcon onClick =
            Button.button [
                Button.Color buttonColor;
                Button.OnClick onClick
                Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.None; MarginLeft "5px"]]
            ] [buttonIcon]

        let startSimulationUpdateCache clock =
            startSimulation model ()
            simCache <- { simCache with ClockTickRefresh = clock }

        let createRefreshButtonForSimData sim model dispatch =
            match sim with
            | Ok (simData: SimulationData) ->
                if InputDefaultsEqualInputsRefresh simData.FastSim model then
                    createRefreshButton buttonColor buttonIcon (fun _ ->
                        let clock = simData.ClockTickNumber
                        startSimulationUpdateCache clock)
                else
                    createRefreshButton buttonColor buttonIcon (fun _ ->
                    match simRes with
                    | Ok _, _ ->
                        dialogPopupRefresh
                            "Refresh"
                            (confirmRefreshPopup model dispatch simData)
                            []
                            dispatch
                    | Error _, _ -> startSimulationUpdateCache simData.ClockTickNumber)
            | _ -> emptyRefreshSVG

        let createRefreshButtonError =
            createRefreshButton buttonColor buttonIcon (fun _ ->
                let clock = simCache.ClockTickRefresh
                startSimulationUpdateCache clock)

        let refreshButton =
            match canvasStateChange, sim with
            | true, Ok _ -> createRefreshButtonForSimData sim model dispatch
            | true, Error _ -> createRefreshButtonError
            | _ -> emptyRefreshSVG
    
        div [Style [Height "100%"]] [
            div [Style [Height "40px"]] [
            Button.button
                [
                    Button.Color IsDanger;
                    Button.OnClick (fun _ ->
                        simReset dispatch
                        dispatch EndSimulation);
                    Button.Props [Style [Display DisplayOptions.Inline; Float FloatOptions.Left;]]]
                [ str "End simulation" ]
            refreshButton
            setDefaultButton
            ]
            br []; 
            div [Style [Display DisplayOptions.Block;]] []
            str "The simulation uses the diagram as it was at the moment of
                 pressing the \"Start simulation\" or \"Refresh\" button using default input values."
            hr []
            body
        ]

let tryStartSimulationAfterErrorFix (simType:SimSubTab) (model:Model) =
    let withMsg msg model = model, Cmd.ofMsg msg
    let withMsgs msgs model = model, Cmd.batch (msgs |> List.map Cmd.ofMsg)
    let withCmdTTMsg ttMsg model = model, Cmd.ofMsg (TruthTableMsg ttMsg)
    let conns = BusWire.extractConnections model.Sheet.Wire
    let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
    let canvasState = comps,conns
    let simErrFeedback simErr otherMsg =
            (getSimErrFeedbackMessages simErr model) @ [otherMsg]

    match simType with
        | StepSim ->
            tryGetSimData false canvasState model
            |> function
                | Ok (simData) -> 
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsg (StartSimulation (Ok simData))
                | Error simError ->
                    model
                    |> set currentStepSimulationStep_ (simError |> Error |> Some)
                    |> withMsgs (simErrFeedback simError (StartSimulation (Error simError)))

        | TruthTable ->
            simulateModel false None 2 canvasState model
            |> function
                | Ok (simData), state ->
                    if simData.IsSynchronous = false then
                        model
                        |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                        |> withCmdTTMsg (GenerateTruthTable (Some (Ok simData, state)))
                    else
                        { model with CurrentStepSimulationStep = None }
                        |> withCmdTTMsg CloseTruthTable
                | Error simError, state ->
                    let feedbackMsg = GenerateTruthTable (Some (Error simError, state)) |> TruthTableMsg
                    model
                    |> set currentStepSimulationStep_ (simError |> Error |> Some)
                    |> withMsgs (simErrFeedback simError feedbackMsg)

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
            let wsModel = getWSModel model
            //printfn $"simSheet={wsSheet}, wsModel sheet = {wsModel.TopSheet},{wsModel.FastSim.SimulatedTopSheet}, state={wsModel.State}"
            match simulateModel
                    true
                    model.WaveSimSheet
                    (wsModel.WSConfig.LastClock + Constants.maxStepsOverflow)
                    canvasState
                    model with

            | (Error simError, _) ->
                model
                |> set currentStepSimulationStep_ (simError |> Error |> Some)
                |> withMsgs (simErrFeedback simError (SetWSModelAndSheet ({ wsModel with State = SimError simError }, wsSheet)))
            | (Ok simData, canvState) ->
                if simData.IsSynchronous then
                    setFastSimInputsToDefault simData.FastSim
                    let wsModel = { wsModel with State = Loading}
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsgs [SetWSModelAndSheet (wsModel, wsSheet) ; RefreshWaveSim wsModel]
                else
                    model
                    |> set currentStepSimulationStep_ (simData |> Ok |> Some)
                    |> withMsg (SetWSModelAndSheet ({ wsModel with State = NonSequential }, wsSheet))


