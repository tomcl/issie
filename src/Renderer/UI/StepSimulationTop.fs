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
        // the simulation is about to use these contents, so say if a linked file did not load
        MemoryEditorView.notifyMemoryFileErrors dispatch model
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
    
    // Whether the design builds, and whether it is synchronous, are read from the model rather than
    // worked out here: this function runs on every render, and working them out means flattening the
    // whole hierarchy. See ModelType.CircuitCheck. A verdict that no longer matches the canvas is
    // still shown - a button a moment out of date is better than an editor that stops - and asking
    // for a new one is all this does about it.
    if circuitCheckIsNeeded model canvasState then
        dispatch RequestCircuitCheck

    /// The last verdict on the design: Ok carrying whether it is synchronous. None before the
    /// first check has run, which is the only time the tab does not know.
    let verdict = model.CircuitCheck.Verdict |> Option.map fst

    /// A design not yet checked reads as buildable. The button does the real build when pressed
    /// and reports any error then, so an optimistic colour costs nothing but a wasted click.
    let buildsOk = match verdict with Some(Error _) -> false | _ -> true

    match model.CurrentStepSimulationStep with
    | None ->
        let isSync = verdict = Some(Ok true)
        let buttonColor, buttonText =
            if buildsOk then IsSuccess, "Start Simulation" else IsWarning, "See Problems"
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
                    | Error simError ->
                        // The error report scrolls as one - it has no controls of its own to keep.
                        div [Style [Flex "1 1 auto"; MinHeight "0px"; OverflowY OverflowOptions.Auto]]
                            [viewSimulationError canvasState simError model StepSim dispatch]
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
            if buildsOk then IsSuccess, refreshSvg "white" "20px" else IsWarning, str "See Problems"

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
                        if buildsOk then
                            dialogPopupRefresh
                                "Refresh"
                                (confirmRefreshPopup model dispatch simData)
                                []
                                dispatch
                        else
                            startSimulationUpdateCache simData.ClockTickNumber)
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
    
        // The pane is a flex column: what controls the simulation stays at the top whatever the
        // design contains, and the signals below it scroll. A design with more inputs and viewers
        // than fit would otherwise push End simulation, Refresh and the clock tick controls off
        // the top of the pane, so that stepping the clock meant scrolling back up to reach them.
        div [Style [Flex "1 1 auto"; MinHeight "0px"; Display DisplayOptions.Flex; FlexDirection "column"]] [
            div [Style [Flex "0 0 auto"]] [
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
            ]
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


