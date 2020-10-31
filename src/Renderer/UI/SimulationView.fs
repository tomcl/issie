(*
    SimulationView.fs

    View for simulation in the right tab.
*)

module SimulationView

open Fulma
open Fable.React
open Fable.React.Props

open NumberHelpers
open Helpers
open JSHelpers
open DiagramStyle
open PopupView
open MemoryEditorView
open MessageType
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open NumberHelpers

//----------------------------View level simulation helpers------------------------------------//

type SimCache = {
    Name: string
    StoredState: CanvasState
    StoredResult: Result<SimulationData, SimulationError>
    }



let simCacheInit name = {
    Name = name; 
    StoredState = ([],[]) 
    StoredResult = Ok {
        Graph = Map.empty
        Inputs = []
        Outputs = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }
        

let mutable simCache: SimCache = simCacheInit ""

let rec prepareSimulationMemoised
        (diagramName : string)
        (canvasState : JSCanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    let rState = extractReducedState canvasState
    if diagramName <> simCache.Name then
        simCache <- simCacheInit diagramName
        prepareSimulationMemoised diagramName canvasState loadedDependencies
    else
        let isSame = rState = simCache.StoredState
        if  isSame then
            simCache.StoredResult, rState
        else
            let simResult = prepareSimulation diagramName rState loadedDependencies
            simCache <- {
                Name = diagramName
                StoredState = rState
                StoredResult = simResult
                }
            simResult, rState

    
    


/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails
let makeSimData model =
    match model.Diagram.GetCanvasState(), model.CurrProject with
    | None, _ -> None
    | _, None -> None
    | Some jsState, Some project ->
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (jsState, otherComponents)
        ||> prepareSimulationMemoised project.OpenFileName
        |> Some


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
        (simulationGraph : SimulationGraph)
        (inputs : (SimulationIO * WireData) list)
        dispatch =
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), wireData) =
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewSimulationInput for %s: expcted %d but got %d" inputLabel width wireData.Length
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation inputs."
            | [bit] ->
                // For simple bits, just have a Zero/One button.
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    (match bit with Zero -> Button.IsOutlined | One -> Button.Color IsPrimary)
                    Button.IsHovered false
                    Button.OnClick (fun _ ->
                        let newBit = match bit with
                                     | Zero -> One
                                     | One -> Zero
                        feedSimulationInput simulationGraph
                                            (ComponentId inputId) [newBit]
                        |> SetSimulationGraph |> dispatch
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
                            match strToIntCheckWidth text width with
                            | Error err ->
                                let note = errorPropsNotification err
                                dispatch  <| SetSimulationNotification note
                            | Ok num ->
                                let bits = convertIntToWireData width num
                                // Close simulation notifications.
                                CloseSimulationNotification |> dispatch
                                // Feed input.
                                feedSimulationInput simulationGraph
                                                    (ComponentId inputId) bits
                                |> SetSimulationGraph |> dispatch
                        ))
                    ]
                ]
        splittedLine (str <| makeIOLabel inputLabel width) valueHandle
    div [] <| List.map makeInputLine inputs

let private staticBitButton bit =
    Button.button [
        Button.Props [ simulationBitStyle ]
        Button.Color IsPrimary
        (match bit with Zero -> Button.IsOutlined | One -> Button.Color IsPrimary)
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
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewSimulationOutput for %s: expcted %d but got %d" outputLabel width wireData.Length
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | [bit] -> staticBitButton bit
            | bits -> staticNumberBox numBase bits
        splittedLine (str <| makeIOLabel outputLabel width) valueHandle
    div [] <| List.map makeOutputLine simOutputs

let private viewStatefulComponents comps numBase model dispatch =
    let getWithDefault (ComponentLabel lab) = if lab = "" then "no-label" else lab
    let makeStateLine (comp : SimulationComponent) =
        match comp.State with
        | DffState bit ->
            let label = sprintf "DFF: %s" <| getWithDefault comp.Label
            [ splittedLine (str label) (staticBitButton bit) ]
        | RegisterState bits ->
            let label = sprintf "Register: %s (%d bits)" (getWithDefault comp.Label) bits.Length
            [ splittedLine (str label) (staticNumberBox numBase bits) ]
        | RamState mem ->
            let label = sprintf "RAM: %s" <| getWithDefault comp.Label
            let initialMem compType = match compType with RAM m -> m | _ -> failwithf "what? viewStatefulComponents expected RAM component but got: %A" compType
            let viewDiffBtn =
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    Button.OnClick (fun _ ->
                        openMemoryDiffViewer (initialMem comp.Type) mem model dispatch
                    )
                ] [ str "View" ]
            [ splittedLine (str label) viewDiffBtn ]
        | NoState -> []
    div [] ( List.collect makeStateLine comps )

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

let private viewSimulationData (simData : SimulationData) model dispatch =
    let hasMultiBitOutputs =
        simData.Outputs |> List.filter (fun (_,_,w) -> w > 1) |> List.isEmpty |> not
    let maybeBaseSelector =
        match hasMultiBitOutputs with
        | false -> div [] []
        | true -> baseSelector simData.NumberBase (changeBase dispatch)
    let maybeClockTickBtn =
        match simData.IsSynchronous with
        | false -> div [] []
        | true ->
            Button.button [
                Button.Color IsSuccess
                Button.OnClick (fun _ ->
                    feedClockTick simData.Graph |> SetSimulationGraph |> dispatch
                    IncrementSimulationClockTick |> dispatch
                )
            ] [ str <| sprintf "Clock Tick %d" simData.ClockTickNumber ]
    let maybeStatefulComponents =
        let stateful = extractStatefulComponents simData.Graph
        match List.isEmpty stateful with
        | true -> div [] []
        | false -> div [] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Stateful components" ]
            viewStatefulComponents (extractStatefulComponents simData.Graph) simData.NumberBase model dispatch
        ]
    div [] [
        splittedLine maybeBaseSelector maybeClockTickBtn

        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewSimulationInputs
            simData.NumberBase
            simData.Graph
            (extractSimulationIOs simData.Inputs simData.Graph)
            dispatch

        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Outputs" ]
        viewSimulationOutputs simData.NumberBase
        <| extractSimulationIOs simData.Outputs simData.Graph

        maybeStatefulComponents
    ]

  

let viewSimulation model dispatch =
    let JSState = model.Diagram.GetCanvasState ()
    let startSimulation () =
        match JSState, model.CurrProject with
        | None, _ -> ()
        | _, None -> failwith "what? Cannot start a simulation without a project"
        | Some jsState, Some project ->
            let otherComponents =
                project.LoadedComponents
                |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            (jsState, otherComponents)
            ||> prepareSimulationMemoised project.OpenFileName
            |> function
               | Ok (simData), state -> Ok simData
               | Error simError, state ->
                  if simError.InDependency.IsNone then
                      // Highlight the affected components and connection only if
                      // the error is in the current diagram and not in a
                      // dependency.
                      (simError.ComponentsAffected, simError.ConnectionsAffected)
                      |> SetHighlighted |> dispatch
                  Error simError
            |> StartSimulation
            |> dispatch
    match model.Simulation with
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
                [ Button.Color buttonColor; Button.OnClick (fun _ -> startSimulation()) ]
                [ str buttonText ]
        ]
    | Some sim ->
        let body = match sim with
                   | Error simError -> viewSimulationError simError
                   | Ok simData -> viewSimulationData simData model dispatch
        let endSimulation _ =
            dispatch CloseSimulationNotification // Close error notifications.
            dispatch <| SetHighlighted ([], []) // Remove highlights.
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
