(*
    SimulationView.fs

    View for simulation in the right tab.
*)

module SimulationView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open NumberHelpers
open Helpers
open JSHelpers
open DiagramStyle
open PopupView
open DiagramMessageType
open DiagramModelType
open DiagramTypes
open SimulatorTypes
open Extractor
open Simulator
open NumberHelpers

let private viewSimulationInputs
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
                        feedSimulationInput simulationGraph (ComponentId inputId) [newBit]
                        |> SetSimulationGraph |> dispatch
                    )
                ] [ str <| bitToString bit ]
            | bits ->
                let bitsStr = bitsToString bits
                Input.text [
                    Input.DefaultValue bitsStr
                    Input.Props [
                        simulationNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match text.Length with
                            | l when l > width ->
                                let err = sprintf "Too many bits. The input expects %d bits, but %d were given." width l
                                errorNotification err CloseSimulationNotification
                                |> SetSimulationNotification |> dispatch
                            | _ ->
                                let maybeBits = padBitsToWidth width text |> stringToBits
                                match maybeBits with
                                | None ->
                                    let err = sprintf "Invalid bits sequence. The only characters allowed are 0 and 1."
                                    errorNotification err CloseSimulationNotification
                                    |> SetSimulationNotification |> dispatch
                                | Some bits ->
                                    // Close simulation notifications.
                                    CloseSimulationNotification |> dispatch
                                    feedSimulationInput simulationGraph (ComponentId inputId) bits
                                    |> SetSimulationGraph |> dispatch
                        ))
                    ]
                ]
        let labelText = match width with
                        | 1 -> inputLabel
                        | w -> sprintf "%s (%d bits)" inputLabel w
        Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
            Level.left [] [
                Level.item [] [ str labelText ]
            ]
            Level.right [] [
                Level.item [] [ valueHandle ]
            ]
        ]
    let inputLines =
        // Sort inputs by label.
        inputs
        |> List.sortBy (fun ((_, ComponentLabel label, _), _) -> label)
        |> List.map makeInputLine
    div [] inputLines

let private viewSimulationOutputs (simOutputs : (SimulationIO * WireData) list) =
    let makeOutputLine ((ComponentId _, ComponentLabel outputLabel, width), wireData) =
        assertThat (List.length wireData = width)
        <| sprintf "Inconsistent wireData length in viewSimulationOutput for %s: expcted %d but got %d" outputLabel width wireData.Length
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation output."
            | [bit] ->
                // For simple bits, just have a Zero/One button.
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    Button.Color IsPrimary
                    (match bit with Zero -> Button.IsOutlined | One -> Button.Color IsPrimary)
                    Button.IsHovered false
                    Button.Disabled true
                ] [ str <| bitToString bit ]
            | bits ->
                let bitsStr = bitsToString bits
                Input.text [
                    Input.IsReadOnly true
                    Input.Value bitsStr
                    Input.Props [simulationNumberStyle]
                ]
        let labelText = match width with
                        | 1 -> outputLabel
                        | w -> sprintf "%s (%d bits)" outputLabel w
        Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
            Level.left [] [
                Level.item [] [ str labelText ]
            ]
            Level.right [] [
                Level.item [] [ valueHandle ]
            ]
        ]
    div [] (
        simOutputs
        |> List.sortBy (fun ((_, ComponentLabel label, _), _) -> label)
        |> List.map makeOutputLine
    )

let private viewSimulationError (simError : SimulationError) =
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

let private viewSimulationData (simData : SimulationData) dispatch =
    let maybeClockTickBtn =
        match simData.IsSynchronous with
        | false -> div [] []
        | true ->
            Button.button [
                Button.OnClick (fun _ ->
                    feedClockTick simData.Graph |> SetSimulationGraph |> dispatch
                )
            ] [ str "Clock Tick" ] 
    div [] [
        maybeClockTickBtn
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewSimulationInputs
            simData.Graph
            (extractSimulationIOs simData.Inputs simData.Graph)
            dispatch
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Outputs" ]
        viewSimulationOutputs <| extractSimulationIOs simData.Outputs simData.Graph
    ]

let viewSimulation model dispatch =
    let startSimulation () =
        match model.Diagram.GetCanvasState (), model.CurrProject with
        | None, _ -> ()
        | _, None -> failwith "what? Cannot start a simulation without a project"
        | Some jsState, Some project ->
            let otherComponents =
                project.LoadedComponents
                |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            (extractState jsState, otherComponents)
            ||> prepareSimulation project.OpenFileName
            |> function
               | Ok simData -> Ok simData
               | Error simError ->
                  if simError.InDependency.IsNone then
                      // Highligh the affected components and connection only if
                      // the error is in the current diagram and not in a
                      // dependency.
                      (simError.ComponentsAffected, simError.ConnectionsAffected)
                      |> SetHighlighted |> dispatch
                  Error simError
            |> StartSimulation
            |> dispatch
    match model.Simulation with
    | None ->
        div [] [
            Button.button
                [ Button.Color IsSuccess; Button.OnClick (fun _ -> startSimulation()) ]
                [ str "Start simulation" ]
        ]
    | Some sim ->
        let body = match sim with
                   | Error simError -> viewSimulationError simError
                   | Ok simData -> viewSimulationData simData dispatch
        let endSimulation _ =
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
