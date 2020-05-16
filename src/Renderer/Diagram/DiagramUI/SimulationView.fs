(*
    SimulationView.fs

    View for simulation in the right tab.
*)

module SimulationView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open DiagramMessageType
open DiagramModelType
open DiagramTypes
open SimulatorTypes
open Extractor
open Simulator

let private viewSimulationInputs
        (simulationGraph : SimulationGraph)
        (inputs : (SimulationIO * WireData) list)
        dispatch =
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel), wireData) =
        // TODO support buses as numbers.
        assertThat (List.length wireData = 1) "Inputs supported are only bits for now"
        let bit = wireData.[0]
        let bitButton =
            Button.button [
                Button.Props [ Style [ Width "50px" ] ]
                Button.Color IsPrimary
                (match bit with Zero -> Button.IsOutlined | One -> Button.Color IsPrimary)
                Button.IsHovered false
                Button.Size IsSmall
                Button.OnClick (fun _ ->
                    let newBit = match bit with
                                 | Zero -> One
                                 | One -> Zero
                    feedSimulationInput simulationGraph (ComponentId inputId) [newBit]
                    |> SetSimulationGraph |> dispatch
                )
            ] [ str (match bit with Zero -> "0" | One -> "1") ]
        Level.level [] [
            Level.left [] [
                Level.item [] [ str inputLabel ]
            ]
            Level.right [] [
                Level.item [] [ bitButton ]
            ]
        ]
    div [] (
        // Sort inputs by label.
        inputs
        |> List.sortBy (fun ((_, ComponentLabel label), _) -> label)
        |> List.map makeInputLine
    )

let private viewSimulationOutputs (simOutputs : (SimulationIO * WireData) list) =
    div [] (
        simOutputs
        |> List.collect (fun ((_, ComponentLabel outputLabel), bit) ->
            [ str <| sprintf "%s\t%A" outputLabel bit; br [] ]
        )
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
    div [] [
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
                  // Highligh the affected componetns if error.
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
            body
        ]
