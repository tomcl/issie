(*
    SimulationView.fs

    View for simulation in the right tab.
*)

module SimulationView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Helpers
open JSHelpers
open DiagramMessageType
open DiagramModelType
open DiagramTypes
open SimulatorTypes
open Extractor
open Simulator

let private bitToString (bit : Bit) : string =
    match bit with Zero -> "0" | One -> "1"

let rec private bitsToString (bits : WireData) : string =
    match bits with
    | [] -> ""
    | bit :: bits' -> (bitToString bit) + (bitsToString bits')

/// Try to convert a string of bits into a Bit list. Return None if such
/// conversion is not possible.
let private stringToBits (bitsStr : string) : WireData option =
    let rec convert (bitChars : char list) : WireData option =
        match bitChars with
        | [] -> Some []
        | bitChar :: bitChars' when bitChar = '0' ->
            Option.map (fun bits -> Zero :: bits) (convert bitChars')
        | bitChar :: bitChars' when bitChar = '1' ->
            Option.map (fun bits -> One :: bits) (convert bitChars')
        | _ -> None
    convert <| Seq.toList bitsStr

let private padBitsToWidth width (bitsStr : string) : string =
    (String.replicate (width - bitsStr.Length) "0") + bitsStr

let private viewSimulationInputs
        (simulationGraph : SimulationGraph)
        (inputs : (SimulationIO * WireData) list)
        dispatch =
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), wireData) =
        let valueHandle =
            match wireData with
            | [] -> failwith "what? Empty wireData while creating a line in simulation inputs."
            | [bit] ->
                // For simple bits, just have a Zero/One button.
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
                ] [ str <| bitToString bit ]
            | bits ->
                let bitsStr = bitsToString bits
                Input.text [
                    Input.Size IsSmall
                    Input.DefaultValue bitsStr
                    Input.Props [
                        Style [Width "100px"]
                        OnBlur (getTextFocusEventValue >> (fun text ->
                            match text.Length with
                            | l when l > width ->
                                // TODO: display error.
                                log <| sprintf "Too many bits. The input expects %d bits, but %d where given" width l
                            | _ ->
                                let maybeBits = padBitsToWidth width text |> stringToBits
                                match maybeBits with
                                | None ->
                                    // TODO: display error.
                                    log <| sprintf "Invalid bits sequence. The only characters allowed are 1 and 0."
                                | Some bits ->
                                    feedSimulationInput simulationGraph (ComponentId inputId) bits
                                    |> SetSimulationGraph |> dispatch
                        ))
                    ]
                ]
        let labelText = match width with
                        | 1 -> inputLabel
                        | w -> sprintf "%s (%d bits)" inputLabel w
        Level.level [] [
            Level.left [] [
                Level.item [] [ str labelText ]
            ]
            Level.right [] [
                Level.item [] [ valueHandle ]
            ]
        ]
    div [] (
        // Sort inputs by label.
        inputs
        |> List.sortBy (fun ((_, ComponentLabel label, _), _) -> label)
        |> List.map makeInputLine
    )

let private viewSimulationOutputs (simOutputs : (SimulationIO * WireData) list) =
    div [] (
        simOutputs
        |> List.collect (fun ((_, ComponentLabel outputLabel, width), wireData) ->
            assertThat (wireData.Length = width) <| sprintf "Inconsistent wireData length in viewSimulationOutput for %s: expcted %d but got %d" outputLabel width wireData.Length
            [ str <| sprintf "%s\t%A" outputLabel wireData; br [] ]
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
