(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module SimulationRunner

open DiagramTypes

// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Take the Input, and feed it to the Component with the specified Id.
/// If the Component is then ready to produce an output, propagate this output
/// by recursively feeding it as an input to the connected Components.
let rec private feedInput
        (graph : SimulationGraph)
        (compId : ComponentId)
        (input : InputPortNumber * Bit)
        : SimulationGraph =
    // Extract component.
    let comp = match graph.TryFind compId with
               | None -> failwithf "what? Could not find component %A in simulationStep" compId
               | Some c -> c
    // Add input to the simulation component.
    let comp = { comp with Inputs = comp.Inputs.Add input }
    let graph = graph.Add (comp.Id, comp)
    // Try to reduce the component.
    match comp.Reducer comp.Inputs with
    | None -> graph // Keep on waiting for more inputs.
    | Some outputMap ->
        // Received enough inputs and produced an output.
        // Propagate each output produced to all the ports connected.
        (graph, outputMap) ||> Map.fold (fun graph outPortNumber bit ->
            match comp.Outputs.TryFind outPortNumber with
            | None -> failwithf "what? Reducer produced inexistent output portNumber %A in component %A" outPortNumber comp
            | Some targets ->
                // Trigger simulation step with the newly produced input in
                // every target.
                (graph, targets) ||> List.fold (fun graph (nextCompId, nextPortNumber) ->
                    feedInput graph nextCompId (nextPortNumber, bit)
                )
        )

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId bit =
    feedInput graph inputId (InputPortNumber 0, bit)

/// Feed zeros to all simulation inputs.
let simulateWithAllInputsToZero
        (inputIds : SimulationIO list)
        (graph : SimulationGraph)
        : SimulationGraph =
    (graph, inputIds) ||> List.fold (fun graph (inputId, _) ->
        feedSimulationInput graph inputId Zero
    )
