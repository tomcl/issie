(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module SimulationRunner

open DiagramTypes
open SimulatorTypes

// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Take the Input, and feed it to the Component with the specified Id.
/// If the Component is then ready to produce an output, propagate this output
/// by recursively feeding it as an input to the connected Components.
let rec private feedInput
        (graph : SimulationGraph)
        (compId : ComponentId)
        (input : InputPortNumber * WireData)
        : SimulationGraph =
    // Extract component.
    let comp = match graph.TryFind compId with
               | None -> failwithf "what? Could not find component %A in simulationStep" compId
               | Some c -> c
    // Add input to the simulation component.
    let comp = { comp with Inputs = comp.Inputs.Add input }
    let graph = graph.Add (comp.Id, comp)
    // Try to reduce the component.
    match comp.Reducer comp.Inputs comp.CustomSimulationGraph with
    | None, _ -> graph // Keep on waiting for more inputs.
    | Some outputMap, updatedCustomSimulationGraph ->
        // Received enough inputs and produced an output.
        // Propagate each output produced to all the ports connected.
        let graph =
            (graph, outputMap) ||> Map.fold (fun graph outPortNumber wireData ->
                match comp.Outputs.TryFind outPortNumber with
                | None -> failwithf "what? Reducer produced inexistent output portNumber %A in component %A" outPortNumber comp
                | Some targets ->
                    // Trigger simulation step with the newly produced input in
                    // every target.
                    (graph, targets) ||> List.fold (fun graph (nextCompId, nextPortNumber) ->
                        feedInput graph nextCompId (nextPortNumber, wireData)
                    )
            )
        // Update the CustomSImulationGraph.
        let comp = { comp with CustomSimulationGraph = updatedCustomSimulationGraph }
        graph.Add (comp.Id, comp)

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId wireData =
    feedInput graph inputId (InputPortNumber 0, wireData)

/// Feed zeros to all simulation inputs.
let simulateWithAllInputsToZero
        (inputIds : SimulationIO list)
        (graph : SimulationGraph)
        : SimulationGraph =
    // TODO: need to know all of the sizes of buses?
    //       For now only feed single bit inputs.
    (graph, inputIds) ||> List.fold (fun graph (inputId, _) ->
        feedSimulationInput graph inputId [Zero]
    )

/// Given a list of IO nodes (i.e. Inputs or outputs) extract their value.
/// If they dont all have a value, an error is thrown.
let extractSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> ((ioId, ioLabel), extractWireData comp.Inputs) :: result
    )

/// Simlar to extractSimulationIOs, but do not fail if a bit is not set, just
/// ignore it.
let extractIncompleteSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * WireData) list =
    let extractWireData (inputs : Map<InputPortNumber, WireData>) : WireData option =
        inputs.TryFind <| InputPortNumber 0
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> match extractWireData comp.Inputs with
                       | None -> result
                       | Some wireData -> ((ioId, ioLabel), wireData) :: result
    )

/// Get the ComponentIds and ComponentLabels of all input and output nodes.
let getSimulationIOs
        (components : Component list)
        : SimulationIO list * SimulationIO list =
    (([], []), components) ||> List.fold (fun (inputs, outputs) comp ->
        match comp.Type with
        // TODO: add the width to simulationIO?
        | Input _  -> ((ComponentId comp.Id, ComponentLabel comp.Label) :: inputs, outputs)
        | Output _ -> (inputs, (ComponentId comp.Id, ComponentLabel comp.Label) :: outputs)
        | _ -> (inputs, outputs)
    )

/// Get the ComponentIds and ComponentLabels of all input and output nodes.
let getSimulationIOsFromGraph
        (graph : SimulationGraph)
        : SimulationIO list * SimulationIO list =
    (([], []), graph) ||> Map.fold (fun (inputs, outputs) compId comp ->
        match comp.Type with
        // TODO: add the width to simulationIO?
        | Input _  -> ((comp.Id, comp.Label) :: inputs, outputs)
        | Output _ -> (inputs, (comp.Id, comp.Label) :: outputs)
        | _ -> (inputs, outputs)
    )
