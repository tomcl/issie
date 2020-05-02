(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open DiagramTypes
open SimulationBuilder
open SimulationRunner
open DependencyMerger

// Simulating a circuit has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...
// 4. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

/// Given a list of IO nodes (i.e. Inputs or outputs) extract their value.
/// If they dont all have a value, an error is thrown.
let extractSimulationIOs
        (simulationIOs : SimulationIO list)
        (graph : SimulationGraph)
        : (SimulationIO * Bit) list =
    let extractBit (inputs : Map<InputPortNumber, Bit>) : Bit =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    ([], simulationIOs) ||> List.fold (fun result (ioId, ioLabel) ->
        match graph.TryFind ioId with
        | None -> failwithf "what? Could not find io node: %A" (ioId, ioLabel)
        | Some comp -> ((ioId, ioLabel), extractBit comp.Inputs) :: result
    )

/// Get the ComponentIds and ComponentLabels of all input and output nodes.
let getSimulationIOs
        (components : Component list)
        : SimulationIO list * SimulationIO list =
    (([], []), components) ||> List.fold (fun (inputIds, outputIds) comp ->
        match comp.Type with
        | Input  -> ((ComponentId comp.Id, ComponentLabel comp.Label) :: inputIds, outputIds)
        | Output -> (inputIds, (ComponentId comp.Id, ComponentLabel comp.Label) :: outputIds)
        | _ -> (inputIds, outputIds)
    )

/// Builds the graph and simulates it with all inputs zeroed.
let prepareSimulation
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =
    match runChecksAndBuildGraph canvasState with
    | Error err -> Error err
    | Ok graph ->
        let components, _ = canvasState
        let inputs, outputs = getSimulationIOs components
        match mergeDependencies graph loadedDependencies with
        | Error err -> Error err
        | Ok graph -> Ok {
            Graph = graph |> simulateWithAllInputsToZero inputs;
            Inputs = inputs;
            Outputs = outputs }

/// Expose the feedSimulationInput function from SimulationRunner.
let feedSimulationInput = SimulationRunner.feedSimulationInput
