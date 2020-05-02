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
            Outputs = outputs
        }

/// Expose the feedSimulationInput function from SimulationRunner.
let feedSimulationInput = SimulationRunner.feedSimulationInput

/// Expose the extractSimulationIOs function from SimulationRunner.
let extractSimulationIOs = SimulationRunner.extractSimulationIOs
