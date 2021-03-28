(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open SimulationBuilder
open SimulationRunner
open DependencyMerger
open SimulationGraphAnalyser

// Simulating a circuit has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...
// 4. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

/// Builds the graph and simulates it with all inputs zeroed.


let rec prepareSimulation
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =

    /// Tune for performance of initial zero-length simulation versus longer run.
    /// Probably this is not critical.
    let initMaxSteps = 25
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph
                                canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it.
            let components, connections = canvasState
            let inputs, outputs = getSimulationIOs components
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None -> 
                try
                    Ok {
                        FastSim = Fast.buildFastSimulation initMaxSteps graph
                        Graph = graph |> InitialiseGraphWithZeros inputs;
                        Inputs = inputs;
                        Outputs = outputs
                        IsSynchronous = hasSynchronousComponents graph
                        NumberBase = Hex
                        ClockTickNumber = 0
                    }
                with
                | e -> 
                    printfn "\nEXCEPTION:\n\n%A\n%A" e.Message e.StackTrace
                    Error {
                        Msg = sprintf "\nInternal ERROR in Issie fast simulation:\n\n%A\n%A\n" e.Message e.StackTrace
                        InDependency = None
                        ComponentsAffected = []
                        ConnectionsAffected = []
                    }
                |> Result.map (fun sd -> (Fast.compareFastWithGraph sd |> ignore); sd)



/// Expose the feedSimulationInput function from SimulationRunner.
let feedSimulationInput = SimulationRunner.feedSimulationInput

/// Expose the feedClockTick function from SimulationRunner.
let feedClockTick = SimulationRunner.feedClockTick

/// Expose the extractSimulationIOs function from SimulationRunner.
let extractSimulationIOs = SimulationRunner.extractSimulationIOs

/// Get some info and the state of all stateful components in a graph.
let extractStatefulComponents
        (graph : SimulationGraph)
        : SimulationComponent list =
    graph
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp -> comp.State <> NoState)
    // TODO: recursively search custom components?


