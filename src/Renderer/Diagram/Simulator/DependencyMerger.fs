(*
    DependencyMerger.fs

    This module collects functions that allow to validate and merge all the
    dependencies of a SimulationGraph.
*)

module DependencyMerger

open DiagramTypes
open SimulationRunner
open SimulationBuilder

/// Map a dependency name to its simulation graph.
type private DependencyMap = Map<string, SimulationGraph>

/// Validate and get simulation graph for all loaded dependencies.
let private buildDependencyMap
        (loadedDependencies : LoadedComponent list)
        : Result<DependencyMap, SimulationError> =
    let dependenciesRes =
        loadedDependencies
        |> List.map (fun dep -> dep.Name, runChecksAndBuildGraph dep.CanvasState)
    // Check if any dependency has given an error.
    let hasError (name, res) = match res with | Error _ -> true | Ok _ -> false
    let extractOk (name, res) = match res with | Ok d -> name, d | Error e -> failwithf "what? Dependency %s expected to be Ok, but has error %A" name e
    match List.tryFind hasError dependenciesRes with
    | Some (name, Error err) -> Error err // TODO: augument error saying that it happened in a dependency, so no affected components or connections will be highlighted.
    | None ->
        // All dependencies are Ok.
        // Create a map from their name to their simulation graph.
        dependenciesRes |> List.map extractOk |> Map.ofList |> Ok
    | _ -> failwith "what? Impossible case in buildDependencyMap"

/// Convert the label of a port on a custom componetnt to its port number.
let private labelToPortNumber label (outputLabels : string list) =
    match List.tryFindIndex ((=) label) outputLabels with
    | None -> failwithf "what? Label %s not present in %A" label outputLabels
    | Some pNumber -> OutputPortNumber pNumber

/// Convert the portNumber of a custom componetnt to its port lablel.
let private portNumberToLabel (InputPortNumber pNumber) (inputLabels : string list) =
    // TODO: assert lenght?
    inputLabels.[pNumber]

/// Find the ComponentId of the Input node with a given label.
let private findInputNodeWithLabel graph label : ComponentId =
    let chooser compId comp =
        if comp.Type = Input && comp.Label = label
        then Some compId
        else None
    // TODO: I have the feeling Map.tryPick has linear complexity. Maybe
    // premapping all these information can improve performance.
    match Map.tryPick chooser graph with
    | None -> failwithf "what? Input node with label %s could not be found in graph" label
    | Some compId -> compId

/// Extract pairs label * value for all outputs of the SimulationGraph.
let private extractOutputs (graph : SimulationGraph) : (string * Bit) list =
    let extractBit (inputs : Map<InputPortNumber, Bit>) : Bit =
        match inputs.TryFind <| InputPortNumber 0 with
        | None -> failwith "what? IO bit not set"
        | Some bit -> bit
    graph
    |> Map.filter (fun compId comp -> match comp.Type with | Output -> true | _ -> false )
    |> Map.map (fun compId comp -> comp.Label, extractBit comp.Inputs)
    |> Map.toList
    |> List.map (fun (compId, res) -> res)

/// Create the Reducer for a custom component.
let private makeCustomReducer
        (custom : CustomComponentType)
        (graph : SimulationGraph)
        : Map<InputPortNumber, Bit> -> Map<OutputPortNumber, Bit> option =
    fun inputs ->
        match inputs.Count = custom.InputLabels.Length with
        | false -> None // Not enough inputs.
        | true ->
            // TODO: feed only new inputs or inputs that changed, for performance.
            //       for now, we feed all the inputs.
            let graph =
                (graph, inputs)
                ||> Map.fold (fun graph inputPortNumber bit ->
                    let inputId =
                        portNumberToLabel inputPortNumber custom.InputLabels
                        |> findInputNodeWithLabel graph
                    feedSimulationInput graph inputId bit
                )
            extractOutputs graph
            |> List.map (fun (label, bit) -> labelToPortNumber label custom.OutputLabels, bit)
            |> Map.ofList
            |> Some

/// Recursively merge the simulationGraph with its dependencies (a dependecy can
/// have its own dependencies).
let rec private merger
        (currGraph : SimulationGraph)
        (dependencyMap : DependencyMap)
        : SimulationGraph =
    // For each custom component, replace the Reducer with one that:
    // - when receiving an (InputPortNumber * Bit) entry (i.e. a new input),
    //   maps the InputPortNumber to the its label.
    // - find the Input node in the dependency simulationGraph with that label.
    // - feed the bit to that Input node.
    // - extracts the outputs.
    // - map the output labels to OutputPortNumbers, and this is the output of
    //   the reducer function.
    //
    // A dependency may have dependencies itself, so recursively call the merger
    // as well.
    let currGraphCopy = currGraph
    (currGraph, currGraphCopy)
    ||> Map.fold (fun currGraph compId comp ->
        match comp.Type with
        | Custom custom ->
            let dependencyGraph =
                match dependencyMap.TryFind custom.Name with
                | None -> failwithf "what? Could not find dependency %s in dependencyMap" custom.Name
                | Some dependencyGraph -> dependencyGraph
            let dependencyGraph = merger dependencyGraph dependencyMap
            let newComp = { comp with Reducer = makeCustomReducer custom dependencyGraph }
            currGraph.Add(compId, newComp)
        | _ -> currGraph // Ignore non-custom components.
    )

/// Try to resolve all the dependencies in a graph, and replace the reducer
/// of the custom components with a simulationgraph.
/// For example, if the graph of an ALU refers to custom component such as
/// adders, replace them with the actual simulation graph for the adders.
let mergeDependencies
        (graph : SimulationGraph)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationGraph, SimulationError> =
    // TODO: only build dependencyMap for the necessary dependencies.
    match buildDependencyMap loadedDependencies with
    | Error e -> Error e
    | Ok dependencyMap ->
        // Recursively replace the dependencies, in a top down fashion.
        Ok <| merger graph dependencyMap
