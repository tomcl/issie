(*
    DependencyMerger.fs

    This module collects functions that allow to validate and merge all the
    dependencies of a SimulationGraph.
*)

module DependencyMerger

open DiagramTypes
open SimulationRunner
open SimulationBuilder
open Helpers

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
    | Some (name, Error err) ->
        // Augument error saying that it happened in a dependency, so no
        // irrelevant affected components or connections will be highlighted.
        Error { err with InDependency = Some name;
                         ComponentsAffected = [];
                         ConnectionsAffected = [] }
    | None ->
        // All dependencies are Ok.
        // Create a map from their name to their simulation graph.
        dependenciesRes |> List.map extractOk |> Map.ofList |> Ok
    | _ -> failwith "what? Impossible case in buildDependencyMap"

/// Convert the label of a port on a custom componetnt to its port number.
/// Assumes that all labels are unique, otherwise it is undefined behaviour.
let private labelToPortNumber label (labels : string list) =
    match List.tryFindIndex ((=) label) labels with
    | None -> failwithf "what? Label %s not present in %A" label labels
    | Some pNumber -> pNumber

/// Convert the portNumber of a custom componetnt to its port lablel.
let private portNumberToLabel (InputPortNumber pNumber) (inputLabels : string list) =
    assertThat (inputLabels.Length > pNumber) "portNumberToLabel"
    inputLabels.[pNumber]

/// Extract simulation input values as map.
let private extractInputValuesAsMap graph graphInputs inputLabels : Map<InputPortNumber, Bit> =
    extractIncompleteSimulationIOs graphInputs graph
    |> List.map (
        fun ((_, ComponentLabel compLabel), bit) ->
            InputPortNumber <| labelToPortNumber compLabel inputLabels, bit)
    |> Map.ofList

/// Extract simulation output values as map.
let private extractOutputValuesAsMap graph graphOutputs outputLabels : Map<OutputPortNumber, Bit> =
    extractSimulationIOs graphOutputs graph
    |> List.map (
        fun ( (_, ComponentLabel label), bit ) ->
            OutputPortNumber <| labelToPortNumber label outputLabels, bit)
    |> Map.ofList

/// Function used in the custom reducer to only feed the inputs that changed.
let private diffSimulationInputs
        (newInputs : Map<InputPortNumber, Bit>)
        (oldInputs : Map<InputPortNumber, Bit>)
        : Map<InputPortNumber, Bit> =
    // New inputs either:
    // - has more keys than oldInputs,
    // - has the same keys as oldInput, but their values have changed.
    assertThat (oldInputs.Count <= newInputs.Count) "diffSimulationInputs"
    (Map.empty, newInputs)
    ||> Map.fold (fun diff inputPortNumber bit ->
        match oldInputs.TryFind inputPortNumber with
        | None -> diff.Add(inputPortNumber, bit)
        | Some oldBit when oldBit <> bit -> diff.Add(inputPortNumber, bit)
        | Some oldBit when oldBit = bit -> diff
        | _ -> failwith "what? Impossible case in diffSimulationInputs"
    )

/// Create the Reducer for a custom component.
/// Passing graphInputs and graphOutputs would not be strictly necessary, but it
/// is good for performance as so the Input and Output nodes don't have to be
/// searched every time.
let private makeCustomReducer
        (custom : CustomComponentType)
        (graphInputs : SimulationIO list)
        (graphOutputs : SimulationIO list)
        : Map<InputPortNumber, Bit>               // Inputs.
          -> SimulationGraph option               // CustomSimulationGraph.
          -> (Map<OutputPortNumber, Bit> option * // Outputs.
              SimulationGraph option)             // Updated CustomSimulationGraph.
        =
    fun inputs graphOption ->
        let graph = match graphOption with
                    | None -> failwithf "what? CustomSimulationGraph should always be Some in Custom component: %s" custom.Name
                    | Some graph -> graph
        match inputs.Count = custom.InputLabels.Length with
        | false -> None, Some graph // Not enough inputs, return graph unchanged.
        | true ->
            // Feed only new inputs or inputs that changed, for performance.
            let oldInputs =
                extractInputValuesAsMap graph graphInputs custom.InputLabels
            let newInputs = diffSimulationInputs inputs oldInputs
            let graph =
                (graph, newInputs)
                ||> Map.fold (fun graph inputPortNumber bit ->
                    let inputLabel =
                        portNumberToLabel inputPortNumber custom.InputLabels
                    let inputId, _ =
                        graphInputs
                        |> List.find (fun (_, ComponentLabel inpLabel) ->
                                      inpLabel = inputLabel)
                    feedSimulationInput graph inputId bit
                )
            let outputs =
                extractOutputValuesAsMap graph graphOutputs custom.OutputLabels
            // Return the outputs toghether with the updated graph.
            Some outputs, Some graph

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
            // Augment the custom component with the initial
            // CustomSimulationGraph and the Custom reducer (that allows to use
            // and update the CustomSimulationGraph).
            let graphInputs, graphOutputs =
                getSimulationIOsFromGraph dependencyGraph
            let newComp = {
                comp with
                    CustomSimulationGraph = Some dependencyGraph
                    Reducer = makeCustomReducer custom graphInputs graphOutputs
            }
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
