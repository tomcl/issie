(*
    SimulationGraphAnalyser.fs

    This module collects functions to analyse a fully merged simulation graph.
    Analyses performed:
    - Combinatorial logic can have no loops.
*)

module SimulationGraphAnalyser

open CommonTypes
open SimulatorTypes
open SynchronousUtils

type private DfsType =
    // No cycle detected in the subtree. Return the new visited set and keep
    // on exploring.
    | NoCycle of Set<ComponentId * InputPortNumber option>
    // Found a cycle and bactracking to record all components that form the
    // cycle. Stop recording when the componentId that closes the loop is
    // reached.
    | Backtracking of ComponentId list * ComponentId
    // Done with backtracking. A cycle has been found and all the components
    // that form the cycle have been recorded.
    | Cycle of ComponentId list

/// Dfs function that spots combinatorial cycles in a graph.
/// visited and currStack also keep track of the port being explored as it makes
/// a difference (only) in the custom components.
let rec private dfs
        (currNodeId : ComponentId)
        (inputPortNumber : InputPortNumber)
        (graph : SimulationGraph)
        (visited : Set<ComponentId * InputPortNumber option>)
        (currStack : Set<ComponentId * InputPortNumber option>)
        getCombOuts
        : DfsType =
    let rec exploreChildren visited currStack children : DfsType =
        match children with
        | [] -> NoCycle visited
        | (childId, childPNum) :: children' ->
            match dfs childId childPNum graph visited currStack getCombOuts with
            | NoCycle visited ->
                // Keep on exploring other children.
                exploreChildren visited currStack children'
            | Backtracking (cycle, cycleEnd) ->
                match cycleEnd = currNodeId with
                | true -> Cycle cycle
                | false -> Backtracking (currNodeId :: cycle, cycleEnd)
            | Cycle cycle ->
                Cycle cycle

    let currNode = getNodeOrFail graph currNodeId
    // Ignore the info about port number unless node is custom node.
    let inputPortNumber =
        match currNode.Type with
        | Custom _ -> Some inputPortNumber
        | _ -> None
    let curr = currNodeId, inputPortNumber

    // Combinational components may form cycles, hence proceed with
    // the DFS.
    match currStack.Contains curr, visited.Contains curr with
    | true, true ->
        // Already visited in this subtree: cycle detected.
        Backtracking ([currNodeId], currNodeId)
    | false, true ->
        // Already visited, and this node is part of no cycles.
        NoCycle visited
    | false, false ->
        // New node.
        let visited = visited.Add curr
        let currStack = currStack.Add curr
        // Get all of the combinatorial outputs of the node using the function
        // getCombinatorialOutputs (already partially applied).
        getCombOuts currNode inputPortNumber
        |> Map.toList
        // Extract all the children for all the ports.
        |> List.collect (fun (_, portChildren) -> portChildren)
        |> exploreChildren visited currStack
    | true, false ->
        // A node in the stack must always be visited.
        failwithf "what? Node never visited but in the stack, while detecting cycle: %A" currNodeId

/// Calculate which connections are involved in a cycle.
let private calculateConnectionsAffected
        (connections : Connection list) 
        (cycle : ComponentId list)
        : ConnectionId list =
    let rec findConnection (connections: Connection list) (compIdFrom, compIdTo) : ConnectionId =
        match connections with
        | [] -> failwithf "what? Could not find connection among %A and %A" compIdFrom compIdTo
        | conn :: connections' ->
            match ComponentId conn.Source.HostId = compIdFrom,
                  ComponentId conn.Target.HostId = compIdTo with
            | true, true -> ConnectionId conn.Id
            | _ -> findConnection connections' (compIdFrom, compIdTo)
    if cycle.Length < 2 then failwithf "what? Cycle with length less than 2: %A" cycle
    // Find a connection among all pairs.
    cycle
    |> List.mapi (fun i compId -> (compId, cycle[(i + 1) % cycle.Length]))
    |> List.map (findConnection connections)

/// Check that the combinatorial logic contains no cycles, in a given graph.
/// If connections are passed, a possible error message will contain the
/// connections affected.
let private checkCombinatorialCycle
        (graph : SimulationGraph)
        (connectionsOpt : (Connection list) option)
        (inDependency : string option)
        getCombOuts
        : SimulationError option =
    let rec checkGraphForest nodeIdsAndPNums visited =
        match nodeIdsAndPNums with
        | [] -> None
        | (nodeId, pNum) :: nodeIdsAndPNums' ->
            match dfs nodeId pNum graph visited Set.empty getCombOuts with
            | NoCycle visited -> checkGraphForest nodeIdsAndPNums' visited
            | Cycle cycle ->
                let connectionsAffected =
                    match connectionsOpt with
                    | None -> []
                    | Some conns -> 
                        try
                            calculateConnectionsAffected conns cycle
                        with
                            | e -> []
                Some {
                    Msg = "Cycle detected in combinatorial logic."
                    InDependency = inDependency
                    ComponentsAffected = cycle
                    ConnectionsAffected = connectionsAffected
                }
            | Backtracking (c, ce) -> failwithf "what? Dfs should never terminate while backtracking: %A" (c, ce)

    let visited = Set.empty
    let allIdsAndPNums = graph |> Map.toList |> List.collect (fun (id, comp) ->
        match comp.Type with
        | Custom custom ->
            // Explore ever input port of a custom component.
            custom.InputLabels |> List.mapi (fun i _ -> id, InputPortNumber i)
        | _ ->
            // The input port number does not matter for non custom components,
            // it will be ignored in the dfs.
            [id, InputPortNumber 0]
    )
    checkGraphForest allIdsAndPNums visited

/// Recursively make sure that there are no combinatorial loops in any of the
/// dependencies of a graph, and in the graph itself.
/// Keep track of the alreadyChecked graphs in order to waste time uselessly as
/// the same component may be used multiple times.
let rec private recursivelyCheckCombinatorialCycles
        (currGraph : SimulationGraph)
        (connectionsOpt : (Connection list) option)
        (dependencyName : string)
        (alreadyChecked : Set<string>)
        getCombOuts
        : Result<Set<string>, SimulationError> =
    let rec iterateChildren
            (alreadyChecked : Set<string>)
            (children : (ComponentId * SimulationComponent) list) =
        match children with
        | [] -> Ok alreadyChecked
        | (_, child) :: children' ->
            let childGraph = Option.get child.CustomSimulationGraph
            let childName = getCustomName child.Type
            recursivelyCheckCombinatorialCycles
                childGraph None childName alreadyChecked getCombOuts
            |> Result.bind (fun alreadyChecked ->
                iterateChildren alreadyChecked children'
            )

    match alreadyChecked.Contains dependencyName with
    | true -> Ok alreadyChecked // Already checked.
    | false ->
        // Add curr dependency to the already checked set.
        let alreadyChecked = alreadyChecked.Add dependencyName
        // Check all custom components in this graph.
        currGraph
        |> Map.filter (fun compId comp -> isCustom comp.Type)
        |> Map.toList
        |> iterateChildren alreadyChecked
        |> Result.bind (fun alreadyChecked ->
            // Children are fine. Check the current graph.
            // connectionsOpt is populated only for the intial call to this
            // function. In such case, we are not in a dependency, otherwise we
            // are.
            let inDependency = match connectionsOpt with
                               | None -> Some dependencyName
                               | Some _ -> None 
            checkCombinatorialCycle currGraph connectionsOpt
                                    inDependency getCombOuts
            |> function
            | None -> Ok alreadyChecked
            | Some err -> Error err
        )

/// Analyse a SimulationGraph and return any error (or None).
/// The SimulationGraph should be fully merged with its dependency, so this
/// function has to be called after the dependency merger has finished.
/// This function assumes that there are no cyclic dependencies.
let analyseSimulationGraph
        (diagramName : string)
        (graph : SimulationGraph)
        (connections : Connection list)
        : SimulationError option =
    match calculateCustomComponentsCombinatorialPaths diagramName graph with
    | None ->
        // It was not possible to infer combinatorial paths for custom
        // components, probably due to a cyclic dependency.
        // The dependency merger should have already errored if there were such
        // problems.
        failwithf "what? calculateCustomComponentsCombinatorialPaths returned None whithin analyseSimulationGraph. This should never happen"
    | Some cccp ->
        // cccp: custom components combinatorial paths.
        // This is a map that lists all of the combinatorial paths from inputs
        // to outputs for the needed custom components.
        // The getCombinatorialOutputs function uses cccp to determine the
        // combinatorial children of a node.
        let getCombOuts = getCombinatorialOutputs cccp
        recursivelyCheckCombinatorialCycles graph (Some connections) diagramName
                                            Set.empty getCombOuts
        |> function
        | Ok _ -> None
        | Error err -> Some err
