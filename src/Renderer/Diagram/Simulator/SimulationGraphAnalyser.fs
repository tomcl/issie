module SimulationGraphAnalyser

open DiagramTypes
open SimulatorTypes
open SynchronousUtils

// Checks performed:
// - Combinatorial logic can have no loops.

type private DfsType =
    // No cycle detected in the subtree. Return the new visited set and keep
    // on exploring.
    | NoCycle of Set<ComponentId>
    // Found a cycle and bactracking to record all components that form the
    // cycle. Stop recording when the componentId that closes the loop is
    // reached.
    | Backtracking of ComponentId list * ComponentId
    // Done with backtracking. A cycle has been found and all the components
    // that form the cycle have been recorded.
    | Cycle of ComponentId list

/// Dfs function that spots combinatorial cycles in a graph.
let rec private dfs
        (currNodeId : ComponentId)
        (graph : SimulationGraph)
        (visited : Set<ComponentId>)
        (currStack : Set<ComponentId>)
        (isSynchronous : ComponentType -> bool)
        : DfsType =
    let rec exploreChildren visited currStack children : DfsType =
        match children with
        | [] -> NoCycle visited
        | child :: children' ->
            match dfs child graph visited currStack isSynchronous with
            | NoCycle visited ->
                // Keep on exploring other children.
                exploreChildren visited currStack children'
            | Backtracking (cycle, cycleEnd) ->
                match cycleEnd = currNodeId with
                | true -> Cycle cycle
                | false -> Backtracking (currNodeId :: cycle, cycleEnd)
            | Cycle cycle ->
                Cycle cycle

    // Extract node.
    let currNode =
        match graph.TryFind currNodeId with
        | None -> failwithf "what? Could not find component %A in cycle detection" currNodeId
        | Some c -> c
    match isSynchronous currNode.Type with
    | true ->
        // If the node is a synchronous component, it cannot be part of a
        // combinatorial cycle.
        NoCycle visited
    | false ->
        // Combinational components may form cycles, hence proceed with
        // the DFS.
        match currStack.Contains currNodeId, visited.Contains currNodeId with
        | true, true ->
            // Already visited in this subtree: cycle detected.
            Backtracking ([currNodeId], currNodeId)
        | false, true ->
            // Already visited, and this node is part of no cycles.
            NoCycle visited
        | false, false ->
            // New node.
            let visited = visited.Add currNodeId
            let currStack = currStack.Add currNodeId
            currNode.Outputs
            |> Map.toList
            |> List.collect // Extract all children Ids for all of the ports.
                (fun (_, portChildren) ->
                    portChildren |> List.map (fun (childId, _) -> childId))
            |> exploreChildren visited currStack
        | true, false ->
            // A node in the stack must always be visited.
            failwithf "what? Node never visited but in the stack, while detecting cycle: %A" currNodeId

// TODO with clocked components (which can have cycles) you can extend this
// algorithm by just ignoring a node if it is clocked.

/// Calculate which connections are involved in a cycle.
let private calculateConnectionsAffected
        (connections : Connection list) 
        (cycle : ComponentId list)
        : ConnectionId list =
    let rec findConnection connections (compIdFrom, compIdTo) : ConnectionId =
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
    |> List.mapi (fun i compId -> (compId, cycle.[(i + 1) % cycle.Length]))
    |> List.map (findConnection connections)

/// Check that the combinatorial logic contains no cycles.
let private checkCombinatorialCycle
        (graph : SimulationGraph)
        (connections : Connection list)
        (isSynchronous : ComponentType -> bool)
        : SimulationError option =
    let rec checkGraphForest nodeIds visited =
        match nodeIds with
        | [] -> None
        | nodeId :: nodeIds' ->
            match dfs nodeId graph visited Set.empty isSynchronous with
            | NoCycle visited -> checkGraphForest nodeIds' visited
            | Cycle cycle -> Some {
                Msg = "Cycle detected in combinatorial logic"
                InDependency = None
                ComponentsAffected = cycle
                ConnectionsAffected = calculateConnectionsAffected connections cycle }
            | Backtracking (c, ce) -> failwithf "what? Dfs should never terminate while backtracking: %A" (c, ce)

    let visited = Set.empty
    let allIds = graph |> Map.toList |> List.map (fun (id, _) -> id)
    checkGraphForest allIds visited

/// Analyse a SimulationGraph and return any error (or None).
/// The SimulationGraph should be fully merged with its dependency, so this
/// function has to be called after the dependency merger has finished.
/// This function assumes that there are no cyclic dependencies.
let analyseSimulationGraph
        (diagramName : string)
        (graph : SimulationGraph)
        (connections : Connection list)
        : SimulationError option =
    match makeIsCustomComponentCombinatorialMap diagramName graph with
    | None ->
        // It was not possible to infer wether custom components are
        // combinatorial or synchronous, probably due to a cyclic dependency.
        // The dependency merger should have already errored if there were such
        // problems.
        failwithf "what? makeIsCustomComponentCombinatorialMap returned None whithin analyseSimulationGraph. This should never happen"
    | Some icccm ->
        // icccm: is custom component combinatorial map. This is a map that
        // states whether a custom component is considered combinatorial or
        // synchronous.
        // The isSynchronous function uses icccm to determine whether a
        // component is synchronous or not.
        let isSynchronous = isSynchronousComponent icccm
        checkCombinatorialCycle graph connections isSynchronous

// TODO recursively check all nested components for combinatorial cycles.
