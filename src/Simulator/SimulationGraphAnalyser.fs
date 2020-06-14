module SimulationGraphAnalyser

open CommonTypes
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
/// TODO: this has to keep track of the input port. Need a mapping from
/// inputportnumber to a list of outputportnumber, corresponding to
/// combinatorially connected connections.
/// Visited has to become a set of ComponentId * (option InputPortNumber). The
/// option is always None except for custom components.
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
    // TODO: this has to return no for all custom components. Use
    // couldBeCombinatorial instead.
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
            // TODO: If custom component, need to lookup the combinatorial
            // outputs connected to the input port, rather than all the outputs.
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

/// Check that the combinatorial logic contains no cycles, in a given graph.
/// If connections are passed, a possible error message will contain the
/// connections affected.
let private checkCombinatorialCycle
        (graph : SimulationGraph)
        (connectionsOpt : (Connection list) option)
        (inDependency : string option)
        (isSynchronous : ComponentType -> bool)
        : SimulationError option =
    let rec checkGraphForest nodeIds visited =
        match nodeIds with
        | [] -> None
        | nodeId :: nodeIds' ->
            match dfs nodeId graph visited Set.empty isSynchronous with
            | NoCycle visited -> checkGraphForest nodeIds' visited
            | Cycle cycle ->
                let connectionsAffected =
                    match connectionsOpt with
                    | None -> []
                    | Some conns -> calculateConnectionsAffected conns cycle
                let containsCombinatorialCustomComponent =
                    cycle
                    |> List.map (getNodeOrFail graph)
                    |> List.filter (fun comp -> isCustom comp.Type &&
                                                not <| isSynchronous comp.Type)
                    |> List.isEmpty |> not
                let extraMsg =
                    if containsCombinatorialCustomComponent
                    then " The cycle contains at least one combinatorial custom
                           component. Note that a custom component is considered
                           combinatorial if there is at least one combinatorial
                           path from input to output (i.e. at least one path
                           from input to output that encounters no clocked
                           component)."
                    else ""
                Some {
                    Msg = "Cycle detected in combinatorial logic." + extraMsg
                    InDependency = inDependency
                    ComponentsAffected = cycle
                    ConnectionsAffected = connectionsAffected
                }
            | Backtracking (c, ce) -> failwithf "what? Dfs should never terminate while backtracking: %A" (c, ce)

    let visited = Set.empty
    let allIds = graph |> Map.toList |> List.map (fun (id, _) -> id)
    checkGraphForest allIds visited

/// Recursively make sure that there are no combinatorial loops in any of the
/// dependencies of a graph, and in the graph itself.
/// Keep track of the alreadyChecked graphs in order to waste time uselessly as
/// the same component may be used multiple times.
let rec private recursivelyCheckCombinatorialCycles
        (currGraph : SimulationGraph)
        (connectionsOpt : (Connection list) option)
        (dependencyName : string)
        (alreadyChecked : Set<string>)
        (isSynchronous : ComponentType -> bool)
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
                childGraph None childName alreadyChecked isSynchronous
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
                                    inDependency isSynchronous
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
    match makeIsCustomComponentCombinatorialMap diagramName graph with
    | None ->
        // It was not possible to infer wether custom components are
        // combinatorial or synchronous, probably due to a cyclic dependency.
        // The dependency merger should have already errored if there were such
        // problems.
        failwithf "what? makeIsCustomComponentCombinatorialMap returned None whithin analyseSimulationGraph. This should never happen"
    | Some icccm ->
        // icccm: is custom component combinatorial? map
        // This is a map that states whether a custom component is considered
        // combinatorial or synchronous.
        // The isSynchronous function uses icccm to determine whether a
        // component is synchronous or not.
        let isSynchronous = isSynchronousComponent icccm
        recursivelyCheckCombinatorialCycles graph (Some connections) diagramName
                                            Set.empty isSynchronous
        |> function
        | Ok _ -> None
        | Error err -> Some err
