(*
    Analyser.fs

    This module collects a series of functions that perform checks on
    CanvasState and SimulationGraph.
*)

module Analyser

open DiagramTypes

// Ports constraints:
// - Source ports must be output ports.
// - Target ports must be input ports.
// - All ports have at least one connection that touches them.
// - Input ports have precisely one connection that touches them.

// Combinatorial logic can have no loops.

/// Check that:
/// 1- all source ports in connections are Output ports,
/// 2- all target ports in connections are Input ports,
/// 3- all input ports in a component are actually input ports,
/// 4- all output ports in a component are actually output ports,
/// 5- all ports on components have a port number,
/// 6- all ports on connection do not have a port number.
/// These conditions should always hold, unless there are bugs in the code (i.e.
/// no user behaviour should be able to trigger such errors).
/// The costruction of the Simulation graph assumes that these rules hold.
/// TODO: should they crash the program then?
let checkPortTypesAreConsistent (canvasState : CanvasState) : SimulationError option =
    let rec checkComponentPorts (ports : Port list) (correctType : PortType) =
        match ports with
        | [] -> None
        | port :: _ when port.PortNumber = None -> Some {
            Msg = sprintf "%A port appears to not have a port number" correctType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | port :: _ when port.PortType <> correctType -> Some {
            Msg = sprintf "%A port %d appears to be an %A port" correctType (Option.get port.PortNumber) port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | _ :: ports' ->
            checkComponentPorts ports' correctType
    /// Check conditions 3, 4, 5
    let rec checkComponentsPorts (components : Component list) =
        match components with
        | [] -> None
        | comp :: components' ->
            match checkComponentPorts comp.InputPorts PortType.Input,
                  checkComponentPorts comp.OutputPorts PortType.Output with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkComponentsPorts components' // Check next.

    let checkConnectionPort (port : Port) (correctType : PortType) (connId : string) =
        match port.PortType = correctType, port.PortNumber with
        | false, _ -> Some {
            Msg = sprintf "%A port appears to be an %A port" correctType port.PortType
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | _, Some pNumber -> Some {
            Msg = sprintf "%A port appears to have a port number: %d" correctType pNumber
            InDependency = None
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | true, None -> None // All right.
    /// Check conditions 1, 2, 6
    let rec checkConnectionsPorts (connections : Connection list) =
        match connections with
        | [] -> None
        | conn :: connections' ->
            match checkConnectionPort conn.Source PortType.Output conn.Id,
                  checkConnectionPort conn.Target PortType.Input conn.Id with
            | Some err, _ | _, Some err -> Some err
            | None, None -> checkConnectionsPorts connections' // Check next.

    let components, connections = canvasState
    match checkComponentsPorts components,
          checkConnectionsPorts connections with
    | Some err, _ | _, Some err -> Some err
    | None, None -> None // All right.

/// Return all the Ids of all input ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllInputPortIds (components : Component list) : (InputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.InputPorts |> List.map (fun port -> InputPortId port.Id, ComponentId comp.Id))

/// Return all the Ids of all ouput ports across all components.
/// Return also the ComponentId which may be used in error messages.
let private getAllOutputPortIds (components : Component list) : (OutputPortId * ComponentId) list =
    components |> List.collect
        (fun comp -> comp.OutputPorts |> List.map (fun port -> OutputPortId port.Id, ComponentId comp.Id))

/// Count the number of connections that target each port.
let rec private countPortsConnections
        (connections : Connection list)
        (inputCounts : Map<InputPortId * ComponentId, int>)
        (outputCounts : Map<OutputPortId * ComponentId, int>)
        : Result< Map<InputPortId * ComponentId, int> * Map<OutputPortId * ComponentId, int>, SimulationError> =
    match connections with
    | [] -> Ok (inputCounts, outputCounts)
    | conn :: connections' ->
        let sourceId = OutputPortId conn.Source.Id
        let targetId = InputPortId conn.Target.Id
        let sourceHostId = ComponentId conn.Source.HostId
        let targetHostId = ComponentId conn.Target.HostId
        let outputCountsRes =
            match outputCounts.TryFind (sourceId, sourceHostId) with
            | None -> failwithf "Connection refers to a source port that does not exist: %s" conn.Source.Id
            | Some count -> Ok <| outputCounts.Add ((sourceId, sourceHostId), count + 1)
        let inputCountsRes =
            match inputCounts.TryFind (targetId, targetHostId) with
            | None -> failwithf "what? Connection refers to a target port that does not exist: %s" conn.Target.Id
            | Some count -> Ok <| inputCounts.Add ((targetId, targetHostId), count + 1)
        match inputCountsRes, outputCountsRes with
        | Error err, _ | _, Error err -> Error err
        | Ok inputCounts, Ok outputCounts ->
            countPortsConnections connections' inputCounts outputCounts

/// Apply condition on evaery element of the map (tailored to this specific
/// problem).
let private checkEvery
        (counts : Map<'a * ComponentId, int>) // 'a is either InputPortId or OutputPortId.
        (cond : int -> bool)
        errMsg
        : SimulationError option =
    (None, counts) ||> Map.fold (fun maybeErr (_, componentId) count ->
        match maybeErr with
        | Some err -> Some err
        | None ->
            // Return special error message if there are zero connections.
            match cond count with
            | true -> None
            | false when count = 0 -> Some {
                Msg = "All ports must have at least one connection."
                InDependency = None
                ComponentsAffected = [componentId]
                ConnectionsAffected = [] }
            | false -> Some {
                Msg = sprintf errMsg count 
                InDependency = None
                ComponentsAffected = [componentId]
                ConnectionsAffected = [] }
    )

/// Check that:
/// - any port has at least one connection,
/// - any input port has precisely one connection.
/// These conditions may not hold due to user errors.
let checkPortsAreConnectedProperly
        (canvasState : CanvasState)
        : SimulationError option =
    let components, connections = canvasState
    let inputCounts =
        getAllInputPortIds components
        |> List.map (fun portId -> portId, 0)
        |> Map.ofList
    let outputCounts =
        getAllOutputPortIds components
        |> List.map (fun portId -> portId, 0)
        |> Map.ofList
    match countPortsConnections connections inputCounts outputCounts with
    | Error err -> Some <| err
    | Ok (inputCounts, outputCounts) ->
        let inputRes = checkEvery inputCounts ((=) 1) "Input port receives %d connections. An input port should receive precisely one connection."
        let outputRes = checkEvery outputCounts ((<=) 1) "Output port receives an unexpected number of connections: %d"
        match inputRes, outputRes with
        | None, None -> None
        | Some err, _ | _, Some err -> Some err

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

/// Dfs function that spots cycles in a graph.
let rec private dfs
        (currNodeId : ComponentId)
        (graph : SimulationGraph)
        (visited : Set<ComponentId>)
        (currStack : Set<ComponentId>)
        : DfsType =
    let rec exploreChildren visited currStack children : DfsType =
        match children with
        | [] -> NoCycle visited
        | child :: children' ->
            match dfs child graph visited currStack with
            | NoCycle visited ->
                // Keep on exploring other children.
                exploreChildren visited currStack children'
            | Backtracking (cycle, cycleEnd) ->
                match cycleEnd = currNodeId with
                | true -> Cycle cycle
                | false -> Backtracking (currNodeId :: cycle, cycleEnd)
            | Cycle cycle ->
                Cycle cycle

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
        let currNode =
            match graph.TryFind currNodeId with
            | None -> failwithf "what? Could not find component %A in cycle detection" currNodeId
            | Some c -> c
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
        : SimulationError option =
    let rec checkGraphForest nodeIds visited =
        match nodeIds with
        | [] -> None
        | nodeId :: nodeIds' ->
            match dfs nodeId graph visited Set.empty with
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

/// Analyse the simulation graph and return any error (or None).
let analyseGraph
        (graph : SimulationGraph)
        (connections : Connection list)
        : SimulationError option =
    checkCombinatorialCycle graph connections
