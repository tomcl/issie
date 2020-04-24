module Analyser

open DiagramTypes
open JSHelpers

// Ports constraints:
// - Source ports must be output ports.
// - Target ports must be input ports.
// - All ports have at least one connection that touches them.
// - Input ports have precisely one connection that touches them.

// Check combinatorial loops?

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
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [] }
        | port :: _ when port.PortType <> correctType -> Some {
            Msg = sprintf "%A port %d appears to be an %A port" correctType (Option.get port.PortNumber) port.PortType
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
            ComponentsAffected = [ComponentId port.HostId]
            ConnectionsAffected = [ConnectionId connId] }
        | _, Some pNumber -> Some {
            Msg = sprintf "%A port appears to have a port number: %d" correctType pNumber
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
let private getAllInputPortIds (components : Component list) : InputPortId list =
    components |> List.collect
        (fun comp -> comp.InputPorts |> List.map (fun port -> InputPortId port.Id))

/// Return all the Ids of all ouput ports across all components.
let private getAllOutputPortIds (components : Component list) : OutputPortId list =
    components |> List.collect
        (fun comp -> comp.OutputPorts |> List.map (fun port -> OutputPortId port.Id))

let rec countPortsConnections
        (connections : Connection list)
        (inputCounts : Map<InputPortId, int>)
        (outputCounts : Map<OutputPortId, int>)
        : Result< Map<InputPortId, int> * Map<OutputPortId, int>, SimulationError> =
    match connections with
    | [] -> Ok (inputCounts, outputCounts)
    | conn :: connections' ->
        let sourceId = OutputPortId conn.Source.Id
        let targetId = InputPortId conn.Target.Id
        let outputCountsRes =
            match outputCounts.TryFind sourceId with
            | None -> Error { // This should never happen.
                Msg = sprintf "Connection refers to a source port that does not exist: %s" conn.Source.Id
                ComponentsAffected = [ComponentId conn.Source.HostId]
                ConnectionsAffected = [ConnectionId conn.Id] }
            | Some count -> Ok <| outputCounts.Add (sourceId, count + 1)
        let inputCountsRes =
            match inputCounts.TryFind targetId with
            | None -> Error { // This should never happen.
                Msg = sprintf "Connection refers to a target port that does not exist: %s" conn.Target.Id
                ComponentsAffected = [ComponentId conn.Target.HostId]
                ConnectionsAffected = [ConnectionId conn.Id] }
            | Some count -> Ok <| inputCounts.Add (targetId, count + 1)
        match inputCountsRes, outputCountsRes with
        | Error err, _ | _, Error err -> Error err
        | Ok inputCounts, Ok outputCounts ->
            countPortsConnections connections' inputCounts outputCounts

let private checkEvery
        (counts : Map<'a, int>) // 'a is either InputPortId or OutputPortId.
        (cond : int -> bool)
        (errMsg : string)
        : SimulationError option =
    (None, counts) ||> Map.fold (fun maybeErr portId count ->
        match maybeErr with
        | Some err -> Some err
        | None ->
            match cond count with
            | true -> None
            | false -> Some {
                Msg = sprintf "%s: %d" errMsg count 
                ComponentsAffected = [] // TODO: add portId?
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
        let inputRes = checkEvery inputCounts ((=) 1) "Input port receives an unexpected number of connections"
        let outputRes = checkEvery outputCounts ((<=) 1) "Output port receives an unexpected number of connections"
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

/// Check that the combinatorial logic contains no cycles.
let private checkCombinatorialCycle (graph : SimulationGraph) : SimulationError option =
    let rec checkGraphForest nodeIds visited =
        match nodeIds with
        | [] -> None
        | nodeId :: nodeIds' ->
            match dfs nodeId graph visited Set.empty with
            | NoCycle visited -> checkGraphForest nodeIds' visited
            | Cycle cycle -> Some {
                Msg = "Cycle detected in combinatorial logic"
                ComponentsAffected = cycle
                ConnectionsAffected = [] }
            | Backtracking (c, ce) -> failwithf "what? Dfs should never terminate while backtracking: %A" (c, ce)

    let visited = Set.empty
    let allIds = graph |> Map.toList |> List.map (fun (id, _) -> id)
    checkGraphForest allIds visited

/// Analyse the simulation graph and return any error (or None).
let analyseGraph (graph : SimulationGraph) : SimulationError option =
    checkCombinatorialCycle graph
