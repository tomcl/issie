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

/// Analyse the simulation graph and return any error (or None).
let analyseGraph (graph : SimulationGraph) : SimulationError option =
    None
