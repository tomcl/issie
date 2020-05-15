module BusWidthInferer

open BusTypes
open DiagramTypes
open Helpers

// 1. Initialise Map<ConnectionId, int option> for all connections, to None
//    (None means width not inferred yet).
// 2. Extract Input Components.
// 3. Starting from all those input components, run the inference process:
//    a. Case: component has all information about connections connected to it.
//       (for example, an input node or an and gate. They know they expect bits).
//       - Get the width of the incoming wires
//       - If there is any inconsistence, return the error
//       - Set the width of the outgoing wires
//       - follow the wires you just set and repeat the inference process on the
//         new components.
//    b. Case: component does not have all the information for computing the
//       width of outgoing wires yet.
//       (for example, a mergeBus components with only one bus connected)
//       - return

/// Extract the port number of a component port. Port numbers on Components
/// should always be populated (while they are always None for ports in
/// Connections).
let private extractComponentPortNumber port =
    match port.PortNumber with
    | None -> failwithf "what? extractComponentPortNumber should always be called with component ports: %A" port
    | Some pNumber -> pNumber

let private assertInputsSize
        (inputs : Map<InputPortNumber, int option>)
        (expected : int)
        (comp : Component) =
    assertThat (inputs.Count = expected)
    <| sprintf "assertInputsSize failed for: %A" comp

let private getOutputPortId (comp : Component) (idx : int) : OutputPortId =
    match List.tryFind (fun p -> extractComponentPortNumber p = idx) comp.OutputPorts with
    | None -> failwithf "what? getOutputPortId called with inexistent port idx (%d): %A " idx comp
    | Some port -> OutputPortId port.Id

/// Extract the widths of the connections to input ports of a component.
/// The values are returned in the the passed order. E.g. if portNumbers is
/// [0, 1, 2], the returned value will be [width0, width1, width2].
let rec private getWidthsForPorts
        (inputs : Map<InputPortNumber, int option>)
        (portNumbers : InputPortNumber list)
        : (int option) list =
    match portNumbers with
    | [] -> []
    | portNumber :: portNumbers' ->
        match inputs.TryFind portNumber with
        | None -> failwithf "what? getWidthForPorts received a not extistent port: %A %A" portNumber inputs
        | Some width -> width :: getWidthsForPorts inputs portNumbers'

let private makeWidthInferErrorEqual expected actual connectionsAffected = Error {
    Msg = sprintf "Wrong wire width. Expecting %d but got %d." expected actual
    ConnectionsAffected = connectionsAffected
}

let private makeWidthInferErrorAtLeast atLeast actual connectionsAffected = Error {
    Msg = sprintf "Wrong wire width. Expecting at least size %d but got %d." atLeast actual
    ConnectionsAffected = connectionsAffected
}

/// Given a component and a set of input connection widths, check these inputs
/// widths are as expected and try to calculate the width of the outgoing
/// connections.
/// Components can produce outputs as soon as they have enough info (e.g.
/// gates can output width 1 straight away, PushToBusFirst can output n + 1 as
/// soon as it finds out n, and so on). This is possible because
/// setConnectionsWidth will make sure that we do not re-explore an already set
/// connection. This should allow partially connected components to still work
/// if they have already enough info.
/// TODO add connectionId to inputConnectionWidth, to improve errors.
let private calculateOutputPortsWidth
        (comp : Component)
        (inputConnectionsWidth : Map<InputPortNumber, int option>)
        : Result<Map<OutputPortId, int>, WidthInferError> =
    match comp.Type with
    | Input ->
        // Expects no inputs, and has an outgoing wire of width 1.
        assertInputsSize inputConnectionsWidth 0 comp
        Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
    | Output ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] | [Some 1] -> Ok Map.empty // Output node has no outputs.
        | [Some n] -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | Not ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] | [Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n] -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | And | Or | Xor | Nand | Nor | Xnor ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [None; _] | [_; None]
        | [Some 1; Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | Mux2 ->
        // TODO: also allow buses? Need to change also simulation reducer.
        assertInputsSize inputConnectionsWidth 3 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
        | [None; _; _] | [_; None; _] | [_; _; None]
        | [Some 1; Some 1; Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 1)
        | [Some n; _; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [_; Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [_; _; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | Custom custom ->
        assertInputsSize inputConnectionsWidth custom.InputLabels.Length comp
        let inputWidths =
            [0..custom.InputLabels.Length - 1]
            |> List.map InputPortNumber
            |> getWidthsForPorts inputConnectionsWidth
        // Make sure that input Widths match what expected.
        let maybeError =
            (inputWidths, custom.InputLabels)
            ||> List.map2 (fun actual (_, expected) ->
                match actual with
                | None -> None // Cannot determine if it is ok yet.
                | Some w when w = expected -> None // No error.
                | Some w -> Some <| makeWidthInferErrorEqual expected w [] // TODO: pass connections affected.
            )
        match List.tryFind (fun el -> el <> None) maybeError with
        | Some (Some err) -> err
        | None -> custom.OutputLabels
                  |> List.mapi (fun idx (_, w) -> getOutputPortId comp idx, w)
                  |> Map.ofList |> Ok
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | MakeBus2 ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [None; _] | [_; None]
        | [Some 1; Some 1] -> Ok <| Map.empty.Add (getOutputPortId comp 0, 2)
        | [Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | SplitBus2 ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] | [Some 2] ->
            let out = Map.empty.Add (getOutputPortId comp 0, 1)
            let out = out.Add (getOutputPortId comp 1, 1)
            Ok out
        | [Some n] when n <> 2 -> makeWidthInferErrorEqual 2 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | PushToBusFirst ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [Some n; _] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [_; Some n] when n <= 1 -> makeWidthInferErrorAtLeast 2 n [] // TODO: pass connections affected.
        | [_; None] -> Ok Map.empty // Keep on waiting.
        | [_; Some n] when n > 1 -> Ok <| Map.empty.Add (getOutputPortId comp 0, n + 1)
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | PushToBusLast ->
        assertInputsSize inputConnectionsWidth 2 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0; InputPortNumber 1] with
        | [_; Some n] when n <> 1 -> makeWidthInferErrorEqual 1 n [] // TODO: pass connections affected.
        | [Some n; _] when n <= 1 -> makeWidthInferErrorAtLeast 2 n [] // TODO: pass connections affected.
        | [None; _] -> Ok Map.empty // Keep on waiting.
        | [Some n; _] when n > 1 -> Ok <| Map.empty.Add (getOutputPortId comp 0, n + 1)
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | PopFirstFromBus ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok Map.empty // Keep on waiting.
        | [Some n] when n > 2 ->
            let out = Map.empty.Add (getOutputPortId comp 0, 1)
            let out = out.Add (getOutputPortId comp 1, n - 1)
            Ok out
        | [Some n] when n <= 2 -> makeWidthInferErrorAtLeast 3 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type
    | PopLastFromBus ->
        assertInputsSize inputConnectionsWidth 1 comp
        match getWidthsForPorts inputConnectionsWidth [InputPortNumber 0] with
        | [None] -> Ok Map.empty // Keep on waiting.
        | [Some n] when n > 2 ->
            let out = Map.empty.Add (getOutputPortId comp 0, n - 1)
            let out = out.Add (getOutputPortId comp 1, 1)
            Ok out
        | [Some n] when n <= 2 -> makeWidthInferErrorAtLeast 3 n [] // TODO: pass connections affected.
        | _ -> failwithf "what? Impossible case in case in calculateOutputPortsWidth for: %A" comp.Type

/// Find the connection connected to an input port. Return None if no such
/// connection exists.
let private findConnectionToInputPort
        (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>)
        (portId : InputPortId)
        : ConnectionId option =
    inputPortIdsToConnectionIds.TryFind portId

/// Find all the connections connected to an output port.
let private findConnectionsFromOutputPort
        (outputPortIdsToConnections : Map<OutputPortId, Connection list>)
        (portId : OutputPortId)
        : (Connection list) option =
    outputPortIdsToConnections.TryFind portId

/// Lookup the width of a connection in the connectionsWidth map or fail.
let private getConnectionWidth
        (connectionsWidth : ConnectionsWidth)
        (connId : ConnectionId)
        : int option =
    match connectionsWidth.TryFind connId with
    | None -> failwithf "what? getConnectionWidth received inexistent connectionId: %A" connId
    | Some width -> width

/// For each port on a given component, obtain the width of the wire connecting
/// to it, or None if there is no wire connecting to it or the wire width is
/// unknown.
let private getInputPortsConnectionsWidth
        (connectionsWidth : ConnectionsWidth)
        (currNode : Component)
        (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>)
        : Map<InputPortNumber, int option> =
    currNode.InputPorts
    |> List.map (fun inputPort ->
        InputPortId inputPort.Id
        |> findConnectionToInputPort inputPortIdsToConnectionIds
        |> function
           | Some connId ->
               // If some connection is present, try to etract is width.
               InputPortNumber <| extractComponentPortNumber inputPort,
               getConnectionWidth connectionsWidth connId
           | None ->
               // If no connection is present, just use None.
               InputPortNumber <| extractComponentPortNumber inputPort,
               None
    )
    |> Map.ofList


let private setConnectionWidth
        (connectionId : ConnectionId)
        (connectionWidth : int)
        (connectionsWidth : ConnectionsWidth) =
    connectionsWidth.Add (connectionId, Some connectionWidth)

/// Set the width of a bunch of connections, and return the updated
/// connectionsWidth together with the list of connections that have had their
/// width value updated.
/// If the width for a connection is already set:
/// - if the value we are going to set is identical to the already set value:
///   connection already visited (loop). Do not return the connection.
/// - if the value is different, return an error.
let private setConnectionsWidth
        (connections : Connection list)
        (connWidth : int)
        (connectionsWidth : ConnectionsWidth)
        : Result<ConnectionsWidth * (Connection list), WidthInferError> =
    (Ok (connectionsWidth, []), connections)
    ||> List.fold (fun res conn ->
        res |> Result.bind (fun (connectionsWidth, connectionsToReturn) ->
            let connId = ConnectionId conn.Id
            match getConnectionWidth connectionsWidth connId with
            | None ->
                // Width for the connection was never set. Set it and return
                // the connection.
                Ok (setConnectionWidth connId connWidth connectionsWidth,
                    conn :: connectionsToReturn)
            | Some oldWidth when oldWidth = connWidth ->
                // Width for the connection is already set. The old width
                // matches the current width. Do not return the connection.
                Ok (connectionsWidth, connectionsToReturn)
            | Some oldWidth when oldWidth <> connWidth ->
                // Width for the connection is already set, but the old width
                // does not match the current width.
                Error {
                    Msg = sprintf "Wire has been inferred to have two different widths: %d and %d. This is probably due to an error such as a combinatorial loop." oldWidth connWidth
                    ConnectionsAffected = [connId]
                }
            | _ -> failwithf "what? Impossible case in setConnectionsWidth."
        )
    )

let private getComponentFromId
        (compId : ComponentId)
        (compIdsToComps : Map<ComponentId, Component>)
        : Component =
    match compIdsToComps.TryFind compId with
    | None -> failwithf "what? getComponentFromId called with invalid componentId: %A" compId
    | Some comp -> comp

/// Given a node, try to infer the width of its outgoing connections, and
/// possibly recur on the nodes targeted by those connections.
let rec private infer
        (connectionsWidth : ConnectionsWidth)
        (currNode : Component)
        // Static maps. Necessary for fast lookups.
        (inputPortIdsToConnectionIds : Map<InputPortId, ConnectionId>)
        (outputPortIdsToConnections : Map<OutputPortId, Connection list>)
        (compIdsToComps : Map<ComponentId, Component>)
        : Result<ConnectionsWidth, WidthInferError> =
    let iterateChildren outgoingConnections connectionsWidth =
        let children =
            outgoingConnections
            |> List.map (fun conn -> getComponentFromId (ComponentId conn.Target.HostId) compIdsToComps)
        (Ok connectionsWidth, children)
        ||> List.fold (fun connectionsWidthRes child ->
            connectionsWidthRes
            |> Result.bind (fun connectionsWidth ->
                infer
                    connectionsWidth child inputPortIdsToConnectionIds
                    outputPortIdsToConnections compIdsToComps
            )
        )

    getInputPortsConnectionsWidth connectionsWidth currNode
                                  inputPortIdsToConnectionIds
    |> calculateOutputPortsWidth currNode
    |> Result.bind (fun outputPortsWidths ->
        // For each output in the map:
        // - Get all connections that are connected to that port.
        // - Set the width of the connection to the inferred value. If the
        //   connection has already been inferred, it must be because of a loop.
        //   If the value is different, return error. If the value is the same,
        //   just ignore the connection otherwise you would get stuck in the
        //   loop.
        // - For the non-ingored connections, take recur infer on the
        //   Target.HostId.
        (Ok connectionsWidth, outputPortsWidths)
        ||> Map.fold (fun connectionsWidthRes outPortId connWidth ->
            connectionsWidthRes
            |> Result.bind (fun connectionsWidth ->
                match findConnectionsFromOutputPort
                          outputPortIdsToConnections outPortId with
                | None ->
                    // Unconnected port. Do not recur.
                    Ok connectionsWidth
                | Some outgoingConnections ->
                    setConnectionsWidth outgoingConnections connWidth connectionsWidth
                    |> Result.bind (fun (connectionsWidth, updatedConnections) ->
                        iterateChildren updatedConnections connectionsWidth
                    )
            )
        )
    )

let private initialiseConnectionsWidth connections : ConnectionsWidth =
    connections
    |> List.map (fun conn -> ConnectionId conn.Id, None)
    |> Map.ofList

let private getAllInputNodes components : Component list =
    components |> List.filter (fun comp -> comp.Type = Input)

/// For each connected Input port, map the connection that is connected to it.
/// Fail if there are multiple connections connected to the same input port.
/// Such scenario would mean that a wire is driven by multiple components.
let private mapInputPortIdsToConnectionIds
        (connections : Connection list)
        : Result<Map<InputPortId, ConnectionId>, WidthInferError> =
    (Ok Map.empty, connections)
    ||> List.fold (fun mapRes conn ->
        mapRes |> Result.bind (fun map ->
            let inputPortId = InputPortId conn.Target.Id
            let connId = ConnectionId conn.Id
            match map.TryFind inputPortId with
            | None -> Ok <| map.Add (inputPortId, connId)
            | Some otherConnId -> Error {
                Msg = "Wire driven by multiple outputs"
                ConnectionsAffected = [connId; otherConnId]
            }
        )
    )

let private mapComponentIdsToComponents
        (components : Component list)
        : Map<ComponentId, Component> =
    components
    |> List.map (fun comp -> ComponentId comp.Id, comp)
    |> Map.ofList 

let private mapOutputPortIdsToConnections
        (connections : Connection list)
        : Map<OutputPortId, Connection list> =
    connections
    |> List.groupBy (fun conn -> OutputPortId conn.Source.Id)
    |> Map.ofList

let inferConnectionsWidth
        (state : CanvasState)
        : Result<ConnectionsWidth, WidthInferError> =
    let components, connections = state
    let connectionsWidth = initialiseConnectionsWidth connections
    let compIdsToComps = mapComponentIdsToComponents components
    let outputPortIdsToConnections = mapOutputPortIdsToConnections connections
    match mapInputPortIdsToConnectionIds connections with
    | Error e -> Error e
    | Ok inputPortIdsToConnectionIds ->
        // If this is too slow, one could start the process only from input
        // components. To do so, pass the (getAllInputNodes components) instead
        // of components.
        (Ok connectionsWidth, components)
        ||> List.fold (fun connectionsWidthRes inputNode ->
            connectionsWidthRes |> Result.bind (fun connectionsWidth ->
                infer
                    connectionsWidth inputNode inputPortIdsToConnectionIds
                    outputPortIdsToConnections compIdsToComps
            )
        )
