(*
    Builder.fs

    This module collects functions to build a SimulationGraph, starting from
    a CanvasState. It also runs all the checks contained in Analyser to validate
    the graph is correct (and can be built in the first place).
*)

module SimulationBuilder

open Helpers
open DiagramTypes
open SimulatorTypes
open Analyser

/// This function should only be called on Component ports, never on Connection
/// ports: ports in Components should always have Some portNumber, ports in
/// Connections should have None.
let private getPortNumberOrFail port =
    match port with
    | None -> failwithf "what? Component ports should always have a portNumber"
    | Some p -> p

let private bitNot bit =
    match bit with
    | Zero -> One
    | One -> Zero

let private bitAnd bit0 bit1 =
    match bit0, bit1 with
    | One, One -> One
    | _, _ -> Zero

let private bitOr bit0 bit1 =
    match bit0, bit1 with
    | Zero, Zero -> Zero
    | _, _ -> One

let private bitXor bit0 bit1 =
    match bit0, bit1 with
    | Zero, One | One, Zero -> One
    | _, _ -> Zero

let private bitNand bit0 bit1 =
    bitAnd bit0 bit1 |> bitNot

let private bitNor bit0 bit1 =
    bitOr bit0 bit1 |> bitNot

let private bitXnor bit0 bit1 =
    bitXor bit0 bit1 |> bitNot

/// Make sure that the size of the inputs of a SimulationComponent is as
/// expected.
let private assertNotTooManyInputs
        (inputs : Map<InputPortNumber, WireData>)
        (cType : ComponentType)
        (expected : int)
        : unit =
    if inputs.Count > expected
    then failwithf "what? assertNotTooManyInputs failed for %A: %d > %d" cType inputs.Count expected  

/// Extract the values of the inputs of a SimulationComponent.
/// If any of these inputs is missing, return None.
/// The values are returned in the the passed order. E.g. if portNumbers is
/// [0, 1, 2], the returned value will be [bit0, bit1, bit2].
let rec private getValuesForPorts
        (inputs : Map<InputPortNumber, WireData>)
        (portNumbers : InputPortNumber list)
        : (WireData list) option =
    match portNumbers with
    | [] -> Some []
    | portNumber :: portNumbers' ->
        match inputs.TryFind portNumber with
        | None -> None
        | Some wireData ->
            match getValuesForPorts inputs portNumbers' with
            | None -> None
            | Some values -> Some <| wireData :: values

/// Assert that the wireData only contain a single bit, and return such bit.
let extractBit (wireData : WireData) : Bit =
    assertThat (wireData.Length = 1) <| sprintf "extractBit called with wireData: %A" wireData
    wireData.[0]

let packBit (bit : Bit) : WireData = [bit]

let private getBinaryGateReducer (op : Bit -> Bit -> Bit) componentType =
    fun inputs _ ->
        assertNotTooManyInputs inputs componentType 2
        match getValuesForPorts inputs [InputPortNumber 0; InputPortNumber 1] with
        | None -> None, None // Wait for more inputs.
        | Some [bit0; bit1] ->
            let bit0 = extractBit bit0
            let bit1 = extractBit bit1
            Some <| Map.empty.Add (OutputPortNumber 0, packBit (op bit1 bit0)), None
        | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs

/// Given a component type, return a function takes its inputs and transforms
/// them into outputs. The reducer should return None if there are not enough
/// inputs to calculate the outputs.
/// For custom components, return a fake version of the reducer, that has to be
/// replaced when resolving the dependencies.
let private getReducer
        (componentType : ComponentType)
        : Map<InputPortNumber, WireData>               // Inputs.
          -> SimulationGraph option                    // CustomSimulationGraph.
          -> (Map<OutputPortNumber, WireData> option * // Outputs.
              SimulationGraph option)                  // Updated CustomSimulationGraph.
        =
    // Always ignore the CustomSimulationGraph here, both in inputs and output.
    // The Reducer for Custom components, which use it, will be replaced in the
    // DependencyMerger.
    match componentType with
    | Input ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 1
            // Simply forward the input.
            // Note that the input of and Input node must be feeded manually.
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None -> None, None // Wait for more inputs.
            | Some [bit] -> Some <| Map.empty.Add (OutputPortNumber 0, bit), None
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Output ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 1
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None | Some [_] -> None, None // Do nothing with it. Just make sure it is received.
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Not ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 1
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None -> None, None // Wait for more inputs.
            | Some [bit] ->
                let bit = extractBit bit
                Some <| Map.empty.Add (OutputPortNumber 0, packBit (bitNot bit)), None
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | And  -> getBinaryGateReducer bitAnd And
    | Or   -> getBinaryGateReducer bitOr Or
    | Xor  -> getBinaryGateReducer bitXor Xor
    | Nand -> getBinaryGateReducer bitNand Nand
    | Nor  -> getBinaryGateReducer bitNor Nor
    | Xnor -> getBinaryGateReducer bitXnor Xnor
    | Mux2 ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 3
            match getValuesForPorts inputs [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
            | None -> None, None // Wait for more inputs.
            | Some [bit0; bit1; bitSelect] ->
                // TODO: allow mux2 to deal with buses? To do so, just remove
                // the extractBit code.
                let bit0 = extractBit bit0
                let bit1 = extractBit bit1
                let out = if (extractBit bitSelect) = Zero then bit0 else bit1
                Some <| Map.empty.Add (OutputPortNumber 0, packBit out), None
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | Custom c ->
        fun _ _ ->
            failwithf "what? Custom components reducer should be overridden before using it in a simulation: %A" c
    | MakeBus2 ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 2
            match getValuesForPorts inputs [InputPortNumber 0; InputPortNumber 1] with
            | None -> None, None
            | Some [bit0; bit1] ->
                let bit0 = extractBit bit0
                let bit1 = extractBit bit1
                Some <| Map.empty.Add (OutputPortNumber 0, [bit0; bit1]), None
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs
    | SplitBus2 ->
        fun inputs _ ->
            assertNotTooManyInputs inputs componentType 1
            match getValuesForPorts inputs [InputPortNumber 0] with
            | None -> None, None
            | Some [[bit0; bit1]] ->
                let out = Map.empty.Add (OutputPortNumber 0, packBit bit0)
                let out = out.Add (OutputPortNumber 1, packBit bit1)
                Some out, None
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType inputs

/// Build a map that, for each source port in the connections, keeps track of
/// the ports it targets.
/// It makes no sense to extract the PortNumber in this function as it is always
/// set to None for ports in connections.
let private buildSourceToTargetPortMap
        (connections : Connection list)
        : Map<OutputPortId, (ComponentId * InputPortId) list> =
    (Map.empty, connections) ||> List.fold (fun map conn ->
        let key = OutputPortId conn.Source.Id
        let target = ComponentId conn.Target.HostId, InputPortId conn.Target.Id
        // Append the new target to the list associated with the key.
        let newValue =
            match map.TryFind key with
            | None -> [target]
            | Some oldValue -> target :: oldValue
        map.Add (key, newValue)
    )

/// For each input port in each component, map it to its port number.
let private mapInputPortIdToPortNumber
        (components : Component list)
        : Map<InputPortId, InputPortNumber> =
    (Map.empty, components) ||> List.fold (fun map comp ->
        (map, comp.InputPorts) ||> List.fold (fun map port ->
            map.Add (InputPortId port.Id,
                     InputPortNumber (getPortNumberOrFail port.PortNumber))
        )
    )

/// Build a simulation component.
let private buildSimulationComponent
        (sourceToTargetPort : Map<OutputPortId, (ComponentId * InputPortId) list>)
        (portIdToPortNumber : Map<InputPortId, InputPortNumber>)
        (comp : Component)
        : SimulationComponent =
    // Remove portIds and use portNumbers instead.
    let mapPortIdsToPortNumbers
            (targets : (ComponentId * InputPortId) list)
            : (ComponentId * InputPortNumber) list =
        targets |> List.map (fun (compId, portId) ->
            match portIdToPortNumber.TryFind <| portId with
            | None -> failwithf "what? Input port with portId %A has no portNumber associated" portId
            | Some portNumber -> compId, portNumber
        )
    // For each output port, find out which other components and ports are
    // connected to it.
    let outputs =
        comp.OutputPorts
        |> List.map (fun port ->
            match sourceToTargetPort.TryFind <| OutputPortId port.Id with
            | None -> failwithf "what? Unconnected output port %s in comp %s" port.Id comp.Id
            | Some targets -> OutputPortNumber (getPortNumberOrFail port.PortNumber),
                              mapPortIdsToPortNumbers targets
        )
        |> Map.ofList
    {
        Id = ComponentId comp.Id
        Type = comp.Type
        Label = ComponentLabel comp.Label
        Inputs = Map.empty // The inputs will be set during the simulation.
        Outputs = outputs
        CustomSimulationGraph = None // Custom components will be augumented by the DependencyMerger.
        Reducer = getReducer comp.Type
    }

/// Transforms a canvas state into a simulation graph.
let private buildSimulationGraph (canvasState : CanvasState) : SimulationGraph =
    let components, connections = canvasState
    let sourceToTargetPort = buildSourceToTargetPortMap connections
    let portIdToPortNumber = mapInputPortIdToPortNumber components
    let mapper = buildSimulationComponent sourceToTargetPort portIdToPortNumber
    components
    |> List.map (fun comp -> ComponentId comp.Id, mapper comp)
    |> Map.ofList

/// Validate a diagram and generate its simulation graph.
let runChecksAndBuildGraph
        (canvasState : CanvasState)
        : Result<SimulationGraph, SimulationError> =
    match analyseState canvasState with
    | Some err -> Error err
    | None ->
        let _, connections = canvasState
        let graph = canvasState |> buildSimulationGraph
        match analyseGraph graph connections with
        | Some err -> Error err
        | None -> Ok graph
