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
        (reducerInput : ReducerInput)
        (cType : ComponentType)
        (expected : int)
        : unit =
    assertThat (reducerInput.Inputs.Count <= expected)
    <| sprintf "assertNotTooManyInputs failed for %A: %d > %d" cType reducerInput.Inputs.Count expected  

/// Make sure combinational logic does not handle clock ticks.
let private assertNoClockTick
            (reducerInput : ReducerInput)
            (cType : ComponentType)
            : unit =
    assertThat (not reducerInput.IsClockTick)
    <| sprintf "Unexpected IsClockTick = true in combinational logic reducer input for %A" cType

let private assertValidBus (bus : WireData) (minWidth : int) compType : unit =
    assertThat (bus.Length >= minWidth)
    <| sprintf "%A bus has invalid width: %d < %d" compType bus.Length minWidth

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
let private extractBit (wireData : WireData) : Bit =
    assertThat (wireData.Length = 1) <| sprintf "extractBit called with wireData: %A" wireData
    wireData.[0]

let private packBit (bit : Bit) : WireData = [bit]

/// Reducer outputs for when a component has not enough inputs to produce an
/// actual output.
let private notReadyReducerOutput = {
    Outputs = None
    NewCustomSimulationGraph = None
}

/// Make reducer outputs for NOT-custom components.
let private makeReducerOutput outputs = {
    Outputs = Some outputs
    NewCustomSimulationGraph = None
}

let private getBinaryGateReducer (op : Bit -> Bit -> Bit) componentType : ReducerInput -> ReducerOutput =
    fun reducerInput ->
        assertNoClockTick reducerInput componentType
        assertNotTooManyInputs reducerInput componentType 2
        match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
        | None -> notReadyReducerOutput // Wait for more inputs.
        | Some [bit0; bit1] ->
            let bit0 = extractBit bit0
            let bit1 = extractBit bit1
            Map.empty.Add (OutputPortNumber 0, packBit (op bit1 bit0))
            |> makeReducerOutput
        | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput

/// Given a component type, return a function takes a ReducerInput and
/// transform it into a ReducerOuptut.
/// The ReducerOutput should have Outputs set to None if there are not enough
/// ReducerInput.Inputs to calculate the outputs.
/// For custom components, return a fake version of the reducer, that has to be
/// replaced when resolving the dependencies.
let private getReducer (componentType : ComponentType) : ReducerInput -> ReducerOutput =
    // Always ignore the CustomSimulationGraph here, both in inputs and output.
    // The Reducer for Custom components, which use it, will be replaced in the
    // DependencyMerger.
    match componentType with
    | Input width ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            // Simply forward the input.
            // Note that the input of and Input node must be feeded manually.
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length = width) <| sprintf "Input node reducer received wrong number of bits: expected %d but got %d" width bits.Length
                Map.empty.Add (OutputPortNumber 0, bits) |> makeReducerOutput
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Output width ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length = width) <| sprintf "Output node reducer received wrong number of bits: expected %d but got %d" width bits.Length
                notReadyReducerOutput // Do nothing with it. Just make sure it is received.
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Not ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bit] ->
                let bit = extractBit bit
                Map.empty.Add (OutputPortNumber 0, packBit (bitNot bit))
                |> makeReducerOutput
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | And  -> getBinaryGateReducer bitAnd And
    | Or   -> getBinaryGateReducer bitOr Or
    | Xor  -> getBinaryGateReducer bitXor Xor
    | Nand -> getBinaryGateReducer bitNand Nand
    | Nor  -> getBinaryGateReducer bitNor Nor
    | Xnor -> getBinaryGateReducer bitXnor Xnor
    | Mux2 ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 3
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bit0; bit1; bitSelect] ->
                // TODO: allow mux2 to deal with buses? To do so, just remove
                // the extractBit code.
                let bit0 = extractBit bit0
                let bit1 = extractBit bit1
                let out = if (extractBit bitSelect) = Zero then bit0 else bit1
                Map.empty.Add (OutputPortNumber 0, packBit out)
                |> makeReducerOutput
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Demux2 ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 2
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bitIn; bitSelect] ->
                // TODO: allow demux2 to deal with buses? To do so, just remove
                // the extractBit code.
                let bitIn = extractBit bitIn
                let out0, out1 = if (extractBit bitSelect) = Zero
                                 then bitIn, Zero else Zero, bitIn
                let out = Map.empty.Add (OutputPortNumber 0, packBit out0)
                let out = out.Add (OutputPortNumber 1, packBit out1)
                makeReducerOutput out
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Custom c ->
        fun _ ->
            // The custom reducer is produced by the DependencyMerger.
            failwithf "what? Custom components reducer should be overridden before using it in a simulation: %A" c
    | MergeWires ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 2
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bits0; bits1] ->
                Map.empty.Add (OutputPortNumber 0, bits0 @ bits1)
                |> makeReducerOutput
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | SplitWire topWireWidth ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length >= topWireWidth + 1)
                <| sprintf "SplitWire received too little bits: expected at least %d but got %d" (topWireWidth + 1) bits.Length
                let bits0, bits1 = List.splitAt topWireWidth bits
                let out = Map.empty.Add (OutputPortNumber 0, bits0)
                let out = out.Add (OutputPortNumber 1, bits1)
                makeReducerOutput out
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | DFF ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | false ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                notReadyReducerOutput
            | true ->
                // Propagate the current inputs. If there are no current inputs,
                // propagate zero. This behaviour is equivalent to initialising
                // the flip flop with zero.
                let outBit =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
                    | None -> Zero
                    | Some [bit] -> extractBit bit
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                Map.empty.Add (OutputPortNumber 0, packBit outBit)
                |> makeReducerOutput

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
    // The inputs will be set during the simulation, we just need to initialise
    // the ones for Output nodes, see below.
    let inputs =
        match comp.Type with
        | Output width ->
            // Initialise all outputs to zero. This is necessary because upon
            // starting the simulation we need to feed all zeros. To do so, we
            // need to feed all simulation inputs set to zero and a global clock
            // tick. The problem is that both operations expect all outputs to
            // be set as result, but they don't necessarily set all the outputs
            // themselves. Therefore there is not an order you can run them in
            // that will always work. Presetting the outputs solves the problem
            // and the value does not matter as all outputs will be set again
            // in that initialization process.
            Map.empty.Add (InputPortNumber 0, List.replicate width Zero)
        | _ -> Map.empty
    {
        Id = ComponentId comp.Id
        Type = comp.Type
        Label = ComponentLabel comp.Label
        Inputs = inputs
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
