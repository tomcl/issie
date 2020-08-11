(*
    Builder.fs

    This module collects functions to build a SimulationGraph, starting from
    a CanvasState. It also runs all the checks contained in Analyser to validate
    the graph is correct (and can be built in the first place).
*)

module SimulationBuilder

open Helpers
open NumberHelpers
open CommonTypes
open SimulatorTypes
open CanvasStateAnalyser

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
    assertThat (reducerInput.IsClockTick = No)
    <| sprintf "Unexpected IsClockTick = Yes in combinational logic reducer input for %A" cType

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

/// Read the content of the memory at the specified address.
let private readMemory (mem : Memory) (address : WireData) : WireData =
    assertThat (mem.Data.Length = pow2 mem.AddressWidth)
    <| sprintf "Memory has wrong Data.Length: expected %d but got %d" (pow2 mem.AddressWidth) mem.Data.Length
    let intAddr = convertWireDataToInt address
    let outDataInt = mem.Data.[int intAddr]
    convertIntToWireData mem.WordWidth outDataInt

/// Write the content of the memory at the specified address.
let private writeMemory (mem : Memory) (address : WireData) (data : WireData) : Memory =
    assertThat (mem.Data.Length = pow2 mem.AddressWidth)
    <| sprintf "Memory has wrong Data.Length: expected %d but got %d" (pow2 mem.AddressWidth) mem.Data.Length
    let intAddr = int <| convertWireDataToInt address
    let intData = convertWireDataToInt data
    {mem with Data = listSet mem.Data intData intAddr}

/// Reducer outputs for when a component has not enough inputs to produce an
/// actual output.
let private notReadyReducerOutput state = {
    Outputs = None
    NewCustomSimulationGraph = None
    NewState = state
}

/// Make reducer outputs for NOT-custom components.
let private makeReducerOutput state outputs = {
    Outputs = Some outputs
    NewCustomSimulationGraph = None
    NewState = state
}

let private getBinaryGateReducer (op : Bit -> Bit -> Bit) componentType : ReducerInput -> ReducerOutput =
    fun reducerInput ->
        assertNoClockTick reducerInput componentType
        assertNotTooManyInputs reducerInput componentType 2
        match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
        | None -> notReadyReducerOutput NoState // Wait for more inputs.
        | Some [bit0; bit1] ->
            let bit0 = extractBit bit0
            let bit1 = extractBit bit1
            Map.empty.Add (OutputPortNumber 0, packBit (op bit1 bit0))
            |> makeReducerOutput NoState
        | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput

let private getDffStateBit state =
    match state with
    | DffState bit -> bit
    | _ -> failwithf "what? getDffStateBit called with an invalid state: %A" state

let private getRegisterStateBits state =
    match state with
    | RegisterState bits -> bits
    | _ -> failwithf "what? getRegisterStateBits called with an invalid state: %A" state

let private getRamStateMemory state =
    match state with
    | RamState memory -> memory
    | _ -> failwithf "what? getRamStateMemory called with an invalid state: %A" state

/// Given a component type, return a function takes a ReducerInput and
/// transform it into a ReducerOuptut.
/// The ReducerOutput should have Outputs set to None if there are not enough
/// ReducerInput.Inputs to calculate the outputs.
/// For custom components, return a fake version of the reducer, that has to be
/// replaced when resolving the dependencies.
/// TODO: some components reducers are quite similar, for example Register and
/// RegisterE and DFF and DFFE. It is probably a good idea to merge them
/// together to avoid duplicated logic.
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
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length = width) <| sprintf "Input node reducer received wrong number of bits: expected %d but got %d" width bits.Length
                Map.empty.Add (OutputPortNumber 0, bits) |> makeReducerOutput NoState
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Output width ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length = width) <| sprintf "Output node reducer received wrong number of bits: expected %d but got %d" width bits.Length
                notReadyReducerOutput NoState // Do nothing with it. Just make sure it is received.
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | IOLabel -> 
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits] ->
                let out = Map.empty.Add (OutputPortNumber 0, bits)
                makeReducerOutput NoState out
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput

    | Not ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bit] ->
                let bit = extractBit bit
                Map.empty.Add (OutputPortNumber 0, packBit (bitNot bit))
                |> makeReducerOutput NoState
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | BusSelection(width, lsb) ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length >= width + lsb)
                <| sprintf "Bus Selection received too few bits: expected at least %d but got %d" (width + lsb) bits.Length
                let outBits = bits.[lsb .. lsb + width - 1]
                let out = Map.empty.Add (OutputPortNumber 0, outBits)
                makeReducerOutput NoState out
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
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits0; bits1; bitSelect] ->
                assertThat (bits0.Length = bits1.Length)
                <| sprintf "Mux received two inputs with different widths: %A and %A" bits0 bits1
                let out = if (extractBit bitSelect) = Zero then bits0 else bits1
                Map.empty.Add (OutputPortNumber 0, out)
                |> makeReducerOutput NoState
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | Demux2 ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 2
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bitsIn; bitSelect] ->
                let zeros = List.replicate bitsIn.Length Zero
                let out0, out1 = if (extractBit bitSelect) = Zero
                                 then bitsIn, zeros else zeros, bitsIn
                let out = Map.empty.Add (OutputPortNumber 0, out0)
                let out = out.Add (OutputPortNumber 1, out1)
                makeReducerOutput NoState out
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | NbitsAdder numberOfBits ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 3
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1; InputPortNumber 2] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [cin; A; B] ->
                let sum, cout =
                    [cin; A; B]
                    |> List.map convertWireDataToInt
                    |> List.reduce (+)
                    |> convertIntToWireData (numberOfBits + 1)
                    |> List.splitAt numberOfBits
                let out = Map.empty.Add (OutputPortNumber 0, sum)
                let out = out.Add (OutputPortNumber 1, cout)
                makeReducerOutput NoState out
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
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits0; bits1] ->
                // Little endian, bits coming from the top wire are the least
                // significant.
                Map.empty.Add (OutputPortNumber 0, bits0 @ bits1)
                |> makeReducerOutput NoState
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | SplitWire topWireWidth ->
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Wait for more inputs.
            | Some [bits] ->
                assertThat (bits.Length >= topWireWidth + 1)
                <| sprintf "SplitWire received too little bits: expected at least %d but got %d" (topWireWidth + 1) bits.Length
                let bits0, bits1 = List.splitAt topWireWidth bits
                // Little endian, bits leaving from the top wire are the least
                // significant.
                let out = Map.empty.Add (OutputPortNumber 0, bits0)
                let out = out.Add (OutputPortNumber 1, bits1)
                makeReducerOutput NoState out
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | DFF ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                // The newState returned does not matter! It is ignored unless
                // Input is a clock tick.
                notReadyReducerOutput NoState
            | Yes dffState ->
                let stateBit = getDffStateBit dffState
                // Store and propagate the current inputs.
                let newStateBit =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
                    | None -> stateBit
                    | Some [bit] -> extractBit bit
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let newState = DffState newStateBit
                Map.empty.Add (OutputPortNumber 0, packBit newStateBit)
                |> makeReducerOutput newState
    | DFFE ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                // The newState returned does not matter! It is ignored unless
                // Input is a clock tick.
                notReadyReducerOutput NoState
            | Yes dffState ->
                let stateBit = getDffStateBit dffState
                // Store and propagate the current inputs.
                let newStateBit =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
                    | None -> stateBit
                    | Some [bit; enable] -> if (extractBit enable = Zero)
                                            then stateBit else extractBit bit
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let newState = DffState newStateBit
                Map.empty.Add (OutputPortNumber 0, packBit newStateBit)
                |> makeReducerOutput newState
    | Register width ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                // The newState returned does not matter! It is ignored unless
                // Input is a clock tick.
                notReadyReducerOutput NoState
            | Yes regState ->
                let stateBits = getRegisterStateBits regState
                // Store and propagate the current inputs.
                let newStateBits =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
                    | None -> stateBits
                    | Some [bits] ->
                        assertThat (bits.Length = width)
                        <| sprintf "Register received data with wrong width: expected %d but got %A" width bits.Length
                        bits
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let newState = RegisterState newStateBits
                Map.empty.Add (OutputPortNumber 0, newStateBits)
                |> makeReducerOutput newState
    | RegisterE width ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                // The newState returned does not matter! It is ignored unless
                // Input is a clock tick.
                notReadyReducerOutput NoState
            | Yes regState ->
                let stateBits = getRegisterStateBits regState
                // Store and propagate the current inputs.
                let newStateBits =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0; InputPortNumber 1] with
                    | None -> stateBits
                    | Some [bits; enable] ->
                        assertThat (bits.Length = width)
                        <| sprintf "RegisterE received data with wrong width: expected %d but got %A" width bits.Length
                        if (extractBit enable = Zero)
                        then stateBits else bits
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let newState = RegisterState newStateBits
                Map.empty.Add (OutputPortNumber 0, newStateBits)
                |> makeReducerOutput newState
    | AsyncROM mem -> // Asynchronous ROM.
        fun reducerInput ->
            assertNoClockTick reducerInput componentType
            assertNotTooManyInputs reducerInput componentType 1
            match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
            | None -> notReadyReducerOutput NoState // Not ready yet.
            | Some [addr] ->
                assertThat (addr.Length = mem.AddressWidth)
                <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
                let outData = readMemory mem addr
                Map.empty.Add (OutputPortNumber 0, outData)
                |> makeReducerOutput NoState
            | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
    | ROM mem -> // Synchronous ROM.
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input.
                notReadyReducerOutput NoState
            | Yes state ->
                assertThat (state = NoState) "ROM component is stateless (only defined by initial data)."
                let address =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
                    | None ->
                        // By default, output the content of mem[0].
                        List.replicate mem.AddressWidth Zero
                    | Some [addr] ->
                        assertThat (addr.Length = mem.AddressWidth)
                        <| sprintf "ROM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
                        addr
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let outData = readMemory mem address
                Map.empty.Add (OutputPortNumber 0, outData)
                |> makeReducerOutput NoState
    | RAM _ ->
        fun reducerInput ->
            match reducerInput.IsClockTick with
            | No ->
                // If it is not a clock tick, just ignore the changes on the
                // input. The state returned is ignored.
                notReadyReducerOutput NoState
            | Yes state ->
                let mem = getRamStateMemory state
                let address =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 0] with
                    | None -> List.replicate mem.AddressWidth Zero
                    | Some [addr] ->
                        assertThat (addr.Length = mem.AddressWidth)
                        <| sprintf "RAM received address with wrong width: expected %d but got %A" mem.AddressWidth addr
                        addr
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let dataIn =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 1] with
                    | None -> List.replicate mem.WordWidth Zero
                    | Some [dataIn] ->
                        assertThat (dataIn.Length = mem.WordWidth)
                        <| sprintf "RAM received data-in with wrong width: expected %d but got %A" mem.WordWidth dataIn
                        dataIn
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                let write =
                    match getValuesForPorts reducerInput.Inputs [InputPortNumber 2] with
                    | None -> packBit Zero
                    | Some [bit] -> bit |> extractBit |> packBit // Ensure it is a single bit.
                    | _ -> failwithf "what? Unexpected inputs to %A: %A" componentType reducerInput
                // If write flag is on, write the memory content.
                let mem, dataOut =
                    match extractBit write with
                    | Zero ->
                        // Read memory address and return memory unchanged.
                        mem, readMemory mem address
                    | One ->
                        // Update memory and return new content.
                        writeMemory mem address dataIn, dataIn
                Map.empty.Add (OutputPortNumber 0, dataOut)
                |> makeReducerOutput (RamState mem)

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

/// Get the default state for a component.
/// Note that custom components are stateless, even though they may contain
/// stateful components. The state of such stateful components is maintained
/// in the CustomSimulationGraph.
/// ROMs are stateless (they are only defined by their initial content).
let private getDefaultState compType =
    match compType with
    | Input _ | Output _ | IOLabel | BusSelection _ | Not | And | Or | Xor | Nand | Nor | Xnor | Mux2
    | Demux2 | NbitsAdder _ | Custom _ | MergeWires | SplitWire _ | ROM _
    | AsyncROM _ -> NoState
    | DFF | DFFE -> DffState Zero
    | Register w | RegisterE w -> RegisterState <| List.replicate w Zero
    | RAM memory -> RamState memory // The RamState content may change during
                                    // the simulation.

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
        |> List.collect (fun port ->
            match sourceToTargetPort.TryFind <| OutputPortId port.Id with
            | None when comp.Type=IOLabel -> [] // IOLabels are allowed to be connected to nothing
            | None -> failwithf "what? Unconnected output port %s in comp %s" port.Id comp.Id
            | Some targets -> [
                                OutputPortNumber (getPortNumberOrFail port.PortNumber),
                                mapPortIdsToPortNumbers targets
                              ]
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
        State = getDefaultState comp.Type
        Reducer = getReducer comp.Type
    }

let getLabelConnections (comps:Component list) (conns: Connection list) =
    let labels = 
        comps 
        |> List.filter (fun co -> co.Type = IOLabel)

    let compIdMap =
        labels
        |> List.map (fun co -> ComponentId co.Id, co)
        |> Map.ofList

    let getComp n = compIdMap.[n]

    let targetMap =
        conns
        |> List.map (fun conn -> ComponentId conn.Target.HostId, conn)
        |> Map.ofList

    let getConnection (compTarget:Component) = targetMap.[ComponentId compTarget.Id]

    let copyConnection (conn: Connection) (compTarget:Component) (tagNum:int) =
        {conn with Target = compTarget.InputPorts.[0]; Id = sprintf "iolab%d" tagNum + conn.Id}

    let getDriverConnection (comps: Component list) =
        comps
        |> List.tryFind (fun co -> (Map.tryFind (ComponentId co.Id) targetMap) <> None)
        |> function 
            | None -> failwithf "What? component cannot be found in %A" targetMap
            | Some comp -> targetMap.[ComponentId comp.Id]

    labels
    |> List.groupBy (fun co -> co.Label)
    |> List.collect (fun (lab,lst) -> 
        let dConn = getDriverConnection lst
        lst
        |> List.filter (fun co -> co.Id <> dConn.Target.HostId)
        |> List.indexed
        |> List.map (fun (i, co) -> copyConnection dConn co i))



/// Transforms a canvas state into a simulation graph.
let private buildSimulationGraph (canvasState : CanvasState) : (SimulationGraph) =
    let components, connections' = canvasState
    let labConns = getLabelConnections components connections'
    let connections = labConns @ connections'
    let sourceToTargetPort = buildSourceToTargetPortMap connections
    let portIdToPortNumber = mapInputPortIdToPortNumber components
    let mapper = buildSimulationComponent sourceToTargetPort portIdToPortNumber
    components
    |> List.map (fun comp -> ComponentId comp.Id, mapper comp)
    |> Map.ofList
    |> (fun m -> m)

/// Validate a diagram and generate its simulation graph.
let runCanvasStateChecksAndBuildGraph
        (canvasState : CanvasState)
        : Result<SimulationGraph, SimulationError> =
    match analyseState canvasState with
    | Some err -> Error err
    | None -> Ok <| buildSimulationGraph canvasState
