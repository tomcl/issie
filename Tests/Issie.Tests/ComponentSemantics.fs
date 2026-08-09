/// Component semantics tests: each component type is simulated through the full
/// pipeline (canvas -> graph -> FastSim) in a minimal circuit, and its outputs are
/// compared against an independent reference implementation. Exhaustive at small
/// widths; FsCheck drives the >32-bit (bigint) paths in Properties.fs.
module ComponentSemantics

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasBuilder

let private maxArraySize = 40

let private mask (width: int) (v: bigint) = v % (1I <<< width)

/// Build the canvas: one Input1 per input width, the component under test, one Output
/// per output width
let private dutCanvas (compType: ComponentType) (inWidths: int list) (outWidths: int list) : CanvasState =
    let dut = makeComp "dut" (List.length inWidths) (List.length outWidths) compType "DUT"
    let ins = inWidths |> List.mapi (fun i w -> makeComp $"in{i}" 0 1 (Input1(w, None)) $"I{i}")
    let outs = outWidths |> List.mapi (fun i w -> makeComp $"out{i}" 1 0 (Output w) $"O{i}")
    let conns =
        (ins |> List.mapi (fun i c -> conn c 0 dut i))
        @ (outs |> List.mapi (fun i c -> conn dut i c 0))
    dut :: ins @ outs, conns

let private startSim (compType: ComponentType) (inWidths: int list) (outWidths: int list) =
    let canvas = dutCanvas compType inWidths outWidths
    let ldc = makeLdc "dut_sheet" None canvas
    match Simulator.startCircuitSimulation maxArraySize "dut_sheet" canvas [ ldc ] with
    | Error e -> failwith $"Simulation setup failed for %A{compType}: %A{e}"
    | Ok simData -> simData

let private driveInputs (simData: SimulationData) (tick: int) (inputVals: bigint list) =
    simData.Inputs
    |> List.iter (fun (cid, ComponentLabel label, width) ->
        let idx = int (label.Substring 1)
        let fd = NumberHelpers.convertBigintToFastData width inputVals[idx]
        FastExtract.changeInput cid (IData fd) tick simData.FastSim)

let private readOutputs (simData: SimulationData) (tick: int) : bigint list =
    simData.Outputs
    |> List.sortBy (fun (_, ComponentLabel label, _) -> label)
    |> List.map (fun (cid, _, _) ->
        match FastExtract.extractFastSimulationOutput simData.FastSim tick (cid, []) (OutputPortNumber 0) with
        | IData fd -> fd.GetBigInt
        | IAlg _ -> failwith "algebraic output")

/// One combinational evaluation of a component
let simulate compType inWidths outWidths (inputVals: bigint list) : bigint list =
    let simData = startSim compType inWidths outWidths
    driveInputs simData 0 inputVals
    readOutputs simData 0

/// Clocked simulation: returns the outputs after driving each tick's inputs
let simulateClocked compType inWidths outWidths (stimuliPerTick: bigint list list) : bigint list list =
    let simData = startSim compType inWidths outWidths
    stimuliPerTick
    |> List.mapi (fun tick inputVals ->
        if tick > 0 then
            FastRun.runFastSimulation None tick simData.FastSim |> ignore
        driveInputs simData tick inputVals
        readOutputs simData tick)

/// All input combinations for the given widths (exhaustive)
let private allInputs (widths: int list) : bigint list list =
    widths
    |> List.fold
        (fun acc w -> [ for prefix in acc do for v in 0 .. (1 <<< w) - 1 -> prefix @ [ bigint v ] ])
        [ [] ]

/// Destructure the inputs handed to a reference function.
/// A reference takes a bigint list, since checkExhaustive builds one from the widths, but each
/// reference below knows how many inputs its component has. A plain list pattern in the lambda
/// would be an incomplete match; these are single-case active patterns, so they are total as far
/// as the compiler is concerned and say what went wrong if a reference and its widths disagree.
let private arityError n (l: bigint list) =
    failwithf "reference function expected %d inputs but was given %d: %A" n l.Length l

let private (|In1|) (l: bigint list) =
    match l with | [ a ] -> a | _ -> arityError 1 l

let private (|In2|) (l: bigint list) =
    match l with | [ a; b ] -> a, b | _ -> arityError 2 l

let private (|In3|) (l: bigint list) =
    match l with | [ a; b; c ] -> a, b, c | _ -> arityError 3 l

let private (|In5|) (l: bigint list) =
    match l with | [ a; b; c; d; e ] -> a, b, c, d, e | _ -> arityError 5 l

/// Check a combinational component against a reference function over every input combination
let private checkExhaustive name compType inWidths outWidths (reference: bigint list -> bigint list) =
    test name {
        for inputVals in allInputs inWidths do
            let expected = reference inputVals
            let actual = simulate compType inWidths outWidths inputVals
            if actual <> expected then
                failtest $"{name}: inputs %A{inputVals} gave %A{actual}, expected %A{expected}"
    }

let private gateRef (gateType: GateComponentType) (bits: bigint list) =
    let vals = bits |> List.map ((=) 1I)
    let result =
        match gateType with
        | And -> List.forall id vals
        | Or -> List.exists id vals
        | Xor -> (vals |> List.filter id |> List.length) % 2 = 1
        | Nand -> not (List.forall id vals)
        | Nor -> not (List.exists id vals)
        | Xnor -> (vals |> List.filter id |> List.length) % 2 = 0
    [ if result then 1I else 0I ]

let private w = 3 // exhaustive bus width

let tests =
    testList "ComponentSemantics" [
        // gates, 2 and 3 inputs
        for gateType in [ And; Or; Xor; Nand; Nor; Xnor ] do
            for n in [ 2; 3 ] do
                checkExhaustive
                    $"GateN %A{gateType} {n}"
                    (GateN(gateType, n))
                    (List.replicate n 1)
                    [ 1 ]
                    (gateRef gateType)

        // adders: inputs Cin, A, B; outputs Sum, Cout
        checkExhaustive "NbitsAdder" (NbitsAdder w) [ 1; w; w ] [ w; 1 ] (fun (In3(cin, a, b)) ->
            let sum = a + b + cin
            [ mask w sum; sum >>> w ])
        checkExhaustive "NbitsAdderNoCin" (NbitsAdderNoCin w) [ w; w ] [ w; 1 ] (fun (In2(a, b)) ->
            let sum = a + b
            [ mask w sum; sum >>> w ])
        checkExhaustive "NbitsAdderNoCout" (NbitsAdderNoCout w) [ 1; w; w ] [ w ] (fun (In3(cin, a, b)) ->
            [ mask w (a + b + cin) ])
        checkExhaustive "NbitsAdderNoCinCout" (NbitsAdderNoCinCout w) [ w; w ] [ w ] (fun (In2(a, b)) ->
            [ mask w (a + b) ])

        // n-bit logic
        checkExhaustive "NbitsXor" (NbitsXor(w, None)) [ w; w ] [ w ] (fun (In2(a, b)) -> [ a ^^^ b ])
        checkExhaustive "NbitsXor multiply" (NbitsXor(w, Some Multiply)) [ w; w ] [ w ] (fun (In2(a, b)) ->
            [ mask w (a * b) ])
        checkExhaustive "NbitsAnd" (NbitsAnd w) [ w; w ] [ w ] (fun (In2(a, b)) -> [ a &&& b ])
        checkExhaustive "NbitsOr" (NbitsOr w) [ w; w ] [ w ] (fun (In2(a, b)) -> [ a ||| b ])
        checkExhaustive "NbitsNot" (NbitsNot w) [ w ] [ w ] (fun (In1(a)) -> [ a ^^^ ((1I <<< w) - 1I) ])
        checkExhaustive "NbitSpreader" (NbitSpreader w) [ 1 ] [ w ] (fun (In1(a)) ->
            [ if a = 1I then (1I <<< w) - 1I else 0I ])

        // multiplexing: select is the last input
        checkExhaustive "Mux2" Mux2 [ w; w; 1 ] [ w ] (fun (In3(a, b, sel)) -> [ if sel = 0I then a else b ])
        checkExhaustive "Mux4" Mux4 [ w; w; w; w; 2 ] [ w ] (fun (In5(a, b, c, d, sel)) ->
            [ [ a; b; c; d ].[int sel] ])
        checkExhaustive "Demux2" Demux2 [ w; 1 ] [ w; w ] (fun (In2(a, sel)) ->
            if sel = 0I then [ a; 0I ] else [ 0I; a ])

        // bus manipulation: MergeWires/SplitWire are little-endian, first port is LSBs
        checkExhaustive "MergeWires" MergeWires [ 2; w ] [ 2 + w ] (fun (In2(a, b)) -> [ (b <<< 2) ||| a ])
        checkExhaustive "SplitWire" (SplitWire 2) [ 2 + w ] [ 2; w ] (fun (In1(a)) ->
            [ mask 2 a; a >>> 2 ])
        checkExhaustive "MergeN" (MergeN 3) [ 1; 2; 2 ] [ 5 ] (fun (In3(a, b, c)) ->
            [ a ||| (b <<< 1) ||| (c <<< 3) ])
        // SplitN slices need not tile the input: slice 1 starts at bit 1, slice 2 at bit 3
        checkExhaustive "SplitN" (SplitN(2, [ 2; 2 ], [ 1; 3 ])) [ 5 ] [ 2; 2 ] (fun (In1(a)) ->
            [ mask 2 (a >>> 1); mask 2 (a >>> 3) ])
        checkExhaustive "BusSelection" (BusSelection(2, 1)) [ w + 2 ] [ 2 ] (fun (In1(a)) ->
            [ mask 2 (a >>> 1) ])
        checkExhaustive "BusCompare1" (BusCompare1(w, 5I, "5")) [ w ] [ 1 ] (fun (In1(a)) ->
            [ if a = 5I then 1I else 0I ])

        // shifts: input 0 data, input 1 amount; 2-bit shifter covers amounts 0..3 at width 3,
        // including shifting by >= the bus width
        checkExhaustive "Shift LSL" (Shift(w, 2, LSL)) [ w; 2 ] [ w ] (fun (In2(a, amt)) ->
            [ if amt >= bigint w then 0I else mask w (a <<< int amt) ])
        checkExhaustive "Shift LSR" (Shift(w, 2, LSR)) [ w; 2 ] [ w ] (fun (In2(a, amt)) ->
            [ if amt >= bigint w then 0I else a >>> int amt ])
        checkExhaustive "Shift ASR" (Shift(w, 2, ASR)) [ w; 2 ] [ w ] (fun (In2(a, amt)) ->
            let signSet = a >>> (w - 1) = 1I
            let allOnes = (1I <<< w) - 1I
            let result =
                if amt >= bigint w then
                    if signSet then allOnes else 0I
                else
                    let shifted = a >>> int amt
                    if signSet then
                        shifted ||| (allOnes &&& (allOnes <<< (w - int amt)))
                    else
                        shifted
            [ result ])

        // constants: the stored value must be the width-wide two's complement bit pattern
        test "Constant values are masked to width" {
            let check width value expected =
                let actual = simulate (Constant1(width, value, string value)) [] [ width ] []
                if actual <> [ expected ] then
                    failtest $"Constant {value} at width {width} gave %A{actual}, expected {expected}"
            check 4 5I 5I
            check 4 -1I 15I
            check 32 -1I ((1I <<< 32) - 1I)
            check 40 -1I ((1I <<< 40) - 1I)
            check 40 -2I ((1I <<< 40) - 2I)
        }

        // multiply at width exactly 32: naive masking with (1u <<< 32) - 1u gives a zero mask
        test "Multiply at width 32 wraps correctly" {
            let a, b = (1I <<< 31) + 3I, 5I
            let expected = (a * b) % (1I <<< 32)
            let actual = simulate (NbitsXor(32, Some Multiply)) [ 32; 32 ] [ 32 ] [ a; b ]
            Expect.equal actual [ expected ] "32-bit multiply"
        }

        // clocked components: outputs reflect the previous cycle's inputs
        test "Register" {
            let stimuli = [ [ 5I ]; [ 2I ]; [ 7I ]; [ 0I ] ]
            let outs = simulateClocked (Register w) [ w ] [ w ] stimuli
            Expect.equal outs [ [ 0I ]; [ 5I ]; [ 2I ]; [ 7I ] ] "register delays input by one cycle"
        }
        test "DFF" {
            let stimuli = [ [ 1I ]; [ 0I ]; [ 1I ]; [ 1I ] ]
            let outs = simulateClocked DFF [ 1 ] [ 1 ] stimuli
            Expect.equal outs [ [ 0I ]; [ 1I ]; [ 0I ]; [ 1I ] ] "DFF delays input by one cycle"
        }
        test "RegisterE" {
            // inputs: data, enable
            let stimuli = [ [ 5I; 1I ]; [ 2I; 0I ]; [ 7I; 1I ]; [ 1I; 0I ] ]
            let outs = simulateClocked (RegisterE w) [ w; 1 ] [ w ] stimuli
            Expect.equal outs [ [ 0I ]; [ 5I ]; [ 5I ]; [ 7I ] ] "RegisterE holds value when disabled"
        }
        test "Counter" {
            // inputs: load data, load, enable
            let stimuli = List.replicate 5 [ 0I; 0I; 1I ]
            let outs = simulateClocked (Counter w) [ w; 1; 1 ] [ w ] stimuli
            Expect.equal outs [ [ 0I ]; [ 1I ]; [ 2I ]; [ 3I ]; [ 4I ] ] "counter increments when enabled"
        }
        // Every counter wraps to 0 at 2^width, and each variant has its own copy of the
        // increment. Width 32 is the case to watch: the wrap cannot be tested against
        // 1 <<< width there, since at 32 bits that is 1.
        let countUp ticks = [ 0 .. ticks - 1 ] |> List.map (fun i -> [ bigint (i % (1 <<< w)) ])
        test "Counter wraps at the bus width" {
            let stimuli = List.replicate 10 [ 0I; 0I; 1I ]
            let outs = simulateClocked (Counter w) [ w; 1; 1 ] [ w ] stimuli
            Expect.equal outs (countUp 10) "counter wraps to 0 after 2^w - 1"
        }
        test "Counter wraps at width 32" {
            // load the top value, then count past it
            let top = 4294967295I
            let stimuli = [ [ top; 1I; 1I ]; [ 0I; 0I; 1I ]; [ 0I; 0I; 1I ] ]
            let outs = simulateClocked (Counter 32) [ 32; 1; 1 ] [ 32 ] stimuli
            Expect.equal outs [ [ 0I ]; [ top ]; [ 0I ] ] "32-bit counter wraps to 0 after 2^32 - 1"
        }
        test "CounterNoEnable" {
            // inputs: load data, load - counts on every cycle it is not loading
            let stimuli = [ [ 5I; 1I ]; [ 0I; 0I ]; [ 0I; 0I ]; [ 0I; 0I ] ]
            let outs = simulateClocked (CounterNoEnable w) [ w; 1 ] [ w ] stimuli
            Expect.equal outs [ [ 0I ]; [ 5I ]; [ 6I ]; [ 7I ] ] "loads, then counts from the loaded value"
        }
        test "CounterNoEnable wraps at the bus width" {
            let stimuli = List.replicate 10 [ 0I; 0I ]
            let outs = simulateClocked (CounterNoEnable w) [ w; 1 ] [ w ] stimuli
            Expect.equal outs (countUp 10) "counter wraps to 0 after 2^w - 1"
        }
        test "CounterNoLoad" {
            // input: enable
            let stimuli = [ [ 1I ]; [ 1I ]; [ 0I ]; [ 1I ] ]
            let outs = simulateClocked (CounterNoLoad w) [ 1 ] [ w ] stimuli
            Expect.equal outs [ [ 0I ]; [ 1I ]; [ 2I ]; [ 2I ] ] "counts when enabled, holds when not"
        }
        test "CounterNoLoad wraps at the bus width" {
            let stimuli = List.replicate 10 [ 1I ]
            let outs = simulateClocked (CounterNoLoad w) [ 1 ] [ w ] stimuli
            Expect.equal outs (countUp 10) "counter wraps to 0 after 2^w - 1"
        }
        test "CounterNoEnableLoad wraps at the bus width" {
            // no inputs at all: free running
            let outs = simulateClocked (CounterNoEnableLoad w) [] [ w ] (List.replicate 10 [])
            Expect.equal outs (countUp 10) "counter wraps to 0 after 2^w - 1"
        }

        // AsyncRAM1: writes land in the state one cycle later; the async read must use the
        // *current* cycle's address. Ticks 1-3 read a different address from the one driven
        // on the previous tick, so a stale-address read gives the wrong word.
        // inputs: address, data-in, write
        let asyncRamStimuli =
            [ [ 1I; 11I; 1I ]   // write 11 -> addr 1
              [ 2I; 22I; 1I ]   // write 22 -> addr 2; read addr 2 (still 0)
              [ 1I; 0I; 0I ]    // read addr 1 -> 11
              [ 2I; 0I; 0I ] ]  // read addr 2 -> 22
        let asyncRamExpected = [ [ 0I ]; [ 0I ]; [ 11I ]; [ 22I ] ]
        test "AsyncRAM1" {
            let mem = { Init = FromData; AddressWidth = 4; WordWidth = 8; Data = Map.empty; Comments = None }
            let outs = simulateClocked (AsyncRAM1 mem) [ 4; 8; 1 ] [ 8 ] asyncRamStimuli
            Expect.equal outs asyncRamExpected "async read uses the current cycle's address"
        }
        // The ROMs: contents fixed, so FastReducers reads them into a table at build time.
        // The last tick reads an address the memory has no entry for, which must give 0.
        let romMem =
            { Init = FromData
              AddressWidth = 4
              WordWidth = 8
              Data = Map [ 1I, 11I; 2I, 22I; 3I, 33I ]
              Comments = None }
        test "ROM1" {
            let stimuli = [ [ 1I ]; [ 2I ]; [ 3I ]; [ 0I ]; [ 0I ] ]
            let outs = simulateClocked (ROM1 romMem) [ 4 ] [ 8 ] stimuli
            Expect.equal outs [ [ 0I ]; [ 11I ]; [ 22I ]; [ 33I ]; [ 0I ] ]
                "synchronous ROM reads the address presented on the previous clock edge"
        }
        test "AsyncROM1" {
            for addr, expected in [ 1I, 11I; 2I, 22I; 3I, 33I; 0I, 0I; 15I, 0I ] do
                Expect.equal (simulate (AsyncROM1 romMem) [ 4 ] [ 8 ] [ addr ]) [ expected ]
                    $"asynchronous ROM reads address {addr} combinationally"
        }
        test "AsyncRAM1 with >32-bit address" {
            // bigint address, uint32 data: exercises the mixed-width FastSim path
            let mem = { Init = FromData; AddressWidth = 33; WordWidth = 8; Data = Map.empty; Comments = None }
            let outs = simulateClocked (AsyncRAM1 mem) [ 33; 8; 1 ] [ 8 ] asyncRamStimuli
            Expect.equal outs asyncRamExpected "async read uses the current cycle's address"
        }
    ]
