/// Tests for the FData ("truth table") fast simulation. This is a second, independent
/// implementation of every component reducer, so its main risk is silently drifting away
/// from the uint32/bigint reducer in EvalReference.fs. The parity tests below run the same
/// circuit through both and require them to agree.
module TruthTableSimTests

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasBuilder

let private maxArraySize = 40

/// one Input1 per input width, the component under test, one Output per output width
let private dutCanvas (compType: ComponentType) (inWidths: int list) (outWidths: int list) : CanvasState =
    let dut = makeComp "dut" (List.length inWidths) (List.length outWidths) compType "DUT"
    let ins = inWidths |> List.mapi (fun i w -> makeComp $"in{i}" 0 1 (Input1(w, None)) $"I{i}")
    let outs = outWidths |> List.mapi (fun i w -> makeComp $"out{i}" 1 0 (Output w) $"O{i}")

    let conns =
        (ins |> List.mapi (fun i c -> conn c 0 dut i))
        @ (outs |> List.mapi (fun i c -> conn dut i c 0))

    dut :: ins @ outs, conns

let private inputIndex (label: string) = int (label.Substring 1)

/// Run the circuit through the FData (truth table) simulation, returning each output's FData
let private simFData (compType: ComponentType) inWidths outWidths (inputVals: bigint list) : FastData list =
    let canvas = dutCanvas compType inWidths outWidths
    let ldc = makeLdc "tt_sheet" None canvas

    match Simulator.startCircuitSimulationFData maxArraySize "tt_sheet" canvas [ ldc ] with
    | Error e -> failtest $"FData simulation setup failed for %A{compType}: %A{e}"
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, ComponentLabel label, width) ->
            let fd = NumberHelpers.convertBigintToFastData width inputVals[inputIndex label]
            FastExtract.changeInputFData cid (IData fd) 0 simData.FastSim)

        simData.Outputs
        |> List.sortBy (fun (_, ComponentLabel l, _) -> l)
        |> List.map (fun (cid, _, _) ->
            match FastExtract.extractFastSimulationOutputFData simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> fd
            | IAlg e -> failtest $"unexpected algebra %A{e} from %A{compType}")

/// Run the same circuit through the normal uint32/bigint simulation
let private simFast (compType: ComponentType) inWidths outWidths (inputVals: bigint list) : bigint list =
    let canvas = dutCanvas compType inWidths outWidths
    let ldc = makeLdc "fast_sheet" None canvas

    match Simulator.startCircuitSimulation maxArraySize "fast_sheet" canvas [ ldc ] with
    | Error e -> failtest $"simulation setup failed for %A{compType}: %A{e}"
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, ComponentLabel label, width) ->
            let fd = NumberHelpers.convertBigintToFastData width inputVals[inputIndex label]
            FastExtract.changeInput cid (IData fd) 0 simData.FastSim)

        simData.Outputs
        |> List.sortBy (fun (_, ComponentLabel l, _) -> l)
        |> List.map (fun (cid, _, _) ->
            match FastExtract.extractFastSimulationOutput simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> fd.GetBigInt
            | IAlg _ -> failtest "unexpected algebra")

// ---- masking ----

let private maskingTests =
    testList "masking invariant" [
        test "NbitsNot masks to the bus width" {
            // this reducer used to be a bare ~~~a, so NOT of 0101 at width 4 came out as
            // 0xFFFFFFFA - every bit above the bus width set
            for width, input, expected in
                [ 1, 0I, 1I // NOT 0 = 1
                  4, 5I, 10I // NOT 0101 = 1010
                  4, 0I, 15I
                  8, 165I, 90I // NOT 10100101 = 01011010
                  31, 0I, 2147483647I ] do
                let res = simFData (NbitsNot width) [ width ] [ width ] [ input ]
                Expect.equal res.Head.GetBigInt expected $"NOT {input} at width {width}"
        }
        test "NbitsNot at width 32 keeps all 32 bits" {
            let res = simFData (NbitsNot 32) [ 32 ] [ 32 ] [ 0I ]
            Expect.equal res.Head.GetBigInt 4294967295I "NOT 0 at width 32 is 0xFFFFFFFF"
        }
        testProperty "every FData output lies within its declared width"
        <| fun (a: uint32) (b: uint32) ->
            let width = 6
            let m = (1I <<< width) - 1I
            let a, b = bigint a &&& m, bigint b &&& m

            [ NbitsNot width, [ a ]
              NbitsAnd width, [ a; b ]
              NbitsOr width, [ a; b ]
              NbitsXor(width, None), [ a; b ]
              NbitsXor(width, Some Multiply), [ a; b ] ]
            |> List.forall (fun (ct, args) ->
                let inWidths = List.replicate (List.length args) width
                simFData ct inWidths [ width ] args
                |> List.forall (fun fd -> fd.GetBigInt >= 0I && fd.GetBigInt < (1I <<< width)))
    ]

// ---- memory word width ----

let private memoryTests =
    let mem addressWidth wordWidth data =
        { Init = FromData
          AddressWidth = addressWidth
          WordWidth = wordWidth
          Data = data
          Comments = None }

    testList "memory" [
        test "ROM output is WordWidth wide, not AddressWidth" {
            // readMemoryFData used to width the value it read with mem.AddressWidth
            let m = mem 4 8 (Map [ 1I, 200I ])
            let res = simFData (AsyncROM1 m) [ 4 ] [ 8 ] [ 1I ]
            Expect.equal res.Head.Width 8 "data out is WordWidth"
            Expect.equal res.Head.GetBigInt 200I "value read"
        }
        test "an unwritten ROM address reads as zero" {
            let m = mem 4 8 (Map [ 1I, 200I ])
            let res = simFData (AsyncROM1 m) [ 4 ] [ 8 ] [ 3I ]
            Expect.equal res.Head.GetBigInt 0I "sparse representation: missing means zero"
        }
    ]

// ---- parity between the two reducers ----

/// (component, input widths, output widths, stimuli)
let private parityCases: (ComponentType * int list * int list * bigint list list) list =
    [ NbitsNot 4, [ 4 ], [ 4 ], [ [ 0I ]; [ 5I ]; [ 15I ] ]
      NbitsNot 1, [ 1 ], [ 1 ], [ [ 0I ]; [ 1I ] ]
      NbitsAnd 4, [ 4; 4 ], [ 4 ], [ [ 12I; 10I ]; [ 15I; 0I ] ]
      NbitsOr 4, [ 4; 4 ], [ 4 ], [ [ 12I; 10I ]; [ 0I; 0I ] ]
      NbitsXor(4, None), [ 4; 4 ], [ 4 ], [ [ 12I; 10I ]; [ 15I; 15I ] ]
      NbitsXor(4, Some Multiply), [ 4; 4 ], [ 4 ], [ [ 3I; 5I ]; [ 15I; 15I ] ]
      NbitsAdder 4, [ 1; 4; 4 ], [ 4; 1 ], [ [ 0I; 7I; 9I ]; [ 1I; 15I; 15I ]; [ 0I; 1I; 1I ] ]
      NbitsAdderNoCin 4, [ 4; 4 ], [ 4; 1 ], [ [ 7I; 9I ]; [ 15I; 15I ] ]
      NbitSpreader 4, [ 1 ], [ 4 ], [ [ 0I ]; [ 1I ] ]
      Shift(4, 2, LSL), [ 4; 2 ], [ 4 ], [ [ 5I; 1I ]; [ 5I; 0I ]; [ 5I; 3I ] ]
      Shift(4, 2, LSR), [ 4; 2 ], [ 4 ], [ [ 10I; 1I ]; [ 10I; 3I ] ]
      Shift(4, 2, ASR), [ 4; 2 ], [ 4 ], [ [ 10I; 1I ]; [ 5I; 1I ]; [ 12I; 2I ] ]
      BusSelection(2, 1), [ 4 ], [ 2 ], [ [ 10I ]; [ 5I ] ]
      BusCompare1(4, 5I, "5"), [ 4 ], [ 1 ], [ [ 5I ]; [ 6I ] ]
      MergeWires, [ 2; 2 ], [ 4 ], [ [ 3I; 1I ]; [ 0I; 3I ] ]
      SplitWire 2, [ 4 ], [ 2; 2 ], [ [ 13I ]; [ 0I ] ]
      Mux2, [ 4; 4; 1 ], [ 4 ], [ [ 3I; 12I; 0I ]; [ 3I; 12I; 1I ] ]
      Demux2, [ 4; 1 ], [ 4; 4 ], [ [ 9I; 0I ]; [ 9I; 1I ] ]
      GateN(And, 2), [ 1; 1 ], [ 1 ], [ [ 1I; 1I ]; [ 1I; 0I ] ]
      GateN(Nand, 3), [ 1; 1; 1 ], [ 1 ], [ [ 1I; 1I; 1I ]; [ 1I; 1I; 0I ] ]
      Not, [ 1 ], [ 1 ], [ [ 0I ]; [ 1I ] ]

      // The shapes above are the ones a truth table is usually asked for. These are the rest of
      // what EvalAlgebraic implements: the wider multiplexers and demultiplexers, the n-way merge
      // and split, and the two adder variants with a port missing. Each is a separate branch of
      // that file, and a branch nothing compares against the specification is one that can drift.
      NbitsAdderNoCout 4, [ 1; 4; 4 ], [ 4 ], [ [ 1I; 15I; 15I ]; [ 0I; 7I; 9I ] ]
      NbitsAdderNoCinCout 4, [ 4; 4 ], [ 4 ], [ [ 7I; 9I ]; [ 15I; 15I ] ]
      Mux4, [ 4; 4; 4; 4; 2 ], [ 4 ], [ [ 1I; 2I; 3I; 4I; 0I ]; [ 1I; 2I; 3I; 4I; 3I ] ]
      Mux8, [ 4; 4; 4; 4; 4; 4; 4; 4; 3 ], [ 4 ],
        [ [ 1I; 2I; 3I; 4I; 5I; 6I; 7I; 8I; 0I ]; [ 1I; 2I; 3I; 4I; 5I; 6I; 7I; 8I; 7I ] ]
      Demux4, [ 4; 2 ], [ 4; 4; 4; 4 ], [ [ 9I; 0I ]; [ 9I; 2I ] ]
      Demux8, [ 4; 3 ], [ 4; 4; 4; 4; 4; 4; 4; 4 ], [ [ 9I; 0I ]; [ 9I; 7I ] ]
      MergeN 3, [ 1; 2; 2 ], [ 5 ], [ [ 1I; 2I; 3I ]; [ 0I; 0I; 0I ] ]
      // slices that neither tile the input nor start at zero
      SplitN(2, [ 2; 2 ], [ 1; 3 ]), [ 5 ], [ 2; 2 ], [ [ 26I ]; [ 0I ]; [ 31I ] ] ]

let private parityTests =
    testList "FData reducer agrees with the fast reducer" [
        for compType, inWidths, outWidths, stimuli in parityCases do
            // the widths are in the name because some component types carry none of their own
            test $"%A{compType} at %A{inWidths}" {
                for inputVals in stimuli do
                    let fast = simFast compType inWidths outWidths inputVals
                    let ttData = simFData compType inWidths outWidths inputVals
                    let tt = ttData |> List.map (fun fd -> fd.GetBigInt)

                    Expect.equal tt fast $"outputs for inputs %A{inputVals}"

                    List.zip ttData outWidths
                    |> List.iteri (fun i (fd, w) ->
                        Expect.equal fd.Width w $"output {i} width for inputs %A{inputVals}")
            }
    ]

/// The boundary: a truth table is for logic narrow enough to tabulate, and says so.
let private widthLimitTests =
    testList "bus width limit" [
        test "a bus wider than the limit is refused, naming the component and the width" {
            let canvas = dutCanvas (NbitsAnd 40) [ 40; 40 ] [ 40 ]
            let ldc = makeLdc "wide_sheet" None canvas
            match Simulator.startCircuitSimulationFData maxArraySize "wide_sheet" canvas [ ldc ] with
            | Ok _ -> failtest "a 40-bit design should not build an FData simulation"
            | Error e ->
                let msg = SimGraphTypes.errMsg e.ErrType
                Expect.stringContains msg "40-bit" "says how wide the offending bus is"
                Expect.stringContains msg "Wave Simulation" "points at what does handle wide designs"
                Expect.isNonEmpty e.ComponentsAffected "and names components to highlight"
        }
        test "the limit itself is allowed" {
            // 32 bits is the widest the algebraic evaluator carries, and must not be refused
            let res = simFData (NbitsNot 32) [ 32 ] [ 32 ] [ 0I ]
            Expect.equal res.Head.GetBigInt 4294967295I "a 32-bit design still tabulates"
        }
    ]

let tests =
    testList "TruthTableSim" [ maskingTests; memoryTests; parityTests; widthLimitTests ]
