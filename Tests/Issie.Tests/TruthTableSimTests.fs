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
        test "ROM with a word wider than 32 bits is not truncated" {
            // AddressWidth <= 32 < WordWidth used to send the read down the uint32 branch
            let big = (1I <<< 39) + 12345I
            let m = mem 4 40 (Map [ 2I, big ])
            let res = simFData (AsyncROM1 m) [ 4 ] [ 40 ] [ 2I ]
            Expect.equal res.Head.Width 40 "data out is WordWidth"
            Expect.equal res.Head.GetBigInt big "a 40-bit word survives the read"
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
      Not, [ 1 ], [ 1 ], [ [ 0I ]; [ 1I ] ] ]

let private parityTests =
    testList "FData reducer agrees with the fast reducer" [
        for compType, inWidths, outWidths, stimuli in parityCases do
            test $"%A{compType}" {
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

let tests =
    testList "TruthTableSim" [ maskingTests; memoryTests; parityTests ]
