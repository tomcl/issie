/// Tests for truth-table algebraic simulation: direct unit tests of the evalExp
/// simplifier, of the append-handling helpers, and end-to-end symbolic simulation
/// through the FData fast simulation.
module AlgebraTests

open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasBuilder

// --- expression building helpers ---

let private io (name: string) (w: int) : SimulationIO =
    ComponentId name, ComponentLabel name, w

let private term name w : FastAlgExp = SingleTerm(io name w)
let private wordLit (w: int) (v: uint32) = DataLiteral { Dat = Word v; Width = w }
let private bigLit (w: int) (v: bigint) = DataLiteral { Dat = BigWord v; Width = w }

let private a4 = term "A" 4
let private b4 = term "B" 4
let private a1 = term "A" 1
let private b1 = term "B" 1
let private cin1 = term "CIN" 1

/// The set of flattened arithmetic terms of an expression
let private termsOf exp = flattenNestedArithmetic exp |> Set.ofList

let private evalTests =
    testList "evalExp" [
        test "OR with a general constant keeps OR" {
            let expr = BinaryExp(a4, BitOrOp, wordLit 4 5u)
            Expect.equal (evalExp expr) expr "A | 5 must stay an OR, not become an AND"
        }
        test "OR with all-ones is all-ones" {
            let expr = BinaryExp(a4, BitOrOp, wordLit 4 15u)
            Expect.equal (evalExp expr) (wordLit 4 15u) "annulment"
        }
        test "A or not A is all-ones" {
            let expr = BinaryExp(a4, BitOrOp, UnaryExp(NotOp, a4))
            Expect.equal (evalExp expr) (wordLit 4 15u) "complement, width-wide"
        }
        test "not A or A is all-ones" {
            let expr = BinaryExp(UnaryExp(NotOp, a4), BitOrOp, a4)
            Expect.equal (evalExp expr) (wordLit 4 15u) "complement, NOT on the left"
        }
        test "A and not A is zero" {
            let expr = BinaryExp(a4, BitAndOp, UnaryExp(NotOp, a4))
            Expect.equal (evalExp expr) (wordLit 4 0u) "complement"
        }
        test "not A and A is zero" {
            let expr = BinaryExp(UnaryExp(NotOp, a4), BitAndOp, a4)
            Expect.equal (evalExp expr) (wordLit 4 0u) "complement, NOT on the left"
        }
        test "XOR with a general constant stays XOR" {
            let expr = BinaryExp(a4, BitXorOp, wordLit 4 5u)
            Expect.equal (evalExp expr) expr "A XOR 5 is not A + 5"
        }
        test "XOR with all-ones is NOT" {
            let expr = BinaryExp(a4, BitXorOp, wordLit 4 15u)
            Expect.equal (evalExp expr) (UnaryExp(NotOp, a4)) "XOR all-ones"
        }
        test "A XOR A is zero" {
            let expr = BinaryExp(a4, BitXorOp, a4)
            Expect.equal (evalExp expr) (wordLit 4 0u) "self-XOR at width 4"
        }
        test "width-1 XOR becomes addition" {
            let result = evalExp (BinaryExp(a1, BitXorOp, b1))
            Expect.equal (termsOf result) (Set.ofList [ a1; b1 ]) "A XOR B = A + B mod 2"
            match result with
            | BinaryExp(_, AddOp, _) -> ()
            | other -> failtest $"expected an addition, got %A{other}"
        }
        test "width-1 A XOR A is zero" {
            let expr = BinaryExp(a1, BitXorOp, a1)
            Expect.equal (evalExp expr) (wordLit 1 0u) "term count reduces mod 2"
        }
        test "constant total that truncates to zero leaves no term" {
            let expr = BinaryExp(BinaryExp(a4, AddOp, wordLit 4 8u), AddOp, wordLit 4 8u)
            Expect.equal (evalExp expr) a4 "8 + 8 = 16 = 0 mod 16: no spurious - 0"
        }
        test "wide AND with all-ones is identity" {
            let expr = BinaryExp(term "W" 40, BitAndOp, bigLit 40 (bigIntMask 40))
            Expect.equal (evalExp expr) (term "W" 40) "BigWord all-ones identity"
        }
        test "wide XOR with all-ones is NOT" {
            let expr = BinaryExp(term "W" 40, BitXorOp, bigLit 40 (bigIntMask 40))
            Expect.equal (evalExp expr) (UnaryExp(NotOp, term "W" 40)) "BigWord all-ones"
        }
        test "wide OR with zero is identity" {
            let expr = BinaryExp(term "W" 40, BitOrOp, bigLit 40 0I)
            Expect.equal (evalExp expr) (term "W" 40) "BigWord zero identity"
        }
        test "full-adder carry shape is recognised" {
            // CIN & (A + B)  |  A & B   ->  carry(CIN + (A + B))
            let expr =
                BinaryExp(
                    BinaryExp(cin1, BitAndOp, BinaryExp(a1, AddOp, b1)),
                    BitOrOp,
                    BinaryExp(a1, BitAndOp, b1))
            match evalExp expr with
            | UnaryExp(CarryOfOp, _) -> ()
            | other -> failtest $"expected carry recognition, got %A{other}"
        }
        test "gate-built adder sum flattens to a three-term addition" {
            // (A XOR B) XOR CIN at width 1
            let expr = BinaryExp(BinaryExp(a1, BitXorOp, b1), BitXorOp, cin1)
            let result = evalExp expr
            Expect.equal (termsOf result) (Set.ofList [ a1; b1; cin1 ]) "A + B + CIN"
        }
    ]

let private appendTests =
    testList "append handling" [
        test "adjacent ranges merge only in MSB-first order" {
            let high = UnaryExp(BitRangeOp(2, 3), a4)
            let low = UnaryExp(BitRangeOp(0, 1), a4)
            Expect.equal
                (foldAppends [ high; low ])
                [ UnaryExp(BitRangeOp(0, 3), a4) ]
                "A[3:2] followed by A[1:0] merges to A[3:0]"
            Expect.equal
                (foldAppends [ low; high ])
                [ low; high ]
                "A[1:0] followed by A[3:2] is a swapped-halves value and must not merge"
        }
        test "per-bit operations collapse only in MSB-first bit order" {
            let a2, b2 = term "A" 2, term "B" 2
            let bitOp i =
                BinaryExp(UnaryExp(BitRangeOp(i, i), a2), BitAndOp, UnaryExp(BitRangeOp(i, i), b2))
            Expect.equal
                (tryBitwiseOperation [ bitOp 1; bitOp 0 ])
                (Some(BinaryExp(a2, BitAndOp, b2)))
                "descending bit order collapses to the bus operation"
            Expect.equal (tryBitwiseOperation [ bitOp 0; bitOp 1 ]) None
                "ascending bit order is a bit-reversed value and must not collapse"
        }
    ]

// --- end-to-end: FData simulation with algebraic inputs ---

let private maxArraySize = 4

let private dutCanvas (compType: ComponentType) (inWidths: int list) (outWidths: int list) : CanvasState =
    let dut = makeComp "dut" (List.length inWidths) (List.length outWidths) compType "DUT"
    let ins = inWidths |> List.mapi (fun i w -> makeComp $"in{i}" 0 1 (Input1(w, None)) $"I{i}")
    let outs = outWidths |> List.mapi (fun i w -> makeComp $"out{i}" 1 0 (Output w) $"O{i}")
    let conns =
        (ins |> List.mapi (fun i c -> conn c 0 dut i))
        @ (outs |> List.mapi (fun i c -> conn dut i c 0))
    dut :: ins @ outs, conns

/// Simulate one combinational component with every `I`-labelled input algebraic and
/// return the FData on each output, in output label order
let private simulateAlg (canvas: CanvasState) (name: string) (numericInputs: Map<string, FastData>) : FData list =
    let ldc = makeLdc name None canvas
    match Simulator.startCircuitSimulationFData maxArraySize name canvas [ ldc ] with
    | Error e -> failtest $"FData simulation setup failed: %A{e}"
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun ((cid, ComponentLabel label, _) as simIO) ->
            match Map.tryFind label numericInputs with
            | Some fd -> FastExtract.changeInputFData cid (IData fd) 0 simData.FastSim
            | None -> FastExtract.changeInputFData cid (IAlg(SingleTerm simIO)) 0 simData.FastSim)
        simData.Outputs
        |> List.sortBy (fun (_, ComponentLabel label, _) -> label)
        |> List.map (fun (cid, _, _) ->
            match FastExtract.extractFastSimulationOutputFData simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> Data fd
            | IAlg exp -> Alg exp)

/// The SimulationIO of the input component labelled I0 etc, as the simulator sees it
let private inputTerm (canvas: CanvasState) (label: string) (w: int) : FastAlgExp =
    let comps, _ = canvas
    let comp = comps |> List.find (fun c -> c.Label = label)
    SingleTerm(ComponentId comp.Id, ComponentLabel label, w)

let private e2eTests =
    testList "end-to-end" [
        test "AND gate produces an AND expression" {
            let canvas = dutCanvas (GateN(And, 2)) [ 1; 1 ] [ 1 ]
            match simulateAlg canvas "alg_and" Map.empty with
            | [ Alg(BinaryExp(_, BitAndOp, _)) ] -> ()
            | other -> failtest $"expected an AND expression, got %A{other}"
        }
        test "adder produces sum and carry expressions" {
            let canvas = dutCanvas (NbitsAdderNoCin 4) [ 4; 4 ] [ 4; 1 ]
            match simulateAlg canvas "alg_add" Map.empty with
            | [ Alg(BinaryExp(_, AddOp, _)); Alg(UnaryExp(CarryOfOp, _)) ] -> ()
            | other -> failtest $"expected sum + carry, got %A{other}"
        }
        test "XOR with a constant operand keeps its value" {
            // I0 algebraic, I1 driven with the constant 5
            let canvas = dutCanvas (NbitsXor(4, None)) [ 4; 4 ] [ 4 ]
            let five = { Dat = Word 5u; Width = 4 }
            match simulateAlg canvas "alg_xorc" (Map [ "I1", five ]) with
            | [ Alg(BinaryExp(exp, BitXorOp, DataLiteral d)) ] ->
                Expect.equal d five "the constant must survive at full width and value"
                Expect.equal exp (inputTerm canvas "I0" 4) "the algebraic operand"
            | other -> failtest $"expected XOR with the constant, got %A{other}"
        }
        test "multiply with an algebraic input is a clean refusal" {
            let canvas = dutCanvas (NbitsXor(4, Some Multiply)) [ 4; 4 ] [ 4 ]
            try
                simulateAlg canvas "alg_mult" Map.empty |> ignore
                failtest "expected AlgebraNotImplemented"
            with
            | AlgebraNotImplemented _ -> ()
        }
        test "split then merge reconstructs a contiguous slice" {
            // A -> SplitWire 2 -> MergeWires, straight through: out0->in0, out1->in1
            let a = makeComp "a" 0 1 (Input1(4, None)) "I0"
            let split = makeComp "split" 1 2 (SplitWire 2) "SPLIT"
            let merge = makeComp "merge" 2 1 MergeWires "MERGE"
            let out = makeComp "out" 1 0 (Output 4) "O0"
            let canvas =
                [ a; split; merge; out ],
                [ conn a 0 split 0; conn split 0 merge 0; conn split 1 merge 1; conn merge 0 out 0 ]
            match simulateAlg canvas "alg_splitmerge" Map.empty with
            | [ Alg(AppendExp [ UnaryExp(BitRangeOp(0, 3), exp) ]) ] ->
                Expect.equal exp (inputTerm canvas "I0" 4) "merged back to A[3:0]"
            | other -> failtest $"expected the merged slice A[3:0], got %A{other}"
        }
        test "swapped split halves do not merge" {
            // cross the wires: out0 (LSBs) -> merge MSB port, out1 -> merge LSB port
            let a = makeComp "a" 0 1 (Input1(4, None)) "I0"
            let split = makeComp "split" 1 2 (SplitWire 2) "SPLIT"
            let merge = makeComp "merge" 2 1 MergeWires "MERGE"
            let out = makeComp "out" 1 0 (Output 4) "O0"
            let canvas =
                [ a; split; merge; out ],
                [ conn a 0 split 0; conn split 0 merge 1; conn split 1 merge 0; conn merge 0 out 0 ]
            match simulateAlg canvas "alg_swapped" Map.empty with
            | [ Alg(AppendExp [ _; _ ]) ] -> ()
            | other -> failtest $"swapped halves must stay a two-part append, got %A{other}"
        }
        test "shift left by a constant is a shifted slice" {
            // I0 algebraic data, I1 numeric shift amount = 1
            let canvas = dutCanvas (Shift(4, 2, LSL)) [ 4; 2 ] [ 4 ]
            let one = { Dat = Word 1u; Width = 2 }
            match simulateAlg canvas "alg_lsl" (Map [ "I1", one ]) with
            | [ Alg(AppendExp [ UnaryExp(BitRangeOp(0, 2), exp); zero ]) ] ->
                Expect.equal exp (inputTerm canvas "I0" 4) "top bits are A[2:0]"
                Expect.equal zero (zeroLiteral 1) "LSB is 0"
            | other -> failtest $"expected A[2:0] || 0, got %A{other}"
        }
        test "arithmetic shift right by a constant replicates the sign bit" {
            let canvas = dutCanvas (Shift(4, 2, ASR)) [ 4; 2 ] [ 4 ]
            let one = { Dat = Word 1u; Width = 2 }
            match simulateAlg canvas "alg_asr" (Map [ "I1", one ]) with
            | [ Alg(AppendExp [ UnaryExp(BitRangeOp(3, 3), _); UnaryExp(BitRangeOp(1, 3), _) ]) ] -> ()
            | other -> failtest $"expected sign :: A[3:1], got %A{other}"
        }
        test "spreader replicates the bit" {
            let canvas = dutCanvas (NbitSpreader 4) [ 1 ] [ 4 ]
            match simulateAlg canvas "alg_spread" Map.empty with
            | [ Alg(AppendExp [ x1; x2; x3; x4 ]) ] ->
                let bit = inputTerm canvas "I0" 1
                Expect.equal [ x1; x2; x3; x4 ] (List.replicate 4 bit) "four copies of the input bit"
            | other -> failtest $"expected a four-way replicate, got %A{other}"
        }
    ]

let private katexTests =
    testList "expToKatex" [
        test "single-bit addition prints as XOR" {
            let s = expToKatex (BinaryExp(a1, AddOp, b1))
            Expect.equal s "A \\oplus B" "at width 1, + is XOR"
        }
        test "single-bit add chain prints as XOR chain" {
            let s = expToKatex (BinaryExp(BinaryExp(a1, AddOp, b1), AddOp, cin1))
            Expect.equal s "A \\oplus B \\oplus CIN" "full-adder sum shape"
        }
        test "single-bit subtraction prints as XOR" {
            let s = expToKatex (BinaryExp(a1, SubOp, b1))
            Expect.equal s "A \\oplus B" "at width 1, - is also XOR and negation is identity"
        }
        test "multi-bit addition keeps the plus sign" {
            let s = expToKatex (BinaryExp(a4, AddOp, b4))
            Expect.equal s "A + B" "arithmetic rendering above width 1"
        }
    ]

let tests =
    testList "Algebra" [ evalTests; appendTests; e2eTests; katexTests ]
