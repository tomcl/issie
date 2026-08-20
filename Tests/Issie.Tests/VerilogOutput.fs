/// Tests for the Verilog written by Verilog.getVerilog.
///
/// There is no Verilog compiler in the test environment, so these check the generated text
/// directly. Three kinds of check, in increasing order of usefulness:
///   - structural invariants that hold for every component (literals fit their declared width,
///     every net used is declared, instance names are unique)
///   - semantic checks, where the emitted expression is evaluated and compared against the same
///     reference the simulator is held to (gates)
///   - assertions pinning individual constructs that were previously emitted wrongly
module VerilogOutput

open System.Text.RegularExpressions
open Expecto
open CommonTypes
open CanvasBuilder

//------------------------------------------------------------------------------------------//
//------------------------------------ generating output -----------------------------------//
//------------------------------------------------------------------------------------------//

/// One component with an Input1 per input width and an Output per output width
let private dutCanvas (compType: ComponentType) (inWidths: int list) (outWidths: int list) : CanvasState =
    let dut = makeComp 1 (List.length inWidths) (List.length outWidths) compType "DUT"
    let ins = inWidths |> List.mapi (fun i w -> makeComp (2 + i) 0 1 (Input1(w, None)) $"I{i}")
    let outs = outWidths |> List.mapi (fun i w -> makeComp (2 + List.length inWidths + i) 1 0 (Output w) $"O{i}")
    let conns =
        (ins |> List.mapi (fun i c -> conn c 0 dut i))
        @ (outs |> List.mapi (fun i c -> conn dut i c 0))
    dut :: ins @ outs, conns

/// The Verilog for a canvas, or a test failure if it cannot be simulated
let verilogOf (mode: Verilog.VMode) (profile: Verilog.CompilationProfile) (canvas: CanvasState) : string =
    let ldc = makeLdc "v_sheet" None canvas
    match Simulator.startCircuitSimulation 40 "v_sheet" canvas [ ldc ] with
    | Error e -> failtestf "simulation setup failed: %A" e.ErrType
    | Ok sim -> Verilog.getVerilog mode sim.FastSim profile

/// The synthesis Verilog for a single component in isolation
let private dutVerilog compType inWidths outWidths =
    dutCanvas compType inWidths outWidths
    |> verilogOf Verilog.ForSynthesis Verilog.Release

//------------------------------------------------------------------------------------------//
//-------------------------------- reading the output back ---------------------------------//
//------------------------------------------------------------------------------------------//

/// The text of the top-level module. Memory modules are emitted before it, so everything from
/// "module main" on is main and nothing else.
let private mainModule (v: string) =
    match v.IndexOf "module main" with
    | -1 -> failtestf "no main module in output:\n%s" v
    | i -> v[i..]

/// The right-hand side of "assign <name> = ...;"
let private assignRhs (name: string) (v: string) =
    let m = Regex.Match(v, @"assign\s+" + Regex.Escape name + @"\s*=\s*([^;]*);")
    if not m.Success then failtestf "no assignment to %s in:\n%s" name v
    m.Groups[1].Value.Trim()

/// Names declared as a port, net or variable, one line at a time.
/// Only sound for the declaration style this module emits: one name per line.
let private declaredNames (v: string) =
    Regex.Matches(v, @"(?m)^\s*(?:input|output|inout|wire|reg)\s+(?:reg\s+)?(?:\[[^\]]*\]\s*)?([A-Za-z_][\w$]*)")
    |> Seq.map (fun m -> m.Groups[1].Value)
    |> Set.ofSeq

/// Names used in statements: everything that looks like an identifier outside declarations,
/// string literals and sized constants, minus the Verilog keywords this module emits.
let private usedNames (v: string) =
    let keywords =
        set [ "module"; "endmodule"; "assign"; "always"; "posedge"; "negedge"; "begin"; "end"
              "initial"; "if"; "else"; "case"; "endcase"; "default"; "integer"; "for"; "while"
              "input"; "output"; "inout"; "wire"; "reg"; "main" ]
    v
    |> fun s -> Regex.Replace(s, "\"[^\"]*\"", " ")                    // string literals
    // system functions. The lookbehind matters: multi-output components name their ports
    // NAME$o0, and without it the $o0 would be stripped and NAME left looking undeclared
    |> fun s -> Regex.Replace(s, @"(?<![\w$])\$[A-Za-z_]\w*", " ")     // $display, $signed, $time
    |> fun s -> Regex.Replace(s, @"\d+'[hbdoHBDO][\w?]+", " ")         // sized constants
    |> fun s -> Regex.Replace(s, @"(?m)^\s*(?:input|output|inout|wire|reg)\b[^;]*;", " ")
    |> fun s -> Regex.Matches(s, @"[A-Za-z_][\w$]*") |> Seq.map (fun m -> m.Value)
    |> Seq.filter (fun n -> not (keywords.Contains n))
    |> Set.ofSeq

/// (width, radix, digits) of every sized Verilog literal
let private sizedLiterals (v: string) =
    Regex.Matches(v, @"(\d+)'([hbdoHBDO])(\w+)")
    |> Seq.map (fun m -> int m.Groups[1].Value, System.Char.ToLower m.Groups[2].Value.[0], m.Groups[3].Value)
    |> List.ofSeq

/// (module, instance) of every module instantiation in main
let private instantiations (v: string) =
    Regex.Matches(mainModule v, @"(?m)^\s*([A-Za-z_][\w$]*)\s+([A-Za-z_][\w$]*)\s*\(")
    |> Seq.map (fun m -> m.Groups[1].Value, m.Groups[2].Value)
    |> Seq.filter (fun (m, _) -> m <> "module")
    |> List.ofSeq

//------------------------------------------------------------------------------------------//
//---------------------------- evaluating the gate expressions -----------------------------//
//------------------------------------------------------------------------------------------//

/// Evaluate the fragment of Verilog expression syntax the gate emitter uses: identifiers,
/// ! & | ^ and parentheses, over one-bit values. Precedence, tightest first: ! & ^ |.
/// Anything outside that fragment fails the test rather than being guessed at.
let evalBitExpr (env: Map<string, int>) (expr: string) : int =
    let chars =
        expr.Replace("&&", "&").Replace("||", "|")
        |> Seq.filter (fun c -> c <> ' ')
        |> List.ofSeq
    let rec pOr cs =
        let v, rest = pXor cs
        match rest with
        | '|' :: t -> let v2, r = pOr t in (v ||| v2), r
        | _ -> v, rest
    and pXor cs =
        let v, rest = pAnd cs
        match rest with
        | '^' :: t -> let v2, r = pXor t in (v ^^^ v2), r
        | _ -> v, rest
    and pAnd cs =
        let v, rest = pUnary cs
        match rest with
        | '&' :: t -> let v2, r = pAnd t in (v &&& v2), r
        | _ -> v, rest
    and pUnary cs =
        match cs with
        | '!' :: t -> let v, r = pUnary t in (1 - v), r
        | '(' :: t ->
            match pOr t with
            | v, ')' :: r -> v, r
            | _ -> failtestf "unbalanced parentheses in %s" expr
        | _ ->
            let name = cs |> List.takeWhile (fun c -> System.Char.IsLetterOrDigit c || c = '_' || c = '$')
            if name.IsEmpty then failtestf "cannot parse %s" expr
            let name = System.String(List.toArray name)
            match Map.tryFind name env with
            | Some v -> v, List.skip name.Length cs
            | None -> failtestf "unknown identifier %s in %s" name expr
    match pOr chars with
    | v, [] -> v
    | _, rest -> failtestf "unparsed trailing input %A in %s" rest expr

/// What an n-input gate of this type should compute - the same reference ComponentSemantics
/// holds the simulator to
let private gateRef (gateType: GateComponentType) (bits: int list) =
    let anyOn = List.exists ((=) 1) bits
    let allOn = List.forall ((=) 1) bits
    let oddParity = (bits |> List.filter ((=) 1) |> List.length) % 2 = 1
    match gateType with
    | And -> allOn
    | Or -> anyOn
    | Xor -> oddParity
    | Nand -> not allOn
    | Nor -> not anyOn
    | Xnor -> not oddParity
    |> fun b -> if b then 1 else 0

//------------------------------------------------------------------------------------------//
//------------------------------------- a mixed circuit ------------------------------------//
//------------------------------------------------------------------------------------------//

/// A sheet using a spread of component types at once, for the whole-file structural checks.
/// Deliberately has no memory: memory modules bring their own scope, and are tested separately.
let private mixedCanvas () : CanvasState =
    let i0 = makeComp 1 0 1 (Input1(8, None)) "I0"
    let i1 = makeComp 2 0 1 (Input1(8, None)) "I1"
    let i2 = makeComp 3 0 1 (Input1(1, None)) "I2"
    let k = makeComp 4 0 1 (Constant1(8, 200I, "200")) "K"
    let add = makeComp 5 3 2 (NbitsAdder 8) "ADD"
    let nt = makeComp 6 1 1 (NbitsNot 8) "NT"
    let mux = makeComp 7 3 1 Mux2 "MUX"
    let rg = makeComp 8 2 1 (RegisterE 8) "RG"
    let sel = makeComp 9 1 1 (BusSelection(3, 0)) "SEL"
    let sh = makeComp 10 2 1 (Shift(8, 3, ASR)) "SH"
    let nand3 = makeComp 11 3 1 (GateN(Nand, 3)) "NAND3"
    let cmp = makeComp 12 1 1 (BusCompare1(8, 200I, "200")) "CMP"
    let o0 = makeComp 13 1 0 (Output 8) "O0"
    let o1 = makeComp 14 1 0 (Output 1) "O1"
    let o2 = makeComp 15 1 0 (Output 8) "O2"
    let o3 = makeComp 16 1 0 (Output 1) "O3"
    let comps = [ i0; i1; i2; k; add; nt; mux; rg; sel; sh; nand3; cmp; o0; o1; o2; o3 ]
    let conns =
        [ conn i2 0 add 0; conn i0 0 add 1; conn i1 0 add 2
          conn add 0 nt 0
          conn nt 0 mux 0; conn i0 0 mux 1; conn i2 0 mux 2
          conn mux 0 rg 0; conn i2 0 rg 1
          conn i0 0 sel 0; conn nt 0 sh 0; conn sel 0 sh 1
          conn add 1 nand3 0; conn i2 0 nand3 1; conn add 1 nand3 2
          conn k 0 cmp 0
          conn rg 0 o0 0; conn nand3 0 o1 0; conn sh 0 o2 0; conn cmp 0 o3 0 ]
        // conn pairs the two port ids, so even add.1 driving nand3 twice gets distinct ids
    comps, conns

/// A memory of the given shape holding the given (address, data) pairs
let private mem aw dw (data: (int * int) list) =
    { Init = FromData
      AddressWidth = aw
      WordWidth = dw
      Data = data |> List.map (fun (a, d) -> bigint a, bigint d) |> Map.ofList
      Comments = None }

//------------------------------------------------------------------------------------------//
//----------------------------------------- tests ------------------------------------------//
//------------------------------------------------------------------------------------------//

/// Every gate type at every arity the emitter treats differently
let private gateComponents : (string * ComponentType * int list * int list) list =
    [ for gateType in [ And; Or; Xor; Nand; Nor; Xnor ] do
        for n in [ 2; 3; 4 ] -> $"GateN %A{gateType} {n}", GateN(gateType, n), List.replicate n 1, [ 1 ] ]

/// Every component type that getVerilogComponent can be reached with, in a shape that simulates.
/// Legacy types (Decode4, Input, AsyncROM, ROM, RAM) are left out: FastCreate rejects them, so no
/// FastSimulation containing one can be built and their Verilog is unreachable.
let private allComponents : (string * ComponentType * int list * int list) list =
    gateComponents @
    [ "Not", Not, [ 1 ], [ 1 ]
      "Viewer", Viewer 4, [ 4 ], []
      "IOLabel-in-line", IOLabel, [ 4 ], [ 4 ]
      "Constant1", Constant1(8, 200I, "200"), [], [ 8 ]
      "Constant1 wide", Constant1(40, 1099511627775I, "x"), [], [ 40 ]
      "BusCompare1", BusCompare1(8, 200I, "200"), [ 8 ], [ 1 ]
      "BusSelection", BusSelection(3, 2), [ 8 ], [ 3 ]
      "Mux2", Mux2, [ 4; 4; 1 ], [ 4 ]
      "Mux4", Mux4, [ 4; 4; 4; 4; 2 ], [ 4 ]
      "Mux8", Mux8, [ 4; 4; 4; 4; 4; 4; 4; 4; 3 ], [ 4 ]
      "Demux2", Demux2, [ 4; 1 ], [ 4; 4 ]
      "Demux4", Demux4, [ 4; 2 ], [ 4; 4; 4; 4 ]
      "Demux8", Demux8, [ 4; 3 ], List.replicate 8 4
      "NbitsAdder", NbitsAdder 4, [ 1; 4; 4 ], [ 4; 1 ]
      "NbitsAdderNoCin", NbitsAdderNoCin 4, [ 4; 4 ], [ 4; 1 ]
      "NbitsAdderNoCout", NbitsAdderNoCout 4, [ 1; 4; 4 ], [ 4 ]
      "NbitsAdderNoCinCout", NbitsAdderNoCinCout 4, [ 4; 4 ], [ 4 ]
      "NbitsXor", NbitsXor(4, None), [ 4; 4 ], [ 4 ]
      "NbitsXor multiply", NbitsXor(4, Some Multiply), [ 4; 4 ], [ 4 ]
      "NbitsAnd", NbitsAnd 4, [ 4; 4 ], [ 4 ]
      "NbitsOr", NbitsOr 4, [ 4; 4 ], [ 4 ]
      "NbitsNot", NbitsNot 4, [ 4 ], [ 4 ]
      "NbitSpreader", NbitSpreader 4, [ 1 ], [ 4 ]
      "MergeWires", MergeWires, [ 2; 3 ], [ 5 ]
      "MergeN", MergeN 3, [ 1; 2; 3 ], [ 6 ]
      "SplitWire", SplitWire 2, [ 5 ], [ 2; 3 ]
      "SplitN", SplitN(3, [ 1; 2; 3 ], [ 0; 1; 3 ]), [ 6 ], [ 1; 2; 3 ]
      "DFF", DFF, [ 1 ], [ 1 ]
      "DFFE", DFFE, [ 1; 1 ], [ 1 ]
      "Register", Register 4, [ 4 ], [ 4 ]
      "RegisterE", RegisterE 4, [ 4; 1 ], [ 4 ]
      "Counter", Counter 4, [ 4; 1; 1 ], [ 4 ]
      "CounterNoLoad", CounterNoLoad 4, [ 1 ], [ 4 ]
      "CounterNoEnable", CounterNoEnable 4, [ 4; 1 ], [ 4 ]
      "CounterNoEnableLoad", CounterNoEnableLoad 4, [], [ 4 ]
      "Shift LSL", Shift(8, 3, LSL), [ 8; 3 ], [ 8 ]
      "Shift LSR", Shift(8, 3, LSR), [ 8; 3 ], [ 8 ]
      "Shift ASR", Shift(8, 3, ASR), [ 8; 3 ], [ 8 ]
      "AsyncROM1", AsyncROM1(mem 3 4 [ 0, 5; 1, 12 ]), [ 3 ], [ 4 ]
      "ROM1", ROM1(mem 3 4 [ 0, 5; 1, 12 ]), [ 3 ], [ 4 ]
      "RAM1", RAM1(mem 3 4 [ 0, 5; 1, 12 ]), [ 3; 4; 1 ], [ 4 ]
      "AsyncRAM1", AsyncRAM1(mem 3 4 [ 0, 5; 1, 12 ]), [ 3; 4; 1 ], [ 4 ] ]

let tests =
    testList "VerilogOutput" [

        //-------------------------------------------------------------------------------------//
        // invariants that must hold for every component
        //-------------------------------------------------------------------------------------//

        testList "every component" [
            // A sized literal whose digits do not fit its declared width is silently truncated by
            // Verilog. This is what catches decimal digits printed after a 'h prefix.
            test "sized literals fit their declared width" {
                for name, cType, inW, outW in allComponents do
                    for width, radix, digits in sizedLiterals (dutVerilog cType inW outW) do
                        let bitsPerDigit =
                            match radix with
                            | 'h' -> 4
                            | 'o' -> 3
                            | 'b' -> 1
                            | _ -> 0 // decimal: no per-digit bound to check
                        let value =
                            match radix with
                            | 'd' -> Some(bigint.Parse digits)
                            | 'h' -> Some(System.Convert.ToUInt64(digits, 16) |> bigint)
                            | 'b' -> Some(System.Convert.ToUInt64(digits, 2) |> bigint)
                            | 'o' -> Some(System.Convert.ToUInt64(digits, 8) |> bigint)
                            | _ -> None
                        match value with
                        | Some v when v >= (1I <<< width) ->
                            failtestf
                                "%s: literal %d'%c%s does not fit in %d bits (value %A)"
                                name width radix digits width v
                        | _ -> ()
                        ignore bitsPerDigit
            }

            test "every identifier is at most 50 characters" {
                for name, cType, inW, outW in allComponents do
                    let v = dutVerilog cType inW outW
                    for m in Regex.Matches(v, @"[A-Za-z_][\w$]*") do
                        Expect.isLessThanOrEqual
                            m.Value.Length 50 $"{name}: identifier {m.Value} is too long"
            }

            test "module and endmodule are balanced" {
                for name, cType, inW, outW in allComponents do
                    let v = dutVerilog cType inW outW
                    let opens = Regex.Matches(v, @"(?m)^\s*module\b").Count
                    let closes = Regex.Matches(v, @"(?m)^\s*endmodule\b").Count
                    Expect.equal closes opens $"{name}: unbalanced module/endmodule"
            }

            test "instance names are unique" {
                for name, cType, inW, outW in allComponents do
                    let instances = instantiations (dutVerilog cType inW outW) |> List.map snd
                    Expect.equal
                        (List.distinct instances) instances $"{name}: duplicate instance name"
            }

            // A hybrid component (AsyncRAM1) is in both the clocked and the combinational
            // component arrays; its Verilog describes the whole of it and must be written once.
            test "each component is written exactly once" {
                for name, cType, inW, outW in allComponents do
                    let body = mainModule (dutVerilog cType inW outW)
                    let statements =
                        body.Split '\n'
                        |> Array.map (fun l -> l.Trim())
                        |> Array.filter (fun l ->
                            l <> "" && not (Regex.IsMatch(l, @"^(module|endmodule|input|output|wire|reg)\b")))
                    Expect.equal
                        (Array.distinct statements) statements $"{name}: statement written twice"
            }

            test "no undeclared net is used" {
                for name, cType, inW, outW in allComponents do
                    match cType with
                    | Memory _ -> () // memory instantiations name a module and an instance, not nets
                    | _ ->
                        let body = mainModule (dutVerilog cType inW outW)
                        let undeclared = Set.difference (usedNames body) (declaredNames body)
                        Expect.isEmpty undeclared $"{name}: undeclared names used"
            }
        ]

        //-------------------------------------------------------------------------------------//
        // gates: evaluate what was emitted
        //-------------------------------------------------------------------------------------//

        testList "gate logic" [
            // An n-input NAND/NOR/XNOR inverts once, after combining all n inputs. Folding the
            // negated two-input operator pairwise instead gives different logic for n > 2.
            for gateType in [ And; Or; Xor; Nand; Nor; Xnor ] do
                for n in [ 2; 3; 4; 5 ] do
                    test $"GateN %A{gateType} {n} inputs" {
                        let rhs =
                            dutVerilog (GateN(gateType, n)) (List.replicate n 1) [ 1 ]
                            |> assignRhs "DUT"
                        for pattern in 0 .. (1 <<< n) - 1 do
                            let bits = [ for i in 0 .. n - 1 -> (pattern >>> i) &&& 1 ]
                            let env =
                                bits |> List.mapi (fun i b -> $"I{i}", b) |> Map.ofList
                            Expect.equal
                                (evalBitExpr env rhs)
                                (gateRef gateType bits)
                                $"%A{gateType}/{n} inputs %A{bits}: {rhs}"
                    }

            test "Not" {
                let rhs = dutVerilog Not [ 1 ] [ 1 ] |> assignRhs "DUT"
                Expect.equal (evalBitExpr (Map [ "I0", 0 ]) rhs) 1 "!0 = 1"
                Expect.equal (evalBitExpr (Map [ "I0", 1 ]) rhs) 0 "!1 = 0"
            }
        ]

        //-------------------------------------------------------------------------------------//
        // individual constructs
        //-------------------------------------------------------------------------------------//

        testList "constructs" [
            test "constants are emitted in hex" {
                // 200 decimal is c8 hex: printing the decimal digits after 'h would give 8'h200,
                // which Verilog reads as 0x200 and truncates to 8 bits, i.e. zero
                Expect.stringContains
                    (dutVerilog (Constant1(8, 200I, "200")) [] [ 8 ] |> assignRhs "DUT" |> fun s -> s.ToLower())
                    "8'hc8"
                    "Constant1 8 bits, value 200"
                Expect.stringContains
                    (dutVerilog (Constant1(40, 1099511627775I, "x")) [] [ 40 ] |> assignRhs "DUT" |> fun s -> s.ToLower())
                    "40'hffffffffff"
                    "Constant1 40 bits, all ones"
            }

            test "bus comparison is against a hex constant" {
                Expect.stringContains
                    (dutVerilog (BusCompare1(8, 200I, "200")) [ 8 ] [ 1 ] |> assignRhs "DUT" |> fun s -> s.ToLower())
                    "8'hc8"
                    "BusCompare1 against 200"
            }

            test "arithmetic shift right is signed" {
                // >>> shifts arithmetically only when its left operand is signed; every signal
                // here is an unsigned wire, so without $signed this is a logical shift
                Expect.stringContains
                    (dutVerilog (Shift(8, 3, ASR)) [ 8; 3 ] [ 8 ] |> assignRhs "DUT")
                    "$signed("
                    "ASR"
                for shiftType, op in [ LSL, "<<"; LSR, ">>" ] do
                    let rhs = dutVerilog (Shift(8, 3, shiftType)) [ 8; 3 ] [ 8 ] |> assignRhs "DUT"
                    Expect.stringContains rhs op $"%A{shiftType} uses {op}"
                    Expect.isFalse (rhs.Contains "$signed") $"%A{shiftType} is unsigned"
            }

            test "multiply is a plain product" {
                // a bit-select on a parenthesised expression is not legal Verilog; the assignment
                // to the n-bit output already truncates, which is what the simulation does
                let rhs = dutVerilog (NbitsXor(4, Some Multiply)) [ 4; 4 ] [ 4 ] |> assignRhs "DUT"
                Expect.stringContains rhs "*" "multiply"
                Expect.isFalse (rhs.Contains "[") $"no bit-select on the product: {rhs}"
                Expect.isFalse (rhs.Contains "n-1") $"no uninterpolated width: {rhs}"
            }

            test "a sheet with no ports has no port list" {
                // Input -> NotConnected, written for simulation: no module port survives
                let i = makeComp 1 0 1 (Input1(4, None)) "I"
                let nc = makeComp 2 1 0 NotConnected "NC"
                let v = verilogOf Verilog.ForSimulation Verilog.Release ([ i; nc ], [ conn i 0 nc 0 ])
                Expect.isFalse (Regex.IsMatch(v, @"module main\s*\(\s*\)")) "no empty port list"
                Expect.stringContains v "module main;" "bare module header"
            }

            test "the uart is included only for the debug profile" {
                let canvas = mixedCanvas ()
                Expect.isFalse
                    ((verilogOf Verilog.ForSynthesis Verilog.Release canvas).Contains "uart.v")
                    "release build does not include the uart"
                Expect.stringContains
                    (verilogOf Verilog.ForSynthesis Verilog.Debug canvas)
                    "uart.v"
                    "debug build includes the uart"
            }

            test "simulation inputs are assigned procedurally, not continuously" {
                // inputs are regs; "assign" inside initial is a procedural continuous assignment,
                // which pins them to zero for the whole run
                let v = dutVerilog DFFE [ 1; 1 ] [ 1 ] |> ignore
                let sim =
                    dutCanvas DFFE [ 1; 1 ] [ 1 ]
                    |> verilogOf Verilog.ForSimulation Verilog.Release
                let initialBlock =
                    let i = sim.IndexOf "initial"
                    sim[i..]
                Expect.isFalse
                    (Regex.IsMatch(initialBlock, @"assign\s+I\d"))
                    "no procedural continuous assignment to an input"
                Expect.isTrue
                    (Regex.IsMatch(initialBlock, @"(?m)^\s*I0\s*=\s*1'h0;"))
                    "input initialised by a blocking assignment"
                ignore v
            }
        ]

        //-------------------------------------------------------------------------------------//
        // memories
        //-------------------------------------------------------------------------------------//

        testList "memories" [
            test "each memory is instantiated exactly once" {
                for name, cType in
                    [ "AsyncROM1", AsyncROM1(mem 3 4 [ 0, 5 ])
                      "ROM1", ROM1(mem 3 4 [ 0, 5 ])
                      "RAM1", RAM1(mem 3 4 [ 0, 5 ])
                      "AsyncRAM1", AsyncRAM1(mem 3 4 [ 0, 5 ]) ] do
                    let inW = match cType with | AsyncROM1 _ | ROM1 _ -> [ 3 ] | _ -> [ 3; 4; 1 ]
                    Expect.hasLength (instantiations (dutVerilog cType inW [ 4 ])) 1 name
            }

            test "the memory module name does not collide with a net name" {
                for cType, inW in
                    [ AsyncROM1(mem 3 4 []), [ 3 ]
                      ROM1(mem 3 4 []), [ 3 ]
                      RAM1(mem 3 4 []), [ 3; 4; 1 ]
                      AsyncRAM1(mem 3 4 []), [ 3; 4; 1 ] ] do
                    let v = dutVerilog cType inW [ 4 ]
                    let nets = declaredNames (mainModule v)
                    for moduleName, instanceName in instantiations v do
                        Expect.isFalse (nets.Contains moduleName) $"module {moduleName} is also a net"
                        Expect.isFalse (nets.Contains instanceName) $"instance {instanceName} is also a net"
            }

            test "the asynchronous RAM module is well formed" {
                let v = dutVerilog (AsyncRAM1(mem 3 4 [ 0, 5 ])) [ 3; 4; 1 ] [ 4 ]
                let moduleText = v[.. v.IndexOf "module main" - 1]
                // the read is combinational, so it is a continuous assignment outside the always
                // block - a non-blocking assignment there would not be legal
                Expect.stringContains moduleText "assign q = ram[a];" "combinational read"
                Expect.isFalse (moduleText.Contains "q <= ram[a];") "no procedural read outside a block"
                // every port in the header is declared, with a name
                Expect.stringContains moduleText "output [3:0] q;" "q is declared as an output"
                for port in [ "d"; "a" ] do
                    Expect.isTrue
                        (Regex.IsMatch(moduleText, @"input\s*\[\d+:0\]\s*" + port + ";"))
                        $"{port} is declared"
                Expect.isFalse
                    (Regex.IsMatch(moduleText, @"(?m)^\s*(?:input|output)[^;]*\[\d+:0\];"))
                    "no port declared without a name"
            }

            test "the synchronous RAM reads the value from before the write" {
                // matches the simulator, which returns the old content when writing
                let moduleText =
                    let v = dutVerilog (RAM1(mem 3 4 [ 0, 5 ])) [ 3; 4; 1 ] [ 4 ]
                    v[.. v.IndexOf "module main" - 1]
                Expect.stringContains moduleText "q <= ram[a];" "registered read"
                Expect.stringContains moduleText "ram[a] <= d;" "registered write"
            }
        ]

        //-------------------------------------------------------------------------------------//
        // a whole sheet
        //-------------------------------------------------------------------------------------//

        testList "mixed sheet" [
            test "no undeclared net is used" {
                let body = mainModule (verilogOf Verilog.ForSynthesis Verilog.Release (mixedCanvas ()))
                let undeclared = Set.difference (usedNames body) (declaredNames body)
                Expect.isEmpty undeclared $"undeclared names %A{undeclared} in:\n{body}"
            }

            test "every declared net is driven exactly once" {
                let body = mainModule (verilogOf Verilog.ForSynthesis Verilog.Release (mixedCanvas ()))
                // the target of a statement, which may be a concatenation of several nets
                let targets (pattern: string) =
                    [ for m in Regex.Matches(body, pattern) do
                        for n in Regex.Matches(m.Groups[1].Value, @"[A-Za-z_][\w$]*") -> n.Value ]
                let driven =
                    targets @"assign\s+([^=]+)="
                    @ targets @"always\s*@\(posedge clk\)\s*([^<]+)<="
                Expect.equal (List.distinct driven) driven $"a net is driven twice in:\n{body}"
                let inputs =
                    Regex.Matches(body, @"(?m)^\s*input\s+(?:\[[^\]]*\]\s*)?([A-Za-z_][\w$]*)")
                    |> Seq.map (fun m -> m.Groups[1].Value)
                    |> Set.ofSeq
                let undriven =
                    Set.difference (Set.difference (declaredNames body) (Set.ofList driven)) inputs
                Expect.isEmpty undriven $"declared but undriven %A{undriven} in:\n{body}"
            }

            test "simulation output declares a clock and a testbench" {
                let v = verilogOf Verilog.ForSimulation Verilog.Release (mixedCanvas ())
                Expect.stringContains v "reg clk;" "clock is a reg driven by the testbench"
                Expect.stringContains v "$display" "outputs are displayed"
                Expect.isFalse (v.Contains "input clk;") "clock is not a port"
            }
        ]
    ]
