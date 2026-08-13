/// End-to-end tests for the Verilog *input* compiler. Source text is parsed by the real
/// nearley parser via node (`run_parser.mjs` — the same parse the in-app editor runs), the
/// AST JSON is deserialised into VerilogTypes.VerilogInput, semantic-checked with
/// ErrorCheck.getSemanticErrors, synthesised to a sheet by SheetCreator.createSheet, and the
/// sheet is simulated. So a test here covers grammar, error checks, synthesis and the
/// simulated behaviour of the generated circuit in one assertion.
///
/// Requires `node` on PATH (already a build prerequisite for Issie).
module VerilogCompiler

open System.IO
open System.Diagnostics
open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open TestFixtures

//------------------------------------------------------------------------------------------//
//----------------------------- source -> AST (via node parser) ----------------------------//
//------------------------------------------------------------------------------------------//

let private verilogDir =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Renderer", "VerilogComponent"))

/// The parser CLI's output envelope. All fields optional: exactly one of Ok/Err is present.
type private ParseError = { Line: int; Col: int; Length: int; Message: string }
type private ParseEnvelope =
    { Ok: VerilogTypes.VerilogInput option
      Err: ParseError option
      NewLinesIndex: int array option }

/// Parses Verilog source with the app's own parser. Ok gives the fixed AST and the
/// newline index that the semantic checks need for error locations.
let parseVerilog (source: string) : Result<VerilogTypes.VerilogInput * int list, string> =
    let tmp = Path.GetTempFileName()
    try
        File.WriteAllText(tmp, source)
        let psi =
            ProcessStartInfo(
                FileName = "node",
                Arguments = (let script = Path.Combine(verilogDir, "run_parser.mjs") in $"\"{script}\" \"{tmp}\""),
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false)
        use p = Process.Start psi
        let stdout = p.StandardOutput.ReadToEnd()
        let stderr = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then
            failtestf "node run_parser.mjs failed (%d): %s" p.ExitCode stderr
        // the parser prints plain JS-shaped JSON, which the reflective reader written for
        // SimpleJson's encoding also reads
        let envelope =
            match SimpleJsonDotNet.tryDeserialise<ParseEnvelope> stdout with
            | Ok envelope -> envelope
            | Error msg -> failtestf "could not read run_parser.mjs output: %s\n%s" msg stdout
        match envelope.Ok, envelope.Err with
        | Some ast, _ ->
            Ok(ast, envelope.NewLinesIndex |> Option.defaultValue [||] |> Array.toList)
        | _, Some err -> Error err.Message
        | None, None -> failtestf "run_parser.mjs printed neither Ok nor Err: %s" stdout
    finally
        File.Delete tmp

let private emptyProject =
    { ProjectPath = ""
      OpenFileName = ""
      WorkingFileName = Some ""
      LoadedComponents = [] }

/// Parse + semantic check + synthesise to a canvas; test failure on any error.
let private compile (source: string) : CanvasState =
    match parseVerilog source with
    | Error e -> failtestf "parse error: %s" e
    | Ok(ast, linesIndex) ->
        match ErrorCheck.getSemanticErrors ast linesIndex VerilogTypes.NewVerilogFile emptyProject with
        | [] ->
            // model/dispatch are only touched on the parameter-override save path,
            // which none of these programs reach
            SheetCreator.createSheet ast emptyProject Unchecked.defaultof<_> (fun _ -> ())
        | errors -> failtestf "semantic errors: %A" (errors |> List.map (fun e -> e.Message))

//------------------------------------------------------------------------------------------//
//---------------------------------- simulating the result ---------------------------------//
//------------------------------------------------------------------------------------------//

/// Compile Verilog source and evaluate the resulting sheet combinationally: inputs are
/// given and outputs returned by (upper-cased) port label.
let private simulateVerilog (source: string) (inputs: (string * bigint) list) : Map<string, bigint> =
    let canvas = compile source
    let ldc = CanvasBuilder.makeLdc "verilog_dut" None canvas
    match Simulator.startCircuitSimulation 40 "verilog_dut" canvas [ ldc ] with
    | Error e -> failtestf "simulation setup failed: %A" e.ErrType
    | Ok simData ->
        simData.Inputs
        |> List.iter (fun (cid, ComponentLabel label, width) ->
            match inputs |> List.tryFind (fun (n, _) -> n.ToUpper() = label) with
            | Some(_, value) ->
                let fd = NumberHelpers.convertBigintToFastData width value
                FastExtract.changeInput cid (IData fd) 0 simData.FastSim
            | None -> failtestf "no test value for input %s" label)
        simData.Outputs
        |> List.map (fun (cid, ComponentLabel label, _) ->
            match FastExtract.extractFastSimulationOutput simData.FastSim 0 (cid, []) (OutputPortNumber 0) with
            | IData fd -> label, fd.GetBigInt
            | IAlg _ -> failtestf "algebraic output %s" label)
        |> Map.ofList

let private binaryOpModule (op: string) =
    $"""module dut(a, b, o);
input bit [1:0] a;
input bit [1:0] b;
output bit [1:0] o;
assign o = a {op} b;
endmodule
"""

//------------------------------------------------------------------------------------------//
//------------------------------------------ tests ------------------------------------------//
//------------------------------------------------------------------------------------------//

[<Tests>]
let tests =
    testSequenced <| testList "VerilogCompiler" [

        testList "operators" [
            // ~^ was compiled identically to ^, silently producing XOR hardware
            test "xnor is the complement of xor" {
                let xnor = binaryOpModule "~^"
                for a in 0..3 do
                    for b in 0..3 do
                        let r = simulateVerilog xnor [ "a", bigint a; "b", bigint b ]
                        Expect.equal r["O"] (bigint ((a ^^^ b) ^^^ 3)) $"{a} ~^ {b}"
            }
            test "xor still compiles to xor" {
                let xor = binaryOpModule "^"
                for a in 0..3 do
                    for b in 0..3 do
                        let r = simulateVerilog xor [ "a", bigint a; "b", bigint b ]
                        Expect.equal r["O"] (bigint (a ^^^ b)) $"{a} ^ {b}"
            }
            // ~& and ~| parsed but crashed AST conversion: parseOperation only knew a "!&"
            // spelling the lexer can never produce, and had no Nor case at all
            test "nand and nor reductions" {
                let src =
                    """module dut(a, o$nand, o$nor);
input bit [1:0] a;
output bit o$nand;
output bit o$nor;
assign o$nand = (~&a);
assign o$nor = (~|a);
endmodule
"""
                for a in 0..3 do
                    let r = simulateVerilog src [ "a", bigint a ]
                    Expect.equal r["O$NAND"] (bigint (if a = 3 then 0 else 1)) $"~&{a}"
                    Expect.equal r["O$NOR"] (bigint (if a = 0 then 1 else 0)) $"~|{a}"
            }
            test "xnor works as a subexpression" {
                let src =
                    """module dut(a, b, c, o);
input bit [1:0] a;
input bit [1:0] b;
input bit [1:0] c;
output bit [1:0] o;
assign o = (a ~^ b) & c;
endmodule
"""
                for a in 0..3 do
                    for b in 0..3 do
                        let r = simulateVerilog src [ "a", bigint a; "b", bigint b; "c", bigint 3 ]
                        Expect.equal r["O"] (bigint ((a ^^^ b) ^^^ 3)) $"({a} ~^ {b}) & 3"
            }
        ]

        testList "identifiers" [
            // $ and leading _ are legal in Verilog identifiers; the lexer used to reject both
            test "dollar and leading underscore in identifiers compile and simulate" {
                let src =
                    """module dut(a$in, _out);
input bit [3:0] a$in;
output bit [3:0] _out;
wire [3:0] t$mp;
assign t$mp = a$in;
assign _out = t$mp;
endmodule
"""
                let r = simulateVerilog src [ "a$in", bigint 9 ]
                Expect.equal r["_OUT"] (bigint 9) "passthrough via $/_ names"
            }
            test "an identifier may not start with a dollar" {
                let src = "module dut(a, o);\ninput bit a;\noutput bit o;\nassign o = $x;\nendmodule\n"
                Expect.isTrue (Result.isError (parseVerilog src)) "leading $ must stay rejected"
            }
        ]

        testList "shifts" [
            // issue #510: `>>` parsed and the component was created, but simulating it failed
            // with "Legacy components, not Implemented" out of determineBigIntState
            test "issue 510: variable right shift simulates" {
                let src =
                    """module right_shifter(data_in, shift_amount, data_out);
input bit [10:0] data_in;
input bit [4:0] shift_amount;
output bit [10:0] data_out;
assign data_out = data_in >> shift_amount;
endmodule
"""
                for sh in 0..11 do
                    let r = simulateVerilog src [ "data_in", 1365I; "shift_amount", bigint sh ]
                    Expect.equal r["DATA_OUT"] (1365I >>> sh) $"1365 >> {sh}"
            }
            // issue #511: this module hung the app
            test "issue 511: 16-way case of concatenations compiles and simulates" {
                let src =
                    """module shifter1 (in, shift, out);
input bit [15:0] in;
input bit [3:0] shift;
output bit [31:0] out;
always_comb begin
    case (shift)
      4'd0: out = {16'b0, in};
      4'd1: out = {15'b0, in, 1'b0};
      4'd2: out = {14'b0, in, 2'b0};
      4'd3: out = {13'b0, in, 3'b0};
      4'd4: out = {12'b0, in, 4'b0};
      4'd5: out = {11'b0, in, 5'b0};
      4'd6: out = {10'b0, in, 6'b0};
      4'd7: out = {9'b0, in, 7'b0};
      4'd8: out = {8'b0, in, 8'b0};
      4'd9: out = {7'b0, in, 9'b0};
      4'd10: out = {6'b0, in, 10'b0};
      4'd11: out = {5'b0, in, 11'b0};
      4'd12: out = {4'b0, in, 12'b0};
      4'd13: out = {3'b0, in, 13'b0};
      4'd14: out = {2'b0, in, 14'b0};
      4'd15: out = {1'b0, in, 15'b0};
      default: out = {16'b0, in};
    endcase
end
endmodule
"""
                for sh in 0..15 do
                    let r = simulateVerilog src [ "in", 43981I; "shift", bigint sh ]
                    Expect.equal r["OUT"] (43981I <<< sh) $"43981 << {sh}"
            }
        ]

        testList "whitespace" [
            // the lexer's longest-match makes whitespace before '[' redundant; it was mandatory
            test "no space needed between bit and a range" {
                let src =
                    """module dut(a, o);
input bit[3:0] a;
output bit[3:0] o;
bit[3:0] t;
assign t = a;
assign o = t;
endmodule
"""
                let r = simulateVerilog src [ "a", bigint 5 ]
                Expect.equal r["O"] (bigint 5) "bit[3:0] declarations"
            }
            test "input without bit keyword is still rejected" {
                let src = "module dut(a, o);\ninput [3:0] a;\noutput bit [3:0] o;\nassign o = a;\nendmodule\n"
                Expect.isTrue (Result.isError (parseVerilog src)) "bit stays mandatory in IO declarations"
            }
        ]

        testList "arrays" [
            // the unassigned/unread checks enumerated an array's vector bits but track
            // assignments per word, so width <> word count reported phantom words
            test "array with width different from word count compiles" {
                let src =
                    """module dut(clk, a, o);
input bit clk;
input bit [7:0] a;
output bit [7:0] o;
bit [7:0] hist [3:0];
bit [1:0] i;
always_ff @(posedge clk) begin
    for (i = 2'd0; i <= 2'd3; i = i + 2'd1) begin
        hist[i] <= a;
    end
end
assign o = (hist[0] ^ hist[1]) | (hist[2] & hist[3]);
endmodule
"""
                let r = simulateVerilog src [ "a", bigint 5 ]
                Expect.equal r["O"] (bigint 0) "registers all zero before the first clock edge"
            }
            test "a genuinely unwritten word is still reported, phantom words are not" {
                let src =
                    """module dut(clk, a, o);
input bit clk;
input bit [7:0] a;
output bit [7:0] o;
bit [7:0] hist [2:0];
always_ff @(posedge clk) begin
    hist[0] <= a;
    hist[1] <= a;
end
assign o = (hist[0] ^ hist[1]) | hist[2];
endmodule
"""
                match parseVerilog src with
                | Error e -> failtestf "parse error: %s" e
                | Ok(ast, lines) ->
                    let msgs =
                        ErrorCheck.getSemanticErrors ast lines VerilogTypes.NewVerilogFile emptyProject
                        |> List.map (fun e -> e.Message)
                    Expect.isTrue (msgs |> List.exists (fun m -> m.Contains "hist[2]")) $"hist[2] unwritten: {msgs}"
                    Expect.isFalse (msgs |> List.exists (fun m -> m.Contains "hist[7]")) $"no phantom bit-words: {msgs}"
            }
        ]

        testList "corpus" [
            // every file the compiler's own valid-input corpus holds must still parse
            test "all valid corpus files parse" {
                let dir = Path.Combine(verilogDir, "test", "input", "valid")
                let files = Directory.GetFiles(dir, "*.sv")
                Expect.isGreaterThan files.Length 10 "corpus present"
                for f in files do
                    match parseVerilog (File.ReadAllText f) with
                    | Ok _ -> ()
                    | Error e -> failtestf "%s no longer parses: %s" (Path.GetFileName f) e
            }
        ]
    ]
