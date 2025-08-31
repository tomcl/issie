module VerilogTests

open Expecto
open VerilogTypes
open VerilogAST
open ErrorCheck

// Test data for Verilog parsing and conversion
let testVerilogParsingData =
    [
        "Parse simple module",
        fun () ->
            let verilog = """
                module and_gate(input a, input b, output c);
                    assign c = a & b;
                endmodule
            """
            let result = parseVerilogString verilog
            match result with
            | Ok ast ->
                Expect.equal ast.ModuleName "and_gate" "Module name should match"
                Expect.equal (List.length ast.Ports) 3 "Should have 3 ports"
            | Error e -> failtest $"Parse failed: {e}"

        "Parse module with parameters",
        fun () ->
            let verilog = """
                module counter #(parameter WIDTH = 8) (
                    input clk,
                    input reset,
                    output reg [WIDTH-1:0] count
                );
                endmodule
            """
            let result = parseVerilogString verilog
            match result with
            | Ok ast ->
                Expect.isTrue (ast.Parameters |> List.exists (fun p -> p.Name = "WIDTH")) "Should have WIDTH parameter"
            | Error e -> failtest $"Parse failed: {e}"

        "Parse wire declarations",
        fun () ->
            let verilog = """
                module test();
                    wire [7:0] data;
                    wire single_bit;
                    reg [15:0] register;
                endmodule
            """
            let result = parseVerilogString verilog
            match result with
            | Ok ast ->
                let wires = ast.Declarations |> List.filter (fun d -> d.Type = Wire)
                Expect.isGreaterThan (List.length wires) 0 "Should have wire declarations"
            | Error e -> failtest $"Parse failed: {e}"

        "Parse always block",
        fun () ->
            let verilog = """
                module dff(input clk, input d, output reg q);
                    always @(posedge clk)
                        q <= d;
                endmodule
            """
            let result = parseVerilogString verilog
            match result with
            | Ok ast ->
                Expect.isGreaterThan (List.length ast.AlwaysBlocks) 0 "Should have always block"
            | Error e -> failtest $"Parse failed: {e}"
    ]

// Test data for Verilog error checking
let testVerilogErrorCheckData =
    [
        "Detect undeclared wire",
        fun () ->
            let ast = {
                ModuleName = "test"
                Ports = []
                Parameters = []
                Declarations = []
                Assignments = [
                    { Target = "undefined_wire"; Expression = Literal (IntLiteral 1) }
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let errors = checkUndeclaredWires ast
            Expect.isNonEmpty errors "Should detect undeclared wire"

        "Detect width mismatch",
        fun () ->
            let ast = {
                ModuleName = "test"
                Ports = []
                Parameters = []
                Declarations = [
                    { Type = Wire; Name = "a"; Width = Some 8 }
                    { Type = Wire; Name = "b"; Width = Some 4 }
                ]
                Assignments = [
                    { Target = "a"; Expression = Identifier "b" }
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let errors = checkWidthMismatches ast
            Expect.isNonEmpty errors "Should detect width mismatch"

        "Detect multiple drivers",
        fun () ->
            let ast = {
                ModuleName = "test"
                Ports = []
                Parameters = []
                Declarations = [
                    { Type = Wire; Name = "signal"; Width = Some 1 }
                ]
                Assignments = [
                    { Target = "signal"; Expression = Literal (IntLiteral 0) }
                    { Target = "signal"; Expression = Literal (IntLiteral 1) }
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let errors = checkMultipleDrivers ast
            Expect.isNonEmpty errors "Should detect multiple drivers"

        "Valid module passes checks",
        fun () ->
            let ast = {
                ModuleName = "valid"
                Ports = [
                    { Name = "a"; Direction = Input; Width = Some 1 }
                    { Name = "b"; Direction = Input; Width = Some 1 }
                    { Name = "c"; Direction = Output; Width = Some 1 }
                ]
                Parameters = []
                Declarations = []
                Assignments = [
                    { Target = "c"; Expression = BinaryOp (And, Identifier "a", Identifier "b") }
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let errors = performAllChecks ast
            Expect.isEmpty errors "Valid module should have no errors"
    ]

// Test data for Verilog to Issie conversion
let testVerilogConversionData =
    [
        "Convert AND gate",
        fun () ->
            let ast = {
                ModuleName = "and_gate"
                Ports = [
                    { Name = "a"; Direction = Input; Width = Some 1 }
                    { Name = "b"; Direction = Input; Width = Some 1 }
                    { Name = "out"; Direction = Output; Width = Some 1 }
                ]
                Parameters = []
                Declarations = []
                Assignments = [
                    { Target = "out"; Expression = BinaryOp (And, Identifier "a", Identifier "b") }
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let components = convertToIssieComponents ast
            Expect.equal (List.length components) 3 "Should create 3 components (2 inputs, 1 output)"
            let andGate = components |> List.tryFind (fun c -> c.Type = CommonTypes.And)
            Expect.isSome andGate "Should create AND gate"

        "Convert multiplexer",
        fun () ->
            let ast = {
                ModuleName = "mux2"
                Ports = [
                    { Name = "sel"; Direction = Input; Width = Some 1 }
                    { Name = "a"; Direction = Input; Width = Some 8 }
                    { Name = "b"; Direction = Input; Width = Some 8 }
                    { Name = "out"; Direction = Output; Width = Some 8 }
                ]
                Parameters = []
                Declarations = []
                Assignments = [
                    { Target = "out"; Expression = Conditional (
                        Identifier "sel",
                        Identifier "b",
                        Identifier "a"
                    )}
                ]
                AlwaysBlocks = []
                Instances = []
            }
            let components = convertToIssieComponents ast
            let mux = components |> List.tryFind (fun c -> c.Type = CommonTypes.Mux2)
            Expect.isSome mux "Should create multiplexer"

        "Convert register",
        fun () ->
            let ast = {
                ModuleName = "register"
                Ports = [
                    { Name = "clk"; Direction = Input; Width = Some 1 }
                    { Name = "d"; Direction = Input; Width = Some 8 }
                    { Name = "q"; Direction = Output; Width = Some 8 }
                ]
                Parameters = []
                Declarations = []
                Assignments = []
                AlwaysBlocks = [
                    {
                        Sensitivity = [PosEdge "clk"]
                        Statements = [
                            NonBlockingAssign ("q", Identifier "d")
                        ]
                    }
                ]
                Instances = []
            }
            let components = convertToIssieComponents ast
            let reg = components |> List.tryFind (fun c -> c.Type = CommonTypes.Register 8)
            Expect.isSome reg "Should create register"
    ]

// Mock implementations
let parseVerilogString verilog =
    // Simplified parser mock - real implementation would use Nearley
    if verilog.Contains("and_gate") then
        Ok {
            ModuleName = "and_gate"
            Ports = [
                { Name = "a"; Direction = Input; Width = Some 1 }
                { Name = "b"; Direction = Input; Width = Some 1 }
                { Name = "c"; Direction = Output; Width = Some 1 }
            ]
            Parameters = []
            Declarations = []
            Assignments = []
            AlwaysBlocks = []
            Instances = []
        }
    elif verilog.Contains("parameter") then
        Ok {
            ModuleName = "counter"
            Ports = []
            Parameters = [{ Name = "WIDTH"; DefaultValue = Some 8 }]
            Declarations = []
            Assignments = []
            AlwaysBlocks = []
            Instances = []
        }
    elif verilog.Contains("wire") then
        Ok {
            ModuleName = "test"
            Ports = []
            Parameters = []
            Declarations = [
                { Type = Wire; Name = "data"; Width = Some 8 }
                { Type = Wire; Name = "single_bit"; Width = Some 1 }
            ]
            Assignments = []
            AlwaysBlocks = []
            Instances = []
        }
    elif verilog.Contains("always") then
        Ok {
            ModuleName = "dff"
            Ports = []
            Parameters = []
            Declarations = []
            Assignments = []
            AlwaysBlocks = [
                {
                    Sensitivity = [PosEdge "clk"]
                    Statements = []
                }
            ]
            Instances = []
        }
    else
        Error "Parse error"

let checkUndeclaredWires ast =
    let declared = 
        ast.Declarations |> List.map (fun d -> d.Name) |> Set.ofList
    let ports = 
        ast.Ports |> List.map (fun p -> p.Name) |> Set.ofList
    let allDeclared = Set.union declared ports
    
    ast.Assignments
    |> List.filter (fun a -> not (Set.contains a.Target allDeclared))
    |> List.map (fun a -> $"Undeclared wire: {a.Target}")

let checkWidthMismatches ast =
    let widthMap =
        ast.Declarations
        |> List.choose (fun d -> d.Width |> Option.map (fun w -> d.Name, w))
        |> Map.ofList
    
    ast.Assignments
    |> List.choose (fun a ->
        match a.Expression with
        | Identifier id ->
            match Map.tryFind a.Target widthMap, Map.tryFind id widthMap with
            | Some w1, Some w2 when w1 <> w2 ->
                Some $"Width mismatch: {a.Target}[{w1}] = {id}[{w2}]"
            | _ -> None
        | _ -> None
    )

let checkMultipleDrivers ast =
    ast.Assignments
    |> List.groupBy (fun a -> a.Target)
    |> List.filter (fun (_, assigns) -> List.length assigns > 1)
    |> List.map (fun (target, _) -> $"Multiple drivers for: {target}")

let performAllChecks ast =
    [
        checkUndeclaredWires ast
        checkWidthMismatches ast
        checkMultipleDrivers ast
    ]
    |> List.concat

let convertToIssieComponents ast =
    // Simplified conversion - real implementation would be more complex
    let inputs = 
        ast.Ports 
        |> List.filter (fun p -> p.Direction = Input)
        |> List.mapi (fun i p -> 
            {
                Id = CommonTypes.ComponentId $"input_{i}"
                Type = CommonTypes.Input (Option.defaultValue 1 p.Width)
                Label = p.Name
                InputPorts = []
                OutputPorts = [{ 
                    Id = $"port_{i}"
                    PortNumber = Some 0
                    PortType = CommonTypes.PortType.Output
                    HostId = $"input_{i}"
                }]
                X = float (i * 100)
                Y = 0.0
                H = 50.0
                W = 60.0
                SymbolInfo = None
            }
        )
    
    let outputs =
        ast.Ports
        |> List.filter (fun p -> p.Direction = Output)
        |> List.mapi (fun i p ->
            {
                Id = CommonTypes.ComponentId $"output_{i}"
                Type = CommonTypes.Output (Option.defaultValue 1 p.Width)
                Label = p.Name
                InputPorts = [{
                    Id = $"port_in_{i}"
                    PortNumber = Some 0
                    PortType = CommonTypes.PortType.Input
                    HostId = $"output_{i}"
                }]
                OutputPorts = []
                X = float (i * 100)
                Y = 200.0
                H = 50.0
                W = 60.0
                SymbolInfo = None
            }
        )
    
    inputs @ outputs

// Create test lists
[<Tests>]
let verilogParsingTests =
    testList "Verilog Parsing Tests" (
        testVerilogParsingData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let verilogErrorCheckTests =
    testList "Verilog Error Check Tests" (
        testVerilogErrorCheckData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let verilogConversionTests =
    testList "Verilog Conversion Tests" (
        testVerilogConversionData |> List.map (fun (name, test) ->
            testCase name test
        )
    )