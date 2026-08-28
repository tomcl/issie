// to run this script do: 
// dotnet fable --noCache
// node .\testParser.fs.js

// maybe add code to generate parser
// need to run parser.js somehow
// if it says wrong Fable version, check the version Issie uses and update it here
// #r "nuget: Fable, 4.0.0-theta-018"
// #load "../NearleyBindings.fs"
module TestParser


// Copied from NearleyBindings module, make sure it's up to date
open Fable.Core
open Fable.Core.JsInterop
open VerilogTypes
open CommonTypes
open ErrorCheck
open ErrorCheckProcedural
open SimulationView
open SimGraphTypes
open SimTypes
open SheetCreator
open FastBuild
open FastRun
open GraphBuilder
open Node.Api
open Node.ChildProcess
open FSharp.Data
open EEExtensions
open NearleyBindings
open FSharp.Core
open TimeHelpers
open GraphMerger
open ElectronAPI
open JSHelpers



importGrammar
importParser
importFix
// //////////// Types for testing
type ExtraErrorInfoJson = {Text: string; Copy: bool; Replace: string}

type ErrorInfoJson = {Line:int; Col:int; Length: int; Message: string; ExtraErrors: ExtraErrorInfoJson array option}

let replaceTypeToString (replaceType: ReplaceType) =
    match replaceType with
    | IODeclaration -> "IODeclaration"
    | Assignment -> "Assignment"
    | Variable var -> var
    | NoReplace -> "NoReplace"

let stringToReplaceType replaceType =
    match replaceType with
    | "IODeclaration" -> IODeclaration
    | "Assignment" -> Assignment 
    | "NoReplace" -> NoReplace
    | var -> Variable var

let errorInfoToJson (errorInfo: ErrorInfo) =
    {
        Line = errorInfo.Line;
        Col = errorInfo.Col;
        Length = errorInfo.Length;
        Message = errorInfo.Message;
        ExtraErrors = 
            match errorInfo.ExtraErrors with
            | None -> None
            | Some extra -> Some (extra |> Array.map (fun extraError -> 
                let json =
                    {
                        Text = extraError.Text;
                        Copy = extraError.Copy;
                        Replace = replaceTypeToString extraError.Replace;
                    }
                json
            ))
            
    }


/////////////// File read/write functions ////////////////

open FilesIO
open Fable.SimpleJson



let parseFile src dst =
    printfn $"[TEST] Parsing file {baseNameWithoutExtension src}"
    let input = tryReadFileSync src
    let parse, linesIndex = 
        match input with
        | Ok msg -> 
            let parseRes = (parseFromFile msg) |> Json.parseAs<ParserOutput>
            Option.get parseRes.Result, Option.get parseRes.NewLinesIndex |> Array.toList

            //|> Json.parseAs<VerilogInput>
        | Error msg -> msg, []
    let output = writeFile dst parse 
    // match output with
    // | Ok msg -> "File parsed in"
    // | Error msg -> msg
    let fixedAST = fix parse
    let ast = Json.parseAs<VerilogInput> fixedAST

    ast, linesIndex

let project =
    {
        ProjectPath = "";
        OpenFileName = "";
        WorkingFileName = Some ""
        LoadedComponents = []
    }

let getSemanticErrorsWithLoadedComponents loadedComps ast linesIndex =
    ErrorCheck.getSemanticErrors ast linesIndex NewVerilogFile {project with LoadedComponents = loadedComps}

let getSemanticErrors ast linesIndex =
    getSemanticErrorsWithLoadedComponents [] ast linesIndex

let errorCheck ast linesIndex src dst =
    let refFile = FilesIO.pathJoin [|"./src/Renderer/VerilogComponent/test/ref/semantic"; (baseNameWithoutExtension src)+".json"|]
    let refOutput = tryReadFileSync refFile
    let refErrors =
        match refOutput with
        | Ok msg -> 
            Json.parseAs<ErrorInfoJson list> msg
            //|> Json.parseAs<VerilogInput>
        | Error msg -> 
            printfn $"[TEST] Failed reading in reference output file {refFile} {msg}"
            []
        //|> Set.ofList
    let outputErrors = getSemanticErrors ast linesIndex |> List.map errorInfoToJson //|> Set.ofList
    let writeRes = 
        outputErrors
        //|> Set.toList
        |> Json.stringify
        |> writeFile dst
    let outText = List.sort outputErrors |> Json.stringify
    let refText = List.sort refErrors |> Json.stringify
    if outText = refText then 
        1
    else 
        printfn $"[{baseNameWithoutExtension src}] FAIL"
        printfn $"Output errors: {outputErrors}"
        printfn $"Reference errors: {refErrors}"
        // let testDIFF = (outText = refText)
        0

let printSemanticErrors src (errors: ErrorInfo list) =
    let outputErrors = errors |> List.map errorInfoToJson
    printfn $"[TEST] couldn't parse input {baseNameWithoutExtension src}"
    printfn $"Output errors: {outputErrors}"
    printfn $"Output JSON: {outputErrors |> Json.stringify}"

    
let semanticErrorTests _ =
    let inputPath = "./src/Renderer/VerilogComponent/test/input/semantic"
    let outputPath = "./src/Renderer/VerilogComponent/test/output/semantic"
    let inputFiles =
        readFilesFromDirectory inputPath
        |> List.map (fun file -> pathJoin [|inputPath; file|])
    printfn "Testing semantic error checks"
    let nrOfTestCases = List.length inputFiles
    let outputFiles = List.map (fun file -> pathJoin [|outputPath; (baseNameWithoutExtension file) + ".json"|]) inputFiles
    let res =
        List.zip inputFiles outputFiles
        |> List.map (
            fun (src,dst) -> 
                let ast, linesIndex = parseFile src dst
                errorCheck ast linesIndex src dst
            )
        |> List.filter (fun res -> res=1)
        |> List.length
    printfn $"[TEST] {res}/{nrOfTestCases} passed"
    res, nrOfTestCases

type codegenInput = {Label: string; Width: int; Values: int list}
type codegenOutput = {Label: string; Values: bigint list}
type PortInfo = {Label: string; Width: int}
type Input = {Inputs:codegenInput List; Outputs: PortInfo List; IsClocked: bool; ModuleName: string}

let simulateAST ast src dst loadedComps=
    let model = Unchecked.defaultof<ModelType.Model>
    let dispatch = ignore
    let cs = createSheet ast {project with LoadedComponents=loadedComps} model dispatch
    // generate list of loaded components, then do the same for top level component
    let (loadedComp: LoadedComponent), _ = makeLoadedComponentFromCanvasData cs "" System.DateTime.MinValue None None
    let inputValuesFile = pathJoin [|(dirName src); (baseNameWithoutExtension src) + ".json"|]
    let inputValuesFile = tryReadFileSync inputValuesFile
    //let input = {Label="in"; Width=3; Values=[7;1;3;1;0;1;2;3;4;5]}
    //let inputs=[input]
    let inputs = 
        match inputValuesFile with
        | Ok data -> Json.parseAs<Input> data
        | Error error -> failwithf $"Couldn't open input data file {error}"
    let width = 8
    let ticks = inputs.Inputs[0].Values.Length+1
    let cnt = createComponent (CounterNoEnableLoad width) "cnt"
    // make a rom for each component
    let memories = 
        inputs.Inputs
        |> List.map (fun inputPort ->
            let data = inputPort.Values |> List.indexed |> List.map (fun (k, v) -> bigint k, bigint v) |> Map.ofList
            let (mem: Memory1) = {AddressWidth=width; Data=data; Init=FromData; WordWidth=inputPort.Width; Comments=None}
            let rom = createComponent (AsyncROM1 mem) "rom"
            // connect cnt out to rom in
            let conn = createConnection cnt.OutputPorts[0] rom.InputPorts[0]
            rom, conn
        )
    let comps, conns = List.unzip memories
    // same order as in Verilog input
    let verilog: CustomComponentType = {
            Name = loadedComp.Name
            InputLabels = CanvasExtractor.getOrderedCompLabels (Input1 (0, None)) cs
            OutputLabels = CanvasExtractor.getOrderedCompLabels (Output 0) cs
            Form = loadedComp.Form
            Description = loadedComp.Description
            ParameterBindings = None
        }
    let (verilogComp:Component) = createComponent (Custom verilog) "verilog"
    let verilogConns = 
        List.zip comps verilogComp.InputPorts
        |> List.map (fun (rom, inputport) -> createConnection rom.OutputPorts[0] inputport)
    let outputs, outConns = 
        List.zip verilog.OutputLabels verilogComp.OutputPorts
        |> List.map (fun (label, port) ->
            let out = createComponent (Output (snd label)) (fst label)
            let conn = createConnection port out.InputPorts[0]
            out, conn
        )
        |> List.unzip
    let outputCompIds = 
        outputs
        |> List.map (fun output -> output.Label, output.Id)
        |> Map.ofList

    let (topCanvas:CanvasState) =
        ([cnt]@comps@[verilogComp]@outputs, conns@verilogConns@outConns) 
        |> fixCanvasState

    let topLoadedComp, _ = makeLoadedComponentFromCanvasData topCanvas "" System.DateTime.MinValue None None
    let dependencies = ([loadedComp; topLoadedComp] @ loadedComps) |> List.distinct
    let simulationGraph = 
        match runCanvasStateChecksAndBuildGraph topCanvas dependencies with
        | Ok graph -> graph
        | _ -> failwithf "Wrong canvas state"
    
    let res =
        match mergeDependencies "test" simulationGraph topCanvas ([loadedComp]@loadedComps|>List.distinct) with
        | Ok graph -> 
            match buildFastSimulation ticks "wires" graph with// check if diagramname is fine?
            | Ok fs -> 
                runFastSimulation None (ticks-1) fs |> ignore
                
                // go through outputs and get the fastdata for them
                outputCompIds
                |> Map.map (fun label id ->
                    match fs.ComponentOf(id, []) with
                    | Some fc -> 
                        let data = FastExtract.getArrayOfOutputs fc 0 ticks
                        {Label=label; Values= data |> Array.toList}
                    | _ -> failwithf "What? output doesn't have a fastcomponent"            
                )
                |> Map.valuesL
            
            | _ -> failwithf "couldn't build simulation graph"
        | Error error -> failwithf $"wrong simulation graph - {error}"
        
    let writeRes =
        res
        |> Json.stringify
        |> writeFile dst 

    let refFilePath = pathJoin [|"./src/Renderer/VerilogComponent/test/ref/codegen"; baseNameWithoutExtension src|] + ".json"
    let refFile = tryReadFileSync refFilePath
    let refOutput = 
        match refFile with
        | Ok out -> Json.parseAs<codegenOutput list> out
        | _ -> failwithf "Couldn't open codegen reference output!"
        |> List.map (fun out -> {out with Label=out.Label.ToUpper()})
    match List.sort res = List.sort refOutput with
    | true -> 
        1
    | false -> 
        printfn $"[{baseNameWithoutExtension refFilePath}] FAIL"
        0

let formatSimulationError (error: exn) =
    error.Message

let runCodeGenTests _ =
    let inputPath = "./src/Renderer/VerilogComponent/test/input/codegen/single"
    let outputPath = "./src/Renderer/VerilogComponent/test/output/codegen"
    let inputFiles =
        readFilesFromDirectory inputPath
        |> List.filter (fun path -> hasExtn ".sv" path)
        |> List.map (fun file -> pathJoin [|inputPath; file|])
    let outputFiles = List.map (fun file -> pathJoin [|outputPath; (baseNameWithoutExtension file) + ".json"|]) inputFiles
    let res =
        List.zip inputFiles outputFiles
        |> List.map (
            fun (src,dst) -> 
                let ast, linesIndex = parseFile src dst
                match getSemanticErrors ast linesIndex with
                | [] ->  
                    try 
                        simulateAST ast src dst []
                    with error -> 
                        printfn $"[TEST] couldn't simulate AST for {baseNameWithoutExtension src}: {formatSimulationError error}"
                        0
                | errors -> 
                    printSemanticErrors src errors
                    0
            )
        |> List.filter (fun res -> res=1)
        |> List.length

    let inputPath = "./src/Renderer/VerilogComponent/test/input/codegen/multiple"
    let srcDirs = readFilesFromDirectory inputPath
    let multRes =
        srcDirs
        |> List.map (fun dir ->
            let modulePaths = 
                readFilesFromDirectory (pathJoin [|inputPath; dir|])
                |> List.sort
                |> List.map (fun file -> pathJoin [|inputPath; dir; file|])
                |> List.filter  (hasExtn ".sv")
            let topModulePath = List.last modulePaths
            let dst = pathJoin [|outputPath; baseNameWithoutExtension topModulePath|] + ".json"
            let loadedComps =
                ([], modulePaths)
                ||> List.fold (fun comps file ->
                    let ast, linesIndex = parseFile file dst
                    let cs =
                        match getSemanticErrorsWithLoadedComponents comps ast linesIndex with
                        | [] -> 
                            let model = Unchecked.defaultof<ModelType.Model>
                            let dispatch = ignore
                            createSheet ast {project with LoadedComponents=comps} model dispatch
                        | errors -> 
                            printSemanticErrors file errors
                            [],[]
                    
                    let lc, _ = makeLoadedComponentFromCanvasData cs ast.Module.ModuleName.Name System.DateTime.MinValue None None
                    comps@[lc]
                )
            let topast, toplinesIndex = parseFile topModulePath dst
            try
                simulateAST topast topModulePath dst loadedComps
            with error ->
                printfn $"[TEST] couldn't simulate AST for {baseNameWithoutExtension topModulePath}: {formatSimulationError error}"
                0
            
        )
        |> List.sum
    let nrOfTestCases = List.length inputFiles + List.length srcDirs
    printfn $"[TEST] {res+multRes}/{nrOfTestCases} passed"
    res+multRes

/// Compile or run a verilog file with Icarus, writing stdout to dst.
///
/// This used to spawn the program here. That single call was the last thing dragging node's
/// child process module into the renderer bundle - and it is exactly the shape a bridge must not
/// offer, since "run this command with these arguments" is the whole capability back again.
/// (Spelled without the module name on purpose: grepping the built bundle for a stray require is
/// how this class of problem gets found, and a comment that matches the search is a false lead.)
/// Main runs it instead, from a closed pair of programs; see Bridge.runDevTool. Fire and forget,
/// as this always was: every caller ignored the result.
let executeCommand (command1: string) (args1: List<string>) (dst: Option<string>) =
    #if FABLE_COMPILER
    Bridge.runDevTool command1 (Array.ofList args1) (Option.defaultValue "" dst)
    #else
    // Under plain .NET this is unreachable: nothing in the test suite drives the Icarus harness,
    // which needs iverilog on the path and the test checkout around it.
    printfn $"executeCommand '{command1}' is only available in the app"
    ignore args1
    ignore dst
    #endif


let icarusCompile src dst driver=
    let command = "iverilog"
    let args = ["-Wall"; "-g"; "2012"; "-o"; dst; "-s"; "top_module"; driver; src];
    executeCommand command args None 
    |> ignore

let icarusCompileTestCases () =
    let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/single"
    let driverDir = "./src/Renderer/VerilogComponent/test/input/driver"
    let dstDir = "./src/Renderer/VerilogComponent/test/bin"
    let srcFileNames =  
        readFilesFromDirectory srcDir
        |> List.filter (hasExtn ".sv")

    srcFileNames
    |> List.map (fun filename ->
        let srcFilePath = pathJoin [|srcDir; filename|] // extension?
        let driverFilePath = pathJoin [|driverDir; filename|]
        let dstFilePath = pathJoin [|dstDir; baseNameWithoutExtension filename|]
        icarusCompile srcFilePath dstFilePath driverFilePath
    )
    |> ignore

    // compile multiple module tests
    let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/multiple"
    let srcDirs = readFilesFromDirectory srcDir
    srcDirs
    |> List.map (fun dir ->
        let srcFilePath = pathJoin [|srcDir;dir|]+"/*.sv"
        let driverFilePath = pathJoin [|driverDir; dir|] + ".sv"
        let dstFilePath = pathJoin [|dstDir; dir|]
        icarusCompile srcFilePath dstFilePath driverFilePath
    )
    |> ignore


let icarusRun src dst =  
    let command = "vvp"
    let args = [src]
    executeCommand command args (Some dst) |> ignore

let icarusRunTestCases () =
    let srcDir = "./src/Renderer/VerilogComponent/test/bin"
    let dstDir = "./src/Renderer/VerilogComponent/test/ref/codegen"
    let binaries = readFilesFromDirectory srcDir
    binaries
    |> List.map (fun bin ->
        let binaryPath = pathJoin [|srcDir; bin|]
        let refFilePath = pathJoin [|dstDir; bin|] + ".json"
        icarusRun binaryPath refFilePath
    )
    |> ignore


let stringAppend (str1: string) (str2: string) =
    str2 + str1
/// Generate a top level module for a given DUT, return the Verilog module as a string
/// inputs stores the label and width of each input and output and the width and label of the output ports
let genDriver inputs =
    let module_definition = "module top_module;\n"
    let inputLabels =
        inputs.Inputs
        |> List.map (fun input -> {Label=input.Label; Width = input.Width})
    let cycles = inputs.Inputs[0].Values.Length
    let input_declarations =
        inputLabels @ inputs.Outputs
        |> List.map (fun input ->
            sprintf "  bit [%d:0] %s;\n" (input.Width - 1) input.Label
            |> stringAppend (sprintf "  bit [%d:0] %s [%d:0];\n" (input.Width-1) (input.Label+"_array") (cycles-1))
            )
        |> String.concat ""
        |> stringAppend "bit clk;\n" 
        |> stringAppend "integer i_, j_;\n"

    let initial =
        let clockGen =
            "  initial begin\n"
            |> stringAppend "   clk=0;\n"
            |> stringAppend $"    repeat({cycles*2+4 }) begin\n"
            |> stringAppend "      #1;\n"
            |> stringAppend "      clk=!clk;\n"
            |> stringAppend  "    end \n"
            |> stringAppend "    $display(\"[\");\n" 
        let outputs =
            inputs.Outputs
            |> List.mapi (fun i output ->
                let sep = if i<inputs.Outputs.Length-1 then ", " else ""
                sprintf "    $write(\"{\\\"Label\\\": \\\"%s\\\", \\\"Values\\\": [\");\n" output.Label
                |> stringAppend (sprintf "    for(i_=0;i_<%d; i_=i_+1) begin $write(\"%%d, \", %s[i_]); end\n" (cycles-1) (output.Label+"_array"))
                |> stringAppend (sprintf "    $display(\"%%d]}%s\", %s[%i]);\n" sep (output.Label+"_array") (cycles-1))
            )
            |> String.concat ""
            |> stringAppend "    $write(\"]\");\n"
            |> stringAppend "    $finish(0);\n"
        clockGen + outputs + "  end\n"
    let initial2 =
        let inputArrayAssigns =
            inputs.Inputs
            |> List.map (fun (input: codegenInput) ->
                input.Values
                |> List.mapi (fun i value ->
                    sprintf "      %s[%d] = %d'd%d;\n" (input.Label+"_array") i input.Width value
                )
                |> String.concat ""
            )
            |> String.concat ""
        let forLoop = sprintf "    for(j_=0; j_<%d; j_=j_+1) begin\n" cycles
        let inputAssigns =
            inputs.Inputs
            |> List.map (fun input ->
                sprintf "        %s=%s[j_];\n" input.Label (input.Label+"_array")
            )
            |> String.concat ""
        let outputAssigns =
            inputs.Outputs
            |> List.map (fun output ->
                sprintf "        %s[j_]=%s;\n" (output.Label+"_array") output.Label
            )
            |> String.concat ""
        "  initial begin\n"
        |> stringAppend inputArrayAssigns
        |> stringAppend forLoop
        //|> stringAppend "      @(posedge clk);\n"
        |> stringAppend inputAssigns
        |> stringAppend "      #0.5;\n"
        |> stringAppend outputAssigns
        |> stringAppend "      @(negedge clk);\n"
        |> stringAppend "end\n"
        |> stringAppend "  end\n"


    let dut_instantiation =
        let input_params =
            inputLabels@inputs.Outputs
            |> List.map (fun input -> sprintf ".%s(%s), " input.Label input.Label)
            |> String.concat ""
        let clock = if inputs.IsClocked then ".clk(clk), " else ""
            // add clock if necessary
        sprintf "  %s dut (%s);\n" inputs.ModuleName (String.trimEnd [|','; ' '|] (input_params+clock))


    module_definition + input_declarations + initial + initial2 + dut_instantiation + "endmodule"

let testInput: Input =
    {
        Inputs = [
            {
            Label="in";
            Width=3;
            Values=[7;1;3;1;0;1;2;3;4;5]
            }
        ];
        Outputs = [
            {Label="out"; Width=3}
        ];
        IsClocked=false;
        ModuleName="buffer"
    }

let genDriverFiles () =
    let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/single"
    let dstDir = "./src/Renderer/VerilogComponent/test/input/driver"

    let srcFileNames = 
        readFilesFromDirectory srcDir
        |> List.filter (hasExtn "json")
        |> List.map (fun f -> pathJoin [|srcDir; f|])

    let multSrcDir = "./src/Renderer/VerilogComponent/test/input/codegen/multiple"
    let multSrcFileNames = 
        readFilesFromDirectory multSrcDir
        |> List.collect (fun dir -> 
            readFilesFromDirectory (pathJoin [|multSrcDir; dir|])
            |> List.map (fun f -> pathJoin [|dir; f|])
        )
        |> List.filter (hasExtn "json")
        |> List.map (fun f -> pathJoin [|multSrcDir; f|])
    let srcFileNames = srcFileNames@multSrcFileNames
    srcFileNames
    |> List.map (fun filename -> // extension?
        let refFile = tryReadFileSync filename
        let inputs = 
            match refFile with
            | Ok out -> Json.parseAs<Input> out
            | _ -> failwithf "Couldn't open port info file for driver module generation %s" filename
        inputs, filename
    )
    |> List.map (fun (inputs, filename) ->
        let driverCode = genDriver inputs
        // write file to dstDir
        let dst = pathJoin [|dstDir; baseNameWithoutExtension filename|] + ".sv"
        driverCode
        |> writeFile dst 
    )
    |> ignore


let runCompilerTests _ =
    let filenames =
        readFilesFromDirectory "./src/Renderer/VerilogComponent/test/input/valid"
        |> List.map (fun file -> pathJoin [|"./src/Renderer/VerilogComponent/test/input/valid"; file|])
    let destinations = List.map (fun file -> FilesIO.pathJoin [|"./src/Renderer/VerilogComponent/test/output/valid"; (baseNameWithoutExtension file)+".json"|]) filenames
    List.zip filenames destinations 
    |> List.map (fun (src,dst) -> 
        parseFile src dst)
    |> ignore
    
    printfn "Testing compiler error checks"
    semanticErrorTests () |> ignore

    printfn "Testing code generation tests"
    runCodeGenTests () |> ignore

let rec unzip5 lst =
    match lst with
    | [] -> [],[],[],[],[]
    | (a,b,c,d,e) :: t -> 
        let a',b',c',d',e' = unzip5 t
        [a]@a', [b]@b', [c]@c', [d]@d', [e]@e'

let dummyProject = {ProjectPath=""; LoadedComponents=[]; OpenFileName="";WorkingFileName=None}

let standardDev (list: float list) =
    let count = List.length list
    let mean = List.sum list / float count
    let squaredDifferences = List.map (fun x -> (x - mean) ** 2.0) list
    let meanOfSquaredDifferences = List.sum squaredDifferences / float count
    let standardDeviation = sqrt meanOfSquaredDifferences
    standardDeviation


let unzip6 xs =
    xs
    |> List.fold (fun (a,b,c,d,e,f) (x1,x2,x3,x4,x5,x6) ->
        (x1::a, x2::b, x3::c, x4::d, x5::e, x6::f))
        ([],[],[],[],[],[])
    |> fun (a,b,c,d,e,f) ->
        (List.rev a, List.rev b, List.rev c, List.rev d, List.rev e, List.rev f)

let runPerformanceTests () =
    let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/single"
    let files = 
        readFilesFromDirectory srcDir
        |> List.filter (hasExtn ".sv")
    let model = Unchecked.defaultof<ModelType.Model>
    let dispatch = ignore
    
    files
    |> List.map (fun file ->
        let filePath = pathJoin [|srcDir; file|]
        let input = 
            match (tryReadFileSync filePath) with | Ok msg -> msg | _ -> failwithf "Couldn't read file"
        printfn "file: %A" file
        [1..100] 
        |>  List.map (fun _ ->
            // globalGC ()
            // start measuring
            let parseStart = getTimeMs()
            let parse, linesIndex = 
                let parseRes = (parseFromFile input) |> Json.parseAs<ParserOutput>
                Option.get parseRes.Result, Option.get parseRes.NewLinesIndex |> Array.toList
            let fixedAST = fix parse
            let ast = Json.parseAs<VerilogInput> fixedAST
            let parseEnd = getTimeMs()
            // parsing done
            // semantic error check starts
            ErrorCheck.getSemanticErrors ast linesIndex NewVerilogFile dummyProject |> ignore
            // semantic error check done
            // start synthesis
            let errorCheckEnd = getTimeMs()
            let components, connections = createSheet ast dummyProject model dispatch
            // synthesis end
            // stop measuring
            let synthesisEnd = getTimeMs ()
            
            (linesIndex.Length, components.Length, parseEnd-parseStart, errorCheckEnd-parseEnd, synthesisEnd-errorCheckEnd, synthesisEnd-parseStart)
        )
        |> unzip6
        |> (fun (lineNums, compNo, parse,error,synth,total) ->
            let a,b,c,d,e, f = lineNums[0], compNo[0], List.average parse, List.average error, List.average synth, List.average total 
            $"{a}, {b}, {c}, {d}, {e}, {f}\n"
        )
    )
    |> String.concat ""
    |> writeFile "./verilogPerformance.txt" |> ignore 

    files
    |> List.map (fun file ->
        let filePath = pathJoin [|srcDir; file|]
        let input = 
            match (tryReadFileSync filePath) with | Ok msg -> msg | _ -> failwithf "Couldn't read file"
        [1..100] 
        |>  List.map (fun _ ->
            let parse, linesIndex = 
                let parseRes = (parseFromFile input) |> Json.parseAs<ParserOutput>
                Option.get parseRes.Result, Option.get parseRes.NewLinesIndex |> Array.toList
            let fixedAST = fix parse
            let ast = Json.parseAs<VerilogInput> fixedAST
            let errorCheckEnd = getTimeMs()
            let cs = createSheet ast dummyProject model dispatch
            let synthesisEnd = getTimeMs ()
            (List.length (fst cs), synthesisEnd-errorCheckEnd)
        )
        |> List.unzip
        |> (fun (compNum, synth) ->
            let a,b = compNum[0], List.average synth
            $"{a}, {b}\n"
        )
    )
    |> String.concat ""
    |> printfn "%A"
    
    //ErrorCheck.getSemanticErrors  ast lineslocation NewVerilogFile dummyProject

let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
let heapLimitSize = () |> heapLimit |> float |> (fun v -> v / 1000000.)
let heapUsage = usedHeapSize / heapLimitSize * 100.

// let runMemoryTests () =
//     let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/single"
//     let files = 
//         readFilesFromDirectory srcDir
//         |> List.filter (hasExtn ".sv")

//     let model = Unchecked.defaultof<ModelType.Model>
//     // let dispatch = ignore

    
//     let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
//     let heapLimitSize = () |> heapLimit |> float |> (fun v -> v / 1000000.)
//     let heapUsage = usedHeapSize / heapLimitSize * 100.

//     files
//     |> List.map (fun file ->
//         let filePath = pathJoin [|srcDir; file|]
//         let input = 
//             match (tryReadFileSync filePath) with | Ok msg -> msg | _ -> failwithf "Couldn't read file"
//         [1..100] 
//         |>  List.map (fun _ ->
//             // start measuring

//             let mutable peak = usedHeapSize
//             let updatePeak() =
//                 peak <- max peak (usedHeapSize)
            
//             let initialMem = usedHeapSize

//             let parse, linesIndex = 
//                 let parseRes = (parseFromFile input) |> Json.parseAs<ParserOutput>
//                 Option.get parseRes.Result, Option.get parseRes.NewLinesIndex |> Array.toList
//             let fixedAST = fix parse
//             let ast = Json.parseAs<VerilogInput> fixedAST

//             // parsing done

//             let afterParseMem = usedHeapSize

//             // semantic error check starts
//             ErrorCheck.getSemanticErrors ast linesIndex NewVerilogFile dummyProject |> ignore
//             // semantic error check done
//             let afterErrorCheckMem = usedHeapSize

//             // start synthesis

//             let cs = createSheet ast dummyProject model dispatch
//             let components = fst cs
//             let toSaveCanvasState = Helpers.JsonHelpers.stateToJsonString (cs, None, Some {
//                                 Form = Some (Verilog "name");
//                                 Description=None;
//                                 ParameterDefinitions = None})
//             // synthesis end
//             // stop measuring
//             let afterSynthMem = usedHeapSize
            
//             (linesIndex.Length, components.Length, initialMem, afterParseMem,  afterErrorCheckMem, afterSynthMem)
//         )
//         |> unzip6
//         |> (fun (lineNums, compNo, parse,error,synth,total) ->
//             let a,b,c,d,e, f = lineNums[0], compNo[0], List.average parse, List.average error, List.average synth, List.average total 
//             $"{a}, {b}, {c}, {d}, {e}, {f}\n"
//         )
//     )
//     |> String.concat ""
//     |> writeFile "./verilogMemory.txt" |> ignore 
    
    
//     files
//     |> List.map (fun file ->
//         let filePath = pathJoin [|srcDir; file|]
//         let input = 
//             match (tryReadFileSync filePath) with | Ok msg -> msg | _ -> failwithf "Couldn't read file"
//         [1..100] 
//         |>  List.map (fun _ ->
//             let parse, linesIndex = 
//                 let parseRes = (parseFromFile input) |> Json.parseAs<ParserOutput>
//                 Option.get parseRes.Result, Option.get parseRes.NewLinesIndex |> Array.toList
//             let fixedAST = fix parse
//             let ast = Json.parseAs<VerilogInput> fixedAST
//             let errorCheckEnd = getTimeMs()
//             let cs = createSheet ast dummyProject model dispatch
//             let synthesisEnd = getTimeMs ()
//             (List.length (fst cs), synthesisEnd-errorCheckEnd)
//         )
//         |> List.unzip
//         |> (fun (compNum, synth) ->
//             let a,b = compNum[0], List.average synth
//             $"{a}, {b}\n"
//         )
//     )
//     |> String.concat ""
//     |> printfn "%A"


// Run node with --expose-gc for forceGC to work:
// "testmemory": "cd src/Renderer/VerilogComponent/test && dotnet fable --noCache && node --expose-gc testParser.fs.js"

let mbHeap () = usedHeap () |> float |> fun v -> v / 1_000_000.

/// Returns (before, after) heap in MB, with a forced GC before each snapshot
let measureMem (action: unit -> unit) : float * float =
    // globalGC ()
    let before = mbHeap ()
    action ()
    // globalGC ()
    let after = mbHeap ()
    before, after

let unzip4 xs =
    xs
    |> List.fold (fun (a,b,c,d) (x1,x2,x3,x4) ->
        (x1::a, x2::b, x3::c, x4::d))
        ([],[],[],[])
    |> fun (a,b,c,d) ->
        (List.rev a, List.rev b, List.rev c, List.rev d)

let runMemoryTests () =
    let srcDir = "./src/Renderer/VerilogComponent/test/input/codegen/single"
    let files = 
        readFilesFromDirectory srcDir
        |> List.filter (hasExtn ".sv")

    let model = Unchecked.defaultof<ModelType.Model>
    let dispatch = ignore

    let results =
        files
        |> List.map (fun file ->
            let filePath = pathJoin [|srcDir; file|]
            let input = 
                match tryReadFileSync filePath with
                | Ok msg -> msg
                | _ -> failwithf "Couldn't read file"

            printfn "[TEST] Measuring memory for %s" (baseNameWithoutExtension file)

            // Run 10 iterations and average 
            // (accumulated heap pressure corrupts later readings)
            let iterations = 10
            let measurements =
                [1..iterations]
                |> List.map (fun i ->
                    globalGC ()
                    // --- Parse phase ---
                    let parseBefore, parseAfter =
                        measureMem (fun () ->
                            let parseRes = parseFromFile input |> Json.parseAs<ParserOutput>
                            let parse = Option.get parseRes.Result
                            let linesIndex = Option.get parseRes.NewLinesIndex |> Array.toList
                            let fixedAST = fix parse
                            let _ast = Json.parseAs<VerilogInput> fixedAST
                            ()
                        )

                    // Re-parse cleanly for subsequent phases so each phase
                    // is measured on a consistent AST (not one mixed in with parse allocs)
                    let parseRes = parseFromFile input |> Json.parseAs<ParserOutput>
                    let parse = Option.get parseRes.Result
                    let linesIndex = Option.get parseRes.NewLinesIndex |> Array.toList
                    let fixedAST = fix parse
                    let ast = Json.parseAs<VerilogInput> fixedAST

                    // --- Error check phase ---
                    let errorBefore, errorAfter =
                        measureMem (fun () ->
                            ErrorCheck.getSemanticErrors ast linesIndex NewVerilogFile dummyProject |> ignore
                        )

                    // --- Synthesis phase ---
                    let synthBefore, synthAfter =
                        measureMem (fun () ->
                            let cs = createSheet ast dummyProject model dispatch
                            // Include serialisation as that's part of real usage
                            Helpers.JsonHelpers.stateToJsonString (cs, None, Some {
                                Form = Some (Verilog "name")
                                Description = None
                                ParameterDefinitions = None
                                IsTopSheet = None}) |> ignore
                        )

                    let parseNet  = parseAfter  - parseBefore
                    let errorNet  = errorAfter  - errorBefore
                    let synthNet  = synthAfter  - synthBefore
                    let total = synthAfter - parseBefore
                    parseNet, errorNet, synthNet, total
                )

            let parseNets, errorNets, synthNets, total = unzip4 measurements
            let avg = List.average

            // Re-parse once more to get line/component counts
            let parseRes = parseFromFile input |> Json.parseAs<ParserOutput>
            let parse = Option.get parseRes.Result
            let linesIndex = Option.get parseRes.NewLinesIndex |> Array.toList
            let fixedAST = fix parse
            let ast = Json.parseAs<VerilogInput> fixedAST
            let cs = createSheet ast dummyProject model dispatch
            let lineCount = List.length linesIndex
            let compCount = List.length (fst cs)

            printfn "[TEST] %s: %d lines, %d components" (baseNameWithoutExtension file) lineCount compCount
            printfn "  parse net:      avg=%.3f MB  stddev=%.3f" (avg parseNets) (standardDev parseNets)
            printfn "  error check net: avg=%.3f MB  stddev=%.3f" (avg errorNets) (standardDev errorNets)
            printfn "  synthesis net:   avg=%.3f MB  stddev=%.3f" (avg synthNets) (standardDev synthNets)
            printfn ("mbHeap heap: %A") usedHeapSize
            printfn ("heap limit: %A") heapLimitSize


            // CSV: filename, lines, components, parseNet, errorNet, synthNet
            sprintf "%s, %d, %d, %.6f, %.6f, %.6f, %.6f\n"
                (baseNameWithoutExtension file) lineCount compCount
                (avg parseNets) (avg errorNets) (avg synthNets) (avg total)
        )

    let csv =
        "file, lines, components, parseNetMB, errorCheckNetMB, synthesisNetMB, totalMB\n"
        + String.concat "" results

    csv |> writeFile "./verilogMemory.csv" |> ignore
    printfn "Memory tests done"