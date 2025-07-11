open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Newtonsoft.Json

// Data structures for AST analysis
type FunctionInfo = {
    Name: string
    Module: string
    Parameters: string list
    ReturnType: string option
    StartLine: int
    EndLine: int
    FilePath: string
}

type CallInfo = {
    Caller: string
    Callee: string
    Location: int * int  // line, column
    FilePath: string
    CallerModule: string
    CalleeModule: string option  // None if not resolved
    IsResolved: bool
}

type ModuleInfo = {
    Name: string
    FilePath: string
    Functions: string list
}

type AnalysisResult = {
    Functions: FunctionInfo list
    Calls: CallInfo list
    Modules: ModuleInfo list
    UnresolvedCalls: CallInfo list
}

// Global function registry for cross-file resolution
type GlobalFunctionRegistry = {
    Functions: Map<string, FunctionInfo>  // Key: fully qualified name
    ModuleFunctions: Map<string, string list>  // Key: module name, Value: function names
}

// Simple AST visitor
type ASTVisitor(filePath: string) =
    let mutable functions = []
    let mutable calls = []
    let mutable modules = []
    let mutable currentModule = ""
    
    member this.VisitImplementationFile(implFile: ParsedImplFileInput) =
        match implFile with
        | ParsedImplFileInput(_, _, _, _, _, moduleDecls, _, _, _) ->
            moduleDecls |> List.iter (fun m -> this.VisitModuleOrNamespace m)
    
    member this.VisitModuleOrNamespace(moduleOrNamespace: SynModuleOrNamespace) =
        match moduleOrNamespace with
        | SynModuleOrNamespace(longId, _, _, decls, _, _, _, _, _) ->
            let moduleName = longId |> List.map (fun id -> id.idText) |> String.concat "."
            currentModule <- moduleName
            
            // Track module
            let moduleInfo = {
                Name = moduleName
                FilePath = filePath
                Functions = []
            }
            modules <- moduleInfo :: modules
            
            // Visit declarations
            decls |> List.iter (fun decl -> this.VisitModuleDecl decl)
    
    member this.VisitModuleDecl(decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.Let(_, bindings, _) ->
            bindings |> List.iter (fun binding -> this.VisitBinding binding)
        | SynModuleDecl.NestedModule(_, _, decls, _, _, _) ->
            decls |> List.iter (fun decl -> this.VisitModuleDecl decl)
        | _ -> ()
    
    member this.VisitBinding(binding: SynBinding) =
        match binding with
        | SynBinding(_, _, _, _, _, _, _, pattern, _, expr, range, _, _) ->
            let funcName = this.ExtractPatternName pattern
            match funcName with
            | Some name ->
                let funcInfo = {
                    Name = name
                    Module = currentModule
                    Parameters = []
                    ReturnType = None
                    StartLine = range.StartLine
                    EndLine = range.EndLine
                    FilePath = filePath
                }
                functions <- funcInfo :: functions
                
                // Visit the expression to find calls
                this.VisitExpression expr name
            | None -> ()
    
    member this.VisitExpression(expr: SynExpr) (currentFunction: string) =
        // Use comprehensive function call detection
        this.FindFunctionCalls expr currentFunction
    
    member this.ExtractFunctionName(expr: SynExpr) : string option =
        match expr with
        | SynExpr.Ident(ident) -> Some ident.idText
        | SynExpr.LongIdent(_, longIdent, _, _) ->
            Some (longIdent.LongIdent |> List.map (fun id -> id.idText) |> String.concat ".")
        | _ -> None
    
    // Helper method to recursively find all function calls in an expression
    member this.FindFunctionCalls(expr: SynExpr) (currentFunction: string) : unit =
        match expr with
        | SynExpr.Ident(ident) ->
            // Filter out common keywords and built-in identifiers that are not function calls
            let isKeywordOrBuiltIn = 
                match ident.idText with
                | "true" | "false" | "null" | "None" | "Some" | "Ok" | "Error" | "List" | "Array" | "Map" | "Set" | "Option" | "Result" | "string" | "int" | "float" | "bool" | "unit" | "obj" | "byte" | "char" | "decimal" | "double" | "int16" | "int32" | "int64" | "sbyte" | "single" | "uint16" | "uint32" | "uint64" -> true
                | _ -> false
            
            if not isKeywordOrBuiltIn && ident.idText <> currentFunction then
                let callInfo = {
                    Caller = currentFunction
                    Callee = ident.idText
                    Location = (expr.Range.StartLine, expr.Range.StartColumn)
                    FilePath = filePath
                    CallerModule = currentModule
                    CalleeModule = None
                    IsResolved = false
                }
                calls <- callInfo :: calls
        
        | SynExpr.LongIdent(_, longIdent, _, _) ->
            let calleeName = longIdent.LongIdent |> List.map (fun id -> id.idText) |> String.concat "."
            let callInfo = {
                Caller = currentFunction
                Callee = calleeName
                Location = (expr.Range.StartLine, expr.Range.StartColumn)
                FilePath = filePath
                CallerModule = currentModule
                CalleeModule = None
                IsResolved = false
            }
            if calleeName <> currentFunction then
                calls <- callInfo :: calls
        
        | SynExpr.App(_, _, funcExpr, argExpr, _) ->
            // Extract function name from the application
            let calleeOpt = this.ExtractFunctionName funcExpr
            match calleeOpt with
            | Some callee when callee <> currentFunction ->
                let callInfo = {
                    Caller = currentFunction
                    Callee = callee
                    Location = (funcExpr.Range.StartLine, funcExpr.Range.StartColumn)
                    FilePath = filePath
                    CallerModule = currentModule
                    CalleeModule = None
                    IsResolved = false
                }
                calls <- callInfo :: calls
            | _ -> ()
            
            // Recursively traverse both function and argument expressions
            this.FindFunctionCalls funcExpr currentFunction
            this.FindFunctionCalls argExpr currentFunction
            
            // Special handling for high-order functions where the argument is a lambda
            match argExpr with
            | SynExpr.Lambda(_, _, _, lambdaBody, _, _, _) ->
                // Process the lambda body with the current function as the caller
                this.FindFunctionCalls lambdaBody currentFunction
            | SynExpr.Paren(SynExpr.Lambda(_, _, _, lambdaBody, _, _, _), _, _, _) ->
                // Handle parenthesized lambda expressions
                this.FindFunctionCalls lambdaBody currentFunction
            | _ -> ()
        
        | SynExpr.Paren(innerExpr, _, _, _) ->
            this.FindFunctionCalls innerExpr currentFunction
        
        | SynExpr.Tuple(_, exprs, _, _) ->
            exprs |> List.iter (fun e -> this.FindFunctionCalls e currentFunction)
        
        | SynExpr.ArrayOrList(_, exprs, _) ->
            exprs |> List.iter (fun e -> this.FindFunctionCalls e currentFunction)
        
        | SynExpr.Record(_, _, fields, _) ->
            fields |> List.iter (fun field -> 
                match field with
                | SynExprRecordField(_, _, expr, _) -> 
                    expr |> Option.iter (fun e -> this.FindFunctionCalls e currentFunction))
        
        | SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, _, _, _, _) ->
            this.FindFunctionCalls ifExpr currentFunction
            this.FindFunctionCalls thenExpr currentFunction
            elseExpr |> Option.iter (fun e -> this.FindFunctionCalls e currentFunction)
        
        | SynExpr.Lambda(_, _, _, body, _, _, _) ->
            // For lambda expressions, we should still track calls within the lambda
            // but they should be attributed to the containing function
            // This is crucial for capturing calls like: (fun fc -> couldBeSynchronousComponent fc.FType)
            this.FindFunctionCalls body currentFunction
        
        | SynExpr.Match(_, expr, clauses, _, _) ->
            this.FindFunctionCalls expr currentFunction
            clauses |> List.iter (fun clause -> 
                match clause with
                | SynMatchClause(_, whenExpr, resultExpr, _, _, _) ->
                    whenExpr |> Option.iter (fun e -> this.FindFunctionCalls e currentFunction)
                    this.FindFunctionCalls resultExpr currentFunction)
        
        | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
            this.FindFunctionCalls expr1 currentFunction
            this.FindFunctionCalls expr2 currentFunction
        
        | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
            bindings |> List.iter (fun binding -> 
                match binding with
                | SynBinding(_, _, _, _, _, _, _, _, _, expr, _, _, _) ->
                    this.FindFunctionCalls expr currentFunction)
            this.FindFunctionCalls body currentFunction
        
        | SynExpr.TypeApp(expr, _, _, _, _, _, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.Typed(expr, _, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.DotGet(expr, _, _, _) ->
            // Process property access - traverse the base expression
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.DotSet(expr1, _, expr2, _) ->
            this.FindFunctionCalls expr1 currentFunction
            this.FindFunctionCalls expr2 currentFunction
        
        | SynExpr.Set(expr1, expr2, _) ->
            this.FindFunctionCalls expr1 currentFunction
            this.FindFunctionCalls expr2 currentFunction
        
        | SynExpr.DotIndexedGet(expr, indexExpr, _, _) ->
            this.FindFunctionCalls expr currentFunction
            this.FindFunctionCalls indexExpr currentFunction
        
        | SynExpr.DotIndexedSet(expr1, indexExpr, expr2, _, _, _) ->
            this.FindFunctionCalls expr1 currentFunction
            this.FindFunctionCalls indexExpr currentFunction
            this.FindFunctionCalls expr2 currentFunction
        
        | SynExpr.While(_, whileExpr, doExpr, _) ->
            this.FindFunctionCalls whileExpr currentFunction
            this.FindFunctionCalls doExpr currentFunction
        
        | SynExpr.Do(expr, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.Upcast(expr, _, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.Downcast(expr, _, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.InferredUpcast(expr, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | SynExpr.InferredDowncast(expr, _) ->
            this.FindFunctionCalls expr currentFunction
        
        | _ -> ()
    
    member this.ExtractPatternName(pattern: SynPat) : string option =
        match pattern with
        | SynPat.Named(synIdent, _, _, _) -> 
            let (SynIdent(ident, _)) = synIdent
            Some ident.idText
        | SynPat.LongIdent(longDotId, _, _, _, _, _) ->
            Some (longDotId.LongIdent |> List.map (fun id -> id.idText) |> String.concat ".")
        | _ -> None
    
    member this.GetResults() : AnalysisResult =
        {
            Functions = List.rev functions
            Calls = List.rev calls
            Modules = List.rev modules
            UnresolvedCalls = []
        }

// Main analysis function
let analyzeFile (filePath: string) =
    try
        let sourceText = File.ReadAllText(filePath)
        let sourceFile = SourceText.ofString sourceText
        
        let checker = FSharpChecker.Create()
        let parseOptions = { 
            FSharpParsingOptions.Default with 
                SourceFiles = [|filePath|] 
        }
        
        let parseResults = checker.ParseFile(filePath, sourceFile, parseOptions) |> Async.RunSynchronously
        
        match parseResults.ParseTree with
        | tree ->
            let visitor = ASTVisitor(filePath)
            
            match tree with
            | ParsedInput.ImplFile(implFile) ->
                visitor.VisitImplementationFile implFile
            | ParsedInput.SigFile(_) -> ()
            
            visitor.GetResults()
    with
    | ex ->
        printfn "Error analyzing file %s: %s" filePath ex.Message
        { Functions = []; Calls = []; Modules = []; UnresolvedCalls = [] }

// Build global function registry
let buildGlobalRegistry (results: AnalysisResult list) : GlobalFunctionRegistry =
    let allFunctions = results |> List.collect (fun r -> r.Functions)
    let functionMap = 
        allFunctions
        |> List.map (fun f -> 
            let qualifiedName = if f.Module = "" then f.Name else f.Module + "." + f.Name
            (qualifiedName, f))
        |> Map.ofList
    
    let moduleFunctions = 
        allFunctions
        |> List.groupBy (fun f -> f.Module)
        |> List.map (fun (moduleName, funcs) -> 
            (moduleName, funcs |> List.map (fun f -> f.Name)))
        |> Map.ofList
    
    { Functions = functionMap; ModuleFunctions = moduleFunctions }

// Resolve cross-file function calls
let resolveCalls (registry: GlobalFunctionRegistry) (calls: CallInfo list) : CallInfo list * CallInfo list =
    let resolvedCalls = System.Collections.Generic.List<CallInfo>()
    let unresolvedCalls = System.Collections.Generic.List<CallInfo>()
    
    for call in calls do
        let mutable resolved = false
        
        // Try exact match first
        if registry.Functions.ContainsKey(call.Callee) then
            let targetFunc = registry.Functions.[call.Callee]
            resolvedCalls.Add({
                call with 
                    CalleeModule = Some targetFunc.Module
                    IsResolved = true
            })
            resolved <- true
        
        // Try module-qualified resolution
        if not resolved && call.Callee.Contains(".") then
            let parts = call.Callee.Split('.')
            if parts.Length >= 2 then
                let moduleName = parts.[0]
                let functionName = parts.[1]
                let fullName = moduleName + "." + functionName
                
                if registry.Functions.ContainsKey(fullName) then
                    let targetFunc = registry.Functions.[fullName]
                    resolvedCalls.Add({
                        call with 
                            CalleeModule = Some targetFunc.Module
                            IsResolved = true
                    })
                    resolved <- true
        
        // Try same-module resolution
        if not resolved then
            let sameModuleName = call.CallerModule + "." + call.Callee
            if registry.Functions.ContainsKey(sameModuleName) then
                let targetFunc = registry.Functions.[sameModuleName]
                resolvedCalls.Add({
                    call with 
                        CalleeModule = Some targetFunc.Module
                        IsResolved = true
                })
                resolved <- true
        
        // Try searching in all modules
        if not resolved then
            let mutable found = false
            for kvp in registry.ModuleFunctions do
                if not found && kvp.Value |> List.contains call.Callee then
                    let fullName = kvp.Key + "." + call.Callee
                    if registry.Functions.ContainsKey(fullName) then
                        let targetFunc = registry.Functions.[fullName]
                        resolvedCalls.Add({
                            call with 
                                CalleeModule = Some targetFunc.Module
                                IsResolved = true
                        })
                        found <- true
                        resolved <- true
        
        if not resolved then
            unresolvedCalls.Add(call)
    
    (resolvedCalls |> List.ofSeq, unresolvedCalls |> List.ofSeq)

// Analyze multiple files
let analyzeProject (projectPath: string) =
    let fsFiles = 
        if File.Exists(projectPath) then
            [projectPath]
        elif Directory.Exists(projectPath) then
            Directory.GetFiles(projectPath, "*.fs", SearchOption.AllDirectories) |> Array.toList
        else
            []
    
    printfn "Analyzing %d F# files..." fsFiles.Length
    let results = fsFiles |> List.map analyzeFile
    
    // Build global function registry
    let registry = buildGlobalRegistry results
    printfn "Built registry with %d functions across %d modules" 
        registry.Functions.Count registry.ModuleFunctions.Count
    
    // Collect all calls
    let allCalls = results |> List.collect (fun r -> r.Calls)
    
    // Resolve cross-file calls
    let (resolvedCalls, unresolvedCalls) = resolveCalls registry allCalls
    printfn "Resolved %d/%d function calls" resolvedCalls.Length allCalls.Length
    
    {
        Functions = results |> List.collect (fun r -> r.Functions)
        Calls = resolvedCalls
        Modules = results |> List.collect (fun r -> r.Modules)
        UnresolvedCalls = unresolvedCalls
    }

// Main entry point
[<EntryPoint>]
let main args =
    if args.Length < 1 then
        printfn "Usage: FSharpParser <project-path> [output-file]"
        1
    else
        let projectPath = args.[0]
        let outputFile = if args.Length >= 2 then args.[1] else "ast-analysis.json"
        
        try
            printfn "Analyzing F# project: %s" projectPath
            let results = analyzeProject projectPath
            
            printfn "Found %d functions, %d calls, %d modules" 
                results.Functions.Length results.Calls.Length results.Modules.Length
            
            let json = JsonConvert.SerializeObject(results, Formatting.Indented)
            File.WriteAllText(outputFile, json)
            
            printfn "Analysis results written to: %s" outputFile
            0
        with
        | ex ->
            printfn "Error: %s" ex.Message
            1