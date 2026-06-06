module ErrorCheck
open EEExtensions
open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes
open VerilogAST
open ErrorCheckProcedural
open ErrorCheckHelpers
open FilesIO
open NearleyBindings
open Fable.SimpleJson
open Fable.Core.JsInterop

NearleyBindings.importGrammar
NearleyBindings.importFix
NearleyBindings.importParser

let private getFileInProject (name:string) project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name.ToUpper() = name.ToUpper())

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Helper function to create an ErrorInfo-type Error Message 
/// given the location, the variable name, and the message
let createErrorMessage 
    (newLinesLocations: int list)
    (currLocation: int)
    (message: string)
    (extraMessages: ExtraErrorInfo array)
    (name: string)
        : ErrorInfo list = 
    
    let prevIndex = List.findIndexBack (fun x -> x <= currLocation) newLinesLocations
    let line = prevIndex+1
    let prevLineLocation = newLinesLocations[prevIndex]
    let length = String.length name
    
    [{Line = line; Col=currLocation-prevLineLocation+1;Length=length;Message = message;ExtraErrors=Some extraMessages}]


/// Checks whether all ports given in the beginning of the module are defined as input/output
/// Also if all ports have distinct names
let portCheck (ast: VerilogInput) linesLocations errorList  = 
    let portList = ast.Module.PortList |> Array.toList
    let distinctPortList = portList |> Seq.distinct |> List.ofSeq

    let locationList = ast.Module.Locations |> Array.toList
    let locationMap =
        (portList, locationList) ||> List.map2 (fun p i -> (p,int i)) |> Map.ofList
    match ast.Module.Type with
    | "module_new" -> errorList //if new-style there is no port list
    |_ ->
        match List.length portList = List.length distinctPortList with
        | false ->  //CASE 1: ports with same name
            portList
            |> List.map (fun name -> name.ToUpper())
            |> Seq.countBy id
            |> Map.ofSeq
            |> Map.filter (fun name count -> count > 1)
            |> Map.toList
            |> List.map fst
            |> List.collect (fun name ->
                let message = "Ports must have different names"     
                let extraMessages = [|
                    {Text=sprintf "Name '%s' has already been used for a port \n Please use a different name" name ;Copy=false;Replace=NoReplace}
                |]       
                createErrorMessage linesLocations locationMap[name] message extraMessages name
                )        
            |> List.append errorList 
    
        | true -> // Distinct names
            let items = ast.Module.ModuleItems.ItemList |> Array.toList
            let decls = 
                items |> List.collect (fun x -> 
                    match (x.IODecl |> isNullOrUndefined) with
                    | false -> 
                        match x.IODecl with
                        | Some d -> 
                            d.Variables 
                            |> Array.toList 
                            |> List.collect (fun x -> [x.Name]) 
                        | None -> []
                    | true -> []
                )
            let diff = List.except decls portList
            match Seq.isEmpty diff with
            | false ->  //CASE 2: ports not declared as input/output
                diff
                |> List.collect (fun name ->
                    let message = sprintf "Port '%s' is not declared either as input or output" name
                    let extraMessages = 
                        [|
                            {Text=sprintf "Port '%s' must be declared as input or output" name;Copy=false;Replace=NoReplace}
                            {Text=sprintf "input bit %s;|output bit %s;" name name;Copy=true;Replace=IODeclaration}
                        |]
                    createErrorMessage linesLocations locationMap[name] message extraMessages name
                )
                |> List.append errorList
            | true -> //CASE 3: no errors 
                errorList




/// Checks whether all ports defined as input/output are declared as ports in the module header
/// Also checks for double definitions and for input ports not used in the assignments
let checkIODeclarations 
    (ast: VerilogInput)
    (portWidthDeclarationMap: Map<string,int*int>) 
    (portLocationMap: Map<string,int>) 
    (linesLocations: int list) 
    (nonUniquePortDeclarations: string list)
    (portMap: Map<string,DeclarationDU>)
    (project: Project)
    (errorList: ErrorInfo list)
        : ErrorInfo list = 
    
    let portList = ast.Module.PortList |> Array.toList

    // let getPrimaryName (p: PrimaryDU) =
    //     match p with
    //     | Identifier id
    //     | IdentifierBit (id, _)
    //     | IdentifierBits (id, _, _)
    //     | IdentifierBitsSelect (id, _, _, _)
    //     | IdentifierArray (id, _) -> id.Name

    let moduleInstantiationsPrimaries = 
        ([], (VerilogInput ast)) ||> foldAST getModuleInstantiationStatements
        |> List.collect (fun modInst -> getModuleInstantiationInputPrimaries modInst project)
        |> List.map getPrimaryName
    // get variables from other expressions too
    let PrimariesUsedExpr =
        foldAST getAllExpressions' [] (VerilogInput(ast))
        |> List.map (fun expr -> primariesUsedInAssignment [] expr)
        |> List.concat
        |> List.map getPrimaryName
        |> List.append moduleInstantiationsPrimaries
    portWidthDeclarationMap
    |> Map.toList
    |> List.map fst
    |> List.collect (fun port -> 
        match ((List.contains port PrimariesUsedExpr),(Map.tryFind port portMap)) with
        | false, Some InputDecl -> // CASE 1: port is not used in the assignments
            // if port is clk we check if there are clocked always blocks
            let alwaysFFs = foldAST getAlwaysBlocks [] (VerilogInput(ast)) |> List.filter (fun always -> always.AlwaysType=AlwaysFF)
            if port = "clk" && alwaysFFs <> [] then errorList
            else
                let currLocation = Map.find port portLocationMap
                let message = sprintf "Variable '%s' is defined as an input port but is not used" port
                let extraMessages =
                    [|
                        {Text=sprintf "Variable '%s' is defined as an input port but is not used \n Please delete it if it is not needed" port;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations currLocation message extraMessages port
        | _, _ ->
            match (List.contains port portList) with
            | false -> // CASE 2: Doesn't exist in the module header (declaration present but not in module header)
                let currLocation = Map.find port portLocationMap
                let message = sprintf "Port '%s' is not defined as a port in the module declaration" port
                let extraMessages =
                    [|
                        {Text=sprintf "Port '%s' is not defined as a port \n Please define it in the module declaration" port;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations currLocation message extraMessages port
            | true -> // Exists in module header
                match List.contains port nonUniquePortDeclarations with
                | true -> // CASE 3: Double definition
                    let currLocation = Map.find port portLocationMap
                    let message = sprintf "Port '%s' is already defined" port
                    let extraMessages =
                        [|
                            {Text=sprintf "Port '%s' is already defined" port ;Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages port
                | false -> [] //CASE 4: No errors
    )
    |> List.append errorList   

/// Checks whether the IO declarations have correct width format (i.e. Little-endian)
let checkIOWidthDeclarations (ast: VerilogInput) paramMap linesLocations errorList  =
    let moduleAST = convertModule ast.Module
    moduleAST.ModuleItems.ItemList
    |> Array.filter (function ItemDU.IOItem {DeclarationType=declType} when declType = OutputDecl || declType = InputDecl -> true | _ -> false)
    |> Array.toList
    // |> List.map (function ItemDU.IOItem ioDecl -> ioDecl)
    // |> List.map (fun item -> Option.get item.IOItem)
    |> List.collect (function 
        | ItemDU.IOItem ioDecl -> 
            match ioDecl.Range with
            | None -> [] //No range given (i.e. one bit)
            | Some range ->
                // CASE 1: Wrong width format
                let bStart = evalExprWithParams range.Start paramMap
                let bEnd = evalExprWithParams range.End paramMap
                if (bEnd <> 0 || bStart <= bEnd) then
                    let message = "Wrong width declaration"
                    let temp = if bStart <= bEnd then "\nBig-Endian format is not allowed yet by ISSIE" else ""
                    let extraMessages = 
                        [|
                            {Text=(sprintf "A port's width can't be '[%i:%i]'\nCorrect form: [X:0]" bStart bEnd)+temp;Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations range.Location message extraMessages (string bStart + "[:0]")
                else [] //CASE 2: No Errors
        | _ -> []
    )
    |> List.append errorList

/// Checks if the name of the module is valid (i.e. this sheet doesn't exist)
let nameCheck (ast:VerilogInput) linesLocations (origin:CodeEditorOpen) (project:Project)  errorList = 
    let moduleName =  ast.Module.ModuleName.Name
    let exists, initialFileName = 
        match origin with
        |NewVerilogFile -> isFileInProject moduleName project , ""
        |UpdateVerilogFile initialName -> moduleName <> initialName, initialName

    let localError = 
        match (exists,origin) with
        |true,NewVerilogFile -> 
            let message = "A sheet/component with that name already exists"
            let extraMessages = 
                [|
                    {Text="Module Name must be different from existing Sheets/Components";Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations ast.Module.ModuleName.Location message extraMessages moduleName
        |true,UpdateVerilogFile _ ->
            let message = "Verilog component's name cannot be changed "
            let extraMessages = 
                [|
                    {Text="Module Name of Verilog component cannot be changed";Copy=false;Replace=NoReplace}
                    {Text= sprintf "%s" initialFileName ;Copy=true;Replace=Variable moduleName}
                |]
            createErrorMessage linesLocations ast.Module.ModuleName.Location message extraMessages moduleName
        |false,_ ->
            []
    
    List.append localError errorList

let getIdentifiers identifiers (node: ASTNode) =
    // match node with
    // | ASTNode.Expression expr ->
    //     identifiers @ [expr]
    // | _ -> expressions
    match node with
    | ASTNode.Primary p -> 
        let priName = getPrimaryName p
        identifiers @ [priName]
    // | ASTNode.AssignmentLHS a -> 
    //     let priName = getPrimaryName a.PrimaryType
    //     identifiers @ [priName]
    | _ -> identifiers

let getParamsUsed ast (paramMap: Map<string,int>) : List<string>=
    let identifiers = 
        foldAST getIdentifiers [] (VerilogInput ast)
    
    identifiers 
    |> List.fold (fun parameters i -> 
        match Map.tryFind i paramMap with
        | Some p -> [i] @ parameters
        | None -> parameters
        ) []

let getParamDeclarations paramDeclarations node =
    match node with
    | ParamDecl paramDecl -> 
        paramDecl.Parameters |> Array.toList |> List.append paramDeclarations
    | _ -> paramDeclarations


/// Checks parameters declared have been used
let checkParamsUsed 
    (ast: VerilogInput)
    (linesLocations: int list) 
    (paramMap: Map<string,int>)
    (errorList: ErrorInfo list)
        : ErrorInfo list = 

    let paramsUsed = getParamsUsed ast paramMap
    let paramDecls = foldAST getParamDeclarations [] (VerilogInput ast)

    let paramUsedErrors = 
        paramDecls
        |> List.collect (fun parameter -> 
            let paramName = parameter.Identifier.Name
            let paramLoc = parameter.Identifier.Location
            match (List.contains paramName paramsUsed) with
            | false ->
                let message = sprintf "Parameter '%s' is defined but is not used" paramName
                let extraMessages =
                    [|
                        {Text=sprintf "Parameter '%s' is defined but is not used \n Please delete it if it is not needed" paramName;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations paramLoc message extraMessages paramName
            | true -> []
        )
        |> List.append errorList  
    
    let decls = 
        foldAST getDeclarations [] (VerilogInput ast)
        |> List.map (fun d -> d.Variables |> Array.toList)

    let moduleAST = convertModule ast.Module
    let ioDecls = 
        moduleAST.ModuleItems.ItemList
        |> Array.filter (function ItemDU.IOItem {DeclarationType=declType} when declType = OutputDecl || declType = InputDecl -> true | _ -> false)
        |> Array.toList
        |> List.map (function 
            | ItemDU.IOItem ioDecl -> ioDecl.Variables |> Array.toList
            | _ -> [])
    
    let decls = 
        decls @ ioDecls
        |> List.concat

    let paramSet =
        paramDecls
        |> List.map (fun p -> p.Identifier.Name)
        |> Set.ofList
    
    printfn "decls %A, params: %A" decls paramSet

    let varOverlapErrors =
        decls
        |> List.filter (fun d -> Set.contains d.Name paramSet)
        |> List.collect (fun decl ->
            let extraMessages =
                [|
                    { Text = sprintf "Identifier '%s' is declared both as a parameter and as a variable/port\n Please change one." decl.Name; Copy = false; Replace = NoReplace }
                |]
            createErrorMessage linesLocations decl.Location (sprintf "Conflicting declaration '%s'" decl.Name) extraMessages decl.Name
        )
    
    paramUsedErrors @ varOverlapErrors


/// Checks if all declared output ports have a value assigned to them
/// The check is done bit-by-bit
let checkAllOutputsAssigned
    (ast:VerilogInput) 
    (portMap: Map<string,DeclarationDU>)
    (portSizeMap: Map<string,int>)  
    (paramMap: Map<string, int>)
    (linesLocations: int list)
    (errorList: ErrorInfo list)
        : ErrorInfo list =

    // List of declared ports, bit by bit
    // e.g. output [2:0] b -> b0,b1,b2
    let outputPortListMap = 
        portMap 
        |> Map.filter (fun _ s -> s = OutputDecl) 
        |> Map.toList 
        |> List.map fst
        |> List.collect (fun x -> 
            let size = Map.find x portSizeMap
            let names = [0..size-1] |> List.map (fun y -> (x+(string y),x))
            names 
        )
    let outputPortList = List.map fst outputPortListMap

    let getVariablesAssigned vars node =
        match node with
        | ContStatement contAssign ->
            match getPrimaryRange contAssign.Assignment.LHS.PrimaryType paramMap with
            | None ->
                vars @ [(getPrimaryName contAssign.Assignment.LHS.PrimaryType, -1, -1)]
            | Some (bStart, bEnd) ->
                [(getPrimaryName contAssign.Assignment.LHS.PrimaryType, bStart, bEnd)]
                |> List.append vars
        | Statement stmt ->
            match stmt with
            | BlockingAssign (blocking, _) ->
                vars @ [(getPrimaryName blocking.LHS.PrimaryType, -1, -1)]
            | NonBlockingAssign (nonblocking, _) ->
                vars @ [(getPrimaryName nonblocking.LHS.PrimaryType, -1, -1)]
            | _ -> vars
        | ModuleInstantiation modInst -> 
            modInst.Connections
            |> Array.toList
            |> List.map (fun connection ->
                match getPrimaryRange connection.Primary paramMap with
                | None -> (getPrimaryName connection.Primary, -1, -1)
                | Some (bStart, bEnd) -> (getPrimaryName connection.Primary, bStart, bEnd)
            )
            |> List.append vars
        | _ -> vars

    let variablesAssigned = foldAST getVariablesAssigned [] (VerilogInput ast)
    // List of assigned ports, bit by bit
    let assignmentPortList =
        variablesAssigned
        |> List.collect ( fun x ->
            match x with
            |(name,-1,-1)->
                match Map.tryFind name portSizeMap with
                | Some size -> 
                    let names = [0..size-1] |> List.map (fun y -> name+(string y))
                    names
                | None -> []
            |(name,x,y) when x=y ->
                [name+(string x)]
            |(name,bStart,bEnd)->
                let names = [bEnd..bStart] |> List.map (fun y -> name+ (string y))
                names
        )

    let genErrorMessage portList mapping errorType mess  = 
        match List.isEmpty portList with
        |true -> []
        |false ->
            // transform names from "b2" to "b[2]" 
            let fullNames = 
                portList 
                |> List.collect(fun x ->
                    match Map.tryFind x (Map.ofList mapping) with
                    | Some name -> 
                        let length = (Seq.except name x) |> Seq.map string |> String.concat ""
                        [name+"["+length+"]"]
                    | None -> []
                )
            let currLocation = ast.Module.EndLocation
            let message = mess
            let extraMessages = 
                match errorType with
                |Unassigned ->
                    [|
                        {Text=sprintf "The following ports are declared but not assigned: %A" fullNames;Copy=false;Replace=NoReplace};
                        {Text=sprintf "assign %s = 1'b0;" fullNames[0];Copy=true;Replace=ReplaceType.Assignment}
                    |]
                |DoubleAssignment ->
                    // handled in errorcheckprocedural
                    [||]
            match errorType with
            | Unassigned ->
                createErrorMessage linesLocations currLocation message extraMessages "endmodule"
            | _ -> []
    
    let countAssignments = assignmentPortList |> List.countBy id
    let notUnique = 
        countAssignments
        |> List.filter (fun (_,y)->y>1)
        |> List.map fst

    let unassignedPorts = List.except (List.toSeq assignmentPortList) (outputPortList)

    let localErrors =
        match unassignedPorts with
        |[] -> genErrorMessage notUnique outputPortListMap DoubleAssignment "Some output ports have been assigned more than once"
        |_ -> genErrorMessage unassignedPorts outputPortListMap Unassigned "All output ports must be assigned"

    List.append errorList localErrors

/// Checks that loop variables in for loops are not assigned to inside the loop body
let checkForLoopVar
    (ast: VerilogInput)
    (linesLocations: int list)
    (errorList: ErrorInfo list) =
    let forStatementsWithLoc = foldAST getForStatementsWithLoc [] (VerilogInput ast)
    let checkForStatement (forStmt, location) =
        let loopVarName = getPrimaryName forStmt.Initialisation.LHS.PrimaryType
        let assignments = foldAST getAssignments' [] (Statement forStmt.Statement)
        let loopVarAssignments = 
            assignments
            |> List.filter (fun assign -> 
                let assignedVarName = getPrimaryName assign.LHS.PrimaryType
                assignedVarName = loopVarName
            )

        if List.isEmpty loopVarAssignments then []
        else loopVarAssignments
            |> List.collect (fun assign ->
                let extraMessages =
                    [|
                        {Text=sprintf "The loop variable '%s' is assigned to inside the loop. The loop variable should not be assigned to inside the loop body." loopVarName;Copy=false;Replace=NoReplace};
                    |]
                let message = sprintf "Loop variable should not be assigned to inside the loop"
                createErrorMessage linesLocations (lhsLocation assign.LHS) message extraMessages loopVarName
                )
    let localErrors = List.collect checkForStatement forStatementsWithLoc
    List.append errorList localErrors


/// Checks estimated program size after loop unrolling.
let checkForLoopUnrollCost (ast: VerilogInput) (linesLocations: int list) (errorList: ErrorInfo list) : ErrorInfo list =
    let maxUnrollCost = 500 // TODO: instrument size and adjust this accordingly
    let tryEval expr =
        try 
            Ok (evalExpr expr) 
        with _ -> Error "For loop bounds must be constant."

    let tryGetIterations (f: ForStatement) =
        let startRes = tryEval f.Initialisation.RHS
        // let stepRes = 
        let stepRes, localErrors = 
            match f.Step.RHS with
            | ExpressionDU.Additive (Plus, _, step) -> 
                tryEval step, []
            | ExpressionDU.Additive (Minus, _, step) -> 
                tryEval step, []
            | _ -> 
                let extraMessages = [| {Text="For loop step must be an addition or subtraction.";Copy=false;Replace=NoReplace} |]
                Error "Invalid step", createErrorMessage linesLocations f.Location "For loop step must be an addition or subtraction." extraMessages (getPrimaryName f.Initialisation.LHS.PrimaryType)
        let condRes =
            printfn "Condition: %A" f.Condition
            match f.Condition with
            | Comparison (op, _, rhs) -> Ok (op, rhs)
            | _ -> Error "For loop condition must be a comparison."
        match startRes, stepRes, condRes with
        | Ok startV, Ok stepV, Ok (op, rhs) ->
            match tryEval rhs with
            | Ok endV ->
                // match op with
                //     | Lt -> Ok (stepV, endV - startV), localErrors
                //     | Lte -> Ok (stepV, endV - startV + 1), localErrors
                //     | Gt -> Ok (stepV, startV - endV), localErrors
                //     | Gte -> Ok (stepV, startV - endV + 1), localErrors
                //     | _ -> Error "Unsupported for loop comparison operator.", List.append localErrors (createErrorMessage linesLocations f.Location "Unsupported for loop comparison operator." [| {Text="For loop condition must be a comparision." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
                match f.Step.RHS with
                | ExpressionDU.Additive (Plus, _, _) ->
                    let startFloat, endFloat, stepFloat = float (startV), float (endV), float (stepV)
                    match op with
                    | Lt -> Ok (stepV, int (floor (endFloat - startFloat)/stepFloat)), localErrors
                    | Lte -> Ok (stepV, int (floor (endFloat - startFloat + 1.0)/stepFloat)), localErrors
                    | Gte -> Ok (stepV, int (floor (startFloat - endFloat + 1.0)/stepFloat)), localErrors
                    | Gt -> 
                        // printfn "startV: %d, endV: %d, stepV: %d" startV endV stepV
                        Ok (stepV, int (floor (startFloat - endFloat)/stepFloat)), localErrors
                    // | Gt -> Error "Infinite for loop", List.append localErrors (createErrorMessage linesLocations f.Location "Infinite for loop!" [| {Text="For loop bounds must produce a finite number of iterations." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
                    // | Gte -> Ok (stepV, startV - endV + 1), localErrors
                    | _ -> Error "Unsupported for loop comparison operator.", List.append localErrors (createErrorMessage linesLocations f.Location "Unsupported for loop comparison operator." [| {Text="For loop condition must be a comparision." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
                | ExpressionDU.Additive (Minus, _, _) ->
                    match op with 
                    | Gt -> Ok (stepV, startV - endV), localErrors
                    | Gte -> Ok (stepV, startV - endV + 1), localErrors
                    | Lt -> Ok (stepV, startV - endV), localErrors
                    | Lte -> Ok (stepV, startV - endV + 1), localErrors
                    // | Lte -> Error "Infinite for loop", List.append localErrors (createErrorMessage linesLocations f.Location "Infinite for loop!" [| {Text="For loop bounds must produce a finite number of iterations." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
                    | _ -> Error "Unsupported for loop comparison operator.", List.append localErrors (createErrorMessage linesLocations f.Location "Unsupported for loop comparison operator." [| {Text="For loop condition must be a comparision." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
                | _ -> Error "Invalid for loop step", List.append localErrors (createErrorMessage linesLocations f.Location "Invalid for loop step." [| {Text="For loop step must be an addition or subtraction." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
            | Error msg -> Error msg, List.append localErrors (createErrorMessage linesLocations f.Location msg [| {Text="For loop condition must evaluate to a constant." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
            // let loopCount = evalExpr rhs
            // if loopCount < 0 then 
            //     let message = sprintf "For loop produces infinite loop!"
            //     let extraMessages = 
            //         [|
            //             {Text=sprintf "For loop produces infinite loop! \n Please make sure that the loop bounds produce a finite number of iterations." ;Copy=false;Replace=NoReplace}
            //         |]
            //     Error "Invalid loop bounds", List.append localErrors (createErrorMessage linesLocations f.Location message extraMessages (getPrimaryName f.Initialisation.LHS.PrimaryType)) 
            // else
        | Error msg, _, _ -> Error msg, List.append localErrors (createErrorMessage linesLocations f.Location msg [| {Text="For loop initialisation must evaluate to a constant." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
        | _, Error msg, _ -> Error msg, List.append localErrors (createErrorMessage linesLocations f.Location msg [| {Text="For loop step must be an addition or subtraction." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))
        | _, _, Error msg -> Error msg, List.append localErrors (createErrorMessage linesLocations f.Location msg [| {Text="For loop condition must be a comparison." ;Copy=false;Replace=NoReplace} |] (getPrimaryName f.Initialisation.LHS.PrimaryType))

    let mkError loc name message detail =
        let extraMessages = [| {Text=detail;Copy=false;Replace=NoReplace} |]
        createErrorMessage linesLocations loc message extraMessages name

    let rec estimateUnrolledStatementCost (stmt: StatementDU) : ErrorInfo list * int =
        match stmt with
        | BlockingAssign (a, _) ->
            [], estimateAssignmentCost a
        | NonBlockingAssign (a, _) ->
            [], estimateAssignmentCost a
        | SeqBlock (stmts, _) ->
            (([], 0), stmts)
            ||> Array.fold (fun (errs, cost) s ->
                let errs', cost' = estimateUnrolledStatementCost s
                (errs @ errs', cost + cost'))
        | StatementDU.Case (c, _) ->
            let caseErrors, caseCost =
                (([], 0), c.CaseItems)
                ||> Array.fold (fun (errs, cost) item ->
                    let errs', cost' = estimateUnrolledStatementCost item.Statement
                    (errs @ errs', cost + cost'))
            let defaultErrors, defaultCost =
                c.Default
                |> Option.map estimateUnrolledStatementCost
                |> Option.defaultValue ([], 0)
            let errors = caseErrors @ defaultErrors
            let cost = 1 + estimateExprCost c.Expression + caseCost + defaultCost
            errors, cost
        | Conditional (ifStmt, elseStmt, _) ->
            let ifErrors, ifCost = estimateUnrolledStatementCost ifStmt.Statement
            let elseErrors, elseCost =
                elseStmt
                |> Option.map estimateUnrolledStatementCost
                |> Option.defaultValue ([], 0)
            let errors = ifErrors @ elseErrors
            let cost = 1 + estimateExprCost ifStmt.Condition + ifCost + elseCost
            errors, cost
        | StatementDU.ForStatement (f, _) ->
            let loopVarName = getPrimaryName f.Initialisation.LHS.PrimaryType
            match tryGetIterations f with
            | Error msg, localErrors ->
                // let localErrors = List.append localErrors (mkError f.Location loopVarName "Invalid for loop bounds" msg)
                // printfn "Local errors: %A" localErrors
                let newError = mkError f.Location loopVarName "Invalid for loop bounds" msg
                localErrors @ newError, 0
                // List.append errorList localErrors, 0
            | Ok (stepV, iterations), localErrors ->
                if stepV = 0 then
                    let localErrors = List.append localErrors (mkError f.Location loopVarName "Infinite for loop!" "For loop step must be a non-zero constant.")
                    localErrors, 0
                    // List.append errorList localErrors, 0
                elif iterations <= 0 then
                    let localErrors = List.append localErrors (mkError f.Location loopVarName "Infinite for loop!" "For loop bounds must not create an infinite loop.")
                    // List.append errorList localErrors, 0
                    localErrors, 0
                else
                    let bodyErrors, bodyCost = estimateUnrolledStatementCost f.Statement
                    let totalCost = iterations * bodyCost
                    List.append localErrors bodyErrors, totalCost
                    // List.append errorList localErrors, totalCost

    let estimateProgramCost (items: ItemT list) =
        (([], 0), items)
        ||> List.fold (fun (errs, cost) (item: ItemT) ->
            let itemErrors, itemCost =
                match item.Statement, item.AlwaysConstruct with
                | Some contAssign, _ ->
                    let assign = convertAssignment contAssign.Assignment Blocking
                    [], estimateAssignmentCost assign
                | None, Some always ->
                    estimateUnrolledStatementCost (convertStatement always.Statement)
                | _ ->
                    [], 0
            (errs @ itemErrors, cost + itemCost))

    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let loopErrors, totalCost = estimateProgramCost items
    let totalError =
        if totalCost > maxUnrollCost then
            let name = ast.Module.ModuleName.Name
            let loc = ast.Module.ModuleName.Location
            let detail = sprintf "Estimated unrolled program cost (%d) exceeds the limit (%d). Consider reducing for loop size if possible." totalCost maxUnrollCost
            mkError loc name "Program too large" detail
        else []
    List.append errorList (loopErrors @ totalError)

/// Helper recursive function to transform the produced OneUnary-type tree
/// by RHSUnaryAnalysis to a string which can be used for ErrorInfo
let rec unaryTreeToString treeDepth targetLength (unary:OneUnary)  =
    let targetLength' = targetLength //if targetLength=(-2) then 1 else targetLength
    let depthToSpaces = ("",[0..treeDepth])||>List.fold (fun s v -> s+"   ") 
    let sizeString =
        match targetLength' with
        |(-1) -> (string unary.ResultWidth)
        |(-2) when unary.ResultWidth<>1  -> (string unary.ResultWidth)+" -> ERROR! (Exp: 1, condition must be a single bit!)"
        |(-2) -> (string unary.ResultWidth)
        |x when x=(unary.ResultWidth)-> (string unary.ResultWidth)
        |_ -> (string unary.ResultWidth)+" -> ERROR! (Exp: "+(string targetLength')+")"
    
    let propagatedLength =
            match unary.Name with
            |"{...}" -> (-1)
            |"[condition]" -> targetLength
            |"[reduction]" -> (-1)
            |"[logical_op]" -> (-1)
            | _ -> unary.ResultWidth

    let elem =
        match unary.Name with
        |"[bitwise_op]" |"[logical_op]" ->
            let s1 =  (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Head))
            let s2 = (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Tail))
            s1+s2
        |"[conditional]" ->
            let cond = unaryTreeToString (treeDepth+2) (-2) unary.Elements[0]
            let s1 =  (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Head))
            let s2 = (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Tail))
            cond+s1+s2
        |"[reduction]" when unary.Elements = [] -> ""
        |"[reduction]" |"(...)" ->
            unaryTreeToString (treeDepth+2) propagatedLength unary.Elements[0]
        |"[shift]" -> ""    
        |"{...}" ->
            ("",[0..((List.length unary.Elements)-1)])||>List.fold (fun s v -> s+(unaryTreeToString (treeDepth+2) propagatedLength unary.Elements[v]))
        |_ -> ""

    match elem with
    |"" ->
        depthToSpaces+
        "-'"+
        unary.Name+
        "' with Width: "+
        sizeString+
        "\n"
    |_ ->
        depthToSpaces+
        "-'"+
        unary.Name+
        "' with Width: "+
        sizeString+
        "\n"+
        depthToSpaces+
        "   "+
        "Elements: \n"+
        elem
    


/// Checks one-by-one all wire and output port assignments for:
/// 1) LHS Name and Width
/// 2) RHS Names
/// 3) RHS Width of inputs/wires
/// 4) Width LHS = Width RHS 
let checkWiresAndAssignments 
    (ast:VerilogInput) 
    (portMap: Map<string,DeclarationDU>)
    (portSizeMap:Map<string,int>)
    (portWidthDeclarationMap: Map<string,(int*int)>)
    (inputNameList: string list) 
    (linesLocations: int list) 
    (wireNameList: string list) 
    (wireSizeMap: Map<string,int>) 
    (wireLocationMap: Map<string,int>) 
    (arraySizeMap: Map<string, int * int array>)
    (paramMap: Map<string,int>)
    (errorList: ErrorInfo list) 
        : ErrorInfo list =

    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    let logicNameList = 
        declarations
        |> List.collect (fun decl -> List.ofArray decl.Variables)
        |> List.map (fun id -> id.Name)
    let wireNameList' = wireNameList @ logicNameList
    
    let portAndWireNames =
        portMap
        |> Map.toList
        |> List.map fst
        |> List.append wireNameList'

    let outputNameList = portMap |> Map.keysL
    /// Helper function to extract all inputs + wires declared + outputs
    /// prior to the assignment being checked
    let getCurrentInputWireList location = 
        wireNameList'
        |> List.filter (fun x -> 
            match (Map.tryFind x wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> List.append inputNameList
        |> List.append outputNameList

    let getCurrentParamList = 
        paramMap
        |> Map.toList
        |> List.map fst

    /// Helper function to print errors
    let getPortTypesString portType= 
        match portType with
        | InputDecl -> "input"
        | OutputDecl -> "output"
        | LogicDecl -> "logic"
        | ParameterDecl -> "parameter"
    
    /// Checks the name and width of a wire assignment
    /// Name : if the variable is free
    /// Width : correct definition of width (i.e. Little-endian)
    let checkWireNameAndWidth wire notUniqueNames (localErrors:ErrorInfo list) =     
        let lhs = wire.LHS.PrimaryType
        let lhsName = getPrimaryName lhs
        let lhsLoc = getPrimaryLocation lhs
        match Map.tryFind lhsName portMap with
        | Some portType  ->  //CASE 1: Invalid Name (already used variable by port)
            let portTypeS = getPortTypesString portType
            let message = sprintf "Variable '%s' is already used by a port" lhsName
            let extraMessages = 
                [|
                    {Text=(sprintf "Variable '%s' is declared as an %s port\nPlease use a different name for this wire" lhsName portTypeS);Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations lhsLoc message extraMessages lhsName
        | _ -> 
            match List.tryFind (fun x -> x=lhsName) notUniqueNames with
            | Some found  -> //CASE 2: Invalid Name (already used variable by another wire)
                let message = sprintf "Identifier '%s' is already used by another variable" lhsName
                let extraMessages = 
                    [|
                        {Text=(sprintf "Identifier '%s' is already used by another variable\nPlease use a different name for this wire" lhsName);Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations lhsLoc message extraMessages lhsName
            | _ ->
                match getPrimaryRange lhs paramMap with
                | None -> localErrors // No errors
                | Some (bStart, bEnd) -> 
                    // CASE 3: Wrong Width declaration
                    if (bEnd <> 0 || bStart <= bEnd) then
                        let message = "Wrong width declaration"
                        let extraMessages = 
                            [|
                                {Text=(sprintf "A port's width can't be '[%i:%i]'\nCorrect form: [X:0]" bStart bEnd);Copy=false;Replace=NoReplace}
                            |]
                        createErrorMessage linesLocations lhsLoc message extraMessages lhsName
                    else localErrors // No errors

    let checkLogicName (decl: Declaration) notUniqueNames (localErrors:ErrorInfo list) =
        let variables = decl.Variables
        (localErrors, variables)
        ||> Array.fold (fun errorList lhs ->
            match Map.tryFind lhs.Name portMap with
            | Some portType  ->  //CASE 1: Invalid Name (already used variable by port)
                let portTypeS = getPortTypesString portType
                let message = sprintf "Variable '%s' is already used by a port or variable" lhs.Name
                let extraMessages = 
                    [|
                        {Text=(sprintf "Variable '%s' is declared as an %s port\nPlease use a different name for this variable" lhs.Name portTypeS);Copy=false;Replace=NoReplace}
                    |]
                errorList @ createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
            | _ -> 
                match List.tryFind (fun x -> x=lhs.Name) notUniqueNames with
                | Some found  -> //CASE 2: Invalid Name (already used variable by another wire)
                    let message = sprintf "Variable '%s' is already used by another wire" lhs.Name
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Variable '%s' is already used by another wire\nPlease use a different name for this wire" lhs.Name);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
                | _ ->
                    match isNullOrUndefined decl.Range with
                    |true -> localErrors // No errors
                    |false -> 
                        let bStart = evalExprWithParams (Option.get decl.Range).Start paramMap
                        let bEnd = evalExprWithParams (Option.get decl.Range).End paramMap
                        // CASE 3: Wrong Width declaration
                        if (bEnd <> 0 || bStart <= bEnd) then
                            let message = "Wrong width declaration"
                            let extraMessages = 
                                [|
                                    {Text=(sprintf "A port's width can't be '[%i:%i]'\nCorrect form: [X:0]" bStart bEnd);Copy=false;Replace=NoReplace}
                                |]
                            createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
                        else localErrors // No 
        )

    /// Checks the name and width of an output port assignment
    /// Name : if the variable is indeed an output port
    /// Width : width is within the declared width range
    let checkAssignmentNameAndWidth assignment localErrors = 
        let lhs = assignment.LHS.PrimaryType
        let lhsName = getPrimaryName lhs
        let lhsLoc = getPrimaryLocation lhs
        match Map.tryFind lhsName portMap with
        | Some found when found = OutputDecl -> 
            match Map.tryFind lhsName portWidthDeclarationMap with
            | Some (bStart,bEnd) -> 
                match getPrimaryRange lhs paramMap with
                | Some (lhsStart, lhsEnd) ->
                    if (bStart >= lhsStart) && (bEnd <= lhsEnd) then
                        localErrors
                    else 
                        let definition =
                            match bStart=bEnd with
                            |true -> " a single bit "
                            |false -> sprintf " %s[%i:0] " lhsName (bStart)
                        let usedWidth, message =
                            match lhsStart = lhsEnd with
                            |true -> 
                                sprintf " %s[%i] " lhsName lhsStart, sprintf "Out of bounds index for variable '%s'" lhsName
                            |false -> 
                                sprintf " %s[%i:%i] " lhsName lhsStart lhsEnd, sprintf "Out of bounds range for variable '%s'" lhsName
                        //let message = sprintf "Wrong width of variable: '%s'" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" lhsName)+definition+"\nTherefore,"+usedWidth+"is invalid" ; Copy=false;Replace=NoReplace}
                                {Text=sprintf "assign %s = 0;"lhsName; Copy=true;Replace=ReplaceType.Assignment}
                            |]
                        List.append 
                            localErrors 
                            (createErrorMessage linesLocations lhsLoc message extraMessages lhsName)
                | None -> localErrors
            | None -> failwithf "Can't happen! PortMap and PortSizeMap should have the same keys"
        | _ -> 
            // check if a logic with this name has been declared
            let wiresDeclared = getCurrentInputWireList lhsLoc
            match List.tryFind (fun wire -> wire = lhsName) wiresDeclared  with
            | Some _ -> 
                errorList
            | _ ->
                let message = sprintf "Variable '%s' is not declared as an output port" lhsName
                let extraMessagesMain = 
                    [|
                        {Text=(sprintf "Variable '%s' is not declared as an output port" lhsName);Copy=false;Replace=NoReplace}
                    |]

                let possibleAddition =
                    match ast.Module.Type with
                    |"module_new" -> [||]
                    |_ -> [|{Text=(sprintf "output bit %s;" lhsName);Copy=true;Replace=IODeclaration}|]

                let extraMessages = Array.append extraMessagesMain possibleAddition

                List.append 
                    localErrors 
                    (createErrorMessage linesLocations lhsLoc message extraMessages lhsName)

    /// Checks if the variables used in the RHS of on assignment
    /// (either output port or wire) have been declared as input or wire
    let checkNamesInPrimaries (primariesRHS: PrimaryDU list) currentInputWireList localErrors = 
        //let PrimariesRHS = primariesUsedInAssignment [] expression
        
        let namesWithLocRHS =
            primariesRHS
            |> List.map (fun x -> (getPrimaryName x, getPrimaryLocation x))
        let namesRHS = namesWithLocRHS |> List.map fst
        let namesToLocMap = namesWithLocRHS |> Map.ofList

        let diff = List.except (List.toSeq (List.append currentInputWireList ["delete123"])) namesRHS
        match List.isEmpty diff with
        | true -> localErrors
        | false -> 
            diff
            |> List.collect (fun name ->
                let currLocation = Map.find name namesToLocMap
                match List.exists (fun x->x=name) wireNameList' with
                |true ->
                    let message = sprintf "Variable '%s' is defined after this assignment" name
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Variable '%s' is defined after this assignment" name);Copy=false;Replace=NoReplace}
                            {Text=(sprintf "Move the definition of variable '%s' above this line" name);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages name
                |false ->
                    let closeVariables = findCloseVariable name portAndWireNames 
                    match List.isEmpty closeVariables with
                    |true ->
                        let message = sprintf "Variable '%s' is not declared as input or variable" name
                        let extraMessagesMain = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or variable" name);Copy=false;Replace=NoReplace}
                            |]

                        let possibleAddition =
                            match ast.Module.Type with
                            |"module_new" -> [||]
                            |_ -> [|{Text=(sprintf "input bit %s;|bit %s = 1'b0;" name name);Copy=true;Replace=IODeclaration}|]

                        let extraMessages = Array.append extraMessagesMain possibleAddition
                        
                        createErrorMessage linesLocations currLocation message extraMessages name
                    |false ->
                        let message = sprintf "Variable '%s' is not declared as input or variable" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or variable" name);Copy=false;Replace=NoReplace}
                                {Text=(sprintf "%s" closeVariables[0]);Copy=true;Replace=Variable name}
                            |]
                        createErrorMessage linesLocations currLocation message extraMessages name
            )
    let checkNamesOnRHSOfAssignment (expression: ExpressionDU) currentInputWireList localErrors =
        let primariesRHS = primariesUsedInAssignment [] expression
        let paramList = getCurrentParamList
        let paramAndVarList = List.append currentInputWireList paramList
        checkNamesInPrimaries primariesRHS paramAndVarList localErrors
            
    /// Check if the width of each wire/input used
    /// is within the correct range (defined range)
    let checkSizesOnRHSOfAssignment (assignment: Assignment) currentInputWireSizeMap arraySizeMap localErrors =
        checkExpr linesLocations currentInputWireSizeMap arraySizeMap paramMap localErrors assignment.RHS

    /// Helper function to extract all inputs + wires declared 
    /// prior to the assignment being checked
    let getCurrentInputWireSizeMap location = 
        wireSizeMap
        |> Map.filter (fun wire _ ->
            match (Map.tryFind wire wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> Map.toList
        |> List.append (Map.toList portSizeMap)
        |> Map.ofList
    
    let declarationsNames = 
        foldAST getDeclarations [] (VerilogInput(ast)) 
        |> List.collect (fun decl -> List.ofArray decl.Variables)
        |> List.map (fun var -> var.Name)

    let notUniqeWireNames = 
                wireNameList @ declarationsNames 
                |> List.countBy id
                |> List.filter (fun (name,count) -> count>1)
                |> List.map fst
    
    let assignmentsWithLocation =
        foldAST getAssignmentsWithLocations [] (VerilogInput(ast))
    let moduleInstantiationPrimaries =
        foldAST getModuleInstantiationStatements [] (VerilogInput ast)
        |> List.collect (fun modInst -> modInst.Connections |> Array.toList)
        |> List.map (fun conn -> conn.Primary)
    let moduleInstantiationErrors =
        moduleInstantiationPrimaries
        |> List.collect (fun primary ->
            let currentInputWireList = getCurrentInputWireList (getPrimaryLocation primary)
            let currentInputWireSizeMap = getCurrentInputWireSizeMap (getPrimaryLocation primary)
            checkNamesInPrimaries [primary] currentInputWireList []
            |> List.append (checkPrimariesWidths linesLocations currentInputWireSizeMap arraySizeMap paramMap [] [primary] [])
        )
        
    let localErrors =
        assignmentsWithLocation
        |> List.collect (fun (assignment, location)->
            let currentInputWireList = getCurrentInputWireList location
            let currentInputWireSizeMap = getCurrentInputWireSizeMap location

            match assignment.Type with
                | WireAssign -> checkWireNameAndWidth assignment notUniqeWireNames []
                |_ -> checkAssignmentNameAndWidth assignment []
            |> checkNamesOnRHSOfAssignment assignment.RHS currentInputWireList
            |> (fun errlst -> 
                match assignment.LHS.VariableBitSelect with
                | Some expr -> checkNamesOnRHSOfAssignment expr currentInputWireList errlst
                | _ -> errlst)
            |> checkSizesOnRHSOfAssignment assignment currentInputWireSizeMap arraySizeMap
            |> (fun errlst -> 
                match assignment.LHS.VariableBitSelect with
                | Some expr -> checkExpr linesLocations currentInputWireSizeMap arraySizeMap paramMap errlst expr
                | _ -> errlst)
            //|> checkWidthOfAssignment assignment currentInputWireSizeMap location 
        )
        |> List.append moduleInstantiationErrors
        |> List.append moduleInstantiationErrors
    // checking other expressions (conditional, case expression)
    let expressions = foldAST getCondAndCaseExpressions [] (VerilogInput ast)
    let exprErrors = 
        expressions
        |> List.collect (fun (expr, location) -> checkNamesOnRHSOfAssignment expr (getCurrentInputWireList location) [])
    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    let localErrorsDecl =
        declarations
        |> List.collect (fun decl -> checkLogicName decl notUniqeWireNames  [])

    errorList @ localErrors @ localErrorsDecl @ exprErrors


let checkUnsupportedKeywords 
    (ast:VerilogInput) 
    (linesLocations: int list) 
    (errorList: ErrorInfo list) 
        : ErrorInfo list =

    let localErrors =
        ast.Module.ModuleItems.ItemList
        |> Array.toList
        |> List.filter( fun item -> item.Type = "NO-COMB" || item.Type = "NO-CASE" || item.Type = "WIRE-DECL" )
        |> List.map (fun item -> item.Type,item.ItemType, item.Location)
        |> List.collect (fun (tp,keyW,loc) ->
            let message = 
                match tp with
                |"NO-COMB" -> "Non-Combinational logic is not supported"
                |"NO-CASE" -> "Case statement is not supported"
                |"WIRE-DECL" -> "Assign directly a value to the wire \n 'wire {name} = {value};'"
                |_ -> "Non-Combinational logic is not supported"
            let extraMessages = 
                [|{Text=message; Copy=false;Replace=NoReplace}|]
            createErrorMessage linesLocations loc message extraMessages keyW
        
        )

    List.append errorList localErrors

/// Checks if the RHS expression is wider than the LHS of an assignment.
/// Checks every assignment: continuous and combinational
let checkAssignmentWidths
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string,int>)
    (arraySizeMap: Map<string, int * int array>)
    (paramMap: Map<string,int>)
    (errorList: ErrorInfo list) =

    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let assignments = foldAST getAssignmentsWithLocations [] (VerilogInput ast)

    let localErrors = 
        assignments
        |> List.collect (fun (assign, loc) ->
            let rhsW = getWidthOfExpr assign.RHS wireAndPortSizeMap arraySizeMap paramMap
            let lhsW = getLHSWidth assign wireAndPortSizeMap arraySizeMap
            if rhsW > lhsW then
                let message = sprintf "The RHS expression (%A bits wide) doesn't fit in the variable on the LHS (%A bits wide)" rhsW lhsW
                let extraMessages = 
                    [|{Text=message; Copy=false;Replace=NoReplace}|]
                createErrorMessage linesLocations loc message extraMessages (lhsName assign.LHS)
            else []
        )
    errorList @ localErrors

let checkInputsAssigned ast linesLocations portMap errorInfoList =
    let assignments = foldAST getAssignments' [] (VerilogInput ast)
    assignments
    |> List.collect (fun assign ->
        match Map.tryFind (lhsName assign.LHS) portMap with
        | Some InputDecl -> 
            let message = sprintf "Cannot assign to input port '%s'" (lhsName assign.LHS)
            let extraMessages = 
                [|{Text=message; Copy=false;Replace=NoReplace}|]
            createErrorMessage linesLocations (lhsLocation assign.LHS) message extraMessages (lhsName assign.LHS)
        | _ -> []
    )
    |> List.append errorInfoList
/////////////////////////////


let getNotUniquePortDeclarations items =
    items
    |> List.collect (fun x -> 
        match x with
        | ItemDU.IOItem d -> 
            d.Variables
            |> Array.toList
            |> List.collect (fun x -> [x.Name])
        | _ -> []
    )
    |> List.countBy id
    |> List.filter (fun (name, size) -> size>1)
    |> List.map fst

/// Returns the port-size map (e.g. (port "a" => 4 bits wide))
let getPortSizeAndLocationMap (items: ItemDU list) paramMap = 
    let portSizeLocation = 
        items |> List.collect (fun x -> 
            match x with
            | ItemDU.IOItem ioItem -> 
                let size =
                    match isNullOrUndefined ioItem.Range with
                    | true -> 1
                    | false -> (evalExprWithParams (Option.get ioItem.Range).Start paramMap) - (evalExprWithParams (Option.get ioItem.Range).End paramMap) + 1
                let location = ioItem.Location
                ioItem.Variables
                |> Array.toList
                |> List.collect (fun identifier -> [(identifier.Name,size,identifier.Location)])
            | _ -> []
        )
    let ps = List.map (fun x -> match x with | p,s,l -> (p,s)) portSizeLocation
    let pl = List.map (fun x -> match x with | p,s,l -> (p,l)) portSizeLocation
    (Map.ofList ps, Map.ofList pl)


/// Returns the port-width declaration map (e.g. (  port "a" => (4,0)  ))
let getPortWidthDeclarationMap items paramMap = 
    items 
    |> List.collect (fun x -> 
        match x with
        | ItemDU.IOItem d -> 
            let size = 
                match isNullOrUndefined d.Range with
                | true -> (0,0)
                | false -> (evalExprWithParams (Option.get d.Range).Start paramMap),(evalExprWithParams (Option.get d.Range).End paramMap)
            d.Variables 
            |> Array.toList 
            |> List.collect (fun x -> [(x.Name,size)])
        | _ -> []) 
    |> Map.ofList

/// Returns the port-type map (e.g. (port "a" => INPUT))
let getPortMap (items: ItemDU list) = 
    items |> List.collect (fun x -> 
            match x with 
            | ItemDU.IOItem ioItem -> 
                ioItem.Variables
                |> Array.toList
                |> List.collect (fun identifier -> [(identifier.Name,ioItem.DeclarationType)])
            | _ -> []
            // match (x.IODecl |> isNullOrUndefined) with
            // | false -> 
            //     match x.IODecl with
            //     | Some d -> 
            //         d.Variables 
            //         |> Array.toList 
            //         |> List.collect (fun x -> [(x.Name,d.DeclarationType)]) 
            //     | None -> []
            // | true -> []
    ) |> Map.ofList
    

let getInputSizeMap inputNameList portSizeMap =
    portSizeMap
    |> Map.filter (fun n s -> (List.exists (fun x -> x = n) inputNameList))

/// Returns the names of the ports declared as INPUT
let getInputNames portMap = 
    portMap 
    |> Map.filter (fun n s -> s = InputDecl) 
    |> Map.toList 
    |> List.map fst

let getArraySizeMap (items: ItemDU list) paramMap : Map<string, int * int array> = 
    items 
    |> List.collect (fun x -> 
        match x with
        | ItemDU.Decl decl -> 
            decl.Variables
            |> Array.toList
            |> List.collect (fun var -> 
                match decl.DeclarationType with 
                | LogicDecl ->
                    match decl.ArrayRanges with
                    | Some arrayRanges ->
                        let width = 
                            match decl.Range with
                            | Some _ ->
                                let start = evalExprWithParams (decl.Range |> Option.get).Start paramMap
                                let end_ = evalExprWithParams (decl.Range |> Option.get).End paramMap
                                start - end_ + 1
                            | None -> 1
                        let arraySizes = 
                            arrayRanges
                            |> Array.map (fun range ->
                                let rStart = evalExprWithParams range.Start paramMap
                                let rEnd = evalExprWithParams range.End paramMap
                                rStart - rEnd + 1
                            )
                        [(var.Name, (width, arraySizes))]
                    | None -> []
                | _ -> []
            )
        | _ -> [])
    |> Map.ofList


/// Returns the names of the declared WIRES
let getWireSizeMap (items: ItemDU list) paramMap = 
    items 
    |> List.collect (fun x -> 
        match x with
        | ItemDU.ContStatement cont -> 
            match cont.StatementType with
            | Wire -> 
                let lhs = cont.Assignment.LHS 
                match lhs.PrimaryType with
                | Identifier id -> [id.Name,1]
                | IdentifierBit (id, _) -> [id.Name,1]
                | VariableBitSelect (id, idx) -> 
                    let size = evalExprWithParams idx paramMap
                    [id.Name,size]
                | IdentifierBits (id, bStart, bEnd) -> 
                    // let bStart = evalExpr bitsstart
                    // let bEnd = evalExpr bitsend
                    let size = bStart - bEnd + 1
                    [id.Name,size]
                | IdentifierBitsSelect (id, bitsstart, width, select) -> 
                    [id.Name,width]
                | IdentifierArray _ -> []
            | _ -> []
        | _ -> [])
    |> Map.ofList


let getWireNames (items: ItemDU list) =
    items 
    |> List.collect (fun x -> 
        match x with
        | ItemDU.ContStatement cont ->
            match cont.StatementType with
            | Wire -> [getPrimaryName cont.Assignment.LHS.PrimaryType]
            | _ -> []
        | _ -> [])

let getWireLocationMap (items: ItemDU list) = 
    items 
    |> List.collect (fun x -> 
        match x with
        | ItemDU.ContStatement cont ->
            match cont.StatementType with
            | Wire -> 
                let name = getPrimaryName cont.Assignment.LHS.PrimaryType
                let loc = cont.Location
                [name,loc]
            | _ -> []
        | _ -> [])
    |> Map.ofList


let getParamMap 
    (ast:VerilogInput) 
    (linesLocations: int list)
    (items: ItemDU list) : Map<string, int> * ErrorInfo list=
    let paramDecls =
        items
        |> List.filter (function ItemDU.ParamDecl _ -> true | _ -> false)
    let paramDeclsList =
        paramDecls
        |> List.collect (function
            | ItemDU.ParamDecl paramDecl -> paramDecl.Parameters |> Array.toList
            | _ -> [])
    
    let paramMap, errors =
        paramDeclsList
        |> List.fold (fun (paramMap, errors) param ->
            let paramName = param.Identifier.Name
            let paramLoc = param.Identifier.Location
            let duplicateParamErrors = 
                match Map.tryFind paramName paramMap with
                | Some p -> 
                    let message = sprintf "Identifier '%s' is already used by another parameter" paramName
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Identifier '%s' is already used by another parameter\nPlease use a different name for this parameter" paramName);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations paramLoc message extraMessages paramName
                | None -> []
            try 
                let paramValue = evalExprWithParams param.RHS paramMap
                Map.add paramName paramValue paramMap, duplicateParamErrors @ errors
            with _ ->
                let extraMessages=                    
                    [|
                        {Text=sprintf "Parameter '%s' cannot be evaluated to a constant" paramName; Copy=false;Replace=NoReplace};
                    |]
                let message = sprintf "Wrong parameter declaration"
                let err = createErrorMessage linesLocations paramLoc message extraMessages paramName
                paramMap, err @ duplicateParamErrors @ errors
        ) (Map.empty, [])
    paramMap, List.rev errors


/// Helper error-finder function 
/// Called after parameters are evaluated to ensure overriding works
/// Returns a list of errors (type ErrorInfo)
let getSemanticErrorsNoParamOverride (ast: VerilogTypes.VerilogInput) linesLocations paramMap (origin:CodeEditorOpen) (project:Project) =
    let (verilogitems: ItemT list) = ast.Module.ModuleItems.ItemList |> Array.toList
    let (items: ItemDU list) = verilogitems |> List.map convertItem
    ///////// STATIC MAPS, LISTS NEEDED  ////////////////
    let portMap  = getPortMap items
    // let paramMap = getParamMap items
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap items paramMap
    let portWidthDeclarationMap = getPortWidthDeclarationMap items paramMap
    
    let notUniquePortDeclarations = getNotUniquePortDeclarations items
    
    let inputNameList = getInputNames portMap

    let wireSizeMap = getWireSizeMap items paramMap

    let arraySizeMap = getArraySizeMap items paramMap

    let loopErr = checkForLoopUnrollCost ast linesLocations []

    if not (List.isEmpty loopErr) then
        loopErr
    else
        let declarations = foldAST getDeclarations [] (VerilogInput(ast))
        printfn "Declarations: %A" declarations
        let wireSizeMap =
            (wireSizeMap, declarations)
            ||> List.fold (fun map decl ->
                (map, decl.Variables)
                ||> Array.fold (fun map' variable -> 
                    if isNullOrUndefined decl.Range then Map.add variable.Name 1 map'
                    else
                        let bStart = evalExprWithParams (Option.get decl.Range).Start paramMap
                        let bEnd = evalExprWithParams (Option.get decl.Range).End paramMap
                        Map.add variable.Name (bStart - bEnd + 1) map'
                )
            )
        printfn "Wire Size Map: %A" wireSizeMap
        let wireNameList = getWireNames items
        let wireLocationMap = getWireLocationMap items //need to add declarations
        let wireLocationMap = 
            (wireLocationMap, declarations)
            ||> List.fold (fun (wireLocMap: Map<string, int>) (decl: Declaration) -> 
                    (wireLocMap, decl.Variables)
                    ||> Array.fold (fun map var -> Map.add var.Name var.Location map))
        //////////////////////////////////////////////
        
        let errors =
            []  //begin with empty list and add errors to it
            |> checkIOWidthDeclarations ast paramMap linesLocations //correct port width declaration (e.g. [1:4] -> invalid)
            |> checkWiresAndAssignments ast portMap portSizeMap portWidthDeclarationMap inputNameList linesLocations wireNameList wireSizeMap wireLocationMap arraySizeMap paramMap //checks 1-by-1 all assignments (wires & output ports)
            |> checkAllOutputsAssigned ast portMap portSizeMap paramMap linesLocations //checks whether all output ports have been assined a value
            |> checkUnsupportedKeywords ast linesLocations
            |> checkProceduralAssignments ast linesLocations
            // |> checkForLoopVar ast linesLocations
            |> checkVariablesDrivenSimultaneously ast linesLocations arraySizeMap
            |> checkVariablesAlwaysAssigned ast linesLocations portSizeMap wireSizeMap arraySizeMap paramMap
            |> checkArrayStatements ast linesLocations arraySizeMap
            |> checkCasesStatements ast linesLocations portSizeMap wireSizeMap arraySizeMap paramMap
            |> checkExpressions ast linesLocations wireSizeMap arraySizeMap paramMap
            |> checkClk ast linesLocations portMap
            |> checkClkNames ast linesLocations portMap portLocationMap portSizeMap
            |> cycleCheck ast linesLocations portSizeMap wireSizeMap
            |> checkVariablesUsed ast linesLocations portSizeMap wireSizeMap paramMap arraySizeMap
            |> checkAlwaysCombRHS ast linesLocations portSizeMap wireSizeMap
            |> checkAssignmentWidths ast linesLocations portSizeMap wireSizeMap arraySizeMap paramMap
            |> checkModuleInstantiations ast linesLocations portSizeMap wireSizeMap project portMap
            |> checkInputsAssigned ast linesLocations portMap
            |> List.distinct // filter out possible double Errors
        errors



let getExtraParamErrors (ast: VerilogTypes.VerilogInput) linesLocations (origin:CodeEditorOpen) (project:Project) modInst comp =
    // find any module instantiations with parameter overrides
    match modInst.Parameters with
    | Some parameters -> 
        let givenParams = parameters |> Array.map (fun param -> param.Identifier.Name) |> Set.ofArray
        let overrideMap =
            (Map.empty, parameters)
            ||> Array.fold (fun map param ->
                Map.add param.Identifier.Name (evalExpr param.Value) map
            )
        // printf "params for component %A: %A" comp.Name parameters
        // // if param in parammap doesnt exist in override map, then add from param map
        // let overrideMap = 
        //     (overrideMap, givenParamMap)
        //     ||> Map.fold (fun map key value ->
        //         if Map.containsKey key overrideMap then map
        //         else Map.add key value map
        //     )
        // printf "Override map: %A" overrideMap
        match comp.Form with
        | Some (Verilog name) ->
            let folderPath = project.ProjectPath
            let path = pathJoin [| folderPath; name + ".v" |]
            let code = 
                match tryReadFileSync path with
                |Ok text -> text
                |Error _ -> sprintf "Error: file {%s.v} has been deleted from the project directory" name
            let parsedCodeNearley = parseFromFile code
            let output = Json.parseAs<ParserOutput> parsedCodeNearley
            let result = Option.get output.Result
            let fixedAST = fix result
            let parsedAST = fixedAST |> Json.parseAs<VerilogTypes.VerilogInput>

            let expectedParams = // type set
                foldAST getParamDeclarations [] (VerilogInput parsedAST)
                |> List.map (fun (param : VerilogAST.Parameter) -> param.Identifier.Name)
                |> Set.ofList
            
            let expectedParamMap = // type map
                foldAST getParamDeclarations [] (VerilogInput parsedAST)
                |> List.map (fun (param : VerilogAST.Parameter) -> (param.Identifier.Name, evalExpr param.RHS))
                |> Map.ofList
            let overrideMap = 
                (overrideMap, expectedParamMap)
                ||> Map.fold (fun map key value ->
                    if Map.containsKey key overrideMap then map
                    else Map.add key value map
                )

            let extraParams = Set.difference givenParams expectedParams
            let extraParamErrors =
                parameters
                |> Array.toList
                |> List.collect (fun param ->
                    if Set.contains (param.Identifier.Name) extraParams then 
                        let extraMessages=                    
                            [|
                                {Text=sprintf "The parameter %A does not exist for component %A" param.Identifier.Name comp.Name; Copy=false;Replace=NoReplace};
                            |]
                        let message = sprintf "No such parameter for the given component"
                        createErrorMessage linesLocations param.Identifier.Location message extraMessages param.Identifier.Name
                    else []
                )

            let overrideParamErrorList = getSemanticErrorsNoParamOverride parsedAST linesLocations overrideMap origin project

            extraParamErrors @ overrideParamErrorList
        | _ -> failwithf "TODO ADD MORE THAN JUST VERILOG"
    | None -> [] // no parameter overrides, so no extra errors


let getSemanticErrors (ast: VerilogTypes.VerilogInput) linesLocations (origin:CodeEditorOpen) (project:Project) =
    let (verilogitems: ItemT list) = ast.Module.ModuleItems.ItemList |> Array.toList
    let (items: ItemDU list) = verilogitems |> List.map convertItem

    let paramMap, paramDeclErrors = getParamMap ast linesLocations items

    if (paramDeclErrors.IsEmpty = false) then
        paramDeclErrors
    else

    let portMap  = getPortMap items
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap items paramMap
    let portWidthDeclarationMap = getPortWidthDeclarationMap items paramMap
    let notUniquePortDeclarations = getNotUniquePortDeclarations items
    let inputNameList = getInputNames portMap

    let forLoopErrorsBeforeUnroll = 
        []
        |> checkForLoopUnrollCost ast linesLocations

    match forLoopErrorsBeforeUnroll.IsEmpty with
    | true ->
        let moduleInstantiations = foldAST getModuleInstantiationStatements [] (VerilogInput ast)
        let paramErrors =
            moduleInstantiations
            |> List.collect (fun (modInst : VerilogAST.ModuleInstantiation) -> 
                match  List.filter (fun comp -> comp.Name = modInst.Module.Name)project.LoadedComponents with
                | [] -> []
                | [comp] -> 
                    let extraParamErrors = getExtraParamErrors ast linesLocations origin project modInst comp
                    match extraParamErrors.IsEmpty with
                    | false -> 
                        let extraMessages = 
                            [|
                                {Text=sprintf "The overriden parameters instantiated with component %A are not compatible. Please see errors below and fix accordingly." modInst.Module.Name; Copy=false;Replace=NoReplace};
                            |]
                        let message = sprintf "Parameter override error for component"
                        createErrorMessage linesLocations modInst.Identifier.Location message extraMessages modInst.Module.Name
                        @ extraParamErrors
                    | true -> []
                | _ -> failwithf "There are multiple custom components with this name!"
            )

        let forLoopErrorsBeforeUnroll =
            if moduleInstantiations.IsEmpty then 
                []
            else forLoopErrorsBeforeUnroll

        let allOtherErrors =
            forLoopErrorsBeforeUnroll
            |> nameCheck ast linesLocations origin project //name is valid (not used by another sheet/component)
            |> portCheck ast linesLocations //all ports are declared as input/output
            |> checkForLoopUnrollCost ast linesLocations
            |> checkIODeclarations ast portWidthDeclarationMap portLocationMap linesLocations notUniquePortDeclarations portMap project //all ports declared as IO are defined in the module header    
            |> checkParamsUsed ast linesLocations paramMap

        let allOtherErrors = allOtherErrors @ getSemanticErrorsNoParamOverride ast linesLocations paramMap origin project

        paramErrors @ allOtherErrors
    
    | false ->
        forLoopErrorsBeforeUnroll


    // let overridenModules = getOverridenModules project origin
    // the param overriden checker should get the loaded comp ver, call the error checker above
    // get all overriden modules
    // pass it through 

    // should call get semantic errors, then get extra param errors sep