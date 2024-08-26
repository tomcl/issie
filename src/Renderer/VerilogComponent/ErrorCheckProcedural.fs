module ErrorCheckProcedural

open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes
open VerilogAST
open ErrorCheckHelpers
open NumberHelpers
open Helpers

let rec private convert (lst:List<string>) acc =
    match lst with
    | [] -> acc
    | hd::[] -> convert [] (acc + hd)
    | hd::tl -> convert tl (acc + hd + ", ")

/// Checks if always_comb only contains blocking assignments and always_ff only contains nonblocking assignments
let checkProceduralAssignments
    (ast:VerilogInput) 
    (linesLocations: int list)
    (errorList: ErrorInfo list)
        : ErrorInfo list =
    let alwaysBlocks = foldAST getAlwaysBlocks [] (VerilogInput(ast))
    let clockedAlwaysBlocks = alwaysBlocks |> List.filter (fun always -> always.AlwaysType="always_ff")
    let combAlwaysBlocks = alwaysBlocks |> List.filter (fun always -> always.AlwaysType="always_comb")
    

    let checkClockedAlwaysBlock alwaysBlock =
        let blockingAssigns = foldAST getBlockingAssignmentsWithLocation [] (AlwaysConstruct(alwaysBlock))
        let localErrors =
            ([], blockingAssigns) 
            ||> List.fold 
                (fun errors blocking -> 
                    let message = sprintf "Blocking assignment in always_ff block"
                    let extraMessages =
                        [|
                            {Text=sprintf "Blocking assignment in a clocked always block is not supported.\nPlease use nonblocking assignments in clocked always blocks";Copy=false;Replace=NoReplace}
                            {Text=(sprintf "<=" );Copy=true;Replace=Variable "="}
                        |]
                    let currError = createErrorMessage linesLocations (snd blocking) message extraMessages (fst blocking).Assignment.Type
                    errors @ currError
                )
        localErrors

        
    let checkCombAlwaysBlock alwaysBlock =
        let nonBlockingAssigns = foldAST getNonBlockingAssignmentsWithLocation [] (AlwaysConstruct(alwaysBlock))
        let localErrors =
            ([], nonBlockingAssigns) 
            ||> List.fold 
                (fun errors nonblocking -> 
                    let message = sprintf "Nonblocking assignment in always_comb block"
                    let extraMessages =
                        [|
                            {Text=sprintf "Nonblocking assignment in a combinational always block is not supported.\nPlease use blocking assignments in combinational always blocks";Copy=false;Replace=NoReplace}
                            {Text=(sprintf "=" );Copy=true;Replace=Variable "<="}
                        |]
                    let currError = createErrorMessage linesLocations (snd nonblocking) message extraMessages (fst nonblocking).Assignment.Type
                    errors @ currError
                )
        localErrors

    let clockedErrors = List.collect checkClockedAlwaysBlock clockedAlwaysBlocks
    let combErrors = List.collect checkCombAlwaysBlock combAlwaysBlocks
    errorList @ clockedErrors @ combErrors

/// Checks if a variable is driven by multiple always blocks or continuous assignments
/// Could be improved if it printed out the variables / marked the error at the location of the variables
let checkVariablesDrivenSimultaneously     
    (ast:VerilogInput) 
    (linesLocations: int list)
    (errorList: ErrorInfo list)
        : ErrorInfo list = 
    let alwaysBlocks = foldAST getAlwaysBlocks [] (VerilogInput(ast)) 
    let continuousAssignBits = 
        foldAST getContAssignments [] (VerilogInput ast)
        |> List.map (fun assign -> Set.singleton assign.LHS.Primary.Name)

    let getAlwaysAssignmentBits (alwaysBlock: AlwaysConstructT) = 
        let assignments = foldAST getAssignments' [] (AlwaysConstruct(alwaysBlock))
        List.map (fun assign -> assign.LHS.Primary.Name ) assignments
        |> List.distinct
        |> Set.ofList

    let alwaysAssignmentBits = 
        alwaysBlocks |> List.map getAlwaysAssignmentBits
        |> List.append continuousAssignBits

    let getDuplicates ((index1, assignmentBits1), (index2, assignmentBits2)) =
        if index1 = index2 then []
        else Set.intersect assignmentBits1 assignmentBits2 |> Set.toList

    let indexedAssignmentBits =
        alwaysAssignmentBits
        |> List.indexed
    let duplicates = 
        List.allPairs indexedAssignmentBits indexedAssignmentBits
        |> List.collect getDuplicates
        |> List.distinct
    if List.isEmpty duplicates then errorList
    else
    let currLocation = ast.Module.EndLocation
    let extraMessages=                    
        [|
            {Text=sprintf "The following variables are driven by multiple always blocks or continuous assignments: %A.  Please make sure that every port is driven by at most one always block or continuous assignment." (convert duplicates "");Copy=false;Replace=NoReplace};
        |]
    let message = "Some ports or variables are driven by multiple always blocks or continuous assignments."
    errorList @ createErrorMessage linesLocations currLocation message extraMessages "endmodule"
        

/// Checks the case items of the case statements:
/// - Repeated cases
/// - Wrong width
let checkCasesStatements     
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>) 
    (wireSizeMap: Map<string,int>) 
    (errorList: ErrorInfo list)
        : ErrorInfo list =

    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    
    let wireSizeMap =
        (wireSizeMap, declarations)
        ||> List.fold (fun map decl ->
            (map, decl.Variables)
            ||> Array.fold (fun map' variable -> 
                if isNullOrUndefined decl.Range then Map.add variable.Name 0 map'
                else Map.add variable.Name ((Option.get(decl.Range).Start |> int)-(Option.get(decl.Range).End |> int)+1) map'
            )
        )
    let portSizeMap = Map.fold (fun acc key value -> Map.add key value acc) portSizeMap wireSizeMap

    let caseStatementsWithLoc = foldAST getCaseStatementsWithLoc [] (VerilogInput ast)
    // check for repeated cases
    // need to maybe check for missing cases
    let checkCaseStatement (caseStmt, location) =
        let condWidth = getWidthOfExpr caseStmt.Expression portSizeMap
        if condWidth = 0 then []
        else
        let caseNumbers = caseStmt.CaseItems |> Array.collect (fun caseItem -> caseItem.Expressions)
        let _, repeatedErrors =
            ((Map.empty, []), caseNumbers)
            ||> Array.fold (fun (casesCovered, errors) (caseNumber: NumberT) ->
                let numBase, width, num = Option.get caseNumber.Base, Option.get caseNumber.Bits, Option.get caseNumber.AllNumber
                let caseNumberDec = toDecimal (Option.get caseNumber.AllNumber) (Option.get caseNumber.Base) (Option.get caseNumber.Bits)
                let repeated = Map.containsKey caseNumberDec casesCovered
                let repeatedError =
                    if repeated then 
                        let firstLine = getLineNumber linesLocations (Map.find caseNumberDec casesCovered)
                        let extraMessages=                    
                            [|
                                {Text=sprintf "The following case value is duplicated: %A, see line %A. Please make sure there are no repeated case values." (width+numBase+num) firstLine;
                                Copy=false;
                                Replace=NoReplace};
                            |]
                        let message = "Duplicate case value"
                        createErrorMessage linesLocations caseNumber.Location message extraMessages (width+numBase+num)
                    else 
                        []
                let widthError =
                    if width|>int <> condWidth then 
                        let extraMessages=                    
                            [|
                                {Text=sprintf "Width of case expression (%A bits wide) does not match width of this case item (%A bits wide)." condWidth width;Copy=false;Replace=NoReplace};
                                {Text=(string condWidth)+numBase+num; Copy=true; Replace=Variable (width+numBase+num)}
                            |]
                        let message = "Width of case value does not match \n the given case expression"
                        createErrorMessage linesLocations caseNumber.Location message extraMessages (width+numBase+num)
                    else
                        []

                Map.add caseNumberDec caseNumber.Location casesCovered, errors @ repeatedError @ widthError
                )
        match caseStmt.Default with
        | Some _ -> repeatedErrors // no missing cases
        | None -> 
            repeatedErrors

    let localErrors = 
        caseStatementsWithLoc
        |> List.collect checkCaseStatement

    errorList @ localErrors

/// Used for checking if a variable is assigned in every branch, see checkVariablesAlwaysAssigned
let allCasesCovered 
    caseStmt
    (portSizeMap: Map<string,int>) 
    (wireSizeMap: Map<string,int>)  =

    match caseStmt.Default with
    | Some _ -> true
    | _ ->
        let portSizeMap = Map.fold (fun acc key value -> Map.add key value acc) portSizeMap wireSizeMap
        let condWidth = getWidthOfExpr caseStmt.Expression portSizeMap
        if condWidth = 0 then true
        else
        let expNrOfCases = (1I <<< condWidth)
        let caseNumbers = caseStmt.CaseItems |> Array.collect (fun caseItem -> caseItem.Expressions)
        let caseNumsUnique = 
            (Set.empty, caseNumbers) ||> Array.fold (fun casesCovered caseNumber ->
                let numBase, width, num = Option.get caseNumber.Base, Option.get caseNumber.Bits, Option.get caseNumber.AllNumber
                let caseNumberDec = toDecimal num numBase width
                Set.add caseNumberDec casesCovered
            )
        let expCaseVals = [0I..expNrOfCases-1I] |> Set.ofList
        let missingVars = Set.difference expCaseVals (caseNumsUnique)
        Set.count missingVars = 0


let checkVariablesAlwaysAssigned
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>) 
    (wireSizeMap: Map<string,int>) 
    (errorList: ErrorInfo list)
        : ErrorInfo list =

    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    
    let wireSizeMap =
        (wireSizeMap, declarations)
        ||> List.fold (fun map decl ->
            (map, decl.Variables)
            ||> Array.fold (fun map' variable -> 
                if isNullOrUndefined decl.Range then Map.add variable.Name 1 map'
                else Map.add variable.Name ((Option.get(decl.Range).Start |> int)-(Option.get(decl.Range).End |> int)+1) map'
            )
        )
    let portSizeMap = Map.fold (fun acc key value -> Map.add key value acc) portSizeMap wireSizeMap

    let rec getVariablesAlwaysAssigned (node: ASTNode) =
        match node with 
        | VerilogInput verilogInput -> 
            let itemsCompleteVariables = 
                verilogInput.Module.ModuleItems.ItemList
                |> Array.map (fun item -> getVariablesAlwaysAssigned (Item item))
            (Set.empty, itemsCompleteVariables)
            ||> Array.fold Set.union
        | Conditional ifstmt ->
            if Option.isNone ifstmt.ElseStatement then
                Set.empty
            else
            let ifVariables = 
                getVariablesAlwaysAssigned (Statement ifstmt.IfStatement.Statement)
    
            let elseVariables =
                getVariablesAlwaysAssigned (Statement (Option.get ifstmt.ElseStatement))
            Set.intersect ifVariables elseVariables
        | BlockingAssign blocking -> (getLHSBitsAssignedCertainly portSizeMap blocking.Assignment) |> Set.ofList // fix getLHSBits
        | NonBlockingAssign nonBlocking -> (getLHSBitsAssignedCertainly portSizeMap nonBlocking.Assignment) |> Set.ofList // fix getLHSBits
        | Item item -> getVariablesAlwaysAssigned (getItem item)
        | AlwaysConstruct always -> 
            getVariablesAlwaysAssigned (Statement always.Statement)
        | Statement statement ->
            statement
            |> getAlwaysStatement
            |> statementToNode
            |> getVariablesAlwaysAssigned
        | SeqBlock seqBlock ->
            let completeVariables =
                seqBlock.Statements
                |> Array.map (fun s -> getVariablesAlwaysAssigned (Statement s))
            (Set.empty, completeVariables)
            ||> Array.fold Set.union
        | Case case ->
                if allCasesCovered case portSizeMap wireSizeMap then
                    let complCaseVars =
                        case.CaseItems
                        |> Array.map (fun caseItem -> getVariablesAlwaysAssigned (Statement caseItem.Statement))
                    let caseItemVars =
                        (complCaseVars[0], complCaseVars) //there is be at least one assignment in a case item, so at least one var
                        ||> Array.fold Set.intersect
                    match case.Default with
                        | Some dflt -> 
                            Set.intersect (getVariablesAlwaysAssigned (Statement dflt)) caseItemVars
                        | None -> caseItemVars
                else Set.empty            
        | _ -> Set.empty

    let alwaysCombBlocksWithLoc = 
        foldAST getAlwaysBlocksWithLocations [] (VerilogInput(ast))
        |> List.filter (fun (alwaysBlock, _) -> alwaysBlock.AlwaysType="always_comb")
    
    let variablesNotAssigned = 
        alwaysCombBlocksWithLoc
        |> List.map (fun (always, loc) -> 
            let allLHSVariables = 
                foldAST getAssignments' [] (AlwaysConstruct always)
                |> List.collect (getLHSBits' portSizeMap)
                |> Set.ofList
            let variablesNotAssignedInAllBranches =
                getVariablesAlwaysAssigned (AlwaysConstruct always)
            let undefVars =
                Set.difference allLHSVariables variablesNotAssignedInAllBranches
                |> Set.map (fun varBit -> (varBit.Split [|'['|])[0])
                |> Set.toList
            undefVars, loc
            )
        |> List.filter (fun (undefVars, _) -> undefVars <> [])
    
    (errorList, variablesNotAssigned)
    ||> List.fold (fun errors variables->
        let currLocation = snd variables
        let extraMessages=                    
            [|
                {Text=sprintf "The following variables might be undefined as they are not assigned to in every branch of conditional statements or case statements in the always_comb block: " + (convert (fst variables) "");
                Copy=false;Replace=NoReplace};
            |]
        let message = "Some ports or variables might not always be assigned to"
        errors @ createErrorMessage linesLocations currLocation message extraMessages "always_comb"

    )
/// Check if expressions are correct
/// - Indexes within range
/// - Numbers are correctly formatted
let checkExpressions 
    (ast:VerilogInput) 
    (linesLocations: int list)
    (wireSizeMap: Map<string,int>) 
    (errorList: ErrorInfo list) =

    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    
    let wireSizeMap =
        (wireSizeMap, declarations)
        ||> List.fold (fun map decl ->
            (map, decl.Variables)
            ||> Array.fold (fun map' variable -> 
                if isNullOrUndefined decl.Range then Map.add variable.Name 0 map'
                else Map.add variable.Name ((Option.get(decl.Range).Start |> int)-(Option.get(decl.Range).End |> int)+1) map'
            )
        )
    let expressions = foldAST getAllExpressions' [] (VerilogInput ast)
    let caseItemNums = foldAST getCaseItemNums [] (VerilogInput ast)
    let localErrors = List.collect (checkExpr linesLocations wireSizeMap []) expressions
    let caseItemErrors = List.collect (checkNumber linesLocations) caseItemNums
    errorList @ localErrors @ caseItemErrors

/// Checks that clk is an input port when there are always_ff blocks
let checkClk 
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portMap: Map<string,string>)
    (errorList: ErrorInfo list) =

    let alwaysFFsWithLoc = 
        foldAST getAlwaysBlocks [] (VerilogInput ast)
        |> List.filter (fun always -> always.AlwaysType = "always_ff")
    match Map.tryFind "clk" portMap with
    | None | Some "output" ->
        (errorList, alwaysFFsWithLoc)
        ||>List.fold (fun errors always->
            let extraMessages=                    
                [|
                    {Text=sprintf "To use always_ff blocks, please make sure to include 'clk' in the port list." ;Copy=false;Replace=NoReplace};
                |]
            let message = "Variable 'clk' is not defined as an input port."
            let extraMessages' =
                match ast.Module.Type with
                |"module_new" -> [||]
                |_ -> [|{Text="input bit clk;";Copy=true;Replace=IODeclaration}|]
                |> Array.append extraMessages
            errors @ createErrorMessage linesLocations always.ClkLoc message extraMessages' "clk"
            )
    | _ -> errorList

/// Checks if:
/// - no output ports are called clk
/// - clk has width 1
/// - any of the expressions use clk
let checkClkNames 
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portMap: Map<string,string>)
    (portLocationMap: Map<string, int>)
    (portSizeMap: Map<string, int>)
    (errorList: ErrorInfo list) =

    let declarations = 
        foldAST getDeclarations [] (VerilogInput ast)
        |> List.toArray
        |> Array.collect (fun decl -> decl.Variables)
    let contAssigns = 
        foldAST getContAssignments [] (VerilogInput ast)
        |> List.toArray
        |> Array.map (fun assign -> assign.LHS.Primary)


    let portErrors =
        match Map.tryFind "clk" portMap, Map.tryFind "clk" portLocationMap with
        | Some "output", Some location ->
            let extraMessages=                    
                [|
                    {Text=sprintf "'clk' represents the clock signal, which must be an input port" ;Copy=false;Replace=NoReplace};
                |]
            let message = "'clk' must be an input port"
            createErrorMessage linesLocations location message extraMessages "clk"
        | _ -> [] // clk can be input port, duplicates/redundant ports handled elsewhere
    let logicErrors =
        match (Array.append declarations  contAssigns) |> Array.tryFind (fun id -> id.Name = "clk")  with
        | Some primary ->
            let extraMessages=                    
                [|
                    {Text=sprintf "'clk' is reserved for the clock signal, please rename the variable" ;Copy=false;Replace=NoReplace};
                |]
            let message = "Variable cannot be called 'clk'"
            createErrorMessage linesLocations primary.Location message extraMessages "clk"
        | _ -> []

    let clkWidthErrors =
        match Map.tryFind "clk" portSizeMap, Map.tryFind "clk" portLocationMap with
        | Some l, Some location when l>1->
            let extraMessages=                    
                [|
                    {Text=sprintf "'clk' represents the clock signal, which must have width 1, here it has width %A" l;Copy=false;Replace=NoReplace};
                |]
            let message = "'clk' must have width 1"
            createErrorMessage linesLocations location message extraMessages "clk"
        | _ -> []

    // finally check if any expression uses clk
    let clkPrimaries = 
        foldAST getAllExpressions' [] (VerilogInput ast)
        |> List.fold primariesUsedInAssignment []
        |> List.filter (fun prim -> prim.Primary.Name = "clk")
    let expressionErrors =
        ([], clkPrimaries)
        ||> List.fold (fun errors primary ->
            let extraMessages=                    
                [|
                    {Text=sprintf "'clk' represents the clock signal, make sure to only use it as 'always_ff @(posedge clk)'";Copy=false;Replace=NoReplace};
                |]
            let message = "Illegal use of 'clk'"
            let localErrors = createErrorMessage linesLocations primary.Primary.Location message extraMessages primary.Primary.Name
            errors @ localErrors
            )
    errorList @ portErrors @ logicErrors @ clkWidthErrors @ expressionErrors



type Graph = Map<string, string list>
/// Looks for cycles in the general dependency graph using depth first search
let findCycleDFS (graph: Graph) : string list option =
    let rec dfs (node: string) (visited: Set<string>) (recStack: Set<string>) (path: string list) : string list option =
        if recStack.Contains(node) then
            // Cycle detected, return the path as Some
            Some (node :: path)
        elif visited.Contains(node) then
            // Node has already been visited, no cycle found
            None
        else
            // Add the node to the visited and recursion stack sets
            let visited' = visited.Add(node)
            let recStack' = recStack.Add(node)
            // Get the neighbors of the current node
            let neighbors = 
                graph
                |> Map.tryFind node
                |> Option.defaultValue []
            // Recursively visit the neighbors. If a cycle is found, return it as Some, otherwise None
            let findCycle = 
                (None, neighbors)
                ||> List.fold (fun acc neighbor ->
                    match dfs neighbor visited' recStack' (node :: path) with
                    | Some cyclePath -> Some cyclePath
                    | None -> acc) 
            findCycle

    // Iterate through all the nodes in the graph and check for cycles
    graph |> Map.keys
          |> Seq.tryPick (fun node -> dfs node Set.empty Set.empty [])

/// Returns the dependency graph of the AST.
/// In an assignment, RHS variables are dependencies of the LHS variable.
/// Condition expressions are also dependencies of the variables assigned to in the block
let private getDependencies ast variableSizeMap =
    let rec getDependencyFold graph astNode cond =
        match astNode with
        | VerilogInput verilogInput -> getDependencyFold graph (ModuleItems verilogInput.Module.ModuleItems) cond
        | ModuleItems items ->
            (graph, items.ItemList)
            ||> Array.fold (fun acc item -> 
                getDependencyFold acc (Item item) cond)
        | Item item -> getDependencyFold graph (getItem item) cond
        | ContinuousAssign contAssign -> getDependencyFold graph (Assignment contAssign.Assignment) cond
        | Assignment assign -> 
            let lhsBits = getLHSBits' variableSizeMap assign
            let rhsBits = 
                match assign.LHS.VariableBitSelect with
                | Some expr -> Set.union (getRHSBits variableSizeMap assign.RHS)  (getRHSBits variableSizeMap expr)
                | _ -> getRHSBits variableSizeMap assign.RHS
            (graph, lhsBits)
            ||> List.fold (fun graph' lhs -> 
                graph' |> Map.add lhs (Set.union rhsBits cond)
            )
        | AlwaysConstruct always -> // we ignore always_ff in the dependencies
            if always.AlwaysType = "always_comb" then
                getDependencyFold graph (Statement always.Statement) cond
            else graph
        | Statement statement ->
            let statement' = getAlwaysStatement statement |> statementToNode
            getDependencyFold graph statement' cond
        | NonBlockingAssign nonblocking ->
            getDependencyFold graph (Assignment nonblocking.Assignment) cond
        | BlockingAssign blocking ->
            getDependencyFold graph (Assignment blocking.Assignment) cond
        | SeqBlock seq ->
            (graph, seq.Statements)
            ||> Array.fold (fun acc statement -> 
                getDependencyFold acc (Statement statement) cond )
        | Case case ->
            // need to add case.Expression to dependencies
            let condDependencies = getRHSBits variableSizeMap case.Expression
            let caseItemDeps = Array.map (fun item -> getDependencyFold graph (CaseItem item) (Set.union condDependencies cond)) case.CaseItems |> List.ofArray
            let defaultDeps = 
                match case.Default with
                | Some stmt -> getDependencyFold graph (Statement stmt) (Set.union cond condDependencies)
                | _ -> Map.empty
            // need to combine them
            let allDeps: List<Map<string,Set<string>>> = caseItemDeps @ [defaultDeps]
            (Map.empty, allDeps)
            ||> List.fold (fun acc branch ->
                Map.fold (fun acc' key value -> 
                    match Map.tryFind key acc' with
                    | Some dep -> Map.add key (Set.union dep value) acc'
                    | None -> Map.add key value acc' ) acc branch )

        | CaseItem item -> 
            getDependencyFold graph (Statement item.Statement) cond
        | Conditional conditional ->
            let condDep = getRHSBits variableSizeMap conditional.IfStatement.Condition
            let ifDep = getDependencyFold  graph (Statement conditional.IfStatement.Statement) (Set.union cond condDep)
            let elseDep = 
                match conditional.ElseStatement with
                | Some stmt -> getDependencyFold graph (Statement stmt) (Set.union cond condDep)
                | _ -> Map.empty
            Map.fold (fun acc' key value -> 
                match Map.tryFind key acc' with
                | Some dep -> Map.add key (Set.union dep value) acc'
                | None -> Map.add key value acc' ) ifDep elseDep
        | _ ->  graph
    getDependencyFold Map.empty ast Set.empty
    |> Map.map (fun _ value -> value |> Set.toList )

/// Check for dependency cycles in always_comb blocks and continuous assignments
let cycleCheck 
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string, int>)
    (errorList: ErrorInfo list) =
    // set up dependencies for individual bits
    
    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    // dependency graph is a map: variablename[bit] -> List(dependencies), lhs -> rhs
    let dependencyGraph = getDependencies (VerilogInput ast) wireAndPortSizeMap
    let cycle = findCycleDFS dependencyGraph

    match cycle with
    | Some path ->
        let location = ast.Module.EndLocation
        let extraMessages=                    
            [|
                {Text=sprintf "The following variables form a cycle: %A" path; Copy=false;Replace=NoReplace};
            |]
        let message = sprintf "The following variables form a dependency cycle: %A" path
        errorList @ createErrorMessage linesLocations location message extraMessages "endmodule"
    | _ -> errorList 
    
/// Look for unassigned variables (not outputs, just variables)
let checkVariablesUsed
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string, int>)
    (errorList: ErrorInfo list) =

    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let moduleInstantiationPorts = 
        foldAST getModuleInstantiationStatements [] (VerilogInput ast)
        |> List.collect (fun modInst -> modInst.Connections |> Array.toList)
        |> List.collect (fun conn -> getPrimaryBits wireAndPortSizeMap conn.Primary)
        |> Set.ofList
    let assignmentsLHS = 
        foldAST getAssignments' [] (VerilogInput ast)
        |> List.collect (fun assign -> getLHSBits' wireAndPortSizeMap assign)
        |> Set.ofList
        |> Set.union moduleInstantiationPorts

    let variables = 
        foldAST getDeclarations [] (VerilogInput ast)
        |> List.collect (fun decl -> 
            match decl.Range with
            | None -> 
                let res = decl.Variables |> Array.map (fun var -> var.Name + "[0]") |> Array.toList
                res
            | Some range -> 
                let bits = [|int range.End .. int range.Start|]
                let res =
                    decl.Variables
                    |> Array.collect (fun var -> 
                        let varBits = bits |> Array.map (fun bit -> var.Name+"["+(string bit)+"]")
                        varBits)
                    |> Array.toList
                res
            )
    let checkVariable errorBits (variable: string) =
        match Set.contains variable assignmentsLHS with
        | true -> errorBits
        | false -> errorBits @ [variable]
    let varsNotAssigned =
        variables
        |> List.fold checkVariable []
    match varsNotAssigned with
    | [] -> errorList
    | _ ->        
        let location = ast.Module.EndLocation
        let extraMessages=                    
            [|
                {Text=sprintf "The following variables have not been assigned %A" varsNotAssigned; Copy=false;Replace=NoReplace};
            |]
        let message = sprintf "The following variables have not been assigned %A" varsNotAssigned
        errorList @ createErrorMessage linesLocations location message extraMessages "endmodule"

/// Helper function for checking if any variable or port being written to after it is read in always_comb blocks.
let rec getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, errors) node =
    match node with
    | Assignment assign -> 
        let lhsBits = getLHSBits' wireAndPortSizeMap assign |> Set.ofList
        let rhsBits = getRHSBits wireAndPortSizeMap assign.RHS
        let assignedAfterRHS =  Set.intersect lhsBits rhsVars
        let rhsVars' = Set.union rhsVars rhsBits
        if Set.count assignedAfterRHS = 0 then 
            (rhsVars', errors)
        else
            // variable read after assigned
            let location = assign.LHS.Primary.Location
            let variables = 
                assignedAfterRHS
                |> Set.map (fun var -> (var.Split [|'['|])[0])
                |> Set.toList
            let extraMessages=                    
                [|
                    {Text=sprintf "The following variables are read and then updated: %A This creates undefined behaviour in an always_comb block, please make sure to not update a variable after it is read." (convert variables ""); Copy=false;Replace=NoReplace};
                |]
            let message = sprintf "Variable written to after it is read"
            let errors' = errors @ createErrorMessage linesLocations location message extraMessages assign.LHS.Primary.Name
            (rhsVars', errors')
    | Conditional cond ->  
        let ifVars, ifErrors = 
            getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, List.empty) (IfStatement cond.IfStatement)
        let elseVars, elseErrors =
            match cond.ElseStatement with
            | Some stmt -> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, List.empty) (Statement stmt)
            | _ -> Set.empty, []
        (Set.union ifVars elseVars), errors @ ifErrors @ elseErrors
    | IfStatement ifstmt ->
        let rhsVars' = 
            getRHSBits wireAndPortSizeMap ifstmt.Condition
            |> Set.union rhsVars
        getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars', errors) (Statement ifstmt.Statement)
    | Case case ->
        let caseVars, caseErrors=
            case.CaseItems
            |> Array.map (fun (item: CaseItemT) -> 
                getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, List.empty) (Statement item.Statement))
            |> Array.toList
            |> List.unzip
        let defaultVars, defaultErrors=
            match case.Default with
            | Some stmt -> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, List.empty) (Statement stmt)
            | _ -> Set.empty, []
        let rhsVars' = List.fold Set.union Set.empty (caseVars @ [defaultVars])
        let errors' = List.fold List.append [] caseErrors @ defaultErrors
        rhsVars', errors'
    | SeqBlock seq -> 
        seq.Statements
        |> Array.map (fun stmt -> Statement stmt)
        |> Array.fold (getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations) (rhsVars, errors)
    | Statement stmt ->
        getAlwaysStatement stmt
        |> statementToNode
        |> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, errors)
    | BlockingAssign blocking -> 
        Assignment blocking.Assignment
        |> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, errors)
    | NonBlockingAssign nonblocking ->
        Assignment nonblocking.Assignment
        |> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, errors)
    | AlwaysConstruct always -> 
        Statement always.Statement
        |> getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (rhsVars, errors)
    | _ -> rhsVars, errors


/// check that if an always block writes to and reads from the same variable, variable is assigned first and written later
/// a=1; b=a; a=0; -> this shouldn't be allowed
let checkAlwaysCombRHS
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string, int>)
    (errorList: ErrorInfo list) =

    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let alwaysCombs = 
        foldAST getAlwaysBlocks [] (VerilogInput ast)
        |> List.filter (fun always -> always.AlwaysType="always_comb")
    
    let checkAlwaysComb (always: AlwaysConstructT) =
        getVariablesWrittenAfterRead wireAndPortSizeMap linesLocations (Set.empty, []) (AlwaysConstruct always)
        |> snd
    
    let localErrors =
        alwaysCombs
        |> List.collect checkAlwaysComb
    errorList @ localErrors

let getPrimaryWidth portSizeMap (primary: PrimaryT) =
    match primary.BitsStart, primary.BitsEnd with
    | Some s, Some e -> (int s)-(int e)+1;
    | _ ->
        match Map.tryFind primary.Primary.Name portSizeMap with
        | Some w -> w
        | _ -> 1

/// Checks if module instantiation statements are correct:
/// - Does a loaded component exist with the given name?
/// - Are the inputs and outputs the correct width?
/// - Are the portIds correct?
/// - Are all the ports connected?
/// - Are there any duplicate ports?
/// - Make sure inputs are inputs (they have been assigned something), outputs are not driven by anything
let checkModuleInstantiations
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string, int>)
    (project: Project) 
    (portMap: Map<string, string>)
    (errorList: ErrorInfo list) =

    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let moduleInstantiations = 
        foldAST getModuleInstantiationStatements [] (VerilogInput ast)

    let loadedComponentNames =
        project.LoadedComponents
        |> List.map (fun comp -> comp.Name)
        
    let loadedComponentNamesSet =
        loadedComponentNames
        |> Set.ofList
    let moduleTypeErrors = 
        moduleInstantiations
        |> List.collect (fun modInst ->
            match Set.contains modInst.Module.Name loadedComponentNamesSet with
            | true -> []
            | false -> 
                let message = sprintf "Component %s does not exist" modInst.Module.Name
                let closeVariables = findCloseVariable modInst.Module.Name loadedComponentNames
                let extraMessages = 
                    [|
                        {Text=(sprintf "Component '%s' does not exist - there is no custom component or Verilog component with this name" modInst.Module.Name);Copy=false;Replace=NoReplace}
                        
                    |]
                let replaceMsg =
                    match List.isEmpty closeVariables with
                    | false -> 
                        [|{Text=(sprintf "%s" closeVariables[0]);Copy=true;Replace=Variable modInst.Module.Name}|]
                    | _ -> [||]
                
                createErrorMessage linesLocations modInst.Module.Location message (Array.append extraMessages replaceMsg) modInst.Module.Name
        )

    let getMissingPortErrors modInst comp =
        let givenPortNames =
            modInst.Connections
            |> Array.map (fun conn -> conn.PortId.Name.ToUpper())
            |> Set.ofArray
        let expectedPortNames = 
            comp.InputLabels @ comp.OutputLabels 
            |> List.map fst
            |> Set.ofList
        let extraPortNames = Set.difference givenPortNames expectedPortNames
        let missingPortNames = Set.difference expectedPortNames givenPortNames
        let extraPortErrors =
            modInst.Connections
            |> Array.toList
            |> List.collect (fun conn ->
                if Set.contains (conn.PortId.Name.ToUpper()) extraPortNames then 
                    let extraMessages=                    
                        [|
                            {Text=sprintf "The port %A does not exist for component %A" conn.PortId.Name modInst.Module.Name; Copy=false;Replace=NoReplace};
                        |]
                    let message = sprintf "No such port for the given component"
                    createErrorMessage linesLocations conn.PortId.Location message extraMessages conn.PortId.Name
                else []
            ) 
        let missingPortErrors =
            expectedPortNames
            |> Set.toList
            |> List.collect (fun portName ->
                if Set.contains portName missingPortNames then
                    let extraMessages=                    
                        [|
                            {Text=sprintf "The port %A missing for component %A" portName modInst.Module.Name; Copy=false;Replace=NoReplace};
                        |]
                    let message = sprintf "Missing port(s) for component"
                    createErrorMessage linesLocations modInst.Module.Location message extraMessages modInst.Module.Name
                else []
            )
        extraPortErrors @ missingPortErrors
    let inputPorts = 
        portMap
        |> Map.filter (fun k v -> v="input")
        |> Map.keys
        |> Seq.toList
    let lhsVariables = 
        foldAST getAssignments' [] (VerilogInput ast)
        |> List.map (fun assign -> assign.LHS.Primary.Name)
        |> List.append inputPorts
        |> Set.ofList

    let getInputOutputPortErrors modInst comp =
        let inputPorts = 
            comp.InputLabels
            |> List.map fst
            |> Set.ofList
        let outputPorts =
            comp.OutputLabels
            |> List.map fst
            |> Set.ofList
        modInst.Connections
        |> Array.toList
        |> List.collect (fun conn ->
            match (Set.contains (conn.PortId.Name.ToUpper()) inputPorts), 
                (Set.contains (conn.PortId.Name.ToUpper()) outputPorts),
                (Set.contains conn.Primary.Primary.Name lhsVariables) with
            | true, _, false -> []
            | _, true, true ->
                let extraMessages=                    
                    [|
                        {Text=sprintf "Output port %A in module %A is already driven by continuous or procedural assignments" conn.Primary.Primary.Name modInst.Module.Name; Copy=false;Replace=NoReplace};
                    |]
                let message = sprintf "Output port already driven"
                createErrorMessage linesLocations conn.Primary.Primary.Location message extraMessages conn.Primary.Primary.Name
            | _ -> []
        )
        // all inputPorts have to be on lhs
        // all outputport cant be on lhs
    let portWidthErrors =
        moduleInstantiations
        |> List.collect (fun modInst ->
            match  List.filter (fun comp -> comp.Name = modInst.Module.Name)project.LoadedComponents with
            | [] -> []
            | [comp] ->
                modInst.Connections
                |> Array.toList
                |> List.collect (fun (conn: NamedPortConnectionT) ->
                    match List.tryFind (fun port' -> fst port' = conn.PortId.Name.ToUpper()) (comp.InputLabels@comp.OutputLabels) with
                    | Some port -> 
                        let w = getPrimaryWidth wireAndPortSizeMap conn.Primary
                        if (snd port) <> w then
                            let extraMessages=                    
                                [|
                                    {Text=sprintf "Wrong port width for port %A in module %A: %A bits wide but expected %A bits" conn.PortId.Name modInst.Module.Name w (snd port); Copy=false;Replace=NoReplace};
                                |]
                            let message = sprintf "Wrong port width"
                            createErrorMessage linesLocations conn.Primary.Primary.Location message extraMessages conn.Primary.Primary.Name
                        else []
                    | _ ->
                        []
                    )
                |> List.append (getMissingPortErrors modInst comp)
                |> List.append (getInputOutputPortErrors modInst comp)
            | _ -> failwithf "There are multiple custom components with this name!"
        )


    let duplicatePortErrors =
        moduleInstantiations
        |> List.collect (fun modInst ->
            ((List.empty, Set.empty), modInst.Connections)
            ||> Array.fold (fun (errors,ports) conn ->
                let error =
                    if Set.contains conn.PortId.Name ports then
                        let extraMessages=                    
                            [|
                                {Text=sprintf "Duplicate port name '%s' for module %s" conn.PortId.Name modInst.Module.Name; Copy=false;Replace=NoReplace};
                            |]
                        let message = sprintf "Duplicate port"
                        createErrorMessage linesLocations conn.PortId.Location message extraMessages conn.PortId.Name
                    else []
                (errors@error,Set.add conn.PortId.Name ports)
            )
            |> fst
        )
    
    errorList @ moduleTypeErrors @ portWidthErrors @ duplicatePortErrors

// Check for blocking / nonblocking assignments in always_ff/always_comb - done, tested, later: change error length to only cover the operator
// check if variables are well defined in always_comb blocks - done
// Check if clk is input if there are always_ff blocks done
// check if case item numbers and if conditions and always expressions are correct! done
// Finish checking branches for case statement - done
// Check if clk is not output, check logic declarations to not have name clk - done
// debug case item number width check! - done, seems to work now
// check logic declarations - 
    // - check if range is correct - done
    // - check name? should be done probably - done
    // - store width properly - done?
    // - check if variable is on the lhs of any assignments - done
// check width of lhs -rhs when assigning to logic?
    // need to check lhs logic i; i[1] = 1'b1; should give an error
// check if wire is assigned to twice - done
// check if rhs has been assigned to? probs done
// in always comb we always require rhs to be assigned to before 
    // a=b; b=1; not allowed, b=1; a=b; HintPane

// continue unit testing
// improve syntax errors
// cycle checking !! - done, could maybe improve error message with line locations, but this is very complex
// maybe remove width checks, should then output bits not assigned still give an error?
// add / and * operator


// check if clk is not used for anything else than always_ff @(posedge clk) - done 

// how should width check be done?
// how can syntax errors be improved?
 
 // fix endmodule error location - not super important atm
 // fix assignment width check
 // fix width of output port on the rhs - done
 // check if assignment lhs is not an input port
 // change variables driven by multiple always blocks to check full variable and not bit by bit

 // module instantiation statement:
 // make sure input is driven somewhere
 // upper case portnames? - this shouldn't cause any problems
 // make sure primaries used are valid
