module VerilogAST

open VerilogTypes
/////////////////// Types to store the AST internally

// NumberT

// Only one module in one file?


type ItemDU =
    | IOItem of IOItemT
    | ParamDecl of ParameterItemT
    | ContinuousAssign of ContinuousAssignT
    | AlwaysConstruct of AlwaysConstructT

type StatementDU =
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT
    | Case of CaseStatementT
    | Conditional of ConditionalT
    | ForStatement of ForStatementT

// should I add everything in here?
type ASTNode =
    | IOItem of IOItemT
    | ParamDecl of ParameterItemT
    | ContinuousAssign of ContinuousAssignT
    | Declaration of DeclarationT
    | AlwaysConstruct of AlwaysConstructT
    | Statement of StatementT
    | NonBlockingAssign of NonBlockingAssignT
    | BlockingAssign of BlockingAssignT
    | SeqBlock of SeqBlockT
    | Case of CaseStatementT
    | CaseItem of CaseItemT
    | Conditional of ConditionalT
    | IfStatement of IfStatementT
    | ForStatement of ForStatementT
    | Assignment of AssignmentT
    | AssignmentLHS of AssignmentLHST
    | Expression of ExpressionT
    | Primary of PrimaryT
    | ParameterItem of ParameterItemT
    | Parameter of ParameterT
    | Range of RangeT
    | Number of NumberT
    | Item of ItemT
    | ModuleItems of ModuleItemsT
    | Module of ModuleT
    | VerilogInput of VerilogInput
    | ModuleInstantiation of ModuleInstantiationT
type Module = {AST: ASTNode;}


///////////////////// Error handling helpers /////////////////////////

/// converts StatementT into StatementDU
let getAlwaysStatement (s: StatementT) : StatementDU =
    match s.BlockingAssign, s.NonBlockingAssign, s.CaseStatement, s.Conditional, s.SeqBlock, s.ForStatement with
    | Some blocking, None, None, None, None, None -> StatementDU.BlockingAssign(blocking)
    | None, Some nonblocking, None, None, None, None -> StatementDU.NonBlockingAssign(nonblocking)
    | None, None, Some case, None, None, None -> StatementDU.Case(case)
    | None, None, None, Some cond, None, None -> StatementDU.Conditional(cond)
    | None, None, None, None, Some seqBlock, None -> StatementDU.SeqBlock(seqBlock)
    | None, None, None, None, None, Some forStmt -> StatementDU.ForStatement(forStmt)
    | _ -> failwithf "Should not happen!"
//maybe we can combine these two, or do smth smarter
let statementToNode (statement:StatementDU) : ASTNode =
    match statement with
    | StatementDU.BlockingAssign blocking -> BlockingAssign(blocking)
    | StatementDU.NonBlockingAssign nonblocking -> NonBlockingAssign(nonblocking)
    | StatementDU.Case case -> Case(case)
    | StatementDU.Conditional cond -> Conditional(cond)
    | StatementDU.SeqBlock seqBlock -> SeqBlock(seqBlock)
    | StatementDU.ForStatement forStmt -> ForStatement(forStmt)

let getItem (item: ItemT)  =
    //printfn $"{item}"
    match item.IODecl, item.ParamDecl, item.Decl, item.Statement, item.AlwaysConstruct, item.ModuleInstantiation with
    | Some ioDecl, None, None, None, None, None -> IOItem ioDecl
    | None, Some paramDecl, None, None, None, None -> ParamDecl paramDecl
    | None, None, Some decl, None, None, None -> Declaration decl
    | None, None, None, Some contAssign, None, None -> ContinuousAssign contAssign
    | None, None, None, None, Some always, None -> AlwaysConstruct always
    | None, None, None, None, None, Some moduleInst -> ModuleInstantiation moduleInst
    | anything -> 
        printfn $"{anything}" 
        failwithf "Should not happen"

// Helper to evaluate integer expressions (simple constants only)
let tryEvalConst (number:NumberT) =
    let width = (Option.get number.Bits) |> int
    let _base = Option.get number.Base
    let no = (Option.get number.AllNumber)
    let text = 
        match _base with
        |"'b" -> "0b"+no
        |"'h" -> "0x"+no
        |_ -> no
    let constValue: int =
        match NumberHelpers.strToIntCheckWidth width text with
        |Ok n -> int n
        |Error _ -> failwithf "Shouldn't happen!" // TODO: better error handling - indicate that the value and size do not match
    constValue
let rec evalIntExpression (expr0: ExpressionT) : int =
    let rec strip (e: ExpressionT) =
        match e.Unary with
        | Some u when Option.isSome u.Expression -> strip (Option.get u.Expression)
        | _ -> e

    let expr = strip expr0

    match expr.Operator, expr.Head, expr.Tail, expr.Unary with
    // plain literal e.g. 4'd3  (ExpressionT with Unary.Number)
    | None, None, None, Some unary ->
        match unary.Number with
        | Some num -> tryEvalConst num
        | None -> failwith "Unsupported expression in loop: expected numeric literal"

    // allowed binary-like node where RHS is numeric literal (e.g. i < 3'd3 or i + 1)
    | Some _, Some _, Some tail, None ->
        let tail = strip tail
        match tail.Unary with
        | Some u ->
            match u.Number with
            | Some num -> tryEvalConst num
            | None -> failwith "Tail must be a numeric literal"
        | None -> failwith "Tail must be a unary numeric literal"
    | _ -> failwith "Unsupported expression in loop"


let rec substLoopVar (loopVarName:string) (value:int) (width:int) (stmt:StatementT) : StatementT =
    let rec substLoopExpr (loopVarName:string) (value:int) (width:int) (expr:ExpressionT) : ExpressionT =
        let substUnary (unary:UnaryT) : UnaryT =
            match unary.Primary with
            | Some prim when prim.Primary.Name = loopVarName ->
                { unary with
                    Type = "number"
                    Primary = None
                    Number = Some ({
                        Type = "number"
                        NumberType = "all"
                        Bits = Some (string width)
                        Base = Some "'d"
                        AllNumber = Some (string value)
                        UnsignedNumber = None
                        Location = prim.Primary.Location
                    })
                }
            | _ -> 
                let expr' = unary.Expression |> Option.map (substLoopExpr loopVarName value width)
                { unary with Expression = expr' }
            
        match expr.Operator, expr.Head, expr.Tail, expr.Unary with
        | None, None, None, Some unary ->
            let unary' = substUnary unary
            { expr with Unary = Some unary' }
        | Some op, Some head, Some tail, None ->
            { expr with 
                Head = Some (substLoopExpr loopVarName value width head)
                Tail = Some (substLoopExpr loopVarName value width tail) }
        | _ -> expr 
    
    // Drops any assignment whose LHS is the loop variable itself
    let isAssignToLoopVar (lhs: AssignmentLHST) =
        lhs.Primary.Name = loopVarName

    let substLhs (lhs: AssignmentLHST) =
        let vbs = lhs.VariableBitSelect |> Option.map (substLoopExpr loopVarName value width)
        // If variable bit-select reduces to a constant, write it into Primary.BitsStart/BitsEnd
        let lhs' =
            match vbs with
            | Some expr ->
                try
                    let idx = evalIntExpression expr
                    let idxStr = string idx
                    { lhs with VariableBitSelect = None; BitsStart = Some idxStr; BitsEnd = Some idxStr }
                with _ -> { lhs with VariableBitSelect = vbs }
            | None -> lhs
        lhs'

    // TODO: force loop variable to be initialised outside the loop (think about implemnting int?)
    // TODO: currently in always_ff does not require initialisation (error previously?)

    // If loop variable appears on RHS, substitute it with the given value
    let substAssign (a: AssignmentT) =
        { a with
            LHS = substLhs a.LHS
            RHS = substLoopExpr loopVarName value width a.RHS }

    match stmt.BlockingAssign, stmt.NonBlockingAssign, stmt.SeqBlock, stmt.Conditional, stmt.CaseStatement, stmt.ForStatement with
    | Some b, None, None, None, None, None ->
        if isAssignToLoopVar b.Assignment.LHS then
            failwithf "Assignments to loop variable inside loop body are not supported"
        else { stmt with BlockingAssign = Some { b with Assignment = substAssign b.Assignment } }
    | None, Some nb, None, None, None, None ->
        if isAssignToLoopVar nb.Assignment.LHS then
            failwithf "Assignments to loop variable inside loop body are not supported"
        else { stmt with NonBlockingAssign = Some { nb with Assignment = substAssign nb.Assignment } }
    | None, None, Some sb, None, None, None ->
        let stmts' = sb.Statements |> Array.map (substLoopVar loopVarName value width)
        { stmt with SeqBlock = Some { sb with Statements = stmts' } }
    | None, None, None, Some cond, None, None ->
        let ifStmt' = substLoopVar loopVarName value width cond.IfStatement.Statement
        let elseStmt' = cond.ElseStatement |> Option.map (substLoopVar loopVarName value width)
        let condStmt' = substLoopExpr loopVarName value width cond.IfStatement.Condition
        { stmt with Conditional = Some { cond with IfStatement = { cond.IfStatement with 
                                                                    Condition = condStmt'; 
                                                                    Statement = ifStmt' }; 
                                                ElseStatement = elseStmt' } }
    | None, None, None, None, Some caseStmt, None ->
        let caseExpr' = substLoopExpr loopVarName value width caseStmt.Expression
        let caseItems' =
            caseStmt.CaseItems
            |> Array.map (fun ci -> 
                let stmt' = substLoopVar loopVarName value width ci.Statement
                { ci with Statement = stmt' }
            )
        let defaultStmt' = caseStmt.Default |> Option.map (substLoopVar loopVarName value width)
        { stmt with CaseStatement = Some { caseStmt with 
                                                Expression = caseExpr'; 
                                                CaseItems = caseItems'; 
                                                Default = defaultStmt' } }
    | None, None, None, None, None, Some forStmt ->
        // Substitute occurrences of the current loop variable into the inner for-loop
        let initRHS' = substLoopExpr loopVarName value width forStmt.Initialisation.RHS
        let cond' = substLoopExpr loopVarName value width forStmt.Condition
        let stepRHS' = substLoopExpr loopVarName value width forStmt.Step.RHS
        let stmt' = substLoopVar loopVarName value width forStmt.Statement
        let forStmt' = { forStmt with Initialisation = { forStmt.Initialisation with RHS = initRHS' }; Condition = cond'; Step = { forStmt.Step with RHS = stepRHS' }; Statement = stmt' }
        { stmt with ForStatement = Some forStmt' }
        
    // TODO: add nested for loop handling
    | _ -> stmt // other cases not handled for now
let rec unrollForLoops (forstmt:ForStatementT) : SeqBlockT =
    let computeIterations startV op endV step =
        match op with
        | "<"  -> endV - startV
        | "<=" -> endV - startV + 1
        | ">"  -> startV - endV
        | ">=" -> startV - endV + 1
        | _ -> failwith "Unsupported operator"

    let startValue = evalIntExpression forstmt.Initialisation.RHS
    let endValue   = evalIntExpression forstmt.Condition
    let stepValue  = evalIntExpression forstmt.Step.RHS
    let iterations = 
        match forstmt.Condition.Operator with
        | Some op -> computeIterations startValue op endValue stepValue
        | None -> failwith "Unsupported operator in for loop condition"

    if iterations < 0 || iterations > 10 then
        failwithf "Refusing to unroll loop with %d iterations" iterations

    let bodyStatements =
        match forstmt.Statement.SeqBlock with
        | Some seqBlock -> seqBlock.Statements
        | None -> [| forstmt.Statement |]

    let loopVarName = forstmt.Initialisation.LHS.Primary.Name
    let loopVarWidth =
        match forstmt.Initialisation.LHS.Width with
        | Some w -> int w
        | None -> 32

    let repeatedStmts =
        Array.init iterations (fun k -> 
            let value = startValue + k * stepValue
            bodyStatements
            |> Array.map (substLoopVar loopVarName value loopVarWidth)
            |> Array.collect (fun s ->
                match s.ForStatement with
                | Some innerFor ->
                    // recursively unroll inner for after it has been substituted
                    (unrollForLoops innerFor).Statements
                | None -> [| s |]
            )
        )
        |> Array.concat
    let unrolled_seq_block: SeqBlockT =
        { Type = "seq_block"; Statements = repeatedStmts; Location = forstmt.Location }
    // failwithf "STATEMENT UNROLLED: iterations = %d, full block = %A" iterations unrolled_seq_block

    unrolled_seq_block

/// Recursively folds over an ASTNode, calling folder at every level. Only explores parts where there are multiple possibilities within a Node
let rec foldAST folder state (node:ASTNode) =
    let state' = folder state node
    match node with
    | VerilogInput input ->
        foldAST folder state'  (Module(input.Module))
    | Module m ->
        foldAST folder state' (ModuleItems(m.ModuleItems))
    | ModuleItems items -> 
        items.ItemList
        |> Array.map (fun item -> Item(item))
        |> Array.fold (foldAST folder) state'
    | Item item -> 
        foldAST folder state' (getItem item)
    | AlwaysConstruct always ->
        foldAST folder state' (Statement(always.Statement))
    | Statement statement -> 
        statement
        |> getAlwaysStatement
        |> statementToNode
        |> foldAST folder state'
    | SeqBlock seqBlock ->
        seqBlock.Statements
        |> Array.map (fun s -> Statement(s))
        |> Array.fold (foldAST folder) state'
    | Case case ->
        let newState = foldAST folder state' (Expression(case.Expression))
            
        let newState' = 
            case.CaseItems
            |> Array.map (fun item -> CaseItem(item))
            |> Array.fold (foldAST folder) newState
        match case.Default with
        | Some stmt -> foldAST folder newState' (Statement(stmt))
        | _ -> newState'
    | CaseItem caseItem -> 
        let newstate =
            caseItem.Expressions
            |> Array.map (fun expr -> Number expr)
            |> Array.fold (foldAST folder) state'
        foldAST folder newstate (Statement(caseItem.Statement))
    | Conditional cond ->
        let tmpState =
            IfStatement(cond.IfStatement)
            //|> Array.map (fun stmt -> IfStatement(stmt))
            //|> Array.fold (foldAST folder) state'
            |> foldAST folder state'
        match cond.ElseStatement with
        | Some elseStmt -> List.fold (foldAST folder) tmpState [Statement(elseStmt)]
        | _ -> tmpState
    | ContinuousAssign assign ->
        foldAST folder state' (Assignment(assign.Assignment))
    | Assignment assign ->
        (foldAST folder state' (AssignmentLHS(assign.LHS)), (Expression(assign.RHS)))
        ||> foldAST folder 
    | NonBlockingAssign nonblocking ->
        foldAST folder state' (Assignment(nonblocking.Assignment))
    | BlockingAssign blocking ->
        foldAST folder state' (Assignment(blocking.Assignment))
    | IfStatement ifstmt ->
        (foldAST folder state' (Expression(ifstmt.Condition)), (Statement(ifstmt.Statement)))
        ||> foldAST folder
    | ForStatement forstmt ->
        let tmpState = 
            unrollForLoops forstmt
        foldAST folder state' (SeqBlock(tmpState))
    | AssignmentLHS lhs ->
        match lhs.VariableBitSelect with
        | Some expr -> 
            foldAST folder state' (Expression(expr))
        | _ -> state'
    | _ ->
        state'

/// get rhs expressions from always, continuous assign, case stmt... (all of them)
let getAllExpressions' (expressions: List<ExpressionT>) (node: ASTNode) =
    match node with
    | Expression expr ->
        expressions @ [expr]
    | _ -> expressions

let getNumbers (numbers) (node) =
    match node with
    | Number num -> numbers @ [num]
    | _ -> numbers

let getAssignments' (assignments: List<AssignmentT>) (node: ASTNode) =
    match node with
    | Assignment assign ->
        assignments @ [assign]
    | _ -> assignments

let getContAssignments (assignments: List<AssignmentT>) (node: ASTNode) =
    match node with
    | ContinuousAssign contAssign -> assignments @ [contAssign.Assignment]
    | _ -> assignments

let getAlwaysAssignments (assignments: List<AssignmentT>) (node: ASTNode) =
    match node with
    | BlockingAssign blocking -> assignments @ [blocking.Assignment]
    | NonBlockingAssign nonblocking -> assignments @ [nonblocking.Assignment]
    | _ -> assignments

let getAssignmentsWithLocations (assignments: List<AssignmentT*int>) (node: ASTNode) =
    match node with
    | ModuleItems items ->
        let contAssigns =
            items.ItemList
            |> Array.toList 
            |> List.filter (fun item -> item.ItemType = "statement")
            |> List.map (fun item -> (Option.get item.Statement),item.Location)
            |> List.map (fun (statement,loc) -> statement.Assignment,loc)
        assignments @ contAssigns
    | Statement stmt ->
        match getAlwaysStatement stmt |> statementToNode with
        | BlockingAssign blocking ->
            assignments @ [blocking.Assignment, stmt.Location]
        | NonBlockingAssign nonblocking ->
            assignments @ [nonblocking.Assignment, stmt.Location]
        | _ -> assignments
    | _ -> assignments

let getAlwaysBlocks (alwaysBlocks: List<AlwaysConstructT>) (node: ASTNode) =
    match node with
    | AlwaysConstruct alwaysBlock -> alwaysBlocks @ [alwaysBlock]
    | _ -> alwaysBlocks

let getBlockingAssignmentsWithLocation (assignments: List<BlockingAssignT*int>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match getAlwaysStatement stmt |> statementToNode with
        | BlockingAssign blocking ->
            assignments @ [blocking, stmt.Location]
        | _ -> assignments
    | _ -> assignments

let getNonBlockingAssignmentsWithLocation (assignments: List<NonBlockingAssignT*int>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match getAlwaysStatement stmt |> statementToNode with
        | NonBlockingAssign nonblocking ->
            assignments @ [nonblocking, stmt.Location]
        | _ -> assignments
    | _ -> assignments

let getBlockingAssignments (assignments: List<BlockingAssignT>) (node: ASTNode) =
    match node with
    | BlockingAssign blocking -> assignments @ [blocking]
    | _ -> assignments

let getNonBlockingAssignments (assignments: List<NonBlockingAssignT>) (node: ASTNode) =
    match node with
    | NonBlockingAssign nonblocking -> assignments @ [nonblocking]
    | _ -> assignments
