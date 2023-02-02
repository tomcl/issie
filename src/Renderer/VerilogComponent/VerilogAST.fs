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
    match s.BlockingAssign, s.NonBlockingAssign, s.CaseStatement, s.Conditional, s.SeqBlock with
    | Some blocking, None, None, None, None -> StatementDU.BlockingAssign(blocking)
    | None, Some nonblocking, None, None, None -> StatementDU.NonBlockingAssign(nonblocking)
    | None, None, Some case, None, None -> StatementDU.Case(case)
    | None, None, None, Some cond, None -> StatementDU.Conditional(cond)
    | None, None, None, None, Some seqBlock -> StatementDU.SeqBlock(seqBlock)
    | _ -> failwithf "Should not happen!"
//maybe we can combine these two, or do smth smarter
let statementToNode (statement:StatementDU) : ASTNode =
    match statement with
    | StatementDU.BlockingAssign blocking -> BlockingAssign(blocking)
    | StatementDU.NonBlockingAssign nonblocking -> NonBlockingAssign(nonblocking)
    | StatementDU.Case case -> Case(case)
    | StatementDU.Conditional cond -> Conditional(cond)
    | StatementDU.SeqBlock seqBlock -> SeqBlock(seqBlock)


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
