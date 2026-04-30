module VerilogAST

open VerilogTypes
/////////////////// Types to store the AST internally

// NumberT

// Only one module in one file?

type ModuleDU = 
    | ModuleOld
    | ModuleNew

type BaseDU = 
    | Binary
    | Decimal
    | Hex

type Number = 
    // | Decimal of unsignednumber: int * location: int
    | All of bits: int * num_base: BaseDU * allnumber: string * location: int
    | Unsigned of unsignednumber: int * location: int


type OperatorDU =
    | Lor
    | Land
    | OrOp
    | Xor
    | Xnor
    | AndOp
    | Nand
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | Plus
    | Minus
    | Mult
    | Sll
    | Srl
    | Sra
    | NotOp


type SelectDU = 
    | PlusWidth
    | MinusWidth

type PrimaryDU = 
        | Identifier of IdentifierT
        | IdentifierBit of id: IdentifierT * index: int
        | IdentifierBits of id: IdentifierT * start: int * end_: int
        | VariableBitSelect of IdentifierT * index: ExpressionDU
        | IdentifierBitsSelect of id: IdentifierT * start: ExpressionDU * width: int * select: SelectDU
        | IdentifierArray of id: IdentifierT * indices: ExpressionDU array * start: int * end_: int

    and ExpressionDU =
        | LogicalOr of ExpressionDU * ExpressionDU
        | LogicalAnd of ExpressionDU * ExpressionDU
        | BitwiseOr of ExpressionDU * ExpressionDU
        | BitwiseXor of ExpressionDU * ExpressionDU
        | BitwiseXnor of ExpressionDU * ExpressionDU
        | BitwiseAnd of ExpressionDU * ExpressionDU
        | Equality of OperatorDU * ExpressionDU * ExpressionDU
        | Comparison of OperatorDU * ExpressionDU * ExpressionDU
        | ShiftExpr of OperatorDU * ExpressionDU * ExpressionDU
        | Additive of OperatorDU * ExpressionDU * ExpressionDU
        | Multiplicative of OperatorDU * ExpressionDU * ExpressionDU
        | Reduction of OperatorDU * ExpressionDU
        | Negation of UnaryDU
        | Unary of UnaryDU
        | UnaryUnsigned of Number
        | ConditionalOp of cond: ExpressionDU * ifTrue: ExpressionDU * ifFalse: ExpressionDU
    and UnaryDU = 
        | Primary of PrimaryDU
        | Number of Number
        | Parenthesis of ExpressionDU
        | Concat of ExpressionDU array


// ====== Statements =======
type AssignDU = 
    | Blocking
    | NonBlocking
    | WireAssign
    
type Assignment = {Type: AssignDU; LHS: AssignmentLHS; RHS: ExpressionDU }
and AssignmentLHS = {PrimaryType: PrimaryDU; VariableBitSelect: ExpressionDU option}

type StatementDU =
    | NonBlockingAssign of Assignment * location: int
    | BlockingAssign of Assignment * location: int
    // | WireAssign of Assignment
    | SeqBlock of StatementDU array * location: int
    | Case of CaseStatement * location: int
    | Conditional of ifstmt: IfStatement * elseStmt: StatementDU option * location: int
    | ForStatement of ForStatement * location: int

and IfStatement = {Condition: ExpressionDU; Statement: StatementDU; Location: int}

and CaseStatement = {Expression: ExpressionDU; CaseItems: CaseItem array; Default: StatementDU option; Location: int}
and CaseItem = {Expressions: Number array; Statement: StatementDU}

and ForStatement = {Initialisation: Assignment; Condition: ExpressionDU; Step: Assignment; Statement: StatementDU; Location: int}

// ====== Declarations ======
type Range = {Start: ExpressionDU; End: ExpressionDU; Location: int}

type DeclarationDU = 
    | InputDecl
    | OutputDecl
    | LogicDecl // reg decl
    | ParameterDecl

type DataTypeDU = 
    | WireType
    | Bit
    // | Integer

// type Array = {Ranges: Range array; Location: int}

type Declaration = {DeclarationType: DeclarationDU; DataType: DataTypeDU; Range: Range option; ArrayRanges: Range array option; Variables: IdentifierT array; Location: int}

// ====== Always ======
type AlwaysDU = 
    | AlwaysComb
    | AlwaysFF

type AlwaysConstruct = {AlwaysType: AlwaysDU; Statement: StatementDU; ClkLoc: int; Location: int}

// ====== Module Items ======
type ItemDU = 
    | IOItem of IOItem
    | ParamDecl of ParameterItem
    | Decl of Declaration
    | ContStatement of ContinuousAssign
    | AlwaysConstruct of AlwaysConstruct
    | ModuleInstantiation of ModuleInstantiation

and IOItem = {DeclarationType: DeclarationDU; DataType: DataTypeDU; Range : Range option; Variables: IdentifierT array; Location: int}

and Parameter = {Identifier: IdentifierT; RHS: ExpressionDU; Location: int}
and ParameterItem = {DeclarationType: DeclarationDU; Parameters : Parameter array;}

and ContinuousAssign = {StatementType: ContStatementDU; Assignment : Assignment; Location: int}
and ContStatementDU = 
    | Assign
    | Wire

and ModuleInstantiation = {Module: IdentifierT; Identifier: IdentifierT; Parameters: OverridenParameter array option; Connections: NamedPortConnection array}
and NamedPortConnection = {PortId: IdentifierT; Primary: PrimaryDU}
and OverridenParameter = {Identifier: IdentifierT; Value: ExpressionDU}

type ModuleItems = {ItemList: ItemDU array; Location: int}

type Module = {Type: ModuleDU; ModuleName: IdentifierT; PortList: string array; Locations: string array; ModuleItems: ModuleItems; EndLocation: int;}

type ASTNode =
    | IOItem of IOItem
    | ParamDecl of ParameterItem
    | ContStatement of ContinuousAssign
    | Declaration of Declaration
    | AlwaysConstruct of AlwaysConstruct
    | Statement of StatementDU
    // | NonBlockingAssign of NonBlockingAssign
    // | BlockingAssign of BlockingAssign
    // | SeqBlock of SeqBlock
    | Case of CaseStatement
    | CaseItem of CaseItem
    // | Conditional of Conditional
    | IfStatement of IfStatement
    | ForStatement of ForStatement
    | Assignment of Assignment
    | AssignmentLHS of AssignmentLHS
    | Expression of ExpressionDU
    | Primary of PrimaryDU
    | Unary of UnaryDU
    | ParameterItem of ParameterItem
    // | Parameter of Parameter
    | Range of Range
    | Number of Number
    | Item of ItemDU
    | ModuleItems of ModuleItems
    | Module of Module
    | VerilogInput of VerilogInput
    | ModuleInstantiation of ModuleInstantiation
// type Module = {AST: ASTNode;}

//////////////////// EXPRESSION EVALUATION ////////////////////
// Helper to evaluate integer expressions
let tryEvalConst (number:Number) =
    match number with
    | Unsigned (value, _) -> value
    | All (bits, numBase, allNumber, _) ->
        let width = bits |> int
        // let _base = Option.get number.Base
        let digits = string allNumber
        let text = 
            match numBase with
            | Binary -> "0b"+digits
            | Hex -> "0x"+digits
            |_ -> digits
        let constValue: int =
            match NumberHelpers.strToIntCheckWidth width text with
            |Ok n -> int n
            |Error _ -> failwithf "Shouldn't happen!" // TODO: better error handling - indicate that the value and size do not match
        constValue

/// Helper function to evaluate parameters to constants
let rec evalExprWithParams (paramExpr: ExpressionDU) (paramMap: Map<string, int>) : int =
    let rec strip (expr: ExpressionDU) =
        match expr with
        | ExpressionDU.Unary (Parenthesis e) -> strip e
        | _ -> expr
    let expr = strip paramExpr

    match expr with
    | ExpressionDU.UnaryUnsigned n ->  tryEvalConst n
    | ExpressionDU.Negation (UnaryDU.Number n) -> -tryEvalConst n
    | ExpressionDU.Unary (UnaryDU.Number n) -> tryEvalConst n

    | ExpressionDU.Unary (UnaryDU.Primary (Identifier id)) ->
        match Map.tryFind id.Name paramMap with
        | Some v -> v
        | None -> failwithf "Only parameters allowed to be used in width expressions (for now). '%s' is not a parameter or is an undefined parameter." id.Name
    
    | Additive (op, lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        match op with
        | Plus -> l + r
        | Minus -> l - r
        | _ -> failwith "Unsupported additive operator for parameter evaluation"
    | Multiplicative (op, lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        match op with
        | Mult -> l * r
        | _ -> failwith "Unsupported multiplicative operator for parameter evaluation"
    | ShiftExpr (op, lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        match op with
        | Sll -> l <<< r
        | Srl -> l >>> r
        | Sra -> l >>> r 
        | _ -> failwith "Unsupported shift operator for parameter evaluation"
    | BitwiseAnd (lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        l &&& r
    | BitwiseOr (lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        l ||| r
    | BitwiseXor (lhs, rhs) ->
        let l = evalExprWithParams lhs paramMap
        let r = evalExprWithParams rhs paramMap
        l ^^^ r
    | _ -> failwith "Expression does not evaluate to a constant integer or parameter reference"



/// Helper function to convert expressions to ints and back (for width checking)
let rec evalExpr (expr: ExpressionDU) : int =
    match expr with
    | ExpressionDU.UnaryUnsigned n ->  tryEvalConst n
    | ExpressionDU.Negation (UnaryDU.Number n) -> -tryEvalConst n
    | ExpressionDU.Unary (UnaryDU.Number n) -> tryEvalConst n
    
    | Additive (op, lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        match op with
        | Plus -> l + r
        | Minus -> l - r
        | _ -> failwith "Unsupported operator for additive eval"
    | Multiplicative (op, lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        match op with
        | Mult -> l * r
        | _ -> failwith "Unsupported operator for multiplicative eval"
    | ShiftExpr (op, lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        match op with
        | Sll -> l <<< r
        | Srl -> l >>> r
        | Sra -> l >>> r 
        | _ -> failwith "Unsupported operator for shift eval"
    | BitwiseAnd (lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        l &&& r
    | BitwiseOr (lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        l ||| r
    | BitwiseXor (lhs, rhs) ->
        let l = evalExpr lhs
        let r = evalExpr rhs
        l ^^^ r
    | _ -> failwith "Expression does not evaluate to a constant integer"



// ////////// AST Conversion - from string type (JSON) to DU types (internal representation)
let parseDataType = function
    | "bit" -> Bit
    | "wire" -> WireType
    | s -> failwith $"Unknown DataType: {s}"

let parseDeclarationType = function
    | "input" -> InputDecl
    | "output" -> OutputDecl
    | "logic" -> LogicDecl
    | "parameter" -> ParameterDecl
    | s -> failwith $"Unknown DeclarationType: {s}"

let parseOperation = function
    | "||" -> Lor
    | "&&" -> Land
    | "|" -> OrOp
    | "^" -> Xor
    | "~^" | "^~" -> Xnor
    | "&" -> AndOp
    | "!&" -> Nand
    | "==" -> Eq
    | "!=" -> Neq
    | "<" -> Lt
    | "<=" -> Lte
    | ">" -> Gt
    | ">=" -> Gte
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Mult
    | "<<" -> Sll
    | ">>" -> Srl
    | ">>>" -> Sra
    | "!" | "~" -> NotOp
    | s -> failwith $"Unknown operator: {s}"


let convertNumber (raw: NumberT) : Number =
    match raw.NumberType with
    | "decimal" -> Unsigned (raw.UnsignedNumber |> Option.map int |> Option.get, raw.Location)
    | "unsigned" -> Unsigned (raw.UnsignedNumber |> Option.map int |> Option.get, raw.Location)
    | "all" -> 
        let bits = raw.Bits |> Option.map int |> Option.get
        let allnumber = raw.AllNumber |> Option.get
        match raw.Base with
        | Some "'d" -> All (bits, Decimal, allnumber, raw.Location)
        | Some "'b" -> All (bits, Binary, allnumber, raw.Location)
        | Some "'h" -> All (bits, Hex, allnumber, raw.Location)
        | Some b -> failwith $"Unknown number base: {b}"
        | None -> failwith "Base must be specified for 'all' number type"
    | s -> failwith $"Unknown number type: {s}"

let parseSelectType = function
    | "plus" -> PlusWidth
    | "minus" -> MinusWidth
    | s -> failwith $"Unknown select type: {s}"


let rec convertExpression (raw: ExpressionT) : ExpressionDU =
    match raw.Type with
    | "unary" -> ExpressionDU.Unary (convertUnary (Option.get raw.Unary))
    | "unary_unsigned" -> UnaryUnsigned (convertNumber (Option.get (Option.get raw.Unary).Number))
    | "negation" -> Negation (convertUnary (Option.get raw.Unary))
    | "reduction" -> Reduction (parseOperation (Option.get raw.Operator), ExpressionDU.Unary (convertUnary (Option.get raw.Unary)))
    | "multiplicative" -> Multiplicative (parseOperation (Option.get raw.Operator), convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "additive" -> Additive (parseOperation (Option.get raw.Operator), convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "SHIFT" | "shift" -> ShiftExpr (parseOperation (Option.get raw.Operator), convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "comparison" -> Comparison (parseOperation (Option.get raw.Operator), convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "equality" -> Equality (parseOperation (Option.get raw.Operator), convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "bitwise_AND" -> BitwiseAnd (convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "bitwise_OR" -> BitwiseOr (convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "bitwise_XOR" -> BitwiseXor (convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "logical_AND" -> LogicalAnd (convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "logical_OR" -> LogicalOr (convertExpression (Option.get raw.Head), convertExpression (Option.get raw.Tail))
    | "conditional_cond" -> 
        // raw.Head is condition, raw.Tail is conditional_result with Head=ifTrue, Tail=ifFalse
        let cond = convertExpression (Option.get raw.Head)
        let result = Option.get raw.Tail
        let ifTrue = convertExpression (Option.get result.Head)
        let ifFalse = convertExpression (Option.get result.Tail)
        ConditionalOp (cond, ifTrue, ifFalse)
    | "conditional_result" ->
        // This shouldn't be called directly, but handle it just in case
        failwith "conditional_result should not be converted directly"
    | s -> failwith $"Unknown expression type: {s}"

and convertUnary (raw: UnaryT) : UnaryDU =
    let rec flattenUnaryList (raw: ExpressionT) : ExpressionDU array =
        match raw.Type with
        | "unary_list" ->
            let head = raw.Head |> Option.map convertExpression
            let tail = raw.Tail |> Option.map flattenUnaryList |> Option.defaultValue [||]
            match head with
            | Some h -> Array.append [| h |] tail
            | None -> tail
        | _ ->
            [| convertExpression raw |]

    match raw.Type with
    | "primary" -> UnaryDU.Primary (convertPrimary (Option.get raw.Primary))
    | "number" -> UnaryDU.Number (convertNumber (Option.get raw.Number))
    | "parenthesis" -> UnaryDU.Parenthesis (convertExpression (Option.get raw.Expression))
    | "concat" -> UnaryDU.Concat (flattenUnaryList (Option.get raw.Expression))
    | s -> failwith $"Unknown unary type: {s}"

and convertPrimary (raw: PrimaryT) : PrimaryDU =
    match raw.PrimaryType with
    | "identifier" -> 
        Identifier raw.Primary
    | "identifier_bit" ->
        // Single bit select: x[1]
        let idx = int (raw.BitsStart |> Option.map int |> Option.get)
        IdentifierBit (raw.Primary, idx)
    | "identifier_bit2" ->
        let idx = convertExpression (Option.get raw.Expression)
        VariableBitSelect (raw.Primary, idx)
        // failwithf "Currently VBS not implemented"
    | "identifier_bits" -> 
        // Range select: x[start:end]
        let start = int (raw.BitsStart |> Option.map int |> Option.get)
        let end_ = int (raw.BitsEnd |> Option.map int |> Option.get)
        IdentifierBits (raw.Primary, start, end_)
    | "identifier_bits_select" ->
        // Variable bit select: x[i+:width] or x[i-:width]
        let start = convertExpression (Option.get raw.Expression)
        let width = Option.get raw.Width
        let selectType = parseSelectType (Option.get raw.SelectType)
        IdentifierBitsSelect (raw.Primary, start, width, selectType)
    | "identifier_array" ->
        // Array access: x[i][j]...
        let indices = 
            raw.ArrayIndices 
            |> Option.defaultValue [||] 
            |> Array.map convertExpression
        let start = int (raw.BitsStart |> Option.map int |> Option.get)
        let end_ = int (raw.BitsEnd |> Option.map int |> Option.get)
        IdentifierArray (raw.Primary, indices, start, end_)
    | s -> failwith $"Unknown primary type: {s}"

// ====== Statement conversion ======
let convertAssignmentLHS (raw: VerilogTypes.AssignmentLHST) : AssignmentLHS =
    let prim: PrimaryT =
        { Type = ""
          PrimaryType = raw.PrimaryType
          BitsStart = raw.BitsStart
          BitsEnd = raw.BitsEnd
          Primary = raw.Primary
          Expression = raw.VariableBitSelect
          Width = raw.Width
          ArrayIndices = raw.ArrayIndices
          SelectType = raw.SelectType }

    { PrimaryType = convertPrimary prim; 
    VariableBitSelect = raw.VariableBitSelect |> Option.map convertExpression }

let rec convertAssignment (raw: VerilogTypes.AssignmentT) (assignType: AssignDU) : Assignment =
    { Type = assignType; LHS = convertAssignmentLHS raw.LHS; RHS = convertExpression raw.RHS }

and convertRange (raw: VerilogTypes.RangeT) : Range =
    { Start = convertExpression raw.Start; End = convertExpression raw.End; Location = raw.Location }

and convertStatement (raw: VerilogTypes.StatementT) : StatementDU =
    match raw.BlockingAssign, raw.NonBlockingAssign, raw.CaseStatement, raw.Conditional, raw.SeqBlock, raw.ForStatement with
    | Some blocking, None, None, None, None, None ->
        BlockingAssign (convertAssignment blocking.Assignment Blocking, raw.Location)
    | None, Some nonblocking, None, None, None, None ->
        NonBlockingAssign (convertAssignment nonblocking.Assignment NonBlocking, raw.Location)
    | None, None, Some case, None, None, None ->
        StatementDU.Case (convertCaseStatement case, raw.Location)
    | None, None, None, Some cond, None, None ->
        Conditional (convertIfStatement cond.IfStatement, cond.ElseStatement |> Option.map convertStatement, raw.Location)
    | None, None, None, None, Some seqBlock, None ->
        SeqBlock (seqBlock.Statements |> Array.map convertStatement, seqBlock.Location)
    | None, None, None, None, None, Some forStmt ->
        StatementDU.ForStatement (convertForStatement forStmt, forStmt.Location)
    | _ -> failwith "Invalid statement: multiple or no fields set"

and convertIfStatement (raw: VerilogTypes.IfStatementT) : IfStatement =
    { Condition = convertExpression raw.Condition
      Statement = convertStatement raw.Statement
      Location = raw.Location }

and convertCaseStatement (raw: VerilogTypes.CaseStatementT) : CaseStatement =
    { Expression = convertExpression raw.Expression
      CaseItems = raw.CaseItems |> Array.map convertCaseItem
      Default = raw.Default |> Option.map convertStatement
      Location = raw.Location }

and convertCaseItem (raw: VerilogTypes.CaseItemT) : CaseItem =
    { Expressions = raw.Expressions |> Array.map convertNumber
      Statement = convertStatement raw.Statement }

and convertForStatement (raw: VerilogTypes.ForStatementT) : ForStatement =
    { Initialisation = convertAssignment raw.Initialisation Blocking
      Condition = convertExpression raw.Condition
      Step = convertAssignment raw.Step Blocking
      Statement = convertStatement raw.Statement
      Location = raw.Location }

// ====== Declaration conversion ======
let convertDeclaration (raw: VerilogTypes.DeclarationT) : Declaration =
    { DeclarationType = parseDeclarationType raw.DeclarationType
      DataType = parseDataType raw.DataType
      Range = raw.Range |> Option.map convertRange
      ArrayRanges = raw.ArrayRanges |> Option.map (Array.map convertRange)
      Variables = raw.Variables
      Location = raw.Location }


let convertIOItem (raw: VerilogTypes.IOItemT) : IOItem =
    { DeclarationType = parseDeclarationType raw.DeclarationType
      DataType = parseDataType raw.DataType
      Range = raw.Range |> Option.map convertRange
      Variables = raw.Variables
      Location = raw.Location }

let convertParameter (raw: VerilogTypes.ParameterT) : Parameter =
    { Identifier = raw.Identifier; RHS = convertExpression raw.RHS; Location = raw.Location }

let convertParameterItem (raw: VerilogTypes.ParameterItemT) : ParameterItem =
    { DeclarationType = parseDeclarationType raw.DeclarationType
      Parameters = raw.Parameters |> Array.map convertParameter }

let parseAlwaysType = function
    | "always_comb" -> AlwaysComb
    | "always_ff" -> AlwaysFF
    | s -> failwith $"Unknown always type: {s}"

let convertAlwaysConstruct (raw: VerilogTypes.AlwaysConstructT) : AlwaysConstruct =
    { AlwaysType = parseAlwaysType raw.AlwaysType
      Statement = convertStatement raw.Statement
      ClkLoc = raw.ClkLoc
      Location = raw.Location }

let parseContStatementType = function
    | "assign" -> Assign
    | "wire" -> Wire
    | s -> failwith $"Unknown continuous assignment statement type: {s}"

let convertContinuousAssign (raw: VerilogTypes.ContinuousAssignT) : ContinuousAssign =
    { StatementType = parseContStatementType raw.StatementType; Assignment = convertAssignment raw.Assignment Blocking; Location = raw.Location }

let convertNamedPortConnection (raw: VerilogTypes.NamedPortConnectionT) : NamedPortConnection =
    { PortId = raw.PortId; Primary = convertPrimary raw.Primary }
let convertOverridenParameter (raw: VerilogTypes.OverridenParameterT) : OverridenParameter =
    { Identifier = raw.Identifier; Value = convertExpression raw.Value }
let convertModuleInstantiation (raw: VerilogTypes.ModuleInstantiationT) : ModuleInstantiation =
    { Module = raw.Module
      Identifier = raw.Identifier
      Parameters = raw.Parameters |> Option.map (fun arr -> arr |> Array.map convertOverridenParameter)
      Connections = raw.Connections |> Array.map convertNamedPortConnection }

// ====== Module Item conversion ======
let convertItem (raw: VerilogTypes.ItemT) : ItemDU =
    match raw.IODecl, raw.ParamDecl, raw.Decl, raw.Statement, raw.AlwaysConstruct, raw.ModuleInstantiation with
    | Some io, None, None, None, None, None -> ItemDU.IOItem (convertIOItem io)
    | None, Some param, None, None, None, None -> ItemDU.ParamDecl (convertParameterItem param)
    | None, None, Some decl, None, None, None -> ItemDU.Decl (convertDeclaration decl)
    | None, None, None, Some stmt, None, None -> ItemDU.ContStatement (convertContinuousAssign stmt)
    | None, None, None, None, Some always, None -> ItemDU.AlwaysConstruct (convertAlwaysConstruct always)
    | None, None, None, None, None, Some modInst -> ItemDU.ModuleInstantiation (convertModuleInstantiation modInst)
    | _ -> failwith "Invalid item: multiple or no fields set"

let convertModuleItems (raw: VerilogTypes.ModuleItemsT) : ModuleItems =
    { ItemList = raw.ItemList |> Array.map convertItem; Location = 0 }

let parseModuleType = function
    | "module_old" -> ModuleOld
    | "module_new" -> ModuleNew
    | s -> failwith $"Unknown module type: {s}"

let convertModule (raw: VerilogTypes.ModuleT) : Module =
    { Type = parseModuleType raw.Type
      ModuleName = raw.ModuleName
      PortList = raw.PortList
      Locations = raw.Locations
      ModuleItems = convertModuleItems raw.ModuleItems
      EndLocation = raw.EndLocation }
      

// let convertVerilogInput (raw: VerilogTypes.VerilogInput) : Module =
//     convertModule raw.Module

///////////////////// Error handling helpers /////////////////////////

/// converts StatementT into StatementDU
// let getAlwaysStatement (s: StatementT) : StatementDU =
//     match s.BlockingAssign, s.NonBlockingAssign, s.CaseStatement, s.Conditional, s.SeqBlock with
//     | Some blocking, None, None, None, None -> StatementDU.BlockingAssign(blocking)
//     | None, Some nonblocking, None, None, None -> StatementDU.NonBlockingAssign(nonblocking)
//     | None, None, Some case, None, None -> StatementDU.Case(case)
//     | None, None, None, Some cond, None -> StatementDU.Conditional(cond)
//     | None, None, None, None, Some seqBlock -> StatementDU.SeqBlock(seqBlock)
//     | _ -> failwithf "Should not happen!"

// //maybe we can combine these two, or do smth smarter
// let statementToNode (statement:StatementDU) : ASTNode =
//     match statement with
//     | StatementDU.BlockingAssign blocking -> BlockingAssign(blocking)
//     | StatementDU.NonBlockingAssign nonblocking -> NonBlockingAssign(nonblocking)
//     | StatementDU.Case case -> Case(case)
//     | StatementDU.Conditional cond -> Conditional(cond)
//     | StatementDU.SeqBlock seqBlock -> SeqBlock(seqBlock)

let getItem (item: ItemDU) =
    match item with
    | ItemDU.IOItem io -> IOItem io
    | ItemDU.ParamDecl p -> ParamDecl p
    | ItemDU.Decl d -> Declaration d
    | ItemDU.ContStatement c -> ContStatement c
    | ItemDU.AlwaysConstruct a -> AlwaysConstruct a
    | ItemDU.ModuleInstantiation m -> ModuleInstantiation m


let primaryIdName (p: PrimaryDU) =
    match p with
    | Identifier id -> id.Name
    | IdentifierBit (id, _) -> id.Name
    | IdentifierBits (id, _, _) -> id.Name
    | VariableBitSelect (id, _) -> id.Name
    | IdentifierBitsSelect (id, _, _, _) -> id.Name
    | IdentifierArray (id, _, _, _) -> id.Name

let rec substLoopVar (loopVarName:string) (value:int) (width:int) (stmt:StatementDU) : StatementDU =
    let rec substLoopExpr (loopVarName:string) (value:int) (width:int) (expr:ExpressionDU) : ExpressionDU =
        // let substUnary (unary:UnaryT) : UnaryDU =
        let rec substUnary (unary: UnaryDU) : UnaryDU =
            match unary with
            | UnaryDU.Primary (Identifier id) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary (IdentifierBit (id, idx)) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary (VariableBitSelect (id, idx)) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary (IdentifierBits (id, start, end_)) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary (IdentifierBitsSelect (id, start, width, sel)) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary (IdentifierArray (id, indices, start, end_)) when id.Name = loopVarName ->
                UnaryDU.Number (Unsigned (value, id.Location))
            | UnaryDU.Primary p ->
                UnaryDU.Primary (substLoopPrimary loopVarName value p)
            | UnaryDU.Number _ -> unary
            | UnaryDU.Parenthesis e -> UnaryDU.Parenthesis (substLoopExpr loopVarName value width e)
            | UnaryDU.Concat e -> UnaryDU.Concat (e |> Array.map (substLoopExpr loopVarName value width))
        match expr with
        | LogicalOr (a, b) -> LogicalOr (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | LogicalAnd (a, b) -> LogicalAnd (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | BitwiseOr (a, b) -> BitwiseOr (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | BitwiseXor (a, b) -> BitwiseXor (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | BitwiseXnor (a, b) -> BitwiseXnor (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | BitwiseAnd (a, b) -> BitwiseAnd (substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | Equality (op, a, b) -> Equality (op, substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | Comparison (op, a, b) -> Comparison (op, substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | ShiftExpr (op, a, b) -> ShiftExpr (op, substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | Additive (op, a, b) -> Additive (op, substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | Multiplicative (op, a, b) -> Multiplicative (op, substLoopExpr loopVarName value width a, substLoopExpr loopVarName value width b)
        | Reduction (op, e) -> Reduction (op, substLoopExpr loopVarName value width e)
        | Negation u -> Negation (substUnary u)
        | ExpressionDU.Unary u -> ExpressionDU.Unary (substUnary u)
        | UnaryUnsigned _ -> expr
        | ConditionalOp (c, t, f) ->
            ConditionalOp (substLoopExpr loopVarName value width c, substLoopExpr loopVarName value width t, substLoopExpr loopVarName value width f)
    and substLoopPrimary (loopVarName: string) (value: int) (p: PrimaryDU) : PrimaryDU =
        match p with
        | Identifier _ -> p
        // | IdentifierBit (id, idx) -> IdentifierBit (id, substLoopExpr loopVarName value width idx)
        | IdentifierBit (id, idx) -> p
        // Only VBS should have non-constant index
        | VariableBitSelect (id, idx) when id.Name = loopVarName ->
            IdentifierBit (id, evalExpr (substLoopExpr loopVarName value width idx))
        | VariableBitSelect (id, idx) ->
            VariableBitSelect (id, substLoopExpr loopVarName value width idx)
        // | IdentifierBits (id, start, end_) ->
        //     IdentifierBits (id, substLoopExpr loopVarName value width start, substLoopExpr loopVarName value width end_)
        | IdentifierBits (id, start, end_) -> p
        | IdentifierBitsSelect (id, start, width, sel) ->
            IdentifierBitsSelect (id, substLoopExpr loopVarName value width start, width, sel)
        | IdentifierArray (id, indices, start, end_) ->
            IdentifierArray (id, indices |> Array.map (substLoopExpr loopVarName value width), start, end_)    
    // Drops any assignment whose LHS is the loop variable itself
    
    
    let isAssignToLoopVar (lhs: AssignmentLHS) =
        primaryIdName lhs.PrimaryType = loopVarName

    let substLhs (lhs: AssignmentLHS) =
        let vbs = lhs.VariableBitSelect |> Option.map (substLoopExpr loopVarName value width)
        let rewriteIndex (id: IdentifierT) idxExpr =
            let idxExpr' = substLoopExpr loopVarName value width idxExpr
            let idx = evalExpr idxExpr'
            // let idxConst = UnaryUnsigned (Unsigned (idx, id.Location))
            IdentifierBit (id, idx)

        let primary' =
            match lhs.PrimaryType with
            | Identifier id 
            | IdentifierBit (id, _) ->
                match vbs with
                | Some expr ->
                    try
                        let idx = evalExpr expr
                        IdentifierBit (id, idx)
                    with _ -> lhs.PrimaryType
                | None -> lhs.PrimaryType
            // | IdentifierBit (id, idx) ->
            //     rewriteIndex id idx
            | VariableBitSelect (id, idx) when id.Name = loopVarName ->
                rewriteIndex id idx
            | VariableBitSelect (id, idx) ->
                match vbs with
                | Some expr ->
                    try
                        let idx = evalExpr expr
                        IdentifierBit (id, idx)
                    with _ -> lhs.PrimaryType
                | None -> lhs.PrimaryType
            | IdentifierBits (id, start, end_) ->
                // let start' = substLoopExpr loopVarName value width start
                // let end' = substLoopExpr loopVarName value width end_
                IdentifierBits (id, start, end_)
            | IdentifierBitsSelect (id, start, w, sel) ->
                let start' = substLoopExpr loopVarName value width start
                IdentifierBitsSelect (id, start', w, sel)
            | IdentifierArray (id, indices, start, end_) ->
                let indices' = indices |> Array.map (substLoopExpr loopVarName value width)
                IdentifierArray (id, indices', start, end_)

        let vbs' =
            match primary' with
            | IdentifierBit _ -> None
            | _ -> vbs

        { lhs with PrimaryType = primary'; VariableBitSelect = vbs' }

    // TODO: force loop variable to be initialised outside the loop (think about implemnting int?)
    // TODO: currently in always_ff does not require initialisation (error previously?)

    // If loop variable appears on RHS, substitute it with the given value
    let substAssign (a: Assignment) =
        { a with
            LHS = substLhs a.LHS
            RHS = substLoopExpr loopVarName value width a.RHS }

    match stmt with
    | BlockingAssign (a, loc) ->
        if isAssignToLoopVar a.LHS then
            failwith "Assignments to loop variable inside loop body are not supported"
        else BlockingAssign (substAssign a, loc)
    | NonBlockingAssign (a, loc) ->
        if isAssignToLoopVar a.LHS then
            failwith "Assignments to loop variable inside loop body are not supported"
        else NonBlockingAssign (substAssign a, loc)
    | SeqBlock (stmts, loc) ->
        SeqBlock (stmts |> Array.map (substLoopVar loopVarName value width), loc)
    | Conditional (ifStmt, elseStmt, loc) ->
        let ifStmt' =
            { ifStmt with
                Condition = substLoopExpr loopVarName value width ifStmt.Condition
                Statement = substLoopVar loopVarName value width ifStmt.Statement }
        let elseStmt' = elseStmt |> Option.map (substLoopVar loopVarName value width)
        Conditional (ifStmt', elseStmt', loc)
    | StatementDU.Case (c, loc) ->
        let c' =
            { c with
                Expression = substLoopExpr loopVarName value width c.Expression
                CaseItems = c.CaseItems |> Array.map (fun ci -> { ci with Statement = substLoopVar loopVarName value width ci.Statement })
                Default = c.Default |> Option.map (substLoopVar loopVarName value width) }
        StatementDU.Case (c', loc)
    | StatementDU.ForStatement (f, loc) ->
        let f' =
            { f with
                Initialisation = { f.Initialisation with RHS = substLoopExpr loopVarName value width f.Initialisation.RHS }
                Condition = substLoopExpr loopVarName value width f.Condition
                Step = { f.Step with RHS = substLoopExpr loopVarName value width f.Step.RHS }
                Statement = substLoopVar loopVarName value width f.Statement }
        StatementDU.ForStatement (f', loc)

let rec unrollForLoops (forstmt:ForStatement) : StatementDU =
    let computeIterations startV op endV step =
        match op with
        | Lt -> endV - startV
        | Lte -> endV - startV + 1
        | Gt -> startV - endV
        | Gte -> startV - endV + 1
        | _ -> failwith "Unsupported operator"

    let startValue = evalExpr forstmt.Initialisation.RHS
    let endValue, condOp =
        match forstmt.Condition with
        | Comparison (op, _, rhs) -> evalExpr rhs, op
        | _ -> failwith "Unsupported operator in for loop condition"
    let stepValue = 
        match forstmt.Step.RHS with
        | ExpressionDU.Additive (Plus, _, stepExpr) -> evalExpr stepExpr
        | ExpressionDU.Additive (Minus, _, stepExpr) -> -evalExpr stepExpr
        | _ -> failwith "Unsupported step expression in for loop"
    let iterations = computeIterations startValue condOp endValue stepValue

    if iterations < 0 || iterations > 500 then
            failwithf "Refusing to unroll loop: iterations=%d" iterations

    // TODO: BETTER LOOP ITERATION SYSTEM

    let bodyStatements =
        match forstmt.Statement with
        | SeqBlock (stmts, _) -> stmts
        | s -> [| s |]

    let loopVarName = primaryIdName forstmt.Initialisation.LHS.PrimaryType
    let loopVarWidth =
        match forstmt.Initialisation.RHS with
        | ExpressionDU.Unary (UnaryDU.Number n) ->
            match n with
            | Unsigned (_, loc) -> 32 // default width for simple integers
            | All (bits, _, _, loc) -> bits |> int
        | _ -> failwith "Loop variable must be initialized to a constant number"

    let repeatedStmts =
        Array.init iterations (fun k -> 
            let value = startValue + k * stepValue
            bodyStatements
            |> Array.map (substLoopVar loopVarName value loopVarWidth)
            |> Array.collect (fun s ->
                match s with
                | StatementDU.ForStatement (inner, loc) ->
                    match unrollForLoops inner with
                    | SeqBlock (stmts, _) -> stmts
                    | other -> [| other |]
                | _ -> [| s |]
            )
        )
        |> Array.concat
    // let unrolled_seq_block: SeqBlockT =
    //     { Type = "seq_block"; Statements = repeatedStmts; Location = forstmt.Location }
    // failwithf "STATEMENT UNROLLED: iterations = %d, full block = %A" iterations unrolled_seq_block

    SeqBlock (repeatedStmts, forstmt.Location)

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
let rec foldAST folder state (node: ASTNode) =
    let state' = folder state node
    match node with
    | VerilogInput input ->
        foldAST folder state (Module (convertModule input.Module))
    | Module m ->
        foldAST folder state' (ModuleItems m.ModuleItems)
    | ModuleItems items ->
        items.ItemList
        |> Array.map Item
        |> Array.fold (foldAST folder) state'
    | Item item ->
        foldAST folder state' (getItem item)
    | AlwaysConstruct always ->
        foldAST folder state' (Statement always.Statement)
    | Statement stmt ->
        match stmt with
        | NonBlockingAssign (nonblocking, _) -> 
            foldAST folder state' (Assignment nonblocking)
        | BlockingAssign (blocking, _) ->
            foldAST folder state' (Assignment blocking)
        | SeqBlock (stmts, _) ->
            stmts
            |> Array.map Statement
            |> Array.fold (foldAST folder) state'
        | StatementDU.Case (case, _) -> 
            foldAST folder state' (Case case)
        | Conditional (ifStmt, elseStmt, _) ->
            let tmpState =
                IfStatement ifStmt
                |> foldAST folder state'
            match elseStmt with
            | Some elseStmt -> List.fold (foldAST folder) tmpState [Statement elseStmt]
            | _ -> tmpState
        | StatementDU.ForStatement (forstmt, _) ->
            let tmpState = 
                unrollForLoops forstmt
            foldAST folder state' (Statement tmpState)
    | Case case ->
        let newState = foldAST folder state' (Expression case.Expression)
        // let s1 = foldAST folder state' (Expression cs.Expression)
        let newState' = 
            case.CaseItems
            |> Array.map (fun item -> CaseItem(item))
            |> Array.fold (foldAST folder) newState
        match case.Default with
        | Some stmt -> foldAST folder newState' (Statement stmt)
        | _ -> newState'
    | CaseItem caseItem -> 
        let newstate =
            caseItem.Expressions
            |> Array.map (fun expr -> Number expr)
            |> Array.fold (foldAST folder) state'
        foldAST folder newstate (Statement caseItem.Statement)
    | ContStatement assign ->
        foldAST folder state' (Assignment assign.Assignment)
    | Assignment assign ->
        (foldAST folder state' (AssignmentLHS assign.LHS), Expression assign.RHS)
        ||> foldAST folder 
    | IfStatement ifstmt ->
        (foldAST folder state' (Expression ifstmt.Condition), Statement ifstmt.Statement)
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
let getAllExpressions' (expressions: List<ExpressionDU>) (node: ASTNode) =
    match node with
    | Expression expr ->
        expressions @ [expr]
    | _ -> expressions

let getNumbers (numbers) (node) =
    match node with
    | Number num -> numbers @ [num]
    | _ -> numbers

let getAssignments' (assignments: List<Assignment>) (node: ASTNode) =
    match node with
    | Assignment assign ->
        assignments @ [assign]
    | _ -> assignments

let getContAssignments (assignments: List<Assignment>) (node: ASTNode) =
    match node with
    | ContStatement contStmt -> assignments @ [contStmt.Assignment]
    | _ -> assignments

let getAlwaysAssignments (assignments: List<Assignment>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match stmt with
        | BlockingAssign (blocking, _) -> assignments @ [blocking]
        | NonBlockingAssign (nonblocking, _) -> assignments @ [nonblocking]
        | _ -> assignments
    | _ -> assignments

let assignmentLocation (a: Assignment) =
    match a.LHS.PrimaryType with
    | Identifier id -> id.Location
    | IdentifierBit (id, _) -> id.Location
    | IdentifierBits (id, _, _) -> id.Location
    | VariableBitSelect (id, _) -> id.Location
    | IdentifierBitsSelect (id, _, _, _) -> id.Location
    | IdentifierArray (id, _, _, _) -> id.Location

let getAssignmentsWithLocations (assignments: List<Assignment*int>) (node: ASTNode) =
    match node with
    | ModuleItems items ->
        let contAssigns =
            items.ItemList
            |> Array.toList
            |> List.choose (function
                | ItemDU.ContStatement c -> Some (c.Assignment, c.Location)
                | _ -> None)
        assignments @ contAssigns
    | Statement stmt ->
        match stmt with
        | BlockingAssign (a, _) -> assignments @ [a, assignmentLocation a]
        | NonBlockingAssign (a, _) -> assignments @ [a, assignmentLocation a]
        | _ -> assignments
    | _ -> assignments
let getAlwaysBlocks (alwaysBlocks: List<AlwaysConstruct>) (node: ASTNode) =
    match node with
    | AlwaysConstruct alwaysBlock -> alwaysBlocks @ [alwaysBlock]
    | _ -> alwaysBlocks

let getBlockingAssignmentsWithLocation (assignments: List<Assignment*int>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match stmt with
        | BlockingAssign (a, loc) -> assignments @ [a, loc]
        | _ -> assignments
    | _ -> assignments

let getNonBlockingAssignmentsWithLocation (assignments: List<Assignment*int>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match stmt with
        | NonBlockingAssign (a, loc) -> assignments @ [a, loc]
        | _ -> assignments
    | _ -> assignments

let getBlockingAssignments (assignments: List<Assignment>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match stmt with
        | BlockingAssign (a, _) -> assignments @ [a]
        | _ -> assignments
    | _ -> assignments

let getNonBlockingAssignments (assignments: List<Assignment>) (node: ASTNode) =
    match node with
    | Statement stmt ->
        match stmt with
        | NonBlockingAssign (a, _) -> assignments @ [a]
        | _ -> assignments
    | _ -> assignments
