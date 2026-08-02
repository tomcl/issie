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
        | VariableBitSelect of id: IdentifierT * index: ExpressionDU
        | IdentifierBitsSelect of id: IdentifierT * start: ExpressionDU * width: int * select: SelectDU
        | IdentifierArray of id: IdentifierT * indices: ArraySelect array * start: int * end_: int
        | VariableArrayBitSel of id: IdentifierT * indices: ArraySelect array * index: ExpressionDU
    
    and ArraySelect =
        | ConstArraySelect of int
        | VarArraySelect of ExpressionDU

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
        | ParamNumber of PrimaryDU * bits: string

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

type ModuleItems = {ItemList: ItemDU array;}

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
    // | SeqBlock of StatementDU array 
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
        | None -> failwithf "Only parameters allowed to be used in width expressions. '%s' is not a parameter or is an undefined parameter." id.Name
    
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
    | "param_number" -> UnaryDU.ParamNumber ((convertPrimary (Option.get raw.ParamNumber).Primary), (Option.get raw.ParamNumber).Bits)
    | s -> failwith $"Unknown unary type: {s}"

and convertPrimary (raw: PrimaryT) : PrimaryDU =
    // printfn "converting pri: %A" raw
    match raw.PrimaryType with
    | "identifier" -> 
        Identifier raw.Primary
    | "identifier_bit" ->
        // Single bit select: x[1]
        let idx = int (raw.BitsStart |> Option.map int |> Option.get)
        IdentifierBit (raw.Primary, idx)
    | "identifier_bit2" ->
        let idx = convertExpression (Option.get raw.Expression)
        let idxInt = 
            try 
                Some (evalExpr idx)
            with _ -> None
        match idxInt with
        | Some i -> IdentifierBit (raw.Primary, i)
        | None -> VariableBitSelect (raw.Primary, idx)
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
        // Array access: x[i][0]...
        let indices = 
            raw.ArrayIndices 
            |> Option.defaultValue [||] 
            |> Array.map convertArraySelect
        let start = int (raw.BitsStart |> Option.map int |> Option.get)
        let end_ = int (raw.BitsEnd |> Option.map int |> Option.get)
        // let start = ExpressionDU.Unary (UnaryDU.Number (Unsigned (int (raw.BitsStart |> Option.map int |> Option.get), 0)))
        // let end_ = ExpressionDU.Unary (UnaryDU.Number (Unsigned (int (raw.BitsEnd |> Option.map int |> Option.get), 0)))
        IdentifierArray (raw.Primary, indices, start, end_)
    | "identifier_array2" ->
        // Array access: x[i][j]...
        let indices = 
            raw.ArrayIndices 
            |> Option.defaultValue [||] 
            |> Array.map convertArraySelect
        
        let idx = convertExpression (Option.get raw.Expression)
        let idxInt = 
            try 
                Some (evalExpr idx)
            with _ -> None
        match idxInt with
        | Some i -> IdentifierArray (raw.Primary, indices, i, i)
        | None -> VariableArrayBitSel (raw.Primary, indices, idx)

        // let idx = convertExpression (Option.get raw.Expression)
        // VariableArrayBitSel (raw.Primary, indices, idx)
    | s -> failwith $"Unknown primary type: {s}"

and convertArraySelect (raw: ArraySelectT) : ArraySelect =
    // printfn "converting array: %A" raw
    match raw.ArrayType with
    | "const_array" -> ConstArraySelect (int (raw.WordSelect |> Option.get))
    | "var_array" -> VarArraySelect (convertExpression (raw.VariableArraySelect |> Option.get))
    | s -> failwith $"Unknown array type: {s}"

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

    let priType = convertPrimary prim;
    match priType with
    | VariableBitSelect _ -> 
        { PrimaryType = convertPrimary prim; VariableBitSelect = raw.VariableBitSelect |> Option.map convertExpression }
    | _ -> { PrimaryType = convertPrimary prim; VariableBitSelect = None }


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
    // printfn "converting item: %A" raw
    match raw.IODecl, raw.ParamDecl, raw.Decl, raw.Statement, raw.AlwaysConstruct, raw.ModuleInstantiation with
    | Some io, None, None, None, None, None -> ItemDU.IOItem (convertIOItem io)
    | None, Some param, None, None, None, None -> ItemDU.ParamDecl (convertParameterItem param)
    | None, None, Some decl, None, None, None -> ItemDU.Decl (convertDeclaration decl)
    | None, None, None, Some stmt, None, None -> ItemDU.ContStatement (convertContinuousAssign stmt)
    | None, None, None, None, Some always, None -> ItemDU.AlwaysConstruct (convertAlwaysConstruct always)
    | None, None, None, None, None, Some modInst -> ItemDU.ModuleInstantiation (convertModuleInstantiation modInst)
    | _ -> failwith "Invalid item: multiple or no fields set"

let convertModuleItems (raw: VerilogTypes.ModuleItemsT) : ModuleItems =
    { ItemList = raw.ItemList |> Array.map convertItem;}

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

/// Helper functions to extract details from primaries
let getPrimaryName (p: PrimaryDU) =
    match p with
    | Identifier id
    | IdentifierBit (id, _)
    | VariableBitSelect (id, _)
    | IdentifierBits (id, _, _)
    | IdentifierBitsSelect (id, _, _, _)
    | IdentifierArray (id, _, _, _)
    | VariableArrayBitSel (id, _, _) -> id.Name

    
let getPrimaryLocation (p: PrimaryDU) =
    match p with
    | Identifier id
    | IdentifierBit (id, _)
    | VariableBitSelect (id, _)
    | IdentifierBits (id, _, _)
    | IdentifierBitsSelect (id, _, _, _)
    | IdentifierArray (id, _, _, _) 
    | VariableArrayBitSel (id, _, _) -> id.Location

let getPrimaryRange (p: PrimaryDU) paramMap =
    match p with
    | Identifier _ -> None
    | IdentifierArray _ -> None
    | VariableBitSelect (_, idx) -> 
        try
            let idxVal = evalExprWithParams idx paramMap
            Some (idxVal, idxVal)
        with
        | _ -> None
    | VariableArrayBitSel (_, _, idx) ->
        try
            let idxVal = evalExprWithParams idx paramMap
            Some (idxVal, idxVal)
        with
        | _ -> None
    | IdentifierBit (_, idx) ->
        Some (idx, idx)
    | IdentifierBits (_, start, end_) ->
        Some (start, end_)
    | IdentifierBitsSelect (_, start, width, sel) ->
        let bStart = evalExpr start
        let bEnd =
            match sel with
            | PlusWidth -> bStart + width - 1
            | MinusWidth -> bStart - width + 1
        Some (bStart, bEnd)
    | ParamNumber (p, width) ->
        let pVal = evalExprWithParams (ExpressionDU.Unary p) paramMap
        Some (pVal, pVal + width - 1)

let rec substLoopVar (loopVarName:string) (value:int) (width:int) (stmt:StatementDU) : StatementDU =
    let rec substLoopExpr (loopVarName:string) (value:int) (width:int) (expr:ExpressionDU) : ExpressionDU =
        let rec substUnary (unary: UnaryDU) : UnaryDU =
            match unary with
            | UnaryDU.Primary (Identifier id) 
            | UnaryDU.Primary (IdentifierBit (id, _)) 
            | UnaryDU.Primary (VariableBitSelect (id, _))            
            | UnaryDU.Primary (IdentifierBits (id, _, _))            
            | UnaryDU.Primary (IdentifierBitsSelect (id, _, _, _)) 
            | UnaryDU.Primary (IdentifierArray (id, _, _, _)) 
            | UnaryDU.Primary (VariableArrayBitSel (id, _, _)) when id.Name = loopVarName ->
                UnaryDU.Number (All (width, Decimal, string value, id.Location))
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
        | IdentifierBit (id, idx) -> p
        // Only VBS should have non-constant index
        | VariableBitSelect (id, idx) when id.Name = loopVarName ->
            IdentifierBit (id, evalExpr (substLoopExpr loopVarName value width idx))
        | VariableBitSelect (id, idx) ->
            VariableBitSelect (id, substLoopExpr loopVarName value width idx)
        | IdentifierBits (id, start, end_) -> p
        | IdentifierBitsSelect (id, start, width, sel) ->
            IdentifierBitsSelect (id, substLoopExpr loopVarName value width start, width, sel)
        | IdentifierArray (id, indices, start, end_) ->
            IdentifierArray (id, indices |> Array.map (substLoopArraySel loopVarName value), start, end_)  
        | VariableArrayBitSel (id, indices, idx) ->
            VariableArrayBitSel (id, indices |> Array.map (substLoopArraySel loopVarName value), substLoopExpr loopVarName value width idx)
    and substLoopArraySel (loopVarName: string) (value: int) (a: ArraySelect) : ArraySelect =
        match a with
        | ConstArraySelect _ -> a
        | VarArraySelect v -> VarArraySelect (substLoopExpr loopVarName value width v)
    
    // Drops any assignment whose LHS is the loop variable itself
    let isAssignToLoopVar (lhs: AssignmentLHS) =
        getPrimaryName lhs.PrimaryType = loopVarName

    let substLhs (lhs: AssignmentLHS) =
        let rewriteIndex (id: IdentifierT) idxExpr =
            let idxExpr' = substLoopExpr loopVarName value width idxExpr
            let idx = evalExpr idxExpr'
            IdentifierBit (id, idx)

        let loopVarBool = isAssignToLoopVar lhs
        let vbs = lhs.VariableBitSelect |> Option.map (substLoopExpr loopVarName value width)
        let primary' =
            match loopVarBool with
            | true -> 
                lhs.PrimaryType // error handling will flag
            | false -> 

            // let primary' =
                match lhs.PrimaryType with
                | Identifier id 
                | IdentifierBit (id, _) -> lhs.PrimaryType
                | VariableBitSelect (id, idx) when id.Name = loopVarName ->
                    rewriteIndex id idx
                | VariableBitSelect (id, idx) ->
                    match vbs with
                    | Some expr ->
                        try
                            let idx = evalExpr expr
                            IdentifierBit (id, idx)
                        with _ -> VariableBitSelect (id, expr)
                    | None -> lhs.PrimaryType
                | IdentifierBits (id, start, end_) ->
                    IdentifierBits (id, start, end_)
                | IdentifierBitsSelect (id, start, w, sel) ->
                    let start' = substLoopExpr loopVarName value width start
                    IdentifierBitsSelect (id, start', w, sel)
                | IdentifierArray (id, indices, start, end_) ->
                    let indices' = indices |> Array.map (substLoopArraySel loopVarName value)
                    IdentifierArray (id, indices', start, end_)
                | VariableArrayBitSel (id, indices, idx) ->
                    VariableArrayBitSel (id, indices |> Array.map (substLoopArraySel loopVarName value), substLoopExpr loopVarName value width idx)

        let vbs' =
            match primary' with
            | VariableBitSelect _ 
            | VariableArrayBitSel _ -> vbs
            | _ -> None

        // printf "substituted lhs %A" lhs
        { lhs with PrimaryType = primary'; VariableBitSelect = vbs' }

    // TODO: allow loop variable to be initialised inside the loop decl (this is part of system verilog)


    // If loop variable appears on RHS, substitute it with the given value
    let substAssign (a: Assignment) =
        { a with
            LHS = substLhs a.LHS
            RHS = substLoopExpr loopVarName value width a.RHS }

    match stmt with
    | BlockingAssign (a, loc) ->
        BlockingAssign (substAssign a, loc)
    | NonBlockingAssign (a, loc) ->
        NonBlockingAssign (substAssign a, loc)
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
        let startV, endV, step = float(startV), float (endV), float(step)
        match op with
        | Lt -> int (floor((endV - startV) / step))
        | Lte -> int (floor((endV - startV + 1.0) / step))
        | Gt -> int (floor((startV - endV) / step))
        | Gte -> int (floor((startV - endV + 1.0) / step))
        | _ -> failwith "Shouldn't happen: error check should catch unsupported operators in for loop condition"
    
    let startValue = evalExpr forstmt.Initialisation.RHS
    let endValue, condOp =
        match forstmt.Condition with
        | Comparison (op, _, rhs) -> evalExpr rhs, op
        | _ -> failwith "Shouldn't happen: error check should catch unsupported operators in for loop condition"

    let stepValue = 
        match forstmt.Step.RHS with
        | ExpressionDU.Additive (Plus, _, stepExpr) -> evalExpr stepExpr
        | ExpressionDU.Additive (Minus, _, stepExpr) -> -evalExpr stepExpr
        | _ -> failwith "Shouldn't happen: error check should catch unsupported step expressions in for loop"
    let iterations = computeIterations startValue condOp endValue stepValue

    let bodyStatements =
        match forstmt.Statement with
        | StatementDU.SeqBlock (stmts, _) -> stmts
        | s -> [| s |]

    let loopVarName = getPrimaryName forstmt.Initialisation.LHS.PrimaryType
    let loopVarWidth =
        match forstmt.Initialisation.RHS with
        | ExpressionDU.Unary (UnaryDU.Number n) ->
            match n with
            | Unsigned (_, loc) -> 32 // default width for simple integers
            | All (bits, _, _, loc) -> bits |> int
        | _ -> failwith "Loop variable must be initialized to a constant number"

    // let repeatedStmts =
    //     Array.init iterations (fun k -> 
    //         let value = startValue + k * stepValue
    //         bodyStatements
    //         |> Array.map (substLoopVar loopVarName value loopVarWidth)
    //         // |> Array.collect (fun s ->
    //         //     match s with
    //         //     | StatementDU.ForStatement (inner, loc) ->
    //         //         match unrollForLoops inner with
    //         //         | SeqBlock (stmts, _) -> stmts
    //         //         | other -> [| other |]
    //         //     | _ -> [| s |]
    //         // )
    //     )
    //     |> Array.concat
    let repeatedStmts =
        Array.init iterations (fun k ->
            let value = startValue + k * stepValue
            bodyStatements
            |> Array.map (substLoopVar loopVarName value loopVarWidth)
        )
        |> Array.concat
    // let unrolled_seq_block: SeqBlockT =
    //     { Type = "seq_block"; Statements = repeatedStmts; Location = forstmt.Location }
    // failwithf "STATEMENT UNROLLED: iterations = %d, full block = %A" iterations unrolled_seq_block

    StatementDU.SeqBlock (repeatedStmts, forstmt.Location)

// let rec unrollAST node =
//     match node with
//     | ForStatement forstmt -> unrollForLoops forstmt
//     | _ -> // can either just list all or find better way to do this

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
    | IOItem item -> 
        match item.Range with
        | Some r -> foldAST folder state' (Range r)
        | None -> state'
    | Declaration decl ->
        let tmpState = 
            match decl.Range with
            | Some r -> foldAST folder state' (Range r)
            | None -> state'
        let tmpState' =
            match decl.ArrayRanges with
            | Some ar ->
                ar
                |> Array.map (fun r -> Range r)
                |> Array.fold (foldAST folder) tmpState
            | None -> tmpState
        tmpState'
    | Range range ->
        (foldAST folder state' (Expression range.Start), Expression range.End)
        ||> foldAST folder
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
            // let tmpState = 
            //     unrollForLoops forstmt
            // foldAST folder state' (Statement tmpState)
            let tmpState = 
                Assignment forstmt.Initialisation
                |> foldAST folder state'
            let tmpState' =
                List.fold (foldAST folder) tmpState [Assignment forstmt.Step]
            // let tmpState'' = List.fold (foldAST folder) tmpState' [Statement forstmt.Statement]
            let unrolledForStmt = unrollForLoops forstmt
            List.fold (foldAST folder) tmpState' [Statement unrolledForStmt]
            // foldAST folder state' (Statement forstmt)
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
    // | ForStatement forstmt ->
    //     let tmpState = 
    //         unrollForLoops forstmt
    //     foldAST folder state' (Statement(tmpState))
    | AssignmentLHS lhs ->
        match lhs.VariableBitSelect with
        | Some expr -> 
            (foldAST folder state' (Expression expr), Primary lhs.PrimaryType)
            ||> foldAST folder
            // foldAST folder state' (Expression(expr))
        | _ -> foldAST folder state' (Primary lhs.PrimaryType)
    | Expression expr -> 
        match expr with
        | Negation unary
        | ExpressionDU.Unary unary ->
            foldAST folder state' (Unary(unary))
        | UnaryUnsigned _ -> state'
        | LogicalOr (e1, e2)
        | LogicalAnd (e1, e2)
        | BitwiseOr (e1, e2)
        | BitwiseXor (e1, e2)
        | BitwiseXnor (e1, e2)
        | BitwiseAnd (e1, e2)
        | Equality (_, e1, e2)
        | Comparison (_, e1, e2)
        | ShiftExpr (_, e1, e2)
        | Additive (_, e1, e2)
        | Multiplicative (_, e1, e2) -> 
            (foldAST folder state' (Expression e1), Expression e2)
            ||> foldAST folder
        | Reduction (_, e) -> foldAST folder state' (Expression e)
        | ConditionalOp (cond, ifTrue, ifFalse) ->
            let tmpState =
                Expression cond
                |> foldAST folder state'
            let tmpState' =
                List.fold (foldAST folder) tmpState [Expression ifTrue]
            List.fold (foldAST folder) tmpState' [Expression ifFalse]
    | Unary unary ->
        match unary with
        | UnaryDU.Primary p -> foldAST folder state' (Primary(p))
        | Parenthesis e -> foldAST folder state' (Expression e)
        | ParamNumber (p, _) -> foldAST folder state' (Primary(p))
        | _ -> state'
    | _ ->
        state'

// Applies foldAST to fully unrolled AST
// let rec foldASTUnrolled folder state (node: ASTNode) =
//     let state' = folder state node
//     match node with
//     | Statement (StatementDU.ForStatement (forstmt, _)) ->
//         let unrolled = unrollForLoops forstmt
//         foldASTUnrolled folder state' (Statement unrolled)
//     | _ ->

let rec foldParams (paramMap: Map<string, int>) (node: ASTNode) : ASTNode =
    printfn "node %A" node
    match node with
    | Primary pri ->
        let name = getPrimaryName pri
        let loc = getPrimaryLocation pri
        match Map.tryFind name paramMap with
        | Some p -> Number (Unsigned (p, loc))
        | None -> node
    | Expression expr -> node
        // let rec strip (e: ExpressionDU) =
        //     match e with
        //     | ExpressionDU.Unary (Parenthesis e) -> strip e
        //     | _ -> e
        // let expr = strip expr

        // let newExpr = 
        //     match expr with
        //     | ExpressionDU.UnaryUnsigned n -> 
        //         match foldParams paramMap (Number n) with 
        //         | Number n -> UnaryUnsigned n
        //         | _ -> failwithf "Shouldn't happen, expeted Number"
        //     | ExpressionDU.Negation (UnaryDU.Number n) -> 
        //         match foldParams paramMap (Number n) with
        //         | Unary u -> Negation u
        //         | _ -> failwithf "Shouldn't happen, expected unary"
        //     | ExpressionDU.Unary (UnaryDU.Number n) -> 
        //         match foldParams paramMap (Number n) with
        //         | Unary u -> UnaryDU.Number u
        //         | _ -> failwithf "Shouldn't happen, expected unary"

        //     | ExpressionDU.Unary (UnaryDU.Primary (Identifier id)) ->
        //         match Map.tryFind id.Name paramMap with
        //         | Some v -> v
        //         | None -> failwithf "Only parameters allowed to be used in width expressions. '%s' is not a parameter or is an undefined parameter." id.Name
            
        //     | Additive (op, lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         match op with
        //         | Plus -> l + r
        //         | Minus -> l - r
        //         | _ -> failwith "Unsupported additive operator for parameter evaluation"
        //     | Multiplicative (op, lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         match op with
        //         | Mult -> l * r
        //         | _ -> failwith "Unsupported multiplicative operator for parameter evaluation"
        //     | ShiftExpr (op, lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         match op with
        //         | Sll -> l <<< r
        //         | Srl -> l >>> r
        //         | Sra -> l >>> r 
        //         | _ -> failwith "Unsupported shift operator for parameter evaluation"
        //     | BitwiseAnd (lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         l &&& r
        //     | BitwiseOr (lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         l ||| r
        //     | BitwiseXor (lhs, rhs) ->
        //         let l = evalExprWithParams lhs paramMap
        //         let r = evalExprWithParams rhs paramMap
        //         l ^^^ r
        
        // Expression newExpr
    // | Unary of UnaryDU
    | VerilogInput input ->
        foldParams paramMap (Module (convertModule input.Module)) 
    | Module m ->
        foldParams paramMap (ModuleItems m.ModuleItems) 
    | ModuleItems items ->
        let newItems =
            items.ItemList
            |> Array.map (fun item ->
                printfn "item %A" item
                match foldParams paramMap (Item item) with
                | IOItem i -> ItemDU.IOItem i
                | ParamDecl i -> ItemDU.ParamDecl i
                | Declaration i -> ItemDU.Decl i
                | ContStatement i -> ItemDU.ContStatement i
                | AlwaysConstruct i -> ItemDU.AlwaysConstruct i
                | ModuleInstantiation i -> ItemDU.ModuleInstantiation i
                | _ -> failwithf "Shouldn't happen, expected item"
            )
        ModuleItems { items with ItemList = newItems }
    | Item item ->
        foldParams paramMap (getItem item)
    | AlwaysConstruct always ->
        foldParams paramMap (Statement always.Statement)
    | Statement stmt ->
        match stmt with
        | NonBlockingAssign (nonblocking, _) -> 
            foldParams paramMap (Assignment nonblocking)
        | BlockingAssign (blocking, _) ->
            foldParams paramMap (Assignment blocking)
        | SeqBlock (stmts, locs) ->
            let newStmts =
                stmts
                |> Array.map (fun stmt ->
                    match foldParams paramMap (Statement stmt) with
                    | Statement s -> s
                    | x -> failwithf "Shouldn't happen, expected Statement")
            Statement (SeqBlock (newStmts, locs))
        | StatementDU.Case (case, _) -> 
            foldParams paramMap (Case case)
        | Conditional (ifStmt, elseStmt, loc) ->
            let newIfStmt =
                match foldParams paramMap (IfStatement ifStmt) with
                | IfStatement s -> s
                | _ -> failwith "Expected IfStatement"
            let newElseStmt =
                elseStmt
                |> Option.map (fun s ->
                    match foldParams paramMap (Statement s) with
                    | Statement s' -> s'
                    | _ -> failwith "Expected Statement")
            Statement (Conditional (newIfStmt, newElseStmt, loc))
        | StatementDU.ForStatement (forstmt, loc) ->
            let newInit =
                match foldParams paramMap (Assignment forstmt.Initialisation) with
                | Assignment a -> a
                | _ -> failwith "Expected Assignment"
            let newCond =
                match foldParams paramMap (Expression forstmt.Condition) with
                | Expression e -> e
                | _ -> failwith "Expected Expression"
            let newStep =
                match foldParams paramMap (Assignment forstmt.Step) with
                | Assignment a -> a
                | _ -> failwith "Expected Assignment"
            let newStmt =
                match foldParams paramMap (Statement forstmt.Statement) with
                | Statement s -> s
                | _ -> failwith "Expected Statement"
            let forstmt = {Initialisation = newInit; Condition = newCond; Step = newStep; Statement = newStmt; Location = loc}
            Statement (StatementDU.ForStatement (forstmt, loc))
    | Case case ->
        let caseExpr = 
            match foldParams paramMap (Expression case.Expression) with
            | Expression e -> e
            | _ -> failwith "Expected Expression"
        let caseItems = 
            case.CaseItems
            |> Array.map (fun item -> 
                match foldParams paramMap (CaseItem item) with
                | CaseItem c -> c
                | _ -> failwithf "Expected caseitem"
            )
        let caseDefault = 
            match case.Default with
            | Some stmt -> 
                Some (foldParams paramMap (Statement stmt))
            | _ -> None
        let caseStmt =
            match caseDefault with
            | Some (Statement d) -> 
                {Expression = caseExpr; CaseItems = caseItems; Default = Some d; Location = case.Location}
            | _ -> failwithf "Expected stmt"
        Case caseStmt
    | CaseItem caseItem -> 
        let newStmt =  
            match foldParams paramMap (Statement caseItem.Statement) with
            | Statement s -> s
            | x -> failwithf "Shouldn't happen, expected Statement"
        let caseItem = {Expressions = caseItem.Expressions; Statement = newStmt}
        CaseItem caseItem
    | ContStatement assign ->
        foldParams paramMap (Assignment assign.Assignment)
    | Assignment assign ->
        let newLhs =
            match foldParams paramMap (AssignmentLHS assign.LHS) with
            | AssignmentLHS lhs -> lhs
            | _ -> failwith "Expected AssignmentLHS"
        let newRhs =
            match foldParams paramMap (Expression assign.RHS) with
            | Expression e -> e
            | _ -> failwith "Expected Expression"
        let assign = {Type = assign.Type; LHS = newLhs; RHS = newRhs}
        Assignment assign
    | IfStatement ifstmt ->
        let newCond =
            match foldParams paramMap (Expression ifstmt.Condition) with
            | Expression e -> e
            | _ -> failwith "Expected Expression"
        let newStmt =
            match foldParams paramMap (Statement ifstmt.Statement) with
            | Statement s -> s
            | _ -> failwith "Expected Statement"
        let ifstmt = {Condition = newCond; Statement = newStmt; Location = ifstmt.Location}
        IfStatement ifstmt
    | AssignmentLHS lhs ->
        match lhs.VariableBitSelect with
        | Some expr -> 
            foldParams paramMap (Expression(expr))
        | _ -> node
    | _ -> node

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
    // | ForStatement forstmt ->
    //     assignments @ [forstmt.Initialisation; forstmt.Step]
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
    | VariableArrayBitSel (id, _, _) -> id.Location
    

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
        | StatementDU.ForStatement (forstmt, loc) ->
            let initAssign = forstmt.Initialisation, assignmentLocation forstmt.Initialisation
            let stepAssign = forstmt.Step, assignmentLocation forstmt.Step
            assignments @ [initAssign; stepAssign]
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
