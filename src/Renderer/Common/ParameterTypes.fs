module ParameterTypes

open System.Text.RegularExpressions

//------------------------------------------------------------------------------------------------//
//----Types for Parameters defined on sheets and bound to values by custom component instances----//
//------------------------------------------------------------------------------------------------//

/// Probably needs to be bigint eventually to deal with the value of an N bit constant for n > 32.
// There should be no problem doing that but to get started let us use int and move to bigint later when needed.
type ParamInt = int

/// A named parameter in a custom component type
/// For MVP this is ok but maybe names need to be qualified by the
/// design sheet they are in to make functions support parameter inheritance.
type ParamName = ParamName of string

/// An arithmetic expression containing symbolic parameters
/// For MVP this could be limited to PInt and PParameter only.
/// However, it would be useful to have a more general type definition so that
/// functions that manipulate constraints, parameters, etc can be written in a more general way.
/// The actual parameter value is customisable so that the same code can be used for int parameters (normal)
/// and BigInt parameters (needed for constant values in N bit components).
/// For MVP set 'PINT = int
/// TODO: refactor this to use an enumeration DU for operators to reduce cases.
type ParamExpression =
    | PInt of ParamInt
    | PParameter of ParamName
    | PAdd of ParamExpression * ParamExpression
    | PSubtract of ParamExpression * ParamExpression
    | PMultiply of ParamExpression * ParamExpression
    | PDivide of ParamExpression * ParamExpression
    | PRemainder of ParamExpression * ParamExpression

type ParamError = string

/// For MVP could allow only PInt case constraints
/// The Errors are human-readable explanations of why violating the constraint is not allowed.
/// They should if possible be component-specific "constant MyConstName is 3 bit width so not allowed to be less than -4".
type ParamConstraint =
    | MinVal of ParamExpression * ParamError
    | MaxVal of ParamExpression * ParamError

/// A string marking a specific integer value in a case of ComponentType.
/// The values here are arbitrary and ComponentType-case specific and all that matters is that each value is unique
/// within the case.
type CompSlotName =
    | Buswidth
    | NGateInputs
    | IO of Label: string
    | CustomCompParam of ParamName: string // TODO: implement this case

/// A slot in a component instance that can be bound to a parameter expression
/// CompId should be a ComponentId but then we would need these types to be defined after CommonTypes.
/// That is not possible, because we will wnat to modify CommonTypes types to use these!
/// eventually these types can be folded into CommonTypes, and that could if need be be made recursive so
/// solving the problem.
/// In practice this is OK because ParamSlot is strongly typed and we will not be likely to confused CompID with any
/// other string.
type ParamSlot = {CompId: string; CompSlot: CompSlotName}

/// Lenses for ParamSlot
let compId_ = Optics.Lens.create (fun s -> s.CompId) (fun v s -> {s with CompId = v})
let compSlot_ = Optics.Lens.create (fun s -> s.CompSlot) (fun v s -> {s with CompSlot = v})

/// A parameter expression and its corresponding constraints
type ConstrainedExpr = {
    Expression: ParamExpression
    Constraints: ParamConstraint list
}

/// Lenses for ConstrainedExpr
let expression_ = Optics.Lens.create (fun s -> s.Expression) (fun v s -> {s with Expression = v})
let constraints_ = Optics.Lens.create (fun s -> s.Constraints) (fun v s -> {s with Constraints = v})

/// Data for a new parameterised slot being created
type NewParamCompSpec = {
    CompSlot: CompSlotName
    Expression: ParamExpression
    Constraints: ParamConstraint list
    Value: ParamInt
}

/// Lenses for NewParamCompSpec
let newParamCompSlot_ = Optics.Lens.create (fun s -> s.CompSlot) (fun v s -> {s with CompSlot = v})
let newParamExpression_ = Optics.Lens.create (fun s -> s.Expression) (fun v s -> {s with Expression = v})
let newParamConstraints_ = Optics.Lens.create (fun s -> s.Constraints) (fun v s -> {s with Constraints = v})
let newParamValue_ = Optics.Lens.create (fun s -> s.Value) (fun v s -> {s with Value = v})

/// The Elmish Model state used to manage input boxes that can be used to define parameter expressions.
/// Part of Model.PopupDialogData.DialogState.
type ParamBoxDialogState = Map<CompSlotName, Result<NewParamCompSpec, ParamError>>

/// Map from name to expression for each parameter
type ParamBindings = Map<ParamName, ParamExpression>


/// For Part A: alternatively you could store slot information in the component record
/// as an extra field.
/// This field should store all the Component's slot information where slots are bound to parameters.
type ComponentSlotExpr = Map<ParamSlot, ConstrainedExpr>

/// The state used per design sheet to define integer slots
/// that have values defined with parameter expressions
/// LoadedComponent.LCParameterSlots
/// (also used in SheetInfo - to save / load files - but the LoadedComponent field is the only one used by HLP Teams)
type ParameterDefs = {
    DefaultBindings: ParamBindings
    ParamSlots: ComponentSlotExpr
}

/// Lenses for ParamDefs
let defaultBindings_ = Optics.Lens.create (fun s -> s.DefaultBindings) (fun v s -> {s with DefaultBindings = v})
let paramSlots_ = Optics.Lens.create (fun s -> s.ParamSlots) (fun v s -> {s with ParamSlots = v})

/// <summary>
/// Evaluates a parameter expression given a set of parameter bindings.
/// </summary>
/// <param name="paramBindings">Map from parameter names to their bound expressions</param>
/// <param name="paramExpr">The parameter expression to evaluate</param>
/// <returns>
/// Success: The evaluated integer value if all parameters can be resolved to constants
/// Error: A human-readable error message listing any unresolved parameters
/// </returns>
/// <remarks>
/// This function recursively evaluates the expression tree, substituting parameter values 
/// from the bindings and performing arithmetic operations. Parameters are resolved to their 
/// bound expressions, which are then recursively evaluated. If any parameters remain 
/// unresolved after full evaluation, an error is returned listing them.
/// </remarks>
let evaluateParamExpression (paramBindings: ParamBindings) (paramExpr: ParamExpression) : Result<ParamInt, ParamError> =
    // Recursively evaluate the expression, substituting parameters and simplifying arithmetic
    let rec recursiveEvaluation (expr: ParamExpression) : ParamExpression =
        match expr with
        | PInt _ -> expr // constant, nothing needs to be changed
        | PParameter name -> 
            match Map.tryFind name paramBindings with
            | Some evaluated -> evaluated
            | None -> PParameter name
        | PAdd (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l+r)
            | newLeft, newRight -> PAdd (newLeft, newRight) // keep as PAdd type
        | PSubtract (left, right) -> 
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l-r)
            | newLeft, newRight -> PSubtract (newLeft, newRight) // keep as Psubtract type
        | PMultiply (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l*r)
            | newLeft, newRight -> PMultiply (newLeft, newRight)
        | PDivide (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l/r)
            | newLeft, newRight -> PDivide (newLeft, newRight)
        | PRemainder (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l%r)
            | newLeft, newRight -> PRemainder (newLeft, newRight)
        
    
    let unwrapParamName (ParamName name) = name
    
    let rec collectUnresolved expr =
        match expr with
        | PInt _ -> []
        | PParameter name -> [unwrapParamName name]  // Only collect unresolved parameters
        | PAdd (left, right) 
        | PSubtract (left, right) 
        | PMultiply (left, right)
        | PDivide (left, right) 
        | PRemainder (left, right) ->
            collectUnresolved left @ collectUnresolved right

    match recursiveEvaluation paramExpr with
    | PInt evaluated -> Ok evaluated
    | unresolvedExpr ->
        let unresolvedParams = collectUnresolved unresolvedExpr |> List.distinct
        match unresolvedParams with
        | [] -> failwithf "Unexpected error: non-constant expression with no unresolved parameters"
        | [single] -> Error $"Parameter '{single}' is not defined"
        | multiple -> 
            let paramList = multiple |> String.concat ", "
            Error $"Parameters {paramList} are not defined"

/// <summary>
/// Converts a parameter expression to its string representation with proper operator precedence.
/// </summary>
/// <param name="expr">The parameter expression to render</param>
/// <param name="precedence">The precedence context (higher values require more parentheses)</param>
/// <returns>A string representation of the expression with minimal parentheses</returns>
/// <remarks>
/// Precedence levels:
/// - Addition/Subtraction: 1
/// - Multiplication/Division: 2  
/// - Remainder: 3 (always parenthesized)
/// Parentheses are added when the current operator has lower precedence than the context.
/// </remarks>
let rec renderParamExpression (expr: ParamExpression) (precedence:int) : string =
    match expr with
    | PInt value -> string value
    | PParameter (ParamName name) -> name
    | PAdd (left, right) -> 
        let currentPrecedence = 1
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "+" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "+" + renderParamExpression right currentPrecedence
    | PSubtract (left, right) -> 
        let currentPrecedence = 1
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "-" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "-" + renderParamExpression right currentPrecedence
    | PMultiply (left, right) -> 
        let currentPrecedence = 2
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "*" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "*" + renderParamExpression right currentPrecedence
    | PDivide (left, right) -> 
        let currentPrecedence = 2
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "/" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "/" + renderParamExpression right currentPrecedence
    | PRemainder (left, right) -> 
        let currentPrecedence = 3
        "(" + renderParamExpression left currentPrecedence + "%" + renderParamExpression right currentPrecedence + ")"

/// <summary>
/// Parses a string into a parameter expression AST.
/// </summary>
/// <param name="text">The input string to parse</param>
/// <returns>
/// Success: The parsed parameter expression
/// Error: A human-readable error message describing the parsing failure
/// </returns>
/// <remarks>
/// Supports arithmetic expressions with:
/// - Integer constants
/// - Parameter names (alphanumeric identifiers)
/// - Binary operators: +, -, *, /, %
/// - Parentheses for grouping
/// 
/// Operator precedence (higher binds tighter):
/// - *, /, %: Higher precedence
/// - +, -: Lower precedence
/// 
/// The parser uses recursive descent with separate functions for each precedence level.
/// </remarks>
let parseExpression (text: string) : Result<ParamExpression, ParamError> =
    
    let toOperand (operand: string) =
        match System.Int32.TryParse operand with
        | true, intVal -> PInt intVal
        | false, _ -> PParameter <| ParamName operand

    // Parses primary expressions: numbers, variables, and parentheses
    let rec parsePrimary (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        match tokens with
        | [] -> Error "Unfinished expression"
        | "(" :: rest ->
            match parseExpressionTokens rest with  // Using parseExpressionTokens (defined below)
            | Ok (expr, ")" :: remainingTokens) -> Ok (expr, remainingTokens)  // Ensure closing bracket
            | Ok _ -> Error "Mismatched parentheses"
            | Error e -> Error e
        | ")" :: _ -> Error "Unexpected closing parenthesis"
        | operand :: rest -> Ok (toOperand operand, rest)

    // Parses multiplication, division, and modulo (higher precedence)
    and parseFactors (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        match parsePrimary tokens with
        | Ok (firstOperand, rest) ->
            let rec loop expr remainingTokens =
                match remainingTokens with
                | "*" :: rest ->
                    match parsePrimary rest with
                    | Ok (nextOperand, moreTokens) -> loop (PMultiply (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                | "/" :: rest ->
                    match parsePrimary rest with
                    | Ok (nextOperand, moreTokens) -> loop (PDivide (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                | "%" :: rest ->
                    match parsePrimary rest with
                    | Ok (nextOperand, moreTokens) -> loop (PRemainder (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                | _ -> Ok (expr, remainingTokens)
            loop firstOperand rest
        | Error e -> Error e

    /// Parses addition and subtraction (lower precedence)
    and parseExpressionTokens (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        match parseFactors tokens with
        | Ok (firstOperand, rest) ->
            let rec loop expr remainingTokens =
                match remainingTokens with
                | "+" :: rest ->
                    match parseFactors rest with
                    | Ok (nextOperand, moreTokens) -> loop (PAdd (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                | "-" :: rest ->
                    match parseFactors rest with
                    | Ok (nextOperand, moreTokens) -> loop (PSubtract (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                | _ -> Ok (expr, remainingTokens)
            loop firstOperand rest
        | Error e -> Error e

    // Tokenizer: Splits input into numbers, variables, and operators
    let tokenize (input: string) =
        let pattern = @"\d+[a-zA-Z]*|[a-zA-Z]+\d*|[()+\-*/%]"
        Regex.Matches(input, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList
    
    let validPattern = @"^[0-9a-zA-Z()+\-*/%\s]+$"  // Allow only numbers, letters, operators, spaces, and parentheses
    if text = "" then Error "Input Empty"
    elif not (Regex.IsMatch(text, validPattern)) then
        let invalidChars = text |> Seq.filter (fun c -> not (Regex.IsMatch(c.ToString(), validPattern))) |> Seq.distinct |> Seq.toArray
        Error (sprintf "Contains unsupported characters: %A" invalidChars)
    else
        match tokenize text with
        | [] -> Error "Input Empty"
        | tokens ->
            match parseExpressionTokens tokens with
            | Ok (expr, []) -> Ok expr  // Ensure no leftover tokens
            | Ok (_, leftover) -> Error (sprintf "Unexpected characters at end of expression: %s" (String.concat "" leftover))
            | Error e -> Error e

/// <summary>
/// Checks if a parameter expression contains any parameter references.
/// </summary>
/// <param name="expression">The expression to check</param>
/// <returns>True if the expression contains at least one PParameter, false if it only contains constants</returns>
/// <remarks>
/// This function recursively traverses the expression tree to find any PParameter nodes.
/// Useful for determining if an expression can be fully evaluated without parameter bindings.
/// </remarks>
let rec exprContainsParams (expression: ParamExpression) : bool =
    match expression with
    | PInt _ -> false
    | PParameter _ -> true
    | PAdd (left, right)
    | PSubtract (left, right) 
    | PMultiply (left, right)
    | PDivide (left, right)
    | PRemainder (left, right) ->
        exprContainsParams left || exprContainsParams right



