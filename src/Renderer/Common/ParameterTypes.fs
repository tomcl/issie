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
/// The number of inputs of a gate or merge is deliberately absent: an input count sets how many
/// ports a component has, and a parameter records a value, not a change of topology.
type CompSlotName =
    | Buswidth
    | IO of Label: string
    /// A parameter of the sheet INSIDE a custom component instance, bound by that instance.
    /// See ComponentSlots.trySetSlotValue and ParameterView.makeParamBindingEntryBoxes.
    | CustomCompParam of ParamName: string
    // SplitN-specific parameterised slots
    | SplitNWidth of Index: int
    | SplitNLSB of Index: int
    /// The value an Input takes when it is undriven. Distinct from IO, which is the input's width:
    /// both are edited from the same properties pane and so cannot share a slot.
    | InputDefault

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

/// Whether two slot names refer to the same field of a component.
///
/// The label in an `IO` slot is not part of its identity. It records the component's label as it
/// was when the slot was created, and nothing rewrites it when the component is renamed - so a
/// rename would otherwise orphan the slot and let a second one be created for the same field, with
/// which of the two applied decided by Map key order. Every reader already ignores it
/// (ComponentSlots.trySetSlotValue matches `IO _` in every case), so this is what "the same slot"
/// has always meant in effect. The label is kept in the type because existing .dgm files store it,
/// and it is repaired on save by CanvasExtractor.tidyParamSlots so that it stays worth displaying.
let sameSlotName (a: CompSlotName) (b: CompSlotName) =
    match a, b with
    | IO _, IO _ -> true
    | _ -> a = b

/// Whether two slots are the same slot: the same field of the same component. See sameSlotName.
let sameSlot (a: ParamSlot) (b: ParamSlot) =
    a.CompId = b.CompId && sameSlotName a.CompSlot b.CompSlot

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

/// Map from name to expression for each parameter.
/// This is what an INSTANCE binds: a custom component binding carries no description, because the
/// description belongs to the declaration on the sheet inside it.
type ParamBindings = Map<ParamName, ParamExpression>

/// The DECLARATION of one parameter on a sheet: its default value and what it means.
/// The description is compulsory - it is what the user reads when a custom component instance of
/// the sheet asks them for a value, so a parameter without one cannot be explained at the point
/// it has to be understood.
type ParamDefinition = {
    Expression: ParamExpression
    Description: string
}

/// Lenses for ParamDefinition
let paramExpression_ = Optics.Lens.create (fun s -> s.Expression) (fun v s -> {s with Expression = v})
let paramDescription_ = Optics.Lens.create (fun s -> s.Description) (fun v s -> {s with Description = v})

/// The parameters a sheet declares, with their defaults and descriptions.
type ParamDefinitions = Map<ParamName, ParamDefinition>

/// The declarations seen as an evaluation environment: descriptions dropped.
/// Every place that evaluates an expression against a sheet's defaults goes through this.
let bindingsOf (defs: ParamDefinitions) : ParamBindings =
    defs |> Map.map (fun _ def -> def.Expression)


/// For Part A: alternatively you could store slot information in the component record
/// as an extra field.
/// This field should store all the Component's slot information where slots are bound to parameters.
type ComponentSlotExpr = Map<ParamSlot, ConstrainedExpr>

/// The expression filling a slot, found by what the slot IS rather than by the exact key: an `IO`
/// slot stored under the component's old label is still that component's IO slot. See sameSlot.
let tryFindSlot (slot: ParamSlot) (slots: ComponentSlotExpr) : ConstrainedExpr option =
    match Map.tryFind slot slots with
    | Some found -> Some found
    | None -> slots |> Map.toList |> List.tryPick (fun (s, e) -> if sameSlot s slot then Some e else None)

/// Every slot of the map that is not the given slot. Used to replace or delete a slot without
/// leaving behind an older key for the same field - the state that made two slots fight over one
/// component's width.
let private withoutSlot (slot: ParamSlot) (slots: ComponentSlotExpr) : ComponentSlotExpr =
    slots |> Map.filter (fun s _ -> not (sameSlot s slot))

/// Put an expression in a slot, replacing whatever filled it before.
let addSlot (slot: ParamSlot) (exprSpec: ConstrainedExpr) (slots: ComponentSlotExpr) : ComponentSlotExpr =
    withoutSlot slot slots |> Map.add slot exprSpec

/// Empty a slot, so that its field goes back to being an ordinary number.
let removeSlot (slot: ParamSlot) (slots: ComponentSlotExpr) : ComponentSlotExpr =
    withoutSlot slot slots

/// The state used per design sheet to define integer slots
/// that have values defined with parameter expressions
/// LoadedComponent.LCParameterSlots
/// (also used in SheetInfo - to save / load files - but the LoadedComponent field is the only one used by HLP Teams)
type ParameterDefs = {
    DefaultBindings: ParamDefinitions
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
    let unwrapParamName (ParamName name) = name

    /// beingEvaluated is the chain of parameters whose definitions we are inside. A parameter bound
    /// to an expression is evaluated in turn, so parameters may be defined in terms of each other;
    /// the chain stops a parameter defined in terms of itself from recursing for ever.
    let rec eval (beingEvaluated: ParamName list) (expr: ParamExpression) : Result<ParamInt, ParamError> =
        /// Evaluate both operands, reporting the first that fails, then combine them.
        let binary (combine: ParamInt -> ParamInt -> Result<ParamInt, ParamError>) left right =
            match eval beingEvaluated left, eval beingEvaluated right with
            | Ok l, Ok r -> combine l r
            | Error err, _ -> Error err
            | _, Error err -> Error err
        match expr with
        | PInt value -> Ok value
        | PParameter name when List.contains name beingEvaluated ->
            let chain =
                name :: beingEvaluated
                |> List.rev
                |> List.map unwrapParamName
                |> String.concat " which uses "
            Error $"Parameter '{unwrapParamName name}' is defined in terms of itself: {chain}"
        | PParameter name ->
            match Map.tryFind name paramBindings with
            | Some boundExpr -> eval (name :: beingEvaluated) boundExpr
            // A name that is not in scope is nearly always one of two mistakes, and they need
            // different advice: reaching for a parameter that exists under another name, or not
            // knowing that a value has to be declared as a parameter before it can be used. Saying
            // only that the name is undefined helps with neither, so say which names there are -
            // or, where there are none, what to do instead.
            | None when Map.isEmpty paramBindings ->
                Error "This value must be numeric: to use a parameter this must first be added to the sheet"
            | None ->
                let inScope =
                    paramBindings
                    |> Map.toList
                    |> List.map (fst >> unwrapParamName)
                    |> List.sort
                    |> String.concat ", "
                Error $"Parameter '{unwrapParamName name}' is not defined. Parameters of this sheet: {inScope}"
        | PAdd (left, right) -> binary (fun l r -> Ok (l + r)) left right
        | PSubtract (left, right) -> binary (fun l r -> Ok (l - r)) left right
        | PMultiply (left, right) -> binary (fun l r -> Ok (l * r)) left right
        | PDivide (left, right) ->
            binary
                (fun l r ->
                    match r with
                    | 0 -> Error $"Division by zero: {l} cannot be divided by 0"
                    | _ -> Ok (l / r))
                left right
        | PRemainder (left, right) ->
            binary
                (fun l r ->
                    match r with
                    | 0 -> Error $"Remainder by zero: the remainder of {l} divided by 0 is undefined"
                    | _ -> Ok (l % r))
                left right

    eval [] paramExpr

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
        // the right operand renders one level tighter: parsing is left-associative, so
        // a-(b-c) must keep its parentheses
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "-" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "-" + renderParamExpression right (currentPrecedence + 1)
    | PMultiply (left, right) ->
        let currentPrecedence = 2
        // a*(b/c) must keep its parentheses: reparsed left-associatively as (a*b)/c it
        // differs under integer division
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "*" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "*" + renderParamExpression right (currentPrecedence + 1)
    | PDivide (left, right) ->
        let currentPrecedence = 2
        // as with subtraction, a/(b/c) must keep its parentheses
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "/" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "/" + renderParamExpression right (currentPrecedence + 1)
    | PRemainder (left, right) -> 
        let currentPrecedence = 3
        "(" + renderParamExpression left currentPrecedence + "%" + renderParamExpression right currentPrecedence + ")"

/// The names a parameter may have: a letter, then letters and digits.
///
/// This is exactly the parser's name token, exported so that the two cannot drift apart. They had:
/// names were accepted as `[a-zA-Z0-9]+`, while the tokenizer split `W2X` into `W2` and `X` - so a
/// parameter could be declared under a name and then never referred to. A leading digit is
/// excluded for the same reason read the other way round: `123` would be a number.
let isValidParamName (name: string) : bool =
    Regex.IsMatch(name, @"^[a-zA-Z][a-zA-Z0-9]*$")

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
/// - Integer constants, which may be negated
/// - Parameter names, which are a letter followed by letters and digits (see isValidParamName)
/// - Binary operators: +, -, *, /, %
/// - Unary minus
/// - Parentheses for grouping
///
/// Operator precedence (higher binds tighter):
/// - unary -: binds tightest, being part of the operand it precedes
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

    /// Negation. There is no PNegate case and none is wanted: subtraction from zero is the same
    /// expression, so every function over ParamExpression already handles it and no saved file
    /// needs to change. A negated literal is folded so that it renders back as the user typed it;
    /// a negated parameter renders as `0-W`, which means the same and re-parses to itself.
    let negate expr =
        match expr with
        | PInt value -> PInt -value
        | _ -> PSubtract (PInt 0, expr)

    // Parses primary expressions: numbers, variables, negation and parentheses
    let rec parsePrimary (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        match tokens with
        | [] -> Error "Unfinished expression"
        | "(" :: rest ->
            match parseExpressionTokens rest with  // Using parseExpressionTokens (defined below)
            | Ok (expr, ")" :: remainingTokens) -> Ok (expr, remainingTokens)  // Ensure closing bracket
            | Ok _ -> Error "Mismatched parentheses"
            | Error e -> Error e
        | ")" :: _ -> Error "Unexpected closing parenthesis"
        // Unary minus, so that a negative value can be written at all. Without it `-4` parsed the
        // minus sign as though it were a parameter name and then complained about the 4.
        | "-" :: rest -> parsePrimary rest |> Result.map (fun (expr, remaining) -> negate expr, remaining)
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

    // Tokenizer: Splits input into numbers, parameter names, and operators.
    // The name token is exactly isValidParamName's rule, so every name that can be declared can be
    // written in an expression. It used to be `\d+[a-zA-Z]*|[a-zA-Z]+\d*`, which tokenised `W2X`
    // as `W2` then `X` - so a parameter of that name could be declared and then never used, with
    // an error message that pointed at neither problem.
    let tokenize (input: string) =
        let pattern = @"[a-zA-Z][a-zA-Z0-9]*|\d+|[()+\-*/%]"
        Regex.Matches(input, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList
    
    /// A number directly followed by a name, which this grammar never allows. It is worth its own
    /// message because it is what `2W` now becomes: either a multiplication sign is missing, or the
    /// design predates names having to start with a letter. Reported here rather than left to the
    /// parser, which notices several steps later and complains about the wrong token.
    let numberRunIntoName (tokens: string list) =
        List.pairwise tokens
        |> List.tryFind (fun (a, b) -> Regex.IsMatch(a, @"^\d+$") && Regex.IsMatch(b, @"^[a-zA-Z]"))

    let validPattern = @"^[0-9a-zA-Z()+\-*/%\s]+$"  // Allow only numbers, letters, operators, spaces, and parentheses
    if text = "" then Error "Input Empty"
    elif not (Regex.IsMatch(text, validPattern)) then
        let invalidChars = text |> Seq.filter (fun c -> not (Regex.IsMatch(c.ToString(), validPattern))) |> Seq.distinct |> Seq.toArray
        Error (sprintf "Contains unsupported characters: %A" invalidChars)
    else
        match tokenize text with
        | [] -> Error "Input Empty"
        | tokens ->
            match numberRunIntoName tokens with
            | Some (number, name) ->
                Error $"'{number}{name}' is neither a number nor a parameter name: a parameter name \
                        must start with a letter, and a multiplication must be written out as \
                        {number}*{name}"
            | None ->
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

/// The parameters an expression refers to, without duplicates.
/// Used to check the invariant that every parameter referred to on a sheet is defined on that sheet.
let paramNamesOfExpr (expression: ParamExpression) : ParamName list =
    let rec collect expression =
        match expression with
        | PInt _ -> []
        | PParameter name -> [name]
        | PAdd (left, right)
        | PSubtract (left, right)
        | PMultiply (left, right)
        | PDivide (left, right)
        | PRemainder (left, right) ->
            collect left @ collect right
    collect expression
    |> List.distinct

/// The parameters a slot refers to: its value expression and the expressions in its constraints.
let paramNamesOfSlot (exprSpec: ConstrainedExpr) : ParamName list =
    exprSpec.Constraints
    |> List.collect (fun konst -> match konst with | MinVal (expr, _) | MaxVal (expr, _) -> paramNamesOfExpr expr)
    |> List.append (paramNamesOfExpr exprSpec.Expression)
    |> List.distinct

/// The slots of one sheet that refer to the named parameter of that sheet.
/// A custom component instance is not a special case: its CustomCompParam slot holds an expression
/// in the parameters of the sheet the instance sits on, like any other slot.
let slotsUsingParam (name: ParamName) (slots: ComponentSlotExpr) : (ParamSlot * ConstrainedExpr) list =
    slots
    |> Map.toList
    |> List.filter (fun (_, exprSpec) -> List.contains name (paramNamesOfSlot exprSpec))



