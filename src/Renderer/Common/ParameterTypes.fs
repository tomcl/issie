module ParameterTypes

open System.Text.RegularExpressions

//------------------------------------------------------------------------------------------------//
//----Types for Parameters defined on sheets and bound to values by custom component instances----//
//------------------------------------------------------------------------------------------------//

module Constants =

    /// The furthest a shift expression may move a value, in bit positions.
    ///
    /// Bounded because the number on the left grows with it: `1<<n` is a number of n bits, so an
    /// unbounded count mistyped into a properties box would ask for a value too large to hold
    /// rather than report anything. The bound is the widest bus Issie has, so every shift a design
    /// could want is allowed. Written out rather than referred to because
    /// CommonTypes.Constants.maxIssieBusWidth is compiled after this file; Properties.fs holds the
    /// two equal so they cannot drift.
    let maxShiftCount = 16384

/// The type a parameter expression computes in.
///
/// bigint, not int, because the values parameters feed are bigint everywhere else in Issie: a
/// constant, a bus comparison value and an input's default value are all `bigint` in ComponentType,
/// and a bus may be up to NumberHelpers.Constants.maxIssieBusWidth bits wide. Evaluating in int
/// meant a wide input's default could not be range-checked at all, and a literal larger than
/// Int32.MaxValue parsed as a parameter name made of digits.
///
/// The narrowing back to int happens once, at the slot: see tryIntOfParamInt.
type ParamInt = bigint

/// The value as an int, or None when it is too large to be one.
///
/// Every parameter value that reaches a component field narrower than a bigint goes through here.
/// A width, an index and a bit position are all `int` in ComponentType, and silently wrapping a
/// value that does not fit is how a nonsensical width would otherwise reach the canvas.
/// NumberHelpers.convertBigintToInt32 masks with 0xffffffff instead, so is not this.
let tryIntOfParamInt (value: ParamInt) : int option =
    if value >= bigint System.Int32.MinValue && value <= bigint System.Int32.MaxValue then
        Some (int value)
    else
        None

/// The value as an int, falling back to `ifTooLarge` where it is not one.
///
/// For the places that have no way to refuse: a component is being created, and its width has
/// already passed the constraints on the box it was typed into - every one of which bounds the
/// width at or below NumberHelpers.Constants.maxIssieBusWidth. So the fallback is a bug path, and
/// the warning is there to say which bound went missing rather than to be read in normal use.
let intOfParamInt (ifTooLarge: int) (value: ParamInt) : int =
    match tryIntOfParamInt value with
    | Some intValue -> intValue
    | None ->
        Log.warn $"Parameter value {value} is too large to be a component field: using {ifTooLarge}"
        ifTooLarge

/// A named parameter in a custom component type
/// For MVP this is ok but maybe names need to be qualified by the
/// design sheet they are in to make functions support parameter inheritance.
type ParamName = ParamName of string

/// A built-in function of two arguments, named by an enumeration so that adding one is a case here
/// rather than a case of ParamExpression. Every list of these is derived from the type by
/// EEExtensions.Union.allCases, so adding a case reserves its name and reaches the parser at once;
/// the compiler then requires only that binFuncName covers it.
type ParamBinFunc =
    | PMin
    | PMax

/// An arithmetic expression containing symbolic parameters
/// For MVP this could be limited to PInt and PParameter only.
/// However, it would be useful to have a more general type definition so that
/// functions that manipulate constraints, parameters, etc can be written in a more general way.
/// The actual parameter value is customisable so that the same code can be used for int parameters (normal)
/// and BigInt parameters (needed for constant values in N bit components).
/// For MVP set 'PINT = int
/// TODO: refactor this to use an enumeration DU for operators to reduce cases.
///
/// The built-in functions are already in that shape: PBinFunc names its operation with an
/// enumeration rather than taking a case of its own per function.
type ParamExpression =
    | PInt of ParamInt
    | PParameter of ParamName
    | PAdd of ParamExpression * ParamExpression
    | PSubtract of ParamExpression * ParamExpression
    | PMultiply of ParamExpression * ParamExpression
    | PDivide of ParamExpression * ParamExpression
    | PRemainder of ParamExpression * ParamExpression
    /// Multiply by a power of two: `a<<b` is a * 2^b. Written and bound as in Verilog and C, so
    /// more loosely than `+`. See shiftLeftBy.
    | PShiftLeft of ParamExpression * ParamExpression
    /// Divide by a power of two, rounding towards minus infinity: an ARITHMETIC shift, so a
    /// negative value stays negative and `-1>>1` is `-1`. There is no logical right shift to go
    /// with it: a parameter is a number rather than a bit pattern of some width, so there is no
    /// width for zeros to come in from. See shiftRightBy.
    | PShiftRight of ParamExpression * ParamExpression
    /// Bits needed to index the operand's value: ceil(log2 n). See clog2.
    | PCLog2 of ParamExpression
    | PBinFunc of ParamBinFunc * ParamExpression * ParamExpression

type ParamError = string

/// The name a two-argument built-in is written under. Lower case: this is the canonical spelling
/// that renderParamExpression emits, though the parser accepts any case.
let binFuncName (func: ParamBinFunc) : string =
    match func with
    | PMin -> "min"
    | PMax -> "max"

/// The name the one-argument built-in is written under.
let cLog2Name = "clog2"

/// The two-argument built-in of that name, matched without regard to case.
let tryBuiltinBinFunc (name: string) : ParamBinFunc option =
    EEExtensions.Union.allCases<ParamBinFunc> ()
    |> List.tryFind (fun func -> binFuncName func = name.ToLowerInvariant())

/// Whether the name is a built-in function. Matched without regard to case, so that `MAX` cannot be
/// a parameter while `max` is a function: the two would read as the same word.
let isBuiltinFuncName (name: string) : bool =
    name.ToLowerInvariant() = cLog2Name || (tryBuiltinBinFunc name).IsSome

/// Bits needed to index n things: ceil(log2 n), so clog2 8 = 3 and clog2 9 = 4.
/// 0 and 1 both give 0, as Verilog's $clog2 does.
///
/// The n <= 1 guard is load-bearing rather than tidiness: >>> on a negative bigint is an arithmetic
/// shift and never reaches 0, so without it a negative argument would loop for ever. The evaluator
/// rejects negatives before calling this, which is where the user-facing message lives.
///
/// Computed by shifting rather than by System.Math.Log: the float version is wrong at exact powers
/// of two, which is why the one in SheetCreator is commented out.
let clog2 (n: ParamInt) : ParamInt =
    let rec bits (v: ParamInt) (acc: ParamInt) = if v <= 1I then acc else bits (v >>> 1) (acc + 1I)
    if n <= 1I then 0I else bits (n - 1I) 0I + 1I

/// The value of `value<<places`: multiplication by 2^places.
///
/// Written as a multiplication rather than with `<<<` so that the two runtimes cannot disagree
/// about it. Fable's bigint is its own implementation, and it is a NEGATIVE left operand - which
/// nothing else in Issie shifts - whose sign-magnitude representation might move differently there
/// than under .NET, where no test would see it. Multiplying is the same operation in both.
/// Both callers bound places by Constants.maxShiftCount before calling.
let shiftLeftBy (places: int) (value: ParamInt) : ParamInt = value * (1I <<< places)

/// The value of `value>>places`: division by 2^places, rounding towards minus infinity, which is
/// what makes the shift arithmetic.
///
/// bigint division truncates towards zero instead, so `-1/2` is 0 where an arithmetic `-1>>1` is
/// -1. Taking the truncating quotient and stepping it down where it threw something away is the
/// same rounding for every value, and is again the same in both runtimes.
let shiftRightBy (places: int) (value: ParamInt) : ParamInt =
    let divisor = 1I <<< places
    let quotient = value / divisor
    if value.Sign < 0 && quotient * divisor <> value then quotient - 1I else quotient

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

/// What one property input box holds while it is being edited: the text as typed, and what that
/// text means.
///
/// The TEXT is model state, as it is for every other input box in Issie - the Constant's value box,
/// the component name box - which dispatch the unparsed string on each keystroke and render
/// themselves from it. Keeping only the parsed value here left the box with no way to be set except
/// by reaching past React into the DOM, which is what the offer to bind a property to an enclosing
/// sheet's had to do.
///
/// Only a Spec that is Ok ever reaches a component: text that will not parse, will not evaluate, or
/// breaks a constraint stays here with its message, and the design is untouched until it is valid.
type ParamBoxState = {
    Text: string
    Spec: Result<NewParamCompSpec, ParamError>
}

/// The Elmish Model state used to manage input boxes that can be used to define parameter expressions.
/// Part of Model.PopupDialogData.DialogState.
///
/// Keyed by ParamSlot rather than CompSlotName, so that the key names WHICH component's box this is.
/// Slot names are shared by construction - every component with a width has `Buswidth`, every
/// instance of one sheet has `CustomCompParam "w"` - so keying on the name alone meant one
/// component's box read another's entry, and selecting a second component showed the first's error
/// on it in red. A popup has no component and uses an empty CompId; its entries are cleared
/// wholesale when the popup closes.
type ParamBoxDialogState = Map<ParamSlot, ParamBoxState>

/// The dialog-state key for a box belonging to a component, or to a popup where there is none.
let paramBoxKey (compId: string option) (slot: CompSlotName) : ParamSlot =
    {CompId = Option.defaultValue "" compId; CompSlot = slot}

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

        /// Either shift, once its count has been checked. A count must be a bit position: a
        /// negative one would move the value the way the operator does not say, and one beyond
        /// Constants.maxShiftCount asks for a number wider than any bus - `1<<1000000000` is a
        /// hundred megabytes of bigint, arrived at by holding a key down.
        let shift (op: string) (apply: int -> ParamInt -> ParamInt) left right =
            binary
                (fun l r ->
                    if r.Sign < 0 then
                        Error $"Shift by a negative number of places: {l}{op}{r} cannot shift the other way"
                    elif r > bigint Constants.maxShiftCount then
                        Error $"Shift too far: {l}{op}{r} moves further than the {Constants.maxShiftCount} bits of the widest bus"
                    else Ok (apply (int r) l))
                left right

        match expr with
        | PInt value -> Ok value
        | PParameter name when List.contains name beingEvaluated ->
            let chain =
                name :: beingEvaluated
                |> List.rev
                |> List.map unwrapParamName
                |> String.concat " which uses "
            Error $"Property '{unwrapParamName name}' is defined in terms of itself: {chain}"
        | PParameter name ->
            match Map.tryFind name paramBindings with
            | Some boundExpr -> eval (name :: beingEvaluated) boundExpr
            // A name that is not in scope is nearly always one of two mistakes, and they need
            // different advice: reaching for a property that exists under another name, or not
            // knowing that a value has to be declared as a property before it can be used. Saying
            // only that the name is undefined helps with neither, so say which names there are -
            // or, where there are none, what to do instead.
            | None when Map.isEmpty paramBindings ->
                Error "This value must be numeric: to use a property this must first be added to the sheet"
            | None ->
                let inScope =
                    paramBindings
                    |> Map.toList
                    |> List.map (fst >> unwrapParamName)
                    |> List.sort
                    |> String.concat ", "
                Error $"Property '{unwrapParamName name}' is not defined. Properties of this sheet: {inScope}"
        | PAdd (left, right) -> binary (fun l r -> Ok (l + r)) left right
        | PSubtract (left, right) -> binary (fun l r -> Ok (l - r)) left right
        | PMultiply (left, right) -> binary (fun l r -> Ok (l * r)) left right
        | PDivide (left, right) ->
            binary
                (fun l r ->
                    if r.IsZero then Error $"Division by zero: {l} cannot be divided by 0"
                    else Ok (l / r))
                left right
        | PRemainder (left, right) ->
            binary
                (fun l r ->
                    if r.IsZero then Error $"Remainder by zero: the remainder of {l} divided by 0 is undefined"
                    else Ok (l % r))
                left right
        | PShiftLeft (left, right) -> shift "<<" shiftLeftBy left right
        | PShiftRight (left, right) -> shift ">>" shiftRightBy left right
        // A negative argument has no answer, and clog2 would not terminate on one. clog2 0 is 0
        // rather than an error: a width of zero is caught by the component's own constraint, whose
        // message is written for the component and so says more than this one could.
        | PCLog2 expr ->
            match eval beingEvaluated expr with
            | Error err -> Error err
            | Ok value when value.Sign < 0 ->
                Error $"{cLog2Name} is not defined for negative values: {cLog2Name}({value})"
            | Ok value -> Ok (clog2 value)
        | PBinFunc (PMin, left, right) -> binary (fun l r -> Ok (min l r)) left right
        | PBinFunc (PMax, left, right) -> binary (fun l r -> Ok (max l r)) left right

    eval [] paramExpr

/// <summary>
/// Converts a parameter expression to its string representation with proper operator precedence.
/// </summary>
/// <param name="expr">The parameter expression to render</param>
/// <param name="precedence">The precedence context (higher values require more parentheses)</param>
/// <returns>A string representation of the expression with minimal parentheses</returns>
/// <remarks>
/// Precedence levels, which are the parser's read from the other end:
/// - Shifts: 1
/// - Addition/Subtraction: 2
/// - Multiplication/Division: 3
/// - Remainder: 4 (always parenthesized)
/// Parentheses are added when the current operator has lower precedence than the context.
/// </remarks>
let rec renderParamExpression (expr: ParamExpression) (precedence:int) : string =
    match expr with
    | PInt value -> string value
    | PParameter (ParamName name) -> name
    // Every binary operator here is left-associative, so wherever reparsing the right operand
    // against the same level would regroup it - which is all of them but +, whose regrouping means
    // the same - the right operand renders one level tighter and so keeps its parentheses.
    | PShiftLeft (left, right) ->
        let currentPrecedence = 1
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "<<" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "<<" + renderParamExpression right (currentPrecedence + 1)
    | PShiftRight (left, right) ->
        let currentPrecedence = 1
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + ">>" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + ">>" + renderParamExpression right (currentPrecedence + 1)
    | PAdd (left, right) ->
        let currentPrecedence = 2
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "+" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "+" + renderParamExpression right currentPrecedence
    | PSubtract (left, right) ->
        let currentPrecedence = 2
        // the right operand renders one level tighter: parsing is left-associative, so
        // a-(b-c) must keep its parentheses
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "-" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "-" + renderParamExpression right (currentPrecedence + 1)
    | PMultiply (left, right) ->
        let currentPrecedence = 3
        // a*(b/c) must keep its parentheses: reparsed left-associatively as (a*b)/c it
        // differs under integer division
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "*" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "*" + renderParamExpression right (currentPrecedence + 1)
    | PDivide (left, right) ->
        let currentPrecedence = 3
        // as with subtraction, a/(b/c) must keep its parentheses
        if precedence > currentPrecedence then
            "(" + renderParamExpression left currentPrecedence + "/" + renderParamExpression right (currentPrecedence + 1) + ")"
        else renderParamExpression left currentPrecedence + "/" + renderParamExpression right (currentPrecedence + 1)
    | PRemainder (left, right) ->
        let currentPrecedence = 4
        "(" + renderParamExpression left currentPrecedence + "%" + renderParamExpression right currentPrecedence + ")"
    // A function call is atomic - its parentheses are its own - so it needs no parentheses of its
    // own however tightly it is bound, and its arguments are delimited and so render at 0.
    | PCLog2 expr ->
        cLog2Name + "(" + renderParamExpression expr 0 + ")"
    | PBinFunc (func, left, right) ->
        binFuncName func + "(" + renderParamExpression left 0 + "," + renderParamExpression right 0 + ")"

/// Whether the name is one a parameter may not take because it is a built-in function.
/// Kept separate from isValidParamName so that a dialog can say which rule was broken: "clog2 is a
/// function" and "names are letters and digits" are different problems needing different fixes.
let isReservedParamName (name: string) : bool = isBuiltinFuncName name

/// What parseExpression says about an empty box.
///
/// Says what to do rather than what is wrong. An empty box is not a mistake - it is how the user
/// asks to be offered a property to follow - so "Input Empty" reported a fault where there was
/// none, in red, on a gesture the UI supports. The UI recognises that gesture by the box's TEXT
/// being empty, not by this message, so nothing outside the parser reads it.
let private emptyInputError = "Type a number or expression"

/// The names a parameter may have: a letter, then letters and digits, and not a built-in function.
///
/// This is exactly the parser's name token, exported so that the two cannot drift apart. They had:
/// names were accepted as `[a-zA-Z0-9]+`, while the tokenizer split `W2X` into `W2` and `X` - so a
/// parameter could be declared under a name and then never referred to. A leading digit is
/// excluded for the same reason read the other way round: `123` would be a number. A built-in
/// function name is excluded for the third form of the same reason: the parser reads `min` as the
/// function, so a parameter of that name could be declared and then never referred to.
let isValidParamName (name: string) : bool =
    Regex.IsMatch(name, @"^[a-zA-Z][a-zA-Z0-9]*$") && not (isReservedParamName name)

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
/// - Integer constants of any size, which may be negated
/// - Parameter names, which are a letter followed by letters and digits (see isValidParamName)
/// - Binary operators: +, -, *, /, %, &lt;&lt;, &gt;&gt;
/// - Built-in functions: clog2(x), min(x,y), max(x,y), written in any case
/// - Unary minus
/// - Parentheses for grouping
///
/// Operator precedence (higher binds tighter):
/// - a function call: atomic, being delimited by its own parentheses
/// - unary -: binds tightest of the operators, being part of the operand it precedes
/// - *, /, %: Higher precedence
/// - +, -: Lower precedence
/// - &lt;&lt;, &gt;&gt;: Loosest, as in Verilog and C, so that w+1&lt;&lt;2 shifts the sum
///
/// The parser uses recursive descent with separate functions for each precedence level.
/// </remarks>
let parseExpression (text: string) : Result<ParamExpression, ParamError> =

    /// A digit run is a literal of any size, and anything else is a name. Parsed as bigint rather
    /// than int32 because ParamInt is bigint: read as an int32, a literal too large to be one fell
    /// through to being a property whose name was all digits, and failed much later with
    /// "Property '99999999999' is not defined".
    let toOperand (operand: string) =
        match System.Numerics.BigInteger.TryParse operand with
        | true, value -> PInt value
        | false, _ -> PParameter <| ParamName operand

    /// Negation. There is no PNegate case and none is wanted: subtraction from zero is the same
    /// expression, so every function over ParamExpression already handles it and no saved file
    /// needs to change. A negated literal is folded so that it renders back as the user typed it;
    /// a negated parameter renders as `0-W`, which means the same and re-parses to itself.
    let negate expr =
        match expr with
        | PInt value -> PInt -value
        | _ -> PSubtract (PInt 0I, expr)

    // Parses primary expressions: numbers, variables, function calls, negation and parentheses
    let rec parsePrimary (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        /// The argument list of a built-in call: the tokens after its name must open a bracket,
        /// hold the arguments separated by commas, and close it. An argument is a whole expression,
        /// and parseExpressionTokens stops at `,` and `)` and hands them back, so a call needs no
        /// precedence level of its own.
        let parseCall (name: string) (arity: int) (form: string) (tokens: string list) =
            let rec parseArgs remaining args =
                match parseExpressionTokens remaining with
                | Error e -> Error e
                | Ok (arg, rest) ->
                    let args = arg :: args
                    match rest with
                    | "," :: more when List.length args < arity -> parseArgs more args
                    | ")" :: more when List.length args = arity -> Ok (List.rev args, more)
                    | _ -> Error $"{name} takes {arity} values: write {form}"
            match tokens with
            | "(" :: rest -> parseArgs rest []
            | _ -> Error $"{name} is a built-in function: write {form}"

        match tokens with
        | [] -> Error "Unfinished expression"
        | "(" :: rest ->
            match parseExpressionTokens rest with  // Using parseExpressionTokens (defined below)
            | Ok (expr, ")" :: remainingTokens) -> Ok (expr, remainingTokens)  // Ensure closing bracket
            | Ok _ -> Error "Mismatched parentheses"
            | Error e -> Error e
        | ")" :: _ -> Error "Unexpected closing parenthesis"
        // Without this a stray comma would fall through to toOperand and become a parameter named
        // "," , which no message could explain.
        | "," :: _ -> Error "Unexpected comma: a comma separates the two values of min or max"
        // Unary minus, so that a negative value can be written at all. Without it `-4` parsed the
        // minus sign as though it were a parameter name and then complained about the 4.
        | "-" :: rest -> parsePrimary rest |> Result.map (fun (expr, remaining) -> negate expr, remaining)
        // A built-in function name is read as a call, never as a parameter: isValidParamName
        // refuses these names, so there is no parameter for it to shadow.
        | name :: rest when name.ToLowerInvariant() = cLog2Name ->
            parseCall cLog2Name 1 $"{cLog2Name}(expression)" rest
            |> Result.map (fun (args, remaining) -> PCLog2 args.[0], remaining)
        | name :: rest when (tryBuiltinBinFunc name).IsSome ->
            let func = (tryBuiltinBinFunc name).Value
            let funcName = binFuncName func
            parseCall funcName 2 $"{funcName}(a,b)" rest
            |> Result.map (fun (args, remaining) -> PBinFunc (func, args.[0], args.[1]), remaining)
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

    /// Parses addition and subtraction, which bind more tightly than the shifts below
    and parseTerms (tokens: string list) : Result<ParamExpression * string list, ParamError> =
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

    /// Parses the shifts, which bind most loosely of all - as they do in Verilog and C, so that
    /// `w+1<<2` shifts the sum rather than adding a shifted 1. This is the whole-expression level:
    /// what the input as a whole, a bracketed group and a function argument are each parsed as.
    and parseExpressionTokens (tokens: string list) : Result<ParamExpression * string list, ParamError> =
        match parseTerms tokens with
        | Ok (firstOperand, rest) ->
            let rec loop expr remainingTokens =
                let shiftBy make rest =
                    match parseTerms rest with
                    | Ok (nextOperand, moreTokens) -> loop (make (expr, nextOperand)) moreTokens
                    | Error e -> Error e
                match remainingTokens with
                | "<<" :: rest -> shiftBy PShiftLeft rest
                | ">>" :: rest -> shiftBy PShiftRight rest
                | _ -> Ok (expr, remainingTokens)
            loop firstOperand rest
        | Error e -> Error e

    // Tokenizer: Splits input into numbers, parameter names, and operators.
    // The name token is exactly isValidParamName's rule, so every name that can be declared can be
    // written in an expression. It used to be `\d+[a-zA-Z]*|[a-zA-Z]+\d*`, which tokenised `W2X`
    // as `W2` then `X` - so a parameter of that name could be declared and then never used, with
    // an error message that pointed at neither problem.
    // A shift is matched before the single `<` and `>`, which are tokens only so that a lone one is
    // something the input is refused for rather than something dropped on the floor: unmatched
    // characters are discarded by Regex.Matches, so `a<b` would otherwise tokenise as `a b` and be
    // read as a number run into a name.
    let tokenize (input: string) =
        let pattern = @"[a-zA-Z][a-zA-Z0-9]*|\d+|<<|>>|[(),+\-*/%<>]"
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

    /// A single `<` or `>`. Nothing in this language compares two values, so half a shift is the
    /// only thing either can be. Reported here for the same reason as a number run into a name:
    /// left to the parser it surfaces as an unexpected token at the end of the expression, which
    /// points several tokens away from what was typed.
    let loneAngleBracket (tokens: string list) =
        tokens |> List.tryFind (fun token -> token = "<" || token = ">")

    /// What is wrong with the tokens themselves, before any of them is parsed.
    let malformedToken (tokens: string list) =
        match numberRunIntoName tokens with
        | Some (number, name) ->
            Some $"'{number}{name}' is neither a number nor a property name: a property name \
                   must start with a letter, and a multiplication must be written out as \
                   {number}*{name}"
        | None ->
            loneAngleBracket tokens
            |> Option.map (fun bracket -> $"'{bracket}' is not an operator: a shift is written '{bracket}{bracket}'")

    // Allow only numbers, letters, operators, spaces, parentheses and the comma that separates the
    // arguments of min and max. Without the comma here, min(a,b) is refused before it is parsed.
    let validPattern = @"^[0-9a-zA-Z(),+\-*/%<>\s]+$"
    if text = "" then Error emptyInputError
    elif not (Regex.IsMatch(text, validPattern)) then
        let invalidChars = text |> Seq.filter (fun c -> not (Regex.IsMatch(c.ToString(), validPattern))) |> Seq.distinct |> Seq.toArray
        Error (sprintf "Contains unsupported characters: %A" invalidChars)
    else
        match tokenize text with
        | [] -> Error emptyInputError
        | tokens ->
            match malformedToken tokens with
            | Some err -> Error err
            | None ->
                match parseExpressionTokens tokens with
                | Ok (expr, []) -> Ok expr  // Ensure no leftover tokens
                // A comma left over is not just an unexpected character: it is the one that
                // separates the arguments of min and max, so say where it does belong.
                | Ok (_, "," :: _) -> Error "Unexpected comma: a comma separates the two values of min or max"
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
    | PRemainder (left, right)
    | PShiftLeft (left, right)
    | PShiftRight (left, right)
    | PBinFunc (_, left, right) ->
        exprContainsParams left || exprContainsParams right
    | PCLog2 expr -> exprContainsParams expr

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
        | PRemainder (left, right)
        | PShiftLeft (left, right)
        | PShiftRight (left, right)
        | PBinFunc (_, left, right) ->
            collect left @ collect right
        | PCLog2 expr -> collect expr
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



