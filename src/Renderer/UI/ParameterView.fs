module ParameterView

open ParameterTypes
open EEExtensions
open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open PopupHelpers
open Sheet.SheetInterface
open DrawModelType
open Optics
open Optics.Operators
open Optic
open System.Text.RegularExpressions
open Fulma.Extensions.Wikiki

//------------------------------------------------------------------------------------------------
//------------------------------ Handle parameters defined on design sheets ----------------------
//------------------------------------------------------------------------------------------------

(*
 * Parameters are symbols defined constant values that can be used in the design.
 * Parameter definitions have integer default values given in the sheet definition (properties pane).
 * These can be over-ridden per instance by definitions in the component instance (properties pane).
 * Parameter values can in general be defined using parameter expressions containing in-scope parameters
 * Parameter scope is currently defined to be all component instances on the parameter sheet.
 * Parameters are used in parameter expressions in the properties pane of components.
 *
 * See Common/ParameterTypes.fs for the types used to represent parameters and parameter expressions.
 *)



// Lenses & Prisms for accessing sheet parameter information

// Accessing param info from loaded component
let defaultBindingsOfLC_ = lcParameterSlots_ >?> defaultBindings_
let paramSlotsOfLC_ = lcParameterSlots_ >?> paramSlots_

// Accessing param info of currently open sheet from model
let lcParameterInfoOfModel_ = openLoadedComponentOfModel_ >?> lcParameterSlots_ 
let paramSlotsOfModel_ = lcParameterInfoOfModel_ >?> paramSlots_
let defaultBindingsOfModel_ = lcParameterInfoOfModel_ >?> defaultBindings_

let modelToSymbols = sheet_ >-> SheetT.wire_ >-> BusWireT.symbol_ >-> SymbolT.symbols_

let symbolsToSymbol_ (componentId: ComponentId): Optics.Lens<Map<ComponentId, SymbolT.Symbol>, SymbolT.Symbol> =
    Lens.create
        (fun symbols -> 
            match Map.tryFind componentId symbols with
            | Some symbol -> symbol
            | None -> failwithf "Component %A not found in this sheet" componentId)
        (fun symbol symbols -> 
            symbols |> Map.add componentId symbol)


let symbolToComponent_ : Optics.Lens<SymbolT.Symbol, Component> =
    Lens.create
        (fun symbol -> symbol.Component)
        (fun newComponent symbol -> { symbol with Component = newComponent })


let compSlot_ (compSlotName:CompSlotName) : Optics.Lens<Component, int> = 
    Lens.create
        (fun comp ->
            match compSlotName with
            | Buswidth -> 
                match comp.Type with
                | Viewer busWidth -> busWidth
                | BusCompare1 (busWidth, _, _) -> busWidth
                | BusSelection (outputWidth, _) -> outputWidth
                | Constant1 (width, _, _) -> width
                | NbitsAdder busWidth -> busWidth
                | NbitsAdderNoCin busWidth -> busWidth
                | NbitsAdderNoCout busWidth -> busWidth
                | NbitsAdderNoCinCout busWidth -> busWidth
                | NbitsXor (busWidth, _) -> busWidth
                | NbitsAnd busWidth -> busWidth
                | NbitsNot busWidth -> busWidth
                | NbitsOr busWidth -> busWidth
                | NbitSpreader busWidth -> busWidth
                | SplitWire busWidth -> busWidth
                | Register busWidth -> busWidth
                | RegisterE busWidth -> busWidth
                | Counter busWidth -> busWidth
                | CounterNoLoad busWidth -> busWidth
                | CounterNoEnable busWidth -> busWidth
                | CounterNoEnableLoad busWidth -> busWidth
                | Shift (busWidth, _, _) -> busWidth
                | BusCompare (busWidth, _) -> busWidth
                | Input busWidth -> busWidth
                | Constant (width, _) -> width
                | _ -> failwithf $"Invalid component {comp.Type} for buswidth"
            | NGateInputs ->
                match comp.Type with
                | GateN (_, n) -> n
                | _ -> failwithf $"Invalid component {comp.Type} for gate inputs"
            | IO _ ->
                match comp.Type with
                | Input1 (busWidth, _) -> busWidth
                | Output busWidth -> busWidth
                | _ -> failwithf $"Invalid component {comp.Type} for IO"
            | _ -> failwithf $"Lens access not yet implemented for {compSlotName}"
        )
        (fun value comp->
                let newType = 
                    match compSlotName with
                    | Buswidth ->
                        match comp.Type with
                        | Viewer _ -> Viewer value
                        | BusCompare1 (_, compareValue, dialogText) -> BusCompare1 (value, compareValue, dialogText)
                        | BusSelection (_, outputLSBit) -> BusSelection (value, outputLSBit)
                        | Constant1 (_, constValue, dialogText) -> Constant1 (value, constValue, dialogText)
                        | NbitsAdder _ -> NbitsAdder value
                        | NbitsAdderNoCin _ -> NbitsAdderNoCin value
                        | NbitsAdderNoCout _ -> NbitsAdderNoCout value
                        | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout value
                        | NbitsXor (_, arithmeticOp) -> NbitsXor (value, arithmeticOp)
                        | NbitsAnd _ -> NbitsAnd value
                        | NbitsNot _ -> NbitsNot value
                        | NbitsOr _ -> NbitsOr value
                        | NbitSpreader _ -> NbitSpreader value
                        | SplitWire _ -> SplitWire value
                        | Register _ -> Register value
                        | RegisterE _ -> RegisterE value
                        | Counter _ -> Counter value
                        | CounterNoLoad _ -> CounterNoLoad value
                        | CounterNoEnable _ -> CounterNoEnable value
                        | CounterNoEnableLoad _ -> CounterNoEnableLoad value
                        | Shift (_, shifterWidth, shiftType) -> Shift (value, shifterWidth, shiftType)
                        | BusCompare (_, compareValue) -> BusCompare (value, compareValue)
                        | Input _ -> Input value
                        | Constant (_, constValue) -> Constant (value, constValue)
                        | _ -> failwithf $"Invalid component {comp.Type} for buswidth"
                    | NGateInputs ->
                        match comp.Type with
                        | GateN (gateType, _) -> GateN (gateType, value)
                        | _ -> failwithf $"Invalid component {comp.Type} for gate inputs"
                    | IO _ ->
                        match comp.Type with
                        | Input1 (_, defaultValue) -> Input1 (value, defaultValue)
                        | Output _ -> Output value
                        | _ -> failwithf $"Invalid component {comp.Type} for IO"
                    | _ -> failwithf $"Lens access not yet implemented for {compSlotName}"
                { comp with Type = newType}
)


/// Return a Lens that can be used to read or update the value of a component slot integer in the component.
/// The value is contained in the ComponentType part of a Component record.
/// The Component record will be found in various places, depending on the context.
/// For Properties changes, the Component record will be in the Model under SelectedComponent.
/// For changes in a newly created component the component is created by CatalogueView.createComponent.
/// A partial implementation of this function would be OK for MVP.
/// NB - the Lens cannot be part of the slot record because the Lens type can change depending on 'PINT.
/// Maybe this will be fixed by using a D.U. for the slot type: however for MVP
/// we can simplify things by dealing only with int parameters.
let modelToSlot_ (slot: ParamSlot) : Optics.Lens<Model, int> =
    modelToSymbols
    >-> symbolsToSymbol_ (ComponentId slot.CompId)
    >-> symbolToComponent_
    >-> compSlot_ slot.CompSlot


/// Get parameter bindings of current loaded component
let getDefaultBindings model = 
    model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty


/// Get parameter slots of current loaded component
let getParamSlots model =
    model |> get paramSlotsOfModel_ |> Option.defaultValue Map.empty


/// Get parameter bindings of a loaded component
let getDefaultBindingsOfLC loadedComponent = 
    loadedComponent |> get defaultBindingsOfLC_ |> Option.defaultValue Map.empty


/// Get parameter slots of a loaded component
let getParamSlotsOfLC loadedComponent =
    loadedComponent |> get paramSlotsOfLC_ |> Option.defaultValue Map.empty


/// Tries to evaluate a parameter expression given a set of parameter bindings
/// Returns a ParamInt if successful, or ParamError if not
let tryEvaluateExpression
    (paramBindings: ParamBindings)
    (paramExpr: ParamExpression)
    : Result<ParamInt, ParamError> =

    // Expression needs to be evaluated recursively
    let rec recursiveEvaluation (expr: ParamExpression) : ParamExpression =
        match expr with
        | PInt _ -> expr    // Constant, nothing needs to be changed
        | PParameter name -> 
            match Map.tryFind name paramBindings with
            | Some evaluated -> evaluated
            | None -> PParameter name
        | PAdd (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l + r)
            | newLeft, newRight -> PAdd (newLeft, newRight)
        | PSubtract (left, right) -> 
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l - r)
            | newLeft, newRight -> PSubtract (newLeft, newRight)
        | PMultiply (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l * r)
            | newLeft, newRight -> PMultiply (newLeft, newRight)
        | PDivide (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l / r)
            | newLeft, newRight -> PDivide (newLeft, newRight)
        | PRemainder (left, right) ->
            match recursiveEvaluation left, recursiveEvaluation right with
            | PInt l, PInt r -> PInt (l % r)
            | newLeft, newRight -> PRemainder (newLeft, newRight)
    
    let rec collectUnresolved expr =
        match expr with
        | PInt _ -> []
        | PParameter (ParamName name) -> [name] // Only collect unresolved parameters
        | PAdd (left, right) | PSubtract (left, right)
        | PMultiply (left, right) | PDivide (left, right) 
        | PRemainder (left, right) -> collectUnresolved left @ collectUnresolved right

    match recursiveEvaluation paramExpr with
    | PInt evaluated -> Ok evaluated
    | unresolvedExpr ->
        let unresolvedParams = collectUnresolved unresolvedExpr |> List.distinct
        match unresolvedParams with
        | p :: _ -> Error $"{p} has not been defined as a parameter on this sheet"
        | _ -> failwithf "List of unresolved parameters cannot be empty"


/// Evaluates a parameter expression for given parameter bindings
/// Can only be used when it is known that the expression is valid for these bindings
let evaluateExpression
    (paramBindings: ParamBindings)
    (paramExpr: ParamExpression)
    : ParamInt =

    match tryEvaluateExpression paramBindings paramExpr with
    | Ok value -> value
    | Error err -> failwithf $"Known valid expression evaluation threw error '{err}'"


let rec renderParamExpression (expr: ParamExpression) (precedence:int) : string =
    // TODO refactor ParamExpression DU and this function to to elminate duplication
    // for multiple binary operators. Could use a local function here, but the better
    // solution would be refactoring the DU.
    match expr with
    | PInt value -> string value
    | PParameter (ParamName name) -> name
    | PAdd (left, right) -> 
        let currentPrecedence = 1;
        if (precedence > currentPrecedence) then
            "(" + (renderParamExpression left currentPrecedence )+ "+" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "+" + renderParamExpression right currentPrecedence
    | PSubtract (left, right) -> 
        let currentPrecedence = 1;
        if (precedence > currentPrecedence) then
            "(" + (renderParamExpression left currentPrecedence )+ "-" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "-" + renderParamExpression right currentPrecedence
    | PMultiply (left, right) -> 
        let currentPrecedence = 2;
        if (precedence > currentPrecedence) then
            "(" + (renderParamExpression left currentPrecedence )+ "*" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "*" + renderParamExpression right currentPrecedence
    | PDivide (left, right) -> 
        let currentPrecedence = 2;
        if (precedence > currentPrecedence) then
            "(" + (renderParamExpression left currentPrecedence )+ "/" + renderParamExpression right currentPrecedence + ")"
        else renderParamExpression left currentPrecedence + "/" + renderParamExpression right currentPrecedence
    | PRemainder (left, right) -> 
        let currentPrecedence = 3;
        "(" + renderParamExpression left currentPrecedence + "%" + renderParamExpression right currentPrecedence + ")" 


/// Get a loaded component from its name
/// Returns the currently open loaded component if no name is specified
let getLoadedComponent (sheetName: string option) (model: Model): LoadedComponent =
    let project = Option.get model.CurrentProj
    let lcName = sheetName |> Option.defaultValue project.OpenFileName
    project.LoadedComponents |> List.find (fun lc -> lc.Name = lcName)


/// Get a component from its ID and the name of the sheet that it is on
let getComponentById (model: Model) (sheetName: string) (compId: string): Component = 
    let project = Option.get model.CurrentProj
    
    // Need to use current sheet symbols if sheet is open, otherwise loaded component
    if sheetName = project.OpenFileName
    then model.Sheet.GetComponentById <| ComponentId compId
    else
        getLoadedComponent (Some sheetName) model
        |> fun lc -> lc.CanvasState
        |> fst
        |> List.find (fun comp -> comp.Id = compId)


/// Check that an expression passes its constraints for a given set of bindings
let checkConstraints
    (paramBindings: ParamBindings)
    (exprSpec: ConstrainedExpr)
    : Result<Unit, ParamError> =

    let checkConstraint constr =
        let comparator, constrExpr, errMsg = 
            match constr with
            | MaxVal (expr, err) -> (<=), expr, err
            | MinVal (expr, err) -> (>=), expr, err

        let evalExpr = tryEvaluateExpression paramBindings exprSpec.Expression
        let evalConstr = tryEvaluateExpression paramBindings constrExpr

        match evalExpr, evalConstr with
        | Ok value, Ok limit when comparator value limit -> Ok ()
        | Error valueErr, _ -> Error valueErr
        | _ -> Error errMsg

    exprSpec.Constraints
    |> List.map checkConstraint
    |> List.filter Result.isError
    |> function
       | [] -> Ok ()
       | firstError :: _ -> firstError


/// Checks whether all param slots on a sheet their constraints
/// Edited bindings override default bindings on that sheet
/// Returns the first broken constraint
let rec checkAllCompSlots
    (model: Model)
    (sheetName: string)
    (editedBindings: ParamBindings)
    : Result<Unit, ParamError> =

    // Get bindings of sheet that component is on
    let toplevelLC = getLoadedComponent (Some sheetName) model
    let toplevelBindings = getDefaultBindingsOfLC toplevelLC
    let updatedBindings = Helpers.mapUnion editedBindings toplevelBindings

    // Add detailed component information to constraint error messages
    let addDetailedErrorMsg (slot: ParamSlot) (spec: ConstrainedExpr): ConstrainedExpr =
        let renderedExpr = renderParamExpression spec.Expression 0
        let slotText =
            match slot.CompSlot with
            | Buswidth | IO _ -> $"bus width of {renderedExpr}"
            | NGateInputs -> $"{renderedExpr} inputs"
            | CustomCompParam param -> $"parameter binding of {param} = {renderedExpr}"
            | _ -> failwithf $"Cannot not have constraints on slot {slot.CompSlot}"

        let comp = getComponentById model sheetName slot.CompId
        let compInfo = $"{comp.Label} has {slotText}"

        let addDetail constr =
            match constr with
            | MaxVal (expr, err) -> MaxVal (expr, $"{compInfo}. {err}.")
            | MinVal (expr, err) -> MinVal (expr, $"{compInfo}. {err}.")

        let detailedConstraints = spec.Constraints |> List.map addDetail
        {spec with Constraints = detailedConstraints}

    // Check whether a param slot meets its constraints
    let checkSlot (slot: ParamSlot) (spec: ConstrainedExpr) = 
        match slot.CompSlot with
        | CustomCompParam _ ->
            // Get custom component and corresponding loaded component
            let comp = getComponentById model sheetName slot.CompId
            let customComponent = 
                match comp.Type with
                | Custom c ->  c
                | _ -> failwithf "Custom component must have custom component type"
            let customLC = getLoadedComponent (Some customComponent.Name) model
            
            // Create updated set of integer constant bindings for custom component
            let customBindings = 
                customComponent.ParameterBindings
                |> Option.defaultValue Map.empty
                |> Map.map (fun _ expr -> evaluateExpression updatedBindings expr)
                |> Map.map (fun _ value -> PInt value)

            let defaultBindings = getDefaultBindingsOfLC customLC
            let mergedBindings = Helpers.mapUnion customBindings defaultBindings

            // Check all slots inside custom component
            checkAllCompSlots model customComponent.Name mergedBindings
            |> Result.mapError (fun err -> $"{comp.Label}.{err}")

        | SheetParam _ -> failwithf "Sheet parameter cannot be slot in a component"
        | _ -> checkConstraints updatedBindings spec

    let paramSlots =
        model
        |> getLoadedComponent (Some sheetName)
        |> getParamSlotsOfLC

    // Check all component slots for broken constraints, and return first error found
    paramSlots
    |> Map.map addDetailedErrorMsg
    |> Map.map checkSlot
    |> Map.values
    |> List.ofArray
    |> List.filter Result.isError
    |> function
        | [] -> Ok ()
        | firstError :: _ -> firstError


/// Generates a ParameterExpression from input text
/// Operators are left-associative
/// (*), (/), and (%) have higher precedence than (+), (-)
/// Brackets should have highest precedence
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
        let pattern = @"(^-)?\d+|[a-zA-Z][a-zA-Z0-9]*|[()+\-*/%]"
        Regex.Matches(input, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList
    
    let validPattern = @"^[0-9a-zA-Z()+\-*/%\s]+$"  // Allow only numbers, letters, operators, spaces, and parentheses
    if text = "" then Error "Enter a value"
    elif not (Regex.IsMatch(text, validPattern)) then
        text
        |> Seq.filter (fun c -> not (Regex.IsMatch(c.ToString(), validPattern)))
        |> Seq.head
        |> fun c -> Error $"Character '{c}' is not allowed"
    else
        match tokenize text with
        | [] -> Error "Enter a value"
        | tokens ->
            match parseExpressionTokens tokens with
            | Ok (expr, []) -> Ok expr  // Ensure no leftover tokens
            | Ok (_, leftover) ->
                let leftoverChars = String.concat "" leftover
                Error $"Unexpected characters '{leftoverChars}' at end of expression"
            | Error e -> Error e


/// Returns true if an expression contains parameters, and false if not
let rec exprContainsParams (expression: ParamExpression): bool =
    match expression with
    | PInt _ -> false
    | PParameter _ -> true
    | PAdd (left, right)
    | PSubtract (left, right) 
    | PMultiply (left, right)
    | PDivide (left, right)
    | PRemainder (left, right)
        -> exprContainsParams left || exprContainsParams right


/// Adds or updates a parameter slot in loaded component param slots
/// Removes the entry if the expression does not contain parameters
let updateParamSlot
    (slot: ParamSlot)
    (exprSpec: ConstrainedExpr)
    (model: Model)
    : Model = 

    let paramSlots = getParamSlots model
    let newParamSlots =
        match exprContainsParams exprSpec.Expression with
        | true  -> Map.add slot exprSpec paramSlots
        | false -> Map.remove slot paramSlots

    set paramSlotsOfModel_ newParamSlots model


/// Add the parameter information from a newly created component to paramSlots
let addParamComponent
    (newCompSpec: NewParamCompSpec)
    (dispatch: Msg -> Unit)
    (compId: CommonTypes.ComponentId)
    : Unit =

    let compIdStr =
        match compId with
        | ComponentId txt -> txt
    
    let slot = {CompId = compIdStr; CompSlot = newCompSpec.CompSlot}
    let exprSpec = {
        Expression = newCompSpec.Expression
        Constraints = newCompSpec.Constraints
    }

    updateParamSlot slot exprSpec |> UpdateModel |> dispatch


/// Update a custom component parameter binding
let updateCustomCompParam
    (model: Model)
    (toplevelBindings: ParamBindings)
    (slot: ParamSlot)
    (expr: ParamExpression)
    (dispatch: Msg -> Unit)
    : Unit =

    /// Update a custom component with new bindings and I/O widths
    let createUpdatedComp
        (labelToWidth: Map<string, int>)
        (newBindings: ParamBindings)
        (custom: CustomComponentType)
        (comp: Component)
        : Component =

        let updateLabel (label, width) =
            let newWidth = Map.tryFind label labelToWidth |> Option.defaultValue width
            label, newWidth

        let updatedCustom = {
            custom with 
                InputLabels = List.map updateLabel custom.InputLabels
                OutputLabels = List.map updateLabel custom.OutputLabels
                ParameterBindings = Some newBindings
        }
        {comp with Type = Custom updatedCustom}

    // Get custom component from component ID
    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let custom =
        match comp.Type with
        | Custom c -> c
        | other -> failwithf $"Custom component cannot have type '{other}'"
    
    // Find corresponding loaded component for custom component
    let customLC = 
        match model.CurrentProj with
        | None -> failwithf "Must have open project"
        | Some proj ->
            proj.LoadedComponents
            |> List.find (fun lc -> lc.Name = custom.Name)

    let lcParamInfo =
        match customLC.LCParameterSlots with
        | Some paramInfo -> paramInfo
        | None -> failwithf "Parameterised custom component must have parameter info"

    let paramName = 
        match slot.CompSlot with
        | CustomCompParam param -> param
        | _ -> failwithf "Custom component slot must have CustomCompParam type"

    // New bindings for custom component
    let newBindings =
        custom.ParameterBindings
        |> Option.defaultValue Map.empty
        |> Map.add paramName expr

    // Evaluated bindings for custom component
    let constantBindings = 
        newBindings
        |> Map.map (fun _ expr -> PInt <| evaluateExpression toplevelBindings expr)

    // Merge default bindings of custom component with modified binding values
    let mergedBindings =
        Helpers.mapUnion constantBindings lcParamInfo.DefaultBindings

    // Create a map of IO label names on the custom component to their widths
    let ioLabelToWidth = 
        lcParamInfo.ParamSlots
        |> Map.toList
        |> List.choose (fun (paramSlot, expr) -> 
            let value = evaluateExpression mergedBindings expr.Expression
            // Only keep IO port slots
            match paramSlot.CompSlot with
            | IO label -> Some (label, value)
            | _ -> None 
        )
        |> Map.ofList

    // Update custom component and its symbol displayed on sheet    
    let newComponent = createUpdatedComp ioLabelToWidth newBindings custom comp
    (ComponentId comp.Id, comp, newComponent.Type)
    |> SymbolT.ChangeCustom
    |> BusWireT.Symbol
    |> SheetT.Wire
    |> Sheet
    |> dispatch

    // Do bus width inference
    let sheetDispatch msg = dispatch <| Sheet msg
    model.Sheet.DoBusWidthInference sheetDispatch


/// Update component and its symbol using new expression for slot
let updateComponent dispatch model paramBindings slot expr =
    let sheetDispatch sMsg = dispatch <| Sheet sMsg

    // Lookup component and get slot value
    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let compId = ComponentId comp.Id
    let value = evaluateExpression paramBindings expr

    printf $"Updating value of comp slot {slot.CompSlot} to {expr}"

    // Update component slot value
    match slot.CompSlot with
    | Buswidth | IO _ -> model.Sheet.ChangeWidth sheetDispatch compId value 
    | NGateInputs -> 
        match comp.Type with
        | GateN (gate, _) -> model.Sheet.ChangeGate sheetDispatch compId gate value
        | _ -> failwithf $"Gate cannot have type {comp.Type}"
    | CustomCompParam _ -> updateCustomCompParam model paramBindings slot expr dispatch
    | SheetParam _ -> failwithf "Cannot update sheet parameter using updateComponent"

    // Update most recent bus width
    match slot.CompSlot, comp.Type with
    | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
    | Buswidth, _ | IO _, _ -> dispatch <| ReloadSelectedComponent value
    | _ -> ()


/// Update the values of all parameterised components with a new set of bindings
/// This can only be called after the validity and constraints of all
/// expressions are checked
let updateComponents
    (model: Model)
    (newBindings: ParamBindings)
    (dispatch: Msg -> Unit)
    : Unit =

    model
    |> getParamSlots
    |> Map.map (fun _ expr -> expr.Expression)
    |> Map.iter (updateComponent dispatch model newBindings)


/// Create a generic input field which accepts and parses parameter expressions
/// Validity of inputs is checked by parser
/// Specific constraints can be passed by callee
let paramInputField
    (model: Model)
    (prompt: string)
    (compSpec: NewParamCompSpec)
    (comp: Component option)
    (dispatch: Msg -> unit)
    : ReactElement =

    let onChange inputExpr = 
        // Get parameter bindings and name of current sheet
        let paramBindings = getDefaultBindings model
        let sheetName = 
            model.CurrentProj
            |> Option.get
            |> fun proj -> proj.OpenFileName

        // Check that the expression satisfies all the required constraints
        let applyConstraints expr value =
            match compSpec.CompSlot with
            | SheetParam param ->   // Check all slots on current sheet
                match expr with
                | PInt _ -> 
                    let editedBindings = Map.add param (PInt value) paramBindings
                    checkAllCompSlots model sheetName editedBindings
                | _ -> Error "Parameter value must be a constant"
            | CustomCompParam param ->  // Check all slots in custom component
                let editedBindings = Map.empty |> Map.add param (PInt value)
                let paramComp = Option.get comp
                let customComp = 
                    match paramComp.Type with
                    | Custom cc -> cc
                    | _ -> failwithf "Only custom component can have bindings"
                checkAllCompSlots model customComp.Name editedBindings
                |> Result.mapError (fun err -> $"{paramComp.Label}.{err}")
            | _ ->  // Only need to check slot of selected component
                let exprSpec = {Expression = expr; Constraints = compSpec.Constraints}
                checkConstraints paramBindings exprSpec

        // Either update component or prepare creation of new component
        let useExpr expr value =
            // Update PopupDialogInfo for new component creation and error messages
            let newCompSpec = {compSpec with Expression = expr; Value = value}
            dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Ok newCompSpec)
            match comp with
            | Some c ->
                // Update existing component
                let exprSpec = {Expression = expr; Constraints = compSpec.Constraints}
                let slot = {CompId = c.Id; CompSlot = compSpec.CompSlot}
                // TODO: Use a flag to control whether the component auto-updates
                updateComponent dispatch model paramBindings slot expr
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        // Parse, evaluate, and check constraints of input expression
        let parsedExpr = parseExpression inputExpr
        let newVal = Result.bind (tryEvaluateExpression paramBindings) parsedExpr
        let constraintCheck = 
            match parsedExpr, newVal with
            | Ok expr, Ok value -> applyConstraints expr value
            | Error err, _ | _, Error err -> Error err 

        match newVal, constraintCheck, parsedExpr with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ | _, Error err, _ ->
            dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    // Get the input expression and error message as strings    
    let inputResult = 
        model.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind compSpec.CompSlot

    let boxValue, exprText, errText =
        match inputResult with
        | Some (Ok spec) -> spec.Value, renderParamExpression spec.Expression 0, ""
        | Some (Error err) -> compSpec.Value, string compSpec.Value, err
        | None -> compSpec.Value, string compSpec.Value, ""

    // Prompt, input field, potential error message, and result box
    Field.div [] [
        Label.label [] [str prompt]
        Field.div [Field.Option.HasAddons] [
            Control.div [] [
                Input.text [
                    if errText <> "" then
                        Input.Option.CustomClass "is-danger"
                    Input.Props [
                        OnFocus (getTextEventValue >> onChange)
                        OnPaste preventDefault
                        SpellCheck false
                        Name prompt
                        AutoFocus true
                        Style [Width "200px"]
                    ]
                    Input.DefaultValue <| exprText
                    Input.Type Input.Text
                    Input.OnChange (getTextEventValue >> onChange)
                ]
            ]
            if errText = "" && string boxValue <> exprText then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str (string boxValue)
                    ]
                ]
        ]
        p [Style [Color Red]] [str errText]
    ]


/// True if parameter input field for given slot has valid input
let paramInputIsValid (slot: CompSlotName) (model: Model): bool =
    model.PopupDialogData.DialogState
    |> function
       | Some specs -> Map.find slot specs |> Result.isOk
       | None -> failwithf "Dialog state must exist for input box"


/// Get the component specification from a parameter input field
/// Can only be called once it is known that the input expression meets constraints
let getParamFieldSpec (slotName: CompSlotName) (model: Model): NewParamCompSpec =
    let inputFieldDialog = 
        match model.PopupDialogData.DialogState with
        | Some inputSpec -> Map.find slotName inputSpec
        | None -> failwithf "Param input field must set new param info"
    
    match inputFieldDialog with
    | Ok paramSpec -> paramSpec
    | Error err -> failwithf $"Failed to extract expression due to error '{err}'"


/// Create a popup box with a parameter input field
let paramPopupBox
    (text: PopupConfig)
    (compSpec: NewParamCompSpec)
    (comp: Component option)
    (buttonAction: Model -> Unit)
    (dispatch: Msg -> Unit)
    : Unit =

    let inputField model' = paramInputField model' text.Prompt compSpec comp dispatch

    // Disabled if any constraints are violated
    let isDisabled model' = not <| paramInputIsValid compSpec.CompSlot model'

    // Create parameter input field
    dispatch <| AddPopupDialogParamSpec (compSpec.CompSlot, Ok compSpec)
    dialogPopup text.Title inputField text.Button buttonAction isDisabled [] dispatch


/// Create a popup that allows a parameter with an integer value to be added
let addParameterBox dispatch =
    // Dialog popup config
    let title = "Add new parameter"
    let textPrompt _ = Field.div [] [str "Parameter name"]
    let hint = "Enter a name"
    let intPrompt _ = Field.div [] [str "Parameter value"]
    let defaultVal = 1
    let buttonText = "Set value"

    let body = dialogPopupBodyTextAndInt textPrompt hint intPrompt defaultVal dispatch

    // Create empty parameter info if currently None
    let initParamInfo model =
        let loadedComponent = 
            model
            |> get openLoadedComponentOfModel_
            |> function
               | Some lc -> lc
               | None -> failwithf "Must have open sheet" 

        let defaultInfo =  Some {DefaultBindings = Map.empty; ParamSlots = Map.empty}
        let updatedLC = 
            match loadedComponent.LCParameterSlots with
            | Some _ -> loadedComponent
            | None -> {loadedComponent with LCParameterSlots = defaultInfo}
        dispatch <| UpdateModel (set openLoadedComponentOfModel_ updatedLC)

    // Update the parameter value then close the popup
    let buttonAction model =
        initParamInfo model // Needed before adding first parameter
        let newParamName = getText model.PopupDialogData
        let newValue = getInt model.PopupDialogData

        // Update bindings
        let newBindings =
            model 
            |> getDefaultBindings
            |> Map.add (ParamName newParamName) (PInt newValue)
        dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)
        dispatch ClosePopup

    // Parameter names can only contain letters and numbers
    let isDisabled model = 
        let newParamName = getText model.PopupDialogData
        not (Regex.IsMatch(newParamName, "^[a-zA-Z][a-zA-Z0-9]*$"))

    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// Creates a popup that allows a parameter integer value to be edited.
let editParameterBox model paramName dispatch   = 
    // Get current value
    let currentVal =
        model
        |> getDefaultBindings
        |> Map.find (ParamName paramName)
        |> function
           | PInt intVal -> intVal
           | _ -> failwithf "Edit parameter box only supports constants"

    // Treat parameter binding as a slot to enable input error checking
    let slot = SheetParam <| ParamName paramName

    // Update parameter bindings and components
    let buttonAction model' =
        // Get new parameter value from input field
        let compParamSpec = getParamFieldSpec slot model'
        let newValue = compParamSpec.Value

        // Update bindings
        let newBindings =
            model'
            |> getDefaultBindings
            |> Map.add (ParamName paramName) (PInt newValue) 
        dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)

        // Update components
        updateComponents model' newBindings dispatch 
        dispatch ClosePopup

    let popupConfig = {
        Title = "Edit parameter value"
        Prompt = $"New value for parameter {paramName}"
        Button = "Set value"
    }

    let paramSpec = {
        CompSlot = slot
        Expression = PInt currentVal
        Constraints = []
        Value = currentVal
    }

    paramPopupBox popupConfig paramSpec None buttonAction dispatch


// TODO: If the parameter is still used by some components this should
//       either be disabled or warn the user then delete all references
//       to the parameter
let deleteParameterBox model paramName dispatch = 
    let newBindings =
        model
        |> getDefaultBindings
        |> Map.remove (ParamName paramName)

    dispatch <| UpdateModel (set defaultBindingsOfModel_ newBindings)


/// UI to display and manage parameters for a design sheet.
/// TODO: add structural abstraction.
let private makeParamsField model dispatch =
    let sheetDefaultParams = getDefaultBindings model
    match sheetDefaultParams.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "No parameters have been added to this sheet." ]   
            br [] 
            Button.button 
                            [ Fulma.Button.OnClick(fun _ -> addParameterBox dispatch)
                              Fulma.Button.Color IsInfo
                            ] 
                [str "Add Parameter"]
            ]
    | false ->
    
        div [] [
            Label.label [] [str "Parameters"]
            p [] [str "These parameters have been added to this sheet." ]
            br []
            Table.table [
                        Table.IsBordered
                        Table.IsNarrow
                        Table.IsStriped
                        ] [
                thead [] [
                    tr [] [
                        th [] [str "Parameter"]
                        th [] [str "Value"]
                        th [] [str "Action"]
                    ]
                ]
                tbody [] (
                    sheetDefaultParams |> Map.toList |> List.map (fun (key, value) ->
                        let paramName =
                            match key with 
                            | ParameterTypes.ParamName s -> s
                        let paramVal = 
                            match value with
                            |ParameterTypes.PInt i -> string i
                            | x -> string x
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button 
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBox model (paramName) dispatch)
                                      Fulma.Button.Color IsInfo
                                    ] 
                                    [str "Edit"]
                                Button.button 
                                    [ Fulma.Button.OnClick(fun _ -> deleteParameterBox model (paramName) dispatch )
                                      Fulma.Button.Color IsDanger
                                    ] 
                                    [str "Delete"]
                                ]
                            ]
                        )
                    )
                ]
            Button.button 
                [ Fulma.Button.OnClick(fun _ -> addParameterBox dispatch)
                  Fulma.Button.Color IsInfo
                ]
                [str "Add Parameter"]
        ]


/// create a popup to edit in the model a custom component parameter binding
/// TODO - maybe comp should be a ComponentId with actual component looked up from model for safety?
let editParameterBindingPopup
    (model: Model)
    (paramName: ParamName)
    (comp: Component)
    (dispatch: Msg -> Unit)
    : Unit = 

    let customComponent =
        match comp.Type with
        | Custom c -> c
        | _ -> failwithf "Only custom components can have parameter bindings"

    let defaultVal =
        model
        |> getLoadedComponent (Some customComponent.Name)
        |> getDefaultBindingsOfLC
        |> Map.find paramName
        |> function
           | PInt value -> value
           | _ -> failwithf "Default bindings must be constants"

    let currentExpr =
        customComponent.ParameterBindings
        |> Option.defaultValue Map.empty
        |> Map.tryFind paramName
        |> Option.defaultValue (PInt defaultVal)

    // Popup dialog config
    let slot = CustomCompParam paramName

    // Update custom component when button is clicked
    let buttonAction model' =
        // Get new parameter expression from input field
        let paramBindings = getDefaultBindings model'
        let newSpec = getParamFieldSpec slot model'

        // Add new binding to param slots of current sheet
        let compSlot = {CompId = comp.Id; CompSlot = newSpec.CompSlot}
        let exprSpec = {
            Expression = newSpec.Expression
            Constraints = newSpec.Constraints
        }
        dispatch <| UpdateModel (updateParamSlot compSlot exprSpec)

        // Update custom component with new parameter value
        updateCustomCompParam model' paramBindings compSlot newSpec.Expression dispatch
        dispatch ClosePopup

    let popupConfig = {
        Title = "Edit parameter value"
        Prompt = $"New value for parameter {paramName}"
        Button = "Set value"
    }

    let currentSpec = {
        CompSlot = slot
        Expression = currentExpr
        Constraints = []
        Value = defaultVal
    }

    paramPopupBox popupConfig currentSpec (Some comp) buttonAction dispatch


/// UI component for custom component definition of parameter bindings
let makeParamBindingEntryBoxes model (comp:Component) (custom:CustomComponentType) dispatch =
    let ccParams = 
        match custom.ParameterBindings with
        | Some bindings -> bindings
        | None -> Map.empty
    
    let lcDefaultParams =
        match model.CurrentProj with
        | Some proj -> 
            let lcName = List.tryFind (fun c -> custom.Name = c.Name) proj.LoadedComponents
            match lcName with
            | Some lc ->
                match lc.LCParameterSlots with
                | Some paramInfo -> paramInfo.DefaultBindings
                | None -> Map.empty
            | None -> Map.empty
        | None -> Map.empty

    let mergedParamBindings : ParamBindings =
        lcDefaultParams
        |> Map.map (fun key value -> 
            match Map.tryFind key ccParams with
            | Some ccValue -> ccValue // Overwrite if key exists in cc
            | None -> value // use loaded component value if key does not exist in cc
            )

    match mergedParamBindings.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "This component does not use any parameters." ]
        ]   
    | false ->
        div [] [
            Label.label [] [str "Parameters"]
            p [] [str "This component uses the following parameters." ]
            br []
            Table.table [
                        Table.IsBordered
                        Table.IsNarrow
                        Table.IsStriped
                        ] [
                thead [] [
                    tr [] [
                        th [] [str "Parameter"]
                        th [] [str "Value"]
                        th [] [str "Action"]
                    ]
                ]
                tbody [] (
                    mergedParamBindings |> Map.toList |> List.map (fun (key, value) ->
                        let paramName =
                            match key with 
                            | ParameterTypes.ParamName s -> s
                        let paramVal = renderParamExpression value 0
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model (ParamName paramName) comp dispatch)
                                      Fulma.Button.Color IsInfo
                                    ] 
                                    [str "Edit"]
                            ]
                        ]
                    )
                )
            ]
        ]


/// Generate component slots view for design sheet properties panel
/// This is read-only.
let private makeSlotsField (model: ModelType.Model) : ReactElement =

    let paramSlots = getParamSlots model

    // Define a function to display PConstraint<int>
    let constraintExpression (constraint': ParamConstraint) =
        match constraint' with
        | MaxVal (expr, err) ->
            div [] [str ("Max: " + renderParamExpression expr 0)]
        | MinVal (expr, err) ->
            div [] [str ("Min: " + renderParamExpression expr 0)]
    
    let constraintMessage (constraint': ParamConstraint) =
        match constraint' with
            | MaxVal (_, err)  | MinVal (_, err) -> err


    /// UI component to display a single parameterised Component slot definition.
    /// This is read-only.
    let renderSlotSpec (slot: ParamSlot) (expr: ConstrainedExpr) =
        let slotNameStr =
            match slot.CompSlot with
            | Buswidth -> "Buswidth"
            | NGateInputs -> "Num inputs"
            | IO label -> $"Input/output {label}"
            | CustomCompParam param -> $"Parameter {param}"
            | SheetParam _ -> failwithf "Sheet parameter does not belong to a component"

        let name = if Map.containsKey (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols then
                        string model.Sheet.Wire.Symbol.Symbols[ComponentId slot.CompId].Component.Label
                    else
                        "[Nonexistent]" // TODO deleted component slots aren't removed!
        tr [] [
            td [] [
                b [] [str name] 
                br [] 
                str slotNameStr
            ]
            td [] [str (renderParamExpression expr.Expression 0)]
            td [
                Class (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                Tooltip.dataTooltip (List.map constraintMessage expr.Constraints |> String.concat "\n")
            ] (List.map constraintExpression expr.Constraints)
        ]

    /// UI component to display parametrised Component slot definitions 
    /// on the properties panel of a design sheet.
    /// This is read-only - changes can be made via the priperties of the component.
    let slotView (slotMap: ComponentSlotExpr) =
        div [Class "component-slots"] [ 
            label [Class "label"] [ str "Parameterised Components"]
            // br []
            p [] [str "This sheet contains the following parameterised components"]
            br []
            Table.table [
                Table.IsBordered
                Table.IsNarrow
                Table.IsStriped
                ] [
                thead [] [
                    tr [] [
                        th [] [str "Component"]
                        th [] [str "Expression"]
                        th [] [str "Constraint"]
                    ]
                ]
                tbody [] (
                        // slots |> Map.toList |> List.map (fun (slot, expr) -> renderSlotSpec slot expr
                        slotMap |> Map.toList |> List.map (fun (slot, expr) -> renderSlotSpec slot expr)
                    )
                ]
        ]

    match paramSlots.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameterised Components" ]
            p [] [str "This sheet does not contain any parameterised components." ]    
            ]
    | false-> slotView paramSlots

/// UI interface for viewing the parameter expressions of a component
let viewParameters (model: ModelType.Model) dispatch =
    
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [p [] [str $"Currently no parameters added into {comp.Label} sheet." ]    ]    
    | _ -> 
        div [] [
        makeParamsField model dispatch
        br []
        makeSlotsField model]
