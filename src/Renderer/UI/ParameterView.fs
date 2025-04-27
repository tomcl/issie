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
            // TODO-RYAN: Add support for lens access here
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
                    // TODO-RYAN: Add support for lens access here
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
let getLoadedComponent (model: Model) (sheetName: string option): LoadedComponent =
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
        getLoadedComponent model (Some sheetName)
        |> fun lc -> lc.CanvasState
        |> fst
        |> List.find (fun comp -> comp.Id = compId)


/// Get the error message from a constraint
let extractErrorMsg constr =
    match constr with
    | MinVal (_, err) | MaxVal (_, err) -> err


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


/// Evaluates the parameter slots for the given parameter bindings
/// and returns the first violated constraint
/// Recursive evaluation needed for parameter inheritance
/// TODO-RYAN: This function needs a MASSIVE tidy-up
let rec checkAllCompSlots
    (model: Model)
    (sheetName: string)
    (paramBindings: ParamBindings)
    : Result<Unit, ParamError> =

    let addDetailedErrorMsg (slot: ParamSlot) (spec: ConstrainedExpr): ConstrainedExpr =
        let renderedExpr = renderParamExpression spec.Expression 0
        let slotText =
            match slot.CompSlot with
            | Buswidth | IO _ -> $"bus width of {renderedExpr}"
            | NGateInputs -> $"{renderedExpr} inputs"
            | CustomCompParam param -> $"parameter binding to {param}"  // TODO-RYAN: This message needs more detail!
            | _ -> failwithf $"Cannot not have constraints on slot {slot.CompSlot}"

        // TODO-RYAN: Get rid of debug print statements
        // printfn $"Finding component slot in sheet {sheetName}"
        // printfn $"Component slot is {slot} and expr is {expr}"
        // printfn $"Components are {loadedComponent.CanvasState |> fst}"

        let comp = getComponentById model sheetName slot.CompId
        let compInfo = $"{comp.Label} has {slotText}"

        let addDetail constr =
            match constr with
            | MaxVal (paramExpr, err) -> MaxVal (paramExpr, $"{compInfo}. {err}.")
            | MinVal (paramExpr, err) -> MinVal (paramExpr, $"{compInfo}. {err}.")

        let detailedConstraints = spec.Constraints |> List.map addDetail
        {spec with Constraints = detailedConstraints}

    printf $"Checking constraints for all slots on {sheetName}"
    printf $"Updated bindings are {paramBindings}"

    // TODO-RYAN: Model should be the last argument to this function
    let paramSlots =
        getLoadedComponent model (Some sheetName)
        |> fun lc -> lc.LCParameterSlots
        |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}
        |> fun slots -> slots.ParamSlots

    // TODO-RYAN: This function can definitely be improved using lenses/prisms
    let evaluateSlot (slot: ParamSlot) (spec: ConstrainedExpr) = 
        match slot.CompSlot with
        | CustomCompParam param ->
            let comp = getComponentById model sheetName slot.CompId
            let customComponent = 
                match comp.Type with
                | Custom c ->  c
                | _ -> failwithf "Custom component must have custom component type"

            // Get loaded components for toplevel sheet and custom component on it
            let toplevelLC = getLoadedComponent model (Some sheetName)
            let customLC = getLoadedComponent model (Some customComponent.Name)
            
            // TODO-RYAN: Add some more functions to make the code for getting
            //            the default bindings and param slots way cleaner
            // TODO-RYAN: There's probably a lot of code overlap between here and cc update
            let toplevelBindings = 
                match toplevelLC.LCParameterSlots with
                | Some slots -> slots.DefaultBindings
                | None -> Map.empty
                |> Helpers.mapUnion paramBindings

            let customBindings = 
                customComponent.ParameterBindings
                |> Option.defaultValue Map.empty
                |> Map.map (fun _ expr -> evaluateExpression toplevelBindings expr)
                |> Map.map (fun _ value -> PInt value)

            printf $"Custom bindings are {customBindings}"

            let defaultBindings =
                match customLC.LCParameterSlots with
                | Some slots -> slots.DefaultBindings
                | None -> failwithf "Parameterised component must have bindings"

            let mergedBindings = Helpers.mapUnion customBindings defaultBindings

            checkAllCompSlots model customComponent.Name mergedBindings
            |> Result.mapError (fun err -> $"{comp.Label}.{err}")

        | SheetParam _ -> failwithf "Sheet parameter cannot be slot in a component"
        | _ -> checkConstraints paramBindings spec

    let result =
        paramSlots
        |> Map.map addDetailedErrorMsg
        |> Map.map evaluateSlot
        |> Map.values
        |> List.ofArray
        |> List.filter Result.isError

    if List.isEmpty result then Ok()
    else List.head result


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
let rec exprContainsParams 
    (expression: ParamExpression)
    : bool =

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
    

/// Update a custom component with new I/O component widths.
/// Used when these chnage as result of parameter changes.
let createUpdatedComponent (labelToEval: Map<string, int>) (newBindings: ParamBindings) (comp: Component) : Component =
    let updateLabels labels =
        labels |> List.map (fun (label, width) ->
            match Map.tryFind label labelToEval with
            | Some newWidth when newWidth <> width -> (label, newWidth) // Update width if changed
            | _ -> (label, width) // Keep the same if unchanged
        )
    
    match comp.Type with
    | Custom customComponent ->
        let updatedCustom = { customComponent with 
                                    InputLabels = updateLabels customComponent.InputLabels
                                    OutputLabels = updateLabels customComponent.OutputLabels
                                    ParameterBindings = Some newBindings }
        { comp with Type = Custom updatedCustom }
    | _ -> comp


// TODO-RYAN: Add some documentation for this function
// TODO-RYAN: This could probably be split into functions better
let updateCustomCompParam
    (model: Model)
    (toplevelBindings: ParamBindings)
    (slot: ParamSlot)
    (expr: ParamExpression)
    (dispatch: Msg -> Unit)
    : Unit =

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
    let newComponent = createUpdatedComponent ioLabelToWidth newBindings comp
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


/// Check the constraints on the parameter bindings of the current sheet
/// given a new expression for a parameter
/// Need to check recursively for inherited parameters
/// TODO-RYAN: This does not properly support parameter inheritance
/// TODO-RYAN: This function probably needs to be renamed
/// TODO-RYAN: Restructure and comment up this function
let rec checkBindings
    (model: Model)
    (sheetName: string)
    (editedBindings: ParamBindings)
    : Result<unit, ParamError> =

    // Need to extract parameter info using sheet name to support parameter inheritance
    let project = 
        match model.CurrentProj with
        | Some proj -> proj
        | None -> failwithf "Must have open project"

    let loadedComponent = 
        project.LoadedComponents
        |> List.find (fun lc -> lc.Name = sheetName)

    let paramInfo =
        match loadedComponent.LCParameterSlots with
        | Some slots -> slots
        | None -> {DefaultBindings = Map.empty; ParamSlots = Map.empty}

    // Update edited bindings
    let newBindings = Helpers.mapUnion editedBindings paramInfo.DefaultBindings

    // Add information about component and expression to constraint error message
    // let addDetailedErrorMsg (slot: ParamSlot) (expr: ConstrainedExpr): ConstrainedExpr =
    //     let renderedExpr = renderParamExpression expr.Expression 0
    //     let slotText =
    //         match slot.CompSlot with
    //         | Buswidth | IO _ -> $"bus width of {renderedExpr}"
    //         | NGateInputs -> $"{renderedExpr} inputs"
    //         | CustomCompParam param -> $"parameter binding to {param}"  // TODO-RYAN: This message needs more detail!
    //         | _ -> failwithf $"Cannot not have constraints on slot {slot.CompSlot}"

    //     // TODO-RYAN: Get rid of debug print statements
    //     printfn $"Finding component slot in sheet {sheetName}"
    //     printfn $"Component slot is {slot} and expr is {expr}"
    //     printfn $"Components are {loadedComponent.CanvasState |> fst}"

    //     let comp = getComponentById model sheetName slot.CompId
    //     let compInfo = $"Component {comp.Label} has {slotText}"

    //     let addDetail constr =
    //         match constr with
    //         | MaxVal (paramExpr, err) -> MaxVal (paramExpr, $"{compInfo}. {err}.")
    //         | MinVal (paramExpr, err) -> MinVal (paramExpr, $"{compInfo}. {err}.")

    //     let detailedConstraints = expr.Constraints |> List.map addDetail
    //     {expr with Constraints = detailedConstraints}

    // Add more useful error messages to all constraints
    // let detailedSpecs =
    //     paramInfo.ParamSlots
    //     |> Map.map addDetailedErrorMsg

    // Evaluate constraints for given bindings and take first error
    // TODO-RYAN: This will need to be updated for parameter inheritance
    // TODO-RYAN: Need to find a way of extracting detailed error messages from the results
    // detailedSpecs
    checkAllCompSlots model sheetName newBindings







/// Create a generic input field which accepts and parses parameter expressions
/// Validity of inputs is checked by parser
/// Specific constraints can be passed by callee
let paramInputField
    (model: Model)
    (prompt: string)
    (defaultValue: int) // TODO-RYAN: Should this be renamed/removed?
    (constraints: ParamConstraint list)
    (comp: Component option)
    (compSlotName: CompSlotName)
    (dispatch: Msg -> unit)
    : ReactElement =

    let onChange inputExpr = 
        let paramBindings = getDefaultBindings model
        let paramSlots = getParamSlots model

        // Only return first violated constraint
        // TODO-RYAN: Can definitely tidy up some unused code here
        // let checkConstraints expr =
        //     match evaluateConstraints model paramBindings paramSlots with
        //     | Ok () -> Ok ()
        //     | Error constr -> Error <| extractErrorMsg constr

        let exprResult = parseExpression inputExpr
        let newVal = Result.bind (tryEvaluateExpression paramBindings) exprResult
        // TODO-RYAN: This line is getting a bit long
        // TODO-RYAN: Also add support for CustomCompSlot
        // TODO-RYAN: This nested match is terrible, find a better way of writing the 
        //            param-setting parts of this function
        let sheetName = 
            model
            |> get openLoadedComponentOfModel_
            |> function
               | Some lc -> lc.Name
               | None -> failwithf "Must have open sheet"

        // TODO-RYAN: This needs to be revamped to support constraint checking
        //            for parameter bindings with parameter inheritance
        // TODO-RYAN: Nested match is ugly
        let constraintCheck =
            match compSlotName with
            | SheetParam paramName ->
                match exprResult with
                | Ok (PInt value) ->
                    let editedBindings = Map.add paramName (PInt value) paramBindings
                    checkBindings model sheetName editedBindings
                | Ok _ -> Error "Parameter value must be a constant"
                | Error err -> Error err
            | CustomCompParam paramName -> 
                match newVal with
                | Ok value ->
                    let editedBindings = Map.empty |> Map.add paramName (PInt value)
                    let ccName =
                        match comp with
                        | Some c -> 
                            match c.Type with
                            | Custom cc -> cc.Name
                            | _ -> failwithf "Only custom component can have bindings"
                        | _ -> failwithf "Custom component must already exist to edit param bindings"
                    let compName = 
                        match comp with
                        | Some c -> c.Label
                        | _ -> failwithf "Comp must exist"
                    checkBindings model ccName editedBindings
                    |> Result.mapError (fun err -> $"{compName}.{err}")
                | Error err -> Error err
            | _ -> 
                match exprResult with
                | Ok expr -> 
                    let exprSpec = {Expression = expr; Constraints = constraints}
                    checkConstraints paramBindings exprSpec
                | Error msg -> Error msg

        // Either update component or prepare creation of new component
        let useExpr expr value =
            // Update PopupDialogInfo for new component creation and error messages
            let newCompSpec = {
                CompSlot = compSlotName;
                Expression = expr;
                Constraints = constraints;
                Value = value;
            }
            dispatch <| AddPopupDialogParamSpec (compSlotName, Ok newCompSpec)
            match comp with
            | Some c ->
                // Update existing component
                let exprSpec = {Expression = expr; Constraints = constraints}
                let slot = {CompId = c.Id; CompSlot = compSlotName}
                updateComponent dispatch model paramBindings slot expr
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        match newVal, constraintCheck, exprResult with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ 
        | _, Error err, _ -> dispatch <| AddPopupDialogParamSpec (compSlotName, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    // TODO-RYAN: The box on the side needs some serious fixing
    let slots = getParamSlots model
    let inputString = 
        match comp with
        | Some c ->
            let key = {CompId = c.Id; CompSlot = compSlotName}
            if Map.containsKey key slots then
                renderParamExpression slots[key].Expression 0 // Or: Some (Map.find key slots)
            else
                defaultValue |> string
        | None -> defaultValue |> string
    
    let errText = 
        model.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind compSlotName
        |> Option.map (
            function
            | Ok _ -> "" 
            | Error err -> err
        )
        |> Option.defaultValue ""

    // Field name, input box, and potential error message
    Field.div [] [
        Label.label [] [str prompt]
        Field.div [Field.Option.HasAddons] [
            Control.div [] [
                Input.text [
                    if errText <> "" then
                        Input.Option.CustomClass "is-danger"
                    Input.Props [
                        OnPaste preventDefault
                        SpellCheck false
                        Name prompt
                        AutoFocus true
                        Style [Width "200px"]
                    ]
                    Input.DefaultValue <| inputString
                    Input.Type Input.Text
                    Input.OnChange (getTextEventValue >> onChange)
                ]
            ]
            if string defaultValue <> inputString then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str (string defaultValue)
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
/// TODO-RYAN: This should be a specific version of a more general parameter input box
let editParameterBox model paramName dispatch   = 
    // Get current value
    let currentVal =
        model
        |> getDefaultBindings
        |> Map.find (ParamName paramName)
        |> function
           | PInt intVal -> intVal
           | _ -> failwithf "Edit parameter box only supports constants"

    // Dialog box config
    let title = "Edit parameter value"
    let prompt = $"New value for parameter {paramName}"
    let buttonText = "Set value"
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

    // Constraints from parameter bindings are checked in paramInputField
    let inputField model' =
        paramInputField model' prompt currentVal [] None slot dispatch

    // Disabled if any constraints are violated
    let isDisabled model' = not <| paramInputIsValid slot model'

    let defaultParamSpec = {
        CompSlot = slot
        Expression = PInt currentVal
        Constraints = []
        Value = currentVal
    }

    // Create parameter input field
    dispatch <| AddPopupDialogParamSpec (slot, Ok defaultParamSpec)
    dialogPopup title inputField buttonText buttonAction isDisabled [] dispatch


// TODO-RYAN: NEED TO DISABLE THIS IF THERE ARE ANY EXPRS USING GIVEN PARAM
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
/// TODO-RYAN: We could just have a slot as input here
let editParameterBindingPopup
    (model: Model)
    (paramName: ParamName)
    (currentVal: ParamInt)
    (comp: Component)
    (dispatch: Msg -> Unit)
    : Unit = 

    // TODO-RYAN: Need to do some playing around to get currentVal rendering properly
    //            for these input boxes with parameters as well

    // Popup dialog config
    let title = "Edit parameter value"
    let prompt = $"New value for parameter {paramName}"
    let slot = CustomCompParam paramName
    let buttonText = "Set value"

    let inputField model' =
        paramInputField model' prompt currentVal [] (Some comp) slot dispatch

    // Update custom component when button is clicked
    let buttonAction model' =
        // Get new parameter expression from input field
        let paramBindings = getDefaultBindings model'
        let paramSpec = getParamFieldSpec slot model'

        // Add new binding to param slots of current sheet
        let compSlot = {CompId = comp.Id; CompSlot = paramSpec.CompSlot}
        let exprSpec = {
            Expression = paramSpec.Expression
            Constraints = paramSpec.Constraints
        }
        dispatch <| UpdateModel (updateParamSlot compSlot exprSpec)

        // TODO-RYAN: This line is too long and needs a better comment
        updateCustomCompParam model' paramBindings compSlot paramSpec.Expression dispatch
        dispatch ClosePopup

    // Constraints are checked by the parameter input field
    let isDisabled model' = not <| paramInputIsValid slot model'

    // TODO-RYAN:
    // 1. Huge amount of code reuse between this and edit parameter box
    // 2. is this default param spec really necessary????
    let defaultParamSpec = {
        CompSlot = slot
        Expression = PInt currentVal
        Constraints = []
        Value = currentVal
    }

    // Create parameter input field
    dispatch <| AddPopupDialogParamSpec (slot, Ok defaultParamSpec)
    dialogPopup title inputField buttonText buttonAction isDisabled [] dispatch


/// UI component for custom component definition of parameter bindings
let makeParamBindingEntryBoxes model (comp:Component) (custom:CustomComponentType) dispatch =
    let ccParams = 
        match custom.ParameterBindings with
        | Some bindings -> bindings
        | None -> Map.empty

    // TODO-RYAN: Remove this debug statement
    printf "ccParams is %A" ccParams
    
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
                                Button.button // TODO-ELENA: this is sketchy
                                    // TODO-RYAN: This whole thing needs to be refactored and merged with the default slots/spec layout
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model (ParamName paramName) 1 comp dispatch)
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
