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
 * See Common/parameterTypes.fs for the types used to represent parameters and parameter expressions.
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


/// returns the value of a parameter expression given a set of parameter bindings.
/// The simplified value will be either a constant or a linear combination of a constant and a parameter.
/// NB here 'PINT is not a polymorphic type but a type parameter that will be instantiated to int or bigint.
let evaluateParamExpression (paramBindings: ParamBindings) (paramExpr: ParamExpression) : Result<ParamInt, ParamError> =
    // changed the function to be recursive
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
        | p :: _ -> Error $"{p} has not been defined as a parameter on this sheet"
        | _ -> failwithf "List of unresolved parameters cannot be empty"


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


/// Evaluates a set of parameter bindings against a list of constraints
/// and returns a list of violated constraints
let evaluateConstraints
    (paramBindings: ParamBindings)
    (exprSpecs: ConstrainedExpr list)
    : Result<Unit, ParamConstraint list> =

    let failedConstraints konst expr =
        let resultExpression = evaluateParamExpression paramBindings expr
        match resultExpression with
            | Ok value ->        
                konst
                |> List.filter (fun constr ->
                    match constr with
                    | MaxVal (expr, errorMsg) -> 
                        match evaluateParamExpression paramBindings expr with
                        | Ok maxValue -> value > maxValue
                        | Error err -> false    // Failed to evaluate constraint
                    | MinVal (expr, _) -> 
                        match evaluateParamExpression paramBindings expr with
                        | Ok minValue -> value < minValue
                        | Error err -> false    // Failed to evaluate constraint
                    )
            | Error err -> []
    
    let result =
        exprSpecs
        |> List.collect (fun slot ->
            failedConstraints slot.Constraints slot.Expression)
    
    if List.isEmpty result then Ok()
    else Error result


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
        let pattern = @"-?\d+[a-zA-Z]*|[a-zA-Z]+\d*|[()+\-*/%]"
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


/// Get LoadedComponent for currently open sheet
/// This cannot fail, because LoadedComponent must be loaded for sheet to be open
let getCurrentSheet model = 
    let sheetName = 
        match model.CurrentProj with
        | Some proj -> proj.OpenFileName
        | None -> failwithf "Cannot find sheet because no project is open"

    model
    |> ModelHelpers.tryGetLoadedComponents
    |> List.tryFind (fun lc -> lc.Name = sheetName)
    |> function
       | Some lc -> lc
       | None -> failwithf "No loaded component with same name as open sheet"


/// Get default parameter bindings for LoadedComponent 
let getDefaultParams loadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots -> paramSlots.DefaultBindings
    | None -> Map.empty


/// Get default parameter slots for LoadedComponent 
let getParamSlots loadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some sheetinfo -> sheetinfo.ParamSlots
    | None -> Map.empty


/// Get current loaded component parameter info
/// Returns empty maps for ParamSlots and DefaultBindings if None
let getLCParamInfo (model: Model) =
    model
    |> get lcParameterInfoOfModel_
    |> Option.defaultValue {ParamSlots = Map.empty; DefaultBindings = Map.empty}

/// Update a custom component with new I/O component widths.
/// Used when these chnage as result of parameter changes.
let updateCustomComponent (labelToEval: Map<string, int>) (newBindings: ParamBindings) (comp: Component) : Component =
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

/// Use sheet component update functions to perform updates
let updateComponent dispatch model slot value =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let compId = ComponentId comp.Id

    // Update component slot value
    match slot.CompSlot with
    | Buswidth | IO _ -> model.Sheet.ChangeWidth sheetDispatch compId value 
    | NGateInputs -> 
        match comp.Type with
        | GateN (gateType, _) -> model.Sheet.ChangeGate sheetDispatch compId gateType value
        | _ -> failwithf $"Gate cannot have type {comp.Type}"
    | CustomCompParam param -> 
        let custom = 
            match comp.Type with
            | Custom c -> c
            | _ -> failwithf "Custom component slot must be in custom component"

        // TODO-RYAN: Should definitely be using a lens for this
        let currentSheet = 
            match model.CurrentProj with
            | None -> failwithf "Warning: testEditParameterBox called when no project is currently open"
            | Some project ->
                // Prepare dialog popup.
                project.LoadedComponents |> List.find (fun lc -> lc.Name = custom.Name)

        // TODO-RYAN: Use much better functional abstraction
        // let newValue = getInt model'.PopupDialogData
        let newBindings =
            match custom.ParameterBindings with
            | Some bindings -> bindings
            | None -> Map.empty
            |> Map.add (ParamName param) (PInt value)
        
        let labelToEval = 
            match currentSheet.LCParameterSlots with
            | Some sheetInfo ->
                printf $"paramslots = {sheetInfo.ParamSlots}"
                sheetInfo.ParamSlots
                |> Map.toSeq // Convert map to sequence of (ParamSlot, ConstrainedExpr<int>) pairs
                |> Seq.choose (fun (paramSlot, constrainedExpr) -> 
                    match paramSlot.CompSlot with
                    | IO label -> 
                        printf $"label = {label}"
                        let evaluatedValue = 
                            match evaluateParamExpression newBindings constrainedExpr.Expression with
                            | Ok expr -> expr
                            | Error _ -> 0
                        printf $"evaluatedvalue = {evaluatedValue}"
                        Some (label, evaluatedValue)
                    | _ -> None 
                )
                |> Map.ofSeq // Convert to map
            | None -> Map.empty


        let newestComponent = updateCustomComponent labelToEval newBindings comp
        let updateMsg: SymbolT.Msg = SymbolT.ChangeCustom (ComponentId comp.Id, comp, newestComponent.Type)
        let newModel, output = SymbolUpdate.update updateMsg model.Sheet.Wire.Symbol
        let updateModelSymbol (newMod: SymbolT.Model) (model: Model) = {model with Sheet.Wire.Symbol = newMod}
        updateModelSymbol newModel |> UpdateModel |> dispatch

        printf $"{comp}"
        let dispatchnew (msg: DrawModelType.SheetT.Msg) : unit = dispatch (Sheet msg)
        model.Sheet.DoBusWidthInference dispatchnew



    // Update most recent bus width
    match slot.CompSlot, comp.Type with
    | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
    | Buswidth, _ | IO _, _ -> dispatch <| ReloadSelectedComponent value
    | _ -> ()


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


/// Check the constraints on the parameter bindings of the current sheet
/// given a new expression for a parameter
let checkBindings
    (model: Model)
    (paramName: ParamName)
    (value: ParamInt)
    : Result<unit, ParamError> =

    // TODO-RYAN: Should really consider making getting bindings and slots a function
    let newBindings = 
        model
        |> get defaultBindingsOfModel_
        |> Option.defaultValue Map.empty
        |> Map.add paramName (PInt value)

    // TODO-RYAN: Should also really consider making this a function
    let paramSlots =
        model
        |> get paramSlotsOfModel_
        |> Option.defaultValue Map.empty

    let extractErrorMsg constr =
        match constr with
        | MinVal (_, err) | MaxVal (_, err) -> err

    // TODO-RYAN: Add some more helpful info to the constraint error messages
    let exprSpecs = paramSlots |> Map.values |> Array.toList

    evaluateConstraints newBindings exprSpecs
    |> Result.mapError (function lst -> List.head lst)
    |> Result.mapError extractErrorMsg


/// Adds or updates a parameter slot in loaded component param slots
/// Removes the entry if the expression does not contain parameters
let updateParamSlot
    (slot: ParamSlot)
    (exprSpec: ConstrainedExpr)
    (model: Model)
    : Model = 

    let paramSlots = 
        model
        |> get paramSlotsOfModel_
        |> Option.defaultValue Map.empty

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


/// Create a generic input field which accepts and parses parameter expressions
/// Validity of inputs is checked by parser
/// Specific constraints can be passed by callee
let paramInputField
    (model: Model)
    (prompt: string)
    (defaultValue: int)
    (currentValue: Option<int>)
    (constraints: ParamConstraint list)
    (comp: Component option)
    (compSlotName: CompSlotName)
    (dispatch: Msg -> unit)
    : ReactElement =

    let onChange inputExpr = 
        let paramBindings =
            model
            |> get defaultBindingsOfModel_
            |> Option.defaultValue Map.empty

        // Only return first violated constraint
        let checkConstraints expr =
            let exprSpec = {Expression = expr; Constraints = constraints}
            match evaluateConstraints paramBindings [exprSpec] with
            | Ok () -> Ok ()
                // Error (renderParamExpression expr)
            | Error (firstConstraint :: _) ->
                match firstConstraint with
                | MinVal (_, err) | MaxVal (_, err) -> Error err 
            | Error _ -> failwithf "Cannot have error list with no elements"

        let exprResult = parseExpression inputExpr
        let newVal = Result.bind (evaluateParamExpression paramBindings) exprResult
        // TODO-RYAN: This line is getting a bit long
        // TODO-RYAN: Also add support for CustomCompSlot
        let constraintCheck =
            match compSlotName with
            | SheetParam paramName -> Result.bind (checkBindings model paramName) newVal
            | _ -> Result.bind checkConstraints exprResult

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
                updateComponent dispatch model slot value
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        match newVal, constraintCheck, exprResult with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ 
        | _, Error err, _ -> dispatch <| AddPopupDialogParamSpec (compSlotName, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    let slots = model |> getCurrentSheet |> getParamSlots
    let inputString = 
        match comp with
        | Some c ->
            let key = {CompId = c.Id; CompSlot = compSlotName}
            if Map.containsKey key slots then
                renderParamExpression slots[key].Expression 0 // Or: Some (Map.find key slots)
            else
                currentValue |> Option.defaultValue defaultValue |> string
        | None -> currentValue |> Option.defaultValue defaultValue |> string
    
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
            if currentValue.IsSome && string currentValue.Value <> inputString then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str (string currentValue.Value)
                    ]
                ]
        ]
        p [Style [Color Red]] [str errText]
    ]


/// Update the values of all parameterised components with a new set of bindings
/// This can only be called after the validity and constraints of all
/// expressions are checked
let updateComponents
    (newBindings: ParamBindings)
    (model: Model)
    (dispatch: Msg -> Unit)
    : Unit =

    let evalExpression expr =
        match evaluateParamExpression newBindings expr with
        | Ok value -> value
        | Error _ ->  failwithf "Component update cannot have invalid expression"

    model
    |> get paramSlotsOfModel_
    |> Option.defaultValue Map.empty
    |> Map.map (fun _ expr -> evalExpression expr.Expression)
    |> Map.iter (updateComponent dispatch model)
    

/// Updates the LCParameterSlots DefaultParams section.
type UpdateInfoSheetChoise = 
    | DefaultParams of string * int * bool
    | ParamSlots of ParamSlot * ParameterTypes.ParamExpression * ParamConstraint list


let updateInfoSheetDefaultParams (currentSheetInfo:option<ParameterTypes.ParameterDefs>) (paramName: string) (value: int) (delete:bool)=
    if delete then
        match currentSheetInfo with
        | Some infoSheet -> 
            let newDefaultParams = infoSheet.DefaultBindings |> Map.remove (ParamName paramName)
            let currentSheetInfo = {infoSheet with DefaultBindings = newDefaultParams}
            Some currentSheetInfo
        | None -> None
    else
    match currentSheetInfo with
    | Some infoSheet -> 
        let newDefaultParams = infoSheet.DefaultBindings|> Map.add (ParamName paramName) (PInt value)
        let currentSheetInfo = {infoSheet with DefaultBindings = newDefaultParams}
        Some currentSheetInfo
    | None -> 
        let currentSheetInfo = {DefaultBindings= Map.ofList [(ParamName paramName, PInt value)]; ParamSlots= Map.empty}
        Some currentSheetInfo


let updateInfoSheetParamSlots (currentSheetInfo:option<ParameterTypes.ParameterDefs>) (paramSlot: ParameterTypes.ParamSlot) (expression: ParameterTypes.ParamExpression) (constraints: ParameterTypes.ParamConstraint list) =
    match currentSheetInfo with
    | Some infoSheet -> 
        let newParamSlots = infoSheet.ParamSlots |> Map.add paramSlot {Expression = expression; Constraints = constraints}
        let currentSheetInfo = {infoSheet with ParamSlots = newParamSlots}
        Some currentSheetInfo
    | None -> 
        let currentSheetInfo = {DefaultBindings= Map.empty; ParamSlots = Map.ofList [paramSlot, {Expression = expression; Constraints = constraints}]}
        Some currentSheetInfo


let updateParameter (project: CommonTypes.Project) (model: Model) =
    {model with CurrentProj = Some project}


let getParamsSlot (currentSheet: CommonTypes.LoadedComponent) =
    let getter = CommonTypes.lcParameterSlots_ >?> ParameterTypes.paramSlots_
    match currentSheet.LCParameterSlots with
    | Some _ -> currentSheet ^. getter
    | None -> None


/// This function can be used to update the DefaultParams or ParamSlots in the LCParameterSlots of a sheet based on the choise
/// Use case will be either when we want to add, edit or delete the sheet parameter or when we want to add a new component to the sheet
let modifyInfoSheet (project: CommonTypes.Project) (choise: UpdateInfoSheetChoise) dispatch=
    
    let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = project.OpenFileName)
    let updatedSheet = {currentSheet with LCParameterSlots = 
                                                        match choise with
                                                            | DefaultParams (paramName, value, delete) -> updateInfoSheetDefaultParams currentSheet.LCParameterSlots paramName value delete
                                                            | ParamSlots (paramSlot, expression, constraints) -> updateInfoSheetParamSlots currentSheet.LCParameterSlots paramSlot expression constraints}
    let updatedComponents = project.LoadedComponents
                            |> List.map (
                                fun lc ->
                                    if lc.Name = project.OpenFileName
                                    then updatedSheet
                                    else lc
                                )
    let newProject = {project with LoadedComponents = updatedComponents}
    updateParameter newProject |> UpdateModel |> dispatch

/// Creates a popup that allows a parameter integer value to be added.
let addParameterBox model dispatch =
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testAddParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Set parameter value"

        let textPrompt =
            fun _ ->
                div []
                    [
                        str "Specify the parameter name:"
                        br []
                        //str $"(current value is {model.ParameterValue})"
                    ]

        let intPrompt =
            fun _ ->
                div []
                    [
                        str "New value for the parameter:"
                        br []
                        //str $"(current value is {model.ParameterValue})"
                    ]

        let defaultVal = 1
        let body = dialogPopupBodyTextAndInt textPrompt "example: x" intPrompt defaultVal dispatch
        let buttonText = "Set value"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) -> 
                let newParamName = getText model'.PopupDialogData
                let newValue = getInt model'.PopupDialogData

                modifyInfoSheet (project) (DefaultParams (newParamName, newValue, false)) dispatch
                // Close popup window
                ClosePopup |> dispatch

        // Parameter Names can only be made out of letters and numbers
        let isDisabled = 
            fun (model': Model) -> 
                 let newParamName =  getText model'.PopupDialogData
                 not (Regex.IsMatch(newParamName, "^[a-zA-Z0-9]+$"))

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// Creates a popup that allows a parameter integer value to be edited.
/// TODO: this should be a special cases of a more general popup for parameter expressions?
let editParameterBox model paramName dispatch   = 
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testEditParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let title = "Edit parameter value"
        let currentValue = getDefaultParams currentSheet |> Map.find (ParamName paramName)
        let intPrompt = 
            fun _ ->
                div []
                    [
                        str $"New value for the parameter {paramName}:"
                        br []
                        str $"(current value: {currentValue})"
                    ]

        let defaultVal =
            match currentValue with
            | PInt intVal -> intVal
            | _ -> failwithf "Edit parameter box only supports integer bindings"


        // TODO-RYAN: make this all nice and tidy (and bug free)
        // paramInputField model title 

    //             let paramInputField
    // (model: Model)
    // (prompt: string)
    // (defaultValue: int)
    // (currentValue: Option<int>)
    // (constraints: ParamConstraint list)
    // (comp: Component option)
    // (compSlotName: CompSlotName)
    // (dispatch: Msg -> unit)
    // : ReactElement =

        let prompt = $"New value for parameter {paramName}"

        let constraints = 
            model
            |> get paramSlotsOfModel_
            |> Option.defaultValue Map.empty
            |> Map.toList
            |> List.map snd
            |> List.collect (function expr -> expr.Constraints)

        // TODO-RYAN: Figure out how to have something other than buswidth here
        //            - some changes are definitely needed to param input field to make it more generic
        // TODO-RYAN: Need to make it so that this only takes integers
        // let inputField = paramInputField model prompt defaultVal (Some defaultVal) constraints None Buswidth dispatch

        // let body = dialogPopupBodyOnlyInt intPrompt defaultVal dispatch
        let buttonText = "Set value"

        let slot = SheetParam <| ParamName paramName

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) -> 
                // let newParamName =  paramName 
                // let newValue = getInt model'.PopupDialogData

                // TODO-RYAN: Tidy this up
                let inputFieldDialog = 
                    match model'.PopupDialogData.DialogState with
                    | Some inputSpec -> Map.find slot inputSpec
                    | None -> failwithf "Param input field must set new param info"
                let compParamSpec =
                    match inputFieldDialog with
                    | Ok paramSpec -> paramSpec
                    | Error err -> failwithf $"Failed to extract expression due to error '{err}'"

                // TODO-RYAN: There needs to be some error checking forcing this to an int
                let newParamName = paramName
                let newValue =
                    match compParamSpec.Expression with
                    | PInt value -> value
                    | _ -> failwithf "Input field can only accept integers"

                modifyInfoSheet project (DefaultParams (newParamName,newValue,false)) dispatch
                let newBindings =
                    model'
                    |> getLCParamInfo
                    |> (fun info -> info.DefaultBindings)
                    |> Map.add (ParamName newParamName) (PInt newValue) 

                // Value must meet constraints if able to click button
                updateComponents newBindings model dispatch 
                dispatch <| ClosePopup

        // Disabled if any constraints are violated
        let isDisabled = 
            fun (model': Model) ->
                // TODO-RYAN: This code is duplicated everywhere - definitely needs to be
                //            wrapped into a function
                let newParamName =  paramName 
                let newValue = getInt model'.PopupDialogData
                let newBindings =
                    model'
                    |> getLCParamInfo 
                    |> (fun info -> info.DefaultBindings)
                    |> Map.add (ParamName newParamName) (PInt newValue) 

                let exprSpecs = 
                    model'
                    |> get paramSlotsOfModel_
                    |> Option.defaultValue Map.empty
                    |> Map.toList
                    |> List.map snd

                evaluateConstraints newBindings exprSpecs
                |> Result.isError

        let body model' =
            paramInputField model' prompt defaultVal (Some defaultVal) constraints None slot dispatch

        // TODO-RYAN: tidyup
        // dialogPopup title body buttonText buttonAction isDisabled [] dispatch
        dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let deleteParameterBox model paramName dispatch  = 
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testDeleteParameterBox called when no project is currently open"
    | Some project ->
        modifyInfoSheet (project) (DefaultParams(paramName,0,true)) dispatch


/// UI to display and manage parameters for a design sheet.
/// TODO: add structural abstraction.
let private makeParamsField model (comp:LoadedComponent) dispatch =
    let sheetDefaultParams = getDefaultParams comp
    match sheetDefaultParams.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "No parameters have been added to this sheet." ]   
            br [] 
            Button.button 
                            [ Fulma.Button.OnClick(fun _ -> addParameterBox model dispatch)
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
                [ Fulma.Button.OnClick(fun _ -> addParameterBox model dispatch)
                  Fulma.Button.Color IsInfo
                ]
                [str "Add Parameter"]
        ]


/// create a popup to edit in the model a custom component parameter binding
/// TODO - maybe comp should be a ComponentId with actual component looked up from model for safety?
let editParameterBindingPopup model paramName currValue (comp: Component) (custom: CustomComponentType) dispatch   = 
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testEditParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = custom.Name)
        let title = "Edit parameter value"
        let intPrompt = 
            fun _ ->
                div []
                    [
                        str $"New value for the parameter {paramName}:"
                        br []
                        str $"(current value: {currValue})"
                    ]


        // TODO-RYAN: body should be a parameter input field
        // TODO-RYAN: Find a way of adding in an expression evaluation box
        // TODO-RYAN: Add proper constraints

        let prompt = $"New value for parameter {paramName}"

        let inputField model' =
            // paramInputField model' prompt currValue (Some currValue) [] (Some comp) (CustomCompParam paramName) dispatch
            paramInputField model' prompt currValue None [] None (CustomCompParam paramName) dispatch

        let body = dialogPopupBodyOnlyInt intPrompt currValue dispatch
        let buttonText = "Set value"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) -> 
                let newParamName =  paramName 

                // TODO-RYAN: Implement the update function here
                let slot = CustomCompParam paramName

                let inputFieldDialog = 
                    match model'.PopupDialogData.DialogState with
                    | Some inputSpec -> Map.find slot inputSpec
                    | None -> failwithf "Param input field must set new param info"
                let compParamSpec =
                    match inputFieldDialog with
                    | Ok paramSpec -> paramSpec
                    | Error err ->
                        failwithf $"Received error message '{err}' when editing parameter binding"
                
                dispatch <| UpdateModel (updateParamSlot {CompId = comp.Id; CompSlot = compParamSpec.CompSlot} {Expression = compParamSpec.Expression; Constraints = compParamSpec.Constraints})

                let newValue = compParamSpec.Value

                // let newValue = getInt model'.PopupDialogData
                let newBindings =
                    match custom.ParameterBindings with
                    | Some bindings -> bindings
                    | None -> Map.empty
                    |> Map.add (ParamName newParamName) (PInt newValue)
                
                let labelToEval = 
                    match currentSheet.LCParameterSlots with
                    | Some sheetInfo ->
                        printf $"paramslots = {sheetInfo.ParamSlots}"
                        sheetInfo.ParamSlots
                        |> Map.toSeq // Convert map to sequence of (ParamSlot, ConstrainedExpr<int>) pairs
                        |> Seq.choose (fun (paramSlot, constrainedExpr) -> 
                            match paramSlot.CompSlot with
                            | IO label -> 
                                printf $"label = {label}"
                                let evaluatedValue = 
                                    match evaluateParamExpression newBindings constrainedExpr.Expression with
                                    | Ok expr -> expr
                                    | Error _ -> 0
                                printf $"evaluatedvalue = {evaluatedValue}"
                                Some (label, evaluatedValue)
                            | _ -> None 
                        )
                        |> Map.ofSeq // Convert to map
                    | None -> Map.empty

                printf $"labeltoeval = {labelToEval}"

                let newestComponent = updateCustomComponent labelToEval newBindings comp
                let updateMsg: SymbolT.Msg = SymbolT.ChangeCustom (ComponentId comp.Id, comp, newestComponent.Type)
                let newModel, output = SymbolUpdate.update updateMsg model.Sheet.Wire.Symbol
                let updateModelSymbol (newMod: SymbolT.Model) (model: Model) = {model with Sheet.Wire.Symbol = newMod}
                updateModelSymbol newModel |> UpdateModel |> dispatch

                printf $"{comp}"
                let dispatchnew (msg: DrawModelType.SheetT.Msg) : unit = dispatch (Sheet msg)
                model.Sheet.DoBusWidthInference dispatchnew
                dispatch <| ClosePopup

        // if constraints are not met, disable the button
        let isDisabled =
            fun (model': Model) ->
                let newParamName =  paramName 
                let newValue = getInt model'.PopupDialogData
                let newBindings =
                    match custom.ParameterBindings with
                    | Some bindings -> bindings
                    | None -> Map.empty
                    |> Map.add (ParamName newParamName) (PInt newValue)
                let exprSpecs = 
                    match currentSheet.LCParameterSlots with
                    | Some sheetInfo ->
                        printf $"paramslots = {sheetInfo.ParamSlots}"
                        sheetInfo.ParamSlots
                        |> Map.toList
                        |> List.map snd
                    | None -> List.Empty

                evaluateConstraints newBindings exprSpecs
                |> Result.isError

        // dialogPopup title body buttonText buttonAction isDisabled [] dispatch
        dialogPopup title inputField buttonText buttonAction isDisabled [] dispatch

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
            | Some lc -> getDefaultParams lc
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
                        let paramVal = 
                            match value with
                            |ParameterTypes.PInt i -> string i
                            | x -> string x
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button // TODO-ELENA: this is sketchy
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model paramName (int paramVal) comp custom dispatch)
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
let private makeSlotsField (model: ModelType.Model) (comp:LoadedComponent) dispatch = 
    let sheetParamsSlots = getParamsSlot comp

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

    match sheetParamsSlots with
        |None ->
            div [] [
                Label.label [] [ str "Parameterised Components" ]
                p [] [str "This sheet does not contain any parameterised." ]    
                ]
        |Some sheetParamsSlots -> slotView sheetParamsSlots

/// UI interface for viewing the parameter expressions of a component
let viewParameters (model: ModelType.Model) dispatch =
    
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [p [] [str $"Currently no parameters added into {comp.Label} sheet." ]    ]    
    | _ -> 
        match model.CurrentProj with
        |Some proj ->
            let sheetName = proj.OpenFileName
            let sheetLdc = proj.LoadedComponents |> List.find (fun ldc -> ldc.Name = sheetName)
            div [] [
            makeParamsField model sheetLdc dispatch
            br []
            makeSlotsField model sheetLdc dispatch]
        |None -> null
