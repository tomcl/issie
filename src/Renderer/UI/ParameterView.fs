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


/// Evaluates the parameter slots for the given parameter bindings
/// and returns the first violated constraint
/// Recursive evaluation needed for parameter inheritance
/// TODO-RYAN: This function needs a MASSIVE tidy-up
let rec evaluateConstraints
    (model: Model)
    (paramBindings: ParamBindings)
    (paramSlots: ComponentSlotExpr)
    : Result<Unit, ParamConstraint> =

    printf $"Evaluating constraints for bindings={paramBindings} and slots = {paramSlots}"

    let failedConstraints constraints expr =
        let resultExpression = tryEvaluateExpression paramBindings expr
        match resultExpression with
            | Ok value ->        
                constraints
                |> List.filter (fun constr ->
                    match constr with
                    | MaxVal (expr, _) -> 
                        match tryEvaluateExpression paramBindings expr with
                        | Ok maxValue -> value > maxValue
                        | Error _ -> false  // Failed to evaluate constraint
                    | MinVal (expr, _) -> 
                        match tryEvaluateExpression paramBindings expr with
                        | Ok minValue -> value < minValue
                        | Error _ -> false  // Failed to evaluate constraint
                    )
            | Error _ -> []
        


        // TODO-RYAN: remove these notes
        (*
            For each constraint:
                If slot type is CustomCompParam
                    1. Evaluate the param using current bindings
                    2. Get corresponding custom component
                    3. Get corresponding loaded component
                    4. Form new merged bindings from new cc and lc
                    5. Get param slots from lc
                    6. Recursive call
                Else:
                    Normal evaluation
        
        
        
        *)


    // TODO-RYAN: This function can definitely be improved using lenses/prisms
    let evaluateSlot (slot: ParamSlot) (spec: ConstrainedExpr) = 
        match slot.CompSlot with
        | CustomCompParam param ->
            let paramVal = tryEvaluateExpression paramBindings spec.Expression
            let comp =
                ComponentId slot.CompId
                |> model.Sheet.GetComponentById
            let customComponent = 
                match comp.Type with
                | Custom c -> c
                | _ -> failwithf "Custom component parameter can only belong to custom component"
            let loadedComponent =
                match model.CurrentProj with
                | Some proj -> proj
                | None -> failwithf "Must have open project"
                // TODO-RYAN: Improve the formatting here
                |> fun proj -> proj.LoadedComponents
                |> List.find (fun lc -> lc.Name = customComponent.Name)
            let lcBindings = 
                match loadedComponent.LCParameterSlots with
                | Some slots -> slots.DefaultBindings
                | None -> Map.empty
            let ccBindings = 
                match customComponent.ParameterBindings with
                | Some bindings -> bindings
                | None -> Map.empty
                // Convert these to constants
                // TODO-RYAN: This entire function is so messy. Fix it!
                |> Map.map (fun name expr ->
                    tryEvaluateExpression paramBindings expr
                    |> function
                       | Ok value -> PInt value
                       | Error err -> failwithf $"Parameter binding evaluation failed with error message {err}"
                )


            let mergedBindings = 
                lcBindings
                |> Map.map (fun key value -> 
                    match Map.tryFind key ccBindings with
                    | Some ccValue -> ccValue // Overwrite if key exists in cc
                    | None -> value // use loaded component value if key does not exist in cc
                )

            printf $"customComponent paramBindings are {customComponent.ParameterBindings}"
            printf $"paramBindings are {paramBindings}"
            printf $"lcBindings is {lcBindings}"
            printf $"ccBindings is {ccBindings}"
            printf $"Merged bindings is {mergedBindings}"

            let lcSlots = 
                match loadedComponent.LCParameterSlots with
                | Some slots -> slots.ParamSlots
                | None -> Map.empty
            evaluateConstraints model mergedBindings lcSlots
            |> function
               | Ok () -> []
               | Error failedConstraint -> [failedConstraint]
        | SheetParam _ -> failwithf "Sheet parameter cannot be slot in a component"
        | _ -> failedConstraints spec.Constraints spec.Expression

    let result =
        paramSlots
        |> Map.toList
        |> List.collect (fun (slot, spec) -> evaluateSlot slot spec)

    if List.isEmpty result then Ok()
    else Error <| List.head result


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
/// TODO-RYAN: This needs to be updated with expr instead of value to support parameter inheritance!
/// TODO-RYAN: Add some types and potentially rearrange the header for this function
let updateComponent dispatch model paramBindings slot expr =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    // TODO-RYAN: These failwithf messages really need to be improved
    // TODO-RYAN: Maybe we can use evaluateExpression and tryEvaluateExpression
    let value =
        match tryEvaluateExpression paramBindings expr with
        | Ok  intVal -> intVal
        | Error err -> failwithf $"Encountered error '{err}' while evaluating expression"

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
        // let newBindings =
        //     match custom.ParameterBindings with
        //     | Some bindings -> bindings
        //     | None -> Map.empty
        //     |> Map.add (ParamName param) (PInt value)
        
        // let newValue = getInt model'.PopupDialogData
        let newBindings =
            match custom.ParameterBindings with
            | Some bindings -> bindings
            | None -> Map.empty
            |> Map.add param (PInt value)
        
        let toplevelBindings = paramBindings

        printf $"toplevel bindings are {toplevelBindings}"

        let constantBindings = 
            newBindings
            |> Map.map (fun key expr ->
                match tryEvaluateExpression toplevelBindings expr with
                | Ok value -> PInt value
                | Error _ -> failwithf "Must be able to evaluate expression for binding"
            )

        // TODO-RYAN: currentSheet is not the right name for this
        let sheetBindings = 
            match currentSheet.LCParameterSlots with
            | Some slots -> slots.DefaultBindings
            | None -> failwithf "Parameterised custom component must have bindings"

        let mergedBindings = 
            sheetBindings
            |> Map.map (fun key value -> 
                match Map.tryFind key constantBindings with
                | Some ccValue -> ccValue // Overwrite if key exists in cc
                | None -> value // use loaded component value if key does not exist in cc
                )

        // TODO-RYAN: Make sure that all the failwithf's contain the error thrown if possible

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
                            match tryEvaluateExpression mergedBindings constrainedExpr.Expression with
                            | Ok expr -> expr
                            | Error _ -> failwithf "Failed to evaluate parameterised IO"
                        printf $"evaluatedvalue = {evaluatedValue}"
                        Some (label, evaluatedValue)
                    | _ -> None 
                )
                |> Map.ofSeq // Convert to map
            | None -> Map.empty


        // TODO-RYAN: See if using the old bindings here fixes parameter inheritance
        let oldBindings =
            match custom.ParameterBindings with
            | Some bindings -> bindings
            | None -> Map.empty
        // let newestComponent = updateCustomComponent labelToEval newBindings comp
        let newestComponent = updateCustomComponent labelToEval oldBindings comp
        let updateMsg: SymbolT.Msg = SymbolT.ChangeCustom (ComponentId comp.Id, comp, newestComponent.Type)
        let newModel, output = SymbolUpdate.update updateMsg model.Sheet.Wire.Symbol
        let updateModelSymbol (newMod: SymbolT.Model) (model: Model) = {model with Sheet.Wire.Symbol = newMod}
        updateModelSymbol newModel |> UpdateModel |> dispatch

        printf $"{comp}"
        let dispatchnew (msg: DrawModelType.SheetT.Msg) : unit = dispatch (Sheet msg)
        model.Sheet.DoBusWidthInference dispatchnew
    | SheetParam _ -> failwithf "Cannot update sheet parameter using updateComponent"


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


/// Get the error message from a constraint
let extractErrorMsg constr =
    match constr with
    | MinVal (_, err) | MaxVal (_, err) -> err


/// Check the constraints on the parameter bindings of the current sheet
/// given a new expression for a parameter
/// Need to check recursively for inherited parameters
/// TODO-RYAN: This does not properly support parameter inheritance
let rec checkBindings
    (model: Model)
    (sheetName: string)
    (paramName: ParamName)
    (value: ParamInt)
    : Result<unit, ParamError> =

    // Need to extract parameter info using sheet name to support parameter inheritance
    let project = 
        match model.CurrentProj with
        | Some proj -> proj
        | None -> failwithf "Must have open project"

    let paramInfo =
        project.LoadedComponents
        |> List.find (fun lc -> lc.Name = sheetName)
        |> fun lc -> lc.LCParameterSlots
        |> function
           | Some slots -> slots
           | None -> {DefaultBindings = Map.empty; ParamSlots = Map.empty}

    // Add new binding
    let newBindings = paramInfo.DefaultBindings |> Map.add paramName (PInt value)

    // Add information about component and expression to constraint error message
    let addDetailedErrorMsg (slot: ParamSlot) (expr: ConstrainedExpr): ConstrainedExpr =
        let renderedExpr = renderParamExpression expr.Expression 0
        let slotText =
            match slot.CompSlot with
            | Buswidth | IO _ -> $"bus width of {renderedExpr}"
            | NGateInputs -> $"{renderedExpr} inputs"
            | CustomCompParam param -> $"parameter binding to {param}"  // TODO-RYAN: This message needs more detail!
            | _ -> failwithf $"Cannot not have constraints on slot {slot.CompSlot}"

        let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
        let compInfo = $"Component {comp.Label} has {slotText}"

        let addDetail constr =
            match constr with
            | MaxVal (paramExpr, err) -> MaxVal (paramExpr, $"{compInfo}. {err}.")
            | MinVal (paramExpr, err) -> MinVal (paramExpr, $"{compInfo}. {err}.")

        let detailedConstraints = expr.Constraints |> List.map addDetail
        {expr with Constraints = detailedConstraints}

    // Add more useful error messages to all constraints
    let detailedSpecs =
        paramInfo.ParamSlots
        |> Map.map addDetailedErrorMsg

    // Evaluate constraints for given bindings and take first error
    // TODO-RYAN: This will need to be updated for parameter inheritance
    detailedSpecs
    |> evaluateConstraints model newBindings
    |> Result.mapError extractErrorMsg


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
        let paramBindings = getDefaultBindings model
        let paramSlots = getParamSlots model

        // Only return first violated constraint
        // TODO-RYAN: Can definitely tidy up some unused code here
        let checkConstraints expr =
            match evaluateConstraints model paramBindings paramSlots with
            | Ok () -> Ok ()
            | Error constr -> Error <| extractErrorMsg constr

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
        let constraintCheck =
            match compSlotName with
            | SheetParam paramName ->
                match exprResult with
                | Ok (PInt value) -> checkBindings model sheetName paramName value
                | Ok _ -> Error "Parameter value must be a constant"
                | Error err -> Error err
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
                updateComponent dispatch model paramBindings slot expr
                dispatch <| UpdateModel (updateParamSlot slot exprSpec)
            | None -> ()

        match newVal, constraintCheck, exprResult with
        | Ok value, Ok (), Ok expr -> useExpr expr value
        | Error err, _, _ 
        | _, Error err, _ -> dispatch <| AddPopupDialogParamSpec (compSlotName, Error err)
        | _ -> failwithf "Value cannot exist with invalid expression"

    let slots = getParamSlots model
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


/// True if parameter input field for given slot has valid input
let paramInputIsValid (slot: CompSlotName) (model: Model): bool =
    model.PopupDialogData.DialogState
    |> function
       | Some specs -> Map.find slot specs |> Result.isOk
       | None -> failwithf "Dialog state must exist for input box"


/// Update the values of all parameterised components with a new set of bindings
/// This can only be called after the validity and constraints of all
/// expressions are checked
let updateComponents
    (newBindings: ParamBindings)
    (model: Model)
    (dispatch: Msg -> Unit)
    : Unit =

    model
    |> getParamSlots
    |> Map.map (fun _ expr -> expr.Expression)
    |> Map.iter (updateComponent dispatch model newBindings)


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
        updateComponents newBindings model' dispatch 
        dispatch ClosePopup

    // Constraints from parameter bindings are checked in paramInputField
    let inputField model' =
        paramInputField model' prompt currentVal (Some currentVal) [] None slot dispatch

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

    // Popup dialog config
    let title = "Edit parameter value"
    let prompt = $"New value for parameter {paramName}"
    let slot = CustomCompParam paramName
    let buttonText = "Set value"

    let inputField model' =
        paramInputField model' prompt currentVal None [] None slot dispatch

    // TODO-RYAN: Update this comment
    // Update the parameter value then close the popup
    let buttonAction model' =
        // Get new parameter expression from input field
        let paramSpec = getParamFieldSpec slot model'
        
        // Add new binding to param slots of current sheet
        let compSlot = {CompId = comp.Id; CompSlot = paramSpec.CompSlot}
        let exprSpec = {
            Expression = paramSpec.Expression
            Constraints = paramSpec.Constraints
        }
        dispatch <| UpdateModel (updateParamSlot compSlot exprSpec)

        // New bindings for custom component
        let newBindings =
            custom.ParameterBindings
            |> Option.defaultValue Map.empty
            |> Map.add paramName paramSpec.Expression
        
        // Bindings of current sheet
        let toplevelBindings = getDefaultBindings model

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

        // Create updated custom component
        let newComponent = updateCustomComponent ioLabelToWidth newBindings comp

        // Update custom component symbol displayed on sheet
        let msg = SymbolT.ChangeCustom (ComponentId comp.Id, comp, newComponent.Type)
        let newSymbolModel = fst <| SymbolUpdate.update msg model.Sheet.Wire.Symbol
        let updateModelSymbol symbol model'' = {model'' with Sheet.Wire.Symbol = symbol}
        updateModelSymbol newSymbolModel |> UpdateModel |> dispatch

        // Do bus width inference
        let dispatchMsg msg' = dispatch (Sheet msg')
        model.Sheet.DoBusWidthInference dispatchMsg
    
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
                        let paramVal = 
                            match value with
                            |ParameterTypes.PInt i -> string i
                            | x -> string x
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramVal]
                            td [] [
                                Button.button // TODO-ELENA: this is sketchy
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model (ParamName paramName) (int paramVal) comp dispatch)
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
