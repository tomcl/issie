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
            | IO _ ->
                match comp.Type with
                | Input1 (busWidth, _) -> busWidth
                | Output busWidth -> busWidth
                | _ -> failwithf $"Invalid component {comp.Type} for IO"
            | InputDefault ->
                match comp.Type with
                | Input1 (_, defaultValue) -> int (Option.defaultValue 0I defaultValue)
                | _ -> failwithf $"Invalid component {comp.Type} for default value"
            | SplitNWidth idx ->
                match comp.Type with
                | SplitN (_, widths, _) ->
                    if idx >= 0 && idx < List.length widths then
                        widths[idx]
                    else failwithf $"SplitNWidth index %d{idx} out of range"
                | _ -> failwithf $"Invalid component {comp.Type} for SplitNWidth"
            | SplitNLSB idx ->
                match comp.Type with
                | SplitN (_, _, lsbs) ->
                    if idx >= 0 && idx < List.length lsbs then
                        lsbs[idx]
                    else failwithf $"SplitNLSB index %d{idx} out of range"
                | _ -> failwithf $"Invalid component {comp.Type} for SplitNLSB"
            | CustomCompParam paramName ->
                match comp.Type with
                | Custom customComp ->
                    // Look up the parameter value from the custom component's parameter bindings
                    match customComp.ParameterBindings with
                    | Some bindings ->
                        match Map.tryFind (ParamName paramName) bindings with
                        | Some (PInt value) -> value
                        | _ -> failwithf $"Parameter {paramName} not found in custom component {customComp.Name} bindings"
                    | None -> failwithf $"No parameter bindings found for custom component {customComp.Name}"
                | _ -> failwithf $"CustomCompParam can only be used with Custom components, not {comp.Type}"
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
                        | Shift (_, _, shiftType) -> Shift (value, shifterWidthFor value, shiftType)
                        | BusCompare (_, compareValue) -> BusCompare (value, compareValue)
                        | Input _ -> Input value
                        | Constant (_, constValue) -> Constant (value, constValue)
                        | _ -> failwithf $"Invalid component {comp.Type} for buswidth"
                    | IO _ ->
                        match comp.Type with
                        | Input1 (_, defaultValue) -> Input1 (value, defaultValue)
                        | Output _ -> Output value
                        | _ -> failwithf $"Invalid component {comp.Type} for IO"
                    | InputDefault ->
                        match comp.Type with
                        | Input1 (busWidth, _) -> Input1 (busWidth, Some (bigint value))
                        | _ -> failwithf $"Invalid component {comp.Type} for default value"
                    | SplitNWidth idx ->
                        match comp.Type with
                        | SplitN (n, widths, lsbs) ->
                            if idx < 0 || idx >= List.length widths then failwithf $"SplitNWidth index %d{idx} out of range"
                            let newWidths = widths |> List.mapi (fun i w -> if i = idx then value else w)
                            SplitN (n, newWidths, lsbs)
                        | _ -> failwithf $"Invalid component {comp.Type} for SplitNWidth"
                    | SplitNLSB idx ->
                        match comp.Type with
                        | SplitN (n, widths, lsbs) ->
                            if idx < 0 || idx >= List.length lsbs then failwithf $"SplitNLSB index %d{idx} out of range"
                            let newLsbs = lsbs |> List.mapi (fun i l -> if i = idx then value else l)
                            SplitN (n, widths, newLsbs)
                        | _ -> failwithf $"Invalid component {comp.Type} for SplitNLSB"
                    | CustomCompParam paramName ->
                        match comp.Type with
                        | Custom customComp ->
                            // Update the parameter value in the custom component's bindings
                            let newBindings = 
                                match customComp.ParameterBindings with
                                | Some bindings -> Map.add (ParamName paramName) (PInt value) bindings
                                | None -> Map.ofList [(ParamName paramName, PInt value)]
                            Custom { customComp with ParameterBindings = Some newBindings }
                        | _ -> failwithf $"CustomCompParam can only be used with Custom components, not {comp.Type}"
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


// evaluateParamExpression, renderParamExpression, parseExpression, and exprContainsParams
// have been moved to ParameterTypes module 


/// Evaluates a list of constraints got from slots against a set of parameter bindings to
/// check what values of param are allowed.
/// NB here 'PINT is not a polymorphic type but a type parameter that will be instantiated to int or bigint.
let evaluateConstraints
        (paramBindings: ParamBindings)
        (exprSpecs: ConstrainedExpr list)
        (dispatch: Msg -> unit)
            : Result<Unit, ParamConstraint list> =


    let failedConstraints konst expr =
        let resultExpression = ParameterTypes.evaluateParamExpression paramBindings expr
        match resultExpression with
            | Ok value ->
                konst
                |> List.filter (fun constr ->
                    // a bound that cannot be evaluated cannot pass the value it guards
                    match constr with
                    | MaxVal (expr, errorMsg) ->
                        match ParameterTypes.evaluateParamExpression paramBindings expr with
                        | Ok maxValue -> value > maxValue
                        | Error err -> // evaluation of constraint failed
                            let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                            dispatch <| SetPopupDialogText (Some (string errMsg))
                            true
                    | MinVal (expr, _) ->
                        match ParameterTypes.evaluateParamExpression paramBindings expr with
                        | Ok minValue -> value < minValue
                        | Error err -> // evaluation of constraint failed
                            let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                            dispatch <| SetPopupDialogText (Some (string errMsg))
                            true
                    )
            | Error err ->
                // an expression that cannot be evaluated fails its constraints: returning no
                // failures here would let an undefined value through the guard
                let errMsg = sprintf "Expression Evaluation of Constraint failed because %s" (string err)
                dispatch <| SetPopupDialogText (Some (string errMsg))
                [MinVal (expr, errMsg)]
    
    let result =
        exprSpecs
        |> List.collect (fun slot ->
            failedConstraints slot.Constraints slot.Expression)
    
    if List.isEmpty result then Ok()
    else Error result


// Generates a ParameterExpression from input text
// Operators are left-associative
// parseExpression has been moved to ParameterTypes module


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

/// Update a custom component's input/output label widths based on parameter evaluations
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
let updateComponent dispatch model slot (value:int) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let comp = model.Sheet.GetComponentById <| ComponentId slot.CompId
    let compId = ComponentId comp.Id

    // Update component slot value
    match comp.Type, slot.CompSlot with
    | BusSelection _, IO _ -> model.Sheet.ChangeLSB sheetDispatch compId (bigint value)
    | _, Buswidth | _, IO _ -> model.Sheet.ChangeWidth sheetDispatch compId value
    | _, InputDefault ->
        match comp.Type with
        | Input1 _ -> model.Sheet.ChangeInputValue sheetDispatch compId (bigint value)
        | _ -> failwithf $"Default value cannot be set on {comp.Type}"
    | _, SplitNWidth idx ->
        match comp.Type with
        | SplitN (n, widths, lsbs) ->
            if idx < 0 || idx >= List.length widths then failwithf $"SplitNWidth index %d{idx} out of range"
            let newWidths = widths |> List.mapi (fun i w -> if i = idx then value else w)
            model.Sheet.ChangeSplitN sheetDispatch compId n newWidths lsbs
        | _ -> failwithf $"SplitNWidth cannot be applied to {comp.Type}"
    | _, SplitNLSB idx ->
        match comp.Type with
        | SplitN (n, widths, lsbs) ->
            if idx < 0 || idx >= List.length lsbs then failwithf $"SplitNLSB index %d{idx} out of range"
            let newLsbs = lsbs |> List.mapi (fun i l -> if i = idx then value else l)
            model.Sheet.ChangeSplitN sheetDispatch compId n widths newLsbs
        | _ -> failwithf $"SplitNLSB cannot be applied to {comp.Type}"
    | _, CustomCompParam paramName ->
        // For custom component parameters, we need to update the parameter bindings
        match comp.Type with
        | Custom customComp ->
            let newBindings = 
                match customComp.ParameterBindings with
                | Some bindings -> Map.add (ParamName paramName) (PInt value) bindings
                | None -> Map.ofList [(ParamName paramName, PInt value)]
            
            // Get the custom component's loaded component to find parameter slot definitions
            match model.CurrentProj with
            | Some project ->
                let currentSheet = 
                    project.LoadedComponents
                    |> List.tryFind (fun lc -> lc.Name = customComp.Name)
                
                // Calculate updated label widths based on parameter evaluations
                let labelToEval = 
                    match currentSheet with
                    | Some sheet ->
                        match sheet.LCParameterSlots with
                        | Some sheetInfo ->
                            sheetInfo.ParamSlots
                            |> Map.toSeq
                            |> Seq.choose (fun (paramSlot, constrainedExpr) -> 
                                match paramSlot.CompSlot with
                                | IO label -> 
                                    let evaluatedValue = 
                                        match ParameterTypes.evaluateParamExpression newBindings constrainedExpr.Expression with
                                        | Ok expr -> expr
                                        | Error _ -> 0
                                    Some (label, evaluatedValue)
                                | _ -> None 
                            )
                            |> Map.ofSeq
                        | None -> Map.empty
                    | None -> Map.empty
                
                // Update the custom component with new parameter bindings and updated port widths
                let updatedCustom = updateCustomComponent labelToEval newBindings comp
                dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (compId, comp, updatedCustom.Type))))
            | None ->
                // Fallback to just updating bindings if no project context
                let newCustomComp = { customComp with ParameterBindings = Some newBindings }
                dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (compId, comp, Custom newCustomComp))))
        | _ -> failwithf $"CustomCompParam can only be used with Custom components"

    // Update most recent bus width
    match slot.CompSlot, comp.Type with
    | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
    | Buswidth, _ | IO _, _ -> dispatch <| ReloadSelectedComponent value
    | _ -> ()


// exprContainsParams has been moved to ParameterTypes module


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
        match ParameterTypes.exprContainsParams exprSpec.Expression with
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
            match evaluateConstraints paramBindings [exprSpec] dispatch with
            | Ok () -> Ok ()
                // Error (ParameterTypes.renderParamExpression expr)
            | Error (firstConstraint :: _) ->
                match firstConstraint with
                | MinVal (_, err) | MaxVal (_, err) -> Error err 
            | Error _ -> failwithf "Cannot have error list with no elements"

        let exprResult = ParameterTypes.parseExpression inputExpr
        let newVal = Result.bind (ParameterTypes.evaluateParamExpression paramBindings) exprResult
        let constraintCheck = Result.bind checkConstraints exprResult

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
                ParameterTypes.renderParamExpression slots[key].Expression 0 // Or: Some (Map.find key slots)
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

    /// A slot naming a component that has gone, or referring to a parameter that has gone, breaks
    /// the sheet invariant and cannot be pushed onto the canvas. Skip it rather than kill the app:
    /// the checks made when parameters and components are deleted are what keep this from happening.
    let liveSlotValue (slot: ParamSlot) (exprSpec: ConstrainedExpr) =
        match Map.containsKey (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols with
        | false ->
            JSHelpers.log $"Skipping parameter slot of component {slot.CompId}, which is not on this sheet"
            None
        | true ->
            match ParameterTypes.evaluateParamExpression newBindings exprSpec.Expression with
            | Ok value -> Some value
            | Error err ->
                JSHelpers.log $"Skipping parameter slot of component {slot.CompId}: {err}"
                None

    model
    |> get paramSlotsOfModel_
    |> Option.defaultValue Map.empty
    |> Map.iter (fun slot exprSpec ->
        match liveSlotValue slot exprSpec with
        | Some value -> updateComponent dispatch model slot value
        | None -> ())
    

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
                // a new parameter may be the missing ancestor that lets unbound same-named
                // parameters in the sheets below be bound to it
                dispatch <| CheckBindToTopOffers (ParameterAnalysis.NewParam (project.OpenFileName, ParamName newParamName))

        // Parameter Names can only be made out of letters and numbers
        let isDisabled = 
            fun (model': Model) -> 
                 let newParamName =  getText model'.PopupDialogData
                 not (Regex.IsMatch(newParamName, "^[a-zA-Z0-9]+$"))

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// Creates a popup that allows a parameter integer value to be edited.
/// TODO: this should be a special cases of a more general popup for parameter expressions?
let editParameterBox model parameterName dispatch   = 
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testEditParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let title = "Edit parameter value"
        match getDefaultParams currentSheet |> Map.tryFind (ParamName parameterName) with
        | None ->
            // the row was rendered from an older model in which the parameter still existed
            JSHelpers.log $"Cannot edit parameter {parameterName}: it is not defined on this sheet"
        | Some (PParameter _ | PAdd _ | PSubtract _ | PMultiply _ | PDivide _ | PRemainder _) ->
            dispatch <| SetPropertiesNotification (Notifications.errorPropsNotification
                $"Parameter {parameterName} is bound to an expression. Only integer parameter values can be edited here.")
        | Some currentValue ->
        let intPrompt =
            fun _ ->
                div []
                    [
                        str $"New value for the parameter {parameterName}:"
                        br []
                        str $"(current value: {currentValue})"
                    ]

        let defaultVal =
            match currentValue with
            | PInt intVal -> intVal
            | _ -> 1 // non-integer bindings are rejected above

        let body = dialogPopupBodyOnlyInt intPrompt defaultVal dispatch
        let buttonText = "Set value"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) -> 
                let newParamName =  parameterName 
                let newValue = getInt model'.PopupDialogData
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
                let newParamName =  parameterName 
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

                evaluateConstraints newBindings exprSpecs dispatch
                |> Result.isError

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// Human readable name of the slot a parameter expression fills, for use in messages.
let describeSlot (model: Model) (slot: ParamSlot) =
    let slotName =
        match slot.CompSlot with
        | Buswidth -> "Buswidth"
        | IO label -> $"Input/output {label}"
        | SplitNWidth idx -> $"SplitN output {idx} width"
        | SplitNLSB idx -> $"SplitN output {idx} LSB"
        | CustomCompParam paramName -> $"Custom parameter {paramName}"
        | InputDefault -> "Default value"
    match Map.tryFind (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols with
    | Some symbol -> $"{symbol.Component.Label}: {slotName}"
    | None -> $"[deleted component]: {slotName}"

/// A binding of a parameter of `sheetName` on an instance of that sheet no longer means anything
/// once the parameter has gone, so drop it from every other sheet in the project.
/// Bindings live in two places: the instance's own ParameterBindings, and a CustomCompParam slot
/// of the sheet the instance sits on.
let removeParamFromInstances (sheetName: string) (name: ParamName) (model: Model) : Model =
    let dropFromSheet (ldc: LoadedComponent) =
        let comps, conns = ldc.CanvasState
        let isInstance (comp: Component) =
            match comp.Type with
            | Custom custom -> custom.Name = sheetName
            | _ -> false
        let dropBinding (comp: Component) =
            match comp.Type with
            | Custom custom when custom.Name = sheetName && Option.exists (Map.containsKey name) custom.ParameterBindings ->
                {comp with Type = Custom {custom with ParameterBindings = Option.map (Map.remove name) custom.ParameterBindings}}
            | _ -> comp
        let instanceIds = comps |> List.filter isInstance |> List.map (fun comp -> comp.Id) |> Set.ofList
        let paramString = match name with | ParamName s -> s
        let dropSlots (slots: ComponentSlotExpr) =
            slots
            |> Map.filter (fun (slot: ParamSlot) _ ->
                match slot.CompSlot with
                | CustomCompParam p -> not (p = paramString && Set.contains slot.CompId instanceIds)
                | _ -> true)
        let ldc' =
            {ldc with
                CanvasState = List.map dropBinding comps, conns
                LCParameterSlots = ldc.LCParameterSlots |> Option.map (Optic.map paramSlots_ dropSlots)}
        match ldc'.CanvasState = ldc.CanvasState && ldc'.LCParameterSlots = ldc.LCParameterSlots with
        | true -> ldc
        | false -> {ldc' with LoadedComponentIsOutOfDate = true}
    let updateSheets (ldcs: LoadedComponent list) =
        ldcs
        |> List.map (fun ldc -> if ldc.Name = sheetName then ldc else dropFromSheet ldc)
    model
    |> Optic.map (projectOpt_ >?> loadedComponents_) updateSheets

/// Delete a sheet parameter. A slot referring to a parameter that does not exist is an undefined
/// design, so this refuses while any slot on the sheet still refers to it and says which ones.
let deleteParameterBox model parameterName dispatch  =
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: deleteParameterBox called when no project is currently open"
    | Some project ->
        let name = ParamName parameterName
        let sheet = getCurrentSheet model
        // custom component instances are not a special case: their slot expression is in the
        // parameters of the sheet they sit on, like the expression of any other slot.
        // A slot whose component has just been deleted is not a real use: it is pruned on save.
        let users =
            getParamSlots sheet
            |> ParameterTypes.slotsUsingParam name
            |> List.filter (fun (slot, _) -> Map.containsKey (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols)
        match users with
        | [] ->
            modifyInfoSheet project (DefaultParams (parameterName, 0, true)) dispatch
            dispatch <| UpdateModel (removeParamFromInstances sheet.Name name)
        | _ ->
            let body =
                div []
                    [ str $"Parameter {parameterName} cannot be deleted because it is still used by \
                            the following component slots on this sheet:"
                      br []
                      ul [Style [MarginLeft "20px"; ListStyleType "disc"]]
                          (users |> List.map (fun (slot, _) -> li [] [str (describeSlot model slot)]))
                      br []
                      str "Give each of them a value that does not use this parameter, then delete it." ]
            closablePopup $"Cannot delete parameter {parameterName}" body (div [] []) [] dispatch


/// UI to display and manage parameters for a design sheet.
/// TODO: add structural abstraction.
let private makeParamsField model (comp:LoadedComponent) dispatch =
    let sheetDefaultParams = getDefaultParams comp
    // parameters create dependencies across the whole design, so changing them under a running
    // simulation would leave it describing hardware that no longer exists
    let simIsOpen = ModelHelpers.simulationIsOpen model
    let simWarning =
        match simIsOpen with
        | false -> div [] []
        | true -> p [Style [Color "red"]] [str "Close all simulations to change the parameters of this sheet."]
    // What each parameter resolves to across the instances of this sheet under the current top:
    // one agreed value is shown as the real value; disagreeing instances are enumerated and the
    // default shown. Editing always targets the definition (the default); inherited values are
    // read-only annotations naming their source.
    let topSheetOpt, displayValues =
        match model.CurrentProj with
        | None -> None, Map.empty
        | Some proj ->
            let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
            match ParameterAnalysis.effectiveTopSheet ldcs with
            | None -> None, Map.empty
            | Some top -> Some top, ParameterAnalysis.displayValues ldcs top comp.Name
    /// annotation shown under a parameter's value, or None for a plain default
    let annotate (key: ParamName) (defaultText: string) : string * string option =
        let top = Option.defaultValue "" topSheetOpt
        match Map.tryFind key displayValues with
        | Some (ParameterAnalysis.ExactValue v) when string v <> defaultText ->
            string v, Some $"from {top}; default {defaultText}"
        | Some (ParameterAnalysis.MultipleValues (shown, values)) ->
            let describeValue (v, paths) =
                let examples =
                    paths
                    |> List.truncate 2
                    |> List.map (ParameterAnalysis.renderInstancePath top)
                    |> String.concat ", "
                let more = if List.length paths > 2 then ", …" else ""
                $"{v} at {examples}{more}"
            let note =
                values
                |> List.map describeValue
                |> String.concat "; "
            defaultText, Some $"{note}; showing default {shown}"
        | _ -> defaultText, None
    match sheetDefaultParams.IsEmpty with
    | true ->
        div [] [
            Label.label [] [ str "Parameters" ]
            p [] [str "No parameters have been added to this sheet." ]
            simWarning
            br []
            Button.button
                            [ Fulma.Button.OnClick(fun _ -> addParameterBox model dispatch)
                              Fulma.Button.Color IsInfo
                              Fulma.Button.Disabled simIsOpen
                            ]
                [str "Add Parameter"]
            ]
    | false ->

        div [] [
            Label.label [] [str "Parameters"]
            p [] [str "These parameters have been added to this sheet." ]
            simWarning
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
                        let defaultVal =
                            match value with
                            |ParameterTypes.PInt i -> string i
                            | x -> string x
                        let paramVal, note = annotate key defaultVal
                        tr [] [
                            td [] [str paramName]
                            td [] (
                                [str paramVal]
                                @ (match note with
                                   | Some noteText -> [p [Style [FontSize "11px"; Color "grey"]] [str noteText]]
                                   | None -> []))
                            td [] [
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBox model (paramName) dispatch)
                                      Fulma.Button.Color IsInfo
                                      Fulma.Button.Disabled simIsOpen
                                    ]
                                    [str "Edit"]
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> deleteParameterBox model (paramName) dispatch )
                                      Fulma.Button.Color IsDanger
                                      Fulma.Button.Disabled simIsOpen
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
                  Fulma.Button.Disabled simIsOpen
                ]
                [str "Add Parameter"]
        ]

/// Evaluate parameter expression using parameter bindings - exposed for external use

/// Helper function for simulation: resolve parameter expressions for a component
/// Returns the component type with resolved parameter values
// Create prisms for component type parameter updates using the existing Optics library
let buswidthPrism : Prism<ComponentType, int> =
    Prism.create
        (function
            | Viewer w | Input w | Output w 
            | NbitsAdder w | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
            | NbitsAnd w | NbitsNot w | NbitsOr w | NbitSpreader w | SplitWire w
            | Register w | RegisterE w | Counter w | CounterNoLoad w 
            | CounterNoEnable w | CounterNoEnableLoad w -> Some w
            | BusCompare1 (w, _, _) | Constant1 (w, _, _) | BusSelection (w, _) 
            | NbitsXor (w, _) | Shift (w, _, _) | BusCompare (w, _) 
            | Input1 (w, _) | Constant (w, _) -> Some w
            | _ -> None)
        (fun w compType ->
            match compType with
            | Viewer _ -> Viewer w
            | BusCompare1 (_, cv, dt) -> BusCompare1 (w, cv, dt)
            | BusSelection (_, lsb) -> BusSelection (w, lsb)
            | Constant1 (_, cv, dt) -> Constant1 (w, cv, dt)
            | NbitsAdder _ -> NbitsAdder w
            | NbitsAdderNoCin _ -> NbitsAdderNoCin w
            | NbitsAdderNoCout _ -> NbitsAdderNoCout w
            | NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout w
            | NbitsXor (_, op) -> NbitsXor (w, op)
            | NbitsAnd _ -> NbitsAnd w
            | NbitsNot _ -> NbitsNot w
            | NbitsOr _ -> NbitsOr w
            | NbitSpreader _ -> NbitSpreader w
            | SplitWire _ -> SplitWire w
            | Register _ -> Register w
            | RegisterE _ -> RegisterE w
            | Counter _ -> Counter w
            | CounterNoLoad _ -> CounterNoLoad w
            | CounterNoEnable _ -> CounterNoEnable w
            | CounterNoEnableLoad _ -> CounterNoEnableLoad w
            | Shift (_, _, st) -> Shift (w, shifterWidthFor w, st)
            | BusCompare (_, cv) -> BusCompare (w, cv)
            | Input _ -> Input w
            | Input1 (_, dv) -> Input1 (w, dv)
            | Output _ -> Output w
            | Constant (_, cv) -> Constant (w, cv)
            | _ -> compType)

let defaultValuePrism : Prism<ComponentType, int> =
    Prism.create
        (function Input1 (_, dv) -> Some (int (Option.defaultValue 0I dv)) | _ -> None)
        (fun dv -> function Input1 (w, _) -> Input1 (w, Some (bigint dv)) | t -> t)

let ioPortPrism : Prism<ComponentType, int> =
    Prism.create
        (function | Input1 (w, _)
                  | Output w -> Some w
                  | BusSelection(_w,lsb) -> Some lsb
                  | _ -> None)
        (fun iow -> function 
            | Input1 (_, dv) -> Input1 (iow, dv) 
            | Output _ -> Output iow
            | BusSelection(w, _lsb) -> BusSelection (w, iow)
            | t -> t)

let resolveParametersForComponent 
    (paramBindings: ParamBindings) 
    (paramSlots: Map<ParamSlot, ConstrainedExpr>) 
    (comp: Component) 
    : Result<Component, string> =
    
    let compIdStr = comp.Id
    let relevantSlots = 
        paramSlots 
        |> Map.filter (fun slot _ -> slot.CompId = compIdStr)

    if Map.isEmpty relevantSlots then
        Ok comp
    else
        relevantSlots
        |> Map.toList
        |> List.fold 
            (fun (currentType, errorOpt) (slot, constrainedExpr) ->
                match errorOpt with
                | Some _ -> (currentType, errorOpt)
                | None ->
                    match ParameterTypes.evaluateParamExpression paramBindings constrainedExpr.Expression with
                    | Ok evaluatedValue -> 
                        let newType =
                            match slot.CompSlot with
                            | Buswidth -> currentType |> (evaluatedValue ^= buswidthPrism)
                            | IO _ -> currentType |> (evaluatedValue ^= ioPortPrism)
                            | SplitNWidth idx ->
                                match currentType with
                                | SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length widths ->
                                    let newWidths = widths |> List.mapi (fun i w -> if i = idx then evaluatedValue else w)
                                    SplitN (n, newWidths, lsbs)
                                | _ -> currentType
                            | SplitNLSB idx ->
                                match currentType with
                                | SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length lsbs ->
                                    let newLsbs = lsbs |> List.mapi (fun i l -> if i = idx then evaluatedValue else l)
                                    SplitN (n, widths, newLsbs)
                                | _ -> currentType
                            | InputDefault -> currentType |> (evaluatedValue ^= defaultValuePrism)
                            | CustomCompParam _ -> currentType
                        (newType, None)
                    | Error err -> (currentType, Some err)
            )
            (comp.Type, None)
        |> function
            | (_, Some err) -> Error err
            | (updatedType, None) -> Ok { comp with Type = updatedType }

/// Update LoadedComponent port labels after parameter resolution
let updateLoadedComponentPorts (loadedComponent: LoadedComponent) : LoadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots when not (Map.isEmpty paramSlots.ParamSlots) ->
        // Apply parameter resolution to get updated port labels
        let (comps, conns) = loadedComponent.CanvasState
        let resolvedComps = 
            comps |> List.map (fun comp ->
                match resolveParametersForComponent paramSlots.DefaultBindings paramSlots.ParamSlots comp with
                | Ok resolvedComp -> resolvedComp
                | Error _ -> comp // Keep original on error
            )
        let resolvedCanvas = (resolvedComps, conns)
        let newInputLabels = CanvasExtractor.getOrderedCompLabels (Input1 (0, None)) resolvedCanvas
        let newOutputLabels = CanvasExtractor.getOrderedCompLabels (Output 0) resolvedCanvas
        
        { loadedComponent with 
            InputLabels = newInputLabels
            OutputLabels = newOutputLabels }
    | _ -> loadedComponent

/// Update a custom component with new I/O component widths.
/// Used when these chnage as result of parameter changes.

/// create a popup to edit in the model a custom component parameter binding
/// TODO - maybe comp should be a ComponentId with actual component looked up from model for safety?
let editParameterBindingPopup model parameterName currValue comp (custom: CustomComponentType) dispatch   = 
    match model.CurrentProj with
    | None -> JSHelpers.log "Warning: testEditParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Edit parameter value"
        let compSlotName = CustomCompParam parameterName
        
        // Initialize the popup dialog state to clear any previous parameter specs
        dispatch <| ClearPopupDialogParamSpec compSlotName
        
        let body = fun (model: Model) ->
            div [] [
                str $"New value for the parameter {parameterName}:"
                br []
                str $"(current value: {currValue})"
                br []
                // Use the existing paramInputField with no constraints for custom component parameters
                paramInputField model $"Parameter {parameterName}" currValue (Some currValue) [] (Some comp) compSlotName dispatch
            ]
        
        let buttonText = "Set value"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) -> 
                // Get the parameter spec from dialog state
                let paramSpecs = model'.PopupDialogData.DialogState |> Option.defaultValue Map.empty
                match Map.tryFind compSlotName paramSpecs with
                | Some (Ok paramSpec) ->
                    // Parse and evaluate the parameter expression from the spec
                    let paramBindings = model' |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
                    match ParameterTypes.evaluateParamExpression paramBindings paramSpec.Expression with
                    | Ok newValue ->
                        let newBindings =
                            match custom.ParameterBindings with
                            | Some bindings -> bindings
                            | None -> Map.empty
                            |> Map.add (ParamName parameterName) (PInt newValue)
                        
                        // Get the custom component's loaded component to find parameter slot definitions
                        let currentSheet = 
                            project.LoadedComponents
                            |> List.tryFind (fun lc -> lc.Name = custom.Name)
                        
                        // Calculate updated label widths based on parameter evaluations
                        let labelToEval = 
                            match currentSheet with
                            | Some sheet ->
                                match sheet.LCParameterSlots with
                                | Some sheetInfo ->
                                    sheetInfo.ParamSlots
                                    |> Map.toSeq
                                    |> Seq.choose (fun (paramSlot, constrainedExpr) -> 
                                        match paramSlot.CompSlot with
                                        | IO label -> 
                                            let evaluatedValue = 
                                                match ParameterTypes.evaluateParamExpression newBindings constrainedExpr.Expression with
                                                | Ok expr -> expr
                                                | Error _ -> 0
                                            Some (label, evaluatedValue)
                                        | _ -> None 
                                    )
                                    |> Map.ofSeq
                                | None -> Map.empty
                            | None -> Map.empty
                        
                        // Update the custom component with new parameter bindings and updated port widths
                        let updatedCustom = updateCustomComponent labelToEval newBindings comp
                        dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (ComponentId comp.Id, comp, updatedCustom.Type))))
                        
                        let dispatchnew (msg: DrawModelType.SheetT.Msg) : unit = dispatch (Sheet msg)
                        model.Sheet.DoBusWidthInference dispatchnew
                        dispatch <| ClosePopup
                    | Error _ -> 
                        // Should not happen as paramInputField already validated the expression
                        ()
                | _ -> 
                    // No valid parameter spec found, don't close popup
                    ()

        // Button is disabled if there's no valid parameter spec
        let isDisabled =
            fun (model': Model) ->
                let paramSpecs = model'.PopupDialogData.DialogState |> Option.defaultValue Map.empty
                match Map.tryFind compSlotName paramSpecs with
                | Some (Ok _) -> false
                | _ -> true

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

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
    
    // Get the parameter slots from the current sheet to find expressions
    let slots = model |> getCurrentSheet |> getParamSlots

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
                        
                        // Look for the expression in the parameter slots
                        let paramValStr =
                            let slotKey = {CompId = comp.Id; CompSlot = CustomCompParam paramName}
                            match Map.tryFind slotKey slots, Map.containsKey key ccParams with
                            | Some constrainedExpr, _ ->
                                // If there's an expression, render it as a string
                                ParameterTypes.renderParamExpression constrainedExpr.Expression 0
                            | None, true ->
                                // Otherwise show the evaluated value
                                match value with
                                | ParameterTypes.PInt i -> string i
                                | x -> string x
                            | None, false ->
                                // nothing binds this parameter on this instance: the sheet
                                // inside elaborates with its declared default
                                match value with
                                | ParameterTypes.PInt i -> $"{i} (default; unbound)"
                                | x -> $"{x} (default; unbound)"
                        
                        let paramValInt = 
                            match value with
                            | ParameterTypes.PInt i -> i
                            | _ -> 0
                        
                        tr [] [
                            td [] [str paramName]
                            td [] [str paramValStr]
                            td [] [
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model paramName paramValInt comp custom dispatch)
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
            div [] [str ("Max: " + ParameterTypes.renderParamExpression expr 0)]
        | MinVal (expr, err) ->
            div [] [str ("Min: " + ParameterTypes.renderParamExpression expr 0)]
    
    let constraintMessage (constraint': ParamConstraint) =
        match constraint' with
            | MaxVal (_, err)  | MinVal (_, err) -> err


    /// UI component to display a single parameterised Component slot definition.
    /// This is read-only.
    let renderSlotSpec (slot: ParamSlot) (expr: ConstrainedExpr) =
        let slotNameStr =
            match slot.CompSlot with
            | Buswidth -> "Buswidth"
            | IO label -> $"Input/output {label}"
            | SplitNWidth idx -> $"SplitN output {idx} width"
            | SplitNLSB idx -> $"SplitN output {idx} LSB"
            | CustomCompParam paramName -> $"Custom parameter {paramName}"
            | InputDefault -> "Default value"
        
        let name = if Map.containsKey (ComponentId slot.CompId) model.Sheet.Wire.Symbol.Symbols then
                        string model.Sheet.Wire.Symbol.Symbols[ComponentId slot.CompId].Component.Label
                    else
                        // slots are pruned when the sheet is saved or left, so this should not persist
                        "[Nonexistent]"
        tr [] [
            td [] [
                b [] [str name] 
                br [] 
                str slotNameStr
            ]
            td [] [str (ParameterTypes.renderParamExpression expr.Expression 0)]
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

//------------------------------------------------------------------------------------------------//
//------------------------------------- Bind-to-top offers ---------------------------------------//
//------------------------------------------------------------------------------------------------//

let private emptyParamDefs : ParameterDefs = {DefaultBindings = Map.empty; ParamSlots = Map.empty}

/// Apply one accepted chain action to the LoadedComponent of the sheet it names.
/// Both stores of an instance binding are kept in step: the CustomCompParam slot on the parent
/// sheet, and the ParameterBindings of the instance component in the parent's canvas.
let private applyChainActionToLdc (action: ParameterAnalysis.ChainAction) (ldc: LoadedComponent) : LoadedComponent =
    match action with
    | ParameterAnalysis.AddSheetParam (sheet, name, defVal) when sheet = ldc.Name ->
        let defs = Option.defaultValue emptyParamDefs ldc.LCParameterSlots
        match Map.containsKey name defs.DefaultBindings with
        | true -> ldc
        | false ->
            {ldc with LCParameterSlots = Some {defs with DefaultBindings = Map.add name (PInt defVal) defs.DefaultBindings}}
    | ParameterAnalysis.BindInstance (sheet, instId, _, _, name) when sheet = ldc.Name ->
        let (ParamName nameStr) = name
        let defs = Option.defaultValue emptyParamDefs ldc.LCParameterSlots
        let slot = {CompId = instId; CompSlot = CustomCompParam nameStr}
        let defs' = {defs with ParamSlots = Map.add slot {Expression = PParameter name; Constraints = []} defs.ParamSlots}
        let comps, conns = ldc.CanvasState
        let comps' =
            comps
            |> List.map (fun c ->
                match c.Id = instId, c.Type with
                | true, Custom cc ->
                    let bindings = cc.ParameterBindings |> Option.defaultValue Map.empty
                    {c with Type = Custom {cc with ParameterBindings = Some (Map.add name (PParameter name) bindings)}}
                | _ -> c)
        {ldc with LCParameterSlots = Some defs'; CanvasState = comps', conns}
    | _ -> ldc

/// Apply accepted bind-to-top offers: create the parameters and bindings of their chains,
/// update the open sheet's symbols to match, and persist. A sheet whose file was in step with
/// memory is written through immediately; the open sheet (whose canvas belongs to the draw
/// block) is marked as needing saving instead, so accepting an offer never silently commits
/// unrelated circuit edits.
let applyBindOffers (offers: ParameterAnalysis.BindOffer list) (model: Model) (dispatch: Msg -> unit) : unit =
    match model.CurrentProj with
    | None -> ()
    | Some project ->
        let actions = offers |> List.collect (fun offer -> offer.Actions) |> List.distinct
        let modifiedSheets = offers |> List.collect ParameterAnalysis.sheetsModifiedByOffer |> Set.ofList
        let openName = project.OpenFileName
        let updateLdc (ldc: LoadedComponent) =
            match Set.contains ldc.Name modifiedSheets with
            | false -> ldc
            | true ->
                let ldc' = (ldc, actions) ||> List.fold (fun l action -> applyChainActionToLdc action l)
                match ldc.Name = openName || ldc.LoadedComponentIsOutOfDate with
                | false ->
                    MenuHelpers.writeComponentToFile ldc' |> ignore
                    ldc'
                | true -> {ldc' with LoadedComponentIsOutOfDate = true}
        let ldcs' = project.LoadedComponents |> List.map updateLdc
        dispatch <| UpdateModel (fun m ->
            {m with CurrentProj = Some {project with LoadedComponents = ldcs'}}
            |> (fun m' ->
                match Set.contains openName modifiedSheets with
                | true -> Optic.set savedSheetIsOutOfDate_ true m'
                | false -> m'))

        // The open sheet's canvas lives in the draw block: bindings created on its instances
        // must go through symbol messages as well, with the instance's port widths updated to
        // the value the binding takes at this sheet's displayed parameter values.
        // All parameters bound on one instance are combined into a single ChangeCustom - each
        // message replaces the whole custom type, so per-parameter messages built from the same
        // symbol snapshot would overwrite each other.
        let openDefaults =
            ldcs'
            |> List.tryFind (fun ldc -> ldc.Name = openName)
            |> Option.bind (fun ldc -> ldc.LCParameterSlots)
            |> Option.map (fun defs -> defs.DefaultBindings)
            |> Option.defaultValue Map.empty
        actions
        |> List.choose (fun action ->
            match action with
            | ParameterAnalysis.BindInstance (sheet, instId, _, childSheet, name) when sheet = openName ->
                Some ((instId, childSheet), name)
            | _ -> None)
        |> List.groupBy fst
        |> List.iter (fun ((instId, childSheet), namedBinds) ->
            let names = namedBinds |> List.map snd |> List.distinct
            match Map.tryFind (ComponentId instId) model.Sheet.Wire.Symbol.Symbols with
            | None -> ()
            | Some symbol ->
                let comp = symbol.Component
                match comp.Type with
                | Custom cc ->
                    let newBindings =
                        (cc.ParameterBindings |> Option.defaultValue Map.empty, names)
                        ||> List.fold (fun bindings name -> Map.add name (PParameter name) bindings)
                    let labelToEval =
                        match project.LoadedComponents |> List.tryFind (fun ldc -> ldc.Name = childSheet) with
                        | None -> Map.empty
                        | Some childLdc ->
                            let childDefaults = ParameterAnalysis.declaredParams childLdc
                            let evalInParent expr =
                                match ParameterTypes.evaluateParamExpression openDefaults expr with
                                | Ok v -> Some (PInt v)
                                | Error _ -> None
                            let childEffective =
                                childDefaults
                                |> Map.map (fun n defExpr ->
                                    Map.tryFind n newBindings
                                    |> Option.bind evalInParent
                                    |> Option.defaultValue defExpr)
                            ParameterAnalysis.sheetParamSlots childLdc
                            |> Map.toSeq
                            |> Seq.choose (fun (slot, cexpr) ->
                                match slot.CompSlot with
                                | IO label ->
                                    match ParameterTypes.evaluateParamExpression childEffective cexpr.Expression with
                                    | Ok v -> Some (label, v)
                                    | Error _ -> None
                                | _ -> None)
                            |> Map.ofSeq
                    let updated = updateCustomComponent labelToEval newBindings comp
                    dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (ComponentId comp.Id, comp, updated.Type))))
                | _ -> ())
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        model.Sheet.DoBusWidthInference sheetDispatch

/// The bind-to-top offers an event should surface. Offers are suppressed while a simulation is
/// open, because accepting one changes the parameters of the design being simulated.
let private offersForScope (scope: ParameterAnalysis.BindOfferScope) (model: Model) : ParameterAnalysis.BindOffer list =
    match model.CurrentProj with
    | None -> []
    | Some _ when ModelHelpers.simulationIsOpen model -> []
    | Some proj ->
        let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
        match ParameterAnalysis.effectiveTopSheet ldcs with
        | None -> []
        | Some top ->
            ParameterAnalysis.findBindOffers ldcs top None
            |> ParameterAnalysis.offersInScope ldcs scope

/// A popup offering to bind qualifying unbound instance parameters to the same-named parameter
/// of an ancestor sheet, materialising the chain of parameters and bindings if accepted.
/// Returns None when the event's scope surfaces nothing, which is almost always.
let bindToTopOfferCheck (scope: ParameterAnalysis.BindOfferScope) (model: Model) : ((Msg -> unit) -> Model -> ReactElement) option =
    match offersForScope scope model with
    | [] -> None
    | offers ->
        let sheets = offers |> List.collect ParameterAnalysis.sheetsModifiedByOffer |> List.distinct
        let describeOffer (offer: ParameterAnalysis.BindOffer) =
            let (ParamName name) = offer.Param
            li [] [
                str $"{offer.InstanceLabel} on sheet {offer.OnSheet}: bind parameter "
                b [] [str name]
                str $" to {offer.BindsTo}:{name}"
            ]
        let body =
            div [] [
                str "A parameter of the same name is defined on a sheet above these component \
                     instances, but nothing binds them together. Binding keeps them in step: \
                     changing the value near the top of the design changes them too. Instances \
                     that already have their own explicit bindings are left unchanged."
                br []; br []
                ul [Style [MarginLeft "20px"; ListStyleType "disc"]] (List.map describeOffer offers)
                br []
                str ("Parameters and bindings will be created in sheets: " + String.concat ", " sheets + ". \
                     The modified sheets are saved. Declining leaves each parameter at its default \
                     value, noted in the instance's properties.")
            ]
        let foot (dispatch: Msg -> unit) (_: Model) =
            Level.level [Level.Level.Props [Style [Width "100%"]]] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> dispatch ClosePopup)
                        ] [str "Keep defaults"]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ ->
                                dispatch <| ExecFuncInMessage(applyBindOffers offers, dispatch)
                                dispatch ClosePopup)
                        ] [str "Create bindings"]
                    ]
                ]
            ]
        Some (buildPopup "Bind parameters to the top sheet?" (fun _ _ -> body) foot (fun dispatch _ -> dispatch ClosePopup) [])

//------------------------------------------------------------------------------------------------//
//----------------------------------- Top sheet choice on open -----------------------------------//
//------------------------------------------------------------------------------------------------//

/// Project paths whose top-sheet choice popup the user has cancelled this session. Cancelling
/// opens the sheet displaying defaults; the question is not asked again until the project is
/// next opened, so the popup can never nag.
let mutable private topChoiceDeclinedFor: Set<string> = Set.empty

/// A popup asking the user to choose the top sheet, or None. It fires only when several
/// top-level sheets exist, none has been chosen, and they disagree about the parameter values
/// the opened sheet displays with - roughly once per project. It never blocks opening.
let topSheetChoiceCheck (model: Model) : ((Msg -> unit) -> Model -> ReactElement) option =
    match model.CurrentProj with
    | None -> None
    | Some proj when Set.contains proj.ProjectPath topChoiceDeclinedFor -> None
    | Some proj when proj.LoadedComponents |> List.exists (fun ldc -> ldc.IsTopSheet) -> None
    | Some proj ->
        let ldcs = proj.LoadedComponents
        let sheetName = proj.OpenFileName
        let rootsContaining =
            ParameterAnalysis.instanceForestRoots ldcs
            |> List.filter (fun root -> Set.contains sheetName (ParameterAnalysis.sheetsUnderTop ldcs root))
        let shownValues root =
            ParameterAnalysis.displayValues ldcs root sheetName
            |> Map.map (fun _ display -> ParameterAnalysis.shownValue display)
        match rootsContaining with
        | [] | [_] -> None
        | _ when (rootsContaining |> List.map shownValues |> List.distinct |> List.length) <= 1 -> None
        | _ ->
            let decline (dispatch: Msg -> unit) =
                topChoiceDeclinedFor <- Set.add proj.ProjectPath topChoiceDeclinedFor
                dispatch ClosePopup
            let body =
                div [] [
                    str $"Sheet {sheetName} is used in more than one top-level design, and its \
                          parameter values differ between them. Choose which design the editor \
                          should display it as part of."
                    br []; br []
                    str "You can change this at any time by right-clicking a sheet in the Sheets \
                         menu and choosing 'Set as top'. Cancelling shows the sheet with its \
                         default parameter values."
                ]
            let foot (dispatch: Msg -> unit) (_: Model) =
                let chooseButton root =
                    Level.item [] [
                        Button.button [
                            Button.Color IsPrimary
                            Button.OnClick (fun _ ->
                                dispatch <| UpdateModel (MenuHelpers.setTopSheetState root)
                                dispatch ClosePopup
                                // the top has (for display purposes) just changed: re-run the
                                // bind-to-top check under it
                                dispatch <| CheckBindToTopOffers ParameterAnalysis.WholeDesign)
                        ] [str root]
                    ]
                Level.level [Level.Level.Props [Style [Width "100%"]]] [
                    Level.left [] []
                    Level.right [] (
                        [ Level.item [] [
                            Button.button [
                                Button.Color IsLight
                                Button.OnClick (fun _ -> decline dispatch)
                            ] [str "Not now"] ] ]
                        @ List.map chooseButton rootsContaining)
                ]
            Some (buildPopup "Choose the top sheet" (fun _ _ -> body) foot (fun dispatch _ -> decline dispatch) [])
