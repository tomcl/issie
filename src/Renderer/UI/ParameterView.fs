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
/// The open sheet's parameter DECLARATIONS (defaults with descriptions).
/// Use paramBindingsOfModel below where an evaluation environment is wanted.
let defaultBindingsOfModel_ = lcParameterInfoOfModel_ >?> defaultBindings_

/// The open sheet's default bindings as an evaluation environment.
let paramBindingsOfModel (model: Model) : ParamBindings =
    model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty |> bindingsOf

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


/// Get the parameter declarations (defaults and descriptions) for a LoadedComponent
let getDefaultParamDefs loadedComponent : ParamDefinitions =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots -> paramSlots.DefaultBindings
    | None -> Map.empty

/// Get default parameter bindings for LoadedComponent, for use as an evaluation environment
let getDefaultParams loadedComponent : ParamBindings =
    getDefaultParamDefs loadedComponent |> bindingsOf


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

/// The port widths an instance of `childSheet` has when it binds its parameters as given.
///
/// An instance binding is an expression in the parameters of the sheet the instance SITS ON, so it
/// is evaluated there first; the child sheet's own declared defaults then fill in every parameter
/// the instance does not bind. That merged environment - the same one GraphMerger.effectiveBindings
/// builds for elaboration - is what the child's IO slot expressions have to be evaluated in.
///
/// Evaluating them against the raw instance bindings instead leaves every unbound parameter
/// undefined, and makes a parameter bound to a same-named parent parameter look self-referential,
/// so the width silently comes out as zero.
///
/// A label whose width cannot be worked out is left out of the result rather than guessed at, so
/// its port keeps the width it already has.
let portWidthsOfInstance
        (ldcs: LoadedComponent list)
        (parentBindings: ParamBindings)
        (childSheet: string)
        (instanceBindings: ParamBindings)
        : Map<string, int> =
    match ldcs |> List.tryFind (fun lc -> lc.Name = childSheet) with
    | None -> Map.empty
    | Some childLdc ->
        let effective =
            ParameterAnalysis.declaredParams childLdc
            |> Map.map (fun name defExpr ->
                Map.tryFind name instanceBindings
                |> Option.bind (fun expr ->
                    ParameterTypes.evaluateParamExpression parentBindings expr
                    |> Result.toOption
                    |> Option.map PInt)
                |> Option.defaultValue defExpr)
        ParameterAnalysis.sheetParamSlots childLdc
        |> Map.toSeq
        |> Seq.choose (fun (slot, exprSpec) ->
            match slot.CompSlot with
            | IO label ->
                ParameterTypes.evaluateParamExpression effective exprSpec.Expression
                |> Result.toOption
                |> Option.map (fun width -> label, width)
            | _ -> None)
        |> Map.ofSeq

/// Push the values of the parameterised slots of ONE component onto the canvas.
/// All of a component's slots are applied together because two of the messages replace a whole
/// field of the component type - a SplitN's width and LSB lists, a custom component's parameter
/// bindings - and are built here from `model`, which is a snapshot: issued one slot at a time
/// they would overwrite each other, leaving all but the last slot at its old value.
/// (ChangeWidth and ChangeInputValue read the live symbol and so do not have this problem.)
let updateComponentSlots dispatch (model: Model) (compIdStr: string) (slotValues: (CompSlotName * int) list) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let comp = model.Sheet.GetComponentById <| ComponentId compIdStr
    let compId = ComponentId comp.Id
    let valueOf slot = slotValues |> List.tryPick (fun (s, v) -> if s = slot then Some v else None)

    match comp.Type with
    | SplitN (n, widths, lsbs) ->
        let newWidths = widths |> List.mapi (fun i w -> valueOf (SplitNWidth i) |> Option.defaultValue w)
        let newLsbs = lsbs |> List.mapi (fun i l -> valueOf (SplitNLSB i) |> Option.defaultValue l)
        model.Sheet.ChangeSplitN sheetDispatch compId n newWidths newLsbs

    | Custom customComp ->
        let newBindings =
            (customComp.ParameterBindings |> Option.defaultValue Map.empty, slotValues)
            ||> List.fold (fun bindings (slot, value) ->
                match slot with
                | CustomCompParam paramName -> Map.add (ParamName paramName) (PInt value) bindings
                | _ -> bindings)

        match model.CurrentProj with
        | Some project ->
            // the instance's port widths at the new bindings, from the child sheet's IO slots
            let labelToEval =
                portWidthsOfInstance
                    project.LoadedComponents (paramBindingsOfModel model) customComp.Name newBindings

            let updatedCustom = updateCustomComponent labelToEval newBindings comp
            dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (compId, comp, updatedCustom.Type))))
        | None ->
            // Fallback to just updating bindings if no project context
            let newCustomComp = { customComp with ParameterBindings = Some newBindings }
            dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (compId, comp, Custom newCustomComp))))

    | _ ->
        slotValues
        |> List.iter (fun (slot, value) ->
            match comp.Type, slot with
            // an IO slot is a width only on an Input/Output. On these two the properties pane puts
            // the BusSelection LSB and the BusCompare comparison value in it - see
            // SelectedComponentView.makeLsbBitNumberField - and ChangeLSB is what sets both.
            | (BusSelection _ | BusCompare _), IO _ ->
                model.Sheet.ChangeLSB sheetDispatch compId (bigint value)
            | _, Buswidth | _, IO _ -> model.Sheet.ChangeWidth sheetDispatch compId value
            | Input1 _, InputDefault -> model.Sheet.ChangeInputValue sheetDispatch compId (bigint value)
            | _, InputDefault -> failwithf $"Default value cannot be set on {comp.Type}"
            | _, (SplitNWidth _ | SplitNLSB _) -> failwithf $"SplitN slots cannot be applied to {comp.Type}"
            | _, CustomCompParam _ -> failwithf $"CustomCompParam can only be used with Custom components")

    // Update most recent bus width
    slotValues
    |> List.iter (fun (slot, value) ->
        match slot, comp.Type with
        | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
        | Buswidth, _ | IO _, _ -> dispatch <| ReloadSelectedComponent value
        | _ -> ())

/// Use sheet component update functions to perform an update to a single slot.
let updateComponent dispatch model (slot: ParamSlot) (value:int) =
    updateComponentSlots dispatch model slot.CompId [slot.CompSlot, value]


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


/// Add the parameter information from several slots of one newly created component.
/// Used when a custom component instance is placed: it binds every parameter its sheet declares
/// at once, so one hook has to create all of their slots.
let addParamComponents
    (newCompSpecs: NewParamCompSpec list)
    (dispatch: Msg -> Unit)
    (compId: CommonTypes.ComponentId)
    : Unit =
    newCompSpecs |> List.iter (fun spec -> addParamComponent spec dispatch compId)


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
        let paramBindings = paramBindingsOfModel model

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
    |> Map.toList
    |> List.choose (fun (slot, exprSpec) ->
        liveSlotValue slot exprSpec |> Option.map (fun value -> slot.CompId, (slot.CompSlot, value)))
    // a component's slots must go together: see updateComponentSlots
    |> List.groupBy fst
    |> List.iter (fun (compIdStr, entries) ->
        updateComponentSlots dispatch model compIdStr (List.map snd entries))


//------------------------------------------------------------------------------------------------//
//------------------------------ Drawing at computed parameter values ----------------------------//
//------------------------------------------------------------------------------------------------//

/// The values the open sheet's parameters take under the current top sheet, for display.
/// A parameter is included only where every instance of the sheet under the top agrees on a known
/// value (ParamDisplayValue.ExactValue); anything else - instances disagreeing, a value that could
/// not be evaluated, or a sheet not instantiated under the top - keeps the declared default,
/// because it is not a fact about the sheet and must not be drawn as one.
let computedBindingsForOpenSheet (model: Model) : ParamBindings =
    let declared = paramBindingsOfModel model
    match model.CurrentProj with
    | None -> declared
    | Some proj ->
        let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
        match ParameterAnalysis.effectiveTopSheet ldcs with
        | None -> declared
        | Some top ->
            (declared, ParameterAnalysis.displayValues ldcs top proj.OpenFileName)
            ||> Map.fold (fun bindings name display ->
                match display with
                | ParameterAnalysis.ExactValue v -> Map.add name (PInt v) bindings
                | ParameterAnalysis.DefaultValue _
                | ParameterAnalysis.MultipleValues _ -> bindings)

/// The declared value of every parameterised slot whose displayed value differs from it, grouped
/// by the component the slot belongs to. This is what has to be put back before the sheet is
/// saved; a slot displaying its declared value needs nothing remembering.
let private declaredSlotValues (model: Model) : Map<ComponentId, Map<CompSlotName, int>> =
    let declared = paramBindingsOfModel model
    let computed = computedBindingsForOpenSheet model
    model
    |> get paramSlotsOfModel_
    |> Option.defaultValue Map.empty
    |> Map.toList
    |> List.choose (fun (slot, exprSpec) ->
        match
            ParameterTypes.evaluateParamExpression declared exprSpec.Expression,
            ParameterTypes.evaluateParamExpression computed exprSpec.Expression
            with
        | Ok declaredValue, Ok computedValue when declaredValue <> computedValue ->
            Some (ComponentId slot.CompId, (slot.CompSlot, declaredValue))
        | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (compId, entries) -> compId, entries |> List.map snd |> Map.ofList)
    |> Map.ofList

/// Record on each symbol the declared value of the slots it is about to display differently.
/// Every symbol is written, so a symbol that no longer differs has its record cleared: this must
/// be safe to repeat, and on a later call the values may have been computed for a different top
/// sheet. Only the slot values are recorded, so no other edit to the symbol is disturbed.
let private stashDeclaredSlots (model: Model) : Model =
    let byComp = declaredSlotValues model
    let stash cid (sym: SymbolT.Symbol) =
        {sym with DeclaredSlots = Map.tryFind cid byComp |> Option.defaultValue Map.empty}
    model |> Optic.map modelToSymbols (Map.map stash)

/// Draw the open sheet at the values its parameters take under the current top sheet.
/// What is saved is unaffected: the declared value of every slot displaying something different
/// is kept in the symbol's DeclaredSlots, which SymbolUpdate.extractComponent puts back.
/// Values are pushed through the same symbol-change path the properties pane uses, so symbol
/// size, ports and geometry are recomputed rather than patched.
let applyComputedDisplayValues (model: Model) (dispatch: Msg -> unit) : unit =
    let computed = computedBindingsForOpenSheet model
    // the stash is dispatched first so that it is applied before the value changes that follow
    dispatch <| UpdateModel stashDeclaredSlots
    updateComponents computed model dispatch


/// Give each pasted component the parameter slot expressions of the component it was copied from,
/// so that a pasted copy stays parameterised rather than freezing at whatever value it was
/// showing. Slots are keyed by component id, and a paste mints new ids, so without this the
/// parameterisation is silently lost.
/// A slot is only copied when every parameter it refers to is declared on this sheet: pasting
/// into a sheet that does not declare them would otherwise leave a slot referring to nothing,
/// which breaks the invariant that every parameter used on a sheet is defined on it.
let copyParamSlotsToPastedComponents (pairs: (string * string) list) (model: Model) : Model =
    let slots = model |> get paramSlotsOfModel_ |> Option.defaultValue Map.empty
    let declared = model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
    let isDeclaredHere name = Map.containsKey name declared
    let pastedIds = pairs |> List.map snd |> Set.ofList

    // 1. the slot expressions, keyed by component id, copied onto the new ids
    let copied =
        pairs
        |> List.collect (fun (sourceId, pastedId) ->
            slots
            |> Map.toList
            |> List.filter (fun (slot, exprSpec) ->
                slot.CompId = sourceId
                && ParameterTypes.paramNamesOfSlot exprSpec |> List.forall isDeclaredHere)
            |> List.map (fun (slot, exprSpec) -> {slot with CompId = pastedId}, exprSpec))
    let withSlots =
        match copied with
        | [] -> model
        | _ ->
            let slots' = (slots, copied) ||> List.fold (fun acc (slot, exprSpec) -> Map.add slot exprSpec acc)
            set paramSlotsOfModel_ slots' model

    // 2. a custom component instance stores its bindings twice: in the CustomCompParam slot above
    // and on the component itself. The component was copied verbatim, so drop from it any binding
    // whose expression names a parameter this sheet does not declare - the instance then
    // elaborates that parameter at the child sheet's default, which is the whole point of the
    // default. Without this a paste into another sheet leaves a binding referring to nothing.
    /// what a pasted instance loses, so that it can be reported rather than just happening
    let droppedFrom (sym: SymbolT.Symbol) (bindings: ParamBindings) (kept: ParamBindings) =
        bindings
        |> Map.toList
        |> List.map fst
        |> List.filter (fun name -> not (Map.containsKey name kept))
        |> List.map (fun (ParamName name) -> sym.Component.Label, name)

    let pruneBindings _ (sym: SymbolT.Symbol) =
        match Set.contains sym.Component.Id pastedIds, sym.Component.Type with
        | true, Custom cc ->
            let bindings = cc.ParameterBindings |> Option.defaultValue Map.empty
            let kept =
                bindings
                |> Map.filter (fun _ expr -> ParameterTypes.paramNamesOfExpr expr |> List.forall isDeclaredHere)
            match Map.count kept = Map.count bindings with
            | true -> sym, []
            | false ->
                let cc' = {cc with ParameterBindings = if Map.isEmpty kept then None else Some kept}
                {sym with Component = {sym.Component with Type = Custom cc'}}, droppedFrom sym bindings kept
        | _ -> sym, []

    let pruned = withSlots |> get modelToSymbols |> Map.map pruneBindings
    let dropped = pruned |> Map.toList |> List.collect (snd >> snd)
    let model' = withSlots |> set modelToSymbols (pruned |> Map.map (fun _ (sym, _) -> sym))

    // Losing a binding changes what the instance means - it falls back to the default of the sheet
    // inside it - so say so. Silently is how a pasted copy ends up quietly describing different
    // hardware from the one it was copied from.
    match dropped with
    | [] -> model'
    | _ ->
        let described =
            dropped
            |> List.groupBy fst
            |> List.map (fun (label, entries) ->
                $"""{label} ({entries |> List.map snd |> String.concat ", "})""")
            |> String.concat "; "
        let sheetName = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue "this sheet"
        let message =
            $"Pasted here, these components lost parameter bindings that {sheetName} does not \
              declare, and now use the default values of the sheets inside them: {described}."
        model' |> set (notifications_ >-> fromDiagram_) (Some (Notifications.warningNotification message CloseDiagramNotification))


/// Updates the LCParameterSlots DefaultParams section.
type UpdateInfoSheetChoise =
    | DefaultParams of Name: string * Value: int * Description: string * Delete: bool
    | ParamSlots of ParamSlot * ParameterTypes.ParamExpression * ParamConstraint list


let updateInfoSheetDefaultParams
        (currentSheetInfo: option<ParameterTypes.ParameterDefs>)
        (paramName: string)
        (value: int)
        (description: string)
        (delete: bool) =
    let name = ParamName paramName
    if delete then
        match currentSheetInfo with
        | Some infoSheet ->
            let newDefaultParams = infoSheet.DefaultBindings |> Map.remove name
            Some {infoSheet with DefaultBindings = newDefaultParams}
        | None -> None
    else
    let definition = {Expression = PInt value; Description = description}
    match currentSheetInfo with
    | Some infoSheet ->
        let newDefaultParams = infoSheet.DefaultBindings |> Map.add name definition
        Some {infoSheet with DefaultBindings = newDefaultParams}
    | None ->
        Some {DefaultBindings = Map.ofList [name, definition]; ParamSlots = Map.empty}


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
                                                            | DefaultParams (paramName, value, description, delete) -> updateInfoSheetDefaultParams currentSheet.LCParameterSlots paramName value description delete
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
        let title = "Add parameter"

        let textPrompt =
            fun _ ->
                div []
                    [
                        str "Specify the parameter name:"
                        br []
                    ]

        let descriptionPrompt =
            fun _ ->
                div []
                    [
                        str "What does this parameter mean?"
                        br []
                    ]

        let intPrompt =
            fun _ ->
                div []
                    [
                        str "Default value for the parameter:"
                        br []
                    ]

        let defaultVal = 1
        let body =
            dialogPopupBodyTextDescriptionAndInt
                textPrompt "example: x"
                descriptionPrompt "example: width of the data bus in bits"
                intPrompt defaultVal dispatch
        let buttonText = "Add parameter"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) ->
                let newParamName = getText model'.PopupDialogData
                let newValue = getInt model'.PopupDialogData
                let newDescription = getText2 model'.PopupDialogData

                modifyInfoSheet (project) (DefaultParams (newParamName, newValue, newDescription, false)) dispatch
                // Close popup window
                ClosePopup |> dispatch
                // a new parameter may be the missing ancestor that lets unbound same-named
                // parameters in the sheets below be bound to it; that now shows up as a bind
                // button in those instances' properties rather than as a popup raised here

        // Parameter names can only be made out of letters and numbers, and every parameter must
        // be described: the description is what instances of this sheet show when asking for a value
        let isDisabled =
            fun (model': Model) ->
                 let newParamName = getText model'.PopupDialogData
                 not (Regex.IsMatch(newParamName, "^[a-zA-Z0-9]+$"))
                 || getText2 model'.PopupDialogData = ""

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
        let title = "Edit parameter"
        match getDefaultParamDefs currentSheet |> Map.tryFind (ParamName parameterName) with
        | None ->
            // the row was rendered from an older model in which the parameter still existed
            JSHelpers.log $"Cannot edit parameter {parameterName}: it is not defined on this sheet"
        | Some {Expression = (PParameter _ | PAdd _ | PSubtract _ | PMultiply _ | PDivide _ | PRemainder _)} ->
            dispatch <| SetPropertiesNotification (Notifications.errorPropsNotification
                $"Parameter {parameterName} is bound to an expression. Only integer parameter values can be edited here.")
        | Some currentDef ->
        let currentValue = currentDef.Expression
        let descriptionPrompt =
            fun _ ->
                div []
                    [
                        str $"What does {parameterName} mean?"
                        br []
                    ]

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

        // the existing description is seeded into the dialog so that editing the value alone
        // does not silently blank it
        dispatch <| SetPopupDialogText2 (Some currentDef.Description)
        let body = dialogPopupBodyDescriptionAndInt descriptionPrompt currentDef.Description intPrompt defaultVal dispatch
        let buttonText = "Set value"

        /// the declarations of this sheet with the edited parameter replaced, as an evaluation
        /// environment
        let editedBindings (model': Model) =
            model'
            |> getLCParamInfo
            |> (fun info -> info.DefaultBindings)
            |> Map.add (ParamName parameterName) {Expression = PInt (getInt model'.PopupDialogData); Description = getText2 model'.PopupDialogData}
            |> bindingsOf

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) ->
                let newValue = getInt model'.PopupDialogData
                let newDescription = getText2 model'.PopupDialogData
                modifyInfoSheet project (DefaultParams (parameterName, newValue, newDescription, false)) dispatch

                // Value must meet constraints if able to click button
                updateComponents (editedBindings model') model dispatch
                dispatch <| ClosePopup

        // Disabled if the description has been emptied or any constraints are violated
        let isDisabled =
            fun (model': Model) ->
                let exprSpecs =
                    model'
                    |> get paramSlotsOfModel_
                    |> Option.defaultValue Map.empty
                    |> Map.toList
                    |> List.map snd

                getText2 model'.PopupDialogData = ""
                || (evaluateConstraints (editedBindings model') exprSpecs dispatch |> Result.isError)

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
            modifyInfoSheet project (DefaultParams (parameterName, 0, "", true)) dispatch
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


/// A note for the properties pane of one component: for each of its parameterised slots whose
/// displayed value differs from what this sheet declares, the value shown and the declared one.
/// Nothing at all unless the sheet is being drawn at values computed for the current top sheet.
let computedValueNote (model: Model) (comp: Component) : ReactElement =
    let isDisplayingComputedValues =
        Map.tryFind (ComponentId comp.Id) model.Sheet.Wire.Symbol.Symbols
        |> Option.map (fun sym -> not (Map.isEmpty sym.DeclaredSlots))
        |> Option.defaultValue false
    match isDisplayingComputedValues with
    | false -> null
    | true ->
        let declared = paramBindingsOfModel model
        let computed = computedBindingsForOpenSheet model
        let differing =
            model
            |> get paramSlotsOfModel_
            |> Option.defaultValue Map.empty
            |> Map.toList
            |> List.filter (fun (slot, _) -> slot.CompId = comp.Id)
            |> List.choose (fun (slot, exprSpec) ->
                match
                    ParameterTypes.evaluateParamExpression declared exprSpec.Expression,
                    ParameterTypes.evaluateParamExpression computed exprSpec.Expression
                    with
                | Ok declaredValue, Ok shownValue when declaredValue <> shownValue ->
                    Some (describeSlot model slot, shownValue, declaredValue)
                | _ -> None)
        match differing with
        | [] -> null
        | _ ->
            let describe (name, shownValue, declaredValue) =
                li [] [str $"{name}: {shownValue} (declared {declaredValue})"]
            div [Style [FontSize "11px"; Color "grey"]] [
                str "Shown at the values this sheet's parameters take in the current design:"
                ul [Style [MarginLeft "20px"; ListStyleType "disc"]] (List.map describe differing)
            ]

/// UI to display and manage parameters for a design sheet.
/// TODO: add structural abstraction.
let private makeParamsField model (comp:LoadedComponent) dispatch =
    let sheetDefaultParams = getDefaultParamDefs comp
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
                        th [] [str "Means"]
                        th [] [str "Value"]
                        th [] [str "Action"]
                    ]
                ]
                tbody [] (
                    sheetDefaultParams |> Map.toList |> List.map (fun (key, definition) ->
                        let paramName =
                            match key with
                            | ParameterTypes.ParamName s -> s
                        let defaultVal =
                            match definition.Expression with
                            |ParameterTypes.PInt i -> string i
                            | x -> string x
                        let paramVal, note = annotate key defaultVal
                        tr [] [
                            td [] [str paramName]
                            td [Style [FontSize "12px"]] [str definition.Description]
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

/// Resolve every parameterised slot of one component against the given bindings.
/// Used to work out the port widths of a custom component instance before it is placed, and to
/// refresh a LoadedComponent's port labels. The slot-to-field mapping is ComponentSlots'.
let resolveParametersForComponent
    (paramBindings: ParamBindings)
    (paramSlots: Map<ParamSlot, ConstrainedExpr>)
    (comp: Component)
    : Result<Component, string> =

    paramSlots
    |> Map.filter (fun slot _ -> slot.CompId = comp.Id)
    |> Map.toList
    |> List.fold
        (fun compRes (slot, constrainedExpr) ->
            compRes
            |> Result.bind (fun (compType: ComponentType) ->
                ParameterTypes.evaluateParamExpression paramBindings constrainedExpr.Expression
                |> Result.map (fun value -> ComponentSlots.setSlotValue slot.CompSlot value compType)))
        (Ok comp.Type)
    |> Result.map (fun compType -> { comp with Type = compType })

/// Update LoadedComponent port labels after parameter resolution
let updateLoadedComponentPorts (loadedComponent: LoadedComponent) : LoadedComponent =
    match loadedComponent.LCParameterSlots with
    | Some paramSlots when not (Map.isEmpty paramSlots.ParamSlots) ->
        // Apply parameter resolution to get updated port labels
        let (comps, conns) = loadedComponent.CanvasState
        let resolvedComps = 
            comps |> List.map (fun comp ->
                match resolveParametersForComponent (bindingsOf paramSlots.DefaultBindings) paramSlots.ParamSlots comp with
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
                    let paramBindings = paramBindingsOfModel model'
                    match ParameterTypes.evaluateParamExpression paramBindings paramSpec.Expression with
                    | Ok newValue ->
                        let newBindings =
                            match custom.ParameterBindings with
                            | Some bindings -> bindings
                            | None -> Map.empty
                            |> Map.add (ParamName parameterName) (PInt newValue)
                        
                        // the instance's port widths at the new bindings, from the child sheet's
                        // IO slots. Only this parameter is being set, so the others are still
                        // expressions in THIS sheet's parameters: see portWidthsOfInstance.
                        let labelToEval =
                            portWidthsOfInstance
                                project.LoadedComponents paramBindings custom.Name newBindings


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

//------------------------------------------------------------------------------------------------//
//------------------------------------- Bind-to-top offers ---------------------------------------//
//------------------------------------------------------------------------------------------------//

let private emptyParamDefs : ParameterDefs = {DefaultBindings = Map.empty; ParamSlots = Map.empty}

/// Apply one accepted chain action to the LoadedComponent of the sheet it names.
/// Both stores of an instance binding are kept in step: the CustomCompParam slot on the parent
/// sheet, and the ParameterBindings of the instance component in the parent's canvas.
let private applyChainActionToLdc (action: ParameterAnalysis.ChainAction) (ldc: LoadedComponent) : LoadedComponent =
    match action with
    | ParameterAnalysis.AddSheetParam (sheet, name, defVal, description) when sheet = ldc.Name ->
        let defs = Option.defaultValue emptyParamDefs ldc.LCParameterSlots
        match Map.containsKey name defs.DefaultBindings with
        | true -> ldc
        | false ->
            let definition = {Expression = PInt defVal; Description = description}
            {ldc with LCParameterSlots = Some {defs with DefaultBindings = Map.add name definition defs.DefaultBindings}}
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
            |> Option.map (fun defs -> bindingsOf defs.DefaultBindings)
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
                    // the sheets are the updated ones: an accepted offer can have just declared a
                    // parameter on the child sheet
                    let labelToEval =
                        portWidthsOfInstance ldcs' openDefaults childSheet newBindings
                    let updated = updateCustomComponent labelToEval newBindings comp
                    dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (ComponentId comp.Id, comp, updated.Type))))
                | _ -> ())
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        model.Sheet.DoBusWidthInference sheetDispatch


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

    /// Offers to bind an unbound parameter of THIS instance up to a same-named parameter on an
    /// ancestor sheet, materialising the chain of parameters and bindings along the way.
    /// Offered as a button rather than raised as a popup: the user meets it when they look at the
    /// instance, so nothing has to guess the moment at which to interrupt them.
    /// Suppressed while a simulation is open, as accepting one changes the design being simulated.
    let bindOffers =
        match model.CurrentProj with
        | None -> []
        | Some _ when ModelHelpers.simulationIsOpen model -> []
        | Some proj ->
            let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
            match ParameterAnalysis.effectiveTopSheet ldcs with
            | None -> []
            | Some top ->
                ParameterAnalysis.findBindOffers ldcs top (Some proj.OpenFileName)
                |> List.filter (fun offer -> offer.InstanceId = comp.Id)

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
                        
                        let bindButton =
                            match bindOffers |> List.tryFind (fun offer -> offer.Param = key) with
                            | None -> null
                            | Some offer ->
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ ->
                                        dispatch <| ExecFuncInMessage(applyBindOffers [offer], dispatch))
                                      Fulma.Button.Color IsSuccess
                                      Fulma.Button.IsLight
                                    ]
                                    [str $"Bind to {offer.BindsTo}"]

                        tr [] [
                            td [] [str paramName]
                            td [] [str paramValStr]
                            td [] [
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> editParameterBindingPopup model paramName paramValInt comp custom dispatch)
                                      Fulma.Button.Color IsInfo
                                    ]
                                    [str "Edit"]
                                bindButton
                            ]
                        ]
                    )
                )
            ]
        ]

//------------------------------------------------------------------------------------------------//
//------------------------- Asking for parameters when an instance is placed ---------------------//
//------------------------------------------------------------------------------------------------//

/// The value a parameter of `childLdc` takes when the sheet is viewed on its own.
let private childDefaultValue (childDefs: ParamDefinitions) (name: ParamName) : ParamInt =
    Map.tryFind name childDefs
    |> Option.map (fun def -> ParameterTypes.evaluateParamExpression (bindingsOf childDefs) def.Expression)
    |> Option.bind Result.toOption
    |> Option.defaultValue 1

/// A popup asking for a value for every parameter the sheet inside a custom component declares,
/// raised when an instance is placed.
/// Placing an instance without asking would silently freeze the child sheet's defaults into it -
/// the stale-chain problem - so the choice is made explicitly instead. Where the sheet the
/// instance is being placed on declares a parameter of the same name, a button binds to that
/// parameter rather than entering a value, and the two then stay in step.
/// `place` receives the bindings to put on the instance and the slots to create for it.
let customComponentParamPopup
        (childLdc: LoadedComponent)
        (place: ParamBindings -> NewParamCompSpec list -> unit)
        (model: Model)
        (dispatch: Msg -> unit)
        : unit =
    let childDefs = getDefaultParamDefs childLdc
    let parentDefs = model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
    let parentBindings = bindingsOf parentDefs
    let parentSheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""

    let slotOf (ParamName nameStr) = CustomCompParam nameStr

    /// the spec a parameter starts at: its child-sheet default, entered as a literal
    let literalSpec name : NewParamCompSpec =
        let value = childDefaultValue childDefs name
        {CompSlot = slotOf name; Expression = PInt value; Constraints = []; Value = value}

    /// the spec for a parameter bound to the same-named parameter of the parent sheet
    let boundSpec name : NewParamCompSpec =
        let value =
            ParameterTypes.evaluateParamExpression parentBindings (PParameter name)
            |> Result.toOption
            |> Option.defaultValue (childDefaultValue childDefs name)
        {CompSlot = slotOf name; Expression = PParameter name; Constraints = []; Value = value}

    // every parameter starts at its default, so confirming without touching anything places the
    // instance exactly as the sheet would be viewed standalone
    childDefs
    |> Map.toList
    |> List.iter (fun (name, _) -> dispatch <| AddPopupDialogParamSpec (slotOf name, Ok (literalSpec name)))

    let specOf (model': Model) name =
        model'.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind (slotOf name)

    let body (model': Model) =
        let renderParam (name: ParamName, definition: ParamDefinition) =
            let (ParamName nameStr) = name
            let isBoundToParent =
                match specOf model' name with
                | Some (Ok spec) -> spec.Expression = PParameter name
                | _ -> false
            let canBindToParent = Map.containsKey name parentDefs
            let valueEntry =
                match isBoundToParent with
                | true ->
                    div [Style [Color "grey"]] [str $"takes the value of {parentSheet}.{nameStr}"]
                | false ->
                    paramInputField model' $"Value for {nameStr}"
                        (childDefaultValue childDefs name) None [] None (slotOf name) dispatch
            let bindButton =
                match canBindToParent with
                | false -> null
                | true ->
                    let label, spec =
                        match isBoundToParent with
                        | true -> "Enter a value instead", literalSpec name
                        | false -> $"Bind to {parentSheet}.{nameStr}", boundSpec name
                    Button.button [
                        Button.Color IsInfo
                        Button.IsLight
                        Button.OnClick (fun _ -> dispatch <| AddPopupDialogParamSpec (slotOf name, Ok spec))
                    ] [str label]
            div [Key nameStr; Style [MarginBottom "12px"]] [
                b [] [str nameStr]
                p [Style [FontSize "11px"; Color "grey"]] [str definition.Description]
                valueEntry
                bindButton
            ]
        div [] [
            str $"{childLdc.Name} has parameters. Give each one a value for this instance, or bind \
                 it to a parameter of {parentSheet} so that the two stay in step."
            br []; br []
            div [] (childDefs |> Map.toList |> List.map renderParam)
        ]

    let buttonAction (model': Model) =
        let specs =
            childDefs
            |> Map.toList
            |> List.choose (fun (name, _) ->
                match specOf model' name with
                | Some (Ok spec) -> Some spec
                | _ -> None)
        let bindings =
            (Map.empty, childDefs |> Map.toList)
            ||> List.fold (fun acc (name, _) ->
                match specOf model' name with
                | Some (Ok spec) -> Map.add name spec.Expression acc
                | _ -> acc)
        place bindings specs
        dispatch ClosePopup

    // any parameter whose expression does not parse or breaks a constraint blocks placement
    let isDisabled (model': Model) =
        childDefs
        |> Map.toList
        |> List.exists (fun (name, _) ->
            match specOf model' name with
            | Some (Ok _) -> false
            | _ -> true)

    dialogPopup $"Parameters for {childLdc.Name}" body "Place" buttonAction isDisabled [] dispatch


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
            label [Class "label"] [ str "Parametrised Components"]
            // br []
            p [] [str "This sheet contains the following parametrised components"]
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
                Label.label [] [ str "Parametrised Components" ]
                p [] [str "This sheet does not contain any parametrised components." ]    
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
//----------------------------------- Top sheet choice on open -----------------------------------//
//------------------------------------------------------------------------------------------------//

/// A popup asking the user to choose the top sheet, or None. It fires only when several
/// top-level sheets exist, none has been chosen, and they disagree about the parameter values
/// the opened sheet displays with - roughly once per project. It never blocks opening.
/// Cancelling records the project in Model.TopSheetChoiceDeclined and the question is not asked
/// for it again, so the popup can never nag.
let topSheetChoiceCheck (model: Model) : ((Msg -> unit) -> Model -> ReactElement) option =
    match model.CurrentProj with
    | None -> None
    | Some proj when Set.contains proj.ProjectPath model.TopSheetChoiceDeclined -> None
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
                dispatch <| UpdateModel (Optic.map topSheetChoiceDeclined_ (Set.add proj.ProjectPath))
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
                                // the top has (for display purposes) just changed: redraw the
                                // open sheet at the values it now takes
                                dispatch ApplyComputedDisplayValues)
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
