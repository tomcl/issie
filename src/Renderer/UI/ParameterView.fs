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
///
/// Pure: the constraints that are not met are returned, not dispatched. This used to send a
/// notification from inside a List.filter, and one of its two callers is editParameterBox's
/// isDisabled - which the popup asks, while rendering, whether its button should be greyed out. A
/// constraint that could not be evaluated would have dispatched from there, re-rendered, and
/// dispatched again. It also sent SetPopupDialogText, which is where some popups keep the text the
/// user is typing.
///
/// Everything the caller has to say is carried in the ParamConstraint it gets back, which is where
/// the caller already looks for it: a bound that cannot be worked out comes back as a constraint
/// whose message says so.
let evaluateConstraints
        (paramBindings: ParamBindings)
        (exprSpecs: ConstrainedExpr list)
            : Result<Unit, ParamConstraint list> =

    /// The constraints on one expression that its value does not meet.
    let unmetConstraints (exprSpec: ConstrainedExpr) =
        match ParameterTypes.evaluateParamExpression paramBindings exprSpec.Expression with
        // an expression that cannot be evaluated fails its constraints: returning no failures here
        // would let an undefined value through the guard
        | Error err -> [MinVal (exprSpec.Expression, $"This value could not be worked out. {err}")]
        | Ok value ->
            exprSpec.Constraints
            |> List.choose (fun konst ->
                let bound, message =
                    match konst with
                    | MaxVal (bound, message) | MinVal (bound, message) -> bound, message
                match ParameterTypes.evaluateParamExpression paramBindings bound with
                // a bound that cannot be worked out cannot pass the value it guards
                | Error err -> Some (MinVal (bound, $"{message} - but that limit could not be worked out. {err}"))
                | Ok limit ->
                    match konst with
                    | MaxVal _ when value > limit -> Some konst
                    | MinVal _ when value < limit -> Some konst
                    | _ -> None)

    match exprSpecs |> List.collect unmetConstraints with
    | [] -> Ok ()
    | unmet -> Error unmet


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
/// The work is CanvasExtractor.signatureOfInstance's, which is the one place that knows what an
/// instance's ports are; this only turns its answer into the label-to-width map that
/// updateCustomComponent wants. The label comes from the child sheet's Input or Output COMPONENT,
/// not from the `IO` slot that sets its width, so a port renamed since its slot was created is
/// still sized by that slot - see ParameterTypes.sameSlotName.
let portWidthsOfInstance
        (ldcs: LoadedComponent list)
        (parentBindings: ParamBindings)
        (childSheet: string)
        (instanceBindings: ParamBindings)
        : Map<string, int> =
    CanvasExtractor.signatureOfInstance ldcs parentBindings childSheet instanceBindings
    |> Option.map (fun (ins, outs) -> ins @ outs |> Map.ofList)
    |> Option.defaultValue Map.empty

/// Push the values of the parameterised slots of ONE component onto the canvas.
/// All of a component's slots are applied together because two of the messages replace a whole
/// field of the component type - a SplitN's width and LSB lists, a custom component's parameter
/// bindings - and are built here from `model`, which is a snapshot: issued one slot at a time
/// they would overwrite each other, leaving all but the last slot at its old value.
/// (ChangeWidth and ChangeInputValue read the live symbol and so do not have this problem.)
let updateComponentSlots dispatch (model: Model) (compIdStr: string) (slotValues: (CompSlotName * ParamInt) list) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    let comp = model.Sheet.GetComponentById <| ComponentId compIdStr
    let compId = ComponentId comp.Id
    let valueOf slot = slotValues |> List.tryPick (fun (s, v) -> if s = slot then Some v else None)

    /// A width, an index or a bit position is an int in the component and in the message that sets
    /// it, so a value too large to be one cannot be applied. The slot's own constraint should have
    /// stopped it before here - see ComponentSlots.trySetSlotValue, which says the same.
    let asInt (slot: CompSlotName) (value: ParamInt) =
        match tryIntOfParamInt value with
        | Some intValue -> Some intValue
        | None ->
            Log.warn $"Parameter value {value} is too large for slot {slot}, which holds a whole \
                       number: the component is unchanged"
            None
    let intValueOf slot = valueOf slot |> Option.bind (asInt slot)

    match comp.Type with
    | SplitN (n, widths, lsbs) ->
        let newWidths = widths |> List.mapi (fun i w -> intValueOf (SplitNWidth i) |> Option.defaultValue w)
        let newLsbs = lsbs |> List.mapi (fun i l -> intValueOf (SplitNLSB i) |> Option.defaultValue l)
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
            // A ChangeCustom leaves the wires at their old widths: unlike ChangeWidth, which runs
            // inference itself, the symbol message returns Cmd.none. Binding a parameter here
            // changes the instance's port widths, so the wires attached to them have to be
            // re-inferred or they keep the widths they had before the edit.
            model.Sheet.DoBusWidthInference sheetDispatch
        | None ->
            // Fallback to just updating bindings if no project context
            let newCustomComp = { customComp with ParameterBindings = Some newBindings }
            dispatch <| Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeCustom (compId, comp, Custom newCustomComp))))
            model.Sheet.DoBusWidthInference sheetDispatch

    | _ ->
        slotValues
        |> List.iter (fun (slot, value) ->
            match comp.Type, slot with
            // an IO slot is a width only on an Input/Output. On these two the properties pane puts
            // the BusSelection LSB and the BusCompare comparison value in it - see
            // SelectedComponentView.makeLsbBitNumberField - and ChangeLSB is what sets both.
            | (BusSelection _ | BusCompare _), IO _ ->
                model.Sheet.ChangeLSB sheetDispatch compId value
            | _, Buswidth | _, IO _ ->
                asInt slot value |> Option.iter (model.Sheet.ChangeWidth sheetDispatch compId)
            | Input1 _, InputDefault -> model.Sheet.ChangeInputValue sheetDispatch compId value
            | _, InputDefault -> failwithf $"Default value cannot be set on {comp.Type}"
            | _, (SplitNWidth _ | SplitNLSB _) -> failwithf $"SplitN slots cannot be applied to {comp.Type}"
            | _, CustomCompParam _ -> failwithf $"CustomCompParam can only be used with Custom components")

    // Update most recent bus width
    slotValues
    |> List.iter (fun (slot, value) ->
        match slot, comp.Type with
        | Buswidth, SplitWire _ | Buswidth, BusSelection _ | Buswidth, Constant1 _ -> ()
        | Buswidth, _ | IO _, _ ->
            asInt slot value |> Option.iter (ReloadSelectedComponent >> dispatch)
        | _ -> ())

/// Use sheet component update functions to perform an update to a single slot.
let updateComponent dispatch model (slot: ParamSlot) (value: ParamInt) =
    updateComponentSlots dispatch model slot.CompId [slot.CompSlot, value]


// exprContainsParams has been moved to ParameterTypes module


/// The open sheet's parameter data has just changed.
///
/// Nothing in the canvas need have changed with it: declaring a parameter, writing its
/// description, or giving a slot an expression that works out to the width already shown all leave
/// the canvas identical. UpdateHelpers.currentSheetIsOutOfDate compares only the canvas, so
/// without this the save button stays dark, switching sheets does not save, and the work is
/// silently dropped.
///
/// LoadedComponentIsOutOfDate is the flag that survives: SavedSheetIsOutOfDate is recomputed from
/// it on every draw block message, and is set here too only so that the button responds before the
/// next such message arrives. Saving the sheet clears both.
/// The work is ParameterAnalysis.markSheetOutOfDate, which is a function of the loaded components
/// alone and so can be tested without building a Model; this only reaches it.
let markSheetParamsChanged (model: Model) : Model =
    match model.CurrentProj with
    | None -> model
    | Some proj ->
        model
        |> Optic.map (projectOpt_ >?> loadedComponents_) (ParameterAnalysis.markSheetOutOfDate proj.OpenFileName)
        |> Optic.set savedSheetIsOutOfDate_ true

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

    // addSlot and removeSlot rather than Map.add and Map.remove: a slot stored under the
    // component's old label is the same slot, and adding a second one for the same field left the
    // two to fight over the component's width in Map key order. See ParameterTypes.sameSlot.
    let newParamSlots =
        match ParameterTypes.exprContainsParams exprSpec.Expression with
        | true  -> ParameterTypes.addSlot slot exprSpec paramSlots
        | false -> ParameterTypes.removeSlot slot paramSlots

    match newParamSlots = paramSlots with
    | true -> model
    | false -> model |> set paramSlotsOfModel_ newParamSlots |> markSheetParamsChanged


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
    (defaultValue: ParamInt)
    (currentValue: Option<ParamInt>)
    /// The expression this field is currently set to, where it is held somewhere other than a slot
    /// of the open sheet - a custom component instance keeps its bindings on the instance. The box
    /// shows what the value IS, so a value set to a symbol reads as that symbol.
    (currentExpr: Option<ParamExpression>)
    (constraints: ParamConstraint list)
    (comp: Component option)
    (compSlotName: CompSlotName)
    (dispatch: Msg -> unit)
    : ReactElement =

    /// Which box this is. The component id is part of it because slot names are shared: every
    /// component with a width has `Buswidth`, so without it one component's box read another's.
    let boxKey = ParameterTypes.paramBoxKey (comp |> Option.map (fun c -> c.Id)) compSlotName

    let onChange inputExpr =
        let paramBindings = paramBindingsOfModel model

        // Only return first violated constraint
        let checkConstraints expr =
            let exprSpec = {Expression = expr; Constraints = constraints}
            match evaluateConstraints paramBindings [exprSpec] with
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
            dispatch <| AddPopupDialogParamSpec (boxKey, {Text = inputExpr; Spec = Ok newCompSpec})
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
        // The text is recorded whether or not it means anything yet, because it is what the box
        // shows: an entry that will not parse stays on screen with its message until it is fixed,
        // and nothing reaches the component in the meantime.
        | Error err, _, _
        | _, Error err, _ -> dispatch <| AddPopupDialogParamSpec (boxKey, {Text = inputExpr; Spec = Error err})
        | _ -> failwithf "Value cannot exist with invalid expression"

    let boxState =
        model.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind boxKey

    let slots = model |> getCurrentSheet |> getParamSlots
    /// What the box shows: the text being typed where there is any, otherwise the value the
    /// component is actually at. Same shape as the Constant box, which shows
    /// `PopupDialogData.Text` and falls back to the component's own stored text.
    let committedString =
        // tryFindSlot, not Map.tryFind: an IO slot created before the component was renamed is
        // stored under the old label and is still this field's slot, so a rename must not blank
        // the expression out of the box. See ParameterTypes.sameSlot.
        comp
        |> Option.bind (fun c -> ParameterTypes.tryFindSlot {CompId = c.Id; CompSlot = compSlotName} slots)
        |> Option.map (fun exprSpec -> exprSpec.Expression)
        // a custom component instance may hold its binding on the instance rather than in a slot
        // of the sheet it sits on, and the box must show what the value IS either way: a binding
        // that reads as a number cannot be told from one the user typed
        |> Option.orElse currentExpr
        |> Option.map (fun expr -> ParameterTypes.renderParamExpression expr 0)
        |> Option.defaultValue (currentValue |> Option.defaultValue defaultValue |> string)

    let inputString =
        boxState |> Option.map (fun s -> s.Text) |> Option.defaultValue committedString

    let errText =
        boxState
        |> Option.map (fun s -> match s.Spec with | Ok _ -> "" | Error err -> err)
        |> Option.defaultValue ""

    // Field name, input box, and potential error message.
    //
    // An empty prompt means the caller has labelled the field itself and this one must not add a
    // second label: a property block heads itself with the name and what it means, on separate
    // lines, which one label string cannot do.
    Field.div [] [
        if prompt <> "" then PropertiesHelp.fieldLabel prompt
        Field.div [Field.Option.HasAddons] [
            Control.div [] [
                Input.text [
                    if errText <> "" then
                        Input.Option.CustomClass "is-danger"
                    Input.Props [
                        OnPaste preventDefault
                        SpellCheck false
                        // stable even when the caller labels the field: the slot names the box
                        Name (if prompt = "" then string compSlotName else prompt)
                        // no AutoFocus - see the name box in SelectedComponentView
                        Style [Width "200px"]
                    ]
                    // Value, not DefaultValue: the box renders from the model, so setting it from
                    // elsewhere - the offer to follow an enclosing sheet's property - is an
                    // ordinary dispatch. That is only safe because the model holds the text AS
                    // TYPED, valid or not; a box rendered from the parsed value would rewrite a
                    // half-typed expression under the caret on every keystroke.
                    Input.Value inputString
                    Input.Type Input.Text
                    Input.OnChange (getTextEventValue >> onChange)
                ]
            ]
            // What the expression works out to, shown only when the box does not already say it -
            // a plain number needs no restating. Written as "= 8" rather than a bare "8", which
            // beside a box reading "W" looked like a second field rather than its value.
            if currentValue.IsSome && string currentValue.Value <> inputString then
                Control.p [] [
                    Button.a [Button.Option.IsStatic true] [
                        str $"= {currentValue.Value}"
                    ]
                ]
        ]
        div [Class "propertyMessage"] [str errText]
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
            Log.warn $"Skipping parameter slot of component {slot.CompId}, which is not on this sheet"
            None
        | true ->
            match ParameterTypes.evaluateParamExpression newBindings exprSpec.Expression with
            | Ok value -> Some value
            | Error err ->
                Log.warn $"Skipping parameter slot of component {slot.CompId}: {err}"
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
//------------------------------- Propagating parameter values -----------------------------------//
//------------------------------------------------------------------------------------------------//

/// Bring every sheet into line with what its design sets its parameters to.
///
/// The recomputation itself is pure (ParameterAnalysis.propagateParameterValues); this is the part
/// that has to touch the world, and it treats the open sheet differently from the rest because its
/// canvas is not in LoadedComponents but in the draw block:
///
///   - a CLOSED sheet whose values changed is rewritten and written to its file at once, because
///     only the open sheet is ever allowed to be unsaved. A sheet that cannot be written - a
///     library component, which belongs to its library - is still brought into line in memory, so
///     that looking at it shows the right thing, but its file is left alone and it is not called
///     unsaved: the difference is derived and there is nothing for the user to save;
///   - the OPEN sheet's values are pushed through the same symbol-change path the properties pane
///     uses, so symbol size, ports and geometry are recomputed rather than patched, and the change
///     joins that sheet's undo history like any other edit.
let propagateParameters (model: Model) (dispatch: Msg -> unit) : unit =
    match model.CurrentProj with
    | None -> ()
    | Some project ->
        let openName = project.OpenFileName
        let before = (ModelHelpers.getUpdatedLoadedComponents project model).LoadedComponents
        let after = ParameterAnalysis.propagateParameterValues before
        // A sheet this recomputation changed differs from its file and so must reach it. Flagging
        // rather than writing here keeps the write in one place - MenuHelpers.saveDirtyClosedSheets
        // is what every path that touches a sheet other than the open one goes through - and picks
        // up sheets the triggering edit dirtied as well, which this recomputation did not touch.
        let updated =
            List.zip before after
            |> List.map (fun (b, a) ->
                match a.CanvasState <> b.CanvasState || a.LCParameterSlots <> b.LCParameterSlots with
                | true -> {a with LoadedComponentIsOutOfDate = true}
                | false -> a)
            |> MenuHelpers.saveDirtyClosedSheets openName
        dispatch <| UpdateModel (fun m ->
            {m with CurrentProj = Some {project with LoadedComponents = updated}})
        // the open sheet's canvas is the draw block's, so its slots go through symbol messages
        let openBindings =
            updated
            |> List.tryFind (fun ldc -> ldc.Name = openName)
            |> Option.bind (fun ldc -> ldc.LCParameterSlots)
            |> Option.map (fun defs -> bindingsOf defs.DefaultBindings)
            |> Option.defaultValue Map.empty
        updateComponents openBindings model dispatch


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
    | DefaultParams of Name: string * Value: ParamInt * Description: string * Delete: bool
    | ParamSlots of ParamSlot * ParameterTypes.ParamExpression * ParamConstraint list


let updateInfoSheetDefaultParams
        (currentSheetInfo: option<ParameterTypes.ParameterDefs>)
        (paramName: string)
        (value: ParamInt)
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
    // the canvas is untouched by a change to what the sheet declares, so say the sheet needs
    // saving rather than leaving it to be inferred from a canvas that is identical
    (updateParameter newProject >> markSheetParamsChanged) |> UpdateModel |> dispatch
    // what this sheet declares is what its subsheets' values are derived from, so they follow
    dispatch PropagateParameters

/// Every instance of a sheet binds every parameter that sheet declares.
///
/// Placing an instance establishes that - customComponentParamPopup asks for a value for each
/// parameter - but a parameter added to a sheet that ALREADY has instances would leave every one
/// of them binding nothing. An unbound parameter is a state the design deliberately does not have:
/// it elaborates at the sheet's own declared value, which is a fact about the sheet and not about
/// the instance, and it makes "default" into a concept the user has to reason about.
///
/// This is the mirror of removeParamFromInstances, which drops a deleted parameter's binding from
/// every instance across the project, and it works the same way: LoadedComponent canvases only,
/// since a sheet cannot instantiate itself and so the open sheet's own canvas can hold no instance
/// of it.
///
/// The value bound is the one just declared, so nothing about the design changes - that is exactly
/// what an unbound parameter elaborated to. What changes is that the binding exists, can be seen
/// and edited, and, being a literal, is what the bind-to-top offer fires on: following an outer
/// parameter of the same name is then offered on each instance rather than having to be found.
/// No slot is created, because a literal needs none (see updateParamSlot).
/// The work itself is ParameterAnalysis.bindParamOnInstances, which is a function of the loaded
/// components alone and so can be tested without building a Model; this only reaches it.
let addParamToInstances (sheetName: string) (name: ParamName) (value: ParamInt) (model: Model) : Model =
    let openName = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
    model
    |> Optic.map (projectOpt_ >?> loadedComponents_)
        (ParameterAnalysis.bindParamOnInstances sheetName name value
         >> MenuHelpers.saveDirtyClosedSheets openName)

/// How many instances of a sheet the rest of the project holds.
let private instanceCountOf (sheetName: string) (project: Project) =
    project.LoadedComponents
    |> List.filter (fun ldc -> ldc.Name <> sheetName)
    |> List.sumBy (fun ldc ->
        fst ldc.CanvasState
        |> List.filter (fun comp ->
            match comp.Type with
            | Custom cc -> cc.Name = sheetName
            | _ -> false)
        |> List.length)

/// Creates a popup that allows a parameter integer value to be added.
let addParameterBox model dispatch =
    match model.CurrentProj with
    | None -> Log.warn "testAddParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Add property"

        let textPrompt =
            fun _ ->
                div []
                    [
                        str "Specify the property name:"
                        br []
                    ]

        let descriptionPrompt =
            fun _ ->
                div []
                    [
                        str "What does this property mean?"
                        br []
                    ]

        let intPrompt =
            fun _ ->
                div []
                    [
                        str "Default value for the property:"
                        br []
                    ]

        let defaultVal = 1I
        /// The parameters this sheet already declares. Read from `project` rather than from the
        /// dialog's own model so that it is the sheet being edited, not whatever is open later.
        let declaredNames =
            project.LoadedComponents
            |> List.tryFind (fun lc -> lc.Name = project.OpenFileName)
            |> Option.map (getDefaultParamDefs >> Map.toList >> List.map (fst >> fun (ParamName n) -> n))
            |> Option.defaultValue []
        let nameIsTaken name = List.contains name declaredNames
        let body =
            dialogPopupBodyTextDescriptionAndInt
                textPrompt "example: x"
                descriptionPrompt "example: width of the data bus in bits"
                intPrompt defaultVal nameIsTaken dispatch
        let buttonText = "Add property"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) ->
                let newParamName = getText model'.PopupDialogData
                let newValue = getInt2 model'.PopupDialogData
                let newDescription = getText2 model'.PopupDialogData

                modifyInfoSheet (project) (DefaultParams (newParamName, newValue, newDescription, false)) dispatch
                // Every instance of this sheet must bind the new parameter: see addParamToInstances.
                // Bound at the value just declared, so the design is unchanged by this.
                let sheetName = project.OpenFileName
                dispatch <| UpdateModel (addParamToInstances sheetName (ParamName newParamName) newValue)
                // Said rather than asked. Filling the bindings silently would leave the user to
                // discover that existing instances had acquired a value; a modal per instance would
                // interrupt for something that changes nothing. The bind button the note points at
                // is the affordance that does the interesting part.
                match instanceCountOf sheetName project with
                | 0 -> ()
                | n ->
                    let plural = if n = 1 then "instance" else "instances"
                    dispatch <| SetPropertiesNotification (Notifications.successPropertiesNotification
                        $"{n} {plural} of {sheetName} now use {newParamName} = {newValue}. Their \
                          properties offer binding it to a parameter of an enclosing sheet.")
                // Close popup window
                ClosePopup |> dispatch
                // a new parameter may be the missing ancestor that lets same-named parameters in
                // the sheets below be bound to it; that shows up as a bind button in those
                // instances' properties rather than as a popup raised here

        // A parameter name is a letter followed by letters and numbers, and not the name of a
        // built-in function - the parser's rule, since a name that cannot be written in an
        // expression is of no use - and it must not be one the sheet already declares, since adding
        // it again silently replaced the declaration. Every parameter must be described: the
        // description is what instances of this sheet show when asking for a value. And the value
        // must be a number: the box takes text so that a parameter larger than an int can be typed.
        // The name rule used to be `[a-zA-Z0-9]+` here while the popup's own red text said the name
        // had to start with a letter, so a name beginning with a digit was flagged and accepted.
        let isDisabled =
            fun (model': Model) ->
                 let newParamName = getText model'.PopupDialogData
                 not (ParameterTypes.isValidParamName newParamName)
                 || nameIsTaken newParamName
                 || getText2 model'.PopupDialogData = ""
                 || model'.PopupDialogData.Int2.IsNone

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch

/// Creates a popup that allows a parameter integer value to be edited.
/// TODO: this should be a special cases of a more general popup for parameter expressions?
let editParameterBox model parameterName dispatch   = 
    match model.CurrentProj with
    | None -> Log.warn "testEditParameterBox called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let currentSheet = project.LoadedComponents
                                   |> List.find (fun lc -> lc.Name = project.OpenFileName)
        let title = "Edit property"
        match getDefaultParamDefs currentSheet |> Map.tryFind (ParamName parameterName) with
        | None ->
            // the row was rendered from an older model in which the parameter still existed
            Log.warn $"Cannot edit parameter {parameterName}: it is not defined on this sheet"
        // Written as "anything that is not a literal" rather than as a list of the expression
        // cases, so that a case added to ParamExpression cannot slip through into the value editor
        // below and be silently replaced by its `| _ -> 1I` default.
        | Some {Expression = expr} when (match expr with PInt _ -> false | _ -> true) ->
            dispatch <| SetPropertiesNotification (Notifications.errorPropsNotification
                $"{parameterName} is set to an expression. Only whole-number property values can be edited here.")
        | Some currentDef ->
        let currentValue = currentDef.Expression
        let descriptionPrompt =
            fun _ ->
                div []
                    [
                        str $"What does {parameterName} mean?"
                        br []
                    ]

        // The pane no longer names the declared value: where the instances agree it shows the
        // agreed value, and calling that a default only asked the user to reason about something
        // that is nearly always overwritten. But this box edits the DECLARED value, so it has to
        // say so - otherwise a user whose instances all bind a literal would change the number
        // here and see nothing move.
        let intPrompt =
            fun _ ->
                div []
                    [
                        str $"Value of {parameterName} when this sheet has no instances:"
                        br []
                        str $"(currently {currentValue})"
                        br []
                        str "Each place this sheet is used gives its own value, and those are \
                             edited on the instance."
                    ]

        let defaultVal =
            match currentValue with
            | PInt intVal -> intVal
            | _ -> 1I // non-integer bindings are rejected above

        /// the declarations of this sheet with the edited parameter replaced, as an evaluation
        /// environment
        let editedBindings (model': Model) =
            model'
            |> getLCParamInfo
            |> (fun info -> info.DefaultBindings)
            |> Map.add (ParamName parameterName) {Expression = PInt (getInt2 model'.PopupDialogData); Description = getText2 model'.PopupDialogData}
            |> bindingsOf

        /// The slots of this sheet, whose constraints are what the new value has to satisfy.
        let slotSpecs (model': Model) =
            model'
            |> get paramSlotsOfModel_
            |> Option.defaultValue Map.empty
            |> Map.toList
            |> List.map snd

        /// What is wrong with the value in the box, in the words of whoever wrote the constraint.
        /// Only the first failure is shown: they are nearly always the same slot objecting from
        /// several components, and a column of repeated sentences reads as noise.
        let valueProblem (model': Model) =
            match model'.PopupDialogData.Int2 with
            | None -> None // the value does not parse at all, which paramValueError already says
            | Some _ ->
                match evaluateConstraints (editedBindings model') (slotSpecs model') with
                | Ok () -> None
                | Error [] -> None
                | Error (firstConstraint :: _) ->
                    match firstConstraint with
                    | MinVal (_, err) | MaxVal (_, err) -> Some err

        // the existing description is seeded into the dialog so that editing the value alone
        // does not silently blank it
        dispatch <| SetPopupDialogText2 (Some currentDef.Description)
        let body =
            dialogPopupBodyDescriptionAndInt
                descriptionPrompt currentDef.Description intPrompt defaultVal valueProblem dispatch
        let buttonText = "Set value"

        // Update the parameter value then close the popup
        let buttonAction =
            fun (model': Model) ->
                let newValue = getInt2 model'.PopupDialogData
                let newDescription = getText2 model'.PopupDialogData
                modifyInfoSheet project (DefaultParams (parameterName, newValue, newDescription, false)) dispatch

                // Value must meet constraints if able to click button
                updateComponents (editedBindings model') model dispatch
                dispatch <| ClosePopup

        // Disabled if the value does not parse, the description has been emptied, or any constraint
        // is violated. The last of those is explained by valueProblem above: the button used to grey
        // out saying nothing, leaving the user to work out which component objected.
        let isDisabled =
            fun (model': Model) ->
                model'.PopupDialogData.Int2.IsNone
                || getText2 model'.PopupDialogData = ""
                || (evaluateConstraints (editedBindings model') (slotSpecs model') |> Result.isError)

        dialogPopup title body buttonText buttonAction isDisabled [] dispatch


/// Human readable name of the slot a parameter expression fills, for use in messages.
let describeSlot (model: Model) (slot: ParamSlot) =
    let slotName =
        match slot.CompSlot with
        | Buswidth -> "Buswidth"
        | IO label -> $"Input/output {label}"
        | SplitNWidth idx -> $"SplitN output {idx} width"
        | SplitNLSB idx -> $"SplitN output {idx} LSB"
        | CustomCompParam paramName -> $"Property {paramName}"
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
    let openName = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""
    let updateSheets (ldcs: LoadedComponent list) =
        ldcs
        |> List.map (fun ldc -> if ldc.Name = sheetName then ldc else dropFromSheet ldc)
        |> MenuHelpers.saveDirtyClosedSheets openName
    model
    |> Optic.map (projectOpt_ >?> loadedComponents_) updateSheets

/// Delete a sheet parameter. A slot referring to a parameter that does not exist is an undefined
/// design, so this refuses while any slot on the sheet still refers to it and says which ones.
let deleteParameterBox model parameterName dispatch  =
    match model.CurrentProj with
    | None -> Log.warn "deleteParameterBox called when no project is currently open"
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
            modifyInfoSheet project (DefaultParams (parameterName, 0I, "", true)) dispatch
            dispatch <| UpdateModel (removeParamFromInstances sheet.Name name)
        | _ ->
            let body =
                div []
                    [ str $"Property {parameterName} cannot be deleted because it is still used by \
                            the following component slots on this sheet:"
                      br []
                      ul [Style [MarginLeft "20px"; ListStyleType "disc"]]
                          (users |> List.map (fun (slot, _) -> li [] [str (describeSlot model slot)]))
                      br []
                      str "Give each of them a value that does not use this property, then delete it." ]
            closablePopup $"Cannot delete property {parameterName}" body (div [] []) [] dispatch


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
        | true -> p [Style [Color "red"]] [str "Close all simulations to change the properties of this sheet."]
    // What each parameter of this sheet is set to by the instances of it in its own design.
    let displayValues =
        match model.CurrentProj with
        | None -> Map.empty
        | Some proj ->
            let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
            ParameterAnalysis.displayValues ldcs (ParameterAnalysis.effectiveTopSheetFor ldcs comp.Name) comp.Name
    /// The value to show for a parameter, and whether anything has settled it.
    ///
    /// Where instances set it, that is the value, and the stored one is not mentioned - it has
    /// been overwritten and saying so would only ask the user to reason about something that no
    /// longer applies. Where one design reaches the sheet by paths that set it differently, the
    /// largest is what the sheet is drawn at and the others follow it, at most two before an
    /// ellipsis: `16 (also 8)`, `16 (also 8, 4)`, `16 (also 8, 4, ...)`.
    ///
    /// Where nothing sets it, the stored value is all there is, and the caller shows it as
    /// provisional rather than as a fact - see atDefault.
    let shownFor (key: ParamName) (storedText: string) : string * bool =
        match Map.tryFind key displayValues with
        | Some (ParameterAnalysis.Values (largest :: others)) ->
            let text =
                match others with
                | [] -> string largest
                | _ ->
                    let listed = others |> List.truncate 2 |> List.map string |> String.concat ", "
                    let ellipsis = if List.length others > 2 then ", ..." else ""
                    $"{largest} (also {listed}{ellipsis})"
            text, true
        | _ -> storedText, false
    /// Adding a parameter is the way in to the feature, so it is also where the feature gets
    /// explained - once per project, before the first parameter the user declares themselves.
    /// The condition is derived rather than recorded, so there is no flag to keep in step; a
    /// project emptied of parameters becomes eligible again, which is right.
    let addParameterButton =
        let explainFirst () =
            let projectUsesParams =
                model.CurrentProj
                |> Option.map (fun proj -> ParameterAnalysis.projectDeclaresParams proj.LoadedComponents)
                |> Option.defaultValue false
            match projectUsesParams with
            | true -> addParameterBox model dispatch
            | false ->
                let body = UIPopups.helpText AppMessages.Confirm.usingParameters
                confirmationPopup "Using properties" "Add a property" body
                    (fun _ ->
                        dispatch ClosePopup
                        addParameterBox model dispatch)
                    dispatch
        Button.button
            [ Fulma.Button.OnClick(fun _ -> explainFirst ())
              Fulma.Button.Color IsInfo
              Fulma.Button.Disabled simIsOpen
            ]
            [str "Add Property"]
    match sheetDefaultParams.IsEmpty with
    // Nothing at all beyond the way in. A sheet with no parameters used to carry a heading and a
    // sentence about not having any, on every sheet, for ever - which is the plainest possible
    // breach of "users who never touch parameters see no change anywhere".
    | true ->
        div [] [
            simWarning
            addParameterButton
            ]
    | false ->

        /// One property: its name and value on a line, what it means underneath in the pane's full
        /// width, and the buttons that act on it below that.
        ///
        /// This was a four-column table. A description is the thing a property most needs read, and
        /// it had about a hundred pixels of a four-hundred pixel pane to be read in, so it wrapped
        /// to four lines beside a value that wanted twelve. Laid down the pane instead, the
        /// description gets the width, and the block is what separates one property from the next.
        let propertyBlock =
            (fun (key: ParamName, definition: ParamDefinition) ->
                        let paramName =
                            match key with
                            | ParameterTypes.ParamName s -> s
                        // rendered, not `string x`: the F# form of the value printed the raw
                        // discriminated union into the table, so a parameter declared as an
                        // expression showed as "PAdd (PInt 1, PInt 2)" rather than as "1+2"
                        let storedVal =
                            ParameterTypes.renderParamExpression definition.Expression 0
                        let paramVal, isSettled = shownFor key storedVal
                        // Grey and italic where nothing has settled the value, so that a
                        // provisional number is never read as a fact about the design. The tooltip
                        // says what will settle it.
                        let valueSpan =
                            match isSettled with
                            | true -> span [Class "propertyValue"] [str paramVal]
                            | false ->
                                span [
                                    Class $"propertyValue isPlaceholder {Tooltip.ClassName} \
                                            {Tooltip.IsTooltipLeft} {Tooltip.IsMultiline}"
                                    Tooltip.dataTooltip
                                        "Nothing uses this sheet yet, so this value is only a \
                                         placeholder. It is replaced by whatever a design gives \
                                         the property once the sheet is used in one."
                                ] [str paramVal]
                        div [Key paramName; Class "propertyBlock"] [
                            div [Class "propertyHead"] [
                                span [Class "propertyName"] [str paramName]
                                valueSpan
                            ]
                            div [Class "propertyMeans"] [str definition.Description]
                            div [Class "propertyControls"] [
                                // Editing is offered only while nothing sets the value. Where a
                                // design does, an edit here would be overwritten the moment the
                                // value was recomputed, so there is nothing to offer.
                                if not isSettled then
                                    Button.button
                                        [ Fulma.Button.OnClick(fun _ -> editParameterBox model (paramName) dispatch)
                                          Fulma.Button.Color IsInfo
                                          Fulma.Button.Size IsSmall
                                          Fulma.Button.Disabled simIsOpen
                                        ]
                                        [str "Edit"]
                                Button.button
                                    [ Fulma.Button.OnClick(fun _ -> deleteParameterBox model (paramName) dispatch )
                                      Fulma.Button.Color IsDanger
                                      Fulma.Button.Size IsSmall
                                      Fulma.Button.Disabled simIsOpen
                                    ]
                                    [str "Delete"]
                                ]
                            ])

        div [] [
            // A sheet DECLARES properties; an instance of a sheet SETS values for them. Both blocks
            // were headed "Parameters" and looked much alike, which was the whole of the confusion.
            // They share a block shape now but not a renderer: this one shows a value and offers
            // Edit/Delete, while the instance's offers a box to type in.
            Label.label [] [str "Properties this sheet declares"]
            simWarning
            div [] (sheetDefaultParams |> Map.toList |> List.map propertyBlock)
            addParameterButton
        ]

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
                {ldc' with LoadedComponentIsOutOfDate = true}
        let ldcs' =
            project.LoadedComponents
            |> List.map updateLdc
            |> MenuHelpers.saveDirtyClosedSheets openName
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


let paramPrompt (nameStr: string) (definition: ParamDefinition) : string =
    match definition.Description with
    | "" -> nameStr
    | description -> $"{nameStr}: {description}"

/// The values one custom component instance supplies for the parameters of the sheet inside it.
///
/// One labelled box per parameter, exactly as a built-in component's width is edited: an instance
/// of a sheet is not a different kind of thing from a Register, and the pane should not make it
/// look like one. The prompt is the parameter's declared description - compulsory precisely so
/// that it can be read where the value is chosen - and the parameter's NAME is deliberately
/// absent. An instance binding is an expression in the parameters of the sheet the instance SITS
/// ON, so the child's name for it is never something the user can type here; it only identifies
/// which slot is being set.
///
/// Parameters come out in Map order, which is alphabetical by that hidden name. With one
/// parameter - the case component libraries are built around - there is nothing to order. With
/// several the sequence is arbitrary from the user's side, and nothing records an authored order
/// to use instead; that is accepted rather than solved.
let makeParamBindingEntryBoxes model (comp:Component) (custom:CustomComponentType) dispatch =
    let ccParams = custom.ParameterBindings |> Option.defaultValue Map.empty

    /// What the sheet inside declares, descriptions included: the prompts come from here.
    let childDefs =
        model.CurrentProj
        |> Option.bind (fun proj ->
            proj.LoadedComponents |> List.tryFind (fun ldc -> ldc.Name = custom.Name))
        |> Option.map getDefaultParamDefs
        |> Option.defaultValue Map.empty

    let slots = model |> getCurrentSheet |> getParamSlots
    let bindings = paramBindingsOfModel model

    /// What this instance sets one property to: the slot expression where there is one, otherwise
    /// the binding held on the instance itself. Every instance sets every property its sheet
    /// declares - repaired on load where an old project does not - so there is always one.
    let exprOf (paramName: string) (key: ParamName) : ParamExpression option =
        Map.tryFind {CompId = comp.Id; CompSlot = CustomCompParam paramName} slots
        |> Option.map (fun spec -> spec.Expression)
        |> Option.orElse (Map.tryFind key ccParams)

    /// What that works out to here, for the `= n` beside a box set to a symbol.
    let valueOf (key: ParamName) (paramName: string) : ParamInt option =
        exprOf paramName key
        |> Option.bind (ParameterTypes.evaluateParamExpression bindings >> Result.toOption)

    /// Offers to bind a parameter of THIS instance up to a same-named parameter on an ancestor
    /// sheet, materialising the chain of parameters and bindings along the way.
    /// Offered as a button rather than raised as a popup: the user meets it when they look at the
    /// instance, so nothing has to guess the moment at which to interrupt them.
    /// Suppressed while a simulation is open, as accepting one changes the design being simulated.
    let bindOffers =
        match model.CurrentProj with
        | None -> []
        | Some _ when ModelHelpers.simulationIsOpen model -> []
        | Some proj ->
            let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
            let top = ParameterAnalysis.effectiveTopSheetFor ldcs proj.OpenFileName
            ParameterAnalysis.findBindOffers ldcs top (Some proj.OpenFileName)
            |> List.filter (fun offer -> offer.InstanceId = comp.Id)

    /// The offer is a suggestion for an empty box, not a second way to set a value that is already
    /// set: emptying the box is how the user says they have nothing to put there, and that is the
    /// moment a name to follow is worth proposing. It names the property it would follow -
    /// `Bind to main.x` - because that name is what goes in the box.
    let bindButton (key: ParamName) (def: ParamDefinition) =
        let (ParamName nameStr) = key
        let boxIsEmpty =
            model.PopupDialogData.DialogState
            |> Option.defaultValue Map.empty
            |> Map.tryFind (ParameterTypes.paramBoxKey (Some comp.Id) (CustomCompParam nameStr))
            |> function
               | Some state -> state.Text = ""
               | None -> false
        match boxIsEmpty, bindOffers |> List.tryFind (fun offer -> offer.Param = key) with
        | false, _ | _, None -> null
        | true, Some offer ->
            Button.button
                [ Fulma.Button.OnClick(fun _ ->
                    dispatch <| ExecFuncInMessage(applyBindOffers [offer], dispatch)
                    // The box is showing the empty text that raised this offer. Dropping that text
                    // lets it fall back to what the instance is now set to, which the offer has
                    // just made this property's name - so the box reads `width`, with what it
                    // works out to beside it. An ordinary dispatch: the box's text is model state,
                    // and nothing here needs to know it is a DOM element.
                    dispatch <| ClearPopupDialogParamSpec
                        (ParameterTypes.paramBoxKey (Some comp.Id) (CustomCompParam nameStr)))
                  Fulma.Button.Color IsSuccess
                  Fulma.Button.IsLight
                ]
                [str $"Bind to {offer.BindsTo}.{nameStr}"]

    /// One property of this instance: what it is called, what it means, and the box that sets it.
    ///
    /// The same block as the sheet's own properties, so the two read as one idea seen from either
    /// end - the sheet declares them, each instance sets them. The name and the description are
    /// separate lines rather than one bold run, which is why the field is given no label of its
    /// own: the block has already said what this is.
    let entry (key: ParamName) (def: ParamDefinition) =
        let paramName = match key with ParamName s -> s
        div [Key paramName; Class "propertyBlock"] [
            div [Class "propertyHead"] [
                span [Class "propertyName"] [str paramName]
            ]
            if def.Description <> "" then
                div [Class "propertyMeans"] [str def.Description]
            // The sheet's own default is never offered here. What this instance is set to is the
            // only thing that matters on an instance, and showing a value the instance does not
            // have invites the user to reason about one that cannot apply to it.
            paramInputField model ""
                (Option.defaultValue 1I (valueOf key paramName)) (valueOf key paramName)
                (exprOf paramName key) [] (Some comp) (CustomCompParam paramName) dispatch
            div [Class "propertyControls"] [bindButton key def]
        ]

    match Map.isEmpty childDefs with
    // nothing to say rather than a sentence saying there is nothing: a component without
    // parameters should look like one that never had the concept
    | true -> null
    | false ->
        let heading =
            match custom.Form with
            // On a library component these are simply its settings. The sheet they belong to is
            // not one the user works on, so calling them parameters explains nothing and spends a
            // word of vocabulary they do not otherwise need.
            | Some (Library _) -> null
            | _ -> Label.label [] [str "Property values"]
        div [] (heading :: (childDefs |> Map.toList |> List.map (fun (key, def) -> entry key def)))

//------------------------------------------------------------------------------------------------//
//------------------------- Asking for parameters when an instance is placed ---------------------//
//------------------------------------------------------------------------------------------------//

/// The value a parameter of `childLdc` takes when the sheet is viewed on its own.
let private childDefaultValue (childDefs: ParamDefinitions) (name: ParamName) : ParamInt =
    Map.tryFind name childDefs
    |> Option.map (fun def -> ParameterTypes.evaluateParamExpression (bindingsOf childDefs) def.Expression)
    |> Option.bind Result.toOption
    |> Option.defaultValue 1I

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
    // This popup is the FIRST parameter UI a novice meets, because placing a parameterised library
    // component raises it. For a library component the values are simply its settings: the sheet
    // they belong to is not one the user works on, so the parameter names and the vocabulary
    // around them explain nothing, and the description alone is what the author wrote to be read
    // here.
    // (The bind-to-parent toggle needs no separate gate: it appears only where the sheet being
    // placed onto declares a parameter of the same name, which cannot happen in a project that has
    // no parameters of its own.)
    let isLibrary, displayName =
        match childLdc.Form with
        | Some (Library (_, compName)) -> true, compName
        | _ -> false, childLdc.Name
    let parentDefs = model |> get defaultBindingsOfModel_ |> Option.defaultValue Map.empty
    let parentBindings = bindingsOf parentDefs
    let parentSheet = model.CurrentProj |> Option.map (fun p -> p.OpenFileName) |> Option.defaultValue ""

    let slotOf (ParamName nameStr) = CustomCompParam nameStr

    /// The expression the instances of this sheet already on this sheet agree on for a parameter,
    /// where there are some and they do agree.
    ///
    /// Instances of one sheet beside each other usually want the same value: a design that has four
    /// 16-bit registers wants the fifth to be 16 too, and offering the sheet's own default there is
    /// offering the one number the user is least likely to want. The EXPRESSION is carried rather
    /// than its value, so an instance placed beside ones that follow a parameter of the enclosing
    /// sheet follows it too, instead of freezing the number those currently take.
    let siblingBinding (name: ParamName) : ParamExpression option =
        let slots = model |> getCurrentSheet |> getParamSlots
        model
        |> get modelToSymbols
        |> Map.toList
        |> List.choose (fun (_, sym: SymbolT.Symbol) ->
            match sym.Component.Type with
            | Custom cc when cc.Name = childLdc.Name ->
                ParameterAnalysis.instanceBindingExprs slots sym.Component cc |> Map.tryFind name
            | _ -> None)
        |> function
           | [] -> None
           | first :: rest when List.forall ((=) first) rest -> Some first
           | _ -> None

    /// The spec a parameter starts at: what its siblings say, or the child sheet's own default.
    let literalSpec name : NewParamCompSpec =
        let expr =
            siblingBinding name
            |> Option.defaultValue (PInt (childDefaultValue childDefs name))
        let value =
            ParameterTypes.evaluateParamExpression parentBindings expr
            |> Result.toOption
            |> Option.defaultValue (childDefaultValue childDefs name)
        {CompSlot = slotOf name; Expression = expr; Constraints = []; Value = value}

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
    |> List.iter (fun (name, _) ->
        let spec = literalSpec name
        dispatch <| AddPopupDialogParamSpec (
            ParameterTypes.paramBoxKey None (slotOf name),
            {Text = ParameterTypes.renderParamExpression spec.Expression 0; Spec = Ok spec}))

    let specOf (model': Model) name =
        model'.PopupDialogData.DialogState
        |> Option.defaultValue Map.empty
        |> Map.tryFind (ParameterTypes.paramBoxKey None (slotOf name))
        |> Option.map (fun state -> state.Spec)

    /// The offer to make a parameter follow a same-named parameter of an enclosing sheet.
    ///
    /// The same offer the properties pane makes for an instance already placed, computed the same
    /// way: any ancestor under the top declaring the name qualifies, and accepting materialises the
    /// parameter on the sheets in between. This used to ask only whether the immediate parent
    /// declared it, which offered the chain in the one case the user did not need it - they could
    /// type the name - and withheld it in the case they did, since parameter scoping is single
    /// level and a name two sheets up cannot be reached by typing anything.
    let bindOffer (name: ParamName) =
        match model.CurrentProj with
        | None -> None
        | Some proj ->
            let ldcs = (ModelHelpers.getUpdatedLoadedComponents proj model).LoadedComponents
            let top = ParameterAnalysis.effectiveTopSheetFor ldcs proj.OpenFileName
            ParameterAnalysis.bindOfferForPlacement ldcs top proj.OpenFileName name

    let body (model': Model) =
        let renderParam (name: ParamName, definition: ParamDefinition) =
            let (ParamName nameStr) = name
            // The box shows what the value is set to, and the offer appears only once the box is
            // emptied - the same rule as the instance's own properties, so the two behave alike.
            // There is no button back to a number: the box is an ordinary one, and deleting a name
            // to type a number needs no telling.
            let boxIsEmpty =
                model'.PopupDialogData.DialogState
                |> Option.defaultValue Map.empty
                |> Map.tryFind (ParameterTypes.paramBoxKey None (slotOf name))
                |> function
                   | Some state -> state.Text = ""
                   | None -> false
            let valueEntry =
                paramInputField model' (paramPrompt nameStr definition)
                    (childDefaultValue childDefs name) None None [] None (slotOf name) dispatch
            let bindButton =
                match boxIsEmpty, bindOffer name with
                | false, _ | _, None -> null
                | true, Some (bindsTo, _) ->
                    Button.button [
                        Button.Color IsSuccess
                        Button.IsLight
                        Button.OnClick (fun _ ->
                            dispatch <| AddPopupDialogParamSpec (
                                ParameterTypes.paramBoxKey None (slotOf name),
                                {Text = nameStr; Spec = Ok (boundSpec name)}))
                    ] [str $"Bind to {bindsTo}.{nameStr}"]
            div [Key nameStr; Style [MarginBottom "12px"]] [
                valueEntry
                bindButton
            ]
        div [] [
            str $"Property values for this instance of {displayName}. Each is a number, or the name \
                  of a property of an enclosing sheet so that the two stay in step."
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

    let title = if isLibrary then displayName else $"Property values for {childLdc.Name}"
    dialogPopup title body "Place" buttonAction isDisabled [] dispatch


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
            | CustomCompParam paramName -> $"Property {paramName}"
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
            label [Class "label"] [ str "Components on this sheet using them"]
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

    // Nothing where there is nothing to list. A sheet with no parameterised components used to
    // carry a heading and a sentence saying so, which is vocabulary spent on a user who may never
    // have met the feature.
    match sheetParamsSlots with
        | None -> null
        | Some slotMap when Map.isEmpty slotMap -> null
        | Some slotMap -> slotView slotMap

/// UI interface for viewing the parameter expressions of a component
let viewParameters (model: ModelType.Model) dispatch =
    
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [p [] [str $"{comp.Label} has no properties." ]    ]    
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
                                dispatch PropagateParameters)
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
