module ComponentSlots

(*
    ComponentSlots.fs

    The one place that knows how a parameter slot maps onto a field of a ComponentType.

    A CompSlotName names an integer inside a component: a bus width, the value an input takes when
    undriven, one of a SplitN's output widths, a parameter bound by a custom component instance.

    Three copies of this mapping used to exist - one for the properties pane, one for simulation
    elaboration, one for reading port widths back when a custom component is placed - and they had
    drifted apart. An IO slot on a BusSelection was applied to the canvas but not to the
    simulation, so the sheet drawn and the sheet simulated disagreed. One place, so that cannot
    happen again.

    Compiled before the draw block so that symbol code can use it, and before ParameterAnalysis so
    that parameter propagation can resolve a whole canvas through it.
*)

open ParameterTypes
open CommonTypes

/// The slots whose field is an `int` in ComponentType: a width, an index, a bit position. Split
/// from trySetSlotValue only so that the narrowing from ParamInt happens once, before this is
/// reached, rather than in every case.
/// The component with `f` applied to the memory inside it, or None where it holds no memory.
///
/// The four memory components differ only in when they read and whether they can be written, so
/// every slot that names part of a memory applies to all four. The legacy `RAM`/`ROM`/`AsyncROM`
/// cases are deliberately absent: they carry the old Memory type with no Init field, and
/// FilesIO.getLatestComp turns each into its numbered form as a sheet loads, so nothing a slot can
/// reach is ever one of them.
let private withMemory (f: Memory1 -> Memory1) (compType: ComponentType) : ComponentType option =
    match compType with
    | ROM1 mem -> Some (ROM1 (f mem))
    | RAM1 mem -> Some (RAM1 (f mem))
    | AsyncROM1 mem -> Some (AsyncROM1 (f mem))
    | AsyncRAM1 mem -> Some (AsyncRAM1 (f mem))
    | _ -> None

let private trySetIntSlotValue (slot: CompSlotName) (value: int) (compType: ComponentType) : ComponentType option =
    match slot, compType with
    // the component's own width
    | Buswidth, Viewer _ -> Some (Viewer value)
    | Buswidth, BusCompare1 (_, cv, dt) -> Some (BusCompare1 (value, cv, dt))
    | Buswidth, BusCompare (_, cv) -> Some (BusCompare (value, cv))
    | Buswidth, BusSelection (_, lsb) -> Some (BusSelection (value, lsb))
    | Buswidth, Constant1 (_, cv, dt) -> Some (Constant1 (value, cv, dt))
    | Buswidth, Constant (_, cv) -> Some (Constant (value, cv))
    | Buswidth, NbitsAdder _ -> Some (NbitsAdder value)
    | Buswidth, NbitsAdderNoCin _ -> Some (NbitsAdderNoCin value)
    | Buswidth, NbitsAdderNoCout _ -> Some (NbitsAdderNoCout value)
    | Buswidth, NbitsAdderNoCinCout _ -> Some (NbitsAdderNoCinCout value)
    | Buswidth, NbitsXor (_, op) -> Some (NbitsXor (value, op))
    | Buswidth, NbitsAnd _ -> Some (NbitsAnd value)
    | Buswidth, NbitsNot _ -> Some (NbitsNot value)
    | Buswidth, NbitsOr _ -> Some (NbitsOr value)
    | Buswidth, NbitSpreader _ -> Some (NbitSpreader value)
    | Buswidth, SplitWire _ -> Some (SplitWire value)
    | Buswidth, Register _ -> Some (Register value)
    | Buswidth, RegisterE _ -> Some (RegisterE value)
    | Buswidth, Counter _ -> Some (Counter value)
    | Buswidth, CounterNoLoad _ -> Some (CounterNoLoad value)
    | Buswidth, CounterNoEnable _ -> Some (CounterNoEnable value)
    | Buswidth, CounterNoEnableLoad _ -> Some (CounterNoEnableLoad value)
    // the SHIFT input width follows the bus width
    | Buswidth, Shift (_, _, st) -> Some (Shift (value, shifterWidthFor value, st))
    | Buswidth, Input _ -> Some (Input value)
    | Buswidth, Input1 (_, dv) -> Some (Input1 (value, dv))
    | Buswidth, Output _ -> Some (Output value)
    // an IO port's width
    | IO _, Input1 (_, dv) -> Some (Input1 (value, dv))
    | IO _, Output _ -> Some (Output value)
    // the field that shares the IO slot for want of anywhere else. The other one, a BusCompare's
    // comparison value, is a bigint and so is set in trySetSlotValue itself.
    | IO _, BusSelection (w, _) -> Some (BusSelection (w, value))
    // the two widths of a memory. The data a memory already holds is left exactly as it is: a
    // width that no longer fits it is reported when the design is simulated, by
    // MemoryData.dataFitsWidths, rather than by throwing away locations the user typed.
    | MemoryAddressWidth, _ -> withMemory (fun mem -> {mem with AddressWidth = value}) compType
    | MemoryWordWidth, _ -> withMemory (fun mem -> {mem with WordWidth = value}) compType
    // one output of a SplitN
    | SplitNWidth idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length widths ->
        Some (SplitN (n, widths |> List.mapi (fun i w -> if i = idx then value else w), lsbs))
    | SplitNLSB idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length lsbs ->
        Some (SplitN (n, widths, lsbs |> List.mapi (fun i l -> if i = idx then value else l)))
    | _ -> None

/// The type with `value` put into the named slot, or None when the component has no such slot.
///
/// `Buswidth` is a component's own width. `IO` is the width of an Input or an Output - and also
/// the LSB of a BusSelection and the comparison value of a BusCompare, because on those two the
/// width has already taken `Buswidth` and the properties pane needs a second slot for a second
/// field (see SelectedComponentView.makeLsbBitNumberField). That overloading is unfortunate but
/// it is what is stored in existing sheets.
///
/// Some integers of a component are deliberately absent, because a parameter records a value and
/// not a change of shape:
///   - the input count of a GateN or a MergeN sets how many ports the component has, so there is
///     no CompSlotName for it and neither component can be parameterised at all;
///   - the output count of a SplitN is the same thing, so it has no slot - but the width and the
///     bit position of a GIVEN output are ordinary values, and SplitNWidth/SplitNLSB name them.
///     Those two are the only slots that can be out of range, and an index past the end of the
///     lists is no slot rather than a silently ignored one.
///
/// This is the one place a parameter value stops being a bigint. The three slots whose field is
/// already a bigint take it as it is - which is why parameter expressions evaluate in bigint at
/// all - and everything else is a width, an index or a bit position, and is narrowed here. A value
/// too large to be an int is no more applicable than a slot the component does not have, so it
/// comes back as None; it should have been stopped by the slot's own constraint before reaching
/// here, so the warning says that it was not.
let trySetSlotValue (slot: CompSlotName) (value: ParamInt) (compType: ComponentType) : ComponentType option =
    match slot, compType with
    // the value a BusCompare compares against, and the value an input takes when undriven: both
    // bigint, so both hold values no int could
    | IO _, BusCompare (w, _) -> Some (BusCompare (w, value))
    | InputDefault, Input1 (w, _) -> Some (Input1 (w, Some value))
    // a parameter of the sheet inside a custom component, bound by this instance. Applying it
    // here is what carries a parameter down the sheet tree: elaboration descends using the
    // bindings of the component as processed.
    | CustomCompParam paramName, Custom cc ->
        let bindings = cc.ParameterBindings |> Option.defaultValue Map.empty
        Some (Custom { cc with ParameterBindings = Some (Map.add (ParamName paramName) (PInt value) bindings) })
    | _ ->
        match tryIntOfParamInt value with
        | Some intValue -> trySetIntSlotValue slot intValue compType
        | None ->
            Log.warn $"Parameter value {value} is too large for slot {slot}, which holds a whole \
                       number: the component is unchanged"
            None

/// True when the component has the named slot, so that a slot can be rejected where it is written
/// rather than quietly doing nothing where it is applied. The value is irrelevant: whether a slot
/// exists depends on the component's type and, for a SplitN output, on the index.
let slotApplies (slot: CompSlotName) (compType: ComponentType) : bool =
    trySetSlotValue slot 0I compType |> Option.isSome

/// The bounds a value must satisfy to be written into this slot of this component.
///
/// The other half of the mapping trySetSlotValue owns, and here for the same reason: what a slot
/// IS includes what may go in it, and the two kept apart is how they drifted. They were built
/// inline at each properties box instead, which had two consequences:
///
///   - a bound computed from the component's width was frozen at the width the box was showing
///     when the expression was typed. Parameterise the width, change the parameter, and an Input's
///     stored "must fit in 8 bits" outlived the 8;
///   - a value that arrived any other way was bounded by nothing. maxIssieBusWidth was enforced
///     only by these lists, so a width bound to a parent's parameter, or written by the sheet
///     description DSL, had no upper limit at all and the first thing to object was the simulator.
///
/// Derived, so it cannot go stale: every caller asks about the component as it is now. The
/// Constraints stored on a slot are still written as they were, for file compatibility, but are no
/// longer what a value is checked against.
///
/// The messages are the ones the boxes used to build, and are written for whoever typed the value:
/// see ParameterView.evaluateConstraints, which hands the author's text straight to the user.
let constraintsFor (slot: CompSlotName) (compType: ComponentType) : ParamConstraint list =
    let maxWidth = bigint CommonTypes.Constants.maxIssieBusWidth
    /// A width, of whatever it is a width of. The name qualifies which width only where the
    /// component has more than one thing that could be meant.
    let widthConstraints (what: string) = [
        MinVal (PInt 1I, $"{what} must be positive")
        MaxVal (PInt maxWidth, $"{what} cannot exceed {CommonTypes.Constants.maxIssieBusWidth}")
    ]
    /// A value that has to fit in a field of the given width. Computed in bigint, so the bound
    /// holds at every width - as `1 <<< width` did not, wrapping at 32 and leaving a wide input's
    /// default unchecked.
    let fitsIn (what: string) (width: int) = [
        MinVal (PInt 0I, $"{what} must be non-negative")
        MaxVal (PInt ((1I <<< width) - 1I), $"{what} must fit in {width} bits")
    ]
    // a slot the component does not have holds nothing, so there is nothing to bound. Asked first
    // so that the width cases below need not each repeat which components have them.
    match slotApplies slot compType with
    | false -> []
    | true ->
        match slot, compType with
        // the second integer of a BusSelection or a BusCompare, which shares the IO slot because
        // Buswidth is already the component's own width
        | IO _, BusSelection _ -> [MinVal (PInt 0I, "LSB position must be non-negative")]
        | IO _, BusCompare (width, _) -> fitsIn "Comparison value" width
        | InputDefault, Input1 (width, _) -> fitsIn "Default value" width
        // a parameter of the sheet inside a custom component instance. Its bounds are the child
        // sheet's, not this one's: the value has to satisfy whatever the slots it feeds require,
        // and those are expressions in the child's parameters, which cannot be evaluated here.
        // ParameterView.instanceBindingProblem checks them by resolving the child sheet.
        | CustomCompParam _, _ -> []
        // a memory's two widths. Both are bus widths - the address port and the data port are
        // buses like any other - so both take the ordinary width bounds, named so that a message
        // says which of the two was too large.
        | MemoryAddressWidth, _ -> widthConstraints "Address width"
        | MemoryWordWidth, _ -> widthConstraints "Word width"
        | SplitNWidth _, _ -> widthConstraints "Width"
        | SplitNLSB _, _ ->
            [ MinVal (PInt 0I, "LSB must be non-negative")
              MaxVal (PInt maxWidth, $"LSB cannot exceed {CommonTypes.Constants.maxIssieBusWidth}") ]
        // every other slot the component can have is a width. The name qualifies which width only
        // on the two components that have more than one thing a width could mean.
        | _, SplitWire _ -> widthConstraints "Top (LSB) output width"
        | _, NbitSpreader _ -> widthConstraints "Output width"
        | _ -> widthConstraints "Width"

/// Apply `value` to the named slot, leaving the type alone where the component has no such slot.
/// Callers that can report a bad slot should use trySetSlotValue; this is for the paths that
/// cannot, where a slot recorded in an old file must not stop a sheet loading or simulating.
let setSlotValue (slot: CompSlotName) (value: ParamInt) (compType: ComponentType) : ComponentType =
    trySetSlotValue slot value compType |> Option.defaultValue compType

/// A sheet's canvas with every parameterised slot resolved at the given bindings.
/// A slot whose expression cannot be evaluated leaves its component alone, so the widths that are
/// known still come out rather than the whole sheet failing.
///
/// Here rather than in CanvasExtractor because two callers need it from opposite ends of the
/// compile order: the instance-signature code in CanvasExtractor, and the parameter propagation in
/// ParameterAnalysis that brings every sheet into line with what its design sets.
let resolveCanvasAtBindings
        (bindings: ParamBindings)
        (slots: ComponentSlotExpr)
        ((comps, conns): CanvasState)
        : CanvasState =
    let slotsOf compId =
        slots |> Map.toList |> List.filter (fun (slot, _) -> slot.CompId = compId)
    let resolve (comp: Component) =
        (comp.Type, slotsOf comp.Id)
        ||> List.fold (fun compType (slot, exprSpec) ->
            match evaluateParamExpression bindings exprSpec.Expression with
            | Ok value -> setSlotValue slot.CompSlot value compType
            | Error _ -> compType)
        |> fun compType -> {comp with Type = compType}
    List.map resolve comps, conns
