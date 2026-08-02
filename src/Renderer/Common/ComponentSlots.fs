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

    Compiled before the draw block so that symbol code can use it: SymbolUpdate needs it to put a
    symbol back to its declared values when a sheet is saved.
*)

open ParameterTypes
open CommonTypes

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
let trySetSlotValue (slot: CompSlotName) (value: int) (compType: ComponentType) : ComponentType option =
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
    // the two fields that share the IO slot for want of anywhere else
    | IO _, BusSelection (w, _) -> Some (BusSelection (w, value))
    | IO _, BusCompare (w, _) -> Some (BusCompare (w, bigint value))
    // the value an input takes when undriven
    | InputDefault, Input1 (w, _) -> Some (Input1 (w, Some (bigint value)))
    // one output of a SplitN
    | SplitNWidth idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length widths ->
        Some (SplitN (n, widths |> List.mapi (fun i w -> if i = idx then value else w), lsbs))
    | SplitNLSB idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length lsbs ->
        Some (SplitN (n, widths, lsbs |> List.mapi (fun i l -> if i = idx then value else l)))
    // a parameter of the sheet inside a custom component, bound by this instance. Applying it
    // here is what carries a parameter down the sheet tree: elaboration descends using the
    // bindings of the component as processed.
    | CustomCompParam paramName, Custom cc ->
        let bindings = cc.ParameterBindings |> Option.defaultValue Map.empty
        Some (Custom { cc with ParameterBindings = Some (Map.add (ParamName paramName) (PInt value) bindings) })
    | _ -> None

/// True when the component has the named slot, so that a slot can be rejected where it is written
/// rather than quietly doing nothing where it is applied. The value is irrelevant: whether a slot
/// exists depends on the component's type and, for a SplitN output, on the index.
let slotApplies (slot: CompSlotName) (compType: ComponentType) : bool =
    trySetSlotValue slot 0 compType |> Option.isSome

/// Apply `value` to the named slot, leaving the type alone where the component has no such slot.
/// Callers that can report a bad slot should use trySetSlotValue; this is for the paths that
/// cannot, where a slot recorded in an old file must not stop a sheet loading or simulating.
let setSlotValue (slot: CompSlotName) (value: int) (compType: ComponentType) : ComponentType =
    trySetSlotValue slot value compType |> Option.defaultValue compType

/// Apply every slot value in the map. Used to put a symbol back to its declared values, and to
/// resolve a component for elaboration.
let setSlotValues (values: Map<CompSlotName, int>) (compType: ComponentType) : ComponentType =
    (compType, values) ||> Map.fold (fun compType slot value -> setSlotValue slot value compType)
