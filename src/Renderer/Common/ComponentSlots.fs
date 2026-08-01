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

/// Apply `value` to the named slot of `compType`, returning the type unchanged where the slot
/// does not apply to that kind of component.
///
/// `Buswidth` is a component's own width. `IO` is the width of an Input or an Output - and also
/// the LSB of a BusSelection and the comparison value of a BusCompare, because on those two the
/// width has already taken `Buswidth` and the properties pane needs a second slot for a second
/// field (see SelectedComponentView.makeLsbBitNumberField). That overloading is unfortunate but
/// it is what is stored in existing sheets.
let setSlotValue (slot: CompSlotName) (value: int) (compType: ComponentType) : ComponentType =
    match slot, compType with
    // the component's own width
    | Buswidth, Viewer _ -> Viewer value
    | Buswidth, BusCompare1 (_, cv, dt) -> BusCompare1 (value, cv, dt)
    | Buswidth, BusCompare (_, cv) -> BusCompare (value, cv)
    | Buswidth, BusSelection (_, lsb) -> BusSelection (value, lsb)
    | Buswidth, Constant1 (_, cv, dt) -> Constant1 (value, cv, dt)
    | Buswidth, Constant (_, cv) -> Constant (value, cv)
    | Buswidth, NbitsAdder _ -> NbitsAdder value
    | Buswidth, NbitsAdderNoCin _ -> NbitsAdderNoCin value
    | Buswidth, NbitsAdderNoCout _ -> NbitsAdderNoCout value
    | Buswidth, NbitsAdderNoCinCout _ -> NbitsAdderNoCinCout value
    | Buswidth, NbitsXor (_, op) -> NbitsXor (value, op)
    | Buswidth, NbitsAnd _ -> NbitsAnd value
    | Buswidth, NbitsNot _ -> NbitsNot value
    | Buswidth, NbitsOr _ -> NbitsOr value
    | Buswidth, NbitSpreader _ -> NbitSpreader value
    | Buswidth, SplitWire _ -> SplitWire value
    | Buswidth, Register _ -> Register value
    | Buswidth, RegisterE _ -> RegisterE value
    | Buswidth, Counter _ -> Counter value
    | Buswidth, CounterNoLoad _ -> CounterNoLoad value
    | Buswidth, CounterNoEnable _ -> CounterNoEnable value
    | Buswidth, CounterNoEnableLoad _ -> CounterNoEnableLoad value
    // the SHIFT input width follows the bus width
    | Buswidth, Shift (_, _, st) -> Shift (value, shifterWidthFor value, st)
    | Buswidth, Input _ -> Input value
    | Buswidth, Input1 (_, dv) -> Input1 (value, dv)
    | Buswidth, Output _ -> Output value
    // an IO port's width
    | IO _, Input1 (_, dv) -> Input1 (value, dv)
    | IO _, Output _ -> Output value
    // the two fields that share the IO slot for want of anywhere else
    | IO _, BusSelection (w, _) -> BusSelection (w, value)
    | IO _, BusCompare (w, _) -> BusCompare (w, bigint value)
    // the value an input takes when undriven
    | InputDefault, Input1 (w, _) -> Input1 (w, Some (bigint value))
    // one output of a SplitN
    | SplitNWidth idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length widths ->
        SplitN (n, widths |> List.mapi (fun i w -> if i = idx then value else w), lsbs)
    | SplitNLSB idx, SplitN (n, widths, lsbs) when idx >= 0 && idx < List.length lsbs ->
        SplitN (n, widths, lsbs |> List.mapi (fun i l -> if i = idx then value else l))
    // a parameter of the sheet inside a custom component, bound by this instance. Applying it
    // here is what carries a parameter down the sheet tree: elaboration descends using the
    // bindings of the component as processed.
    | CustomCompParam paramName, Custom cc ->
        let bindings = cc.ParameterBindings |> Option.defaultValue Map.empty
        Custom { cc with ParameterBindings = Some (Map.add (ParamName paramName) (PInt value) bindings) }
    | _ -> compType

/// Apply every slot value in the map. Used to put a symbol back to its declared values, and to
/// resolve a component for elaboration.
let setSlotValues (values: Map<CompSlotName, int>) (compType: ComponentType) : ComponentType =
    (compType, values) ||> Map.fold (fun compType slot value -> setSlotValue slot value compType)
