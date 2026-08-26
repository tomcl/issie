/// The name a waveform is called, worked out from the component and port it is a wave of.
///
/// Below the UI because both simulators produce it. A name carries the port's BIT WIDTH, and a
/// width is a fact about the elaborated instance rather than about the sheet it instantiates -
/// parameters see to that - so nothing above can work one out from a design. The alternative is
/// the sidecar reimplementing which port of a Mux is called SEL and how an adder's carry in is
/// written, and two answers that agree until they do not.
///
/// Moved out of `WaveSimHelpers`, which is where it was and which now opens it. What stayed there
/// is everything that needs a WaveSimModel: `makeWave` builds a Wave, and a Wave is a thing the
/// selector holds.
module WaveNames

open EEExtensions
open CommonTypes
open SimGraphTypes
open SimTypes

/// get integer from OutputPortNumber
let getInputPortNumber (ipn: InputPortNumber) : int =
    match ipn with
    | InputPortNumber pn -> pn

/// get integer from OutputPortNumber
let getOutputPortNumber (opn: OutputPortNumber) : int =
    match opn with
    | OutputPortNumber pn -> pn
/// convert a string to CamelCase: 
let camelCaseDottedWords (text:string) =
    let camelWord (s:string)=
        match s.Length with
        | 0 -> ""
        | 1 -> s.ToUpper()
        | _ -> s[0..0].ToUpper() + s[1..s.Length-1].ToLower()

    text.Split([|'.'|])
    |> Array.map camelWord
    |> String.concat "."
    /// get string in the [x:x] format given the bit limits

/// output representation of bus width
let bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "(%d)" msb
    | (msb, lsb) -> sprintf "(%d:%d)" msb lsb

/// return sheet with all latters capitalised
let cap (sheet:string) = sheet.ToUpper()


/// Get port names for waves that are from Input ports.
/// Appended to comp.Label
let getInputPortName (compType: ComponentType) (port: InputPortNumber) : string =
    let muxPortName (size: int) : string =
        if port = (InputPortNumber size) then ".SEL"
        else "." + string port

    match compType with
    | Not | BusCompare _ | BusCompare1 _ ->
        ".IN"
    | GateN _ | NbitsNot _ | NbitSpreader _ ->
        ".IN" + string port

    | Mux2 ->
        muxPortName 2
    | Mux4 ->
        muxPortName 4
    | Mux8 ->
        muxPortName 8

    | Decode4 ->
        match port with
        | InputPortNumber 0 -> ".SEL"
        | _ -> ".DATA"

    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ | CounterNoEnableLoad _ | NotConnected ->
        ""
    | DFF | Register _ ->
        ".D"

    | ROM1 _ | AsyncROM1 _ ->
        ".ADDR"

    | Demux2 | Demux4 | Demux8 ->
        match port with
        | InputPortNumber 0 -> ".DATA"
        | _ -> ".SEL"

    | NbitsXor _ | NbitsAnd _ |NbitsOr _ ->
        match port with
        | InputPortNumber 0 -> ".P"
        | _ -> ".Q"

    | NbitsAdder _ |NbitsAdderNoCout _ ->
        match port with
        | InputPortNumber 0 -> ".CIN"
        | InputPortNumber 1 -> ".P"
        | _ -> ".Q"

    | NbitsAdderNoCin _ |NbitsAdderNoCinCout _ ->
        match port with
        | InputPortNumber 0 -> ".P"
        | _ -> ".Q"

    | Shift _ ->
        match port with
        |InputPortNumber 0 -> ".IN"
        |_ -> ".Shifter"
    
    | DFFE | RegisterE _ ->
        match port with
        | InputPortNumber 0 -> ".D"
        | _ -> ".EN"

    | Counter _ ->
        match port with
        | InputPortNumber 0 -> ".D"
        | InputPortNumber 1 -> ".LOAD"
        | _ -> ".EN"

    | CounterNoEnable _ ->
        match port with
        | InputPortNumber 0 -> ".D"
        | _ -> ".LOAD"

    | CounterNoLoad _ -> ".EN"
        
    | RAM1 _ | AsyncRAM1 _ ->
        match port with
        | InputPortNumber 0 -> ".ADDR"
        | InputPortNumber 1 -> ".DIN"
        | _ -> ".WEN"

    | Custom c ->
        "." + fst c.InputLabels[getInputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | IOLabel -> failwithf "IOLabel should not occur in getInputPortName"
    | MergeWires -> failwithf "MergeWires should not occur in getInputPortName"
    | MergeN _ -> failwithf "MergeN should not occur in getInputPortName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getInputPortName"
    | SplitN _ -> failwithf "SplitN should not occur in getInputPortName"
    | BusSelection _ -> failwithf "BusSelection should not occur in getInputPortName"

/// Get port names for waves that are from Output ports
/// Appended to comp.Label
let getOutputPortName (compType: ComponentType) (port: OutputPortNumber) : string =
    match compType with
    | Not | GateN _ | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ | BusCompare1 _ | NbitsXor _ | NbitsNot _  | NbitSpreader _ | NbitsAnd _ | NbitsOr _ |Shift _->
        ".OUT"
    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ | IOLabel | NotConnected ->
        ""
    | Demux2 | Demux4 | Demux8 ->
        "." + string port
    | NbitsAdder _ |NbitsAdderNoCin _ ->
        match port with
        | OutputPortNumber 0 ->
            ".SUM"
        | _ ->
            ".COUT"
    | NbitsAdderNoCout _ |NbitsAdderNoCinCout _ ->
        ".SUM"
        
    | DFF | DFFE | Register _ | RegisterE _ |Counter _ |CounterNoEnable _ |CounterNoLoad _ |CounterNoEnableLoad _ ->
        ".Q"
    | RAM1 _ | AsyncRAM1 _ | AsyncROM1 _ | ROM1 _ ->
        ".DOUT"
    | Custom c ->
        "." + fst c.OutputLabels[getOutputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
    | MergeN _ -> failwithf "MergeN should not occur in getOutputName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
    | SplitN _ -> failwithf "SplitN should not occur in getOutputName"
    | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

// ------------------------------------------------------------------------------------------------
// The same names, from the design plus the port slice - no FastComponent.
//
// The fc-based functions above read widths out of the ELABORATED component's type, which is a
// fact about a build this process need not have made. These take the design's ComponentType -
// which under parameters carries the sheet's DEFAULT widths, good only for structure: which port
// is called SEL, which are one bit whatever the component's width - and the real widths as the
// port slice reports them, which are the instance's own.
//
// Each quirk of the fc-based names is reproduced deliberately, because a selection is keyed by
// what the selector showed: a spreader's one-bit input is named at the SPREAD width, a shifter's
// shift-amount input at the DATA width, a comparator's one-bit output at the COMPARED width, and
// a mux's data ports with no width at all. The equivalence of the two derivations over whole
// designs, parameterised ones included, is pinned by test.
// ------------------------------------------------------------------------------------------------

/// The name of one input port, from design facts plus the instance's port widths.
let getInputNameW
    (withComp: bool)
    (compType: ComponentType)
    (label: string)
    (insWidths: int array)
    (outsWidths: int array)
    (port: InputPortNumber)
    : string =
    let pn = getInputPortNumber port
    let portName = getInputPortName compType port

    let widthAt (widths: int array) i =
        if i >= 0 && i < widths.Length then widths[i] else 0

    let bitLims =
        match compType with
        // the enable and load inputs are one bit whatever the width of the register or counter
        | RegisterE _ | Counter _ | CounterNoEnable _ | CounterNoLoad _
                when portName = ".EN" || portName = ".LOAD" ->
            bitLimsString (0, 0)
        // an adder's carry in is one bit; it is the operands that are as wide as the adder
        | NbitsAdder _ | NbitsAdderNoCout _ when portName = ".CIN" ->
            bitLimsString (0, 0)
        // the spreader's input is one bit but has always been named at the width it spreads TO
        | NbitSpreader _ -> bitLimsString (widthAt outsWidths 0 - 1, 0)
        // both of a shifter's inputs are named at the data width, the shift amount included
        | Shift _ -> bitLimsString (widthAt insWidths 0 - 1, 0)
        | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _
        | NbitsXor _ | NbitsNot _ | NbitsAnd _ | NbitsAdder _ | NbitsOr _
        | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _
        | BusCompare _ | BusCompare1 _ | Register _ | RegisterE _
        | Counter _ | CounterNoEnable _
        | Custom _ ->
            bitLimsString (widthAt insWidths pn - 1, 0)
        | Not | GateN _
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | DFFE | CounterNoLoad _ | CounterNoEnableLoad _ ->
            bitLimsString (0, 0)
        | ROM1 _ | AsyncROM1 _ | RAM1 _ | AsyncRAM1 _ ->
            ""
        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | NotConnected -> failwithf "NotConnected should not occur in getInputNameW"
        | IOLabel -> failwithf "IOLabel should not occur in getInputNameW"
        | MergeWires -> failwithf "MergeWires should not occur in getInputNameW"
        | MergeN _ -> failwithf "MergeN should not occur in getInputNameW"
        | SplitWire _ -> failwithf "SplitWire should not occur in getInputNameW"
        | SplitN _ -> failwithf "SplitN should not occur in getInputNameW"
        | BusSelection _ -> failwithf "BusSelection should not occur in getInputNameW"

    if withComp then
        label + portName + bitLims
    else
        portName[1 .. portName.Length - 1] + bitLims

/// The name of one output port, from design facts plus the instance's port widths. The IOLabel
/// case is the one the fc-based version needed a whole simulation for - which member of the group
/// drives the net - and needs nothing here: the slice width of an IOLabel's output IS the elected
/// driver's, because the group shares its array.
let getOutputNameW
    (withComp: bool)
    (compType: ComponentType)
    (label: string)
    (insWidths: int array)
    (outsWidths: int array)
    (port: OutputPortNumber)
    : string =
    let pn = getOutputPortNumber port
    let portName = getOutputPortName compType port

    let widthAt (widths: int array) i =
        if i >= 0 && i < widths.Length then widths[i] else 0

    let bitLims =
        match compType with
        // a comparator's output is one bit but has always been named at the width it compares
        | BusCompare _ | BusCompare1 _ -> bitLimsString (widthAt insWidths 0 - 1, 0)
        // as with the carry in, the carry out is one bit whatever the width of the adder
        | NbitsAdder _ | NbitsAdderNoCin _ when portName = ".COUT" -> bitLimsString (0, 0)
        | Not | GateN _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)
        | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _
        | NbitsXor _ | NbitsAnd _ | NbitsOr _ | NbitsNot _ | NbitSpreader _ | NbitsAdder _
        | Register _ | RegisterE _
        | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _
        | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _
        | Shift _
        | RAM1 _ | AsyncRAM1 _ | AsyncROM1 _ | ROM1 _
        | Custom _ ->
            bitLimsString (widthAt outsWidths pn - 1, 0)
        | IOLabel ->
            match widthAt outsWidths 0 with
            | 0 -> failwithf $"What? Can't find width for IOLabel {label}$ "
            | width -> bitLimsString (width - 1, 0)
        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | NotConnected -> failwithf "NotConnected should not occur in getOutputNameW"
        | MergeWires -> failwithf "MergeWires should not occur in getOutputNameW"
        | MergeN _ -> failwithf "MergeN should not occur in getOutputNameW"
        | SplitWire _ -> failwithf "SplitWire should not occur in getOutputNameW"
        | SplitN _ -> failwithf "SplitN should not occur in getOutputNameW"
        | BusSelection _ -> failwithf "BusSelection should not occur in getOutputNameW"

    if withComp then
        label + portName + bitLims
    else
        portName[1 .. portName.Length - 1] + bitLims

let caseCompAndPortName (name:string) =
    let parts = name.Split([|'.'|])
    match parts.Length with
    | 0 | 1 -> name.ToUpper()
    | n -> (String.concat "." parts[0..n-2]).ToUpper() + "." + camelCaseDottedWords parts[n-1]
