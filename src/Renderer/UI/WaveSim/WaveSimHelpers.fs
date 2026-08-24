///Miscellaneous helpers used tby waveform simulator
module WaveSimHelpers
//---------------------------------------------------------------------------------------//
//-----------------------Miscellaneous low=level helper functions------------------------//
//---------------------------------------------------------------------------------------//


open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open EEExtensions
open CommonTypes
open ModelType
open ModelHelpers
open SimGraphTypes
open SimTypes
open WaveSimTypes
open WaveSimStyle


//-----------------------------List & Map utilities to deal with exceptions---------------//

/// Helper function to create Bulma buttons
let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]

/// convenience functions
let isWaveSelected (index: WaveIndexT) (wsModel: WaveSimModel) : bool = List.contains index wsModel.SelectedWaves
let isRamSelected (ramId: FComponentId) (wsModel: WaveSimModel) : bool = Map.containsKey ramId wsModel.SelectedRams

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

let portBits n = if n < 2 then "" else $"({n-1}:0)"

/// Which group (for selector classification) is a component of this type in?
///
/// A function of the component TYPE alone, which is all it ever needed: it took a whole
/// FastSimulation only to look the type up. Callers pass the type, so that when a port carries
/// its own type there is nothing here left to change - and so that this says nothing about where
/// the simulation ran.
let getCompGroup (compType: ComponentType) =
    match compType with
    | Input1 _ | Output _ | Constant1 _ | Viewer _ | IOLabel | NotConnected ->
        InputOutput
    | Not | GateN _ ->
        Gates
    | BusCompare _ | BusCompare1 _->
        Buses
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4 ->
        MuxDemux
    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitSpreader _ | NbitsOr _ ->
        Arithmetic
    | Custom _ -> CustomComp
    | DFF | DFFE | Register _ | RegisterE _ |Counter _ |CounterNoEnable _ |CounterNoLoad _ |CounterNoEnableLoad _ ->
        FFRegister
    | AsyncROM1 _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ ->
        Memories
    | BusSelection _ | MergeWires | MergeN _ | SplitWire _ | SplitN _ ->
        failwithf "Bus select, MergeWires, MergeN, SplitWire should not appear"
    | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
        failwithf "Legacy component types should not appear"
    | Shift _ ->
        failwithf "Shift is an internal-only component which should never appear on the canvas"

/// The heading over a group of components within one sheet of the wave selector.
///
/// Every case of ComponentGroup, so that adding one is a compile error rather than a row headed
/// "What? Not used!" - which is what the wildcard this replaces would have given. Three of them,
/// WireLabel, Viewers and Component, are not produced by getCompGroup: they are there for grouping
/// components by type rather than by name, which the selector does not currently do.
let groupHeading (compGroup: ComponentGroup): ReactElement =
    let name =
        match compGroup with
        | InputOutput -> "Inputs / Outputs / Labels / Viewers"
        | Viewers -> "Viewers"
        | WireLabel -> "Wire Labels"
        | Buses -> "Buses"
        | Gates -> "Logic Gates"
        | MuxDemux -> "Multiplexers"
        | Arithmetic -> "Arithmetic"
        | FFRegister -> "Flip Flops and Registers"
        | Memories -> "RAMs and ROMs"
        | Component compLabel -> compLabel
        | CustomComp -> "Custom Components"
    str (name.ToUpper())

/// Convert Wave list to list of WaveIndexT
let wavesToIds (waves: Wave list) =
    waves |> List.map (fun wave -> wave.WaveId)

/// The name of the sheet a wave's component sits on, which is how ComponentsById and
/// ConnectionsByPort are both keyed.
/// A component's access path names the custom component instances it sits inside, and the innermost
/// of those is an instance of the sheet wanted. It is that instance's TYPE which names the sheet:
/// its LABEL is chosen by whoever placed it, and only happens to be the sheet's name until someone
/// renames the instance. Asking the label instead left every wave in a renamed subsheet - REGFILE
/// for reg16x8, say - with no connections found, and so with no wires highlighted on hover.
let sheetOfWave (fs: FastSimulation) (wave: Wave) : string option =
    match snd wave.WaveId.Id with
    | [] ->
        Some fs.SimulatedTopSheet
    | path ->
        Map.tryFind (path[path.Length - 1], path[0 .. path.Length - 2]) fs.FCustomComps
        |> Option.bind (fun instance ->
            match instance.FType with
            | Custom cc -> Some cc.Name
            | _ -> None)

/// Work out a SheetPort from a wave, if one exists
/// SheetPorts may not exist in some corner cases when simulation is ending etc.
///
/// Every step is best effort and gives nothing rather than throwing. The link between a wave and
/// the schematic is only as good as the schematic still being the one that was simulated, and this
/// runs from the mouse handlers on a waveform's name - where an exception is not caught by
/// showWaveforms' error boundary, which has already returned by then.
let waveToSheetPort fs (wave:Wave) =
    let wi = wave.WaveId
    sheetOfWave fs wave
    |> Option.bind (fun sheet ->
        fs.ComponentsById
        |> Map.tryFind sheet
        |> Option.bind (Map.tryFind (fst wi.Id))
        |> Option.map (fun comp ->
            // Which set of ports the wave's port number indexes. A component with neither - which
            // is what the last case is - has nothing to connect and so nothing to highlight.
            let ports =
                match wi.PortType, comp.InputPorts.Length > 0, comp.OutputPorts.Length > 0 with
                | PortType.Input, true, _ | PortType.Output, true, false -> comp.InputPorts
                | PortType.Output ,_, true | PortType.Input, false, true -> comp.OutputPorts
                | _ -> []
            List.tryItem wi.PortNumber ports
            |> Option.map (fun port -> [{ Sheet = sheet; PortOnComp = port }])
            |> Option.defaultValue []))
    |> Option.defaultValue []


/// given a SheetPort, get all directly connected SheetPorts
let connectedPorts (fs: FastSimulation) sheetPort =
    let compMap = fs.ComponentsById
    let portMap = fs.ConnectionsByPort
    let name = sheetPort.Sheet
    Map.tryFind sheetPort portMap
    |> Option.defaultValue []
    |> List.collect (fun conn -> 
        [conn.Source; conn.Target]
        |> List.map (Simulator.portSheetPort compMap[name] name)
        |> List.collect (function | None -> [] | Some sheetPort -> [sheetPort]))

/// given an IOlabel port, get all same-name IOLabels on the same sheet
let connectedIOs (fs: FastSimulation) (sp: SheetPort) =
    let comps = fs.ComponentsById[sp.Sheet]
    match comps[ComponentId sp.PortOnComp.HostId] with
    | {Type = IOLabel} as comp -> 
        let sheet = sp.Sheet
        comps
        |> Map.valuesL
        |> List.collect (
            function | {Type=IOLabel; Label = label} as comp1 when label = comp.Label -> 
                        (
                            (if comp1.OutputPorts.Length > 0 then [{Sheet = sheet; PortOnComp = comp1.OutputPorts[0]}] else [])@
                            (if comp1.InputPorts.Length > 0 then [{Sheet = sheet; PortOnComp = comp1.InputPorts[0]}] else [])@
                            [sp]
                        )
                     | _ -> 
                        [])

    | _ -> [sp]

/// Given a list of ports, get all ports connected to any port in it.
/// used by connsOfWave
let rec allConnectedPorts (fs: FastSimulation) (sp:SheetPort list) =
    let newSP =
        sp
        |> List.collect (connectedIOs fs)
        |> List.distinct
        |> List.collect (connectedPorts fs)
        |> List.distinct
    match newSP.Length - sp.Length with
    | 0 ->
        newSP
    | n when n >= 0 -> 
        allConnectedPorts fs newSP
    | _ -> 
        newSP

/// Get all the connections of a given wave signal
let connsOfWave (fs:FastSimulation) (wave:Wave) =
    wave
    |> waveToSheetPort fs
    |> allConnectedPorts fs
    |> List.collect (fun sp -> match Map.tryFind sp fs.ConnectionsByPort with | None -> [] | Some conns -> conns)
    |> List.map (fun conn -> ConnectionId conn.Id)
    |> List.distinct


/// button driving a popup with a page of info about waveform simulator
let waveInfoButton (name:string) (dispatch: Msg -> Unit) : ReactElement =
    button 
        (topHalfButtonProps IsInfo "RefreshButton" false)
        (fun _ -> (UIPopups.viewWaveInfoPopup dispatch name))
        (str name)

/// remove highlights on components generated by hovering on waveform labels
let removeHighlights (model:Model) dispatch =
    if model.Sheet.SelectedWires.Length > 0 || model.Sheet.SelectedComponents.Length > 0 then
        dispatch <| Sheet (DrawModelType.SheetT.ResetSelection) // Remove highlights.

type WaveSimButtonOptions = {
    IsDirty: bool
    IsRunning: bool
    IsErrored: bool
    StartEndMsg: string
    StartEndColor: IColor
    }

/// end the current simulation
let endButtonAction canvasState model dispatch ev =
    removeHighlights model dispatch
    dispatch <| EndWaveSim

/// Return info about current state of waveform simulator
/// which is used to switch buttons on/off etc.
///
/// Runs on every render of the wave sim panel, so nothing here works out anything about the design
/// that the model has already been told.
let getWaveSimButtonOptions (canv: CanvasState) (model:Model) (ws:WaveSimModel) dispatch : WaveSimButtonOptions =
    let fs = Simulator.getFastSim()
    let simExists = model.WaveSimSheet <> Some "" && model.WaveSimSheet <> None
    let success = (ws.State = Success || ws.State=Loading)

    // Whether the design builds is read from the model rather than worked out here. It used to run
    // the whole of validateCircuitSimulation - every sheet of the design checked and its widths
    // inferred - once per render, to choose a word and a colour. See ModelType.CircuitCheck, and
    // StepSimulationTop, which reads the same verdict for the same reason.
    //
    // The verdict is about the sheet that is OPEN. That is the sheet this is about whenever the
    // answer changes anything: a waveform simulation running on some other sheet is running, and
    // says "End Simulation", whether the design still builds or not.
    if circuitCheckIsNeeded model canv then
        dispatch RequestCircuitCheck
    /// A design not yet checked reads as buildable, as in the step simulator: pressing the button
    /// does the real build and reports any error then, so an optimistic colour costs a click.
    let hasSimErr =
        match model.CircuitCheck.Verdict with
        | Some (Error _, _) -> true
        | _ -> false

    let errored =
        match hasSimErr, ws.State with
        | true, _ -> true
        | false, NonSequential -> true
        | false, _ -> false

    let running = (success || errored) && simExists
        
    let isDirty = 
        simExists &&
        running && 
        not <| FastExtract.compareLoadedStates fs canv model.CurrentProj &&
        model.UIState = None &&
        not model.IsLoading
    
    
    let startEndMsg, startEndColor =
        match running, errored with
        | false, true -> "View Problems", IsWarning
        | false, false -> "Start Simulation" + ModelHelpers.simulatorLabel model, IsSuccess
        | true, _ -> "EndSimulation" + ModelHelpers.simulatorLabel model, IsDanger


    
    {
        IsDirty =  isDirty
        IsRunning = running
        IsErrored = errored
        StartEndMsg = startEndMsg
        StartEndColor = startEndColor
    } 



    

// ---------------------------------------------------------------------------------------------
// What a waveform is called, and the Wave record that carries it
// ---------------------------------------------------------------------------------------------
//
// This was the top third of WaveSimSelect, which compiles after the selection dialog that needs
// it: the dialog now describes the waves of the instances it is drawing rather than looking them
// up in a map of every wave in the simulation, so it has to be able to make a Wave. Nothing here
// is about selection - it is naming, which is what this module is for, and the pieces it is built
// from (camelCaseDottedWords, bitLimsString, portBits) are just above.

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

/// Get names for waves that are from Input ports
/// TODO: unify this with DrawBlock and widthInferror logic

let getInputName (withComp: bool) (comp: FastComponent) (port: InputPortNumber) : string =
    let portName : string = getInputPortName comp.FType port
    let bitLims : string =
        match comp.FType with
        // The enable and load inputs are one bit whatever the width of the register or counter
        // they control - only the data input carries that width. getInputPortName has already put
        // the leading '.' on, which is why the names compared against carry one too.
        | RegisterE _ | Counter _ | CounterNoEnable _ | CounterNoLoad _
                when portName = ".EN" || portName = ".LOAD" ->
            bitLimsString (0, 0)
        // An adder's carry in is one bit; it is the two operands that are as wide as the adder.
        | NbitsAdder _ | NbitsAdderNoCout _ when portName = ".CIN" ->
            bitLimsString (0, 0)
        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor(w, _) | NbitsNot w | NbitsAnd w | NbitsAdder w | NbitsOr w
        | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
        | BusCompare(w,_) | BusCompare1(w,_,_)  |Register w | RegisterE w
        | Counter w | CounterNoEnable w | NbitSpreader w ->
            bitLimsString (w - 1, 0)
        | Not | BusCompare _ | BusCompare1 _ | GateN _
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | Register _ | DFFE | RegisterE _ |Counter _
        |CounterNoEnable _ |CounterNoLoad _ |CounterNoEnableLoad _ ->
            bitLimsString (0, 0)

        | Shift(w,m,tp) -> bitLimsString (w - 1, 0)
        // TODO: Find the right parameters for RAMs and ROMs.
        | ROM1 _ | AsyncROM1 _ | RAM1 _ | AsyncRAM1 _ ->
            ""

        | Custom c ->
            bitLimsString (snd c.InputLabels[getInputPortNumber port] - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | NotConnected -> failwithf "NotConnected should not occur in getInputName"
        | IOLabel -> failwithf "IOLabel should not occur in getInputName"
        | MergeWires -> failwithf "MergeWires should not occur in getInputName"
        | MergeN _ -> failwithf "MergeN should not occur in getInputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getInputName"
        | SplitN _ -> failwithf "SplitN should not occur in getInputName"
        | BusSelection _ -> failwithf "BusSeleciton should not occur in getInputName"

    if withComp then 
        comp.FLabel + portName + bitLims
    else 
        portName[1..portName.Length-1] + bitLims

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

/// Get names for waves that are from Output ports
/// TODO: unify this with DrawBlock and widthInferror logic
let getOutputName (withComp: bool) (comp: FastComponent) (port: OutputPortNumber) (fastSim: FastSimulation): string =
    let portName = getOutputPortName comp.FType port
    let bitLims =
        match comp.FType with
        | BusCompare(w,_) | BusCompare1(w,_,_) -> bitLimsString (w-1, 0)
        // As with the carry in, the carry out is one bit whatever the width of the adder.
        | NbitsAdder _ | NbitsAdderNoCin _ when portName = ".COUT" -> bitLimsString (0, 0)
        | Not | GateN _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor(w,_) | NbitsAnd w | NbitsOr w | NbitsNot w | NbitSpreader w | NbitsAdder w | Register w | RegisterE w 
        | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w | Counter w |CounterNoEnable w |CounterNoLoad w |CounterNoEnableLoad w->
            bitLimsString (w - 1, 0)

        | Shift (w,m,tp) -> bitLimsString (w - 1, 0)
        | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem ->
            bitLimsString (mem.WordWidth - 1, 0)

        | Custom c ->
            bitLimsString (snd c.OutputLabels[getOutputPortNumber port] - 1, 0)

        | IOLabel ->
            let drivingComp = fastSim.FIOActive[ComponentLabel comp.FLabel,snd comp.fId]
            let labelWidth = FastExtract.extractFastSimulationWidth fastSim (drivingComp.Id,snd drivingComp.fId) (OutputPortNumber 0)
            match labelWidth with
            | 0 ->
                failwithf $"What? Can't find width for IOLabel {comp.FLabel}$ "
            | width ->
                bitLimsString (width - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | NotConnected -> failwithf "NotConnected should not occur in getOutputName"
        | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
        | MergeN _ -> failwithf "MergeN should not occur in getOutputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
        | SplitN _ -> failwithf "SplitN should not occur in getOutputName"
        | BusSelection _ -> failwithf "BusSelection should not occur in getOutputName"

    if withComp then 
        comp.FLabel + portName + bitLims
    else 
        portName[1..portName.Length-1] + bitLims


let caseCompAndPortName (name:string) =
    let parts = name.Split([|'.'|])
    match parts.Length with
    | 0 | 1 -> name.ToUpper()
    | n -> (String.concat "." parts[0..n-2]).ToUpper() + "." + camelCaseDottedWords parts[n-1]




/// Get name for a wave. Names are generated from component label, port name, and bit width of wave.
let getName (index: WaveIndexT) (fastSim: FastSimulation) : string =
    let fc = fastSim.WaveComps[index.Id]
    match index.PortType with
    | PortType.Input -> getInputName true fc (InputPortNumber index.PortNumber)
    | PortType.Output -> getOutputName true fc (OutputPortNumber index.PortNumber) fastSim
    |> caseCompAndPortName

/// sheet.component.port, which is what a waveform is called.
///
/// The SHEET is named, not the instance of it: which instance a waveform belongs to is said by
/// where its row sits in the selector and by the combo box beside it, so a name carrying the
/// instance as well said the same thing twice - and said it as a path of labels nobody asked to
/// read. Where that leaves two waveforms with one name, the viewer disambiguates them on hover.
let nameWithSheet (fastSim: FastSimulation) (dispName: string) (waveIndex:WaveIndexT) =
    let fc = fastSim.WaveComps[waveIndex.Id]
    camelCaseDottedWords (fastSim.getSheetNameOfInstance fc.Instance) + "." + dispName

/// Make Wave for each component and port on sheet
let makeWave (ws: WaveSimModel) (fastSim: FastSimulation) (wi: WaveIndexT) : Wave =
    let fc = fastSim.WaveComps[wi.Id]
    let driver = 
        
        match fastSim.Drivers[wi.SimArrayIndex] with
        | Some d -> d
        | None ->
            Log.error $"no simulation waveform driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}] (subsheet {fc.SubSheet}, sheet {fc.SheetName})"
            failwithf "Aborting..."
    if driver.DriverWidth = 0 then 
        Log.warn $"zero-width driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}]"
    let dispName = getName wi fastSim
    let portLabel =
        match wi.PortType with
        | PortType.Input -> getInputName false fc (InputPortNumber wi.PortNumber)
        | PortType.Output -> getOutputName false fc (OutputPortNumber wi.PortNumber) fastSim
 
    {
        WaveId = wi
        SubSheet = fc.SubSheet
        DisplayName = dispName
        ViewerDisplayName = nameWithSheet fastSim dispName wi
        CompLabel = fc.FLabel
        PortLabel = portLabel
        Width = driver.DriverWidth
        DriverIndex = driver.Index
        SheetId = fc.Instance
    }

/// The sheets of the simulated design that came from a component library.
///
/// Read off the design the simulation was built from, so nothing has to thread it down from the
/// project. It was recomputed from Model.CurrentProj on every refresh, which is every tick of a
/// checkbox.
let librarySheetsOf: FastSimulation -> Set<string> =
    Helpers.memoizeByIdentity (fun fs ->
        fs.SimulatedCanvasState
        |> List.filter ComponentLibraries.isLibrarySheet
        |> List.map (fun ldc -> ldc.Name)
        |> Set.ofList)

/// True when the component sits inside an instance of a library sheet.
/// A library component is opaque: its innards are no more offered here than its sheet is offered
/// in the Sheets menu. The instance's own ports are unaffected - they belong to the sheet it was
/// placed on, and appear like any other custom component's, as sheet.L<n>_Comp1.port.
/// AccessPath is the chain of custom component instances the component sits within, so the test
/// is whether any of them is an instance of a library sheet.
///
/// The selector does not need this: the hierarchy it draws already makes a library component
/// opaque, so no instance inside one is ever on show. It is for the two places that reach a
/// component without going through the hierarchy - the default selection, which looks for Viewers
/// anywhere in the design, and the schematic's right-click menu.
let isInsideLibraryComponent (fs: FastSimulation) (fc: FastComponent) =
    let librarySheets = librarySheetsOf fs

    match Set.isEmpty librarySheets with
    | true -> false
    | false ->
        fc.AccessPath
        |> List.mapi (fun i cid -> cid, fc.AccessPath[0 .. i - 1])
        |> List.exists (fun fid ->
            match Map.tryFind fid fs.FCustomComps with
            | Some customComp ->
                match customComp.FType with
                | Custom cc -> Set.contains cc.Name librarySheets
                | _ -> false
            | None -> false)

/// The ports of one elaborated component that carry a waveform, as wave indices into the
/// simulation as it is now.
let waveIndicesOfFComp (fs: FastSimulation) (fId: FComponentId) : WaveIndexT list =
    match Map.tryFind fId fs.WaveComps with
    | None -> []
    | Some fc ->
        let portsOf pType (arrays: IOArray array) =
            if FastCreate.portCarriesWave fs fc pType then
                arrays
                |> Array.toList
                |> List.mapi (fun pn io ->
                    { SimArrayIndex = io.Index
                      Id = fId
                      PortType = pType
                      PortNumber = pn })
            else
                []

        portsOf PortType.Output fc.Outputs @ portsOf PortType.Input fc.InputLinks

/// The wave one index names, in the simulation as it is NOW - or None if that simulation does not
/// offer it any more.
///
/// A WaveIndexT says where its data lies (SimArrayIndex) as well as which port it is. The first is
/// true only of the build it came from, so a selection made before a rebuild has to be resolved
/// against the new one. The other three fields are the stable name and are what this looks up.
///
/// It used to be answered by indexing a map of every wave in the simulation, built by inverting
/// that map's 208,896 keys. The component knows where its own ports are, so a selection of a
/// hundred waves is a hundred lookups.
let reResolveWave (fs: FastSimulation) (wi: WaveIndexT) : WaveIndexT option =
    match Map.tryFind wi.Id fs.WaveComps with
    | Some fc when FastCreate.portCarriesWave fs fc wi.PortType ->
        let arrays =
            match wi.PortType with
            | PortType.Output -> fc.Outputs
            | PortType.Input -> fc.InputLinks

        if wi.PortNumber < arrays.Length then
            Some { wi with SimArrayIndex = arrays[wi.PortNumber].Index }
        else
            None
    | _ -> None

/// Build a wave map in a scope that holds nothing.
///
/// This exists because a Fable map carries the comparer it was built with, and Fable creates that
/// comparer - an object holding a Compare closure - at the construction site. V8 gives all the
/// closures of a function one shared context holding every variable any of them captures, so a
/// comparer made where a FastSimulation is in scope shares a context with it - and the map then
/// pins the whole simulation, step arrays and all, for as long as the map lives. WaveDetails lives
/// in the WaveSimModel, which outlives its simulation by design, and a copy of it sits in every
/// model generation React retains - which is how an ended simulation's memory survived every
/// explicit release. Built here, where no closure can capture anything, the comparer retains
/// nothing. Do not inline this into a caller that has a simulation in scope.
let makeWaveMap (pairs: (WaveIndexT * Wave) list) : Map<WaveIndexT, Wave> = Map.ofList pairs
