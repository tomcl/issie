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


/// determines how components are dispalyed in waveform selector
let getCompDetails fs wave =
    let fc = fs.WaveComps[wave.WaveId.Id]
    let label = fc.FLabel
    let descr, oneLine =
        match fc.FType with
        | Input1 _ -> "Input",true
        | Output _ -> "Output", true
        | Constant1 _ -> "What? can't happen", true
        | Viewer _ -> "Viewer", true
        | IOLabel-> "Net Label", true
        | NotConnected -> "What? can't happen", true
        | Not ->
            let gateType = $"{fc.FType}".ToUpper()
            $"{gateType} gate", false
        | GateN (gateType, n) ->
            $"{n} input {gateType} gate", false
        | BusCompare (width,v) -> $"Bus Compare ='{v}'", false
        | BusCompare1(width,v,s)-> $"Bus Compare ='{s}'", false
        | Mux2 -> "2 input multiplexer", false
        | Mux4 -> "4 input multiplexer", false
        | Mux8 -> "8 input multiplexer", false
        | Demux2 -> "2 input demultiplexer", false
        | Demux4 -> "4 input demultiplexer", false
        | Demux8 -> "8 input demultiplexer", false
        | Decode4 -> "2 line decoder", false
        | NbitsAdder n | NbitsAdderNoCin n 
        | NbitsAdderNoCout n | NbitsAdderNoCinCout n     
            -> $"{n} bit adder",false
        | NbitsXor (n,None) -> $"{n} XOR gates",false
        | NbitsXor(n, Some Multiply) -> $"{n} bit multiply",false
        | NbitsAnd n -> $"{n} AND gates",false
        | NbitsNot n -> $"{n} Not gates",false
        | NbitSpreader n -> $"1 -> {n} bits spreader",false
        | NbitsOr n -> $"{n} OR gates",false
        | Custom x -> $"({x.Name} instance)",false
        | DFF -> "D flipflip", false
        | DFFE -> "D flipflop with enable", false
        | Register n -> $"{n} bit D register", false
        | RegisterE n -> $"{n} bit D register with enable", false
        | Counter n -> $"{n} bit Counter with enable and load", false
        | CounterNoLoad n -> $"{n} bit Counter with enable", false
        | CounterNoEnable n -> $"{n} bit Counter with load", false
        | CounterNoEnableLoad n -> $"{n} bit Counter", false
        | AsyncROM1 mem -> $"ROM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) asynchronous read", false
        | ROM1 mem -> $"ROM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) synchronous read", false
        | RAM1 mem -> $"RAM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) synchronous read", false
        | AsyncRAM1 mem -> $"RAM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) asynchronous read", false             
        | BusSelection _ | MergeWires | MergeN _ | SplitWire _ | SplitN _ ->
            failwithf "Bus select, MergeWires, MergeN, SplitWire, SplitN should not appear"
        | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
            failwithf "Legacy component types should not appear"
        | Shift _ ->
            "Error: Shift is an internal component that should not appear", false
    match oneLine with
    | true -> $"{label}{portBits wave.Width} {descr}"
    | false -> $"{label} {descr}"

/// Which group (for selector classification) is a component in?
let getCompGroup fs wave =
    match fs.WaveComps[wave.WaveId.Id].FType with
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


/// Name for summary field in details element.
/// NB: There are fields which are commented out: these can be added back in
/// later on if we want to group those components together by type rather than
/// separately by name.
let summaryName (ws: WaveSimModel) (cBox: CheckBoxStyle) (subSheet: string list) (waves: Wave list): ReactElement =
    match cBox with
    | PortItem (_,name) ->
        str <| camelCaseDottedWords name
    | ComponentItem fc->
        let descr = getCompDetails (Simulator.getFastSim()) (waves[0])
        str <| descr
        
    | GroupItem (compGroup,_) ->
        match compGroup with
        | InputOutput -> "Inputs / Outputs / Labels / Viewers"
        //| Viewers -> "Viewers"
        //| WireLabel -> "Wire Labels"
        | Buses -> "Buses"
        | Gates -> "Logic Gates"
        | MuxDemux -> "Multiplexers"
        | Arithmetic -> "Arithmetic"
        | FFRegister -> "Flip Flops and Registers"
        | Memories -> "RAMs and ROMs"
        | Component compLabel -> compLabel
        | CustomComp -> "Custom Components"
        | _ -> "What? Not used!"
        |> (fun name -> str $"""{name.ToUpper()}""")

    | SheetItem subSheet ->
        str <| $"Subsheet {camelCaseDottedWords subSheet[subSheet.Length-1]}"

/// get react element to display a subsheet indication as a dotted string
let subSheetsToNameReact (subSheets: string list) =
    subSheets
    |> String.concat "."
    |> camelCaseDottedWords
    |> str

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
let waveToSheetPort fs (wave:Wave) =
    let wi = wave.WaveId
    sheetOfWave fs wave
    |> Option.bind (fun sheet ->
        fs.ComponentsById
        |> Map.tryFind sheet
        |> Option.bind (Map.tryFind (fst wi.Id))
        |> Option.map (fun comp ->
            let port =
                match wi.PortType, comp.InputPorts.Length > 0, comp.OutputPorts.Length > 0 with
                | PortType.Input, true, _ | PortType.Output, true, false -> comp.InputPorts[wi.PortNumber]
                | PortType.Output ,_, true | PortType.Input, false, true -> comp.OutputPorts[wi.PortNumber]
                | _ -> failwithf "What? no parts found in waveToSheetPort"
            [{ Sheet = sheet; PortOnComp = port }]))
    |> Option.defaultValue []


/// given a SheetPort, get all directly connected SheetPorts
let connectedPorts fs sheetPort =
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
let getWaveSimButtonOptions (canv: CanvasState) (model:Model) (ws:WaveSimModel)  : WaveSimButtonOptions =
    let fs = Simulator.getFastSim()
    let simExists = model.WaveSimSheet <> Some "" && model.WaveSimSheet <> None
    let success = (ws.State = Success || ws.State=Loading)

    let simCheck = validateWaveModel model.WaveSimSheet canv model
    let hasSimErr = Result.isError simCheck

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
        | false, false -> "Start Simulation", IsSuccess
        | true, _ -> "EndSimulation", IsDanger

    //printfn $"Running= {running}, Dirty={isDirty}"

    
    {
        IsDirty =  isDirty
        IsRunning = running
        IsErrored = errored
        StartEndMsg = startEndMsg
        StartEndColor = startEndColor
    } 



    
