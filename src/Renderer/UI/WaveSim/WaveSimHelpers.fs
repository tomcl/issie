///Miscellaneous helpers used tby waveform simulator
module WaveSimHelpers
//---------------------------------------------------------------------------------------//
//-----------------------Miscellaneous low=level helper functions------------------------//
//---------------------------------------------------------------------------------------//


open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open TopMenuView
open SimulatorTypes
open FastRun
open NumberHelpers


open WaveSimStyle





//-----------------------------List & Map utilities to deal with exceptions---------------//

/// create a new array which samples the old one every mult cycles.
/// start: index of first cycle.
/// count: number of samples.
let subSamp (arr: 'T array) (start:int) (count: int) (mult:int)  =
    Array.init count (fun n -> arr[start + n*mult])


/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
/// TODO: Remove this since it is unnecessary. Can use WaveValues instead.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne

/// Determines whether a non-binary waveform changes value at the beginning of that clock cycle.
type NonBinaryTransition =
    | Change
    | Const

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

/// Stores information about gaps between NonBinaryTransitions.
/// Used in displayValuesOnWave
type Gap = {
    // First cycle which is Change after a Const cycle
    Start: int
    // How many Const cycles there are immediately after this Change transition
    Length: int
}




/// If true, then show cross-hatch only for non-binary waves when wave is changing value very fast.
let highZoom clkCycleWidth = clkCycleWidth < 2. * Constants.nonBinaryTransLen

/// Left-shift non-binary waveforms by this much.
let xShift clkCycleWidth =
    if highZoom clkCycleWidth then
        clkCycleWidth / 2.
    else Constants.nonBinaryTransLen
        



/// Helper function to create Bulma buttons
let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]



/// Retrieve value of wave at given clock cycle as an int.
/// At extra (sampling) zoom this allows detail clock cycles within one sample
/// therefore clkCycleDetail IS NOT scaled the same as the sample numbers used
/// everywhere else.
let getWaveValue (clkCycleDetail: int) (wave: Wave) (width: int) : FastData =
    match width with
    | w when w > 32 ->
        Array.tryItem clkCycleDetail wave.WaveValues.BigIntStep
        |> function
            | Some (fData) -> 
                { Dat = BigWord fData; Width = width}            
            | _ ->
                // TODO: Find better default value here
                // TODO: Should probably make it so that you can't call this function in the first place.
                printf "Trying to access index %A in wave %A. Default to 0." clkCycleDetail wave.DisplayName
                {Dat = Word 0u; Width = width}
    | _ ->      
        Array.tryItem clkCycleDetail wave.WaveValues.UInt32Step
        |> function
            | Some (fData) -> 
                { Dat = Word fData; Width = width}
            | _ ->
                printf "Trying to access index %A in wave %A. Default to 0." clkCycleDetail wave.DisplayName
                {Dat = Word 0u; Width = width}

/// Make left and right x-coordinates for a clock cycle.
let makeXCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) =
    match transition with
    | BinaryTransition _ ->
        float clkCycle * clkCycleWidth, float (clkCycle + 1) * clkCycleWidth
    | NonBinaryTransition _ ->
        // These are left-shifted by xShift: doing this means that for non-binary
        // waveforms, only the transition at the start of each cycle needs to be considered,
        // rather than the transition at both the start and end of each cycle.
        float clkCycle * clkCycleWidth - xShift clkCycleWidth,
        float (clkCycle + 1) * clkCycleWidth - xShift clkCycleWidth

/// Make top-left, top-right, bottom-left, bottom-right coordinates for a clock cycle.
let makeCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) : XYPos * XYPos * XYPos * XYPos =
    let xLeft, xRight = makeXCoords clkCycleWidth clkCycle transition

    let topL = {X = xLeft; Y = Constants.yTop}
    let topR = {X = xRight; Y = Constants.yTop}
    let botL = {X = xLeft; Y = Constants.yBot}
    let botR = {X = xRight; Y = Constants.yBot}

    topL, topR, botL, botR

/// Generate points for a binary waveform
let binaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: BinaryTransition)  : XYPos array =
    let topL, topR, botL, botR = makeCoords clkCycleWidth (startCycle + index) (BinaryTransition transition)
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero | OneToZero ->
        [|botL; botR|]
    | ZeroToOne | OneToOne ->
        [|topL; topR|]

/// <summary>Generate polyline points for a non-binary waveform via transition info.</summary>
let nonBinaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: NonBinaryTransition)
    : array<XYPos>*array<XYPos> =
    let xLeft, _ = makeXCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)
    let _, topR, _, botR = makeCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)

    let crossHatchMid, crossHatchTop, crossHatchBot =
        {X = xLeft +      xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yTop},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yBot}

    match transition with
    | Change ->
        if highZoom clkCycleWidth then
            [|crossHatchMid; crossHatchTop|], [|crossHatchMid; crossHatchBot|]
        else
            [|crossHatchMid; crossHatchTop; topR|], [|crossHatchMid; crossHatchBot; botR|]
    | Const ->
        [|topR|], [|botR|]

/// <summary>Generate polyfill points for a non-binary gap via gap info.</summary>
let nonBinaryFillPoints (clkCycleWidth: float) (gap: Gap): array<XYPos> =
    let xLeft, _ = makeXCoords clkCycleWidth (gap.Start) (NonBinaryTransition Change)
    let _, xRight = makeXCoords clkCycleWidth (gap.Start+gap.Length-1) (NonBinaryTransition Change)

    let crossHatchMidL, crossHatchTopL, crossHatchBotL =
        {X = xLeft + xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xLeft + 2.0 * xShift clkCycleWidth; Y = Constants.yTop},
        {X = xLeft + 2.0 * xShift clkCycleWidth; Y = Constants.yBot}
    
    let crossHatchMidR, crossHatchTopR, crossHatchBotR =
        {X = xRight + xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xRight; Y = Constants.yTop},
        {X = xRight; Y = Constants.yBot}

    [| crossHatchMidL; crossHatchTopL; crossHatchTopR; crossHatchMidR; crossHatchBotR; crossHatchBotL; crossHatchMidL |]


/// <summary>Find transitions for each clock cycle of a binary waveform.</summary>
let calculateBinaryTransitionsUInt32 (waveValues: array<uint32>) (startCycle: int) (shownCycles: int) (multiplier: int)
    : array<BinaryTransition> =
    let getBit bit = int32 bit
    match startCycle, startCycle+shownCycles with
    | startCyc, endCyc when startCyc = 0 && startCyc < endCyc && endCyc*multiplier < Array.length waveValues ->
        subSamp waveValues startCyc (endCyc-startCyc+1) multiplier
        |> Array.append [| waveValues[0] |]
    | startCyc, endCyc when 0 < startCyc && startCyc < endCyc && endCyc*multiplier < Array.length waveValues ->
        subSamp waveValues (startCyc-1) (endCyc-startCyc+1) multiplier
    | _ ->
        failwithf $"Shown cycles is beyond array bounds: startCyc={startCycle}, shown={shownCycles}, mult={multiplier}"
    |> Array.pairwise
    |> Array.map (fun (x, y) ->
        match getBit x, getBit y with
        | 0, 0 -> ZeroToZero
        | 0, 1 -> ZeroToOne
        | 1, 0 -> OneToZero
        | 1, 1 -> OneToOne
        | _ -> failwithf $"Unrecognised transition {getBit x}, {getBit y}")

let calculateBinaryTransitionsBigInt (waveValues: bigint array) : BinaryTransition array =
    let getBit bit = int32 bit
    Array.append [| waveValues[0] |] waveValues
    |> Array.pairwise
    |> Array.map (fun (x, y) ->
        match getBit x, getBit y with
        | 0, 0 -> ZeroToZero
        | 0, 1 -> ZeroToOne
        | 1, 0 -> OneToZero
        | 1, 1 -> OneToOne
        | _ -> failwithf $"Unrecognised transition {getBit x}, {getBit y}")

/// <summary>Find transitions for each clock cycle of a non-binary waveform.</summary>
let calculateNonBinaryTransitions (waveValues: array<'a>) (startCycle: int) (shownCycles: int) (multiplier: int)
    : array<NonBinaryTransition> =
    match startCycle, startCycle + shownCycles - 1 with
    | startCyc, endCyc when (0 <= startCyc && startCyc <= endCyc && endCyc*multiplier < Array.length waveValues) ->
        subSamp waveValues (startCyc) (endCyc-startCyc+1) multiplier 
    | _ ->
        printfn $"Before failure: waveValues.Length {waveValues.Length} e*m: {(startCycle+shownCycles-1)*multiplier} "
        failwithf $"Shown cycles is beyond array bounds: start={startCycle} shown={shownCycles} mult={multiplier} length = {waveValues.Length}"
    |> Array.pairwise
    |> Array.map (fun (x, y) -> if x = y then Const else Change)
    |> Array.append [| Change |] 


let isWaveSelected (wsModel: WaveSimModel) (index: WaveIndexT) : bool = List.contains index wsModel.SelectedWaves
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
        | IOLabel-> "Wire Label", true
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
        let descr = getCompDetails ws.FastSim (waves[0])
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



let path2fId (fastSim: FastSimulation) (path:ComponentId list) : FComponentId option=
    match path with
    | [] -> 
        None
    | p -> 
        Some <| (p[p.Length-1], p[0..p.Length-2])


let sheetIdToName (fastSim:FastSimulation) sheetId =
    sheetId
    |> path2fId fastSim
    |> function | None -> [] | Some fId -> [fastSim.WaveComps[fId].FLabel]

let sheetIdToSubsheets (fastSim:FastSimulation) (sheetId: ComponentId list) =
    match sheetId.Length with
    | 0 -> []
    | n -> [1..n] |> List.collect (fun i -> sheetId[0..i-1] |> sheetIdToName fastSim)

/// get react element to display a subsheet indication as a dotted string
let subSheetsToNameReact (subSheets: string list) =
    subSheets
    |> String.concat "."
    |> camelCaseDottedWords
    |> str

///Check if one list is an initial sublist of another
let prefixOf (pre:'a list) (whole:'a list) =
    whole.Length >= pre.Length && whole[0..pre.Length-1] = pre

/// Convert Wave list to list of WaveIndexT
let wavesToIds (waves: Wave list) = 
    waves |> List.map (fun wave -> wave.WaveId)




let tr1 react = tr [] [ react ]
let td1 react = td [] [ react ]




/// get all waves electrically connected to a given wave
let getConnectedWaves (ws:WaveSimModel) (wave:Wave) : Wave list =
    ws.AllWaves
    |> Map.filter (fun wi _ -> wi.SimArrayIndex = wave.WaveId.SimArrayIndex)
    |> Map.values
    |> Seq.toList




/// convenience type for use when checking which drivers are connected
type PortIndex = FastComponent * int * PortType

let getFastSimualationLinkedPorts (fs:FastSimulation) ((fc,pNum, pType)) =
    fs.FSComps


 /// Some components have input and output connected - return both ports in that case
let getConnectedComponentPorts (ws:WaveSimModel) ((fc, portNum, portType) as port: PortIndex) : PortIndex list =
    let fs = ws.FastSim
    let portIO = [fc,portNum,PortType.Input; fc,portNum,PortType.Output]
    
    match fc.FType with
    | Output _ -> 
        portIO
    | IOLabel when Map.containsKey (ComponentLabel fc.FLabel,snd fc.fId) fs.FIOActive ->
        portIO
    | _ -> 
        [port]

/// Get the name of a subsheet from its subsheet (string) path list to root of simulation.
let nameOfSubsheet (fs:FastSimulation) (subSheet: string List) =
    match subSheet with
    | [] -> 
        fs.SimulatedTopSheet
    | sheets -> 
        sheets[sheets.Length - 1]

/// Work out a SheetPort from a wave, if one exists
/// SheetPorts may not exist in some corner cases when simulation is ending etc.
let waveToSheetPort fs (wave:Wave) =
    let sheet = nameOfSubsheet fs wave.SubSheet
    let wi = wave.WaveId
    fs.ComponentsById
    |> Map.tryFind (sheet.ToLower()) 
    |> Option.map (Map.tryFind  (fst wi.Id))
    |> Option.flatten
    |> Option.map (fun comp ->
            let port =
                match wi.PortType, comp.InputPorts.Length > 0, comp.OutputPorts.Length > 0 with
                | PortType.Input, true, _ | PortType.Output, true, false -> comp.InputPorts[wi.PortNumber]
                | PortType.Output ,_, true | PortType.Input, false, true -> comp.OutputPorts[wi.PortNumber]
                | _ -> failwithf "What? no parts found in waveToSheetPort"
            {
                Sheet = sheet.ToLower()
                PortOnComp = port
            }
            |> fun p -> [p])
    |> Option.defaultValue []


/// function to print a lits of SheetPort for debugging IOLabels
let printSPL (tp:string) (fs:FastSimulation) (spL:SheetPort list) =
    let comps = fs.ComponentsById
    let printSP (sp: SheetPort) =
        let comp = comps[sp.Sheet][ComponentId sp.PortOnComp.HostId]
        sprintf $"IsIOLabel={comp.Type=IOLabel}, lab={comp.Label}"
    spL
    |> List.map printSP
    |> String.concat ","
    |> printfn "%s:[%s]" tp
    spL

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
        |> Map.values
        |> Seq.toList
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


/// info button generation function
let infoButton  (tooltipMessage:string) (style: CSSProp list) (tooltipPosition:string)  : ReactElement =
    div 
        [
            HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsInfo} {tooltipPosition}"
            Tooltip.dataTooltip tooltipMessage
            Style style
        ]
        [str Constants.infoSignUnicode]

/// button driving a popup with a page of info about waveform simulator
let waveInfoButton (name:string) (dispatch: Msg -> Unit) : ReactElement =
    button 
        (topHalfButtonProps IsInfo "RefreshButton" false)
        (fun _ -> (UIPopups.viewWaveInfoPopup dispatch name))
        (str name)

/// button used to give hover message  about selection filter box.
let selectionInfoButton =
    infoButton Constants.infoMessage [
        FontSize "25px";
        MarginTop "0px";
        MarginLeft "10px";
        Float FloatOptions.Left] Tooltip.IsTooltipRight




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
    printf "endbuttonaction"
    removeHighlights model dispatch
    dispatch <| EndWaveSim

/// Return info about current state of waveform simulator
/// which is used to switch buttons on/off etc.
let getWaveSimButtonOptions (canv: CanvasState) (model:Model) (ws:WaveSimModel)  : WaveSimButtonOptions =
    let fs = ws.FastSim
    let simExists = model.WaveSimSheet <> Some "" && model.WaveSimSheet <> None
    let success = (ws.State = Success || ws.State=Loading)

    let hasSimErr =
        match SimulationView.simulateModel model.WaveSimSheet (ModelHelpers.Constants.maxLastClk + ModelHelpers.Constants.maxStepsOverflow) canv model with
        | (Error e, _) -> true
        | (Ok simData, canvState) -> false
    let errored =
        match hasSimErr, ws.State with
        | true, _ -> true
        | false, NonSequential -> true
        | false, _ -> false

    let running = (success || errored) && simExists
        
    let isDirty = 
        simExists &&
        running && 
        not <| FastRun.compareLoadedStates fs canv model.CurrentProj &&
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

                            
/// Run ws.FastSim if necessary to ensure simulation has number of steps needed to
/// display all cycles on screen. TimeOut is an optional time out used to implement
/// a progress bar.
let extendSimulation timeOut (ws:WaveSimModel) =
    let stepsNeeded = (ws.ShownCycles + ws.StartCycle) * ws.CycleMultiplier
    printfn $"Extending simulation to {stepsNeeded} cycles"
    if stepsNeeded > Constants.maxLastClk then
        failwithf $"Trying to extend simulation to {stepsNeeded} which is beyond available clocks (Constants.maxLastClk)"
    FastRun.runFastSimulation timeOut (stepsNeeded + Constants.extraSimulatedSteps) ws.FastSim

/// returns true if any memory component in fs linked to a .ram file is outofdate because of the .ram file changing
let checkIfMemoryCompsOutOfDate (p: Project) (fs:FastSimulation) = 
    fs.ComponentsById
    |> Map.exists (fun sheet m ->
        m
        |> Map.exists (fun cid comp ->
            match comp.Type with
            | Memory ({Init=FromFile fName} as mem) -> 
                match FilesIO.initialiseMem mem p.ProjectPath with
                | Ok mem' -> mem' <> mem
                | Error _ -> false
            | _ -> true))






    
