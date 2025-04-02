module WaveSimSelect

//---------------------------------------------------------------------------------------//
//-------------Waveform Selection Popup and RAM Selection Popup--------------------------//
//---------------------------------------------------------------------------------------//

// Functions to make modal popups that allows waveforms and RAMs
// to be selected or deselected for display in the waveform simulator.


// TODO: should RAM selection go to separate module or is it too small for that?

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open DiagramStyle
open UIPopups
open MenuHelpers
open TopMenuView
open WaveSimSelectHelpers

//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//----------------------------Miscellaneous subfunctions for Wave Selection-------------------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//


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
        | RegisterE _ when portName = "EN" -> bitLimsString (0, 0)
        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor(w, _) | NbitsNot w | NbitsAnd w | NbitsAdder w | NbitsOr w  
        | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w
        | BusCompare(w,_) | BusCompare1(w,_,_)  |Register w  | NbitSpreader w ->
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
            //printfn $"IOLabel name {comp.FLabel}"
            let drivingComp = fastSim.FIOActive[ComponentLabel comp.FLabel,snd comp.fId]
            //printfn "driving compm done"
            let labelWidth = FastExtract.extractFastSimulationWidth fastSim (drivingComp.Id,snd drivingComp.fId) (OutputPortNumber 0)
            //printfn "label width fdone"
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

/// The sheet name here is the  runtime (simulation) sheet instance name
/// Derived from sheet name with disambiguation if needed
let nameWithSheet (fastSim: FastSimulation) (dispName: string) (waveIndex:WaveIndexT) =
    let fc = fastSim.WaveComps[waveIndex.Id]
    camelCaseDottedWords(fc.SimSheetName) + "." + dispName

/// Make Wave for each component and port on sheet
let makeWave (ws: WaveSimModel) (fastSim: FastSimulation) (wi: WaveIndexT) : Wave =
    let fc = fastSim.WaveComps[wi.Id]
    //printfn $"Making wave for {fc.FullName}, portType={wi.PortType}, portNumber={wi.PortNumber}, SubSheet={fc.SubSheet}, SheetName={fc.SheetName}"
    let driver = 
        
        match fastSim.Drivers[wi.SimArrayIndex] with
        | Some d -> d
        | None ->
            printfn "What? No driver!"
            printfn $"ERROR Making wave for {fc.FullName}, portType={wi.PortType}, portNumber={wi.PortNumber}, SubSheet={fc.SubSheet}, SheetName={fc.SheetName}"
            printfn $"Can't find simulation waveform driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}]"
            failwithf "Aborting..."
    if driver.DriverWidth = 0 then 
        printfn $"Warning! 0 width driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}]"
    let dispName = getName wi fastSim
    let portLabel =
        match wi.PortType with
        | PortType.Input -> getInputName false fc (InputPortNumber wi.PortNumber)
        | PortType.Output -> getOutputName false fc (OutputPortNumber wi.PortNumber) fastSim
 
    {
        WaveId = wi
        StartCycle = ws.StartCycle
        ShownCycles = ws.ShownCycles
        Multiplier = ws.SamplingZoom
        CycleWidth = singleWaveWidth ws
        Radix = ws.Radix
        SubSheet = fc.SubSheet
        DisplayName = dispName
        SheetLabel = fc.SimSheetName
        ViewerDisplayName = nameWithSheet fastSim dispName wi
        CompLabel = fc.FLabel
        PortLabel = portLabel
        Width = driver.DriverWidth
        DriverIndex = driver.Index
        SheetId = fc.SimSheetName
        Conns = []
        SVG = None
        HatchedCycles = EvilHoverCache.initGapStore 0
    }


/// Button to activate wave selection modal
let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let waveCount = Map.count wsModel.AllWaves
    let props, buttonFunc =
        if waveCount > 0 && wsModel.State=Success then
            selectWavesButtonProps "selectButton" true, (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with WaveModalActive = true}))
        else selectWavesButtonPropsLight "selectButton", (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select Waves")



//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//-------------------------------------RAM Selection from Wave Simulator----------------------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//

/// Button to activate RAM selection modal.
let selectRamButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramCount = List.length wsModel.RamComps
    let props, buttonFunc =
        if ramCount > 0 && wsModel.State=Success then
            selectRamButtonProps "selectRamButton", (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = true}))
        else selectRamButtonPropsLight "selectRamButton", (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select RAM")

/// Toggle if a RAM's contents is selected for viewing.
let toggleRamSelection (ramId: FComponentId) (ramLabel: string) (wsModel: WaveSimModel) dispatch =
    let selectedRams =
        if isRamSelected ramId wsModel then
            Map.remove ramId wsModel.SelectedRams
        else
            Map.add ramId ramLabel wsModel.SelectedRams
    dispatch <| UpdateWSModel (fun ws -> {wsModel with SelectedRams = selectedRams})

/// Modal that, when active, allows users to select RAMs to view their contents.
let selectRamModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
        let fs = Simulator.getFastSim()
        let ramRows (ramId: FComponentId) : ReactElement =
            match Map.tryFind ramId fs.FComps with
            | Some ram ->
                tr [] [
                    td []
                        [ Checkbox.checkbox []
                            [ Checkbox.input [
                                Props (checkboxInputProps @ [
                                    Checked <| isRamSelected ram.fId wsModel
                                    OnChange (fun _ -> toggleRamSelection ram.fId ram.FullName wsModel dispatch)
                                ])
                            ] ]
                        ]
                    td [] [ label [ ramRowStyle ] [ str ram.FullName ] ]
                ]
            | None -> tr [] [td [] []]
        Modal.modal [
            Modal.IsActive wsModel.RamModalActive
            Modal.Props [Style [ZIndex 20000]]
        ] [
            Modal.background [
                Props [
                    OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = false}))
                ]
            ] []
            Modal.Card.card [Props [Style [Width 800]]] [
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left [] [ str "Select RAM" ]
                            Level.right [] [
                                Delete.delete [
                                    Delete.Option.Size IsMedium
                                    Delete.Option.OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = false}))
                                ] []
                            ]
                        ]
                    ]
                ]
                Modal.Card.body [] [
                    str "Select ROM or asynchronous RAM components to view their contents in any clock cycle. "
                    str "Note that synchronous RAM components cannot currently be viewed in the waveform simulator. "
                    br []
                    br []
                    str "On a write, the corresponding location will be "; colorSpan "red" "highlighted in red during the clock cycle in which the written value is first output.";
                    str " On a read, the corresponding location will be "; colorSpan "blue" "highlighted in blue.";
                    br [] ; br []
                    str "The RAM display has two modes: "; bSpan "sparse display"; str " and "; bSpan "windowed display. ";
                    br []; str "Type in the"; iSpan " Window start"; str " box to set the locations viewed in a window. Leave it blank for sparse display.";
                    br []; br [] 
                    str "If the RAM has too many non-zero locations to display all at once, the windowed display will be used."
                    hr []
                    Table.table [] [
                        tbody []
                            (List.map ramRows wsModel.RamComps)
                    ]
                ]

                Modal.Card.foot [] []
            ]
        ]
