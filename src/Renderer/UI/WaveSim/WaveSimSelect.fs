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
        StartCycle = ws.StartCycle
        ShownCycles = ws.ShownCycles
        Multiplier = ws.SamplingZoom
        CycleWidth = singleWaveWidth ws
        Radix = ws.Radix
        SubSheet = fc.SubSheet
        DisplayName = dispName
        ViewerDisplayName = nameWithSheet fastSim dispName wi
        CompLabel = fc.FLabel
        PortLabel = portLabel
        Width = driver.DriverWidth
        DriverIndex = driver.Index
        SheetId = fc.Instance
        SVG = None
        HatchedCycles = EvilHoverCache.initGapStore 0
    }


/// Button to activate wave selection modal
let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let waveCount = Map.count wsModel.AllWaves
    let props, buttonFunc =
        if waveCount > 0 && wsModel.State=Success then
            selectWavesButtonProps "selectButton" true, (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with WaveModalActive = true}))
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
            selectRamButtonProps "selectRamButton", (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = true}))
        else selectRamButtonPropsLight "selectRamButton", (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select RAM")

/// Toggle if a RAM's contents is selected for viewing.
/// The selection is read from the model the update is applied to, not from the one this element
/// was rendered with, so that a change made between the two is not undone here.
let toggleRamSelection (ramId: FComponentId) (ramLabel: string) dispatch =
    dispatch <| UpdateWSModel (fun ws ->
        let selectedRams =
            if isRamSelected ramId ws then
                Map.remove ramId ws.SelectedRams
            else
                Map.add ramId ramLabel ws.SelectedRams
        {ws with SelectedRams = selectedRams})

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
                                    OnChange (fun _ -> toggleRamSelection ram.fId ram.FullName dispatch)
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
                    OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = false}))
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
                                    Delete.Option.OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with RamModalActive = false}))
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

//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//---------------------Waveform Selection for One Component Picked on the Schematic-----------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//

// The schematic's right-click menu offers the ports of the component clicked on. That component is
// one on the canvas, of which the simulation may hold more than one copy: a sheet instantiated
// twice holds two of everything in it. Picking between those copies from the canvas would be
// guesswork - the symbol clicked on is not any one of them - so the menu is offered only when there
// is exactly one, and the wave selector is what serves the rest.

/// The copies of each canvas component the simulation holds, by the id that component has on the
/// canvas it was drawn on.
///
/// A component drawn once has one copy per instantiation of the sheet it is on, so this is keyed on
/// the DESIGN's components and its values run over the EXPANSION. Everything asking about a
/// component picked on the schematic wants exactly this and had no way to ask for it: it read the
/// whole of WaveComps and threw all but a handful of entries away. main6 of largeTest holds about
/// 480,000 components, and the probe below asks on every render while the mouse rests on a wire.
///
/// Keyed on the simulation, which is rebuilt rather than mutated, so a new one is the signal that
/// this is stale. Emptied when a simulation ends - see Helpers.clearIdentityMemos.
let private waveCompIdsOfCanvasComp: FastSimulation -> Map<ComponentId, FComponentId list> =
    Helpers.memoizeByIdentity (fun fs ->
        fs.WaveComps
        |> Map.toList
        |> List.map fst
        |> List.groupBy (fun (compId, _accessPath) -> compId)
        |> Map.ofList)

/// The waves of each component of the simulation, by that component's simulation id.
///
/// Not every wave component has waves: an Input or Output inside a subsheet is simulated by the
/// port of the instance holding it and gets no step array of its own, which is the case
/// waveOfInstancePort exists for. So this cannot stand in for waveCompIdsOfCanvasComp when the
/// question is how many copies there are.
let private waveIndicesOfComp: FastSimulation -> Map<FComponentId, WaveIndexT list> =
    Helpers.memoizeByIdentity (fun fs ->
        fs.WaveIndex
        |> Array.toList
        |> List.groupBy (fun wi -> wi.Id)
        |> Map.ofList)

/// The copies of one canvas component that the simulation holds, in no particular order.
let private copiesOfCanvasComp (fs: FastSimulation) (compId: ComponentId) : FComponentId list =
    Map.tryFind compId (waveCompIdsOfCanvasComp fs) |> Option.defaultValue []

/// The waves of one component of the simulation, as indices, in the order WaveIndex holds them.
let private waveIndicesOf (fs: FastSimulation) (fId: FComponentId) : WaveIndexT list =
    Map.tryFind fId (waveIndicesOfComp fs) |> Option.defaultValue []

/// The custom component instance a component sits directly inside, if any.
/// An access path is ordered from the root of the simulation, so its last element is the instance
/// the component is immediately within.
let enclosingInstance (accessPath: ComponentId list) : FComponentId option =
    match accessPath with
    | [] -> None
    | path -> Some (path[path.Length - 1], path[0 .. path.Length - 2])

/// An Input or Output inside a subsheet drives no wave of its own: the signal belongs to the port
/// of the custom component instance that the sheet sits in, and is named after that port. Return
/// that port's wave, so the menu offers these components rather than passing over them.
/// FastCreate.linkFastCustomComponentsToDriverArrays draws the same correspondence when it links
/// the two sets of data arrays together; this follows it in the opposite direction.
let private waveOfInstancePort
        (fs: FastSimulation)
        (allWaves: Map<WaveIndexT, Wave>)
        (fc: FastComponent)
            : Wave list =
    let portOfLabel (labels: (string * int) list) =
        labels |> List.tryFindIndex (fun (label, _) -> label = fc.FLabel)
    match fc.FType, enclosingInstance (snd fc.fId) with
    | (Input1 _ | Output _), Some instanceId ->
        match Map.tryFind instanceId fs.FCustomComps with
        | Some { FType = Custom cc } ->
            let portType, portNum =
                match fc.FType with
                | Input1 _ -> PortType.Input, portOfLabel cc.InputLabels
                | _ -> PortType.Output, portOfLabel cc.OutputLabels
            portNum
            |> Option.bind (fun pNum ->
                waveIndicesOf fs instanceId
                |> List.tryFind (fun wi -> wi.PortType = portType && wi.PortNumber = pNum)
                // waves inside a library component are not offered, so are not in AllWaves
                |> Option.bind (fun wi -> Map.tryFind wi allWaves))
            |> Option.toList
        | _ -> []
    | _ -> []

/// The waves to offer for one component on the canvas, and the number of copies of that component
/// the simulation holds. A component in a sheet instantiated more than once has one copy per
/// instantiation, and none of them is offered.
let wavesOfComponent
        (fs: FastSimulation)
        (allWaves: Map<WaveIndexT, Wave>)
        (compId: ComponentId)
            : Wave list * int =
    match copiesOfCanvasComp fs compId with
    | [ fId ] ->
        let waves =
            waveIndicesOf fs fId
            // waves inside a library component are not offered, so are not in AllWaves
            |> List.choose (fun wi -> Map.tryFind wi allWaves)
        (if waves = [] then waveOfInstancePort fs allWaves fs.WaveComps[fId] else waves), 1
    | copies ->
        [], copies.Length

/// The label the simulation gives one component on the canvas, when it holds exactly one of it.
let private simLabelOfComponent (fs: FastSimulation) (compId: ComponentId) : string option =
    copiesOfCanvasComp fs compId
    |> List.tryHead
    |> Option.map (fun fId -> fs.WaveComps[fId].FLabel)

/// The waves to offer on the schematic's right-click menu for the component clicked on: none
/// unless a wave simulation is running and holds exactly one copy of that component.
let compWavesToOffer (model: Model) (compId: ComponentId) : Wave list =
    let ws = ModelHelpers.getWSModel model
    match model.WaveSimSheet, ws.State with
    | Some sheet, Success when sheet <> "" ->
        match wavesOfComponent (Simulator.getFastSim()) ws.AllWaves compId with
        | waves, 1 -> waves
        | _ -> []
    | _ -> []

//--------------------------------------------------------------------------------------------------------//
//---------------------------Reading one value off the schematic (the probe)------------------------------//
//--------------------------------------------------------------------------------------------------------//

// Both simulators can answer this, and neither needs anything built for it: every FastSimulation
// carries WaveIndex, WaveComps and Drivers, because buildFastSimulation ends in
// addWavesToFastSimulation whichever simulator asked for it. So the whole probe is two map lookups
// and an array read, and the step simulator and the waveform simulator differ only in which
// simulation and which cycle are handed in.

/// The signal a wire of the open sheet carries, as an index into a simulation's waves.
///
/// A wire is driven by one output port, so the wire's value is that port's. Which copy of the port
/// depends on how many times the open sheet is instantiated in the design being simulated: with
/// more than one there is no single answer, so none is given - the same rule wavesOfComponent
/// applies to the schematic's right-click menu.
let waveIndexOfWire
        (fs: FastSimulation)
        (wireModel: DrawModelType.BusWireT.Model)
        (cid: ConnectionId)
            : WaveIndexT option =
    match Map.tryFind cid wireModel.Wires with
    | None -> None
    | Some wire ->
        match Map.tryFind (BlockHelpers.outputPortStr wire.OutputPort) wireModel.Symbol.Ports with
        | None -> None
        | Some port ->
            match port.PortNumber with
            | None -> None
            | Some portNum ->
                match copiesOfCanvasComp fs (ComponentId port.HostId) with
                | [ fId ] ->
                    waveIndicesOf fs fId
                    |> List.tryFind (fun wi ->
                        wi.PortType = PortType.Output && wi.PortNumber = portNum)
                | _ -> None

/// The value of one wave at one cycle, written as the waveform viewer's value column writes it.
///
/// The step index wraps: the step simulator uses its data arrays as a circular buffer, so a long
/// step simulation is holding only the last MaxArraySize cycles. The waveform simulator does not
/// wrap - its array is sized for the whole run - so there the modulo does nothing.
/// This one reads the given simulation DIRECTLY, and is the only value read left that does.
///
/// Not an oversight. The probe is handed whichever simulation it should answer from - the
/// waveform simulator's while one is running, and the STEP simulator's otherwise (MainView's
/// Probe.source) - and the cache models one active source, not a choice made per call. Routing
/// this through it would have the probe read the waveform simulation while the user is stepping
/// the other one. It goes through a provider when the step simulator gets one, which is also when
/// this stops working by reaching into a FastSimulation that .NET mode will not have.
let waveValueAt (fs: FastSimulation) (cycle: int) (radix: NumberBase) (wi: WaveIndexT) : string option =
    match Array.tryItem wi.SimArrayIndex fs.Drivers with
    | Some(Some driver) ->
        let index = if fs.MaxArraySize > 0 then cycle % fs.MaxArraySize else cycle

        (if driver.DriverWidth > 32 then
             driver.DriverData.TryBig index
             |> Option.map (fun v -> { Dat = BigWord v; Width = driver.DriverWidth })
         else
             driver.DriverData.TryU32 index
             |> Option.map (fun v -> { Dat = Word v; Width = driver.DriverWidth }))
        // padded to a width nothing will be truncated at: unlike the viewer's value column, the
        // probe label sizes itself to its text rather than the text to a column
        |> Option.map (fun fd -> (NumberHelpers.fastDataToPaddedString 60 radix fd).Trim())
    | _ -> None

/// What to write beside the cursor for the wire it is resting on: the signal's name and its value
/// at `cycle`. None when the simulation cannot answer - the wire is on a sheet it holds more than
/// one copy of, carries no wave of its own, or has not been simulated as far as this cycle.
///
/// getName, not nameWithSheet: the waveform viewer prefixes the sheet because its rows are drawn
/// away from the design and come from all over it, so a bare "Q" there could be any of them. Here
/// the label is on the wire, on the sheet the user is looking at, and the sheet name is the one
/// thing the position already says. It is also the longest part of the name, on a label that has
/// to fit beside the pointer without covering the circuit.
let probeLabelForWire
        (fs: FastSimulation)
        (cycle: int)
        (radix: NumberBase)
        (wireModel: DrawModelType.BusWireT.Model)
        (cid: ConnectionId)
            : string option =
    waveIndexOfWire fs wireModel cid
    |> Option.bind (fun wi ->
        waveValueAt fs cycle radix wi
        |> Option.map (fun text -> $"{getName wi fs} = {text}"))

//--------------------------------------------------------------------------------------------------------//
//------------------------------What is shown before anything has been chosen-----------------------------//
//--------------------------------------------------------------------------------------------------------//

/// Most waves a first start will select for itself. A design with more top-level ports than this
/// gets the first few rather than a screenful of waveforms nobody asked for; Select Waves is one
/// click away for the rest.
let private maxDefaultWaves = 12

/// The waves of every component of the simulation that `rank` gives a place to, in that order and
/// then by label, capped.
let private wavesRanked
        (rank: FastComponent -> int option)
        (fs: FastSimulation)
        (allWaves: Map<WaveIndexT, Wave>)
            : WaveIndexT list =
    allWaves
    |> Map.toList
    |> List.choose (fun (wi, _) ->
        Map.tryFind wi.Id fs.WaveComps
        |> Option.bind (fun fc -> rank fc |> Option.map (fun r -> (r, fc.FLabel, wi))))
    |> List.sortBy (fun (r, label, _) -> r, label)
    |> List.truncate maxDefaultWaves
    |> List.map (fun (_, _, wi) -> wi)

/// The waves to show when a wave simulation starts with nothing chosen.
///
/// An empty viewer is never what the user wants: it makes the first thing they see after pressing
/// Start a sentence telling them to press a different button.
///
/// First choice is the simulated top sheet's own ports - inputs, then outputs, then Viewers. They
/// are the signals every design has and the ones a beginner came to look at. An access path is
/// empty exactly for components that are not inside any custom component instance, which is how
/// "on the top sheet" is decided.
///
/// A top sheet can have no ports at all: a whole CPU is often a ROM, a RAM and a couple of
/// subsystem instances, wired to each other and to nothing outside. The `3cpu` demo's `eep1` is
/// exactly that. Falling back to Viewers **anywhere in the design** answers it, because a Viewer is
/// placed for one reason only - somebody wanted to watch that net - so wherever they are, they are
/// the signals the author of the design thought were worth looking at.
let defaultSelectedWaves (fs: FastSimulation) (allWaves: Map<WaveIndexT, Wave>) : WaveIndexT list =
    /// Ports of the simulated top sheet: inputs, then outputs, then Viewers - the order they are
    /// read in, and the order a schematic is drawn in.
    let topSheetPort (fc: FastComponent) =
        match fc.AccessPath, fc.FType with
        | [], Input1 _ -> Some 0
        | [], Output _ -> Some 1
        | [], Viewer _ -> Some 2
        | _ -> None

    /// Every Viewer in the design, at whatever depth.
    let anyViewer (fc: FastComponent) =
        match fc.FType with
        | Viewer _ -> Some 0
        | _ -> None

    match wavesRanked topSheetPort fs allWaves with
    | [] -> wavesRanked anyViewer fs allWaves
    | topPorts -> topPorts

/// Choose waves for a wave simulation that has none. Applied only when the user has selected
/// nothing at all - no waves and no RAMs - so a deliberately pared-down selection is never added
/// to, and a selection saved with the sheet is never overridden.
let withDefaultSelectionIfEmpty (fs: FastSimulation) (wsModel: WaveSimModel) : WaveSimModel =
    match List.isEmpty wsModel.SelectedWaves && Map.isEmpty wsModel.SelectedRams with
    | false -> wsModel
    | true -> { wsModel with SelectedWaves = defaultSelectedWaves fs wsModel.AllWaves }

/// Modal that, when active, shows the ports of one component picked on the schematic, which of
/// them are displayed as waveforms, and allows that to be changed. Opened from the right-click
/// menu on that component.
let selectCompWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let close = fun _ -> dispatch <| UpdateWSModel (fun ws -> {ws with PortSelectComp = None})
    match wsModel.PortSelectComp with
    | None ->
        div [] []
    | Some compId ->
        let fs = Simulator.getFastSim()
        let waves =
            wavesOfComponent fs wsModel.AllWaves compId
            |> fst
            |> List.sortBy (fun wave -> wave.WaveId.PortType, wave.WaveId.PortNumber)
        let compLabel = simLabelOfComponent fs compId |> Option.defaultValue "component"

        /// Said only when the waves are not on the component clicked on but on the ports of the
        /// instance holding it, which is where an Input or Output of a subsheet is simulated.
        let portsOfInstanceNote =
            match waves with
            | wave :: _ when wave.CompLabel <> compLabel ->
                [ str $"The signal is on the "
                  bSpan $"{wave.CompLabel}"
                  str $" port of the sheet instance {compLabel} belongs to, which is where the \
                        waveform viewer shows it."
                  br []; br [] ]
            | _ -> []

        let portRow (wave: Wave) =
            let isSelected = isWaveSelected wave.WaveId wsModel
            let fontStyle = if isSelected then boldFontStyle else normalFontStyle
            waveRow fontStyle [
                input [
                    Type "Checkbox"
                    Checked isSelected
                    OnChange (fun _ -> toggleWaveSelection wave.WaveId wsModel dispatch)
                    Style [MarginLeft "10px"; MarginRight "10px"]
                ]
                p [Style fontStyle] [str wave.PortLabel]
                p [Style fontStyle] [str (match wave.WaveId.PortType with
                                          | PortType.Output -> "Output"
                                          | PortType.Input -> "Input")]
                p [Style fontStyle] [str (if wave.Width = 1 then "1 bit" else $"{wave.Width} bits")]
            ]

        Modal.modal [
            Modal.IsActive true
            Modal.Props [Style [ZIndex 20000]]
        ] [
            Modal.background [ Props [ OnClick close ] ] []
            Modal.Card.card [Props [Style [Width 600]]] [
                Modal.Card.head [] [
                    Modal.Card.title [] [
                        Level.level [] [
                            Level.left [] [ str $"Waveforms from {compLabel}" ]
                            Level.right [] [
                                Delete.delete [
                                    Delete.Option.Size IsMedium
                                    Delete.Option.OnClick close
                                ] []
                            ]
                        ]
                    ]
                ]
                Modal.Card.body [] (
                    portsOfInstanceNote @
                    match waves with
                    | [] ->
                        [ str "No waveforms are available from this component." ]
                    | waves ->
                        [ str "Ticked ports are shown in the waveform viewer. Use "
                          bSpan "Select Waves"
                          str " for anything not on this component."
                          hr []
                          wavePropsTable (List.map portRow waves) ])
                Modal.Card.foot [] []
            ]
        ]
