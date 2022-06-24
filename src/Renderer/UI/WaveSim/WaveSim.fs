module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open Sheet.SheetInterface

// TODO: Move all Style definitions into Style.fs
// TODO: Combine Style definitions into same variables where possible

/// Generates SVG to display waveform values when there is enough space
/// TODO: Fix this so it does not generate all 500 cycles.
let displayValuesOnWave wsModel (waveValues: WireData list) (transitions: NonBinaryTransition list) : ReactElement list =
    let changeTransitions =
        transitions
        |> List.indexed
        |> List.filter (fun (i, x) -> x = Change)
        |> List.map (fun (i, x) -> i)

    let gaps : Gap list =
        // Append dummy transition to end to check final gap length
        changeTransitions @ [Constants.maxLastClk]
        |> List.pairwise
        // Get start of gap and length of gap
        |> List.map (fun (i1, i2) -> {
                Start = i1
                Length = i2 - i1
            }
        )

    gaps
    |> List.map (fun gap ->
        let waveValue =
            convertWireDataToInt waveValues[gap.Start]
            |> valToString wsModel.Radix

        let availableWidth = float gap.Length * (zoomLevel wsModel) - 2. * Constants.nonBinaryTransLen
        let requiredWidth = DrawHelpers.getTextWidthInPixels (waveValue, Constants.valueOnWaveText)
        let widthWithPadding = requiredWidth + Constants.valueOnWavePadding
        // Display nothing if there is not enough space
        if availableWidth < requiredWidth then
            []
        else
            let valueText i =
                text (valueOnWaveProps wsModel i gap.Start widthWithPadding)
                    [ str waveValue ]
            /// Calculate how many times the value can be shown in the space available
            let repeats = int <| availableWidth / widthWithPadding
            [ 0 .. repeats ]
            |> List.map valueText
    )
    |> List.concat

/// Called when InitiateWaveSimulation msg is dispatched
/// Generates the polyline(s) for a specific waveform
let generateWaveform (wsModel: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    if List.contains index wsModel.SelectedWaves then
        // let waveName = wave.DisplayName
        // printf "generating wave for %A" waveName
        let polylines =
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            | 1 ->
                let transitions = calculateBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let wavePoints =
                    List.mapi (binaryWavePoints (zoomLevel wsModel) 0) transitions 
                    |> List.concat
                    |> List.distinct

                [ polyline (wavePolylineStyle wavePoints) [] ]
            | _ ->
                let transitions = calculateNonBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let fstPoints, sndPoints =
                    List.mapi (nonBinaryWavePoints (zoomLevel wsModel) 0) transitions 
                    |> List.unzip
                let makePolyline points = 
                    let points =
                        points
                        |> List.concat
                        |> List.distinct
                    polyline (wavePolylineStyle points) []

                let valuesSVG = displayValuesOnWave wsModel wave.WaveValues transitions

                List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG

        {wave with Polylines = Some polylines}
    else wave

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let getInputPortName (compType: ComponentType) (port: InputPortNumber) : string =
    let muxPortName (size: int) : string =
        if port = (InputPortNumber size) then ".SEL"
        else "." + string port

    match compType with
    | Not | BusCompare _ ->
        ".IN"
    | And | Or | Xor | Nand | Nor | Xnor ->
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

    | Input _ | Output _ | Constant1 _ | Constant _ | Viewer _ ->
        ""
    | DFF | Register _ ->
        ".D"

    | ROM1 _ | AsyncROM1 _ ->
        ".ADDR"

    | Demux2 | Demux4 | Demux8 ->
        match port with
        | InputPortNumber 0 -> ".DATA"
        | _ -> ".SEL"

    | NbitsXor _ ->
        match port with
        | InputPortNumber 0 -> ".P"
        | _ -> ".Q"

    | NbitsAdder _ ->
        match port with
        | InputPortNumber 0 -> ".Cin"
        | InputPortNumber 1 -> ".P"
        | _ -> ".Q"

    | DFFE | RegisterE _ ->
        match port with
        | InputPortNumber 0 -> ".D"
        | _ -> ".EN"

    | RAM1 _ | AsyncRAM1 _ ->
        match port with
        | InputPortNumber 0 -> ".ADDR"
        | InputPortNumber 1 -> ".DIN"
        | _ -> ".WEN"

    | Custom c ->
        "." + fst c.InputLabels[getInputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | IOLabel -> failwithf "IOLabel should not occur in getInputPortName"
    | MergeWires -> failwithf "MergeWires should not occur in getInputPortName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getInputPortName"
    | BusSelection _ -> failwithf "BusSelection should not occur in getInputPortName"

let getInputName (comp: NetListComponent) (port: InputPortNumber) : string =
    let portName : string = getInputPortName comp.Type port
    let bitLims : string =
        match comp.Type with
        | Not | BusCompare _ | And | Or | Xor | Nand | Nor | Xnor
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | Register _ | DFFE | RegisterE _ ->
            bitLimsString (0, 0)

        | Input w | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsAdder w  ->
            bitLimsString (w - 1, 0)

        // TODO: Find the right parameters for RAMs and ROMs.
        | ROM1 _ | AsyncROM1 _ | RAM1 _ | AsyncRAM1 _ ->
            ""

        | Custom c ->
            bitLimsString (snd c.InputLabels[getInputPortNumber port] - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | IOLabel -> failwithf "IOLabel should not occur in getInputName"
        | MergeWires -> failwithf "MergeWires should not occur in getInputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getInputName"
        | BusSelection _ -> failwithf "BusSeleciton should not occur in getInputName"

    comp.Label + portName + bitLims

let getOutputPortName (compType: ComponentType) (port: OutputPortNumber) : string =
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ | NbitsXor _ ->
        ".OUT"
    | Input _ | Output _ | Constant1 _ | Constant _ | Viewer _ | IOLabel ->
        ""
    | Demux2 | Demux4 | Demux8 ->
        "." + string port
    | NbitsAdder _ ->
        match port with
        | OutputPortNumber 0 ->
            ".SUM"
        | _ ->
            ".COUT"
    | DFF | DFFE | Register _ | RegisterE _ ->
        ".Q"
    | RAM1 _ | AsyncRAM1 _ | AsyncROM1 _ | ROM1 _ ->
        ".DOUT"
    | Custom c ->
        "." + fst c.OutputLabels[getOutputPortNumber port]

    | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
    | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
    | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

let getOutputName (comp: NetListComponent) (port: OutputPortNumber) (fastSim: FastSimulation): string =
    let portName = getOutputPortName comp.Type port
    let bitLims =
        match comp.Type with
        | Not | And | Or | Xor | Nand | Nor | Xnor  | BusCompare _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)

        | Input w | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsAdder w | Register w | RegisterE w ->
            bitLimsString (w - 1, 0)

        | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem ->
            bitLimsString (mem.WordWidth - 1, 0)

        | Custom c ->
            bitLimsString (snd c.OutputLabels[getOutputPortNumber port] - 1, 0)

        | IOLabel ->
            let drivingComp = fastSim.FIOActive[ComponentLabel comp.Label,[]]
            let labelWidth = FastRun.extractFastSimulationWidth fastSim (drivingComp.Id,[]) (OutputPortNumber 0)
            match labelWidth with
            | None ->
                failwithf $"What? Can't find width for IOLabel {comp.Label}$ "
            | Some width ->
                bitLimsString (width - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
        | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

    comp.Label + portName + bitLims

let getName (comp: NetListComponent) (index: WaveIndexT) (fastSim: FastSimulation) : string =
    match index.PortType with
    | PortType.Input -> getInputName comp (InputPortNumber index.PortNumber)
    | PortType.Output -> getOutputName comp (OutputPortNumber index.PortNumber) fastSim

/// starting with just output ports only: not showing input ports.
let makeWave (fastSim: FastSimulation) (netList: Map<ComponentId, NetListComponent>) (index: WaveIndexT) (comp: NetListComponent) : Wave =
    let driverComp, driverPort =
        match index.PortType with
        | PortType.Output -> comp, (OutputPortNumber index.PortNumber)
        | PortType.Input ->
            match Map.tryFind (InputPortNumber index.PortNumber) comp.Inputs with
            | Some (Some nlSource) -> netList[nlSource.SourceCompId], nlSource.OutputPort
            | Some None -> failwithf "is there an unconnected input?\n wave: %A\n port: %A %A\n type: %A" comp.Label index.PortType index.PortNumber comp.Type
            | None -> failwithf "InputPortNumber %A not in comp.Inputs" (index.PortNumber)

    //need to get driving one so needs to be an output
    let driverId, driverPort = getFastDriver fastSim driverComp driverPort

    let waveValues =
        [ 0 .. Constants.maxLastClk ]
        |> List.map (fun i -> FastRun.extractFastSimulationOutput fastSim i driverId driverPort)

    {
        WaveId = index
        Type = comp.Type
        CompLabel = comp.Label
        SheetId = []
        Driver = {DriverId = driverId; Port = driverPort}
        DisplayName = getName comp index fastSim
        Width =  getFastOutputWidth fastSim.FComps[driverId] driverPort
        WaveValues = waveValues
        Polylines = None
    }

let getWaves (simData: SimulationData) (reducedState: CanvasState) : Map<WaveIndexT, Wave> =
    let fastSim = simData.FastSim
    let netList = Helpers.getNetList reducedState

    /// Adds all input and output ports from each component.
    /// Removes illegal components (MergeWires, SplitWire, BusSelection).
    let getAllPorts ((id, nlc): (ComponentId * NetListComponent)) : (WaveIndexT * NetListComponent) list =
        match nlc.Type with
        // These types should not appear in the waveform simulator.
        | MergeWires | SplitWire _ | BusSelection _ ->
            []
        | _ ->
            let inputNum = Map.count nlc.Inputs
            let outputNum = Map.count nlc.Outputs

            let getWavesForEachPort (portNum: int) (portType: PortType) =
                [0 .. portNum - 1]
                |> List.map (fun x ->
                    {Id = id; PortType = portType; PortNumber = x}, nlc
                )

            let inputs =
                match nlc.Type with
                | IOLabel -> []
                | _ -> getWavesForEachPort inputNum PortType.Input

            let outputs = getWavesForEachPort outputNum PortType.Output

            List.append inputs outputs

    let ioLabels, otherComps =
        netList
        |> Map.toList
        |> List.partition (fun (_, nlc) -> nlc.Type = IOLabel)

    /// Remove duplicate IOLabels. These occur when e.g. you have an IOLabel connected to an output, and the same
    /// IOLabel driving one or more inputs.
    let ioLabels : (ComponentId * NetListComponent) list = List.distinctBy (fun (_, nlc) -> nlc.Label) ioLabels

    List.append ioLabels otherComps
    |> List.collect getAllPorts
    |> Map.ofList
    |> Map.map (makeWave fastSim netList)

/// Sets all waves as selected or not selected depending on value of newState
let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) dispatch : unit =
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}
    // selectConns model conns dispatch

let selectAll (wsModel: WaveSimModel) dispatch =
    let allWavesSelected = Map.forall (fun index _ -> isWaveSelected wsModel index) wsModel.AllWaves

    tr summaryProps [
        th [] [
            Checkbox.checkbox []
                [ Checkbox.input [
                    Props 
                        (checkboxInputProps @ [
                            Checked allWavesSelected
                            OnChange(fun _ -> toggleSelectAll (not allWavesSelected) wsModel dispatch )
                    ])
                ] ]
            ]
        th [] [str "Select All"]
    ]

let toggleWaveSelection (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else [index] @ wsModel.SelectedWaves
    let wsModel = {wsModel with SelectedWaves = selectedWaves}
    dispatch <| InitiateWaveSimulation wsModel
    // changeWaveSelection name model waveSimModel dispatch

let toggleSelectSubGroup (wsModel: WaveSimModel) dispatch (selected: bool) (waves: Map<WaveIndexT, Wave>) =
    let toggledWaves = Map.keys waves |> Seq.toList
    let selectedWaves =
        if selected then
            List.append wsModel.SelectedWaves toggledWaves
        else
            List.except wsModel.SelectedWaves toggledWaves
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}

let checkboxRow (wsModel: WaveSimModel) dispatch (index: WaveIndexT) =
    let fontStyle = if isWaveSelected wsModel index then boldFontStyle else normalFontStyle
    tr  [ fontStyle ]
        [
            td  [ noBorderStyle ]
                [ Checkbox.checkbox []
                    [ Checkbox.input [
                        Props (checkboxInputProps @ [
                            OnChange(fun _ -> toggleWaveSelection index wsModel dispatch )
                            Checked <| isWaveSelected wsModel index
                        ])
                    ] ]
                ]
            td  [ noBorderStyle]
                [str wsModel.AllWaves[index].DisplayName]
        ]

let menuSummary menuType =
    let name =
        match menuType with
        | WireLabels -> "Wire Labels"
        | Components comp -> comp

    summary
        summaryProps
        [ str name ]

let labelRows (menuType: SelectionMenu) (labels: WaveIndexT list) (wsModel: WaveSimModel) dispatch : ReactElement =
    let waves =
        match menuType with
        | WireLabels -> Map.filter (fun _ (wave: Wave) -> wave.Type = IOLabel) wsModel.AllWaves
        | Components compLabel -> Map.filter (fun _ (wave: Wave) -> wave.CompLabel = compLabel) wsModel.AllWaves
    let subGroupSelected = Map.forall (fun index _ -> isWaveSelected wsModel index) waves
    
    tr summaryProps [
        th [] [
            Checkbox.checkbox [] [
                Checkbox.input [
                    Props [
                        Checked subGroupSelected
                        OnChange (fun _ -> toggleSelectSubGroup wsModel dispatch (not subGroupSelected) waves)
                    ]
                ]
            ]
        ]
        th [] [
            details
                detailsProps
                [   menuSummary menuType
                    Table.table [] [
                        tbody []
                            (List.map (checkboxRow wsModel dispatch) labels)
                    ]
                ]
        ]
    ]

let componentRows wsModel dispatch ((compName, waves): string * Wave list) : ReactElement =
    let waveLabels =
        List.sortBy (fun (wave: Wave) -> wave.DisplayName) waves
        |> List.map (fun (wave: Wave) -> wave.WaveId)
    labelRows (Components compName) waveLabels wsModel dispatch

let selectWaves (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let wireLabelWaves, compWaves = Map.partition (fun _ (wave: Wave) -> wave.Type = IOLabel) wsModel.AllWaves
    let wireLabels =
        wireLabelWaves
        |> Map.values |> Seq.toList
        |> List.sortBy (fun wave -> wave.DisplayName)
        |> List.map (fun wave -> wave.WaveId)

    let wireLabelRows =
        if List.length wireLabels > 0 then
            [ labelRows WireLabels wireLabels wsModel dispatch ]
        else []

    let compWaveLabels =
        compWaves
        |> Map.values |> Seq.toList
        |> List.sortBy (fun (wave: Wave) -> wave.CompLabel)
        |> List.groupBy (fun wave -> wave.CompLabel)

    Table.table [
        Table.IsBordered
        Table.IsFullWidth
        Table.Props [
            Style [BorderWidth 0]
        ]
    ] [ thead []
            ( [selectAll wsModel dispatch] @
                wireLabelRows @
                (List.map (componentRows wsModel dispatch) compWaveLabels)
            )
    ]

let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let waveCount = Map.count wsModel.AllWaves
    let props, buttonFunc =
        if waveCount > 0 then
            selectWavesButtonProps, (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = true})
        else selectWavesButtonPropsLight, (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select Waves")

let selectWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Modal.modal [
        Modal.IsActive wsModel.WaveModalActive
    ] [
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = false})
            ]
        ] []
        Modal.Card.card [] [
            Modal.Card.head [] [
                Modal.Card.title [] [
                    Level.level [] [
                        Level.left [] [ str "Select RAM" ]
                        Level.right [
                            Modifiers [
                                Modifier.BackgroundColor IsSuccess
                            ]
                        ] [
                            Level.item [
                                Level.Item.Option.Modifiers [
                                    Modifier.BackgroundColor IsSuccess
                                ]
                            ] [
                                Delete.delete [
                                    Delete.Modifiers [
                                        Modifier.TextColor IsSuccess
                                    ]
                                    Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = false})
                                ] []
                            ]
                            Delete.delete [
                                Delete.Modifiers [
                                    Modifier.TextColor IsSuccess
                                ]
                                Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with WaveModalActive = false})
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                selectWaves wsModel dispatch
            ]
            Modal.Card.foot [] []
        ]
    ]

let selectRamButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramCount = List.length wsModel.RamComponents
    let props, buttonFunc =
        if ramCount > 0 then
            selectRamButtonProps, (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = true})
        else selectRamButtonPropsLight, (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select RAM")

let isRamSelected (ramId: ComponentId) (wsModel: WaveSimModel) : bool =
    Map.containsKey ramId wsModel.SelectedRams

let toggleRamSelection (ramId: ComponentId) (ramLabel: string) (wsModel: WaveSimModel) dispatch =
    let selectedRams =
        if isRamSelected ramId wsModel then
            Map.remove ramId wsModel.SelectedRams
        else
            Map.add ramId ramLabel wsModel.SelectedRams
    dispatch <| InitiateWaveSimulation {wsModel with SelectedRams = selectedRams}

let selectRamModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramRows (ram: Component) : ReactElement =
        tr [] [
            td []
                [ Checkbox.checkbox []
                    [ Checkbox.input [
                        Props (checkboxInputProps @ [
                            Checked <| isRamSelected (ComponentId ram.Id) wsModel
                            OnChange (fun _ -> toggleRamSelection (ComponentId ram.Id) ram.Label wsModel dispatch)
                        ])
                    ] ]
                ]
            td [] [ label [ ramRowStyle ] [ str ram.Label ] ]
        ]

    Modal.modal [
        Modal.IsActive wsModel.RamModalActive
    ] [
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = false})
            ]
        ] []
        Modal.Card.card [] [
            Modal.Card.head [] [
                Modal.Card.title [] [
                    Level.level [] [
                        Level.left [] [ str "Select RAM" ]
                        Level.right [] [
                            Delete.delete [
                                Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = false})
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                str "Select synchronous RAM components to view their contents."
                br []
                str "Note that asynchronous components cannot be viewed in the waveform simulator."
                hr []
                Table.table [] [
                    tbody []
                        (List.map (ramRows) wsModel.RamComponents)
                ]
            ]

            Modal.Card.foot [] []
        ]
    ]

/// Buttons to close waveform simulator, select waves, and select RAMs
let selectBar (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Level.level [ Level.Level.Option.Props [waveSimButtonsBarStyle] ]
        [
            selectWavesButton wsModel dispatch
            selectWavesModal wsModel dispatch
            selectRamButton wsModel dispatch
            selectRamModal wsModel dispatch
        ]

/// Set highlighted clock cycle number
let private setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newClkCycle: int) : unit =
    let newClkCycle = min Constants.maxLastClk newClkCycle |> max 0

    if newClkCycle <= endCycle wsModel then
        if newClkCycle < wsModel.StartCycle then
            printf "StartCycle: %A" newClkCycle
            dispatch <| InitiateWaveSimulation
                {wsModel with 
                    StartCycle = newClkCycle
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
        else
            dispatch <| SetWSModel
                {wsModel with
                    CurrClkCycle = newClkCycle
                    ClkCycleBoxIsEmpty = false
                }
    else
        printf "StartCycle: %A" (newClkCycle - (shownCycles wsModel - 1))
        printf "CurrClkCycle: %A" newClkCycle
        dispatch <| InitiateWaveSimulation
            {wsModel with
                StartCycle = newClkCycle - (shownCycles wsModel - 1)
                CurrClkCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
            }

let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) = 
    let wantedZoomIndex =
        if zoomIn then wsModel.ZoomLevelIndex + 1
        else wsModel.ZoomLevelIndex - 1

    let newIndex =
        Array.tryItem wantedZoomIndex Constants.zoomLevels
        |> function
            | Some zoom -> wantedZoomIndex
            // Index out of range: keep original zoom level
            | None -> wsModel.ZoomLevelIndex

    dispatch <| InitiateWaveSimulation
        { wsModel with
            ZoomLevelIndex = newIndex
        }

/// Click on these buttons to change the number of visible clock cycles.
let zoomButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ clkCycleButtonStyle ]
        [
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> changeZoom wsModel false dispatch)
                zoomOutSVG
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> changeZoom wsModel true dispatch)
                zoomInSVG
        ]

/// Click on these to change the highlighted clock cycle.
let clkCycleButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    /// Controls the number of cycles moved by the "◀◀" and "▶▶" buttons
    let bigStepSize = max 1 (shownCycles wsModel / 2)

    let scrollWaveformsBy (numCycles: int) =
        setClkCycle wsModel dispatch (wsModel.CurrClkCycle + numCycles)

    div [ clkCycleButtonStyle ]
        [
            // Move left by bigStepSize cycles
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> scrollWaveformsBy -bigStepSize)
                (str "◀◀")

            // Move left by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy -1)
                (str "◀")

            // Text input box for manual selection of clock cycle
            Input.number [
                Input.Props clkCycleInputProps

                Input.Value (
                    match wsModel.ClkCycleBoxIsEmpty with
                    | true -> ""
                    | false -> string wsModel.CurrClkCycle
                )
                // TODO: Test more properly with invalid inputs (including negative numbers)
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n ->
                        setClkCycle wsModel dispatch n
                    | false, _ when c.Value = "" ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = true}
                    | _ ->
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = false}
                )
            ]

            // Move right by one cycle
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> scrollWaveformsBy 1)
                (str "▶")

            // Move right by bigStepSize cycles
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> scrollWaveformsBy bigStepSize)
                (str "▶▶")
        ]

/// ReactElement of the tabs for changing displayed radix
let private radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let radixString = [
        Bin,  "Bin"
        Hex,  "Hex"
        Dec,  "uDec"
        SDec, "sDec"
    ]

    let radixTab (radix, radixStr) =
        Tabs.tab [
            Tabs.Tab.IsActive(wsModel.Radix = radix)
            Tabs.Tab.Props radixTabProps
        ] [ a [
            radixTabAStyle
            OnClick(fun _ -> dispatch <| InitiateWaveSimulation {wsModel with Radix = radix})
            ] [ str radixStr ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] (List.map (radixTab) radixString)

/// Buttons to change radix, change zoom, and change clock cycle
let paramsBar (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Level.level [ Level.Level.Option.Props [waveSimButtonsBarStyle] ]
        [   
            // Level.left [] []
            // Level.right [] [
            radixButtons wsModel dispatch
            clkCycleButtons wsModel dispatch
            zoomButtons wsModel dispatch
        ]
    // ]


// /// change the order of the waveforms in the simulator
// let private moveWave (model:Model) (wSMod: WaveSimModel) up =
//     let moveBy = if up then -1.5 else 1.5
//     let addLastPort arr p =
//         Array.mapi (fun i el -> if i <> Array.length arr - 1 then el
//                                 else fst el, Array.append (snd el) [| p |]) arr
//     let svgCache = wSMod.DispWaveSVGCache
//     let movedNames =
//         wSMod.SimParams.DispNames
//         |> Array.map (fun name -> isWaveSelected model wSMod.AllWaves[name], name)
//         |> Array.fold (fun (arr, prevSel) (sel,p) -> 
//             match sel, prevSel with 
//             | true, true -> addLastPort arr p, sel
//             | s, _ -> Array.append arr [| s, [|p|] |], s ) ([||], false)
//         |> fst
//         |> Array.mapi (fun i (sel, ports) -> if sel
//                                                then float i + moveBy, ports
//                                                else float i, ports)
//         |> Array.sortBy fst
//         |> Array.collect snd 
//     setDispNames movedNames wSMod
//     |> SetWSModel

// let moveWave (wsModel: WaveSimModel) (direction: bool) (dispatch: Msg -> unit) : unit =
//     ()

/// Create label of waveform name for each selected wave.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to valueRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
let nameRows (wsModel: WaveSimModel) : ReactElement list =
    selectedWaves wsModel
    |> List.map (fun wave -> label [ labelStyle ] [ str wave.DisplayName ])

/// Create column of waveform names
let namesColumn wsModel : ReactElement =
    let rows = nameRows wsModel

    div [ namesColumnStyle ]
        (List.concat [ topRow; rows ])

/// Create label of waveform value for each selected wave at a given clk cycle.
/// Note that this is generated after calling selectedWaves.
/// Any changes to this function must also be made to nameRows
/// and waveRows, as the order of the waves matters here. This is
/// because the wave viewer is comprised of three columns of many
/// rows, rather than many rows of three columns.
let valueRows (wsModel: WaveSimModel) = 
    selectedWaves wsModel
    |> List.map (getWaveValue wsModel.CurrClkCycle)
    |> List.map (valToString wsModel.Radix)
    |> List.map (fun value -> label [ labelStyle ] [ str value ])

/// Create column of waveform values
let private valuesColumn wsModel : ReactElement =
    let rows = valueRows wsModel

    div [ valuesColumnStyle ]
        (List.concat [ topRow; rows ])

/// Generate list of `line` objects which are the background clock lines.
/// These need to be wrapped by an SVG canvas.
let backgroundSVG (wsModel: WaveSimModel) : ReactElement list =
    let clkLine x = 
        line [
            clkLineStyle
            X1 x
            Y1 0.0
            X2 x
            Y2 Constants.viewBoxHeight
        ] []
    [ wsModel.StartCycle + 1 .. endCycle wsModel + 1 ] 
    |> List.map (fun x -> clkLine (float x * zoomLevel wsModel))

/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles.
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match (zoomLevel wsModel) with
        | width when width < 0.67 && i % 5 <> 0 -> []
        | _ -> [ text (clkCycleText wsModel i) [str (string i)] ]

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> List.append (backgroundSVG wsModel)
    |> svg (clkCycleNumberRowProps wsModel)

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (wsModel: WaveSimModel) : ReactElement =
    /// Note that this is generated after calling selectedWaves.
    /// Any changes to this function must also be made to nameRows
    /// and valueRows, as the order of the waves matters here. This is
    /// because the wave viewer is comprised of three columns of many
    /// rows, rather than many rows of three columns.
    let waveRows : ReactElement list =
        selectedWaves wsModel
        |> List.map (fun wave ->
            match wave.Polylines with
                | Some polylines ->
                    polylines
                // Maybe this shouldn't fail. Could just return a text element saying no waveform was generated
                | None ->
                    printf "no waveform generated for %A" wave.DisplayName
                    [ div [] [] ]//failwithf "No waveform for selected wave %A" wave.DisplayName
            |> List.append (backgroundSVG wsModel)
            |> svg (waveRowProps wsModel)
        )

    div [ waveformColumnStyle ]
        [
            clkCycleHighlightSVG wsModel (List.length wsModel.SelectedWaves)
            div [ waveRowsStyle wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                    waveRows
                )
        ]

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [ showWaveformsStyle ]
        [
            namesColumn wsModel
            waveformColumn wsModel
            valuesColumn wsModel
        ]

let wsClosedPane (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let startButtonOptions = [
        Button.Color IsSuccess
    ]

    let startButtonAction simData (comps, conns) = fun _ ->
        FastRun.runFastSimulation Constants.maxLastClk simData.FastSim

        let wsSheet = Option.get (getCurrFile model)
        let wsModel = getWSModel model
        let allWaves =
            getWaves simData (comps, conns)
            |> Map.map (generateWaveform wsModel)

        let ramComps =
            List.filter (fun (comp: Component) -> match comp.Type with | RAM1 _ -> true | _ -> false) comps
            |> List.sortBy (fun ram -> ram.Label)

        let selectedWaves = List.filter (fun key -> Map.containsKey key allWaves) wsModel.SelectedWaves
        let wsModel = {
            wsModel with
                State = WSOpen
                AllWaves = allWaves
                SelectedWaves = selectedWaves
                RamComponents = ramComps
                FastSim = simData.FastSim
        }

        dispatch <| SetWSModelAndSheet (wsModel, wsSheet)

    div [ waveSelectionPaneStyle ]
        [
            Heading.h4 [] [ str "Waveform Simulator" ]
            str "Simulate sequential logic using this tab."

            hr []

            match SimulationView.makeSimData model with
            | None ->
                div [ errorMessageStyle ]
                    [ str "Please open a project to use the waveform simulator." ]
            | Some (Error e, _) ->
                div [ errorMessageStyle ]
                    [ SimulationView.viewSimulationError e ]
            | Some (Ok simData, reducedState) ->
                if simData.IsSynchronous then
                    button startButtonOptions (startButtonAction simData reducedState) (str "Start Waveform Simulator")
                else
                    div [ errorMessageStyle ]
                        [ str "The circuit must contain sequential logic (clocked components) in order to use the waveform simulator." ]
        ]

let ramTableRow (wsModel: WaveSimModel) (addr, data): ReactElement =
    tr [] [
        td [] [ str (valToString wsModel.Radix addr) ]
        td [] [ str (valToString wsModel.Radix data) ]
    ]

let ramTable (wsModel: WaveSimModel) ((ramId, ramLabel): ComponentId * string) : ReactElement =
    let state = FastRun.extractFastSimulationState wsModel.FastSim wsModel.CurrClkCycle (ramId, [])
    let memData =
        match state with
        | RamState mem ->
            mem.Data
        | _ -> failwithf "Non memory components should not appear here"
        |> Map.toList

    Level.item [
        Level.Item.Option.Props ramTableLevelProps
        Level.Item.Option.HasTextCentered
    ] [
        Heading.h4 [
            Heading.Option.Props [ centerAlignStyle ]
        ] [ str ramLabel ]
        Table.table [
            Table.IsFullWidth
            Table.IsBordered
        ] [ thead [] [
                tr [] [
                    th [ centerAlignStyle ] [ str "Address"]
                    th [ centerAlignStyle ] [ str "Data"]
                ]
            ]
            tbody []
                (List.map (ramTableRow wsModel) memData)
        ]
        br []
    ]

let ramTables (wsModel: WaveSimModel) : ReactElement =
    let selectedRams = Map.toList wsModel.SelectedRams
    Level.level [ Level.Level.Option.Props ramTablesLevelProps ]
        (List.map (ramTable wsModel) selectedRams)

let wsOpenPane (wsModel: WaveSimModel) dispatch : ReactElement =
    div [ waveSelectionPaneStyle ]
        [
            Level.level [] [
                Level.left [] [
                    Heading.h4 [] [ str "Waveform Simulator" ]
                ]
                Level.right [
                ] [
                    Delete.delete [
                        Delete.Option.Size IsLarge
                        Delete.Option.Modifiers [
                            Modifier.BackgroundColor IsGreyLight
                        ]
                        Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with State = WSClosed})
                    ] []
                ]
            ]
            str "Some instructions here"

            hr []

            selectBar wsModel dispatch
            paramsBar wsModel dispatch
            showWaveforms wsModel dispatch

            hr []

            ramTables wsModel

            hr []

        ]

/// Entry point to the waveform simulator. This function returns a ReactElement showing
/// either the WSClosed Pane or the WSOpen pane. The WSClosed Pane allows the user to
/// start the waveform simulator, or shows an error if there is an error in the circuit.
/// The WSOpenPane allows the user to view and select waveforms to be simulated.
let viewWaveSim (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model
    match wsModel.State with
    | WSClosed ->
        wsClosedPane model dispatch
    | WSOpen ->
        wsOpenPane wsModel dispatch
