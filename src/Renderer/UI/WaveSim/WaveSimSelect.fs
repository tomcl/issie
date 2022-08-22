module WaveSimSelect

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes


let cap (sheet:string) = sheet.ToUpper()

let copySheetsToWaveSim (model: Model) (wsModel: WaveSimModel) =

    let getDirectDependencies (cv:CanvasState) =
        fst cv
        |> List.collect (fun comp -> match comp.Type with | Custom ct-> [cap comp.Label, cap ct.Name] | _ -> [])

    let reducedSheets = 
        mapOverProject [] model (fun project -> project.LoadedComponents)
        |> List.map (fun ldc -> cap ldc.Name,  Extractor.extractReducedState ldc.CanvasState)
        |> Map.ofList 

    let topSheet = mapOverProject "" model  (fun project -> project.OpenFileName)

    let childrenOf (sheet:string) =
        reducedSheets[cap sheet]
        |> getDirectDependencies

    let rec sheetsNeeded (sheet:string): string list =
        let children = childrenOf sheet |> List.map snd
        children
        |> List.map sheetsNeeded
        |> List.concat
        |> List.append children
        |> List.append [sheet]
        |> List.distinct

    let waveSheets = sheetsNeeded topSheet
    reducedSheets
    |> Map.filter (fun k v -> List.contains k waveSheets)
    |> (fun sheetMap -> {wsModel with Sheets = sheetMap; TopSheet = cap topSheet})


let checkIfCurrent (wsModel:WaveSimModel) (model:Model) =
    match model.CurrentProj with
    | None -> false
    | Some project ->
        cap project.OpenFileName = wsModel.TopSheet &&
        wsModel.Sheets
        |> Map.map (fun name state ->
            project.LoadedComponents
            |> List.filter (fun ldc -> cap ldc.Name = name && Extractor.extractReducedState ldc.CanvasState = state )
            |> (<>) [])
        |> Map.values
        |> Seq.forall id
    


/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

/// Get port names for waves that are from Input ports.
/// Appended to comp.Label
let getInputPortName (compType: ComponentType) (port: InputPortNumber) : string =
    let muxPortName (size: int) : string =
        if port = (InputPortNumber size) then ".SEL"
        else "." + string port

    match compType with
    | Not | BusCompare _ ->
        ".IN"
    | And | Or | Xor | Nand | Nor | Xnor |NbitsNot _ |NbitSpreader _ ->
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

    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ ->
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
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | IOLabel -> failwithf "IOLabel should not occur in getInputPortName"
    | MergeWires -> failwithf "MergeWires should not occur in getInputPortName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getInputPortName"
    | BusSelection _ -> failwithf "BusSelection should not occur in getInputPortName"

/// Get names for waves that are from Input ports
let getInputName (comp: FastComponent) (port: InputPortNumber) : string =
    let portName : string = getInputPortName comp.FType port
    let bitLims : string =
        match comp.FType with
        | Not | BusCompare _ | And | Or | Xor | Nand | Nor | Xnor
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | Register _ | DFFE | RegisterE _ |NbitSpreader _ ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsNot w | NbitsAnd w | NbitsAdder w | NbitsOr w  ->
            bitLimsString (w - 1, 0)

        // TODO: Find the right parameters for RAMs and ROMs.
        | ROM1 _ | AsyncROM1 _ | RAM1 _ | AsyncRAM1 _ ->
            ""

        | Custom c ->
            bitLimsString (snd c.InputLabels[getInputPortNumber port] - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | IOLabel -> failwithf "IOLabel should not occur in getInputName"
        | MergeWires -> failwithf "MergeWires should not occur in getInputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getInputName"
        | BusSelection _ -> failwithf "BusSeleciton should not occur in getInputName"

    comp.FLabel + portName + bitLims

/// Get port names for waves that are from Output ports
/// Appended to comp.Label
let getOutputPortName (compType: ComponentType) (port: OutputPortNumber) : string =
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ | NbitsXor _ | NbitsNot _  | NbitSpreader _ | NbitsAnd _ | NbitsOr _ ->
        ".OUT"
    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ | IOLabel ->
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
    | Input _ -> failwithf "Legacy Input component types should never occur"
    | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
    | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
    | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

/// Get names for waves that are from Output ports
let getOutputName (comp: FastComponent) (port: OutputPortNumber) (fastSim: FastSimulation): string =
    let portName = getOutputPortName comp.FType port
    let bitLims =
        match comp.FType with
        | Not | And | Or | Xor | Nand | Nor | Xnor  | BusCompare _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsAnd w | NbitsOr w | NbitsNot w | NbitSpreader w | NbitsAdder w | Register w | RegisterE w ->
            bitLimsString (w - 1, 0)

        | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem ->
            bitLimsString (mem.WordWidth - 1, 0)

        | Custom c ->
            bitLimsString (snd c.OutputLabels[getOutputPortNumber port] - 1, 0)

        | IOLabel ->
            //printfn $"IOLabel name {comp.FLabel}"
            let drivingComp = fastSim.FIOActive[ComponentLabel comp.FLabel,snd comp.fId]
            //printfn "driving compm done"
            let labelWidth = FastRun.extractFastSimulationWidth fastSim (drivingComp.Id,snd drivingComp.fId) (OutputPortNumber 0)
            //printfn "label width fdone"
            match labelWidth with
            | None ->
                failwithf $"What? Can't find width for IOLabel {comp.FLabel}$ "
            | Some width ->
                bitLimsString (width - 1, 0)

        | ROM _ | RAM _ | AsyncROM _ -> failwithf "What? Legacy RAM component types should never occur"
        | Input _ -> failwithf "Legacy Input component types should never occur"
        | MergeWires -> failwithf "MergeWires should not occur in getOutputName"
        | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
        | BusSelection _ -> failwithf "BusSelection should not occur in getOutputName"

    comp.FLabel + portName + bitLims

/// Get name for a wave. Names are generated from component label, port name, and bit width of wave.
let getName (index: WaveIndexT) (fastSim: FastSimulation) : string =
    let fc = fastSim.WaveComps[index.Id]
    match index.PortType with
    | PortType.Input -> getInputName fc (InputPortNumber index.PortNumber)
    | PortType.Output -> getOutputName fc (OutputPortNumber index.PortNumber) fastSim

/// Make Wave for each component and port on sheet
let makeWave (fastSim: FastSimulation) (wi: WaveIndexT) : Wave =
    let fc = fastSim.WaveComps[wi.Id]
    //printfn $"Making wave for {fc.FullName}, portType={wi.PortType}, portNumber={wi.PortNumber}"
    let driver = 
        match fastSim.Drivers[wi.SimArrayIndex] with
        | Some d -> d
        | None ->
            printfn "What? No driver!"
            failwithf $"Can't find simulation waveform driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}]"
    if driver.DriverWidth = 0 then 
        printfn $"Warning! 0 width driver for {fc.FullName}.{wi.PortType}[{wi.PortNumber}]"
 
    {
        WaveId = wi
        DisplayName = getName wi fastSim
        CompLabel = fc.FLabel
        Width = driver.DriverWidth
        WaveValues = driver.DriverData.Step
        SheetId = []
        Conns = []
        SVG = None
    }





/// Get all simulatable waves from CanvasState. Includes all Input and Output ports.
let getWaves (simData: SimulationData) : Map<WaveIndexT, Wave> =
    let start = TimeHelpers.getTimeMs ()
    let fs = simData.FastSim
    printfn $"{fs.WaveIndex.Length} possible waves"
    fs.WaveIndex
    |> TimeHelpers.instrumentInterval "getAllPorts" start
    |> Array.map (fun wi -> wi, makeWave fs wi)
    //|> fun x -> printfn $"Made waves!";x
    |> Map.ofArray
    |> TimeHelpers.instrumentInterval "makeWavePipeline" start




/// Sets all waves as selected or not selected depending on value of selected
let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) dispatch : unit =
    let start = TimeHelpers.getTimeMs ()
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    //printf "length: %A" (List.length selectedWaves)
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}
    |> TimeHelpers.instrumentInterval "toggleSelectAll" start

/// Row in wave selection table that selects all values in wsModel.AllWaves
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

/// Toggle selection for a single wave.
let toggleWaveSelection (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    printfn $"toggling {index}"
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else [index] @ wsModel.SelectedWaves
    let wsModel = {wsModel with SelectedWaves = selectedWaves}
    dispatch <| InitiateWaveSimulation wsModel

/// Toggle selection of a list of waves.
let toggleSelectSubGroup (wsModel: WaveSimModel) dispatch (selected: bool) (waves: WaveIndexT list) =
    let selectedWaves =
        if selected then
            List.append wsModel.SelectedWaves waves
        else
            List.except waves wsModel.SelectedWaves
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}

/// Table row of a checkbox and name of a wave.
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
            td  [ noBorderStyle ]
                [ str wsModel.AllWaves[index].DisplayName ]
        ]


/// Search bar to allow users to filter out waves by DisplayName
let searchBar (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    Input.text [
        Input.Option.Props [
            Style [
                MarginBottom "1rem"
            ]
        ]
        Input.Option.Placeholder "Search"
        Input.Option.OnChange (fun c ->
            dispatch <| SetWSModel {wsModel with SearchString = c.Value.ToUpper()}
        )
    ]

/// Group of rows for a subgroup of waves. Grouped by ComponentGroups. Waves are hidden in a details element.
let rec labelRows (subSheet: string list) (compGroup: ComponentGroup) (waves: Wave list) (wsModel: WaveSimModel) dispatch : ReactElement =
    let indices = List.map (fun x -> x.WaveId) waves
    let subGroupSelected = List.forall (fun index -> isWaveSelected wsModel index) indices
    let subSheets = []

    tr summaryProps [
        th [] [
            Checkbox.checkbox [] [
                Checkbox.input [
                    Props [
                        Checked subGroupSelected
                        OnChange (fun _ -> toggleSelectSubGroup wsModel dispatch (not subGroupSelected) indices)
                    ]
                ]
            ]
        ]
        th [] [
            details
                detailsProps
                [   summary
                        summaryProps
                        [ summaryName compGroup ]
                    Table.table [] [
                        tbody [] (
                            List.map (checkboxRow wsModel dispatch) subSheets @
                            List.map (checkboxRow wsModel dispatch) indices)
                    ]
                ]
        ]
    ]



/// Table of selectable waves. Waves are grouped by their component type.
and  selectWaves (wsModel: WaveSimModel) (subSheet: string list) (dispatch: Msg -> unit) : ReactElement =
    let fs = wsModel.FastSim
   
    let selectionRows : ReactElement list =
        Map.values wsModel.AllWaves |> Seq.toList
        |> List.filter (fun x -> x.DisplayName.ToUpper().Contains(wsModel.SearchString))
        |> List.sortBy (fun wave -> wave.DisplayName)
        |> List.groupBy (fun wave ->
            match fs.WaveComps[wave.WaveId.Id].FType with
            | Input1 _ | Output _ | Constant1 _ ->
                InputOutput
            | IOLabel ->
                WireLabel
            | Viewer _ ->
                Viewers
            | Not | And | Or | Xor | Nand | Nor | Xnor ->
                Gates
            | BusCompare _ ->
                Buses
            | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4
                // MuxDemux
            | NbitsAdder _ | NbitsXor _ | NbitsAnd _ | NbitsOr _ | NbitsNot _ | NbitSpreader _
                // Arithmetic
            | Custom _
                // CustomComp
            | DFF | DFFE | Register _ | RegisterE _
                // FFRegister
            | AsyncROM1 _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ ->
                // Memories
                Component wave.CompLabel
            | BusSelection _ | MergeWires | SplitWire _ ->
                failwithf "Bus select, MergeWires, SplitWire should not appear"
            | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
                failwithf "Legacy component types should not appear"
        )
        |> List.map (fun (compGroup, waves) ->
            labelRows [] compGroup waves wsModel dispatch
        )

    Table.table [
        Table.IsBordered
        Table.IsFullWidth
        Table.Props [
            Style [BorderWidth 0]
        ]
    ] [ thead []
            ( [selectAll wsModel dispatch] @
                selectionRows
            )
    ]

/// Button to activate wave selection modal
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

/// Modal that, when active, allows users to select waves to be viewed.
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
                        Level.left [] [ str "Select Waves" ]
                        Level.right [
                        ] [ Delete.delete [
                                Delete.Option.Size IsMedium
                                Delete.Option.OnClick (fun _ ->
                                    dispatch <| SetWSModel
                                        {wsModel with
                                            WaveModalActive = false
                                            SearchString = ""
                                        }
                                )
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                searchBar wsModel dispatch
                selectWaves wsModel [] dispatch
            ]
            Modal.Card.foot [] []
        ]
    ]

//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//
//-------------------------------------RAM Selection from Wave Simulator----------------------------------//
//--------------------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------------------//

/// Button to activate RAM selection modal.
let selectRamButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramCount = List.length wsModel.RamComps
    let props, buttonFunc =
        if ramCount > 0 then
            selectRamButtonProps, (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = true})
        else selectRamButtonPropsLight, (fun _ -> ())
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
    dispatch <| SetWSModel {wsModel with SelectedRams = selectedRams}

/// Modal that, when active, allows users to select RAMs to view their contents.
let selectRamModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let ramRows (ram: FastComponent) : ReactElement =
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
                                Delete.Option.Size IsMedium
                                Delete.Option.OnClick (fun _ -> dispatch <| SetWSModel {wsModel with RamModalActive = false})
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [] [
                str "Select synchronous RAM components to view their contents. "
                str "Note that asynchronous RAM components cannot be viewed in the waveform simulator. "
                br []
                br []
                str "On a write, the corresponding row will be highlighted in red. "
                str "On a read, the corresponding row will be highlighted in blue. "
                str "Any memory address which has not been initialised with a value will not be shown in the table. "
                hr []
                Table.table [] [
                    tbody []
                        (List.map (ramRows) wsModel.RamComps)
                ]
            ]

            Modal.Card.foot [] []
        ]
    ]


