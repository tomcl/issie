module WaveSimSelect

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes


let cap (sheet:string) = sheet.ToUpper()


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

    | Input1 _ | Output _ | Constant1 _ | Constant _ | Viewer _ |CounterNoEnableLoad _ ->
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
        | InputPortNumber 0 -> ".Cin"
        | InputPortNumber 1 -> ".P"
        | _ -> ".Q"

    | NbitsAdderNoCin _ |NbitsAdderNoCinCout _ ->
        match port with
        | InputPortNumber 0 -> ".P"
        | _ -> ".Q"

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
    | SplitWire _ -> failwithf "SplitWire should not occur in getInputPortName"
    | BusSelection _ -> failwithf "BusSelection should not occur in getInputPortName"

/// Get names for waves that are from Input ports
let getInputName (withComp: bool) (comp: FastComponent) (port: InputPortNumber) : string =
    let portName : string = getInputPortName comp.FType port
    let bitLims : string =
        match comp.FType with
        | Not | BusCompare _ | And | Or | Xor | Nand | Nor | Xnor
        | Mux2 | Mux4 | Mux8 | Decode4 | Demux2 | Demux4 | Demux8
        | DFF | Register _ | DFFE | RegisterE _ |Counter _
        |CounterNoEnable _ |CounterNoLoad _ |CounterNoEnableLoad _|NbitSpreader _ ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsNot w | NbitsAnd w | NbitsAdder w | NbitsOr w  
        | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w->
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

    if withComp then 
        comp.FLabel + portName + bitLims
    else 
        portName[1..portName.Length-1] + bitLims

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
    | SplitWire _ -> failwithf "SplitWire should not occur in getOutputName"
    | BusSelection _ -> failwithf "BusSeleciton should not occur in getOutputName"

/// Get names for waves that are from Output ports
let getOutputName (withComp: bool) (comp: FastComponent) (port: OutputPortNumber) (fastSim: FastSimulation): string =
    let portName = getOutputPortName comp.FType port
    let bitLims =
        match comp.FType with
        | Not | And | Or | Xor | Nand | Nor | Xnor  | BusCompare _
        | Decode4 | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | DFF | DFFE ->
            bitLimsString (0, 0)

        | Input1 (w, _) | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w
        | NbitsXor w | NbitsAnd w | NbitsOr w | NbitsNot w | NbitSpreader w | NbitsAdder w | Register w | RegisterE w 
        | NbitsAdderNoCin w | NbitsAdderNoCout w | NbitsAdderNoCinCout w | Counter w |CounterNoEnable w |CounterNoLoad w |CounterNoEnableLoad w->
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

let nameWithSheet (fastSim: FastSimulation) (dispName: string) (waveIndex:WaveIndexT) =
    let fc = fastSim.WaveComps[waveIndex.Id]
    match fc.SubSheet with
    | [] -> fastSim.SimulatedTopSheet + "." + dispName
    | [sheet] -> sheet + "." + dispName      
    | path ->  camelCaseDottedWords(path[path.Length - 2]) + "." + dispName

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
        CycleWidth = singleWaveWidth ws
        Radix = ws.Radix
        SubSheet = fc.SubSheet
        DisplayName = dispName
        ViewerDisplayName = nameWithSheet fastSim dispName wi
        CompLabel = fc.FLabel
        PortLabel = portLabel
        Width = driver.DriverWidth
        WaveValues = driver.DriverData
        SheetId = []
        Conns = []
        SVG = None
    }





/// Get all simulatable waves from CanvasState. Includes top-level Input and Output ports.
/// Waves contain info which will be used later to create the SVGs for those waves actually
/// selected. Init value of these from this function is None.
let getWaves (ws: WaveSimModel) (fs: FastSimulation) : Map<WaveIndexT, Wave> =
    let start = TimeHelpers.getTimeMs ()
    printfn $"{fs.WaveIndex.Length} possible waves"
    fs.WaveIndex
    |> TimeHelpers.instrumentInterval "getAllPorts" start
    |> Array.map (fun wi -> wi, makeWave ws fs wi)
    //|> fun x -> printfn $"Made waves!";x
    |> Map.ofArray
    |> TimeHelpers.instrumentInterval "makeWavePipeline" start




/// Sets all waves as selected or not selected depending on value of selected
let toggleSelectAll (selected: bool) (wsModel: WaveSimModel) dispatch : unit =
    let start = TimeHelpers.getTimeMs ()
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    //printf "length: %A" (List.length selectedWaves)
    dispatch <| GenerateWaveforms {wsModel with SelectedWaves = selectedWaves}
    |> TimeHelpers.instrumentInterval "toggleSelectAll" start

/// Row in wave selection table that selects all values in wsModel.AllWaves
let selectAll (wsModel: WaveSimModel) dispatch =
    let allWavesSelected = Map.forall (fun index _ -> isWaveSelected wsModel index) wsModel.AllWaves

    tr (summaryProps false (SheetItem []) wsModel dispatch) [
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
    dispatch <| GenerateWaveforms wsModel




    

/// Toggle selection of a list of waves.
let toggleSelectSubGroup (wsModel: WaveSimModel) dispatch (selected: bool) (waves: WaveIndexT list) =
    let comps = wsModel.FastSim.WaveComps
    let selectedWaves =
        if selected then
            let wavesWithMinDepth =
                if waves = [] then [] else
                    waves
                    |> List.groupBy (fun waves -> comps[waves.Id].AccessPath.Length)
                    |> List.sort
                    |> List.head
                    |> snd

            List.append wsModel.SelectedWaves wavesWithMinDepth

        else
            List.except waves wsModel.SelectedWaves
    dispatch <| GenerateWaveforms {wsModel with SelectedWaves = selectedWaves}


/// Table row of a checkbox and name of a wave.
let checkboxRow (wsModel: WaveSimModel) dispatch (index: WaveIndexT) =
    let fontStyle = if isWaveSelected wsModel index then boldFontStyle else normalFontStyle
    let wave = wsModel.AllWaves[index]
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
                [ str wave.DisplayName ]
        ]

let infoButton  : ReactElement =
    div 
        [
            HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline} {Tooltip.IsInfo} {Tooltip.IsTooltipRight}"
            Tooltip.dataTooltip Constants.infoMessage
            Style [FontSize "25px"; MarginTop "0px"; MarginLeft "10px"; Float FloatOptions.Left]] 
        [str Constants.infoSignUnicode]

/// Search bar to allow users to filter out waves by DisplayName
/// some special cases '-', '*' collapse or expand (for selected waves)
/// the wave select window.
let searchBar (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    div [] [
        Input.text [
            Input.Option.Props [
                Style [
                    MarginBottom "1rem"
                    Width "30%"
                    Float FloatOptions.Left
                ]
            ]
            Input.Option.Placeholder "Viewer Name"
            Input.Option.OnChange (fun c ->
                dispatch <| UpdateWSModel (fun ws -> {wsModel with SearchString = c.Value.ToUpper()})
            )
        ]
        infoButton
        label [Style [Float FloatOptions.Right; FontSize "24px"]]  [str $"{wsModel.SelectedWaves.Length} waves selected."]
    ]

/// Implemements a checkbox, with toggle state stored in WaveSimModel under ShowDetailMap
/// using  waveIds as key.
let checkBoxItem  wsModel isChecked waveIds  dispatch =
    Checkbox.checkbox [] [
        Checkbox.input [
            Props [
                Checked isChecked
                OnChange (fun _ -> toggleSelectSubGroup 
                                        wsModel 
                                        dispatch 
                                        (not isChecked)
                                        waveIds)
            ]
        ]
    ]

/// Implemements a checkbox, with toggle state determined by SelectedWaves.
let waveCheckBoxItem  (wsModel:WaveSimModel) (waveIds:WaveIndexT list)  dispatch =
    let comps = wsModel.FastSim.WaveComps
    let minDepthSelectedWaves =
        if waveIds = [] then [] else
            waveIds
            |> List.groupBy (fun waveId -> comps[waveId.Id].AccessPath.Length)
            |> List.sort
            |> List.head
            |> snd
    let checkBoxState =
        List.exists (fun w -> List.contains w wsModel.SelectedWaves) minDepthSelectedWaves
    Checkbox.checkbox [] [
        Checkbox.input [
            Props [
                Checked checkBoxState
                OnChange (fun _ -> toggleSelectSubGroup                                         
                                        wsModel 
                                        dispatch
                                        (not checkBoxState)
                                        waveIds)                                         
            ]
        ]
    ]

/// implements one row (with one port)
let makePortRow (ws: WaveSimModel) (dispatch: Msg -> Unit) (waves: Wave list)  =
    let wave = 
        match waves with 
        | [waves] -> waves 
        | _ -> failwithf "What? {waves.Length} waves passed to portRow"
    let subSheet =
        match wave.SubSheet with
        | [] -> str ws.FastSim.SimulatedTopSheet
        | _  -> subSheetsToNameReact wave.SubSheet

    tr [] [
        td [] [waveCheckBoxItem ws  [wave.WaveId] dispatch]
        td [] [str wave.PortLabel]
        //td [] [str <| match (fst wave.WaveId.Id) with ComponentId s -> s[0..5] ]
        td [] [str <| match wave.WaveId.PortType with | PortType.Output -> "Output" | PortType.Input -> "Input"]
        ]

/// returns a tr react element representing a thing with a checkbox with summary name and details beneath
let makeSelectionGroup 
        (showDetails:bool)
        (ws: WaveSimModel) 
        (dispatch: Msg -> Unit)
        (summaryItem: ReactElement) 
        (rowItems: ReactElement list) 
        (cBox: CheckBoxStyle) 
        (waves: Wave list)  =
    let wi = wavesToIds waves
    tr
        (summaryProps false cBox ws dispatch) [
            th [] [
                waveCheckBoxItem ws  wi  dispatch
            ]
            th [] [
                details
                    (detailsProps showDetails cBox ws dispatch)
                    [   
                        summary
                            (summaryProps true cBox ws dispatch)
                            [ summaryItem ]
                    
                        Table.table [] [  tbody [] rowItems                             
                    ]
                ]
            ]
        ]
    


/// Returns a tr react element representing a component with ports detailed beneath
let rec makeComponentRow showDetails (ws: WaveSimModel) (dispatch: Msg->Unit) (fc: FastComponent) (waves: Wave list)  =
    let cBox = ComponentItem fc
    let summaryReact = summaryName ws cBox fc.SubSheet waves
    let rows = 
        waves
        |> List.map (fun wave -> makePortRow ws dispatch [wave])
    makeSelectionGroup showDetails ws dispatch summaryReact rows cBox waves    
   
/// Returns a tr react element representing a component with ports detailed beneath
let rec makeComponentGroup showDetails (ws: WaveSimModel) (dispatch: Msg->Unit) (subSheet: string list) (cGroup: ComponentGroup) (waves: Wave list)  =
    let compWaves = 
        waves
        |> List.groupBy (fun wave -> wave.WaveId.Id)
    if compWaves.Length = 1 then
        let fc = ws.FastSim.WaveComps[(List.head waves).WaveId.Id]
        makeComponentRow showDetails ws dispatch fc waves
    else
        let cBox = GroupItem (cGroup,subSheet)
        let summaryReact = summaryName ws cBox subSheet waves
        let compRows =
            compWaves
            |> List.map (fun (fId,compWaves) -> makeComponentRow showDetails ws dispatch ws.FastSim.WaveComps[fId] compWaves)

        makeSelectionGroup showDetails ws dispatch summaryReact compRows cBox waves  

let rec makeSheetRow  (showDetails: bool) (ws: WaveSimModel) (dispatch: Msg -> Unit) (subSheet: string list) (waves: Wave list) =  
    let cBox = SheetItem subSheet
    let fs = ws.FastSim
    let wavesBySheet = 
        waves
        |> List.groupBy (fun w -> List.truncate (subSheet.Length + 1) w.SubSheet)

    let componentRows = 
        wavesBySheet
        |> List.filter (fun (g,wLst) -> g = subSheet)
        |> List.collect snd
        |> List.groupBy (fun wave -> getCompGroup fs wave) 
        |> List.map (fun (grp,groupWaves) -> makeComponentGroup showDetails ws dispatch subSheet grp groupWaves)

    let subSheetRows =
        wavesBySheet
        |> List.filter (fun (g,_) -> g <> subSheet)
        |> List.map (fun (subSheet',waves') ->  makeSheetRow showDetails ws dispatch subSheet' waves')
    
    let rows = List.append subSheetRows componentRows
    if subSheet = [] then
        Table.table [
            Table.IsBordered
            Table.IsFullWidth
            Table.Props [
                Style [BorderWidth 0]
            ]] [tbody [] rows]
    else
        makeSelectionGroup showDetails ws dispatch (summaryName ws cBox subSheet waves ) rows cBox waves








let  selectWaves (ws: WaveSimModel) (subSheet: string list) (dispatch: Msg -> unit) : ReactElement =

    if not ws.WaveModalActive then div [] []
    else
        let fs = ws.FastSim
        let searchText = ws.SearchString
        let waves = Map.values ws.AllWaves |> Seq.toList
        let wavesToDisplay =
            match searchText with
            | "-" when ws.ShowSheetDetail.Count <> 0 || ws.ShowComponentDetail.Count <> 0  || ws.ShowGroupDetail.Count <> 0 ->
                dispatch <|SetWaveSheetSelectionOpen (ws.ShowSheetDetail |> Set.toList,false)            
                dispatch <| SetWaveGroupSelectionOpen (ws.ShowGroupDetail |> Set.toList,false)
                dispatch <| SetWaveComponentSelectionOpen (ws.ShowComponentDetail |> Set.toList,false)
                []
            | "" | "-" ->
                waves
            | "*" ->
                ws.SelectedWaves
                |> List.map (fun wi -> ws.AllWaves[wi])                       
            | _ ->
                List.filter (fun x -> x.ViewerDisplayName.ToUpper().Contains(searchText)) waves
        let showDetails = ((wavesToDisplay.Length < 10) || searchText.Length > 0) && searchText <> "-"
        wavesToDisplay
        |> makeSheetRow showDetails ws dispatch []




/// Button to activate wave selection modal
let selectWavesButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let waveCount = Map.count wsModel.AllWaves
    let props, buttonFunc =
        if waveCount > 0 && wsModel.State=Success then
            selectWavesButtonProps, (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with WaveModalActive = true}))
        else selectWavesButtonPropsLight, (fun _ -> ())
    button 
        props
        buttonFunc
        (str "Select Waves")

/// Modal that, when active, allows users to select waves to be viewed.
let selectWavesModal (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let endModal _ = 
        dispatch <| UpdateWSModel (fun ws ->
            {wsModel with
                WaveModalActive = false
                SearchString = ""
            })
    Modal.modal [
        Modal.IsActive wsModel.WaveModalActive
    ] [
        Modal.background [
            Props [
                OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with WaveModalActive = false}))
            ]
        ] []
        Modal.Card.card [Props [Style [MinWidth "900px"]]] [
            Modal.Card.head [] [
                Modal.Card.title [] [
                    Level.level [] [
                        Level.left [] [ str "Select Waves" ]
                        Level.right [
                        ] [ Delete.delete [
                                Delete.Option.Size IsMedium
                                Delete.Option.OnClick (
                                    fun _ ->
                                        let numWaves = wsModel.SelectedWaves.Length
                                        if numWaves > 20 then
                                            PopupView.viewWaveSelectConfirmationPopup 
                                                numWaves
                                                (fun finish _ -> 
                                                        dispatch ClosePopup
                                                        match finish with | true -> endModal() | false -> ()) 
                                                dispatch
                                        else
                                            endModal())
                                    
                                    
                                
                            ] []
                        ]
                    ]
                ]
            ]
            Modal.Card.body [Props [Style [OverflowY OverflowOptions.Visible]]] [   
                searchBar wsModel dispatch
                selectWaves wsModel [] dispatch
            ]
            Modal.Card.foot [Props [Style [Display DisplayOptions.InlineBlock; Float FloatOptions.Right]]]
                [
                    Button.button [
                        Button.OnClick endModal; 
                        Button.Color IsSuccess; 
                        Button.Props [Style [Display DisplayOptions.InlineBlock; Float FloatOptions.Right]]
                        ] [str "Done"]
                ]
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
        if ramCount > 0 && wsModel.State=Success then
            selectRamButtonProps, (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = true}))
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
    dispatch <| UpdateWSModel (fun ws -> {wsModel with SelectedRams = selectedRams})

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
                OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = false}))
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
                                Delete.Option.OnClick (fun _ -> dispatch <| UpdateWSModel (fun ws -> {wsModel with RamModalActive = false}))
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


