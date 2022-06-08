module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open DiagramStyle
open WaveSimStyle
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open Sheet.SheetInterface

// TODO: Move all Style definitions into Style.fs
// TODO: Combine Style definitions into same variables where possible

// TODO: Originally located in WaveSimHelpers.fs. Not really sure where this should go.
// TODO: Refactor these functions, then move the relevant ones to WaveSimHelpers

//------------------------------------//
//    Interaction with Model.Diagram  //
//------------------------------------//

/// is the given connection in the given NLTarget list
let private isConnInNLTrgtLst (connId: ConnectionId) (nlTrgtLst: NLTarget list) =
    List.exists (fun nlTrgt -> nlTrgt.TargetConnId = connId) nlTrgtLst

/// is the given component connected to the NLTarget list
let private isCompInNLTrgtLst (nlComponent: NetListComponent) (nlTrgtLst: NLTarget list) =
    Map.exists (fun _ compNLTrgtLst -> compNLTrgtLst = nlTrgtLst) nlComponent.Outputs
    || List.exists (fun nlTrgt -> nlTrgt.TargetCompId = nlComponent.Id) nlTrgtLst

/// is the given NLTarget list selected by the given selection
let private isNLTrgtLstSelected (netList: NetList) ((comps, conns): CanvasState) (nlTrgtLst: NLTarget list) =
    List.exists (fun (comp: Component) -> 
        match Map.tryFind (ComponentId comp.Id) netList with
        | Some comp -> isCompInNLTrgtLst comp nlTrgtLst
        | None -> false ) comps
    || List.exists (fun (conn: Connection) -> 
           isConnInNLTrgtLst (ConnectionId conn.Id) nlTrgtLst) conns

/// is the given waveform selected by the given selection
let private isNetGroupSelected (netList: NetList) ((comps, conns): CanvasState) (trgtLstGroup: NetGroup) =
    Array.append [|trgtLstGroup.driverNet|] trgtLstGroup.connectedNets
    |> Array.exists (isNLTrgtLstSelected netList (comps, conns)) 

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.collect (fun net -> 
        List.toArray net 
        |> Array.map (fun net -> net.TargetConnId))

///Takes a connection and model, and returns the netgroup as a list of connectionIds associated with that connection
let getNetSelection (canvas : CanvasState) (model : Model) =
    let netList = 
        model.LastSimulatedCanvasState
        |> Option.map Helpers.getNetList 
        |> Option.defaultValue (Map.empty)

    let netGroups = makeAllNetGroups netList

    let selectedConnectionIds (ng:NetGroup) =
        if isNetGroupSelected netList canvas ng then 
            wave2ConnIds ng
        else [||]
            
    Array.collect selectedConnectionIds netGroups
    |> Array.toList

/// get common NLSource of list of NLTarget with the same source
let private nlTrgtLst2CommonNLSource (netList: NetList) (nlTrgtLst: NLTarget list) : NLSource option =
    List.tryPick (fun (nlTrgt: NLTarget) -> 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some comp -> 
            match Map.tryFind nlTrgt.InputPort comp.Inputs with
            | Some (Some src) -> Some src
            | _ -> None
        | None -> None ) nlTrgtLst

/// get label without (x:x) part at the end
let private labelNoParenthesis (netList: NetList) compId = 
    let lbl = netList[compId].Label
    match Seq.tryFindIndexBack ((=) '(') lbl with
    | Some i -> lbl[0..i - 1]
    | None -> lbl

/// get integer from OutputPortInt
let private outPortInt2int outPortInt =
    match outPortInt with
    | OutputPortNumber pn -> pn

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList[compId].Inputs[inPortN]

/// get labels of Output and IOLabel components in nlTargetList
let net2outputsAndIOLabels (netList: NetList) (netLst: NLTarget list) =
    let nlTrgt2Lbls st nlTrgt = 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some nlComp -> match nlComp.Type with
                         | IOLabel | Output _ -> List.append st [netList[nlTrgt.TargetCompId].Label]
                         | _ -> st
        | None -> st
    List.fold nlTrgt2Lbls [] netLst
    |> List.distinct

/// get labels of Output and IOLabel components in TargetListGroup
let netGroup2outputsAndIOLabels netList (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.toList
    |> List.collect (net2outputsAndIOLabels netList)
    |> List.distinct

/// get WaveLabel corresponding to a NLTarget list
let rec private findName (compIds: ComponentId Set) (sd: SimulationData) (net: NetList) netGrp nlTrgtList =
    let graph = sd.Graph
    let fs = sd.FastSim
    match nlTrgtLst2CommonNLSource net nlTrgtList with
    //nlTrgtLst is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        //TODO check if its ok to comment this?
        if not (Set.contains nlSource.SourceCompId compIds) then
            // printfn "DEBUG: In findname, if not \n nlSource = %A \n compIds = %A" nlSource compIds
            // printfn "What? graph, net, netGrp, nltrgtList should all be consistent, compIds is deprecated"
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else   
            let compLbl = labelNoParenthesis net nlSource.SourceCompId
            let outPortInt = outPortInt2int nlSource.OutputPort
            let drivingOutputName inPortN =
                match drivingOutput net nlSource.SourceCompId inPortN with
                | Some nlSource' ->
                    net[nlSource'.SourceCompId].Outputs[nlSource'.OutputPort]
                    |> findName compIds sd net netGrp
                | None ->  { OutputsAndIOLabels = []; ComposingLabels = [] } 
            let srcComp = net[nlSource.SourceCompId]
            match net[nlSource.SourceCompId].Type with
            | ROM _ | RAM _ | AsyncROM _ -> 
                    failwithf "What? Legacy RAM component types should never occur"
            | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ -> 
                [ { LabName = compLbl; BitLimits = 0, 0 } ] 
            | Input w | Output w | Constant1(w, _,_) | Constant(w,_) | Viewer w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ] 
            | Demux2 | Demux4 | Demux8 -> 
                [ { LabName = compLbl + "." + string outPortInt; BitLimits = 0, 0 } ]
            | NbitsXor w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ]
            | NbitsAdder w ->
                match outPortInt with
                | 0 -> [ { LabName = compLbl + ".Sum"; BitLimits = w - 1, 0 } ]
                | _ -> [ { LabName = compLbl + ".Cout"; BitLimits = w - 1, 0 } ]
            | DFF | DFFE -> 
                [ { LabName = compLbl + ".Q"; BitLimits = 0, 0 } ]
            | Register w | RegisterE w -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = w-1, 0 } ]
            | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = mem.WordWidth - 1, 0 } ]
            | Custom c -> 
                [ { LabName = compLbl + "." + (fst c.OutputLabels[outPortInt])
                    BitLimits = snd c.OutputLabels[outPortInt] - 1, 0 } ]
            | MergeWires -> 
                List.append (drivingOutputName (InputPortNumber 1)).ComposingLabels 
                            (drivingOutputName (InputPortNumber 0)).ComposingLabels
            | SplitWire w ->
                let mostsigBranch (_, b) =
                    match outPortInt with
                    | 0 -> b >= 16 - w
                    | 1 -> b < 16 - w
                    | _ -> failwith "SplitWire output port number greater than 1"

                let split { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                    |> List.filter mostsigBranch
                    |> List.unzip
                    |> function
                    | [], _ -> None 
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1

                (0, (drivingOutputName (InputPortNumber 0)).ComposingLabels)
                ||> List.mapFold (fun st lstEl -> split lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
            | BusSelection(w, oLSB) ->
                let filtSelec { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
                    |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
                    |> List.unzip
                    |> function
                    | [], _ -> None
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1 

                ((drivingOutputName (InputPortNumber 0)).ComposingLabels, 0)
                ||> List.mapFoldBack (fun lstEl st -> filtSelec lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
                |> List.rev
            | IOLabel -> 
                let drivingComp = fs.FIOActive[ComponentLabel srcComp.Label,[]]
                let ioLblWidth = FastRun.extractFastSimulationWidth fs (drivingComp.Id,[]) (OutputPortNumber 0)
                match ioLblWidth with
                | None ->
                    failwithf $"What? Can't find width for IOLabel {net[srcComp.Id].Label}$ "
                | Some width ->
                            
                            [ { LabName = compLbl
                                BitLimits = width - 1, 0 } ]

            |> (fun composingLbls -> { OutputsAndIOLabels = netGroup2outputsAndIOLabels net netGrp
                                       ComposingLabels = composingLbls })

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let rec private removeSubSeq startC endC chars =
    ((false,[]), chars)
    ||> Seq.fold (fun (removing,res) ch ->
                    match removing,ch with
                    | true, ch when ch = endC -> false,res
                    | true, _ -> true, res
                    | false, ch when ch = startC -> true,res
                    |false, ch -> false, ch :: res)
    |> snd
    |> List.rev
    |> List.map string
    |> String.concat ""

/// truncate names to remove redundant width specs
let private simplifyName name =
    name
    |> removeSubSeq '(' ')'

/// get the label of a waveform
let netGroup2Label compIds (sd:SimulationData) netList (netGrp: NetGroup) =
    // let start = getTimeMs()
    let waveLabel = findName compIds sd netList netGrp netGrp.driverNet
    //printfn "Finding label for %A\n%A\n\n" netGrp.driverComp.Label waveLabel.OutputsAndIOLabels
    let tl =
        match waveLabel.ComposingLabels with
        | [ el ] -> el.LabName + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.LabName + bitLimsString lblSeg.BitLimits + ", "
            List.fold appendName "{" lst 
            |> (fun lbl -> lbl[0..String.length lbl - 3] + "}")
        |  _ -> ""
    let appendName st name = st + name + ", "
    match waveLabel.OutputsAndIOLabels with
    | [] -> tl
    | hdLbls -> 
        List.fold appendName "" hdLbls
        |> (fun hd -> hd[0..String.length hd - 3] + " : " + tl)
    |> simplifyName
    // |> instrumentInterval "netGroup2Label" start

let getWaveFromNetGroup 
        (fs: FastSimulation)
        (connMap: Map<ConnectionId,ConnectionId array>)
        (nameOf: NetGroup -> string) 
        (netGroup: NetGroup) : Wave =
    let netGroupName = nameOf netGroup
    let fId, opn = getFastDriver fs netGroup.driverComp netGroup.driverPort
    let driverConn = netGroup.driverNet[0].TargetConnId
    let conns =
        Map.tryFind driverConn connMap
        |> Option.defaultValue [||]
    if conns = [||] then
        printfn $"Warning: {netGroupName} has no connections"
    // Store first 100 values of waveform
    // TODO: Consider moving the call to this function.
    FastRun.runFastSimulation 500 fs
    let waveValues =
        [ 0 .. 500]
        |> List.map (fun i -> FastRun.extractFastSimulationOutput fs i fId opn)
    {
        WaveId = netGroupName // not unique yet - may need to be changed
        Selected = true
        Conns = List.ofArray conns
        SheetId = [] // all NetGroups are from top sheet at the moment
        Driver = fId, opn
        DisplayName = netGroupName
        Width = getFastOutputWidth fs.FComps[fId] opn
        WaveValues = waveValues
        Polylines = None
    }

let getWaveFromFC (fc: FastComponent) =
    let viewerName = extractLabel fc.SimComponent.Label
        // Store first 100 values of waveform
    // let waveValues =
    //     [ 0 .. 100]
    //     |> List.map (fun i -> FastRun.extractFastSimulationOutput fs i fc.fId opn)
    {
        WaveId = viewerName // not unique yet - may need to be changed
        Selected = true
        // WType = ViewerWaveform false
        Conns = [] // don't use connection nets for Viewer (yet)
        SheetId = snd fc.fId
        Driver = fc.fId, OutputPortNumber 0
        DisplayName = viewerName
        Width = getFastOutputWidth fc (OutputPortNumber 0)
        WaveValues = []//waveValues
        Polylines = None
    }

let getWaveforms
        (netGroupToLabel: Set<ComponentId> -> SimulationData -> NetList -> NetGroup -> string) 
        (simData: SimulationData) 
        (reducedState: CanvasState) =
    let comps, conns = reducedState
    // let compIds = comps |> List.map (fun comp -> comp.Id)
    // let comps, conns = model.Sheet.GetCanvasState ()
    let compIds = comps |> List.map (fun c -> ComponentId c.Id) |> Set

    let fastSim = simData.FastSim
    let fastComps = mapValues fastSim.FComps
    let viewers = 
        fastComps
        |> Array.filter (fun fc -> match fc.FType with Viewer _ -> true | _ -> false)

    /// NetList is a simplified version of circuit with connections and layout
    /// info removed. Component ports are connected directly. ConnectionIds are
    /// preserved so we can reference connections on diagram
    let netList = Helpers.getNetList reducedState
    /// Netgroups are connected Nets: note the iolabel components can connect
    /// together multiple nets on the schematic into a single NetGroup.
    /// Wave simulation allows every distinct NetGroup to be named and displayed
    let netGroups = makeAllNetGroups netList
    /// connMap maps each connection to the set of connected connections within the same sheet
    let connMap = makeConnectionMap netGroups
    /// work out a good human readable name for a Netgroup. Normally this is the
    /// label of the driver of the NetGroup. Merge, Split, and BusSelection components
    /// (as drivers) are removed, replaced by corresponding selectors on busses.
    /// Names are tagged with labels or IO connectors. It is easy to change these
    /// names to make them more human readable.
    let nameOf ng  = netGroupToLabel compIds simData netList ng

    // findName (via netGroup2Label) will possibly not generate unique names for
    // each netgroup. Names are defined via waveSimModel.AllPorts which adds to
    // each name an optional unique numeric suffic (.2 etc). These suffixes are
    // stripped from names when they are displayed
    // TODO: make sure suffixes are uniquely defined based on ComponentIds (which will not change)
    // display them in wave windows where needed to disambiguate waveforms.
    // Allports is the single reference throughout simulation of a circuit that associates names with netgroups

    Array.append
        (Array.map (getWaveFromNetGroup fastSim connMap nameOf) netGroups)
        [||]// (Array.map getWaveFromFC viewers)
    |> Array.groupBy (fun wave -> wave.WaveId)
    |> Array.map (fun (root, specs) -> 
        match specs with 
        | [|_|] as oneSpec -> oneSpec
        | specL -> specL |> Array.mapi (fun i wSpec -> {wSpec with WaveId = $"{wSpec.WaveId}!{i}"}))
    |> Array.concat
    |> Array.map (fun wave -> wave.WaveId, wave)
    |> Map.ofArray

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: WireData list) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "displayValuesOnWave not implemented"

/// Called when InitiateWaveSimulation msg is dispatched
/// Generates the polyline(s) for a specific waveform
let generateWave (wsModel: WaveSimModel) (waveName: string) (wave: Wave): Wave =
    if wave.Selected then
        printf "generating wave for %A" waveName
        let polylines =
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            | 1 ->
                let transitions = calculateBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let wavePoints =
                    List.mapi (binaryWavePoints wsModel.ZoomLevel 0) transitions 
                    |> List.concat
                    |> List.distinct
                printf "wavePoints: %A" wavePoints
                [ polyline (wavePolylineStyle wavePoints) [] ]
            | _ ->
                let transitions = calculateNonBinaryTransitions wave.WaveValues
                /// TODO: Fix this so that it does not generate all 500 points.
                /// Currently takes in 0, but this should ideally only generate the points that
                /// are shown on screen, rather than all 500 cycles.
                let fstPoints, sndPoints =
                    List.mapi (nonBinaryWavePoints wsModel.ZoomLevel 0) transitions 
                    |> List.unzip
                let makePolyline points = 
                    let points =
                        points
                        |> List.concat
                        |> List.distinct
                    polyline (wavePolylineStyle points) []

                printf "fstPoints: %A" fstPoints
                printf "sndPoints: %A" sndPoints


                [makePolyline fstPoints; makePolyline sndPoints]

        {wave with Polylines = Some polylines}
    else wave

let generateAllLabels waves =
    failwithf "generateAllLabels not implemented"

/// TODO: Test if this function actually works.
/// Displays error message if there is a simulation error
let displayErrorMessage error =
    div [ errorMessageStyle ]
        [ SimulationView.viewSimulationError error ]

/// Upon clicking the View buttonm, the wave selection pane will change to the wave viewer pane.
let viewWaveformsButton model dispatch =
    let wsModel = getWSModel model

    let viewButtonOptions, viewButtonAction =
        match Map.count (selectedWaves wsModel) with
        | 0 -> viewButtonLight, (fun _ -> ())
        | _ -> viewButtonProps, (fun _ ->
                let wsMod' = {wsModel with State = Running}
                let msgs = [
                    StartUICmd ViewWaveSim
                    ClosePropertiesNotification
                    InitiateWaveSimulation wsMod'
                ]
                dispatch (Sheet (SheetT.SetSpinner true))
                dispatch <| SendSeqMsgAsynch msgs
                dispatch FinishUICmd
            )

    div [ Style [ Display DisplayOptions.Block ] ]
        [ button viewButtonOptions viewButtonAction (str "View") ]

// let selectConns (model: Model)  (conns: ConnectionId list) (dispatch: Msg -> unit) =
//     let allConns =
//         snd <| model.Sheet.GetCanvasState()
//         |> List.map (fun conn -> ConnectionId conn.Id)
//         |> Set.ofList
//     let otherConns = allConns - Set.ofList conns
//     let sheetDispatch sMsg = dispatch (Sheet sMsg)
//     model.Sheet.SelectConnections sheetDispatch true conns
//     model.Sheet.SelectConnections sheetDispatch false (Set.toList otherConns)

/// Sets all waves as selected or not selected depending on value of newState
let toggleSelectAll newState model dispatch : unit =
    let wsModel = getWSModel model
    let allWaves' = Map.map (fun _ wave -> {wave with Selected = newState}) wsModel.AllWaves
    dispatch <| SetWSModel {wsModel with AllWaves = allWaves'}
    // selectConns model conns dispatch

let selectAll (model: Model) dispatch =
    let wsModel = getWSModel model
    let allWavesSelected = Map.forall (fun name _ -> isWaveSelected wsModel name) wsModel.AllWaves
    tr  [ tableRowStyle ]
        [
            td  selectAllCheckboxProps
                [ input
                    (checkboxInputProps @ [
                    Checked allWavesSelected
                    OnChange(fun _ -> toggleSelectAll (not allWavesSelected) model dispatch )
                ]) ]
            td  [ boldFontStyle ] [ str "Select All" ]
        ]

let toggleConnsSelect (name: string) (waveSimModel: WaveSimModel) (dispatch: Msg -> unit) =
    // let newState = not (isWaveSelected waveSimModel name)
    printf "toggleConnsSelect"
    let wave = waveSimModel.AllWaves[name]
    let wave' = {wave with Selected = not wave.Selected}
    // printf "%A" wave'

    let waveSimModel' = {waveSimModel with AllWaves = Map.add name wave' waveSimModel.AllWaves}
    // printf "%A" waveSimModel'.AllWaves[name]
    dispatch <| SetWSModel waveSimModel'

    // changeWaveSelection name model waveSimModel dispatch

let checkboxAndNameRow (name: string) (model: Model) (dispatch: Msg -> unit) =
    let wsModel = getWSModel model
    let allWaves = wsModel.AllWaves
    let getColorProp name  =
        if Map.containsKey name (selectedWaves wsModel) then
            boldFontStyle
        else
            Style []

    tr  [ tableRowStyle ]
        [
            td  selectAllCheckboxProps
                [ input
                    (checkboxInputProps @ [
                    Checked <| isWaveSelected wsModel name
                    OnChange(fun _ -> toggleConnsSelect name wsModel dispatch)
                ]) ]
            td  []
                [ label [ getColorProp name ] [ str allWaves[name].DisplayName ] ]
        ]

let checkboxListOfWaves (model: Model) (dispatch: Msg -> unit) =
    let wsModel = getWSModel model
    Map.keys wsModel.AllWaves |> Seq.toList
    |> List.map (fun name -> checkboxAndNameRow name model dispatch)

let selectWaves (model: Model) dispatch =
    div [ selectWavesStyle ]
        [ table []
            [ tbody []
                ([ selectAll model dispatch ] @
                (checkboxListOfWaves model dispatch)
                )
            ]
        ]

let closeWaveSimButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let wsModel = {wsModel with State = NotRunning}
    button 
        [Button.Color IsSuccess; Button.Props [closeWaveSimButtonStyle]]
        (fun _ -> dispatch <| SetWSModel wsModel)
        (str "Edit waves / close simulator")

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
        printf "StartCycle: %A" (newClkCycle - (wsModel.ShownCycles - 1))
        printf "CurrClkCycle: %A" newClkCycle
        dispatch <| InitiateWaveSimulation
            {wsModel with
                StartCycle = newClkCycle - (wsModel.ShownCycles - 1)
                CurrClkCycle = newClkCycle
                ClkCycleBoxIsEmpty = false
            }

let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) = 
    let wantedZoomIndex =
        if zoomIn then wsModel.ZoomLevelIndex + 1
        else wsModel.ZoomLevelIndex - 1

    let newIndex, newZoom =
        Array.tryItem wantedZoomIndex Constants.zoomLevels
        |> function
            | Some zoom -> wantedZoomIndex, zoom
            // Index out of range: keep original zoom level
            | None -> wsModel.ZoomLevelIndex, wsModel.ZoomLevel

    let shownCycles = int <| float wsModel.WaveformColumnWidth / (newZoom * 30.0)

    dispatch <| InitiateWaveSimulation
        {wsModel with
            ZoomLevel = newZoom
            ZoomLevelIndex = newIndex
            ShownCycles = shownCycles
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
    let bigStepSize = wsModel.ShownCycles / 2

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
            OnClick(fun _ -> dispatch <| SetWSModel {wsModel with Radix = radix})
            ] [ str radixStr ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] (List.map (radixTab) radixString)

/// Display closeWaveSimButton, zoomButtons, radixButtons, clkCycleButtons
let waveSimButtonsBar (model: Model) (dispatch: Msg -> unit) : ReactElement = 
    let wsModel = getWSModel model
    div [ waveSimButtonsBarStyle ]
        [
            closeWaveSimButton wsModel dispatch
            zoomButtons wsModel dispatch
            radixButtons wsModel dispatch
            clkCycleButtons wsModel dispatch
        ]

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

/// Create label of waveform name for each selected wave
let nameRows (wsModel: WaveSimModel) : ReactElement list =
    selectedWaves wsModel
    |> Map.keys |> Seq.toList
    |> List.map (fun l -> label [ labelStyle ] [ str l ])

/// Create column of waveform names
let namesColumn wsModel : ReactElement =
    let rows = nameRows wsModel

    div [ namesColumnStyle ]
        (List.concat [ topRow; rows ])

/// Create label of waveform value for each selected wave at a given clk cycle.
let valueRows (wsModel: WaveSimModel) = 
    selectedWaves wsModel
    |> Map.values |> Seq.toList
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
    |> List.map (fun x -> clkLine (float x * wsModel.ZoomLevel))

/// Generate a row of numbers in the waveforms column.
/// Numbers correspond to clock cycles.
let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match wsModel.ZoomLevel with
        | width when width < 0.67 && i % 5 <> 0 -> []
        | _ -> [ text (clkCycleText wsModel i) [str (string i)] ]

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> List.append (backgroundSVG wsModel)
    |> svg (clkCycleNumberRowProps wsModel)

/// Generate a column of waveforms corresponding to selected waves.
let waveformColumn (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement =
    let waveRows : ReactElement list =
        selectedWaves wsModel
        |> Map.values |> Seq.toList
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
            clkCycleHighlightSVG wsModel (selectedWavesCount wsModel)
            div [ waveRowsStyle wsModel.WaveformColumnWidth]
                ([ clkCycleNumberRow wsModel ] @
                waveRows
                )
        ]

/// Display the names, waveforms, and values of selected waveforms
let showWaveforms (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let wsModel = getWSModel model
    div [ showWaveformsStyle ]
        [
            namesColumn wsModel
            waveformColumn model wsModel dispatch
            valuesColumn wsModel
        ]

/// View selected waveforms: show buttons to control simulation parameters, and
/// display selected waveforms with their names and values.
let waveViewerPane (model: Model) (dispatch: Msg -> unit) : ReactElement =
    div [ waveViewerPaneStyle ]
        [
            waveSimButtonsBar model dispatch
            showWaveforms model dispatch
        ]

/// TODO: Improve this comment. Update the required inputs.
/// This function needs to show a list of what waveforms can be displayed, as well as a
/// check box list showing which ones are selectable. Should have a 'select all' box
/// available as well.
let waveSelectionPane (model: Model) dispatch : ReactElement = 
    /// Generate popup over waveeditor screen if there are undriven input connections
    let inputWarningPopup (simData:SimulatorTypes.SimulationData) dispatch =
        if simData.Inputs <> [] then
            let inputs = 
                simData.Inputs
                |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                |> String.concat ","
            let popup = Notifications.warningPropsNotification (sprintf "Inputs (%s) will be set to 0." inputs)
            dispatch <| SetPropertiesNotification popup

    // TODO: Improve the str labels here. Currently out-of-date.
    div [ waveSelectionPaneStyle ]
        [
            Heading.h4 [] [ str "Waveform Simulator" ] 
            // str "Ctrl-click on diagram connections or use tick boxes below to add or remove waveforms."
            // str "Test combinational logic by closing this simulator and using Simulate tab."
            hr []
            div [] [
                    viewWaveformsButton model dispatch
                    selectWaves model dispatch
                ]
        ]

/// Entry point to the waveform simulator. This function returns a ReactElement showing
/// either the Wave Selection Pane or the Wave Viewer Pane. The Wave Selection Pane
/// allows the user to select which waveforms they would like to view, and the Wave
/// Viewer Pane displays these selected waveforms, along with their names and values.
let viewWaveSim (model: Model) dispatch : ReactElement =
    match SimulationView.makeSimData model with
        | None -> failwithf "simRes has value None"
        | Some (Ok simData, reducedState) ->
            // TODO: Add a check to see if there is synchronous data
            // let isClocked = SynchronousUtils.hasSynchronousComponents simData.Graph
            let wsModel = getWSModel model
            match wsModel.State with
            // Open waveform adder
            | NotRunning ->
                waveSelectionPane model dispatch
            // Open waveform viewer
            | Running ->
                waveViewerPane model dispatch
        | Some (Error e, _) ->
            displayErrorMessage e
