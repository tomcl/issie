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
let private connInNLTargets (connId: ConnectionId) (nlTargets: NLTarget list) =
    List.exists (fun nlTarget -> nlTarget.TargetConnId = connId) nlTargets

/// is the given component connected to the NLTarget list
let private compInNLTargets (nlComp: NetListComponent) (nlTargets: NLTarget list) =
    Map.exists (fun _ compNLTargets -> compNLTargets = nlTargets) nlComp.Outputs
    || List.exists (fun nlTarget -> nlTarget.TargetCompId = nlComp.Id) nlTargets

/// is the given NLTarget list selected by the given selection
let private nlTargetsIsSelected (netList: NetList) ((comps, conns): CanvasState) (nlTargets: NLTarget list) =
    List.exists (fun (comp: Component) -> 
        match Map.tryFind (ComponentId comp.Id) netList with
        | Some comp -> compInNLTargets comp nlTargets
        | None -> false ) comps
    || List.exists (fun (conn: Connection) -> 
           connInNLTargets (ConnectionId conn.Id) nlTargets) conns

/// is the given waveform selected by the given selection
let private isNetGroupSelected (netList: NetList) ((comps, conns): CanvasState) (netGroup: NetGroup) =
    Array.append [|netGroup.DriverNet|] netGroup.ConnectedNets
    |> Array.exists (nlTargetsIsSelected netList (comps, conns)) 

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (netGroup: NetGroup) =
    Array.append [|netGroup.DriverNet|] netGroup.ConnectedNets
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

/// get common NLSource of NLTarget list with the same source
let private getCommonNLSource (netList: NetList) (nlTargets: NLTarget list) : NLSource option =
    List.tryPick (fun (nlTarget: NLTarget) -> 
        match Map.tryFind nlTarget.TargetCompId netList with
        | Some comp -> 
            match Map.tryFind nlTarget.InputPort comp.Inputs with
            | Some (Some src) -> Some src
            | _ -> None
        | None -> None ) nlTargets

/// get label without (x:x) part at the end
let private labelNoParenthesis (comp: NetListComponent) = 
    let label = comp.Label
    match Seq.tryFindIndexBack ((=) '(') label with
    | Some i -> label[0 .. i - 1]
    | None -> label

/// get integer from OutputPortNumber
let private getOutputPortNumber opn =
    match opn with
    | OutputPortNumber pn -> pn

/// get NLSource option from ComponentId and InputPortNumber
let private drivingSource (netList: NetList) compId (inputPort: InputPortNumber) : NLSource option =
    netList[compId].Inputs[inputPort]

/// get labels of Output and IOLabel components in nlTargets
let outputAndIOLabels (netList: NetList) (nlTargets: NLTarget list) : string list =
    let nlTargetToLabels labels nlTarget =
        match Map.tryFind nlTarget.TargetCompId netList with
        | Some nlComp ->
            match nlComp.Type with
            | IOLabel | Output _ ->
                List.append labels [netList[nlTarget.TargetCompId].Label]
            | _ -> labels
        | None -> labels
    List.fold nlTargetToLabels [] nlTargets
    |> List.distinct

/// get labels of Output and IOLabel components in netGroup
let getOutputAndIOLabels (netList: NetList) (netGroup: NetGroup) : string list =
    let thing = 
        Array.append [|netGroup.DriverNet|] netGroup.ConnectedNets
        |> Array.toList
        |> List.collect (outputAndIOLabels netList)
    printf "%A" thing

    thing
    |> List.distinct

let composingLabels
        (fastSim: FastSimulation)
        (net: NetList)
        (srcComp: NetListComponent)
        (compLabel: string)
        (outputPort: int)
        (drivingSourceName : InputPortNumber -> WaveLabel)
        : LabelSegment list =

    match srcComp.Type with
    | ROM _ | RAM _ | AsyncROM _ -> 
        failwithf "What? Legacy RAM component types should never occur"
    | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ -> 
        [ { LabName = compLabel; BitLimits = 0, 0 } ] 
    | Input w | Output w | Constant1 (w, _, _) | Constant (w, _) | Viewer w -> 
        [ { LabName = compLabel; BitLimits = w - 1, 0 } ] 
    | Demux2 | Demux4 | Demux8 -> 
        [ { LabName = compLabel + "." + string outputPort; BitLimits = 0, 0 } ]
    | NbitsXor w -> 
        [ { LabName = compLabel; BitLimits = w - 1, 0 } ]
    | NbitsAdder w ->
        match outputPort with
        | 0 -> [ { LabName = compLabel + ".Sum"; BitLimits = w - 1, 0 } ]
        | _ -> [ { LabName = compLabel + ".Cout"; BitLimits = w - 1, 0 } ]
    | DFF | DFFE -> 
        [ { LabName = compLabel + ".Q"; BitLimits = 0, 0 } ]
    | Register w | RegisterE w -> 
        [ { LabName = compLabel + ".Q"; BitLimits = w-1, 0 } ]
    | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem -> 
        [ { LabName = compLabel + ".Dout"; BitLimits = mem.WordWidth - 1, 0 } ]
    | Custom c -> 
        [ { LabName = compLabel + "." + (fst c.OutputLabels[outputPort])
            BitLimits = snd c.OutputLabels[outputPort] - 1, 0 } ]
    | MergeWires -> 
        List.append (drivingSourceName (InputPortNumber 1)).ComposingLabels 
                    (drivingSourceName (InputPortNumber 0)).ComposingLabels
        []
    | SplitWire w ->
        let mostsigBranch (_, b) =
            match outputPort with
            | 0 -> b >= 16 - w
            | 1 -> b < 16 - w
            | _ -> failwith "SplitWire output port number greater than 1"

        let split { LabName = name; BitLimits = msb, lsb } st =
            List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
            |> List.filter mostsigBranch
            |> List.unzip
            |> function
            | [], _ -> None 
            | lst, _ -> Some {
                LabName = name
                BitLimits = List.max lst, List.min lst
            } 

        let updateState { LabName = _ ; BitLimits = msb, lsb } st = st + msb - lsb + 1

        (0, (drivingSourceName (InputPortNumber 0)).ComposingLabels)
        ||> List.mapFold (fun st lstEl -> split lstEl st, updateState lstEl st)
        |> fst
        |> List.choose id
        []
    | BusSelection(w, oLSB) ->
        let filtSelec { LabName = name; BitLimits = msb, lsb } st =
            List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
            |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
            |> List.unzip
            |> function
            | [], _ -> None
            | lst, _ -> Some {
                LabName = name
                BitLimits = List.max lst, List.min lst
            }

        let updateState { LabName = _ ; BitLimits = msb, lsb } st = st + msb - lsb + 1 

        ((drivingSourceName (InputPortNumber 0)).ComposingLabels, 0)
        ||> List.mapFoldBack (fun lstEl st -> filtSelec lstEl st, updateState lstEl st)
        |> fst
        |> List.choose id
        |> List.rev
        []
    | IOLabel -> 
        let drivingComp = fastSim.FIOActive[ComponentLabel srcComp.Label,[]]
        let labelWidth = FastRun.extractFastSimulationWidth fastSim (drivingComp.Id,[]) (OutputPortNumber 0)
        match labelWidth with
        | None ->
            failwithf $"What? Can't find width for IOLabel {net[srcComp.Id].Label}$ "
        | Some width ->
                    
                    [ { LabName = compLabel
                        BitLimits = width - 1, 0 } ]

/// get WaveLabel corresponding to a NLTarget list
let rec private getWaveLabel
    (netList: NetList)
    (compIds: ComponentId Set)
    (simData: SimulationData)
    (netGroup: NetGroup)
    (nlTargets: NLTarget list) : WaveLabel =

    match getCommonNLSource netList nlTargets with
    // nlTargets is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        //TODO check if its ok to comment this?
        if not (Set.contains nlSource.SourceCompId compIds) then
            // printfn "DEBUG: In getWaveLabel, if not \n nlSource = %A \n compIds = %A" nlSource compIds
            // printfn "What? graph, net, netGrp, nltrgtList should all be consistent, compIds is deprecated"
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else
            let srcComp = netList[nlSource.SourceCompId]
            let compLabel = labelNoParenthesis srcComp

            let outputPort = getOutputPortNumber nlSource.OutputPort
            
            let drivingSourceName (inputPort: InputPortNumber) =
                match drivingSource netList nlSource.SourceCompId inputPort with
                | Some nlSource ->
                    netList[nlSource.SourceCompId].Outputs[nlSource.OutputPort]
                    |> getWaveLabel netList compIds simData netGroup
                | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }

            composingLabels simData.FastSim netList srcComp compLabel outputPort drivingSourceName
            |> (fun composingLbls -> {
                    OutputsAndIOLabels = getOutputAndIOLabels netList netGroup
                    ComposingLabels = composingLbls
                })

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let rec private removeSubSeq startC endC (chars: char seq) =
    (chars, (false, []))
    ||> Seq.foldBack (
        fun ch (removing, res) ->
            match removing, ch with
            | true, ch when ch = endC -> false, res
            | true, _ -> true, res
            | false, ch when ch = startC -> true, res
            | false, ch -> false, ch :: res
        )
    |> snd
    |> List.map string
    |> String.concat ""

/// work out a good human readable name for a Netgroup. Normally this is the
/// label of the driver of the NetGroup. Merge, Split, and BusSelection components
/// (as drivers) are removed, replaced by corresponding selectors on busses.
/// Names are tagged with labels or IO connectors. It is easy to change these
/// names to make them more human readable.
/// get the label of a waveform
let getWaveName (compIds: Set<ComponentId>) (sd:SimulationData) (netList: NetList) (netGroup: NetGroup) : string =
    // component name
    // port name
    // wire label?
    // let start = getTimeMs()
    let waveLabel = getWaveLabel netList compIds sd netGroup netGroup.DriverNet

    //printfn "Finding label for %A\n%A\n\n" netGrp.driverComp.Label waveLabel.OutputsAndIOLabels
    let tl =
        match waveLabel.ComposingLabels with
        | [ el ] -> el.LabName + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.LabName + bitLimsString lblSeg.BitLimits + ", "
            List.fold appendName "{" lst 
            |> (fun lbl -> lbl[0 .. String.length lbl - 3] + "}")
        |  _ -> ""

    match waveLabel.OutputsAndIOLabels with
    | [] -> tl
    | hdLbls -> 
        String.concat "" hdLbls
        |> (fun hd ->
            hd + " : " + tl)

    |> removeSubSeq '(' ')'
    // |> instrumentInterval "netGroup2Label" start

let getWaveFromNetGroup 
        (simData: SimulationData)
        (connMap: Map<ConnectionId,ConnectionId array>)
        (compIds: Set<ComponentId>)
        (netList: NetList)
        (netGroup: NetGroup)
        : Wave =

    let fastSim = simData.FastSim

    let netGroupName = getWaveName compIds simData netList netGroup
    let fId, opn = getFastDriver fastSim netGroup.DriverComp netGroup.DriverPort
    let driverConn = netGroup.DriverNet[0].TargetConnId
    let conns =
        Map.tryFind driverConn connMap
        |> Option.defaultValue [||]
    if conns = [||] then
        printfn $"Warning: {netGroupName} has no connections"
    
    // Store first 100 values of waveform
    // TODO: Consider moving the call to this function.
    FastRun.runFastSimulation 500 fastSim
    let waveValues =
        [ 0 .. 500]
        |> List.map (fun i -> FastRun.extractFastSimulationOutput fastSim i fId opn)
    
    printf "%A" netGroupName

    let compName = ""
    let portName = ""
    let typ = Mux2
    
    let id = simData.FastSim.FComps[fId].cId
    {
        WaveId = {Id = id; OutputPort = opn}
        // WaveId = netGroupName // not unique yet - may need to be changed
        Conns = List.ofArray conns
        SheetId = [] // all NetGroups are from top sheet at the moment
        Driver = {DriverId = fId; Port = opn}
        DisplayName = netGroupName
        Width = getFastOutputWidth fastSim.FComps[fId] opn
        WaveValues = waveValues
        Polylines = None
    }

// let getWaveFromFC (fastSim: FastSimulation) (fc: FastComponent) =
//     let viewerName = extractLabel fc.SimComponent.Label
//         // Store first 100 values of waveform
//     // let waveValues =
//         // [ 0 .. 500]
//         // |> List.map (fun i -> FastRun.extractFastSimulationOutput fastSim i fc.fId opn)
//     {
//         // WaveId = viewerName // not unique yet - may need to be changed
//         // WType = ViewerWaveform false
//         Conns = [] // don't use connection nets for Viewer (yet)
//         SheetId = snd fc.fId
//         Driver = {DriverId = fc.fId; Port = OutputPortNumber 0}
//         DisplayName = viewerName
//         Width = getFastOutputWidth fc (OutputPortNumber 0)
//         WaveValues = [] //waveValues
//         Polylines = None
//     }

let getWaves (simData: SimulationData) (reducedState: CanvasState) : Map<DriverT, Wave> =
    let comps, conns = reducedState
    let compIds = comps |> List.map (fun c -> ComponentId c.Id) |> Set

    // let fastSim = simData.FastSim
    // let fastComps = mapValues fastSim.FComps
    // let viewers = 
    //     fastComps
    //     |> Array.filter (fun fc -> match fc.FType with Viewer _ -> true | _ -> false)

    // printf "FastComps:"
    // printf "%A" fastComps

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

    // getWaveLabel (via netGroup2Label) will possibly not generate unique names for
    // each netgroup. Names are defined via waveSimModel.AllPorts which adds to
    // each name an optional unique numeric suffic (.2 etc). These suffixes are
    // stripped from names when they are displayed
    // TODO: make sure suffixes are uniquely defined based on ComponentIds (which will not change)
    // display them in wave windows where needed to disambiguate waveforms.
    // Allports is the single reference throughout simulation of a circuit that associates names with netgroups

    Array.append
        (Array.map (getWaveFromNetGroup simData connMap compIds netList) netGroups)
        [||] // (Array.map (getWaveFromFC fastSim) viewers)
    |> Array.groupBy (fun wave -> wave.Driver)
    |> Array.map (fun (root, specs) -> 
        match specs with 
        | [|_|] as oneSpec -> oneSpec
        | specL -> specL |> Array.mapi (fun i wSpec -> {wSpec with DisplayName = $"{wSpec.DisplayName}!{i}"})
    )
    |> Array.concat
    |> Array.map (fun wave -> wave.Driver, wave)
    |> Map.ofArray

let getName (comp: Component) : string = 
    string comp.Label

let getPorts (comp: Component) : (WaveIndexT * Component) list =
    let opn = List.length comp.OutputPorts
    // printf "opn: %A" opn
    [0 .. opn + 1]
    |> List.map (fun x ->
        {Id = ComponentId comp.Id; OutputPort = OutputPortNumber x}, comp
    )

/// starting with just output ports only: not showing input ports.
let makeWave (fastSim: FastSimulation) (index: WaveIndexT) (comp: Component) : Wave =
    // printf "comp: %A" comp
    printf "wave: %A" comp.Label
    printf "port: %A" index.OutputPort

    let driverId, driverPort = getFastDriverNew fastSim comp index.OutputPort

    let dispName = getName comp

    FastRun.runFastSimulation 500 fastSim
    let waveValues =
        [ 0 .. 500 ]
        |> List.map (fun i -> FastRun.extractFastSimulationOutput fastSim i driverId driverPort)

    {
        WaveId = index
        Conns = []
        SheetId = []
        Driver = {DriverId = driverId; Port = driverPort}
        DisplayName = dispName
        Width =  getFastOutputWidth fastSim.FComps[driverId] driverPort
        WaveValues = waveValues
        Polylines = None
    }

let getWavesNew (simData: SimulationData) (reducedState: CanvasState) : Map<WaveIndexT, Wave> =
    let comps, conns = reducedState
    let compIds = comps |> List.map (fun c -> ComponentId c.Id) |> Set

    let fastSim = simData.FastSim
    let fastComps = Map.values fastSim.FComps |> Seq.toList

    List.map (fun (x: Component) -> printf "%A\n" x.Label) comps
    // |> printf "comps: %A"

    let netList = Helpers.getNetList reducedState

    comps
    |> List.map (getPorts)
    |> List.concat
    |> Map.ofList
    |> Map.map (makeWave fastSim)

// type Wave = {
//     // unique within one simulation run, mostly conserved across runs
//     // WaveId: string
//     // unique within design sheet (SheetId)
//     Conns: ConnectionId list
//     // [] for top-level waveform: path to sheet
//     SheetId: ComponentId list
//     // This is used to key the AllWaves map, since this is guaranteed to be unique.
//     Driver: DriverT
//     DisplayName: string
//     // Number of bits in wave
//     Width: int
//     // Map keyed by clock cycle
//     WaveValues: WireData list
//     // Store SVG cache here maybe?
//     Polylines: ReactElement list option
// }

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: WireData list) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "displayValuesOnWave not implemented"

/// Called when InitiateWaveSimulation msg is dispatched
/// Generates the polyline(s) for a specific waveform
let generateWaveform (wsModel: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    let waveName = wave.DisplayName
    if List.contains index wsModel.SelectedWaves then
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

                [makePolyline fstPoints; makePolyline sndPoints]

        {wave with Polylines = Some polylines}
    else wave

/// TODO: Test if this function actually works.
/// Displays error message if there is a simulation error
let displayErrorMessage error =
    div [ errorMessageStyle ]
        [ SimulationView.viewSimulationError error ]

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
let toggleSelectAll (selected: bool) model dispatch : unit =
    let wsModel = getWSModel model
    let selectedWaves = if selected then Map.keys wsModel.AllWaves |> Seq.toList else []
    dispatch <| InitiateWaveSimulation {wsModel with SelectedWaves = selectedWaves}
    // selectConns model conns dispatch

let selectAll (model: Model) dispatch =
    let wsModel = getWSModel model
    let allWavesSelected = Map.forall (fun driver _ -> isWaveSelected wsModel driver) wsModel.AllWaves
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

let toggleConnsSelect (index: WaveIndexT) (wsModel: WaveSimModel) (dispatch: Msg -> unit) =
    let selectedWaves =
        if List.contains index wsModel.SelectedWaves then
            List.except [index] wsModel.SelectedWaves
        else [index] @ wsModel.SelectedWaves

    let wsModel = {wsModel with SelectedWaves = selectedWaves}
    dispatch <| InitiateWaveSimulation wsModel
    // changeWaveSelection name model waveSimModel dispatch

let checkboxAndNameRow (index: WaveIndexT) (model: Model) (dispatch: Msg -> unit) =
    let wsModel = getWSModel model
    let allWaves = wsModel.AllWaves
    let getColorProp name  =
        if List.contains name wsModel.SelectedWaves then
            boldFontStyle
        else
            Style []

    tr  [ tableRowStyle ]
        [
            td  selectAllCheckboxProps
                [ input
                    (checkboxInputProps @ [
                    Checked <| isWaveSelected wsModel index
                    OnChange(fun _ -> toggleConnsSelect index wsModel dispatch)
                ]) ]
            td  []
                [ label [ getColorProp index ] [ str allWaves[index].DisplayName ] ]
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
/// TODO: Change name to editWaves
let closeWaveSimButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let wsModel = {wsModel with State = WSClosed}
    button 
        [Button.Color IsSuccess; Button.Props [closeWaveSimButtonStyle]]
        (fun _ -> dispatch <| SetWSModel wsModel)
        (str "Close wave simulator")

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
    let bigStepSize = max 1 (wsModel.ShownCycles / 2)

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
    wsModel.SelectedWaves
    |> List.map (fun driver ->
        label [ labelStyle ] [ str wsModel.AllWaves[driver].DisplayName ]
    )

/// Create column of waveform names
let namesColumn wsModel : ReactElement =
    let rows = nameRows wsModel

    div [ namesColumnStyle ]
        (List.concat [ topRow; rows ])

/// Create label of waveform value for each selected wave at a given clk cycle.
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
let showWaveforms (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let wsModel = getWSModel model
    div [ showWaveformsStyle ]
        [
            namesColumn wsModel
            waveformColumn model wsModel dispatch
            valuesColumn wsModel
        ]

let searchBarProps : IHTMLProp list = [
    Style [
    ]
]

let searchBar (model: Model) (dispatch: Msg -> unit) : ReactElement =
    Input.text [
        Input.Props searchBarProps

        Input.Placeholder "Search"
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

    div [ waveSelectionPaneStyle ]
        [
            Heading.h4 [] [ str "Waveform Simulator" ]
            str "Some instructions here"

            hr []

            Level.level []
                [
                    searchBar model dispatch
                ]

            selectWaves model dispatch
        ]

let wsClosedPane (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let startButtonOptions = [
        Button.Color IsSuccess
    ]

    let startButtonAction simData reducedState = fun _ ->
        let wsSheet = Option.get (getCurrFile model)
        let wsModel = getWSModel model
        let allWaves =
            getWavesNew simData reducedState
            |> Map.map (generateWaveform wsModel)
        let wsModel = {
            wsModel with
                State = WSOpen
                AllWaves = allWaves
                OutOfDate = false
                ReducedState = reducedState
        }

        dispatch <| SetWSModelAndSheet (wsModel, wsSheet)

    div [ waveSelectionPaneStyle ]
        [
            Heading.h4 [] [ str "Waveform Simulator" ] 
            str "Some instructions here"

            hr []

            match SimulationView.makeSimData model with
                | None ->
                    div [ errorMessageStyle ]
                        [ str "Please open a project to use the waveform simulator." ]
                | Some (Error e, _) ->
                    displayErrorMessage e
                | Some (Ok simData, reducedState) ->
                    if simData.IsSynchronous then
                        button startButtonOptions (startButtonAction simData reducedState) (str "Start Waveform Simulator")
                    else
                        div [ errorMessageStyle ]
                            [ str "There is no sequential logic in this circuit." ]
        ]

let selectWavesMenu model dispatch : ReactElement =
    details [
        Open true
    ]
        [
            summary [] [
                str "thingy\n"
                Checkbox.checkbox []
                    [ Checkbox.input [] ]
            ]
            str "aosdifjasodif"
        ]


let waveTableRows (wave: Wave) : ReactElement =
    

    // Rows in form of:
    tr []
        [
            td  [ OnChange(fun _ -> printf "pressed" ) ]
                [ Checkbox.checkbox []
                    [ Checkbox.input [] ]
                ]
            td [] [str wave.DisplayName]
            td [] [str wave.DisplayName]
        ]


let selectWavesTable (model: Model) (dispatch: Msg -> unit) : ReactElement =
    let wsModel = getWSModel model
    Table.table [
        Table.IsFullWidth
        Table.IsBordered
    ] [
        thead []
            [
                tr []
                    [
                        th  [ OnChange(fun _ -> printf "pressed" ) ]
                            [ Checkbox.checkbox []
                                [ Checkbox.input [] ]
                            ]
                        th [] [str "Component"]
                        th [] [str "Port"]
                    ]
            ]
        tbody []
            (Map.values wsModel.AllWaves
            |> Seq.map waveTableRows)
    ]

let wsOpenPane (model: Model) dispatch : ReactElement =
    div [ waveSelectionPaneStyle ]
        [
            Heading.h4 [] [ str "Waveform Simulator" ]
            str "Some instructions here"

            hr []

            waveSimButtonsBar model dispatch
            showWaveforms model dispatch

            hr []

            Level.level []
                [
                    searchBar model dispatch
                ]
            // selectWavesMenu model dispatch
            // selectWavesMenu model dispatch


            // selectWavesTable model dispatch

            selectWaves model dispatch
        ]

/// Entry point to the waveform simulator. This function returns a ReactElement showing
/// either the Wave Selection Pane or the Wave Viewer Pane. The Wave Selection Pane
/// allows the user to select which waveforms they would like to view, and the Wave
/// Viewer Pane displays these selected waveforms, along with their names and values.
let viewWaveSim (model: Model) dispatch : ReactElement =
    let wsModel = getWSModel model
    match wsModel.State with
    | WSClosed ->
        wsClosedPane model dispatch
    | WSOpen ->
        wsOpenPane model dispatch

