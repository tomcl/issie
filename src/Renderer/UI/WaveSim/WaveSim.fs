module WaveSim

open Fulma
open Fable.React
open Fable.React.Props

open Browser.Types

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

/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne

/// Determines whether a non-binary waveform changes value at the beginning and end of
/// that clock cycle.
type NonBinaryTransition =
    | Change
    | Const

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

let namesCol = Browser.Dom.document.getElementById "namesColumn"
let wavesCol = Browser.Dom.document.getElementById "wavesColumn"
let valuesCol = Browser.Dom.document.getElementById "valuesColumn"
// TODO: See if these need to be reset to None after closing wave sim
let mutable namesColWidth: float option = None
let mutable valuesColWidth: float option = None
let mutable firstRender: bool = true

let endCycle wsModel = wsModel.StartCycle + wsModel.ShownCycles - 1

let singleWaveWidth wsModel = 30.0 * wsModel.ZoomLevel

/// TODO: Make a Constants module
/// TODO: Tweak these values
let nonBinaryTransLen : float = 0.2
/// Height of a waveform
let waveHeight : float = 0.8
let spacing : float = (viewBoxHeight - waveHeight) / 2.

/// TODO: Remove this limit. This stops the waveform simulator moving past 500 clock cycles.
let maxLastClk = 500

let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]

let selectedWaves (wsModel: WaveSimModel) : Map<string, Wave> = Map.filter (fun _ key -> key.Selected) wsModel.AllWaves

// TODO: Originally located in WaveSimHelpers.fs. Not really sure where this should go.

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
        SVG = None
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
        SVG = None
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

let getWaveValue (currClkCycle: int) (wave: Wave): int64 =
    List.tryItem currClkCycle wave.WaveValues
    |> function
    | Some wireData ->
        convertWireDataToInt wireData
    | None ->
        // TODO: Find better default value here
        // TODO: Should probably make it so that you can't call this function in the first place.
        printf "Trying to access index %A in wave %A. Default to 0." currClkCycle wave.DisplayName
        0

let binaryWavePoints (clkCycleWidth: float) (clkCycle: int) (transition: BinaryTransition)  : XYPos list * int =
    let xLeft = float clkCycle * clkCycleWidth
    let xRight = float (clkCycle + 1) * clkCycleWidth
    let yTop = spacing
    let yBot = waveHeight + spacing
    let topL = {X = xLeft; Y = yTop}
    let topR = {X = xRight; Y = yTop}
    let botL = {X = xLeft; Y = yBot}
    let botR = {X = xRight; Y = yBot}
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero ->
        [botL; botR], clkCycle + 1
    | ZeroToOne ->
        [botL; topL; topR], clkCycle + 1
    | OneToZero ->
        [topL; botL; botR], clkCycle + 1
    | OneToOne ->
        [topL; topR], clkCycle + 1

/// TODO: Account for very low zoom levels.
/// TODO: Consider: If singleWaveWidth M nonBinaryTransLen, then show crosshatch.
/// Generates points for non-binary waveforms. Note the left shift by nonBinaryTransLen
let nonBinaryWavePoints (clkCycleWidth: float) (clkCycle: int) (transition: NonBinaryTransition) : (XYPos list * XYPos list) * int =
    // Use start coord to know where to start the polyline
    let xLeft = float clkCycle * clkCycleWidth - nonBinaryTransLen
    let xRight = float (clkCycle + 1) * clkCycleWidth  - nonBinaryTransLen
    let yTop = spacing
    let yBot = waveHeight + spacing
    let topL = {X = xLeft; Y = yTop}
    let topR = {X = xRight; Y = yTop}
    let botL = {X = xLeft; Y = yBot}
    let botR = {X = xRight; Y = yBot}

    let crossHatchMid = {X = xLeft + nonBinaryTransLen; Y = 0.5}

    let crossHatchTop = {X = xLeft + nonBinaryTransLen * 2.; Y = yTop}
    let crossHatchBot = {X = xLeft + nonBinaryTransLen * 2.; Y = yBot}

    match transition with
    // This needs to account for different zoom levels:
    // Can probably just look at screen size and zoom level
    // And then scale the horizontal part accordingly
    // When zoomed out sufficiently and values changing fast enough,
    // The horizontal part will have length zero.
    // May need to account for odd/even clock cycle
    | Change ->
        let topStart = [topL; crossHatchMid; crossHatchTop; topR]
        let botStart = [botL; crossHatchMid; crossHatchBot; botR]
        (topStart, botStart), clkCycle + 1
    | Const ->
        ([topL; topR], [botL; botR]), clkCycle + 1

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: WireData list) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "displayValuesOnWave not implemented"

let determineBinaryTransitions waveValues =
    let firstValue = List.head waveValues
    (firstValue, waveValues)
    ||> List.mapFold
        (fun prev value ->
            match prev, value with
            | [Zero], [Zero] -> ZeroToZero, value
            | [Zero], [One] -> ZeroToOne, value
            | [One], [Zero] -> OneToZero, value
            | [One], [One] -> OneToOne, value
            | _ ->
                failwithf "Unrecognised transition"
        )
    |> fst

let determineNonBinaryTransitions waveValues =
    // Use [] so that first clock cycle always starts with Change
    ([], waveValues)
    ||> List.mapFold
        (fun prev value ->
            if prev = value then
                Const, value
            else
                Change, value
        )
    |> fst

/// Called when InitiateWaveSimulation msg is dispatched
/// Generates the polyline(s) for a specific waveform
let generateWave (wsModel: WaveSimModel) (waveName: string) (wave: Wave): Wave =
    printf "generating wave for %A" waveName

    if wave.Selected then
        let waveLine = 
            match wave.Width with
            | 0 -> failwithf "Cannot have wave of width 0"
            | 1 ->
                let transitions = determineBinaryTransitions wave.WaveValues
                let wavePoints =
                    List.mapFold (binaryWavePoints wsModel.ZoomLevel) wsModel.StartCycle transitions 
                    |> fst
                    |> List.concat
                    |> List.distinct

                // printf "%A: %A" wave.DisplayName wavePoints

                [ polyline (wavePolylineStyle wavePoints) [] ]
            | _ -> 
                let transitions = determineNonBinaryTransitions wave.WaveValues
                let fstPoints, sndPoints =
                    List.mapFold (nonBinaryWavePoints wsModel.ZoomLevel) wsModel.StartCycle transitions 
                    |> fst
                    |> List.unzip
                    
                let fstWave = List.concat fstPoints |> List.distinct
                let sndWave = List.concat sndPoints |> List.distinct

                printf "%A" fstWave
                printf "%A" sndWave

                let fstLine = polyline (wavePolylineStyle fstWave) []
                let sndLine = polyline (wavePolylineStyle sndWave) []

                [fstLine; sndLine]

        let waveform = waveLine

        {wave with SVG = Some waveform}
    else
        wave

let generateAllLabels waves =
    failwithf "generateAllLabels not implemented"

/// TODO: Test if this function actually works.
let displayErrorMessage error =
    div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
        [
            SimulationView.viewSimulationError error
        ]

/// Upon clicking the View buttonm, the wave selection pane will change to the wave viewer pane.
let viewWaveformsButton model dispatch =
    printf "update view butotn"
    let wsModel = model.WaveSim
    let viewButtonAction =
        match Map.count (selectedWaves model.WaveSim) with
        | 0 -> [ Button.IsLight; Button.Color IsSuccess ]
        | _ ->
            [
                Button.Color IsSuccess
                // Button.IsLoading (showSimulationLoading wSModel dispatch)
                Button.OnClick(fun _ ->
                    // let par' = {wSModel.SimParams with DispNames = viewableWaves }
                    let wsMod' = {wsModel with State = Running}

                    let msgs = [
                        (StartUICmd ViewWaveSim)
                        ClosePropertiesNotification
                        InitiateWaveSimulation wsMod'
                        // (InitiateWaveSimulation( WSViewerOpen, par'))
                    ]
                    dispatch (Sheet (SheetT.SetSpinner true))
                    dispatch <| SendSeqMsgAsynch msgs
                    dispatch FinishUICmd
                )
            ]
        |> (fun lst -> 
                Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)

    div [ Style [ Display DisplayOptions.Block ] ]
        [ Button.button viewButtonAction [str "View"] ]

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
    printf "toggle select all"
    let waveSimModel = model.WaveSim
    let waves = Seq.toList (Map.values waveSimModel.AllWaves)
    // let conns =
    //     if newState then
    //         waves
    //         |> List.collect (fun wave -> wave.Conns)
    //     else
    //         []
    let allWaves' = Map.map (fun _ wave -> {wave with Selected = newState}) waveSimModel.AllWaves
    dispatch <| SetWSModel {waveSimModel with AllWaves = allWaves'}
    // selectConns model conns dispatch

let isWaveSelected (waveSimModel: WaveSimModel) (name: string) : bool =
    waveSimModel.AllWaves[name].Selected

let selectAll (model: Model) dispatch =
    let waveSimModel = model.WaveSim
    // TODO: Implement function that returns true if all waves selected.
    let allWavesSelected = Map.forall (fun name _ -> isWaveSelected waveSimModel name) waveSimModel.AllWaves
    // let allOn = Map.forall (fun k wave -> isWaveSelected model wave) wSModel.AllWaves
    tr
        [
          Style [ VerticalAlign "middle"; Height rowHeight ]
        ]
        [ td
            [ Class "wACheckboxCol"
              Style [ VerticalAlign "middle"; Height rowHeight  ]
            ] [ input [
                    Type "checkbox"
                    Class "check"
                    Checked allWavesSelected
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleSelectAll (not allWavesSelected) model dispatch ) 
                ]
            ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ]
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
    let waveSimModel = model.WaveSim
    let allWaves = waveSimModel.AllWaves
    // TODO: Fix this to bold only Selected waves
    let getColorProp name  =
        if Map.containsKey name waveSimModel.AllWaves then
            [FontWeight "Bold"]
        else
            []
    // printf "checkboxAndName %A"  (str <| allWaves[name].DisplayName )

    tr  [
          Style [ VerticalAlign "middle" ; Height rowHeight]
        ] 
        [ td
            [ 
                Class "wAcheckboxCol"
                Style [ VerticalAlign "middle" ; Height rowHeight] ]
                [ input
                    [
                        Type "checkbox"
                        Class "check"
                        Checked <| isWaveSelected waveSimModel name
                        Style [ Float FloatOptions.Left ]
                        OnChange(fun _ -> toggleConnsSelect name waveSimModel dispatch)
                    ] 
                ]
          td [] [ label [Style (getColorProp name)] [ str <| allWaves[name].DisplayName ] ]
        ]

let checkboxListOfWaves (model: Model) (dispatch: Msg -> unit) =
    // printf "checkboxListOfWaves"
    let waveSimModel = model.WaveSim
    Seq.toArray (Map.keys waveSimModel.AllWaves)
    |> Array.map (fun name -> checkboxAndNameRow name model dispatch)
    // failwithf "checkboxListOfWaves not implemented"

let selectWaves (model: Model) dispatch = 
    div [ Style [ Position PositionOptions.Relative
                  CSSProp.Top "20px"
                ]
        ]
        [ table []
            [ tbody []
                (Array.append 
                    [| selectAll model dispatch |]
                    (checkboxListOfWaves model dispatch)
                )
            ]
        ]

let closeWaveSimButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let wsModel' = {wsModel with State = NotRunning}
    button 
        [Button.Color IsSuccess; Button.Props [closeWaveSimButtonStyle]]
        (fun _ ->
            namesColWidth <- None
            valuesColWidth <- None
            dispatch <| CloseWaveSim wsModel'
        )
        (str "Edit waves / close simulator")

/// Set clock cycle number
let private setClkCycle (wsModel: WaveSimModel) (dispatch: Msg -> unit) (newClkCycle: int) : unit =
    let newClkCycle' = min maxLastClk newClkCycle |> max 0

    if newClkCycle' < 0 then
        failwithf "newClkCycle' should not be less than zero"
    else if newClkCycle' <= endCycle wsModel then
        if newClkCycle' < wsModel.StartCycle then
            dispatch <| InitiateWaveSimulation
                {wsModel with 
                    CurrClkCycle = newClkCycle'
                    ClkCycleBoxIsEmpty = false
                    StartCycle = newClkCycle'
                }
        else
            dispatch <| SetWSModel
                {wsModel with
                    CurrClkCycle = newClkCycle'
                    ClkCycleBoxIsEmpty = false
                }
        // dispatch <| UpdateScrollPos true
    else
        dispatch <| InitiateWaveSimulation
            {wsModel with
                CurrClkCycle = newClkCycle'
                StartCycle = newClkCycle' - (wsModel.ShownCycles - 1)
                ClkCycleBoxIsEmpty = false
            }
        // dispatch <| UpdateScrollPos true

let zoomLevels = [|
//   0     1     2    3     4     5    6    7    8    9    10   11    12   13   14   15
    0.25; 0.33; 0.5; 0.67; 0.75; 0.8; 0.9; 1.0; 1.1; 1.5; 1.75; 2.0; 2.50; 3.0; 4.0; 5.0;
|]

let changeZoom (wsModel: WaveSimModel) (zoomIn: bool) (dispatch: Msg -> unit) = 
    let tryIndex =
        if zoomIn then wsModel.ZoomLevelIndex + 1
        else wsModel.ZoomLevelIndex - 1

    let newIndex, newZoom =
        Array.tryItem tryIndex zoomLevels
        |> function
        | Some zoom -> tryIndex, zoom
        // Index out of range: keep original zoom level
        | None -> wsModel.ZoomLevelIndex, wsModel.ZoomLevel

    let shownCycles = int <| float wsModel.WaveformColumnWidth / (newZoom * 30.0)

    dispatch <| InitiateWaveSimulation
        {wsModel with
            ZoomLevel = newZoom
            ZoomLevelIndex = newIndex
            ShownCycles = shownCycles
        }

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

let clkCycleButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let bigStepSize = wsModel.ShownCycles / 2

    div [ clkCycleButtonStyle ]
        [
            button [ Button.Props [clkCycleLeftStyle] ]
                (fun _ -> setClkCycle wsModel dispatch (wsModel.CurrClkCycle - bigStepSize))
                (str "◀◀")
            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> setClkCycle wsModel dispatch (wsModel.CurrClkCycle - 1))
                (str "◀")

            Input.number [
                Input.Props [
                    Min 0
                    clkCycleInputStyle
                    SpellCheck false
                    Step 1
                ]

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
                        printf "fail to parse"
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = true}
                    | _ ->
                        printf "catch all case"
                        dispatch <| SetWSModel {wsModel with ClkCycleBoxIsEmpty = false}
                )
            ]

            button [ Button.Props [clkCycleInnerStyle] ]
                (fun _ -> setClkCycle wsModel dispatch (wsModel.CurrClkCycle + 1))
                (str "▶")
            button [ Button.Props [clkCycleRightStyle] ]
                (fun _ -> setClkCycle wsModel dispatch (wsModel.CurrClkCycle + bigStepSize))
                (str "▶▶")
        ]

/// ReactElement of the tabs for changing displayed radix
let private radixButtons (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let radixString =
        [ Dec,  "uDec"
          Bin,  "Bin"
          Hex,  "Hex"
          SDec, "sDec" ] |> Map.ofList

    let radTab rad =
        Tabs.tab [
            Tabs.Tab.IsActive(wsModel.Radix = rad)
            Tabs.Tab.Props
                [ Style [
                    Width "35px"
                    Height "30px"
                ] ]
        ] [ a [
            Style [
                Padding "0 0 0 0"
                Height "30px"
            ]
            OnClick(fun _ -> dispatch <| InitiateWaveSimulation {wsModel with Radix = rad})
            ] [ str (radixString[rad]) ]
        ]

    Tabs.tabs [
        Tabs.IsToggle
        Tabs.Props [ radixTabsStyle ]
    ] [
        radTab Bin
        radTab Hex
        radTab Dec
        radTab SDec
    ]

let waveSimButtonsBar (model: Model) (dispatch: Msg -> unit) : ReactElement = 
    div [ Style [ Height "45px" ] ]
        [
            closeWaveSimButton model.WaveSim dispatch
            zoomButtons model.WaveSim dispatch
            radixButtons model.WaveSim dispatch
            clkCycleButtons model.WaveSim dispatch
            // TODO: zoomButtons
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

let moveWave (wsModel: WaveSimModel) (direction: bool) (dispatch: Msg -> unit) : unit =
    ()

let nameRows (wsModel: WaveSimModel) : ReactElement list =
    selectedWaves wsModel
    |> Map.keys |> Seq.toList
    |> List.map (fun l -> label [ labelStyle ] [ str l ])

let namesColumn rows : ReactElement = 
    let top = [ div [ topRowStyle ] [] ]
    // let bottom = [ div [ rowHeightStyle; tmpStyle ] [] ]

    div [ 
            Id "namesColumn"
            namesColumnStyle namesColWidth
        ]
        (List.concat [ top; rows ])

/// Iterates over each selected wave to generate the SVG of the value for that wave
let valueRows (wsModel: WaveSimModel) = 
    selectedWaves wsModel
    |> Map.values |> Seq.toList
    |> List.map (getWaveValue wsModel.CurrClkCycle)
    |> List.map (valToString wsModel.Radix)
    |> List.map (fun value -> label [ labelStyle ] [ str value ])

let private valuesColumn rows : ReactElement =
    let top = [ div [ topRowStyle ] [] ]
    // let bottom = [ div [ rowHeightStyle; tmpStyle ] [] ]

    div [ 
            Id "valuesColumn"
            valuesColumnStyle valuesColWidth
        ]
        (List.concat [ top; rows ])

/// Generate list of `line` objects which are the background clock lines.
/// These need to be wrapped by an SVG canvas.
let backgroundSVG (wsModel: WaveSimModel) : ReactElement list =
    let clkLine x = 
        line [
            clkLineStyle
            X1 x
            Y1 0.0
            X2 x
            Y2 viewBoxHeight
        ] []
    [ wsModel.StartCycle + 1 .. endCycle wsModel + 1 ] 
    |> List.map (fun x -> clkLine (float x * wsModel.ZoomLevel))

let clkCycleNumberRow (wsModel: WaveSimModel) =
    let makeClkCycleLabel i =
        match wsModel.ZoomLevel with
        | width when width < 0.67 && i % 5 <> 0 -> []
        | _ -> [ text (clkCycleText wsModel i) [str (string i)] ]

    [ wsModel.StartCycle .. endCycle wsModel]
    |> List.collect makeClkCycleLabel
    |> List.append (backgroundSVG wsModel)
    |> svg (clkCycleNumberRowProps wsModel)

let waveformColumn (model: Model) (wsModel: WaveSimModel) dispatch: ReactElement =
    let waveRows : ReactElement list =
        selectedWaves wsModel
        |> Map.values |> Seq.toList
        |> List.map (fun wave ->
            match wave.SVG with
                | Some waveform ->
                    waveform
                // Maybe this shouldn't fail. Could just return a text element saying no waveform was generated
                | None ->
                    printf "no waveform generated for %A" wave.DisplayName
                    [ div [] [] ]//failwithf "No waveform for selected wave %A" wave.DisplayName
            |> List.append (backgroundSVG wsModel)
            // TODO: Rename this props object
            |> svg (waveRowProps wsModel)
        )

    let selectedWavesCount = Map.count (selectedWaves wsModel)

    div [ 
            Id "wavesColumn"
            Style [
                GridColumnStart 2
                Display DisplayOptions.Grid
            ]
        ] [
            clkCycleHighlightSVG wsModel selectedWavesCount
            div [ waveformColumnStyle model wsModel.WaveformColumnWidth]
                (List.concat [
                    [ clkCycleNumberRow wsModel ]
                    waveRows
                ])
        ]

let showWaveforms (model: Model) (dispatch: Msg -> unit) : ReactElement =
    div [ showWaveformsStyle model]
        [
            namesColumn (nameRows model.WaveSim)
            waveformColumn model model.WaveSim dispatch
            valuesColumn (valueRows model.WaveSim)
        ]

let waveViewerPane simData rState (model: Model) (dispatch: Msg -> unit) : ReactElement =
    div [ waveViewerPaneStyle model ]
        [
            // closeWaveSim, radixTabs, changeClkTick, zoomButtons
            waveSimButtonsBar model dispatch
            showWaveforms model dispatch
        ]

/// This function needs to show a list of what waveforms can be displayed, as well as a
/// check box list showing which ones are selectable. Should have a 'select all' box
/// available as well.
let waveSelectionPane simData reducedState (model: Model) dispatch : ReactElement = 
    /// Generate popup over waveeditor screen if there are undriven input connections
    let inputWarningPopup (simData:SimulatorTypes.SimulationData) dispatch =
        if simData.Inputs <> [] then
            let inputs = 
                simData.Inputs
                |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                |> String.concat ","
            let popup = Notifications.warningPropsNotification (sprintf "Inputs (%s) will be set to 0." inputs)
            dispatch <| SetPropertiesNotification popup

    div [ Style [
            Width "90%"
            MarginLeft "5%"
            MarginTop "15px"
        ] ] [
            Heading.h4 [] [ str "Waveform Simulation" ] 
            str "Ctrl-click on diagram connections or use tick boxes below to add or remove waveforms."
            str "Test combinational logic by closing this simulator and using Simulate tab."
            hr []
            div [] [
                    viewWaveformsButton model dispatch
                    selectWaves model dispatch
                ]
        ]

/// Entry point to the waveform simulator.
let viewWaveSim (model: Model) dispatch : ReactElement =
    let simData = SimulationView.makeSimData model
    match simData with
        | None -> failwithf "simRes has value None"
        | Some (Ok simData', reducedState) ->
            // TODO: Add a check to see if there is synchronous data
            // let isClocked = SynchronousUtils.hasSynchronousComponents simData.Graph

            match model.WaveSim.State with
            // Open waveform adder
            | NotRunning ->
                waveSelectionPane simData' reducedState model dispatch
            // Open waveform viewer
            | Running ->
                waveViewerPane simData' reducedState model dispatch
        | Some (Error e, _) -> 
            displayErrorMessage e //IsWarning, "See Problems"
