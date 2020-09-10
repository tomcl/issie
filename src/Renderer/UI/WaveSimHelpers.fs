module WaveSimHelpers

open Fable.React
open Fable.React.Props

open Fulma

open DiagramMessageType
open DiagramModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open Extractor
open Simulator
open SimulatorTypes

/////////////////////////////
// General WaveSim Helpers //
/////////////////////////////

/// get an option of the reduced canvas state
let private getReducedCanvState model =
    match model.Diagram.GetCanvasState() with
    | Some cS -> Some <| extractReducedState cS
    | None -> None
    
/// get NetList from WaveSimModel
let wsModel2netList wsModel =
    match wsModel.LastCanvasState with
    | Some canvState -> Helpers.getNetList canvState
    | None -> Map.empty

////////////////////////
// Simulation Helpers //
////////////////////////

/// get option of the Simulation Data of the current Diagram
let private makeSimData model =
    match model.Diagram.GetCanvasState(), model.CurrProject with
    | None, _ -> None
    | _, None -> None
    | Some jsState, Some project ->
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> Some

/// get SourceGroup from port which represents the group sources connected by IOLabels
let rec private getTrgtLstGroup (netList: NetList) nlTrgtLst =
    let connectedLblNames = 
        List.filter (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Type = IOLabel) nlTrgtLst
        |> List.map (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Label)
        |> List.distinct
    let connectedNLsources' =
        Map.filter (fun _ (netListComp: NetListComponent) -> 
                        List.contains netListComp.Label connectedLblNames 
                        && netListComp.Type = IOLabel) netList
        |> Map.map (fun _ nlComp -> 
            nlComp.Outputs.[OutputPortNumber 0]
            |> getTrgtLstGroup netList
            |> (fun nlSrcGroup -> Array.append [|nlSrcGroup.mainTrgtLst|] nlSrcGroup.connectedTrgtLsts ) )
        |> Map.toArray
        |> Array.collect snd
    { mainTrgtLst = nlTrgtLst; connectedTrgtLsts = connectedNLsources' }

/// returns a bool representing if the given NLTarget is present in the given NetList
let private isNetListTrgtInNetList (netList: NetList) (nlTrgt: NLTarget) =
    Map.exists (fun _ (nlComp: NetListComponent) -> 
                    Map.exists (fun _ nlTrgtLst -> List.contains nlTrgt nlTrgtLst) nlComp.Outputs) netList

/// get array of TrgtLstGroup with the non-existing NLTargets removed
let private getReloadableTrgtLstGroups (model: Model) (netList: NetList) =
    match currWS model with
    | Some wSMod ->
        Array.map (fun trgtLstGroup -> trgtLstGroup.mainTrgtLst) wSMod.Ports 
        |> Array.map (fun trgtLst -> List.filter (isNetListTrgtInNetList netList) trgtLst)
        |> Array.filter (fun lst -> lst <> [])
        |> Array.map (getTrgtLstGroup netList)
    | None -> [||]

/// advance SimulationData by 1 clock cycle
let private clkAdvance (sD: SimulationData) =
    feedClockTick sD.Graph
    |> (fun graph ->
        { sD with
              Graph = graph
              ClockTickNumber = sD.ClockTickNumber + 1 })

/// array of SimData for the given number of cycles
let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> clkAdvance s, clkAdvance s)
    |> fst

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList.[compId].Inputs.[inPortN]

/// get array of available NLSource in current canvas state
let availableNLTrgtLstGroups (model: Model) =
    match model.Diagram.GetCanvasState() with
    | None -> [||]
    | Some jsState ->
        let netList =
            extractState jsState 
            |> Helpers.getNetList
        Map.toArray netList
        |> Array.filter (fun (_, (nlComp: NetListComponent)) -> 
               match nlComp.Type with
               | SplitWire _ | MergeWires _ | IOLabel  -> false
               | _ -> true )
        |> Array.collect (fun (_,nlComp) -> 
            Map.map (fun _ lst -> getTrgtLstGroup netList lst) nlComp.Outputs 
            |> Map.toArray |> Array.map snd)

/// get instantaneous value of a port
let private simWireData2Wire wireData =
    wireData
    |> List.mapFold (fun weight bit ->
        match bit with
        | SimulatorTypes.Bit.Zero -> bigint 0
        | SimulatorTypes.Bit.One -> weight
        |> (fun r -> r, weight * (bigint 2))) (bigint 1)
    |> fst
    |> List.sum

/// extract current value of the given array of SourceGroup
let getSimTime (trgtLstGroups: TrgtLstGroup array) (simGraph: SimulationGraph) =
    Array.map (fun trgtLstGroup -> trgtLstGroup.mainTrgtLst) trgtLstGroups
    |> Array.map (fun trgtLst ->
        let wD = simGraph.[trgtLst.[0].TargetCompId].Inputs.[trgtLst.[0].InputPort]
        Wire
            { NBits = uint (List.length wD)
              BitData = simWireData2Wire wD } )

/// get values of waveforms
let getWaveData (wsMod: WaveSimModel) =
    Array.map (fun sD -> sD.Graph) wsMod.SimData
    |> Array.map (getSimTime wsMod.Ports) 
    
/// extend WaveSimModel.SimData by n cycles
let private appendSimData (wSModel: WaveSimModel) nCycles = 
    extractSimData (Array.last wSModel.SimData) nCycles 
    |> Array.append wSModel.SimData

/// get JSConnection list from ConnectionId (as a string)
let private connId2JSConn (model: Model) connId =
    match model.Diagram.GetCanvasState() with
    | Some (_, jsConns) -> 
        List.tryFind (fun jsConn -> (extractConnection jsConn).Id = connId) jsConns
    | None -> None
    |> function
       | Some jsConn -> [ jsConn ]
       | None -> []

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (trgtLstGroup: TrgtLstGroup) =
    Array.append [|trgtLstGroup.mainTrgtLst|] trgtLstGroup.connectedTrgtLsts
    |> Array.collect (fun trgtLst -> 
        List.toArray trgtLst 
        |> Array.map (fun nlTrgt -> nlTrgt.TargetConnId))

/// select the connections of a given waveform
let selWave2selConn model (trgtLstGroup: TrgtLstGroup) on =
    wave2ConnIds trgtLstGroup
    |> Array.toList
    |> List.collect (fun (ConnectionId cId) -> connId2JSConn model cId) 
    |> model.Diagram.SetSelected on

///////////////////////////
/// Naming of waveforms ///
///////////////////////////

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
    let lbl = netList.[compId].Label
    match Seq.tryFindIndexBack ((=) '(') lbl with
    | Some i -> lbl.[0..i - 1]
    | None -> lbl

/// get integer from OutputPortInt
let private outPortInt2int outPortInt =
    match outPortInt with
    | OutputPortNumber pn -> pn

/// get labels of Output and IOLabel components in nlTargetList
let trgtLst2outputsAndIOLabels (netList: NetList) (nlTrgtLst: NLTarget list) =
    let nlTrgt2Lbls st nlTrgt= 
        match netList.[nlTrgt.TargetCompId].Type with
        | IOLabel | Output _ -> List.append st [netList.[nlTrgt.TargetCompId].Label]
        | _ -> st
    List.fold nlTrgt2Lbls [] nlTrgtLst
    |> List.distinct

/// get labels of Output and IOLabel components in TargetListGroup
let trgtLstGroup2outputsAndIOLabels netList (trgtLstGroup: TrgtLstGroup) =
    Array.append [|trgtLstGroup.mainTrgtLst|] trgtLstGroup.connectedTrgtLsts
    |> Array.toList
    |> List.collect (trgtLst2outputsAndIOLabels netList)
    |> List.distinct

/// get WaveLabel corresponding to a NLTarget list
let rec private findName (compIds: ComponentId Set) (graph: SimulationGraph) (netList: NetList) nlTrgtListGroup nlTrgtList =
    match nlTrgtLst2CommonNLSource netList nlTrgtList with
    //nlTrgtLst is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        if not (Set.contains nlSource.SourceCompId compIds) then
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else   
            let compLbl = labelNoParenthesis netList nlSource.SourceCompId
            let outPortInt = outPortInt2int nlSource.OutputPort
            let drivingOutputName inPortN =
                match drivingOutput netList nlSource.SourceCompId inPortN with
                | Some nlSource' ->
                    netList.[nlSource'.SourceCompId].Outputs.[nlSource'.OutputPort]
                    |> findName compIds graph netList nlTrgtListGroup
                | None ->  { OutputsAndIOLabels = []; ComposingLabels = [] } 

            match netList.[nlSource.SourceCompId].Type with
            | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 -> 
                [ { Name = compLbl; BitLimits = 0, 0 } ] 
            | Input w | Output w | Constant(w, _) -> 
                [ { Name = compLbl; BitLimits = w - 1, 0 } ] 
            | Demux2 -> 
                [ { Name = compLbl + "_" + string outPortInt; BitLimits = 0, 0 } ]
            | NbitsAdder w ->
                match outPortInt with
                | 0 -> [ { Name = compLbl + "_sum"; BitLimits = w - 1, 0 } ]
                | _ -> [ { Name = compLbl + "Cout"; BitLimits = w - 1, 0 } ]
            | DFF | DFFE -> 
                [ { Name = compLbl + "_Q"; BitLimits = 0, 0 } ]
            | Register w | RegisterE w -> 
                [ { Name = compLbl + "_data-out"; BitLimits = w-1, 0 } ]
            | RAM mem | AsyncROM mem | ROM mem -> 
                [ { Name = compLbl + "_data-out"; BitLimits = mem.WordWidth - 1, 0 } ]
            | Custom c -> 
                [ { Name = compLbl + "_" + fst c.OutputLabels.[outPortInt]
                    BitLimits = snd c.OutputLabels.[outPortInt] - 1, 0 } ]
            | MergeWires -> 
                List.append (drivingOutputName (InputPortNumber 1)).ComposingLabels 
                            (drivingOutputName (InputPortNumber 0)).ComposingLabels
            | SplitWire w ->
                let predicate (_, b) =
                    match outPortInt with
                    | 0 -> b >= w
                    | 1 -> b < w
                    | _ -> failwith "SplitWire output port number greater than 1"

                let split { Name = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                    |> List.filter predicate
                    |> List.unzip
                    |> function
                    | [], _ -> None 
                    | lst, _ -> Some { Name = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { Name = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1

                (0, (drivingOutputName (InputPortNumber 0)).ComposingLabels)
                ||> List.mapFold (fun st lstEl -> split lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
            | BusSelection(w, oLSB) ->
                let filtSelec { Name = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
                    |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
                    |> List.unzip
                    |> function
                    | [], _ -> None
                    | lst, _ -> Some { Name = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { Name = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1 

                ((drivingOutputName (InputPortNumber 0)).ComposingLabels, 0)
                ||> List.mapFoldBack (fun lstEl st -> filtSelec lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
                |> List.rev
            | IOLabel -> 
                let ioLblWidth = List.length graph.[nlTrgtList.[0].TargetCompId].Inputs.[nlTrgtList.[0].InputPort]
                [ { Name = compLbl
                    BitLimits = ioLblWidth - 1, 0 } ]

            |> (fun composingLbls -> { OutputsAndIOLabels = trgtLstGroup2outputsAndIOLabels netList nlTrgtListGroup
                                       ComposingLabels = composingLbls })

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

/// get the label of a waveform
let nlTrgtLstGroup2Label compIds graph netList (nlTrgtLstGroup: TrgtLstGroup) =
    let waveLbl = findName compIds graph netList nlTrgtLstGroup nlTrgtLstGroup.mainTrgtLst
    let tl =
        match waveLbl.ComposingLabels with
        | [ el ] -> el.Name + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.Name + bitLimsString lblSeg.BitLimits + ", "
            List.fold appendName "{" lst 
            |> (fun lbl -> lbl.[0..String.length lbl - 3] + "}")
        |  _ -> ""
    let appendName st name = st + name + ", "
    match waveLbl.OutputsAndIOLabels with
    | [] -> tl
    | hdLbls -> 
        List.fold appendName "" hdLbls
        |> (fun hd -> hd.[0..String.length hd - 3] + " : " + tl)
        

//////////////////
/// SVG shapes ///
//////////////////

let makeLinePoints style (x1, y1) (x2, y2) =
    line
        (List.append style 
             [ X1 x1
               Y1 y1
               X2 x2
               Y2 y2 ]) []

let makeSvg style elements = svg style elements
let makeRect style = rect style []
let makeLine style = line style []
let makeText style t = text style [ str t ]

let backgroundSvg (model: WaveSimModel) =
    let clkLine x = makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1u .. model.LastClk + 1u |] |> Array.map ((fun x -> float x * model.ClkWidth) >> clkLine)

let button options func label = 
    Button.button (List.append options [ Button.OnClick func ]) [ str label ]

////////////////////////
/// Radix conversion ///
////////////////////////


let dec2bin (n:bigint) (nBits:uint32) =
    [nBits - 1u..0u]
    |> List.map (fun bitNum -> if n &&& (1I <<< int bitNum) = 0I then '0' else '1')

let dec2hex (n: bigint) (nBits: uint32): string =
    let seqPad = 
        let times = (4 - int nBits % 4) % 4
        Seq.replicate times '0'

    let paddedBin =
        dec2bin n nBits
        |> Seq.append seqPad
        |> Seq.toList

    let fourBit2HexDig =
        let bit vec n = if (1 <<< n) &&& vec = 0 then '0' else '1'
        let hexDigOf n = (sprintf "%x" n).[0]
        [0..15]
        |> List.map (fun dig -> 
                List.map (bit dig) [3..0], hexDigOf dig)
        |> Map.ofList

    [ 0 .. 4 .. int nBits - 1 ]
    |> List.map ( (fun i -> fourBit2HexDig.[ paddedBin.[i..i + 3] ])
                  >> string )
    |> List.toSeq
    |> String.concat ""

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits).[0] = '1' 
        then n - bigint (2.0 ** (float nBits)) 
        else n
    |> string

/// convert number to number string of the chosen radix
let n2StringOfRadix (n: bigint) (nBits: uint32) (rad: NumberBase) =
    match rad with
    | Dec -> string n
    | Bin -> string <| dec2bin n nBits
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits


///////////////////
/// Transitions ///
///////////////////

/// get m x (n-1) array of integers representing when value change between clock cycles from m x n waveData
/// (1: change, 0 no change)
let transitions waveData =
    let isDiff (ws1, ws2) =
        let folder state e1 e2 =
            match state, e1 = e2 with
            | 0, true -> 0
            | _ -> 1
        match ws1, ws2 with
        | Wire a, Wire b ->
            if a.BitData = b.BitData then 0 else 1
        | StateSample a, StateSample b when Array.length a = Array.length b -> 
            (a, b) ||> Array.fold2 folder 0
        | _ -> 1

    Array.transpose waveData
    |> Array.map (Array.pairwise >> Array.map isDiff)

/// get gaps from transition array
let makeGaps trans =
    Array.append trans [| 1 |]
    |> Array.mapFold (fun tot t -> tot, tot + t) 0
    |> fst
    |> Array.indexed
    |> Array.groupBy snd
    |> Array.map (fun (_, gL) ->
        let times = Array.map fst gL
        {| GapLen = Array.max times - Array.min times + 1
           GapStart = Array.min times |})

///////////////////////
/// WaveSim Actions ///
///////////////////////

/// add entry with key: current fileName and data: initWS to model.WaveSim
let initFileWS model dispatch =
    match getCurrFile model with
    | Some fileName ->
        (fileName, initWS)
        |> AddWaveSimFile
        |> dispatch
    | None -> ()

/// set model.WaveAdder to show the list of waveforms that can be selected
let private setWA compIds model wSMod dispatch simData netList =
    SetViewerWidth minViewerWidth |> dispatch
    getReducedCanvState model |> SetLastSimulatedCanvasState |> dispatch
    SetSimIsStale false |> dispatch

    let wA' =
        let trgtLstGroups = availableNLTrgtLstGroups model
        { SimData = Some simData
          Ports = trgtLstGroups 
          WaveNames = Array.map (nlTrgtLstGroup2Label compIds simData.Graph netList) trgtLstGroups }
    { wSMod with
          WaveAdder = wA'
          LastCanvasState = getReducedCanvState model }
    |> SetCurrFileWSMod
    |> dispatch

/// ReactElement array of the box containing the waveform's SVG
let private waveCol waveSvg clkRulerSvg model wsMod waveData =
    let waveTableRow rowClass cellClass svgClass svgChildren =
        tr rowClass [ td cellClass [ makeSvg svgClass svgChildren ] ]
    let bgSvg = backgroundSvg wsMod
    let cursRectSvg = [| makeRect (cursRectStyle wsMod) |]

    [| waveTableRow [ Class "fullHeight" ] (lwaveCell wsMod) (waveCellSvg wsMod true)
           (Array.append bgSvg cursRectSvg) |]
    |> Array.append
        (Array.map
            (fun wave ->
                waveTableRow [ Class "rowHeight" ] (waveCell wsMod) (waveCellSvg wsMod false)
                    (Array.concat [| cursRectSvg; bgSvg; wave |])) (waveSvg model wsMod waveData))
    |> Array.append [| tr [ Class "rowHeight" ] [ td (waveCell wsMod) [ clkRulerSvg wsMod ] ] |]

/// update the WaveSimModel entry of the current file with new parameters
let updateWSMod waveSvg clkRulerSvg (model: Model) (wsMod: WaveSimModel) 
                (par: {| LastClk: uint; Curs: uint; ClkW: float |}) : WaveSimModel =
    let cursLastClkMax = max par.Curs par.LastClk
    match cursLastClkMax > wsMod.LastClk with
    | true -> 
        { wsMod with LastClk = cursLastClkMax
                     SimData = cursLastClkMax + 1u - uint (Array.length wsMod.SimData)
                               |> appendSimData wsMod  }
    | false -> wsMod
    |> (fun m -> { m with Cursor = par.Curs 
                          ClkWidth = par.ClkW } )
    |> (fun m -> { m with WaveTable = waveCol waveSvg clkRulerSvg model m (getWaveData m) })

/// call waveCol with the current Simulation Data 
let waveGen model waveSvg clkRulerSvg (wSMod: WaveSimModel) ports =
    let simData' = 
        match wSMod.WaveAdder.SimData with
        | Some sD ->
            wSMod.LastClk
            |> extractSimData sD
            |> Array.append [| sD |] 
        | None -> failwith "waveGen called when WaveAdder.SimData is None"

    let wSMod' =
        { wSMod with
            SimData = simData'
            Ports = ports
            WaveAdderOpen = false }

    { wSMod' with WaveTable = waveCol waveSvg clkRulerSvg model wSMod' (getWaveData wSMod') }

//////////////////////////////////////
/// Interaction with Model.Diagram ///
//////////////////////////////////////

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
let private isNLTrgtLstGroupSelected (netList: NetList) ((comps, conns): CanvasState) (trgtLstGroup: TrgtLstGroup) =
    Array.append [|trgtLstGroup.mainTrgtLst|] trgtLstGroup.connectedTrgtLsts
    |> Array.exists (isNLTrgtLstSelected netList (comps, conns)) 

/// is the given waveform selected by the current diagram selection
let isWaveSelected model netList (nlTrgtLstGroup: TrgtLstGroup) = 
    match model.Diagram.GetSelected() with
    | Some selectedCompsConnsJS ->
        let selectedCompsConns = extractState selectedCompsConnsJS
        isNLTrgtLstGroupSelected netList selectedCompsConns nlTrgtLstGroup
    | _ -> false 

////////////////////////////
/// Saving WaveSim Model ///
////////////////////////////

/// get saveable record of waveform setup
let wsModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    { Ports = wsMod.Ports
      ClkWidth = wsMod.ClkWidth
      Cursor = wsMod.Cursor
      Radix = wsMod.Radix
      LastClk = wsMod.LastClk
      WaveAdderOpen = wsMod.WaveAdderOpen
      WaveAdderPorts = wsMod.WaveAdder.Ports }

/// setup current WaveSimModel from saved record
let savedWaveInfo2wsModel compIds waveSvg clkRulerSvg model (sWInfo: SavedWaveInfo) : WaveSimModel =
    match makeSimData model, model.Diagram.GetCanvasState() with
    | Some (Ok sD), Some canvState ->
        let netList = Helpers.getNetList <| extractState canvState
        let ports' = getReloadableTrgtLstGroups model netList
        let sD' = Array.append [| sD |] (extractSimData sD sWInfo.LastClk)
        let waPorts' = availableNLTrgtLstGroups model
        { SimData = sD'
          WaveTable = [||]
          Ports = ports'
          ClkWidth = sWInfo.ClkWidth
          Cursor = sWInfo.Cursor
          CursorEmpty = false
          Radix = sWInfo.Radix
          LastClk = sWInfo.LastClk
          WaveAdderOpen = sWInfo.WaveAdderOpen
          WaveAdder = { SimData = Some sD
                        Ports = waPorts'
                        WaveNames = Array.map (nlTrgtLstGroup2Label compIds sD.Graph netList) waPorts' }
          LastCanvasState = getReducedCanvState model }
        |> (fun m -> { m with WaveTable = waveCol waveSvg clkRulerSvg model m (getWaveData m) } )
    | Some (Error _), _ | None, _ | _, None -> initWS

/////////////////////////////////////////////////////
/// Functions fed into FileMenuView View function ///
/////////////////////////////////////////////////////

/// actions triggered whenever the fileMenuView function is executed
let fileMenuViewActions model dispatch =
    match model.SimulationInProgress with
    | Some par -> SimulateWhenInProgress par |> dispatch
    | None -> ()

    if model.ConnsToBeHighlighted
    then  
        match currWS model with
        | Some wSMod ->
            if wSMod.WaveAdderOpen then wSMod.WaveAdder.Ports else wSMod.Ports
            |> Array.map (fun net -> if isWaveSelected model (wsModel2netList wSMod) net
                                     then wave2ConnIds net
                                     else [||])
            |> Array.concat
            |> SetSelWavesHighlighted
            |> dispatch
        | _ -> ()
    else ()

/// actions triggered by pressing the Simulate >>> button
let simulateButtonFunc compIds model dispatch =
    match model.SimulationIsStale, currWS model, makeSimData model, model.Diagram.GetCanvasState() with
    | true, Some wSMod, Some (Ok simData), Some jsCanvState ->
        let netList = extractState jsCanvState |> Helpers.getNetList
        Button.button
            [ Button.Color IsSuccess
              Button.OnClick(fun _ ->
                  setWA compIds model wSMod dispatch simData netList
                  ChangeRightTab WaveSim |> dispatch) ]
    | true, Some _, Some (Error err), _ -> 
        Button.button
            [ Button.OnClick(fun _ ->
                  Some err |> SetWSError |> dispatch
                  ChangeRightTab WaveSim |> dispatch ) ]
    | _, None, _, _ -> 
              match model.CurrProject with
              | Some _ -> initFileWS model dispatch
              | None -> ()
              Button.button []
    | _ -> Button.button []
    |> (fun but -> but [ str "Waveforms >>" ])