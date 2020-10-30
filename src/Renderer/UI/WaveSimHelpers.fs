module WaveSimHelpers

open Fable.React
open Fable.React.Props

open Fulma

open MessageType
open ModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open Extractor
open Simulator
open SimulatorTypes

let maxLastClk = 500u

/////////////////////////////
// General WaveSim Helpers //
/////////////////////////////

/// get an option of the reduced canvas state
let getReducedCanvState model =
    match model.Diagram.GetCanvasState() with
    | Some cS -> Some <| extractReducedState cS
    | None -> None
    
/// get NetList from WaveSimModel
let wsModel2netList wsModel =
    match wsModel.LastCanvasState with
    | Some canvState -> Helpers.getNetList canvState
    | None -> Map.empty

// Look up netgroup (= waveform) name from WaveData and netgroup
// If Netgroup is not in AllNetGroups return "ERROR"
let waveNameOf (wa:WaveTempT) (ng:NetGroup) =
    Array.tryFindIndex ((=) ng) wa.AllNetGroups
    |> Option.bind (fun i -> Array.tryItem i wa.AllWaveNames)
    |> Option.defaultValue "ERROR"
        
// Look up netgroup (= waveform) name from WaveSimModel and NetGroup.
// if NetGroup is not in AllNetGroups return "NO WAVEDATA"
let waveNameOfWSModel (wSMod: WaveSimModel) =
    match wSMod.WaveData with
    | None -> fun _ -> "NO WAVEDATA"
    | Some wa -> waveNameOf wa
       

    

////////////////////////
// Simulation Helpers //
////////////////////////

// NB - all the NetGroup functions assume a working netlist in which NO NET IS UNDRIVEN
// Every Net must be driven by exactly one componnet output port (NLSource).
// IOLabels are nout counted as drivers themselves; every group of same label IOlabels 
// and all of their output nets 
// makes a netgroup which must be driven by just one NLSource (connected to one of the IOLabel inputs).
// every net is therefore part of one netgroup which is either a single net, or a group of nets associated
// with a set of IOLabel connectors having a given common label.

let mapKeys (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map fst |> Array.ofSeq
let mapValues (map:Map<'a,'b>) = map |> Map.toSeq |> Seq.map snd |> Array.ofSeq
let mapItems (map:Map<'a,'b>) = map |> Map.toSeq |> Array.ofSeq

let private allNComps (netList:NetList) =
    netList |> mapValues

type NGrp = {
    Driven: string list; 
    DriverLabel: string list
    Driver: NLTarget list
    }
 
    
    
let private makeAllNetGroups (netList:NetList) :NetGroup array=

    let comps = allNComps netList

    let labelConnectedNets: Map<string,NLTarget list array> =       
        comps
        |> Array.collect (fun comp ->
            if comp.Type = IOLabel then [|comp.Label, comp.Outputs.[OutputPortNumber 0]|] else [||])
        |> Array.groupBy (fun (label, _) -> label)
        |> Array.map (fun (lab, labOutArr)-> lab, (labOutArr |> Array.map (snd)))
        |> Map.ofArray

    let makeNetGroup (targets:NLTarget list) =
        let connected = 
            targets
            |> List.toArray
            |> Array.collect (fun target -> 
                let comp = netList.[target.TargetCompId]
                if comp.Type = IOLabel then labelConnectedNets.[comp.Label] else [||])
        {driverNet=targets; connectedNets=connected}


    let allNetGroups =
        comps
        |> Array.collect (fun comp -> 
            match comp.Type with
            | IOLabel -> [||]
            | _ -> mapValues comp.Outputs |> Array.map makeNetGroup)
    allNetGroups
          
               
 

        







    
    

/// Get NetGroup from targets which represents the group of nLTargets connected by IOLabels.
/// targets:list of inputs connected to a single driving component output (e.g. a connected Net).
/// Return the containing NetGroup, where Nets connected by IOLabels form single Netgroups.
let rec private getNetGroup (netList: NetList) targets = failwithf "this function is no longer implemented"
(*    let isSubList lstSmall lstBig =
        lstSmall
        |> List.forall (fun el -> List.contains el lstBig) 

    let ioLabelsOfTargets =
        targets
        |> List.filter (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Type = IOLabel)
        |> List.map (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Label)
        |> List.distinct



 

    let driverNet' =
        Map.tryPick (fun _ (nlComp: NetListComponent) -> 
            Map.toArray nlComp.Outputs
            |> Array.tryFind (fun (_, lst) -> isSubList targets lst)) netList
        |> function
        | Some (_, lst) -> lst
        | None -> targets
    { driverNet = driverNet'; connectedNets = connectedNLsources' }*)

/// returns a bool representing if the given NLTarget is present in the given NetList
let private isNetListTrgtInNetList (netList: NetList) (nlTrgt: NLTarget) =
    Map.exists (fun _ (nlComp: NetListComponent) -> 
                    Map.exists (fun _ nlTrgtLst -> List.contains nlTrgt nlTrgtLst) nlComp.Outputs) netList

/// get array of TrgtLstGroup with the non-existing NLTargets removed
let private getReloadableNetGroups (model: Model) (netList: NetList) =
    match currWaveSimModel model with
    | Some wSModel ->
        Array.map (fun netGroup -> netGroup.driverNet) wSModel.DispPorts
        |> Array.map (List.filter <| isNetListTrgtInNetList netList)
        |> Array.filter ((<>) [])
        |> Array.map (getNetGroup netList)
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

let netList2NetGroups netList = makeAllNetGroups netList
    (*Map.toArray netList
    |> Array.filter (fun (_, (nlComp: NetListComponent)) -> 
           match nlComp.Type with
           | SplitWire _ | MergeWires _ | IOLabel |Constant _ -> false
           | _ -> true )
    |> Array.collect (fun (_,nlComp) -> 
        nlComp.Outputs
        |> Map.map (fun _ targets -> getNetGroup netList targets)  
        |> Map.toArray 
        |> Array.map snd)*)

/// get array of available NLSource in current canvas state
let availableNetGroups (model: Model) =
    match getSheetWaveSimOpt model with
    | None -> [||]
    | Some waveSim ->
        waveSim.LastCanvasState
        |> Option.defaultValue ([],[])
        |> Helpers.getNetList
        |> netList2NetGroups

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
let getSimTime (trgtLstGroups: NetGroup array) (simGraph: SimulationGraph) =
    Array.map (fun trgtLstGroup -> trgtLstGroup.driverNet) trgtLstGroups
    |> Array.map (fun trgtLst ->
        let wD = simGraph.[trgtLst.[0].TargetCompId].Inputs.[trgtLst.[0].InputPort]
        Wire
            { NBits = uint (List.length wD)
              BitData = simWireData2Wire wD } )

/// get values of waveforms
let getWaveData (wsMod: WaveSimModel) =
        Array.map (fun sD -> sD.Graph) wsMod.SimDataCache
        |> Array.map (getSimTime wsMod.DispPorts) 
    
/// extend WaveSimModel.SimData by n cycles
let private appendSimData (model: Model) (wSModel: WaveSimModel) nCycles = 
    match wSModel.SimDataCache with
    | [||] ->
        SimulationView.makeSimData model
        |> Option.map fst
        |> ( Option.map (Result.map (fun sd -> extractSimData sd nCycles))) // TODO simulate this ncycles no init data
    | dat ->
        extractSimData (Array.last dat) nCycles
        |> Array.append dat
        |> Ok
        |> Some

/// get JSConnection list from ConnectionId (as a string)
let private connId2JSConn (diagram:Draw2dWrapper.Draw2dWrapper) connId =
    match diagram.GetCanvasState() with
    | Some (_, jsConns) -> 
        List.tryFind (fun jsConn -> (extractConnection jsConn).Id = connId) jsConns
    | None -> None
    |> function
       | Some jsConn -> [ jsConn ]
       | None -> []

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.collect (fun net -> 
        List.toArray net 
        |> Array.map (fun net -> net.TargetConnId))

/// select or deselect the connections of a given netGrp
let selectNetGrpConns diagram (netGrp: NetGroup) on =
    wave2ConnIds netGrp
    |> Array.toList
    |> List.collect (fun (ConnectionId cId) -> connId2JSConn diagram cId) 
    |> diagram.SetSelected on

/// returns labels of all custom component instances of sheet in lComp
let findInstancesOf sheet (lComp:LoadedComponent) =
    lComp.CanvasState
    |> fst
    |> List.collect (function | {Type=(Custom {Name=sheet'})} as comp when sheet' = sheet-> [comp.Label] | _ -> [])
  

/// finds out if current sheet is used in some other sheet.
/// works recursively to find root sheet
/// returns current sheet if there is a cycle, or more than one path to a root
let getRootSheet  (model:Model) =
    match model with
    | {CurrProject = (Some ({LoadedComponents=lComps} as proj))} ->
        let openSheet = proj.OpenFileName
        let getParentData (sheet:string) =
            lComps
            |> List.filter (fun lComp -> lComp.Name <> sheet) // can't be own parent
            |> List.collect (fun lComp -> 
                let parentName = lComp.Name
                match findInstancesOf sheet lComp with
                | [] -> [] //not a parent
                | [_] -> [parentName]
                | _ -> [openSheet]) // if more than one instance can't analyse
        let rec findRoot (traversedSheets:string list) (sheet:string) =
            if List.contains sheet traversedSheets then 
                openSheet
            else
                match getParentData sheet with
                | [root] -> findRoot (sheet :: traversedSheets) root
                | [] -> sheet
                | _ -> openSheet
        Some <| findRoot [] openSheet
    | _ -> None // should never happen

let getCustoms (cid:ComponentId,comp:SimulationComponent) =
    match comp.Type with
    | Custom c -> [cid, c.Name,comp]
    | _ -> []

let simGraphOrFail (comp:SimulationComponent) =
    match comp.CustomSimulationGraph with
    | Some x -> x
    | None -> failwithf "What? a Custom component %A appears to have no simulation graph" comp

/// return a function which can extract the correct SimulationGraph from the current simulation
/// for the given sheet. The parameter is a sample SumulationGraph
let getSimGraph (sheet:string) (graph:SimulationGraph) =
    let makeSelector cids =
        (id, List.rev cids) 
        ||> List.fold (fun selFun cid -> 
                            let func graph = simGraphOrFail (Map.find cid graph)
                            selFun >> func)
       
        
    let rec getGraph (customs:ComponentId list) (thisSheet:string) (graph:SimulationGraph) : (SimulationGraph -> SimulationGraph) array =
        if sheet = thisSheet 
        then 
            [|id|]
        else          
            Map.toArray graph
            |> Array.collect (getCustoms >> List.toArray)
            |> Array.collect (fun (cid,sheet',comp) -> 
                if sheet' = sheet then 
                    [|makeSelector (cid :: customs)|]
                else 
                    getGraph (cid :: customs) sheet' (Option.defaultValue (failwithf "What? %A should have a CustomSimulationGraph" comp) comp.CustomSimulationGraph))
      
    getGraph [] sheet graph
    |> function | [|f|] -> f | x -> failwithf "Wrong number of candidates %A to find %s in Simgraph" x sheet

                    

                

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
let net2outputsAndIOLabels (netList: NetList) (netLst: NLTarget list) =
    let nlTrgt2Lbls st nlTrgt = 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some nlComp -> match nlComp.Type with
                         | IOLabel | Output _ -> List.append st [netList.[nlTrgt.TargetCompId].Label]
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
let rec private findName (compIds: ComponentId Set) (graph: SimulationGraph) (net: NetList) netGrp nlTrgtList =
    match nlTrgtLst2CommonNLSource net nlTrgtList with
    //nlTrgtLst is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        if not (Set.contains nlSource.SourceCompId compIds) then
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else   
            let compLbl = labelNoParenthesis net nlSource.SourceCompId
            let outPortInt = outPortInt2int nlSource.OutputPort
            let drivingOutputName inPortN =
                match drivingOutput net nlSource.SourceCompId inPortN with
                | Some nlSource' ->
                    net.[nlSource'.SourceCompId].Outputs.[nlSource'.OutputPort]
                    |> findName compIds graph net netGrp
                | None ->  { OutputsAndIOLabels = []; ComposingLabels = [] } 

            match net.[nlSource.SourceCompId].Type with
            | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 -> 
                [ { LabName = compLbl; BitLimits = 0, 0 } ] 
            | Input w | Output w | Constant(w, _) -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ] 
            | Demux2 -> 
                [ { LabName = compLbl + "_" + string outPortInt; BitLimits = 0, 0 } ]
            | NbitsAdder w ->
                match outPortInt with
                | 0 -> [ { LabName = compLbl + "_sum"; BitLimits = w - 1, 0 } ]
                | _ -> [ { LabName = compLbl + "Cout"; BitLimits = w - 1, 0 } ]
            | DFF | DFFE -> 
                [ { LabName = compLbl + "_Q"; BitLimits = 0, 0 } ]
            | Register w | RegisterE w -> 
                [ { LabName = compLbl + "_data-out"; BitLimits = w-1, 0 } ]
            | RAM mem | AsyncROM mem | ROM mem -> 
                [ { LabName = compLbl + "_data-out"; BitLimits = mem.WordWidth - 1, 0 } ]
            | Custom c -> 
                [ { LabName = compLbl + "_" + fst c.OutputLabels.[outPortInt]
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

                let split { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                    |> List.filter predicate
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
                let ioLblWidth = List.length graph.[nlTrgtList.[0].TargetCompId].Inputs.[nlTrgtList.[0].InputPort]
                [ { LabName = compLbl
                    BitLimits = ioLblWidth - 1, 0 } ]

            |> (fun composingLbls -> { OutputsAndIOLabels = netGroup2outputsAndIOLabels net netGrp
                                       ComposingLabels = composingLbls })

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

/// get the label of a waveform
let netGroup2Label compIds graph netList (netGrp: NetGroup) =
    let waveLbl = findName compIds graph netList netGrp netGrp.driverNet
    let tl =
        match waveLbl.ComposingLabels with
        | [ el ] -> el.LabName + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.LabName + bitLimsString lblSeg.BitLimits + ", "
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

let private charList2String (charLst: char list) = 
    List.map string charLst
    |> List.toSeq
    |> String.concat ""

let dec2bin (n:bigint) (nBits:uint32) =
    [0u..nBits - 1u]
    |> List.rev
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
                List.map (bit dig) [3..-1..0], hexDigOf dig)
        |> Map.ofList

    [ 0 .. 4 .. List.length paddedBin - 4 ]
    |> List.map (fun i -> fourBit2HexDig.[ paddedBin.[i..i + 3] ])
    |> charList2String

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits).[0] = '1' 
        then n - bigint (2.0 ** (float nBits)) 
        else n
    |> string

/// convert number to number string of the chosen radix
let n2StringOfRadix (n: bigint) (nBits: uint32) (rad: NumberBase) =
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits |> charList2String
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

/// get values position of bus labels from the transition gaps (for one Waveform)
let private gaps2pos (wSModel: WaveSimModel) (wave: Waveform) gaps =
    let clkWidth = int wSModel.ClkWidth
    let nSpaces (g: {| GapLen: int; GapStart: int |}) = 
        (g.GapLen * clkWidth / (maxBusValGap + 1) + 2)
    let gapAndInd2Pos (g: {| GapLen: int; GapStart: int |}) i =
        float g.GapStart + float i * float g.GapLen / float (nSpaces g)
    gaps
    |> Array.map (fun (gap: {| GapLen: int; GapStart: int |}) ->
        {| Sample = wave.[gap.GapStart]
           XPosArray = Array.map (gapAndInd2Pos gap) [| 1 .. nSpaces gap - 1 |] |} )

/// get values position of bus labels
let private busLabels (wSModel: WaveSimModel) waveData =
    (Array.transpose waveData, Array.map makeGaps (transitions waveData)) 
    ||> Array.map2 (gaps2pos wSModel)

/// get the labels of a waveform for a period in which the value doesn't change
let private busLabelOneValue wsMod (busLabelValAndPos: {| Sample: Sample; XPosArray: float [] |}) =
    let addLabel nLabels xInd = makeText (inWaveLabel nLabels xInd wsMod)
    match busLabelValAndPos.Sample with
    | Wire w when w.NBits > 1u ->
        Array.map (fun xInd -> 
            addLabel 1 xInd (n2StringOfRadix w.BitData w.NBits wsMod.Radix)) busLabelValAndPos.XPosArray
    | _ -> [||]

///////////////////////
/// WaveSim Actions ///
///////////////////////

/// get SVG of a single waveform for one clock cycle
let private makeClkSegment (clkW: float) (xInd: int)  =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW
    let mid = left + float clkW / 2.

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke("blue") ] ]

    let clkPoints = [
        (left, top)
        (mid, top)
        (mid, bot)
        (right, bot)
        (right, top)
    ]

    clkPoints
    |> List.pairwise
    |> List.map (fun (p1,p2) -> makeSigLine p1 p2)
    |> List.toArray




/// get SVG of a single waveform for one clock cycle
let private makeSegment (clkW: float) (xInd: int) (data: Sample) (trans: int * int) =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke("blue") ] ]

    match data with
    | Wire w when w.NBits = 1u ->
        let y =
            match w.BitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | 1 -> [| makeSigLine (right, bot + sigLineThick / 2.0) (right, top - sigLineThick / 2.0) |]
        | 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| sigLine |]
    | _ ->
        let leftInner = if fst trans = 1 then left + transLen else left
        let rightInner = if snd trans = 1 then right - transLen else right
        let cen = (top + bot) / 2.0

        //make lines
        let topL = makeSigLine (leftInner, top) (rightInner, top)
        let botL = makeSigLine (leftInner, bot) (rightInner, bot)
        let topLeft = makeSigLine (left, cen) (leftInner, top)
        let botLeft = makeSigLine (left, cen) (leftInner, bot)
        let topRight = makeSigLine (right, cen) (rightInner, top)
        let botRight = makeSigLine (right, cen) (rightInner, bot)

        match trans with
        | 1, 1 -> [| topLeft; botLeft; topRight; botRight |]
        | 1, 0 -> [| topLeft; botLeft |]
        | 0, 1 -> [| topRight; botRight |]
        | 0, 0 -> [||]
        | _ -> failwith "What? Transition has value other than 0 or 1"
        |> Array.append [| topL; botL |]




/// SVG of the clock numbers above the waveforms
let clkRulerSvg (model: WaveSimModel) =
    let makeClkRulLbl i =
        match model.ClkWidth with
        | clkW when clkW < 0.5 && i % 5 <> 0 -> [||]
        | _ -> [| makeText (cursRectText model i) (string i) |]
    [| 0 .. int model.LastClk |]
    |> Array.collect makeClkRulLbl
    |> Array.append (backgroundSvg model)
    |> makeSvg (clkRulerStyle model)


/// get SVG of a positive edge trigerred CLK waveform
let makeClkSvg (sampArr: Waveform) (wsMod): ReactElement [] =
    [|0.. Array.length sampArr - 1|]
    |> Array.map (makeClkSegment (wsMod.ClkWidth / 2.))
    |> Array.concat

 

/// get SVG of the array of waveforms in wsMod
let waveSvg wsMod waveData  =
    let valueLabels =
        busLabels wsMod waveData
        |> Array.map (Array.collect (busLabelOneValue wsMod))

    let makeWaveSvg (sampArr: Waveform) (transArr: (int * int) []): ReactElement [] =
        (sampArr, transArr)
        ||> Array.mapi2 (makeSegment wsMod.ClkWidth)
        |> Array.concat

    let padTrans t =
        match Array.length t with
        | 0 -> [| 1, 1 |]
        | 1 ->
            [| (1, t.[0])
               (t.[0], 1) |]
        | _ ->
            Array.pairwise t
            |> (fun pairs ->
                Array.concat
                    [ [| 1, fst pairs.[0] |]
                      pairs
                      [| snd (Array.last pairs), 1 |] ])

    transitions waveData
    |> Array.map padTrans
    |> Array.map2 makeWaveSvg (Array.transpose waveData)
    |> Array.map2 Array.append valueLabels


/// Calculate and add the waveform SVGs to the current wsModel.
/// waveSVG and clkRulerSvg are constant functions passed in because defined after this
/// TODO: reorder functions!
let addSVGToWaveSimModel wSModel =
    let waveData = getWaveData wSModel
    let waveTableRow rowClass cellClass svgClass svgChildren =
        tr rowClass [ td cellClass [ makeSvg svgClass svgChildren ] ]
    let bgSvg = backgroundSvg wSModel

    let lastRow = [| waveTableRow [ Class "fullHeight" ] (lwaveCell wSModel) (waveCellSvg wSModel true) bgSvg |]
    let firstRow = [| tr [ Class "rowHeight" ] [ td (waveCell wSModel) [ clkRulerSvg wSModel ] ] |]

    let midRows =
        waveSvg wSModel waveData
        |> Array.map (fun wave ->
                let waveWithBg = Array.append bgSvg wave
                waveTableRow [ Class "rowHeight" ] (waveCell wSModel) (waveCellSvg wSModel false) waveWithBg)
    let svgs = Array.concat [| firstRow ; midRows ; lastRow |]
    {wSModel with DispWaveSVGCache = svgs}


/// add entry with key: current fileName and data: initWS to model.WaveSim
let initFileWS (model:Model) dispatch =
    let netListOpt = getSheetWaveNetList model
    match getCurrFile model,netListOpt with
    | Some fileName, Some netList ->
        (fileName, {initWS with 
                        WaveData =
                            netList2NetGroups netList 
                            |> initWA
                            |> Some})
        |> AddWaveSimFile
        |> dispatch
    | _ -> ()

/// set model.WaveSim & model.WaveAdder to show the list of waveforms that can be selected
let startNewWaveSimulation compIds model wSMod dispatch (simData: SimulationData) (rState:CanvasState) =
    dispatch <| SetViewerWidth minViewerWidth 
    dispatch <| SetLastSimulatedCanvasState (Some rState) 
    SetSimIsStale false |> dispatch
    printfn "***Starting simulation with (%A) canvas***" (rState |> 
                                                         (fun (comps,conns) -> List.length comps, List.length conns))
    let netList = Helpers.getNetList rState
    let netGroups = netList2NetGroups netList
    printfn "***Netgroups=%A***" (Array.length netGroups)
    let wA' =
        { InitWaveSimGraph = Some simData
          AllNetGroups = netGroups  
          AllWaveNames = Array.map (netGroup2Label compIds simData.Graph netList) netGroups }
    { wSMod with
          WaveSimState = true
          WaveData = Some wA'
          LastCanvasState = Some rState }
    |> SetCurrFileWSMod
    |> dispatch
    netGroups
    |> Array.map (fun ng -> false)
    |> Array.map2 (selectNetGrpConns model.Diagram) netGroups |> ignore

    ChangeRightTab WaveSim |> dispatch

/// get wave name labels from waveforms names
let makeLabels waveNames =
    let makeLbl l = label [ Class "waveLbl" ] [ str l ]
    Array.map makeLbl waveNames



                     

/// adjust parameters before feeding them into updateWSMod 
let adjustPars (wsMod: WaveSimModel) (pars: {| LastClk: uint; Curs: uint; ClkW: float |}) rightLim dispatch =
    match wsMod.ClkWidth = pars.ClkW, wsMod.Cursor = pars.Curs, wsMod.LastClk = pars.LastClk with
    // zooming
    | false, true, true -> 
        rightLim / (wsMod.ClkWidth * 40.0)
        |> uint
        |> max wsMod.Cursor
        |> (+) 10u
        |> (fun newClk -> {| pars with LastClk = newClk |})
    // changing cursor
    | true, false, true -> 
        UpdateScrollPos true |> dispatch
        {| pars with LastClk = max pars.LastClk (pars.Curs + 10u) |> min maxLastClk |}
    // generating longer simulation
    | true, true, false -> pars
    // other situations should not occur, by default, don't change parameters
    | _ -> pars

/// update the WaveSimModel entry of the current file with new parameters
let updateWSMod (model: Model) (wsMod: WaveSimModel) 
                (par: {| LastClk: uint; Curs: uint; ClkW: float |}) : WaveSimModel =
    { wsMod with LastClk = par.LastClk
                 SimDataCache = par.LastClk + 1u - uint (Array.length wsMod.SimDataCache)
                           |> appendSimData model wsMod
                           |> function | Some (Ok dat) -> dat
                                       | None -> failwithf "No simulation data when Some are expected"
                                       | Some (Error e) -> failwithf "%A" e
                 Cursor = par.Curs 
                 ClkWidth = par.ClkW }
    |> addSVGToWaveSimModel


/// get waveform names
let private getWaveNames compIds netList (wsMod: WaveSimModel) =
    match wsMod.WaveData with
    | Some {InitWaveSimGraph = Some sD} -> Array.map (netGroup2Label compIds sD.Graph netList) wsMod.DispPorts
    | _ -> [||]

/// change sim data if required
/// call waveCol with the current Simulation Data 
/// to generate the required svgs
/// set Disp* fields to display ports netgroups
let waveGen (wSMod: WaveSimModel) ports =
    let simData', wa = 
        match wSMod.WaveData with
        | Some ({InitWaveSimGraph = Some  sD} as wa) ->
            wSMod.LastClk
            |> extractSimData sD
            |> Array.append [| sD |]
            |> (fun sd -> sd, wa)
        | Some {InitWaveSimGraph = None} -> failwith "waveGen called when WaveAdder.SimData is None"
        | None -> failwith "waveGen called when WaveAdder is None"

    let names =
        Array.zip wa.AllNetGroups wa.AllWaveNames
        |> Array.filter (fun (p, _) -> Array.contains p ports)
        |> Array.map snd

    let wSMod' =
        { wSMod with
            SimDataCache = simData'
            DispWaveNames = names
            DispPorts = ports
            WaveSimState = false }

    addSVGToWaveSimModel wSMod'

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
let private isNLTrgtLstGroupSelected (netList: NetList) ((comps, conns): CanvasState) (trgtLstGroup: NetGroup) =
    Array.append [|trgtLstGroup.driverNet|] trgtLstGroup.connectedNets
    |> Array.exists (isNLTrgtLstSelected netList (comps, conns)) 

/// is the given waveform selected by the current diagram selection
let isWaveSelected (diagram:Draw2dWrapper.Draw2dWrapper) netList (nlTrgtLstGroup: NetGroup) = 
    match diagram.GetSelected() with
    | Some selectedCompsConnsJS ->
        let selectedCompsConns = extractState selectedCompsConnsJS
        isNLTrgtLstGroupSelected netList selectedCompsConns nlTrgtLstGroup
    | _ -> false 



/////////////////////////////////////////////////////
/// Functions fed into FileMenuView View function ///
/////////////////////////////////////////////////////

let getAllNetGroups waveSim = 
    match waveSim.WaveData with
    | None -> [||]
    | Some wa -> wa.AllNetGroups

let getAdderOrInit (model:Model) waveSim =
    match waveSim.WaveData with
    | None -> 
        Option.defaultValue ([],[]) model.LastSimulatedCanvasState
        |> Helpers.getNetList
        |> netList2NetGroups
        |> initWA
    | Some wa -> wa


/// actions triggered whenever the fileMenuView function is executed
let fileMenuViewActions model dispatch =
    if model.ConnsToBeHighlighted
    then  
        match currWaveSimModel model with
        | Some wSModel ->
            let netList = 
                model.LastSimulatedCanvasState
                |> Option.map Helpers.getNetList 
                |> Option.defaultValue (Map.empty)
            if wSModel.WaveSimState then 
                netList2NetGroups netList
                
            else wSModel.DispPorts
            |> Array.map (fun net -> if isWaveSelected model.Diagram (wsModel2netList wSModel) net
                                     then wave2ConnIds net
                                     else [||])
            |> Array.concat
            |> SetSelWavesHighlighted
            |> dispatch
        | _ -> ()
    else ()






///////////////////////////
// Auto-scroll functions //
///////////////////////////

/// returns true when the cursor rectangle is in the visible section of the scrollable div
let isCursorVisible wSMod divWidth scrollPos =
    let cursLeftPos = cursorLeftPx wSMod <| float wSMod.Cursor
    let cursMid = cursLeftPos + (wSMod.ClkWidth * 40.0 / 2.0)
    let leftScreenLim = scrollPos
    let rightScreenLim = leftScreenLim + divWidth
    cursLeftPos >= cursMid && cursMid <= rightScreenLim

/// returns horizontal scrolling position required so that the cursor becomes visible
let makeCursorVisiblePos wSMod divWidth = 
    let cursLeftPos = cursorLeftPx wSMod <| float wSMod.Cursor
    let cursMid = cursLeftPos + (wSMod.ClkWidth * 40.0 / 2.0)
    cursMid - (divWidth / 2.0)

/////////////////////////////////////////
// Functions to manage waveSim state   //
/////////////////////////////////////////


