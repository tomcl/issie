module WaveSimHelpers

open Fulma
open Fable.React
open Fable.React.Props

open DiagramStyle
open DiagramMessageType
open DiagramModelType
open CommonTypes
open FileMenuView
open Extractor
open Simulator
open SimulatorTypes
open System

///////////////////////////////
/// General WaveSim Helpers ///
///////////////////////////////

let private getReducedCanvState model =
    match model.Diagram.GetCanvasState() with
    | Some cS -> Some <| extractReducedState cS
    | None -> None

//////////////////////////
/// Simulation Helpers ///
//////////////////////////

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

let rec private getPortsNet (netList: NetList) port =
    let connectedLblNames = 
        netList.[port.CId].Outputs.[port.OutPN]
        |> List.filter (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Type = IOLabel)
        |> List.map (fun netListTrgt -> netList.[netListTrgt.TargetCompId].Label)
        |> List.distinct
    Map.filter (fun _ (netListComp: NetListComponent) -> 
                    List.contains netListComp.Label connectedLblNames 
                    && netListComp.Type = IOLabel) netList
    |> Map.toList
    |> List.map (fun (cId, _) -> 
        match Map.tryFind (OutputPortNumber 0) netList.[cId].Outputs with
        | Some _ -> 
            getPortsNet netList { CId = cId
                                  OutPN = OutputPortNumber 0
                                  TrgtId = None } 
            |> (fun (a,b) -> a :: b)
        | None -> [] )
    |> List.concat
    |> (fun lst -> port, lst)


let private reloadablePorts (model: Model) (netList: NetList) : PortsNet [] =
    let inGraph (port, _) = Map.containsKey port.CId netList
    match currWS model with
    | Some wSMod ->
        Array.filter inGraph wSMod.Ports
        |> Array.map (fun (port, _) ->
            match port.TrgtId with
            | Some trgtId when Map.containsKey trgtId netList ->
                match List.tryFind (fun nlTrgt -> nlTrgt.TargetCompId = trgtId) 
                         netList.[port.CId].Outputs.[port.OutPN] with
                | Some _ -> port
                | None -> { port with TrgtId = None }
            | _ -> { port with TrgtId = None }
            |> getPortsNet netList)
    | None -> [||]



let private clkAdvance (sD: SimulationData) =
    feedClockTick sD.Graph
    |> (fun graph ->
        { sD with
              Graph = graph
              ClockTickNumber = sD.ClockTickNumber + 1 })

let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> clkAdvance s, clkAdvance s)
    |> fst

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList.[compId].Inputs.[inPortN]

/// get NLSource array of the inputs of a component
let private componentInputNLSources (netList: NetList) componentId =
    netList.[componentId].Inputs
    |> Map.toArray 
    |> Array.choose snd

/// get NLSource array of the outputs of a component
let private componentOutputNLSources (netList: NetList) componentId =
    netList.[componentId].Outputs
    |> Map.toArray
    |> Array.collect (fun (outPortN, nlTrgtLst) -> 
        match nlTrgtLst with
        | [] -> [||]
        | _ -> 
            [| { SourceCompId = componentId
                 OutputPort = outPortN
                 SourceConnId = nlTrgtLst.[0].TargetConnId } |] )

/// get NLSource array of the inputs and outputs of a component
let private componentNLSources (netList: NetList) componentId =
    match Map.tryFind componentId netList with
    | Some _ -> Array.append (componentInputNLSources netList componentId) 
                              (componentOutputNLSources netList componentId)
    | None -> [||]

/// get NLSource array of a connection
let private connectionNLSources (netList: NetList) connectionId =
    let iterateThroughComponentInputs (inPortN, nlSourceOpt) =
        match nlSourceOpt with
        | Some nlSource ->
            if nlSource.SourceConnId = connectionId
            then Some nlSource
            else None
        | None -> None
    let iterateThroughNetList (_, (nlComp: NetListComponent)) =
        Map.toArray nlComp.Inputs
        |> Array.choose iterateThroughComponentInputs
    Map.toArray netList
    |> Array.collect iterateThroughNetList

/// get NLSource array given a tuple of components and connections
let compsConns2nlSources (netList: NetList) ((comps, conns):CanvasState) =
    let sourcesFromComps =
        List.toArray comps
        |> Array.collect (fun (comp: Component) -> 
                                componentNLSources netList (ComponentId comp.Id)) 
    let sourcesFromConns = 
        List.toArray conns
        |> Array.collect (fun (conn: Connection) -> 
                                connectionNLSources netList (ConnectionId conn.Id))
    Array.append sourcesFromComps sourcesFromConns
    |> Array.distinct

/// get array of available NLSource in current canvas state
(*let avalPorts (model: Model) =
    match model.Diagram.GetCanvasState(), model.CurrProject with
    | None, _ -> [||]
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents = project.LoadedComponents |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> function
        | Ok simData ->
            List.map (extractComponent >> Comp) (fst jsState) 
            |> compsConns2portLst simData (extractState jsState)
            |> Array.filter (fun port -> match simData.Graph.[port.CId].Type with
                                         | SplitWire _ | MergeWires _ -> false
                                         | IOLabel -> false
                                         | _ -> true )
            |> Array.map (getPortsNet simData.Graph)
        | Error _ -> [||]*)


let private simWireData2Wire wireData =
    wireData
    |> List.mapFold (fun weight bit ->
        match bit with
        | SimulatorTypes.Bit.Zero -> bigint 0
        | SimulatorTypes.Bit.One -> weight
        |> (fun r -> r, weight * (bigint 2))) (bigint 1)
    |> fst
    |> List.sum

let extractSimTime portNets (simGraph: SimulationGraph) =
    Array.map fst portNets
    |> Array.map (fun { CId = compId; OutPN = portN; TrgtId = _ } ->
        match Map.tryFind compId simGraph with
        | Some simComp ->
            match Map.tryFind portN simComp.Outputs with
            | Some(hd :: _) ->
                let wD = simGraph.[fst hd].Inputs.[snd hd]
                Wire
                    { NBits = uint (List.length wD)
                      BitData = simWireData2Wire wD }
            | Some [] -> failwith "Output not connected"
            | None -> failwith "Component doesn't have this output port number"
        | None -> failwith "ComponentId not in simulation graph")

let makeWaveData (wsMod: WaveSimModel) =
    Array.map (fun sD -> sD.Graph) wsMod.SimData
    |> Array.map (extractSimTime wsMod.Ports) 

let port2ConnId (model: Model) (p: WaveSimPort) =
    match model.Diagram.GetCanvasState() with
    | Some s ->
        let outPN =
            match p.OutPN with
            | OutputPortNumber n -> n
        List.map extractComponent (fst s)
        |> List.tryPick (fun c ->
            match ComponentId c.Id = p.CId with
            | true -> Some c.OutputPorts.[outPN].Id
            | false -> None)
        |> function
        | Some portId ->
            List.map extractConnection (snd s)
            |> List.filter (fun conn -> conn.Source.Id = portId)
            |> List.map (fun conn -> ConnectionId conn.Id)
        | None -> []
    | None -> failwith "highlight called when canvas state is None"

let portNet2ConnId (model: Model) ((pMain, pOthers): PortsNet) =
    List.collect (port2ConnId model) (pMain :: pOthers)

let private appendSimData (wSMod: WaveSimModel) nCycles = 
    extractSimData (Array.last wSMod.SimData) nCycles 
    |> Array.append wSMod.SimData


let private connId2JSConn (model: Model) connId =
    match model.Diagram.GetCanvasState() with
    | Some (_, jsConns) -> 
        List.tryFind (fun jsConn -> (extractConnection jsConn).Id = connId) jsConns
    | None -> None
    |> function
       | Some jsConn -> [ jsConn ]
       | None -> []

let private selWave2connIds model (wSMod: WaveSimModel) ind = 
    if wSMod.WaveAdderOpen 
    then wSMod.WaveAdder.Ports.[ind]
    else wSMod.Ports.[ind]
    |> portNet2ConnId model

let selWave2selConn model wSMod ind on =
    selWave2connIds model wSMod ind
    |> List.collect (fun (ConnectionId cId) -> connId2JSConn model cId) 
    |> model.Diagram.SetSelected on

///////////////////////////
/// Naming of waveforms ///
///////////////////////////

let rec findName 
        (compIds: ComponentId Set)
        (netList: NetList)
        ({ CId = compId; OutPN = outPortN; TrgtId = outputOpt }: WaveSimPort)
        : string option * ((string *(int*int)) list) =

    if not (Set.contains compId compIds) then
        None,[] // component is no longer in circuit due to changes
    else
        let compLbl =
            match Map.tryFind compId netList with
            | Some nlComp ->
                match Seq.tryFindIndexBack ((=) '(') nlComp.Label with
                | Some i -> nlComp.Label.[0..i - 1]
                | None -> nlComp.Label
            | None -> failwith "simData.Graph.[compId] doesn't exist"

        let outPortInt =
            match outPortN with
            | OutputPortNumber pn -> pn

        let driveName n compTypeStr =
            match drivingOutput netList compId (InputPortNumber n) with
            | Some Source ->
                findName compIds simGraph
                    { CId = driveCompId
                      OutPN = drivePortN
                      TrgtId = None }
                |> snd
            | None -> failwith (compTypeStr + "input not connected")

        match simGraph.[compId].Type with
        | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 -> 
            [ compLbl, (0, 0) ]
        | Input w | Output w | Constant(w, _) -> 
            [ compLbl, (w - 1, 0) ]
        | Demux2 -> [ compLbl + "_" + string outPortInt, (0, 0) ]
        | NbitsAdder w ->
            match outPortInt with
            | 0 -> [ compLbl + "_sum", (w - 1, 0) ]
            | _ -> [ compLbl + "Cout", (w - 1, 0) ]
        | DFF | DFFE -> 
            [ compLbl + "_Q", (0, 0) ]
        | Register w | RegisterE w -> 
            [ compLbl + "_data-out", (w - 1, 0) ]
        | RAM mem | AsyncROM mem | ROM mem -> 
            [ compLbl + "_data-out", (mem.WordWidth - 1, 0) ]
        | Custom c -> 
            [ compLbl + "_" + fst c.OutputLabels.[outPortInt], (snd c.OutputLabels.[outPortInt] - 1, 0) ]
        | IOLabel ->
            match driveOut simGraph compId (InputPortNumber 0) with
            | Some(driveCompId, drivePortN) ->
                match findName compIds simGraph
                          { CId = driveCompId
                            OutPN = drivePortN
                            TrgtId = None }
                      |> snd with
                | hd :: tl ->
                    ("(" + fst hd, snd hd) :: tl
                    |> function
                    | hd :: [] -> (fst hd + ")", snd hd) :: []
                    | lst ->
                        List.append lst.[0..List.length lst - 2] [ fst (List.last lst) + ")", snd (List.last lst) ]
                | [] -> failwith "Error: IOLabel input names list is empty"
            | None -> failwith "IOLabel input not connected"
        | MergeWires -> 
            List.append (driveName 1 "MergeWires") (driveName 0 "MergeWires")
        | SplitWire w ->
            let predicate (_, b) =
                match outPortInt with
                | 0 -> b >= w
                | 1 -> b < w
                | _ -> failwith "SplitWire output port number greater than 1"

            let split name msb lsb st =
                List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                |> List.filter predicate
                |> List.unzip
                |> function
                | [], _ -> None
                | lst, _ -> Some(name, (List.max lst, List.min lst))

            (0, driveName 0 "SplitWire")
            ||> List.mapFold (fun st (name, (msb, lsb)) -> split name msb lsb st, st + msb - lsb + 1)
            |> fst
            |> List.choose id
        | BusSelection(w, oLSB) ->
            let filtSelec name msb lsb st =
                List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
                |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
                |> List.unzip
                |> function
                | [], _ -> None
                | lst, _ -> Some(name, (List.max lst, List.min lst))
            (driveName 0 "BusSelection", 0)
            ||> List.mapFoldBack (fun (name, (msb, lsb)) st -> filtSelec name msb lsb st, st + msb - lsb + 1)
            |> fst
            |> List.choose id
            |> List.rev

        |> (fun lst ->
            match outputOpt with
            | Some compId ->
                match Map.tryFind compId simGraph with
                | None -> None, lst
                | Some {Label = ComponentLabel lbl} -> Some(lbl + ": "), lst
            | None -> None, lst)

let private bitNums (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let wSPort2Name compIds simGraph p =
    let outNameOpt, nameLst = findName compIds simGraph p
    let tl =
        match nameLst with
        | [ el ] -> fst el + bitNums (snd el)
        | lst when List.length lst > 0 ->
            let appendName st (name, bitLims) = st + name + bitNums bitLims + ", "
            List.fold appendName "{ " lst |> (fun lbl -> lbl.[0..String.length lbl - 3] + " }")
        |  _ -> ""
    match outNameOpt with
    | Some outName -> outName + tl
    | None -> tl

let limBits (name: string): (int * int) option =
    match Seq.tryFind ((=) '[') name, Seq.tryFind ((=) ':') name, Seq.tryFind ((=) ']') name with
    | Some _, Some _, Some _->
        (name.[Seq.findIndexBack ((=) '[') name + 1..Seq.findIndexBack ((=) ':') name - 1],
         name.[Seq.findIndexBack ((=) ':') name + 1..Seq.findIndexBack ((=) ']') name - 1])
        |> (fun (a, b) -> int a, int b)
        |> Some
    | _ -> None

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

////////////////////////
/// Radix conversion ///
////////////////////////


let dec2binAlt (n:bigint) (nBits:uint32) =
    [nBits - 1u..0u]
    |> List.map (fun bitNum -> if n &&& (1I <<< int bitNum) = 0I then '0' else '1')

let dec2bin (n: bigint) (nBits: uint32): string =
    let folder (state: bigint * char list) (digit: int) =
        if fst state / bigint digit = bigint 1
        then (fst state - bigint digit, List.append (snd state) [ '1' ])
        else (fst state, List.append (snd state) [ '0' ])
    [ float nBits - 1.0 .. (-1.0) .. 0.0 ]
    |> List.map ((fun exp -> 2.0 ** exp) >> (fun f -> int f))
    |> List.fold folder (n, [])
    |> snd
    |> List.toSeq
    |> Seq.map string
    |> String.concat ""

let dec2hex (n: bigint) (nBits: uint32): string =
    let seqPad = 
        let times = (4 - int nBits % 4) % 4
        Seq.replicate times '0'

    let paddedBin =
        dec2bin n nBits
        |> Seq.append seqPad
        |> Seq.toList

    let fourBit2HexDigAlt =
        let bit vec n = if (1 <<< n) &&& vec = 0 then '0' else '1'
        let hexDigOf n = (sprintf "%x" n).[0]
        [0..15]
        |> List.map (fun dig -> 
                List.map (bit dig) [3..0], hexDigOf dig)


    let fourBit2HexDig =
        [ [ '0'; '0'; '0'; '0' ], '0'
          [ '0'; '0'; '0'; '1' ], '1'
          [ '0'; '0'; '1'; '0' ], '2'
          [ '0'; '0'; '1'; '1' ], '3'
          [ '0'; '1'; '0'; '0' ], '4'
          [ '0'; '1'; '0'; '1' ], '5'
          [ '0'; '1'; '1'; '0' ], '6'
          [ '0'; '1'; '1'; '1' ], '7'
          [ '1'; '0'; '0'; '0' ], '8'
          [ '1'; '0'; '0'; '1' ], '9'
          [ '1'; '0'; '1'; '0' ], 'A'
          [ '1'; '0'; '1'; '1' ], 'B'
          [ '1'; '1'; '0'; '0' ], 'C'
          [ '1'; '1'; '0'; '1' ], 'D'
          [ '1'; '1'; '1'; '0' ], 'E'
          [ '1'; '1'; '1'; '1' ], 'F' ]
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

let radixChange (n: bigint) (nBits: uint32) (rad: NumberBase) =
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits


///////////////////
/// Transitions ///
///////////////////



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

let transitions waveData = //relies on number of names being correct (= length of elements in WaveData)
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

///////////////////////
/// WaveSim Actions ///
///////////////////////

let initFileWS model dispatch =
    match getCurrFile model with
    | Some fileName ->
        (fileName, initWS)
        |> AddWaveSimFile
        |> dispatch
    | None -> ()

let private setWA compIds model wSMod dispatch simData =
    SetViewerWidth minViewerWidth |> dispatch
    getReducedCanvState model |> SetLastSimulatedCanvasState |> dispatch
    SetSimIsStale false |> dispatch

    let wA' =
        let wSPorts = avalPorts model
        let names' = Array.map (fst >> wSPort2Name compIds simData.Graph) wSPorts
        { SimData = Some simData; Ports = wSPorts; WaveNames = names' }
    { wSMod with
          WaveAdder = wA'
          LastCanvasState = getReducedCanvState model }
    |> SetCurrFileWSMod
    |> dispatch

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

let updateWSMod  waveSvg clkRulerSvg (model: Model) (wsMod: WaveSimModel) 
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
    |> (fun m -> { m with WaveTable = waveCol waveSvg clkRulerSvg model m (makeWaveData m) })

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

    { wSMod' with WaveTable = waveCol waveSvg clkRulerSvg model wSMod' (makeWaveData wSMod') }

//////////////////////////////////////
/// Interaction with Model.Diagram ///
//////////////////////////////////////




let getSelected model: DiagEl list =
    match model.Diagram.GetSelected() with
    | None -> []
    | Some jsState ->
        (fst jsState |> List.map (extractComponent >> Comp), 
         snd jsState |> List.map (extractConnection >> Conn))
        ||> List.append

let isWaveSelected model (wSMod: WaveSimModel) (portsNet: PortsNet) = 
    let ports =
        (fst portsNet) :: (snd portsNet)
    let simD = 
        match wSMod.WaveAdder.SimData with
        | Some sD -> sD
        | None -> failwith "isWaveSelected called when WaveAdder.SimData is None"
    let canvState =
        match wSMod.LastCanvasState with
        | Some cS -> cS
        | _ -> failwith "isWaveSelected called when wSMod.LastCanvasState is None"
    let selectedPorts = 
        getSelected model
        |> compsConns2portLst simD canvState
    let portsEqual p1 p2 =
        p1.CId = p2.CId && p1.OutPN = p2.OutPN
    List.exists (fun p -> Array.exists (portsEqual p) selectedPorts) ports

////////////////////////////
//// Saving WaveSim Model ////
////////////////////////////


let wsModel2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    { Ports = wsMod.Ports
      ClkWidth = wsMod.ClkWidth
      Cursor = wsMod.Cursor
      Radix = wsMod.Radix
      LastClk = wsMod.LastClk
      WaveAdderOpen = wsMod.WaveAdderOpen
      WaveAdderPorts = wsMod.WaveAdder.Ports }

let savedWaveInfo2wsModel compIds waveSvg clkRulerSvg model (sWInfo: SavedWaveInfo) : WaveSimModel =
    match makeSimData model with
    | Some (Ok sD) ->
        let ports' = reloadablePorts model sD
        let sD' = Array.append [| sD |] (extractSimData sD sWInfo.LastClk)
        let waPorts' = avalPorts model
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
                        WaveNames = Array.map (fst >> wSPort2Name compIds sD.Graph) waPorts' }
          LastCanvasState = getReducedCanvState model }
    | Some (Error err) -> initWS//Should probably display error somehow
    | None -> initWS
    |> (fun m -> { m with WaveTable = waveCol waveSvg clkRulerSvg model m (makeWaveData m) } )

/////////////////////////////////////////////////////
/// Functions fed into FileMenuView View function ///
/////////////////////////////////////////////////////

let fileMenuViewActions model dispatch =
    match model.SimulationInProgress with
    | Some par -> SimulateWhenInProgress par |> dispatch
    | None -> ()

    if model.ConnsToBeHighlighted
    then  
        match currWS model with
        | Some wSMod ->
            if wSMod.WaveAdderOpen then wSMod.WaveAdder.Ports else wSMod.Ports
            |> Array.mapi (fun i net -> if isWaveSelected model wSMod net
                                         then selWave2connIds model wSMod i
                                         else [])
            |> List.concat
            |> SetSelWavesHighlighted
            |> dispatch
        | _ -> ()
    else ()

let simulateButtonFunc compIds model dispatch =
    match model.SimulationIsStale,currWS model, makeSimData model with
    | true, Some wSMod, Some (Ok simData) ->
                Button.button
                    [ Button.Color IsSuccess
                      Button.OnClick(fun _ ->
                          setWA compIds model wSMod dispatch simData
                          ChangeRightTab WaveSim |> dispatch) ]
    | true, Some _, Some (Error err) -> 
                Button.button
                    [ Button.OnClick(fun _ ->
                          Some err |> SetWSError |> dispatch
                          ChangeRightTab WaveSim |> dispatch ) ]
    | _, None, _ -> 
              match model.CurrProject with
              | Some _ -> initFileWS model dispatch
              | None -> ()
              Button.button []
    | _ -> Button.button []
    |> (fun but -> but [ str "Waveforms >>" ])