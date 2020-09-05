(*
    FileMenuView.fs

    View for the top menu, and related functionalities.
*)

module FileMenuView

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open DiagramStyle
open DiagramMessageType
open DiagramModelType
open CommonTypes
open FilesIO
open Extractor
open PopupView
open Simulator
open SimulatorTypes
open System

let getCurrFile (model: Model) =
    match model.CurrProject with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let currWS (model: Model) =
    match getCurrFile model with
    | Some fileName when Map.containsKey fileName (fst model.WaveSim) -> Some (fst model.WaveSim).[fileName]
    | _ -> None

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
        
let private displayFileErrorNotification err dispatch =
    errorNotification err CloseFilesNotification
    |> SetFilesNotification
    |> dispatch

let private loadStateIntoCanvas state model dispatch =
    dispatch <| SetHighlighted([], []) // Remove current highlights.
    model.Diagram.ClearCanvas() // Clear the canvas.
    // Finally load the new state in the canvas.
    let components, connections = state
    List.map model.Diagram.LoadComponent components |> ignore
    List.map (model.Diagram.LoadConnection true) connections |> ignore
    model.Diagram.FlushCommandStack() // Discard all undo/redo.
    // Run the a connection widhts inference.
    InferWidths()
    |> JSDiagramMsg
    |> dispatch
    // Set no unsaved changes.
    SetHasUnsavedChanges false
    |> JSDiagramMsg
    |> dispatch
    // set autosave status clean

//helpers

let private getReducedCanvState model =
    match model.Diagram.GetCanvasState() with
    | Some cS -> Some <| extractReducedState cS
    | None -> None

// saving/loading

let private wsMod2SavedWaveInfo (wsMod: WaveSimModel) : SavedWaveInfo =
    { Ports = wsMod.Ports
      ClkWidth = wsMod.ClkWidth
      Cursor = wsMod.Cursor
      Radix = wsMod.Radix
      LastClk = wsMod.LastClk
      WaveAdderOpen = wsMod.WaveAdderOpen
      WaveAdderPorts = wsMod.WaveAdder.Ports }

/// extract SavedwaveInfo from model to be saved
let getSavedWave (model:Model): SavedWaveInfo option = 
    match currWS model with
    | Some wSMod -> wsMod2SavedWaveInfo wSMod |> Some
    | None -> None

let rec private getPortsNet (graph: SimulationGraph) port =
    let connectedLbls = 
        graph.[port.CId].Outputs.[port.OutPN]
        |> List.filter (fun (cId, _) -> graph.[cId].Type = IOLabel)
    let connectedLblNames =
        connectedLbls
        |> List.map (fun (cId, _) -> graph.[cId].Label)
        |> List.distinct
    Map.filter (fun _ comp -> List.contains comp.Label connectedLblNames 
                              && comp.Type = IOLabel) graph
    |> Map.toList
    |> List.map (fun (cId, _) -> 
        match Map.tryFind (OutputPortNumber 0) graph.[cId].Outputs with
        | Some _ -> 
            getPortsNet graph { CId = cId
                                OutPN = OutputPortNumber 0
                                TrgtId = None } 
            |> (fun (a,b) -> a :: b)
        | None -> [] )
    |> List.concat
    |> (fun lst -> port, lst)

let private reloadablePorts (model: Model) (simData: SimulatorTypes.SimulationData) : PortsNet [] =
    let inGraph (port, _) = Map.containsKey port.CId simData.Graph
    match currWS model with
    | Some wSMod ->
        Array.filter inGraph wSMod.Ports
        |> Array.map (fun (port, _) ->
            match port.TrgtId with
            | Some trgtId when Map.exists (fun key _ -> key = trgtId) simData.Graph ->
                match List.tryFind (fun (cid, _) -> cid = trgtId) 
                         simData.Graph.[port.CId].Outputs.[port.OutPN] with
                | Some _ -> port
                | None -> { port with TrgtId = None }
            | _ -> { port with TrgtId = None }
            |> getPortsNet simData.Graph)
    | None -> [||]

let private clkAdvance (sD: SimulatorTypes.SimulationData) =
    feedClockTick sD.Graph
    |> (fun graph ->
        { sD with
              Graph = graph
              ClockTickNumber = sD.ClockTickNumber + 1 })

let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> clkAdvance s, clkAdvance s)
    |> fst

/// Returns a tuple option representing the output to which the target input is connected
let private driveOut simGraph targetCompId inPortN =
    Map.toArray simGraph
    |> Array.tryPick (fun (outCompId, simComp: SimulatorTypes.SimulationComponent) ->
        Map.toArray simComp.Outputs
        |> Array.tryFind (fun (_, lst) -> (List.exists (fun t -> t = (targetCompId, inPortN)) lst))
        |> function
        | Some(outPN, _) -> Some(outCompId, outPN)
        | None -> None)

let private procIns (simGraph: SimulationGraph) compId inputs: WaveSimPort [] =
    Array.collect (fun portN ->
        match simGraph.[compId].Type, driveOut simGraph compId portN with
        | Input _, _ -> [||]
        | Output _, Some(cId, oPN) ->
            [| { CId = cId
                 OutPN = oPN
                 TrgtId = Some compId } |]
        | _, Some(cId, oPN) ->
            [| { CId = cId
                 OutPN = oPN
                 TrgtId = None } |]
        | _, None -> failwith "Input is not connected") inputs

let private procCompIns simData (compId: ComponentId) (inputs: Map<InputPortNumber, WireData>): WaveSimPort [] =
    Map.toArray inputs
    |> Array.map fst
    |> procIns simData.Graph compId

let private procOuts compId outputs: WaveSimPort [] =
    Map.toArray outputs
    |> Array.map (fun (portNum, _) ->
        { CId = compId
          OutPN = portNum
          TrgtId = None })

let private processComp simData cId: WaveSimPort [] =
    match Map.tryFind cId simData.Graph with
    | Some sC -> Array.append (procCompIns simData cId sC.Inputs) 
                              (procOuts cId sC.Outputs)
    | None -> [||]

let private remDuplicates (arrWithDup: WaveSimPort []) : WaveSimPort [] =
    Array.groupBy (fun (p: WaveSimPort) -> p.CId, p.OutPN) arrWithDup
    |> Array.map (fun (_, (ports: WaveSimPort [])) -> 
        { ports.[0] with TrgtId = Array.tryPick (fun (p: WaveSimPort) -> p.TrgtId) ports })

let compsConns2portLst simData canvState diagElLst : WaveSimPort [] =
    let portId2CIdInPN pId =
        fst canvState
        |> List.tryPick (fun c ->
                List.tryFindIndex (fun (p: Port) -> p.Id = pId) c.InputPorts
                |> function
                | Some i -> Some(c.Id, i)
                | None -> None)

    diagElLst
    |> List.toArray
    |> Array.collect (fun compEl ->
        match compEl with
        | Comp c -> processComp simData (ComponentId c.Id)
        | Conn c ->
            match portId2CIdInPN c.Target.Id with
            | Some(cId, inPN) -> procIns simData.Graph (ComponentId cId) [| InputPortNumber inPN |]
            | None -> [||])
    |> remDuplicates

let avalPorts (model: Model) =
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
        | Error _ -> [||]


let rec findName 
        (compIds: ComponentId Set)
        (simGraph: SimulatorTypes.SimulationGraph)
        ({ CId = compId; OutPN = outPortN; TrgtId = outputOpt }: WaveSimPort)
        : string option * ((string *(int*int)) list) =

    if not (Set.contains compId compIds) then
        None,[] // component is no longer in circuit due to changes
    else
        let compLbl =
            match Map.tryFind compId simGraph with
            | Some simComp ->
                match simComp.Label with
                | ComponentLabel lbl ->
                    match Seq.tryFindIndexBack ((=) '(') lbl with
                    | Some i -> lbl.[0..i - 1]
                    | None -> lbl //not robust!
            | None -> failwith "simData.Graph.[compId] doesn't exist"

        let outPortInt =
            match outPortN with
            | OutputPortNumber pn -> pn

        let driveName n compTypeStr =
            match driveOut simGraph compId (InputPortNumber n) with
            | Some(driveCompId, drivePortN) ->
                findName compIds simGraph
                    { CId = driveCompId
                      OutPN = drivePortN
                      TrgtId = None }
                |> snd
            | None -> failwith (compTypeStr + "input not connected")

        match simGraph.[compId].Type with
        | Not
        | And
        | Or
        | Xor
        | Nand
        | Nor
        | Xnor
        | Decode4
        | Mux2 -> [ compLbl, (0, 0) ]
        | Input w
        | Output w 
        | Constant(w, _) -> [ compLbl, (w - 1, 0) ]
        | Demux2 -> [ compLbl + "_" + string outPortInt, (0, 0) ]
        | NbitsAdder w ->
            match outPortInt with
            | 0 -> [ compLbl + "_sum", (w - 1, 0) ]
            | _ -> [ compLbl + "Cout", (w - 1, 0) ]
        | DFF
        | DFFE -> [ compLbl + "_Q", (0, 0) ]
        | Register w
        | RegisterE w -> [ compLbl + "_data-out", (w - 1, 0) ]
        | RAM mem
        | AsyncROM mem
        | ROM mem -> [ compLbl + "_data-out", (mem.WordWidth - 1, 0) ]
        | Custom c -> [ compLbl + "_" + fst c.OutputLabels.[outPortInt], (snd c.OutputLabels.[outPortInt] - 1, 0) ]
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
        | MergeWires -> List.append (driveName 1 "MergeWires") (driveName 0 "MergeWires")
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

let private backgroundSvg (model: WaveSimModel) =
    let clkLine x = makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1u .. model.LastClk + 1u |] |> Array.map ((fun x -> float x * model.ClkWidth) >> clkLine)

//radix change

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

let busLabels (model: Model) waveData =
    match currWS model with
    | Some wSMod ->
        let clkWidth = int wSMod.ClkWidth

        let gaps2pos (wave: Waveform) gaps =
            let nSpaces (g: {| GapLen: int; GapStart: int |}) = (g.GapLen * clkWidth / (maxBusValGap + 1) + 2)
            let gapAndInd2Pos (g: {| GapLen: int; GapStart: int |}) i =
                float g.GapStart + float i * float g.GapLen / float (nSpaces g)
            gaps
            |> Array.map (fun (gap: {| GapLen: int; GapStart: int |}) ->
                wave.[gap.GapStart], Array.map (gapAndInd2Pos gap) [| 1 .. nSpaces gap - 1 |])
        (Array.transpose waveData, Array.map makeGaps (transitions waveData)) ||> Array.map2 gaps2pos
    | None -> failwith "busLabels called when currWS model is None"

let makeSegment (clkW: float) (xInd: int) (data: Sample) (trans: int * int) =
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
        let leftInner =
            if fst trans = 1 then left + transLen else left
        let rightInner =
            if snd trans = 1 then right - transLen else right

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
//Probably should put other option for negative number which prints an error


let waveSvg model wsMod waveData =
    let addLabel nLabels xInd = makeText (inWaveLabel nLabels xInd wsMod)

    let valueLabels =
        let lblEl (sample, xIndArr) =
            match sample with
            | Wire w when w.NBits > 1u ->
                Array.map (fun xInd -> addLabel 1 xInd (radixChange w.BitData w.NBits wsMod.Radix)) xIndArr
            | _ -> [||]
        busLabels model waveData
        |> Array.map (Array.collect lblEl)

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

let private clkRulerSvg (model: WaveSimModel) =
    let makeClkRulLbl i =
        match model.ClkWidth with
        | clkW when clkW < 0.5 && i % 5 <> 0 -> [||]
        | _ -> [| makeText (cursRectText model i) (string i) |]
    [| 0 .. int model.LastClk |]
    |> Array.collect makeClkRulLbl
    |> (fun arr ->
        [ backgroundSvg model
          [| makeRect (cursRectStyle model) |]
          arr ])
    |> Array.concat
    |> makeSvg (clkRulerStyle model)

let private waveCol model wsMod waveData =
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

let private savedWaveInfo2wsMod compIds model (sWInfo: SavedWaveInfo) : WaveSimModel =
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
    |> (fun m -> { m with WaveTable = waveCol model m (makeWaveData m) } )

/// add waveInfo to model
let setSavedWave compIds (wave: SavedWaveInfo option) model : Model =
    match wave, getCurrFile model with
    | None, _ -> model
    | Some waveInfo, Some fileName -> 
        { model with WaveSim = Map.add fileName (savedWaveInfo2wsMod compIds model waveInfo) (fst model.WaveSim), 
                               snd model.WaveSim }
    | Some waveInfo, _ -> model
            

/// Save the file currently open.


let saveOpenFileAction isAuto model =
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ | _, None -> ()
    | Some jsState, Some project ->
        extractState jsState
        |> (fun state -> 
                let savedState = state, getSavedWave model
                if isAuto then
                    saveAutoStateToFile project.ProjectPath project.OpenFileName savedState
                else 
                    saveStateToFile project.ProjectPath project.OpenFileName savedState
                    removeFileWithExtn ".dgmauto" project.ProjectPath project.OpenFileName)


let private getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Create a new empty .dgm file and return corresponding loaded component.
let private createEmptyDiagramFile projectPath name =
    createEmptyDgmFile projectPath name

    {   
        Name = name
        TimeStamp = System.DateTime.Now
        WaveInfo = None
        FilePath = pathJoin [| projectPath; name + ".dgm" |]
        CanvasState = [],[]
        InputLabels = []
        OutputLabels = []
    }

let updateLoadedComponents name setFun lcLst =
    let n = List.tryFindIndex (fun lc -> lc.Name = name) lcLst
    match n with
    | None -> failwithf "Can't find name='%s' in components:%A" name lcLst
    | Some n ->
        List.mapi (fun i x -> if i = n then setFun x else x) lcLst

let setupProject (pPath:string) (ldComps: LoadedComponent list) (model: Model) (dispatch: Msg->Unit)=
    let openFileName, openFileState =
        match ldComps with
        | [] -> // No files in the project. Create one and open it.
            createEmptyDgmFile pPath "main"
            "main", ([],[])
        | comps ->
            // load the most recently saved file
            let comp = comps |> List.maxBy (fun comp -> comp.TimeStamp)
            comp.Name, comp.CanvasState
    dispatch EndSimulation // End any running simulation.
    //
    loadStateIntoCanvas openFileState model dispatch
    {
        ProjectPath = pPath
        OpenFileName =  openFileName
        LoadedComponents = ldComps
    }
    |> SetProject |> dispatch

/// Open the specified file.
let private openFileInProject name project model dispatch =
    match getFileInProject name project with
    | None -> log <| sprintf "Warning: openFileInProject could not find the component %s in the project" name
    | Some loadedComponent ->
        saveOpenFileAction false model
        // make sure correct file gets opened.
        let lcs = updateLoadedComponents name (fun lc -> {lc with TimeStamp = DateTime.Now}) project.LoadedComponents
        setupProject project.ProjectPath lcs model dispatch
        dispatch EndSimulation // End any running simulation.

/// Remove file.
let private removeFileInProject name project model dispatch =
    removeFile project.ProjectPath name
    removeFile project.ProjectPath (name + "auto")
    // Remove the file from the dependencies and update project.
    let newComponents = List.filter (fun lc -> lc.Name <> name) project.LoadedComponents
    // Make sure there is at least one file in the project.
    let newComponents =
        match List.isEmpty newComponents with
        | false -> newComponents
        | true -> [ (createEmptyDiagramFile project.ProjectPath "main") ]

    let project = { project with LoadedComponents = newComponents }
    project
    |> SetProject
    |> dispatch
    // If the file was displayed, open and display another one instead.
    // It is safe to access position 0 as we are guaranteed that there is at
    // least one element in newComponents.
    assertThat (not <| List.isEmpty project.LoadedComponents) "removeFileInProject"
    match name = project.OpenFileName with
    | false -> ()
    | true -> openFileInProject project.LoadedComponents.[0].Name project model dispatch

/// Create a new file in this project and open it automatically.
let addFileToProject model dispatch =
    match model.CurrProject with
    | None -> log "Warning: addFileToProject called when no project is currently open"
    | Some project ->
        // Prepare dialog popup.
        let title = "Add sheet to project"

        let before =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData

                let maybeWarning =
                    if isFileInProject dialogText project
                    then div [ Style [ Color "red" ] ] [ str "This sheet already exists." ]
                    else div [] []
                div []
                    [ str "A new sheet will be created at:"
                      br []
                      str <| pathJoin
                                 [| project.ProjectPath
                                    dialogText + ".dgm" |]
                      maybeWarning ]

        let placeholder = "Insert design sheet name"
        let body = dialogPopupBodyOnlyText before placeholder dispatch
        let buttonText = "Add"

        let buttonAction =
            fun (dialogData: PopupDialogData) ->
                // Save current file.
                saveOpenFileAction false model
                // Create empty file.
                let name = getText dialogData
                createEmptyDgmFile project.ProjectPath name
                // Add the file to the project.
                let newComponent = {
                    Name = name
                    TimeStamp = System.DateTime.Now
                    WaveInfo = None
                    FilePath = pathJoin [|project.ProjectPath; name + ".dgm"|]
                    CanvasState = [],[]
                    InputLabels = []
                    OutputLabels = []
                }
                let updatedProject =
                    { project with
                          LoadedComponents = newComponent :: project.LoadedComponents
                          OpenFileName = name }
                // Update the project.
                updatedProject
                |> SetProject
                |> dispatch
                // Open the file.
                openFileInProject name updatedProject model dispatch
                // Close the popup.
                dispatch ClosePopup
                dispatch EndSimulation // End any running simulation.

        let isDisabled =
            fun (dialogData: PopupDialogData) ->
                let dialogText = getText dialogData
                (isFileInProject dialogText project) || (dialogText = "")

        dialogPopup title body buttonText buttonAction isDisabled dispatch

/// Close current project, if any.
let private closeProject model dispatch _ =
    dispatch EndSimulation // End any running simulation.
    dispatch CloseProject
    model.Diagram.ClearCanvas()

/// Create a new project.
let private newProject model dispatch _ =
    match askForNewProjectPath() with
    | None -> () // User gave no path.
    | Some path ->
        match tryCreateFolder path with
        | Error err ->
            log err
            let errMsg = "Could not create a folder for the project."
            displayFileErrorNotification errMsg dispatch
        | Ok _ ->
            dispatch EndSimulation // End any running simulation.
            // Create empty placeholder projectFile.
            let projectFile = baseName path + ".dprj"
            writeFile (pathJoin [| path; projectFile |]) ""
            // Create empty initial diagram file.
            let initialDiagram = createEmptyDiagramFile path "main"
            // Load the diagram.
            loadStateIntoCanvas initialDiagram.CanvasState model dispatch
            // Add the file to the project.
            { ProjectPath = path
              OpenFileName = "main"
              LoadedComponents = [ initialDiagram ] }
            |> SetProject
            |> dispatch







/// work out what to do opening a file
let rec resolveComponentOpenPopup 
        (pPath:string)
        (components: LoadedComponent list)  
        (resolves: LoadStatus list) 
        (model: Model)
        (dispatch: Msg -> Unit) =
    dispatch ClosePopup
    match resolves with
    | [] -> setupProject pPath components model dispatch
    | Resolve (ldComp,autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let buttonAction autoSave _ =
            let comp = if autoSave then autoComp else ldComp
            let data =  stateToJsonString (comp.CanvasState,comp.WaveInfo)
            writeFile comp.FilePath data
            resolveComponentOpenPopup pPath (comp :: components) rLst  model dispatch   
        // special case when autosave data is most recent
        let title = "Warning!"
        let body = str <|  sprintf "Warning: changes were made to sheet '%s' after your last Save. \
                                There is an automatically saved version which is \
                                more uptodate. Do you want to keep the newer AutoSaved version or \
                                the older saved version?"  ldComp.Name  
        choicePopup title body "Newer AutoSaved file" "Older Saved file" buttonAction dispatch
    | OkAuto autoComp :: rLst ->
         let errMsg = "Could not load saved project file '%s' - using autosave file instead"
         displayFileErrorNotification errMsg dispatch
         resolveComponentOpenPopup pPath (autoComp::components) rLst model dispatch
    | OkComp comp ::rLst -> 
        resolveComponentOpenPopup pPath (comp::components) rLst model dispatch
 

/// open an existing project
let private openProject model dispatch _ =
    match askForExistingProjectPath () with
    | None -> () // User gave no path.
    | Some path ->
        match loadAllComponentFiles path with
        | Error err ->
            log err
            let errMsg = "Could not load diagrams files in the project. The files may be malformed."
            displayFileErrorNotification errMsg dispatch
        | Ok componentsToResolve ->
            resolveComponentOpenPopup path [] componentsToResolve model dispatch


/// Display the initial Open/Create Project menu at the beginning if no project
/// is open.
let viewNoProjectMenu model dispatch =
    let menuItem label action =
        Menu.Item.li
            [ Menu.Item.IsActive false
              Menu.Item.OnClick action ] [ str label ]

    let initialMenu =
        Menu.menu []
            [ Menu.list []
                  [ menuItem "New project" (newProject model dispatch)
                    menuItem "Open project" (openProject model dispatch) ] ]

    match model.CurrProject with
    | Some _ -> div [] []
    | None -> unclosablePopup None initialMenu None []

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

let private viewInfoPopup dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br[]]
    let title = "ISSIE: Interactive Schematic Simulator and Integrated Editor"

    let body = div [] [
        makeH "Version"
        str "v1.0.0a"
        br []; br []
        makeH "Acknowledgments"
        str "ISSIE was created by Marco Selvatici (EIE 3rd year) as his BEng final year project. The waveform viewer was created by Edoardo Santi (EEE 3rd year) during Summer UROP work."
        br []; br []
        makeH "Introduction"
    
        str "Issie designs are made of one or more sheets. Each sheet contains components, and Input and Output Connectors. \
        If you have a single sheet that is your complete design. Otherwise any \
        sheet can include the hardware defined in other sheets by adding a custom component from the My Project section \
        of the Catalogue. Custom components defined like this can be used any number of times, and any sheet can contain custom components \
        so allowing an arbitrary design hierarchy." 
        br[]
        str "Issie provides a simple Simulator used mainly for combinational logic (although it can also show clocked logic) \
        and an interactive Waveform Simulator for clocked logic." 
        br[]
        str "In Issie all clocked components use the same clock signal - so called synchronous logic. \
        Therefore in any hardware design clk connections are not shown: all clk ports are
        automatically connected together. In the waveform simulation active clock edges are indicated \
        by verticals line through all the waveforms that demarcate clock cycles. With this there is then no need to show \
        the clock signal itself. The clock is a square wave with positive edges aligned with the vertical lines."
        br[]    
        str "If you find bugs in Issie or have feature requests you can create issues on the repo: https://github.com/tomcl/ISSIE"
        br[]
        makeH "Keyboard shortcuts"
        str "On Mac use Command instead of Ctrl."
        ul [] [
            li [] [str "Save: Ctrl + S"]
            li [] [str "Copy selected diagram items: Alt + C"]
            li [] [str "Paste diagram items: Alt + V"]
            li [] [str "Undo last diagram action: Alt + Z"]
            li [] [str "Redo last diagram action: Alt + Shift + Z"]
        ]
    ]
    let foot = div [] []
    closablePopup title body foot [Width 800] dispatch

let getSelected model: DiagEl list =
    match model.Diagram.GetSelected() with
    | None -> []
    | Some jsState ->
        (fst jsState |> List.map (extractComponent >> Comp), 
         snd jsState |> List.map (extractConnection >> Conn))
        ||> List.append

let limBits (name: string): (int * int) option =
    match Seq.tryFind ((=) '[') name, Seq.tryFind ((=) ':') name, Seq.tryFind ((=) ']') name with
    | Some _, Some _, Some _->
        (name.[Seq.findIndexBack ((=) '[') name + 1..Seq.findIndexBack ((=) ':') name - 1],
         name.[Seq.findIndexBack ((=) ':') name + 1..Seq.findIndexBack ((=) ']') name - 1])
        |> (fun (a, b) -> int a, int b)
        |> Some
    | _ -> None

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

let updateWSMod (model: Model) (wsMod: WaveSimModel) 
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
    |> (fun m -> { m with WaveTable = waveCol model m (makeWaveData m) })

let waveGen model (wSMod: WaveSimModel) ports =
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

    { wSMod' with WaveTable = waveCol model wSMod' (makeWaveData wSMod') }

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


/// Display top menu.
let viewTopMenu model dispatch =
    let compIds = getComponentIds model
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

    //printfn "FileView"
    let style = Style [ Width "100%" ] //leftSectionWidth model

    let projectPath, fileName =
        match model.CurrProject with
        | None -> "no open project", "no open sheet"
        | Some project -> project.ProjectPath, project.OpenFileName

    let makeFileLine name project =
        Navbar.Item.div [ Navbar.Item.Props [ style ] ]
            [ Level.level [ Level.Level.Props [ style ] ]
                  [ Level.left [] [ Level.item [] [ str name ] ]
                    Level.right [ Props [ Style [ MarginLeft "20px" ] ] ]
                        [ Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsPrimary
                                    Button.Disabled(name = project.OpenFileName)
                                    Button.OnClick(fun _ ->
                                        saveOpenFileAction false model // Save current file.
                                        openFileInProject name project model dispatch) ] [ str "open" ] ]
                          // Add option to rename?
                          //Level.item [] [
                          //    Button.button [
                          //        Button.Size IsSmall
                          //        Button.IsOutlined
                          //        Button.Color IsInfo
                          //    ] [ str "rename" ]
                          //]
                          Level.item []
                              [ Button.button
                                  [ Button.Size IsSmall
                                    Button.IsOutlined
                                    Button.Color IsDanger
                                    Button.OnClick(fun _ ->
                                        let title = "Delete sheet"

                                        let body =
                                            div []
                                                [ str "Are you sure you want to delete the following design sheet?"
                                                  br []
                                                  str <| pathJoin
                                                             [| project.ProjectPath
                                                                name + ".dgm" |]
                                                  br []
                                                  str <| "This action is irreversible." ]

                                        let buttonText = "Delete"

                                        let buttonAction =
                                            fun _ ->
                                                removeFileInProject name project model dispatch
                                                dispatch ClosePopup
                                        confirmationPopup title body buttonText buttonAction dispatch) ]
                                    [ str "delete" ] ] ] ] ]

    let fileTab =
        match model.CurrProject with
        | None -> Navbar.Item.div [] []
        | Some project ->
            let projectFiles = project.LoadedComponents |> List.map (fun comp -> makeFileLine comp.Name project)
            Navbar.Item.div
                [ Navbar.Item.HasDropdown
                  Navbar.Item.Props
                      [ OnClick(fun _ ->
                          if model.TopMenu = Files then Closed else Files
                          |> SetTopMenu
                          |> dispatch) ] ]
                [ Navbar.Link.a [] [ str "Sheets" ]
                  Navbar.Dropdown.div
                      [ Navbar.Dropdown.Props
                          [ Style
                              [ Display
                                  (if (let b = model.TopMenu = Files
                                       b) then
                                      DisplayOptions.Block
                                   else
                                       DisplayOptions.None) ] ] ]
                      ([ Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> addFileToProject model dispatch) ] ]
                             [ str "New Sheet" ]
                         Navbar.divider [] [] ]
                       @ projectFiles) ]

    div [ leftSectionWidth model ]
        [ Navbar.navbar
            [ Navbar.Props
                [ Style
                    [ Height "100%"
                      Width "100%" ] ] ]
              [ Navbar.Brand.div
                  [ Props
                      [ Style
                          [ Height "100%"
                            Width "100%" ] ] ]
                    [ Navbar.Item.div
                        [ Navbar.Item.HasDropdown
                          Navbar.Item.Props
                              [ OnClick(fun _ ->
                                  if model.TopMenu = Project then Closed else Project
                                  |> SetTopMenu
                                  |> dispatch) ] ]
                          [ Navbar.Link.a [] [ str "Project" ]
                            Navbar.Dropdown.div
                                [ Navbar.Dropdown.Props
                                    [ Style
                                        [ Display
                                            (if model.TopMenu = Project then
                                                DisplayOptions.Block
                                             else
                                                 DisplayOptions.None) ] ] ]
                                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick <| newProject model dispatch ] ]
                                      [ str "New project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| openProject model dispatch ] ]
                                      [ str "Open project" ]
                                  Navbar.Item.a [ Navbar.Item.Props [ OnClick <| closeProject model dispatch ] ]
                                      [ str "Close project" ] ] ]
                      fileTab
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Breadcrumb.breadcrumb [ Breadcrumb.HasArrowSeparator ]
                                      [ Breadcrumb.item [] [ str <| cropToLength 30 false projectPath ]
                                        Breadcrumb.item [] [ span [ Style [ FontWeight "bold" ] ] [ str fileName ] ] ] ] ]
                      Navbar.Item.div []
                          [ Navbar.Item.div []
                                [ Button.button
                                    [ Button.Color(if model.HasUnsavedChanges then IsSuccess else IsWhite)
                                      Button.OnClick(fun _ ->
                                          saveOpenFileAction false model
                                          SetHasUnsavedChanges false
                                          |> JSDiagramMsg
                                          |> dispatch) ] [ str "Save" ] ] ]
                      Navbar.End.div []
                          [ 
                            Navbar.Item.div []
                                [ match model.SimulationIsStale,currWS model, makeSimData model with
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
                                |> (fun but -> but [ str "Simulate >>" ]) ] ]
                      Navbar.End.div []
                          [ Navbar.Item.div []
                                [ Button.button [ Button.OnClick(fun _ -> viewInfoPopup dispatch) ] [ str "Info" ] ] ] ] ] ]

