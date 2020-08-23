(*
WaveformSimulationView.fs

View for waveform simulator in tab
*)

module WaveformSimulationView

open Fulma
open Fable.React
open Fable.React.Props
//open Fable.Core
//open System.IO

open DiagramMessageType
open DiagramStyle
open CommonTypes
open OnDiagramButtonsView
open Simulator
open Extractor
open SimulatorTypes

(*let initModel: WaveSimModel =
    { SimData = [||]
      WaveData =
          //modify these two signals to change trial data
          let nbits1 = uint32 1
          let nbits2 = uint32 4
          let s1 = [| 0; 0; 0; 0; 1; 0; 1; 1; 1; 1 |]
          let s2 = [| 1; 1; 1; 1; 14; 14; 14; 14; 2; 8 |]

          let s3 =
              [| [| "state1" |]
                 [| "state1" |]
                 [| "state2"; "state1" |]
                 [| "state2" |]
                 [| "state1" |]
                 [| "state2" |]
                 [| "state1" |]
                 [| "state2" |]
                 [| "state1" |]
                 [| "state2" |] |]

          let makeTrialData (nBits1: uint32) (signal1: int []) (nBits2: uint32) signal2 signal3: SimTime [] =
              let makeTimePointData (s1: int) (s2: int) s3 : SimTime =
                  [| Wire
                      { NBits = nBits1
                        BitData = bigint s1 }
                     Wire
                         { NBits = nBits2
                           BitData = bigint s2 }
                     StateSample s3 |]
              (signal1, signal2, signal3)              
              |||> Array.map3 makeTimePointData

          makeTrialData nbits1 s1 nbits2 s2 s3

      WaveNames = [| "try single Bit"; "try bus"; "try states" |]
      Selected = [| false; false; false |]
      Ports = [| { CId = SimulatorTypes.ComponentId "qwertyuiop";   OutPN = SimulatorTypes.OutputPortNumber 1; TrgtId = None }
                 { CId = SimulatorTypes.ComponentId "qwertyuiopa";  OutPN = SimulatorTypes.OutputPortNumber 1; TrgtId = None }
                 { CId = SimulatorTypes.ComponentId "qwertyuiopas"; OutPN = SimulatorTypes.OutputPortNumber 1; TrgtId = None } |] 
      ClkWidth = 1.0
      Cursor = uint32 0
      Radix = Bin
      LastClk = uint 9
      WaveAdder = { Ports = [||]; WaveNames = [||]; SimData = None }
      LastCanvasState = None }*)

let initModel: WaveSimModel =
    { SimData = [||]
      WaveData = [||]
      WaveNames = [||]
      Selected = [||]
      Ports = [||] 
      ClkWidth = 1.0
      Cursor = 0u
      Radix = Bin
      LastClk = 9u
      WaveAdder = None 
      LastCanvasState = None }

// SVG functions

let makeLine style = line style []
let makeRect style = rect style []
let makeText style t = text style [ str t ]
let makeSvg style elements = svg style elements

let makeLinePoints style (x1, y1) (x2, y2) =
    line
        (List.append style
             [ X1 x1
               Y1 y1
               X2 x2
               Y2 y2 ]) []

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
    let seqPad = [ 1 .. (4 - int nBits % 4) % 4 ] |> List.map (fun _ -> '0')

    let paddedBin =
        dec2bin n nBits
        |> Seq.toList
        |> List.append seqPad

    let fourBitToHexDig fourBit =
        match fourBit with
        | [ '0'; '0'; '0'; '0' ] -> '0'
        | [ '0'; '0'; '0'; '1' ] -> '1'
        | [ '0'; '0'; '1'; '0' ] -> '2'
        | [ '0'; '0'; '1'; '1' ] -> '3'
        | [ '0'; '1'; '0'; '0' ] -> '4'
        | [ '0'; '1'; '0'; '1' ] -> '5'
        | [ '0'; '1'; '1'; '0' ] -> '6'
        | [ '0'; '1'; '1'; '1' ] -> '7'
        | [ '1'; '0'; '0'; '0' ] -> '8'
        | [ '1'; '0'; '0'; '1' ] -> '9'
        | [ '1'; '0'; '1'; '0' ] -> 'A'
        | [ '1'; '0'; '1'; '1' ] -> 'B'
        | [ '1'; '1'; '0'; '0' ] -> 'C'
        | [ '1'; '1'; '0'; '1' ] -> 'D'
        | [ '1'; '1'; '1'; '0' ] -> 'E'
        | [ '1'; '1'; '1'; '1' ] -> 'F'
        | _ -> 'N' // maybe should deal with exception differently

    [ 0 .. 4 .. int nBits - 1 ]
    |> List.map ((fun i -> paddedBin.[i..i + 3]) >> fourBitToHexDig)
    |> List.toSeq
    |> Seq.map string
    |> String.concat ""

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits).[0] = '1' then n - bigint (2.0 ** (float nBits)) else n
    |> string

let radixChange (n: bigint) (nBits: uint32) (rad: NumberBase) =
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits

//auxiliary functions to the viewer function

let toggleSelect ind model =
    { model with Selected = Array.mapi (fun i old ->
                                if i = ind then not old else old) model.Selected }
    |> Ok |> StartWaveSim

let highlight on i (model: DiagramModelType.Model) =
    match model.Diagram.GetCanvasState () with
    | Some s -> 
        let p = model.WaveSim.Ports.[i]
        let outPN = match p.OutPN with
                    | OutputPortNumber n -> n
        List.map extractComponent (fst s)
        |> List.tryPick (fun c -> match ComponentId c.Id = p.CId with
                                  | true -> Some c.OutputPorts.[outPN].Id
                                  | false -> None)
        |> function
           | Some portId -> 
                List.map extractConnection (snd s)
                |> List.tryPick (fun conn -> if conn.Source.Id = portId then Some conn.Id else None)
                |> function
                   | Some connId -> 
                        match on with
                        | true -> 
                            (fst model.Hilighted, List.append (snd model.Hilighted) [ConnectionId connId])
                        | false -> 
                            (fst model.Hilighted, List.filter (fun c -> c <> ConnectionId connId ) (snd model.Hilighted))
                        |> SetHighlighted
                   | None -> model.Hilighted |> SetHighlighted
           | None -> model.Hilighted |> SetHighlighted
    | None -> failwith "Canvas State is None even though the SimData is Some"

let allSelected model = Array.forall ((=) true) model.Selected
let anySelected model = Array.contains true model.Selected
    
let highlightAll (model: DiagramModelType.Model) dispatch =
    let on = not (allSelected model.WaveSim)
    Array.mapi (fun i _ -> highlight on i model |> dispatch) model.WaveSim.Selected

let makeLabels simData model = 
    extractWaveNames simData model reloadablePorts
    |> Array.map (fun l -> label [ Class "waveLbl" ] [ str l ])

let makeSegment (clkW: float) portSelected (xInd: int) (data: Sample) (trans: int * int)  =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine = makeLinePoints [ Class "sigLineStyle"
                                       Style [ Stroke (if portSelected then "red" else "blue") ] ]

    match data with
    | Wire w when w.NBits = 1u ->
        let y =
            match w.BitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        // TODO: define DU so that you can't have values other than 0 or 1
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | 1 -> [| makeSigLine (right, bot + sigLineThick / 2.0) (right, top - sigLineThick / 2.0) |]
        | 0 -> [||]
        | _ ->
            "What? Transition has value other than 0 or 1" |> ignore
            [||]
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
        | _ ->
            "What? Transition has value other than 0 or 1" |> ignore
            [||]
        |> Array.append [| topL; botL |]
//Probably should put other option for negative number which prints an error

let model2WaveList model: Waveform [] = Array.transpose model.WaveData

let transitions (model: WaveSimModel) = //relies on number of names being correct (= length of elements in WaveData)
    let isDiff (ws1, ws2) =
        let folder state e1 e2 =
            match state, e1 = e2 with
            | 0, true -> 0
            | _ -> 1
        match ws1, ws2 with
        | Wire a, Wire b ->
            if a.BitData = b.BitData then 0 else 1
        | StateSample a, StateSample b when Array.length a = Array.length b ->  (a, b) ||> Array.fold2 folder 0
        | _ -> 1

    model2WaveList model 
    |> Array.map (Array.pairwise >> Array.map isDiff)

// functions for bus labels

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

let busLabels model =
    let gaps2pos (wave: Waveform) gaps =
        let nSpaces (g: {| GapLen: int; GapStart: int |}) = (g.GapLen / (maxBusValGap + 1) + 2)
        let gapAndInd2Pos (g: {| GapLen: int; GapStart: int |}) i =
            float g.GapStart + float i * float g.GapLen / float (nSpaces g)
        gaps
        |> Array.map (fun (gap: {| GapLen: int; GapStart: int |}) ->
            wave.[gap.GapStart], Array.map (gapAndInd2Pos gap) [| 1 .. nSpaces gap - 1 |])
    (model2WaveList model, Array.map makeGaps (transitions model))
    ||> Array.map2 gaps2pos

let makeCursVals model =
    let pref =
        match model.Radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""
    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| pref + radixChange w.BitData w.NBits model.Radix |]
        | Wire w -> [| pref + string w.BitData |]
        | StateSample s -> s
        |> Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    match int model.Cursor < Array.length model.WaveData with
    | true -> Array.map makeCursVal model.WaveData.[int model.Cursor]
    | false -> [||]
        (*(Array.length model.WaveData, model.Cursor)
        ||> sprintf "WaveData.length = %d, model.Cursor = %d, WaveData.[model Cursor] doesn't exist" 
        |> failwith*)

//container box and clock lines
let backgroundSvg model =
    let clkLine x =
        makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1 .. Array.length model.WaveData |] 
    |> Array.map ((fun x -> float x * model.ClkWidth) >> clkLine)

let clkRulerSvg (model: WaveSimModel) =
    [| 0 .. int model.LastClk |]
    |> Array.map (fun i -> makeText (cursRectText model i) (string i))
    |> (fun arr -> [ backgroundSvg model; [| makeRect (cursRectStyle model) |]; arr ])
    |> Array.concat 
    |> makeSvg (clkRulerStyle model)
    
let waveSimRows (model: DiagramModelType.Model) dispatch =
    let wsMod = model.WaveSim
// waveforms
    let waveSvg =
        let addLabel nLabels xInd (i: int) = makeText (inWaveLabel nLabels xInd i wsMod)

        let valueLabels =
            let lblEl (sample, xIndArr) =
                match sample with
                | Wire w when w.NBits > 1u ->
                    Array.map (fun xInd -> addLabel 1 xInd 0 (radixChange w.BitData w.NBits wsMod.Radix)) xIndArr
                | StateSample ss ->
                    Array.collect (fun xInd -> Array.mapi (addLabel (Array.length ss) xInd) ss) xIndArr
                | _ -> [||]
            busLabels wsMod |> Array.map (Array.collect lblEl)

        let makeWaveSvg (portSelected: bool) (sampArr: Waveform) (transArr: (int*int) []) : ReactElement [] = 
            (sampArr, transArr)
            ||> Array.mapi2 (makeSegment model.WaveSim.ClkWidth portSelected) 
            |> Array.concat

        let padTrans t =
            match Array.length t with
            | 0 -> 
                [| 1, 1 |]
            | 1 -> 
                [| (1, t.[0]); (t.[0], 1) |]
            | _ ->
                Array.pairwise t
                |> (fun pairs ->  
                        Array.concat [ [| 1, fst pairs.[0] |]
                                       pairs
                                       [| snd (Array.last pairs), 1 |] ] )

        let selPorts = 
            match makeSimData model with
            | (Some (Ok sD)), _ ->
                let allSelPorts = 
                    ( List.map (fun c -> Comp c) (fst model.CurrentSelected),
                      List.map (fun c -> Conn c) (snd model.CurrentSelected) )
                    ||> List.append 
                    |> compsConns2portLst model sD
                Array.map (fun port -> Array.exists (fun selP -> (selP.CId, selP.OutPN) = (port.CId, port.OutPN)) allSelPorts) wsMod.Ports
            | _ -> Array.map (fun _ -> false) wsMod.Ports

        transitions wsMod
        |> Array.map padTrans
        |> Array.map3 makeWaveSvg selPorts (model2WaveList model.WaveSim)
        |> Array.map2 Array.append valueLabels

// name and cursor labels of the waveforms
    let labels = 
        match makeSimData model with
        | (Some (Ok sD)), _ -> makeLabels sD model
        | _ -> [||]
    let cursLabs = makeCursVals wsMod

    let labelCols =
        labels
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Checked wsMod.Selected.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange (fun _ -> toggleSelect i wsMod |> dispatch
                                               highlight (not wsMod.Selected.[i]) i model |> dispatch) ] ]
                  td [ Class "waveNamesCol" ] [ l ]])

    let cursValCol =
        cursLabs
        |> Array.map (fun c ->
            tr [ Class "rowHeight" ]
                [ td [ Class "cursValsCol" ] c ])

    let waveCol =
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
                        (Array.concat [| cursRectSvg; bgSvg; wave |])) waveSvg)
        |> Array.append [| tr [ Class "rowHeight" ] [ td (waveCell wsMod) [ clkRulerSvg wsMod ] ] |]

    waveCol, labelCols, cursValCol

// view function helpers

let zoom plus (m: WaveSimModel) =
    let multBy =
        if plus then zoomFactor else 1.0 / zoomFactor
    { m with ClkWidth = m.ClkWidth * multBy } |> Ok |> StartWaveSim

let button style func label =
    Button.button (List.append [ Button.Props style ] [ Button.OnClick func ]) [ str label ]

let buttonOriginal style func label =
    input
        [ Type "button"
          Value label
          style
          OnClick func ]

let radixString rad =
    match rad with
    | Dec -> "Dec"
    | Bin -> "Bin"
    | Hex -> "Hex"
    | SDec -> "sDec"

let appendSimData model nCycles =
    extractSimData (Array.last model.SimData) nCycles
    |> Array.append model.SimData

let changeTopInd newVal (model: DiagramModelType.Model) = 
    let wsMod = model.WaveSim
    let sD = wsMod.SimData
    match Array.length sD = 0, newVal > wsMod.LastClk, newVal >= 0u  with
    | true, _, _ ->
        { wsMod with LastClk = newVal }
    | false, true, _ -> 
        let sD' = appendSimData wsMod <| newVal + 1u - uint (Array.length sD)
        { wsMod with 
            SimData = sD' //can improve efficiency by not recalculating the whole WaveData every time
            WaveData = extractWaveData model (fun m _ -> m.WaveSim.Ports) sD'.[0..int newVal]
            LastClk = newVal }
    | false, false, true -> 
        { wsMod with 
            LastClk = newVal 
            WaveData = extractWaveData model reloadablePorts sD.[0..int newVal]}
    | _ -> 
        wsMod

let changeCurs (model: DiagramModelType.Model) newVal =
    match 0u <= newVal, newVal <= model.WaveSim.LastClk with
    | true, true ->  { model.WaveSim with Cursor = newVal }
    | true, false -> { changeTopInd newVal model with Cursor = newVal }
    |_ ->  model.WaveSim
    |> Ok |> StartWaveSim

let cursorMove increase (model: DiagramModelType.Model) =
    match increase with
    | true -> model.WaveSim.Cursor + 1u
    | false -> model.WaveSim.Cursor - 1u
    |> changeCurs model

let selectAll s model = { model with Selected = Array.map (fun _ -> s) model.Selected } |> Ok |> StartWaveSim

let delSelected model =
    let filtSelected arr =
        Array.zip model.Selected arr
        |> Array.filter (fun (sel, _) -> not sel)
        |> Array.map snd
    { model with WaveData = Array.map filtSelected model.WaveData
                 //WaveNames = filtSelected model.WaveNames
                 Ports = filtSelected model.Ports
                 Selected = Array.filter not model.Selected }
    |> Ok |> StartWaveSim

let moveWave model up =
    let lastEl (arr: 'a []) = Array.last arr

    let move arr =
        let rev a =
            if up then a else Array.rev a
        rev arr
        |> Array.fold (fun st (bl: {| Sel: bool; Indxs: int [] |}) ->
            match st with
            | [||] -> [| bl |]
            | _ ->
                if bl.Sel then
                    Array.concat
                        [| st.[0..Array.length st - 2]
                           [| bl |]
                           [| lastEl st |] |]
                else
                    Array.append st [| bl |]) [||]
        |> rev

    let indexes' =
        match Array.length model.Selected with
        | len when len < 2 -> [| 0 .. Array.length model.Selected - 1 |]
        | _ ->
            Array.indexed model.Selected
            |> Array.fold (fun (blocks: {| Sel: bool; Indxs: int [] |} []) (ind, sel') ->
                match blocks, sel' with
                | [||], s' ->
                    Array.append blocks
                        [| {| Sel = s'
                              Indxs = [| ind |] |} |]
                | bl, true when (lastEl bl).Sel = true ->
                    Array.append blocks.[0..Array.length blocks - 2]
                        [| {| (lastEl blocks) with Indxs = Array.append (lastEl blocks).Indxs [| ind |] |} |]
                | _, s' ->
                    Array.append blocks
                        [| {| Sel = s'
                              Indxs = [| ind |] |} |]) [||]
            |> move
            |> Array.collect (fun block -> block.Indxs)

    let reorder (arr: 'b []) = Array.map (fun i -> arr.[i]) indexes'
    
    { model with WaveData = Array.map (fun sT -> reorder sT) model.WaveData
                 //WaveNames = reorder model.WaveNames
                 Selected = reorder model.Selected
                 Ports = reorder model.Ports}
    |> Ok |> StartWaveSim

//[<Emit("__static")>]
//let staticDir() : string = jsNative

//view functions of the waveform simulator

let radixTabs model dispatch =
    let radTab rad =
        Tabs.tab [ Tabs.Tab.IsActive (model.Radix = rad)
                   Tabs.Tab.Props [Style [ Width "35px"
                                           Height "30px"] ] ]
                 [ a [ OnClick(fun _ -> Ok { model with Radix = rad } |> StartWaveSim |> dispatch) ]
                 [ str (radixString rad) ] ]
    Tabs.tabs
        [ Tabs.IsToggle; Tabs.Props [ Style [ Width "140px"
                                              Height "30px"
                                              FontSize "80%" 
                                              Float FloatOptions.Right;
                                              Margin "0 10px 0 10px" ] ] ]
        [ radTab Bin; radTab Hex; radTab Dec; radTab SDec ]

let cursorButtons model dispatch =
    div [ Class "cursor-group" ]
        [ buttonOriginal (Class "button-minus") (fun _ -> cursorMove false model |> dispatch) "◄"
          Input.number [
              Input.Props [Min 0; Class "cursor-form"; SpellCheck false; Step 1]
              Input.Id "cursorForm"
              Input.Value (string model.WaveSim.Cursor)
              //Input.DefaultValue <| sprintf "%d" model.WaveSim.Cursor
              Input.OnChange (fun c -> 
                    match System.Int32.TryParse c.Value with
                    | true, n when n >= 0 -> changeCurs model (uint n) |> dispatch
                    | _ -> () )
          ]
          buttonOriginal (Class "button-plus") (fun _ -> cursorMove true model |> dispatch) "►" ] 

let canReload (model: DiagramModelType.Model) = 
    match model.WaveSim.LastCanvasState <> model.Diagram.GetCanvasState(), makeSimData model with
    | true, (Some (Ok _), _) -> true
    | _ -> false

let reloadButStyle model dispatch = 
    match canReload model with
    | true -> [ Button.Color IsSuccess
                Button.OnClick (fun _ ->
                    simLst model dispatch reloadablePorts 
                    |> StartWaveSim |> dispatch) ] 
    | false -> [ Button.Color IsGrey ]
    |> List.append [ Button.CustomClass "reloadButtonStyle" ]

let viewWaveSimButtonsBar model dispatch = 
    div [ Style [ Height "7%" ] ]
        [ Button.button (reloadButStyle model dispatch) [ str "Reload" ]
          //button (reloadButStyle model) (fun _ -> 
          //    simLst model dispatch reloadablePorts |> StartWaveSim |> dispatch) "Reload" 
          radixTabs model.WaveSim dispatch
          cursorButtons model dispatch ]

let cursValsCol rows = 
    let rightCol = Array.append [| tr [ Class "rowHeight" ]
                                      [ td [ Class "rowHeight" ] [] ] |] rows
    div [ Style [ Float FloatOptions.Right; Height "100%"; BorderTop "2px solid rgb(219,219,219)"; BorderLeft "2px solid rgb(219,219,219)" ] ]
        [ table [] [ tbody [] rightCol ] ]

let avalPorts (model: DiagramModelType.Model) dispatch = 
    match model.Diagram.GetCanvasState (), model.CurrProject with
    | None, _ -> [||], None
    | _, None -> failwith "what? Cannot start a simulation without a project"
    | Some jsState, Some project ->
        let otherComponents =
            project.LoadedComponents
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (extractState jsState, otherComponents)
        ||> prepareSimulation project.OpenFileName
        |> function
            | Ok simData -> 
                List.map (extractComponent >> Comp) (fst jsState)
                |> compsConns2portLst model simData, Some simData
            | Error simError ->
                if simError.InDependency.IsNone then
                    (simError.ComponentsAffected, simError.ConnectionsAffected)
                    |> SetHighlighted |> dispatch
                [||], None

let openWaveAdder (model: DiagramModelType.Model) dispatch = 
    match avalPorts model dispatch with
    | wSPorts, Some sD ->
        let ports' = Array.map (fun p -> p, Array.contains p model.WaveSim.Ports) wSPorts
        let names' = Array.map (wSPort2Name sD.Graph) wSPorts
        { model.WaveSim with WaveAdder = Some { Ports = ports'; WaveNames = names'; SimData = sD } }
        |> Ok |> StartWaveSim 
    | _ -> model.WaveSim |> Ok |> StartWaveSim //can I not just do nothing?
    
let cancelAddWave model =
    { model with WaveAdder = None } 
    |> Ok |> StartWaveSim

let waveAdderToggle model ind =
    match model.WaveAdder with
    | Some wA ->
        let ports' = Array.mapi (fun i (p, sel) -> if i = ind then p, not sel else p, sel) 
                                wA.Ports
        { model with WaveAdder = Some { wA with Ports = ports' } }
        |> Ok |> StartWaveSim
    | None -> failwith "waveAdderToggle called when model.WaveAdder is None"

let simulateAddWave (model: DiagramModelType.Model) = 
    match model.WaveSim.WaveAdder with
    | Some wA -> 
        let wsMod = model.WaveSim
        let ports' = 
            Array.filter (fun (_, s) -> s) wA.Ports
            |> Array.map fst
        let simData' = 
            extractSimData wA.SimData wsMod.LastClk
        let waveData' = 
            extractWaveData model (fun _ _ -> ports') simData'
        let waveNames' = 
            Array.zip wA.WaveNames wA.Ports
            |> Array.filter (snd >> snd) 
            |> Array.map fst
        Ok { wsMod with SimData = simData'
                        WaveNames = waveNames'
                        WaveData = waveData'
                        Selected = Array.map (fun _ -> false) ports'
                        Ports = ports'
                        WaveAdder = None}
        |> StartWaveSim
    | None -> failwith "simulateWaveAdd called when WaveAdder is None"

let waveAdderSelectAll model =
    match model.WaveAdder with
    | Some wA -> 
        let setTo = wA.Ports |> Array.forall (fun (_,b) -> b)
        let ports' = Array.map (fun (p,_) -> p, not setTo) wA.Ports
        { model with WaveAdder = Some { wA with Ports = ports' } }
        |> Ok |> StartWaveSim
    | None -> failwith "simulateWaveAdd called when WaveAdder is None"

let nameLabelsCol (model: DiagramModelType.Model) labelRows dispatch =
    let wsMod = model.WaveSim
    let waveAddDelBut =
        match anySelected wsMod with
        | true ->
            [ button [Class "delWaveButton"] (fun _ -> delSelected wsMod |> dispatch) "del"
              div [ Class "updownDiv" ]
                  [ button [Class "updownButton"] (fun _ -> moveWave wsMod true |> dispatch) "▲"
                    button [Class "updownButton"] (fun _ -> moveWave wsMod false |> dispatch) "▼" ] ]
        | false ->
            [ div [ Style [ WhiteSpace WhiteSpaceOptions.Nowrap ] ]
                  [ button [Class "newWaveButton"] (fun _ -> openWaveAdder model dispatch |> dispatch) "+"
                    label [ Class "newWaveLabel" ] [ str "Add wave" ] ] ]
        |> (fun children -> th [ Class "waveNamesCol" ] children)

    let top =
        [| tr []
               [ th [ Class "checkboxCol" ]
                    [ input
                        [ Type "checkbox"
                          Style [ MarginLeft "5px"; MarginRight "5px" ]
                          Checked (allSelected wsMod)
                          OnChange(fun t -> highlightAll model dispatch |> ignore
                                            selectAll t.Checked wsMod |> dispatch ) ] ]
                 waveAddDelBut ] |]

    let bot =
        [| tr [ Class "fullHeight" ]
               [ td [ Class "checkboxCol" ] []
                 td [] [] ] |]

    let leftCol = Array.concat [| top; labelRows; bot |]
    div [ Style [ Float FloatOptions.Left; Height "100%" ] ]
        [ table [ Class "waveSimTableStyle" ] [ tbody [] leftCol ] ]

let wavesCol rows =
    div [ Style [ Height "100%"; OverflowX OverflowOptions.Scroll; BorderTop "2px solid rgb(219,219,219)" ] ] 
        [ table [ Style [ Height "100%" ] ]
                [ tbody [ Style [ Height "100%" ] ] rows ] ]
            
let viewWaveformViewer model dispatch =
    let tableWaves, leftColMid, cursValsRows = waveSimRows model dispatch
    div [ Style [ Height "91.8%"; Width "100%" ] ] 
        [ cursValsCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model leftColMid dispatch
                wavesCol tableWaves ] ]

let viewZoomDiv model dispatch =
    div [ Class "zoomDiv" ]
        [ button [Class "zoomButtonStyle"] (fun _ -> zoom false model |> dispatch) "-"
          //let svgPath = Path.Combine(staticDir(), "hzoom-icon.svg")
          //let svgPath = staticDir() + "\hzoom-icon.svg"
          //embed [ Src svgPath ]
          button [Class "zoomButtonStyle"] (fun _ -> zoom true model |> dispatch) "+" ] 

let waveAdderTopRow model (wA: WaveAdderModel) dispatch =
    tr [] 
       [ td [ Class "checkboxCol" ]
            [ input
               [ Type "checkbox"
                 Style [ MarginLeft "5px"; MarginRight "5px" ]
                 Checked (Array.forall (fun (_,b) -> b) wA.Ports)
                 Style [ Float FloatOptions.Left ]
                 OnChange (fun _ -> waveAdderSelectAll model |> dispatch ) ] ]
         td [] [] ]

let addWaveRow model dispatch ind (_,selected) name =
    tr [] 
        [ td [ Class "checkboxCol" ]
              [ input
                    [ Type "checkbox"
                      Checked selected
                      Style [ Float FloatOptions.Left ]
                      OnChange (fun _ -> waveAdderToggle model ind |> dispatch ) ] ] 
          td [] [ label [] [ str name ] ] ]

let addWaveRows model (wA: WaveAdderModel) dispatch =
    Array.mapi2 (addWaveRow model dispatch) wA.Ports wA.WaveNames

let viewWaveAdder model (wA: WaveAdderModel) dispatch =
    table []
          [ tbody [] (Array.append [| waveAdderTopRow model wA dispatch |] (addWaveRows model wA dispatch)) ]

let waveAdderButs (model: DiagramModelType.Model) dispatch =
    div [ Style [Display DisplayOptions.Block] ]
        [ button [Class "reloadButtonStyle"] (fun _ -> cancelAddWave model.WaveSim |> dispatch) "Cancel"
          button [Class "reloadButtonStyle"] (fun _ -> simulateAddWave model |> dispatch) "Simulate" ]

let isWaveAdderShown (model: DiagramModelType.Model) dispatch =
    match model.Diagram.GetCanvasState() = model.WaveSim.LastCanvasState with
    | true -> model.WaveSim.WaveAdder
    | false -> 
        Ok { model.WaveSim with WaveAdder = None } |> StartWaveSim |> dispatch
        None

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    match isWaveAdderShown model dispatch with 
    | Some wA -> 
        [ Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] 
                     [ str "Select waveforms to simulate them. \n Alternatively, select components/connections in the editor and click \"Simulate\"" ]
          waveAdderButs model dispatch
          viewWaveAdder model.WaveSim wA dispatch ]
    | None -> 
        [ viewWaveSimButtonsBar model dispatch
          viewWaveformViewer model dispatch
          viewZoomDiv model.WaveSim dispatch ]
    
