(*
WaveformSimulationView.fs

View for waveform simulator in tab
*)

module WaveformSimulationView

open Fulma
open Fable.React
open Fable.React.Props

open DiagramMessageType
open DiagramModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open Extractor
open SimulatorTypes

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

//auxiliary functions to the viewer function

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
            |> List.tryPick (fun conn ->
                if conn.Source.Id = portId then Some conn.Id else None)
            |> function
            | Some connId -> [| ConnectionId connId |]
            | None -> [||]
        | None -> [||]
    | None -> failwith "highlight called when canvas state is None"

let setHighlightedConns (model: Model) dispatch ports =
    ports
    |> Array.collect (port2ConnId model)
    |> Array.toList
    |> SetSelWavesHighlighted
    |> dispatch

let toggleSelect ind (model: Model) dispatch =
    match currWS model with
    | Some wSMod ->
        let sel' = 
            Array.mapi (fun i old ->
                if i = ind then not old else old) wSMod.Selected
        { wSMod with Selected = sel' }
        |> SetCurrFileWSMod |> dispatch

        Array.zip wSMod.Ports sel'
        |> Array.filter snd
        |> Array.map fst
        |> setHighlightedConns model dispatch
    | None -> ()

let allSelected model = Array.forall ((=) true) model.Selected
let anySelected model = Array.contains true model.Selected

let makeLabels wSMod =
    let makeLbl l = label [ Class "waveLbl" ] [ str l ]
    Array.map makeLbl wSMod.WaveNames

let makeSegment (clkW: float) portSelected (xInd: int) (data: Sample) (trans: int * int) =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine =
        makeLinePoints
            [ Class "sigLineStyle"
              Style [ Stroke(if portSelected then "green" else "blue") ] ]

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
        | StateSample a, StateSample b when Array.length a = Array.length b -> 
            (a, b) ||> Array.fold2 folder 0
        | _ -> 1

    model2WaveList model |> Array.map (Array.pairwise >> Array.map isDiff)

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

let busLabels (model: Model) =
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
        (model2WaveList wSMod, Array.map makeGaps (transitions wSMod)) ||> Array.map2 gaps2pos
    | None -> failwith "busLabels called when currWS model is None"

let cursValStrings (wSMod: WaveSimModel) =
    let pref =
        match wSMod.Radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""

    let makeCursVal sample =
        match sample with
        | Wire w when w.NBits > 1u -> [| pref + radixChange w.BitData w.NBits wSMod.Radix |]
        | Wire w -> [| pref + string w.BitData |]
        | StateSample s -> s

    match int wSMod.Cursor < Array.length wSMod.WaveData with
    | true -> Array.map makeCursVal wSMod.WaveData.[int wSMod.Cursor]
    | false -> [||]

let makeCursVals model =
    let string2Lbl = Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map string2Lbl <| cursValStrings model

//container box and clock lines
let backgroundSvg model =
    let clkLine x = makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1 .. Array.length model.WaveData |] |> Array.map ((fun x -> float x * model.ClkWidth) >> clkLine)

let clkRulerSvg (model: WaveSimModel) =
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

let waveSimRows model wsMod dispatch =
    // waveforms
    let waveSvg =
        let addLabel nLabels xInd = makeText (inWaveLabel nLabels xInd wsMod)

        let valueLabels =
            let lblEl (sample, xIndArr) =
                match sample with
                | Wire w when w.NBits > 1u ->
                    Array.map (fun xInd -> addLabel 1 xInd (radixChange w.BitData w.NBits wsMod.Radix)) xIndArr
                | _ -> [||]
            busLabels model |> Array.map (Array.collect lblEl)

        let makeWaveSvg (portSelected: bool) (sampArr: Waveform) (transArr: (int * int) []): ReactElement [] =
            (sampArr, transArr)
            ||> Array.mapi2 (makeSegment wsMod.ClkWidth portSelected)
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

        let selPorts =
            let allSelPorts =
                (List.map (fun c -> Comp c) (fst model.CurrentSelected),
                 List.map (fun c -> Conn c) (snd model.CurrentSelected))
                ||> List.append
                |> compsConns2portLst model wsMod.SimData.[0]
            Array.map
                (fun (port: WaveSimPort) ->
                    Array.exists (fun (selP: WaveSimPort) -> (selP.CId, selP.OutPN) = (port.CId, port.OutPN)) allSelPorts)
                wsMod.Ports

        transitions wsMod
        |> Array.map padTrans
        |> Array.map3 makeWaveSvg selPorts (model2WaveList wsMod)
        |> Array.map2 Array.append valueLabels

    let labelCols =
        makeLabels wsMod
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Class "check"
                            Checked wsMod.Selected.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange(fun _ -> toggleSelect i model dispatch) ] ]
                  td
                      [ Class "waveNamesCol"
                        Style [ TextAlign TextAlignOptions.Right ] ] [ l ] ])

    let cursValCol = 
        makeCursVals wsMod 
        |> Array.map (fun c -> tr [ Class "rowHeight" ] [ td [ Class "cursValsCol" ] c ])

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
let maxWidth (wSMod: WaveSimModel) =
    let strWidth s = 
        JSHelpers.getTextWidthInPixels (s, "12px segoe ui") //not sure which font
    let curLblColWidth =
        match cursValStrings wSMod with
        | [||] -> 0.0
        | cVS ->
            Array.map (Array.map strWidth >> Array.max) cVS
            |> Array.max
            |> max 25.0
    let namesColWidth =
        match wSMod.WaveNames with
        | [||] -> 0.0
        | wN ->
            Array.map strWidth wN
            |> Array.max
            |> max 100.0
    let waveColWidth =
        match wSMod.Ports with
        | [||] -> 600.0
        | _ -> maxWavesColWidthFloat wSMod
    let checkboxCol = 25.0
    let extraWidth = 45.0 
    
    curLblColWidth + namesColWidth + waveColWidth + checkboxCol + extraWidth |> int

let appendSimData (wSMod: WaveSimModel) nCycles = 
    extractSimData (Array.last wSMod.SimData) nCycles 
    |> Array.append wSMod.SimData

let changeTopInd newVal (model: Model) (wsMod: WaveSimModel) =
    let sD = wsMod.SimData
    match Array.length sD = 0, newVal > wsMod.LastClk, newVal >= 0u with
    | true, _, _ -> { wsMod with LastClk = newVal }
    | false, true, _ ->
        let sD' = appendSimData wsMod <| newVal + 1u - uint (Array.length sD)
        { wsMod with
              SimData = sD' 
              WaveData = 
                Array.map (fun sD -> sD.Graph) sD' 
                |> Array.map (extractSimTime wsMod.Ports) 
              LastClk = newVal }
    | false, false, true ->
        { wsMod with
              LastClk = newVal
              WaveData = 
                Array.map (fun sD -> sD.Graph) sD.[0..int newVal] 
                |> Array.map (extractSimTime (reloadablePorts model sD.[0]))  }
    | _ -> wsMod

let zoom plus (m: Model) wSMod dispatch =
    let changedTopIndModel = 
        match int (float m.ViewerWidth * zoomFactor) > maxWidth wSMod with
        | true ->
            changeTopInd ((wSMod.LastClk + 1u) * (uint zoomFactor) + 10u) m wSMod
        | false -> wSMod
    if plus then zoomFactor else 1.0 / zoomFactor
    |> (*) wSMod.ClkWidth
    |> function
       | w when w > maxZoom -> { changedTopIndModel with ClkWidth = maxZoom }
       | w when w < minZoom -> { changedTopIndModel with ClkWidth = minZoom }
       | w -> { changedTopIndModel with ClkWidth = w }
    |> SetCurrFileWSMod
    |> dispatch

let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ str label ]

let changeCurs (model: Model) (wSMod: WaveSimModel) dispatch newVal =
    match 0u <= newVal, newVal <= wSMod.LastClk with
    | true, true -> { wSMod with Cursor = newVal }
    | true, false -> { changeTopInd newVal model wSMod with Cursor = newVal }
    | _ -> wSMod
    |> SetCurrFileWSMod
    |> dispatch

let cursorMove increase (model: Model) (wSMod: WaveSimModel) dispatch =
    match increase, wSMod.Cursor with
    | true, n -> n + 1u |> changeCurs model wSMod dispatch
    | false, n when n > 0u -> n - 1u |> changeCurs model wSMod dispatch
    | false, _ -> wSMod |> SetCurrFileWSMod |> dispatch

let selectAll s (model: Model) dispatch =
    match currWS model with
    | Some wSMod ->
        { wSMod with Selected = Array.map (fun _ -> s) wSMod.Selected }
        |> SetCurrFileWSMod |> dispatch
        if s then wSMod.Ports else [||]
        |> setHighlightedConns model dispatch
    | None -> ()

let delSelected model =
    let filtSelected arr =
        Array.zip model.Selected arr
        |> Array.filter (fun (sel, _) -> not sel)
        |> Array.map snd
    { model with
          WaveData = Array.map filtSelected model.WaveData
          WaveNames = filtSelected model.WaveNames
          Ports = filtSelected model.Ports
          Selected = Array.filter not model.Selected }
    |> SetCurrFileWSMod

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

    { model with
          WaveData = Array.map (fun sT -> reorder sT) model.WaveData
          WaveNames = reorder model.WaveNames
          Selected = reorder model.Selected
          Ports = reorder model.Ports }
    |> SetCurrFileWSMod

let radixTabs (model: WaveSimModel) dispatch =
    let radixString =
        [ Dec,  "uDec"
          Bin,  "Bin"
          Hex,  "Hex"
          SDec, "sDec" ] |> Map.ofList

    let radTab rad =
        Tabs.tab
            [ Tabs.Tab.IsActive(model.Radix = rad)
              Tabs.Tab.Props
                  [ Style
                      [ Width "35px"
                        Height "30px" ] ] ]
            [ a
                [ Style
                    [ Padding "0 0 0 0"
                      Height "30px" ]
                  OnClick(fun _ ->
                      { model with Radix = rad }
                      |> SetCurrFileWSMod
                      |> dispatch) ] [ str (radixString.[rad]) ] ]
    Tabs.tabs
        [ Tabs.IsToggle
          Tabs.Props
              [ Style
                  [ Width "140px"
                    Height "30px"
                    FontSize "80%"
                    Float FloatOptions.Right
                    Margin "0 10px 0 10px" ] ] ]
        [ radTab Bin
          radTab Hex
          radTab Dec
          radTab SDec ]

let cursorButtons (model: Model) wSMod dispatch =
    div [ Class "cursor" ]
        [ Button.button
            [ Button.CustomClass "cursLeft"
              Button.OnClick(fun _ -> cursorMove false model wSMod dispatch) ] [ str "◀" ]
          Input.number
              [ Input.Props
                  [ Min 0
                    Class "cursor form"
                    SpellCheck false
                    Step 1 ]
                Input.Id "cursor"
                match currWS model with
                | Some wSMod ->  wSMod.Cursor
                | None -> 0u
                |> (fun curs -> Input.Value(string curs) )
                //Input.DefaultValue <| sprintf "%d" model.WaveSim.Cursor
                Input.OnChange(fun c ->
                    match System.Int32.TryParse c.Value with
                    | true, n when n >= 0 -> changeCurs model wSMod dispatch (uint n) 
                    | _ -> ()) ]
          button [ Button.CustomClass "cursRight" ] (fun _ -> cursorMove true model wSMod dispatch) "▶" ]

let viewWaveSimButtonsBar model wSMod dispatch =
    div [ Style [ Height "45px" ] ]
        [ radixTabs wSMod dispatch
          cursorButtons model wSMod dispatch ]

let cursValsCol rows =
    let rightCol = Array.append [| tr [ Class "rowHeight" ] [ td [ Class "rowHeight" ] [] ] |] rows
    div
        [ Style
            [ Float FloatOptions.Right
              Height "100%"
              BorderTop "2px solid rgb(219,219,219)"
              BorderLeft "2px solid rgb(219,219,219)" ] ] [ table [] [ tbody [] rightCol ] ]

let connId2JSConn (model: Model) connId =
    match model.Diagram.GetCanvasState() with
    | Some (_, jsConns) -> 
        List.tryFind (fun jsConn -> (extractConnection jsConn).Id = connId) jsConns
    | None -> None
    |> function
       | Some jsConn -> [ jsConn ]
       | None -> []

let openCloseWA (wSMod: WaveSimModel) on = 
    { wSMod with WaveAdderOpen = on }
    |> SetCurrFileWSMod

let selWave2selConn model (wSMod: WaveSimModel) ind on = 
    match port2ConnId model wSMod.WaveAdder.Ports.[ind] with 
    | [| ConnectionId el |] -> connId2JSConn model el 
    | _ -> []
    |> function
       | [ jsC ] -> model.Diagram.SetSelected on jsC 
       | _ -> ()

let isWaveSelected model (wSMod: WaveSimModel) port = 
    getSelected model
    |> compsConns2portLst model wSMod.SimData.[0] 
    |> Array.contains port

let waveAdderToggle (model: Model) wSMod ind =
    isWaveSelected model wSMod wSMod.WaveAdder.Ports.[ind]
    |> not
    |> selWave2selConn model wSMod ind

let waveGen model (wSMod: WaveSimModel) dispatch ports =
    setHighlightedConns model dispatch [||]

    { wSMod with
        WaveData = 
            Array.map (fun sD -> sD.Graph) wSMod.SimData
            |> Array.map (extractSimTime ports) 
        WaveNames = Array.map (wSPort2Name wSMod.SimData.[0].Graph) ports
        Selected = Array.map (fun _ -> false) ports
        Ports = ports
        WaveAdderOpen = false }
    |> SetCurrFileWSMod |> dispatch

let waveAdderSelectAll model (wSMod: WaveSimModel) =
    let setTo = 
        wSMod.WaveAdder.Ports 
        |> Array.forall (isWaveSelected model wSMod)

    [| 0 .. Array.length wSMod.WaveAdder.Ports - 1 |]
    |> Array.map (fun i -> selWave2selConn model wSMod i (not setTo)) 
    |> ignore

let nameLabelsCol model wsMod labelRows dispatch =
    let waveAddDelBut =
        match anySelected wsMod with
        | true ->
            [ Button.button
                [ Button.CustomClass "delWaveButton"
                  Button.Color IsDanger
                  Button.OnClick(fun _ -> delSelected wsMod |> dispatch) ] [ str "del" ]
              div [ Class "updownDiv" ]
                  [ Button.button
                      [ Button.CustomClass "updownBut"
                        Button.OnClick(fun _ -> moveWave wsMod true |> dispatch) ] [ str "▲" ]
                    Button.button
                        [ Button.CustomClass "updownBut"
                          Button.OnClick(fun _ -> moveWave wsMod false |> dispatch) ] [ str "▼" ] ] ]
        | false ->
            [ Button.button
                [ Button.CustomClass "newWaveButton"
                  Button.Color IsSuccess
                  Button.OnClick(fun _ -> openCloseWA wsMod true |> dispatch) ] [ str "Edit list..." ] ]
        |> (fun children -> th [ Class "waveNamesCol" ] children)

    let top =
        [| tr [ Class "rowHeight" ]
               [ th [ Class "checkboxCol" ]
                     [ input
                         [ Type "checkbox"
                           Class "check"
                           Checked(allSelected wsMod)
                           OnChange(fun t -> selectAll t.Checked model dispatch) ] ]
                 waveAddDelBut ] |]

    let bot =
        [| tr [ Class "fullHeight" ]
               [ td [ Class "checkboxCol" ] []
                 td [] [] ] |]

    let leftCol = Array.concat [| top; labelRows; bot |]

    div
        [ Style
            [ Float FloatOptions.Left
              Height "100%" ] ] [ table [ Class "leftTable" ] [ tbody [] leftCol ] ]

let wavesCol wSMod rows =
    div [ Style [ MaxWidth(maxWavesColWidth wSMod)
                  MinHeight "100%" ]
          Class "wavesTable" ] 
            [ table [ Style [ Height "100%" ] ] 
                    [ tbody [ Style [ Height "100%" ] ] rows ] ]

let viewWaveformViewer model wSMod dispatch =
    let tableWaves, leftColMid, cursValsRows = waveSimRows model wSMod dispatch
    div
        [ Style
            [ Height "calc(100% - 45px)"
              Width "100%"
              OverflowY OverflowOptions.Auto ] ]
        [ cursValsCol cursValsRows
          div [ Style [ Height "100%" ] ]
              [ nameLabelsCol model wSMod leftColMid dispatch
                wavesCol wSMod tableWaves ] ]

let viewZoomDiv model wSMod dispatch =
    div [ Class "zoomDiv" ]
        [ button [ Button.CustomClass "zoomButLeft" ] (fun _ -> zoom false model wSMod dispatch) "-"
          button [ Button.CustomClass "zoomButRight" ] (fun _ -> zoom true model wSMod dispatch) "+" ]

let waveAdderTopRow model wSMod =
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wACheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked(Array.forall (isWaveSelected model wSMod) wSMod.WaveAdder.Ports)
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderSelectAll model wSMod) ] ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ] ]

let addWaveRow model wSMod ind =
    let selected = isWaveSelected model wSMod wSMod.WaveAdder.Ports.[ind]
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wAcheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked selected
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> waveAdderToggle model wSMod ind) ] ]
          td [] [ label [] [ str wSMod.WaveAdder.WaveNames.[ind] ] ] ]

let addWaveRows model wSMod = 
    Array.mapi (fun i _ -> addWaveRow model wSMod i) wSMod.WaveAdder.Ports 

let viewWaveAdder (model: Model) wSMod =
    div [ Style [ Position PositionOptions.Absolute
                  Top "300px" ] ]
        [ table []
                [ tbody [] 
                        (Array.append [| waveAdderTopRow model wSMod |] (addWaveRows model wSMod)) ] ]

let waveAdderButs (model: Model) wSMod dispatch =
    let simButStyle =
        match Array.exists (isWaveSelected model wSMod) wSMod.WaveAdder.Ports with
        | true ->
            [ Button.Color IsSuccess
              Button.OnClick(fun _ -> 
                Array.filter (isWaveSelected model wSMod) wSMod.WaveAdder.Ports                
                |> waveGen model wSMod dispatch) ]
        | false -> [ Button.CustomClass "disabled" ]
        |> (fun lst -> Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)
    let cancBut =
        Button.button
            [ Button.Color IsDanger
              Button.OnClick(fun _ -> openCloseWA wSMod false |> dispatch) ] [ str "Cancel" ]

    let buts =
        match wSMod.Ports with
        | [||] -> [ Button.button simButStyle [ str "View selected" ] ]
        | _ -> [ cancBut; Button.button simButStyle [ str "View" ] ]
    div [ Style [ Display DisplayOptions.Block ] ] buts

let waveAdderView model wSMod dispatch =
    [ div
    [ Style
        [ Width "90%"
          MarginLeft "5%"
          MarginTop "15px" ] ]
      [ Heading.h4 [] [ str "Waveform Simulation" ]
        str
            "Add waveforms to view simulation. \n You can also add them by selecting components/connections in the editor and clicking \"Simulate\" on the top left menu bar"
        hr []
        div []
            [ waveAdderButs model wSMod dispatch
              viewWaveAdder model wSMod ] ] ]

let waveformsView model wSMod dispatch =
    [ div
        [ Style
            [ Width "calc(100% - 10px)"
              Height "100%"
              MarginLeft "0%"
              MarginTop "0px"
              OverflowX OverflowOptions.Hidden ] ]
          [ viewWaveSimButtonsBar model wSMod dispatch
            viewWaveformViewer model wSMod dispatch
            viewZoomDiv model wSMod dispatch ] ]

let viewWaveSim (model: Model) dispatch =
    match currWS model, snd model.WaveSim with
    | Some wSMod, None ->
        ( model, wSMod, dispatch )
        |||> if wSMod.WaveAdderOpen then waveAdderView else waveformsView 
    | Some _, Some simError ->
        [ div [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
              [ SimulationView.viewSimulationError simError
                button [ Button.Color IsDanger ] (fun _ -> None |> SetWSError |> dispatch) "Ok" ] ]
    | None, _ ->
        initFileWS model dispatch
        []
