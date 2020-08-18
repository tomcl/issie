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

let initModel: WaveSimModel =
    { waveData =
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
                      { nBits = nBits1
                        bitData = bigint s1 }
                     Wire
                         { nBits = nBits2
                           bitData = bigint s2 }
                     StateSample s3 |]
              (signal1, signal2, signal3)              
              |||> Array.map3 makeTimePointData

          makeTrialData nbits1 s1 nbits2 s2 s3

      waveNames = [| "try single Bit"; "try bus"; "try states" |]
      selected = [| false; false; false |]
      ports = [| (SimulatorTypes.ComponentId "qwertyuiop",   SimulatorTypes.OutputPortNumber 1), None
                 (SimulatorTypes.ComponentId "qwertyuiopa",  SimulatorTypes.OutputPortNumber 1), None
                 (SimulatorTypes.ComponentId "qwertyuiopas", SimulatorTypes.OutputPortNumber 1), None |] 
      clkWidth = 1.0
      cursor = uint32 0
      radix = Bin
      viewIndexes = (uint 0, uint 9) }

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
    { model with selected = Array.mapi (fun i old ->
                                if i = ind then not old else old) model.selected }
    |> Ok |> StartWaveSim

let makeLabels model = Array.map (fun l -> label [ Class "waveLbl" ] [ str l ]) model.waveNames

let makeSegment (clkW: float) (xInd: int) (data: Sample) (trans: int * int) =
    let top = spacing
    let bot = top + sigHeight - sigLineThick
    let left = float xInd * clkW
    let right = left + float clkW

    let makeSigLine = makeLinePoints [ Class "sigLineStyle" ]

    match data with
    | Wire w when w.nBits = uint 1 ->
        let y =
            match w.bitData with
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

let model2WaveList model: Waveform [] = Array.transpose model.waveData

let transitions (model: WaveSimModel) = //relies on number of names being correct (= length of elements in waveData)
    let isDiff (ws1, ws2) =
        let folder state e1 e2 =
            match state, e1 = e2 with
            | 0, true -> 0
            | _ -> 1
        match ws1, ws2 with
        | Wire a, Wire b ->
            if a.bitData = b.bitData then 0 else 1
        | StateSample a, StateSample b when Array.length a = Array.length b ->  (a, b) ||> Array.fold2 folder 0
        | _ -> 1

    let trans (wave: Waveform) = Array.pairwise wave |> Array.map isDiff
    model2WaveList model |> Array.map trans

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
    let cursValPrefix =
        match model.radix with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""
    let makeCursVal sample =
        match sample with
        | Wire w when w.nBits > uint 1 -> [| cursValPrefix + radixChange w.bitData w.nBits model.radix |]
        | Wire w -> [| cursValPrefix + string w.bitData |]
        | StateSample s -> s
        |> Array.map (fun l -> label [ Class "cursVals" ] [ str l ])
    Array.map makeCursVal model.waveData.[int model.cursor]

//container box and clock lines
let backgroundSvg model =
    let clkLine x =
        makeLinePoints [ Class "clkLineStyle" ] (x, vPos) (x, vPos + sigHeight + spacing)
    [| 1 .. Array.length model.waveData |] 
    |> Array.map ((fun x -> float x * model.clkWidth) >> clkLine)

let clkRulerSvg (model: WaveSimModel) =
    (fun (a,b) -> [| int a .. int b |]) model.viewIndexes
    |> Array.map (fun i -> makeText (cursRectText model i) (string i))
    |> (fun arr -> [ backgroundSvg model; [| makeRect (cursRectStyle model) |]; arr ])
    |> Array.concat 
    |> makeSvg (clkRulerStyle model)

let waveSimRows (model: WaveSimModel) dispatch =
// waveforms
    let waveSvg =
        let addLabel nLabels xInd (i: int) = makeText (inWaveLabel nLabels xInd i model)

        let valueLabels =
            let lblEl (sample, xIndArr) =
                match sample with
                | Wire w when w.nBits > uint 1 ->
                    Array.map (fun xInd -> addLabel 1 xInd 0 (radixChange w.bitData w.nBits model.radix)) xIndArr
                | StateSample ss ->
                    Array.collect (fun xInd -> Array.mapi (addLabel (Array.length ss) xInd) ss) xIndArr
                | _ -> [||]
            busLabels model |> Array.map (fun row -> Array.collect lblEl row)

        let makeWaveSvg a b = Array.mapi2 (makeSegment model.clkWidth) a b |> Array.concat
        let padTrans (t: (int * int) []) =
            Array.concat [ [| 1, fst t.[0] |]
                           t
                           [| snd (Array.last t), 1 |] ]
        transitions model
        |> Array.map (fun transRow -> Array.pairwise transRow |> padTrans)
        |> Array.map2 makeWaveSvg (model2WaveList model)
        |> Array.map2 (fun a b -> Array.append a b) valueLabels

// name and cursor labels of the waveforms
    let labels = makeLabels model
    let cursLabs = makeCursVals model

    let labelCols =
        labels
        |> Array.mapi (fun i l ->
            tr [ Class "rowHeight" ]
                [ td [ Class "checkboxCol" ]
                      [ input
                          [ Type "checkbox"
                            Checked model.selected.[i]
                            Style [ Float FloatOptions.Left ]
                            OnChange (fun _ -> toggleSelect i model |> dispatch) ] ]
                  td [ Class "waveNamesCol" ] [ l ]])

    let cursValCol =
        cursLabs
        |> Array.map (fun c ->
            tr [ Class "rowHeight" ]
                [ td [ Class "cursValsCol" ] c ])

    let waveCol =
        let waveTableRow rowClass cellClass svgClass svgChildren =
            tr rowClass [ td cellClass [ makeSvg svgClass svgChildren ] ]
        let bgSvg = backgroundSvg model
        let cursRectSvg = [| makeRect (cursRectStyle model) |]

        [| waveTableRow [ Class "fullHeight" ] (lwaveCell model) (waveCellSvg model true)
               (Array.append bgSvg cursRectSvg) |]
        |> Array.append
            (Array.map
                (fun wave ->
                    waveTableRow [ Class "rowHeight" ] (waveCell model) (waveCellSvg model false)
                        (Array.concat [| cursRectSvg; bgSvg; wave |])) waveSvg)
        |> Array.append [| tr [ Class "rowHeight" ] [ td (waveCell model) [ clkRulerSvg model ] ] |]

    waveCol, labelCols, cursValCol

// view function helpers

let zoom plus (m: WaveSimModel) =
    let multBy =
        if plus then zoomFactor else 1.0 / zoomFactor
    { m with clkWidth = m.clkWidth * multBy } |> Ok |> StartWaveSim

let button style func label =
    Button.button (List.append [ Button.Props [ style ] ] [ Button.OnClick func ]) [ str label ]

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

let cursorMove increase model =
    match increase, model.cursor, fst model.viewIndexes, snd model.viewIndexes with
    | (true, c, _, fin) when c < fin -> { model with cursor = c + uint 1 }
    | (false, c, start, _) when c > start -> { model with cursor = c - uint 1 }
    | _ -> model
    |> Ok |> StartWaveSim

let changeCurs newVal model =
    if (fst model.viewIndexes) <= newVal && (snd model.viewIndexes) >= newVal
    then { model with cursor = newVal }
    else model
    |> Ok |> StartWaveSim

let indexesMove increase upper (model: WaveSimModel) = 
    let bot, top = model.viewIndexes
    match upper, increase with 
    | false, false when bot > uint 0 -> bot  - uint 1, top
    | false, true when bot < top -> bot + uint 1, top
    | true, false when top > bot -> bot, top - uint 1
    | true, true -> bot, top + uint 1
    | _ -> bot, top
    |> (fun viewIndexes' -> { model with viewIndexes = viewIndexes' })
    |> Ok |> StartWaveSim

let changeBotInd newVal model = 
    if uint 0 <= newVal && (snd model.viewIndexes) >= newVal
    then { model with viewIndexes = newVal, snd model.viewIndexes }
    else model
    |> Ok |> StartWaveSim

let changeTopInd newVal model = 
    if uint (fst model.viewIndexes) <= newVal
    then { model with viewIndexes = fst model.viewIndexes, newVal }
    else model
    |> Ok |> StartWaveSim

let selectAll s model = { model with selected = Array.map (fun _ -> s) model.selected } |> Ok |> StartWaveSim

let allSelected model = Array.forall ((=) true) model.selected
let anySelected model = Array.exists ((=) true) model.selected

let delSelected model =
    let filtSelected arr =
        Array.zip model.selected arr
        |> Array.filter (fun (sel, _) -> not sel)
        |> Array.map snd
    { model with waveData = Array.map filtSelected model.waveData
                 waveNames = filtSelected model.waveNames
                 ports = filtSelected model.ports
                 selected = Array.filter not model.selected }
    |> Ok |> StartWaveSim

let moveWave model up =
    let lastEl (arr: 'a []) = Array.last arr

    let move arr =
        let rev a =
            if up then a else Array.rev a
        rev arr
        |> Array.fold (fun st (bl: {| sel: bool; indxs: int [] |}) ->
            match st with
            | [||] -> [| bl |]
            | _ ->
                if bl.sel then
                    Array.concat
                        [| st.[0..Array.length st - 2]
                           [| bl |]
                           [| lastEl st |] |]
                else
                    Array.append st [| bl |]) [||]
        |> rev

    let indexes' =
        match Array.length model.selected with
        | len when len < 2 -> [| 0 .. Array.length model.selected - 1 |]
        | _ ->
            Array.indexed model.selected
            |> Array.fold (fun (blocks: {| sel: bool; indxs: int [] |} []) (ind, sel') ->
                match blocks, sel' with
                | [||], s' ->
                    Array.append blocks
                        [| {| sel = s'
                              indxs = [| ind |] |} |]
                | bl, true when (lastEl bl).sel = true ->
                    Array.append blocks.[0..Array.length blocks - 2]
                        [| {| (lastEl blocks) with indxs = Array.append (lastEl blocks).indxs [| ind |] |} |]
                | _, s' ->
                    Array.append blocks
                        [| {| sel = s'
                              indxs = [| ind |] |} |]) [||]
            |> move
            |> Array.collect (fun block -> block.indxs)

    let reorder (arr: 'b []) = Array.map (fun i -> arr.[i]) indexes'
    
    { model with waveData = Array.map (fun sT -> reorder sT) model.waveData
                 waveNames = reorder model.waveNames
                 selected = reorder model.selected
                 ports = reorder model.ports}
    |> Ok |> StartWaveSim

// simulation functions 

let reloadablePorts (model: DiagramModelType.Model) (simData: SimulatorTypes.SimulationData) = 
    Array.filter (fun ((compId, _), _) -> 
        Map.exists (fun key _ -> key = compId) simData.Graph) model.WaveSim.ports
    |> Array.map (fun (a, outOpt) -> 
        match outOpt with
        | Some cId when Map.exists (fun key _ -> key = cId) simData.Graph -> a, Some cId
        | _ -> a, None )

let reloadWaves (model: DiagramModelType.Model) dispatch =
    OnDiagramButtonsView.simLst model dispatch reloadablePorts


//[<Emit("__static")>]
//let staticDir() : string = jsNative

//view functions of the waveform simulator

let radixTabs model dispatch =
    let radTab rad =
        Tabs.tab [ Tabs.Tab.IsActive (model.radix = rad)
                   Tabs.Tab.Props [Style [ Width "25px"
                                           Height "30px"] ] ]
                 [ a [ OnClick(fun _ -> Ok { model with radix = rad } |> StartWaveSim |> dispatch) ]
                 [ str (radixString rad) ] ]
    Tabs.tabs
        [ Tabs.IsBoxed; Tabs.IsToggle; Tabs.Props [ Style [ Width "100px"
                                                            FontSize "80%" 
                                                            Float FloatOptions.Right;
                                                            Margin "0 10px 0 10px" ] ] ]
        [ radTab Bin; radTab Hex; radTab Dec; radTab SDec ]

let simLimits model dispatch =
    div [ Class "limits-group" ]
        [ div [ Class "limits-but-div" ]
              [ button (Class "updownButton") (fun _ -> indexesMove true false model |> dispatch) "▲"
                button (Class "updownButton") (fun _ -> indexesMove false false model |> dispatch) "▼" ]
          input
              [ Id "cursorForm"
                Step 1
                SpellCheck false
                Class "limits-form"
                Type "number"
                Value (fst model.viewIndexes)
                Min 0
                Max (snd model.viewIndexes)
                OnChange(fun c -> changeBotInd (uint c.Value) model |> dispatch) ]
          label [ Style [Float FloatOptions.Left
                         Margin "0 5px 0 5px"] ] [ str "/"]
          input
              [ Id "cursorForm"
                Step 1
                SpellCheck false
                Class "limits-form"
                Type "number"
                Value (snd model.viewIndexes)
                Min (fst model.viewIndexes)
                OnChange(fun c -> changeTopInd (uint c.Value) model |> dispatch) ]
          div [ Class "limits-but-div" ]
              [ button (Class "updownButton") (fun _ -> indexesMove true true model |> dispatch) "▲"
                button (Class "updownButton") (fun _ -> indexesMove false true model |> dispatch) "▼" ] ] 

let cursorButtons model dispatch =
    div [ Class "cursor-group" ]
        [ buttonOriginal (Class "button-minus") (fun _ -> cursorMove false model |> dispatch) "◄"
          input
              [ Id "cursorForm"
                Step 1
                SpellCheck false
                Class "cursor-form"
                Type "number"
                Value model.cursor
                Min (fst model.viewIndexes)
                Max (snd model.viewIndexes)
                OnChange(fun c -> changeCurs (uint c.Value) model |> dispatch) ]
          buttonOriginal (Class "button-plus") (fun _ -> cursorMove true model |> dispatch) "►" ] 

let viewWaveSimButtonsBar model dispatch = 
    div [ Style [ Height "7%" ] ]
        [ button (Class "reloadButtonStyle") (fun _ -> ()) "Reload" 
          radixTabs model dispatch
          simLimits model dispatch
          cursorButtons model dispatch ]

let cursValsCol rows = 
    let rightCol = Array.append [| tr [ Class "rowHeight" ]
                                      [ td [ Class "rowHeight" ] [] ] |] rows
    div [ Style [ Float FloatOptions.Right; Height "100%"; BorderTop "2px solid rgb(219,219,219)"; BorderLeft "2px solid rgb(219,219,219)" ] ]
        [ table [] [ tbody [] rightCol ] ]

let nameLabelsCol model labelRows dispatch =
    let waveAddDelBut =
        match anySelected model with
        | true ->
            [ button (Class "newWaveButton") (fun _ -> delSelected model |> dispatch) "del"
              div [ Class "updownDiv" ]
                  [ button (Class "updownButton") (fun _ -> moveWave model true |> dispatch) "▲"
                    button (Class "updownButton") (fun _ -> moveWave model false |> dispatch) "▼" ] ]
        | false ->
            [ div [ Style [ WhiteSpace WhiteSpaceOptions.Nowrap ] ]
                  [ button (Class "newWaveButton") (fun _ -> ()) "+"
                    label [ Class "newWaveLabel" ] [ str "Add wave" ] ] ]
        |> (fun children -> th [ Class "waveNamesCol" ] children)

    let top =
        [| tr []
               [ th [ Class "checkboxCol" ]
                    [ input
                        [ Type "checkbox"
                          Checked (allSelected model)
                          OnChange(fun t -> selectAll t.Checked model |> dispatch) ] ]
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
        [ button (Class "zoomButtonStyle") (fun _ -> zoom false model |> dispatch) "-"
          //let svgPath = Path.Combine(staticDir(), "hzoom-icon.svg")
          //let svgPath = staticDir() + "\hzoom-icon.svg"
          //embed [ Src svgPath ]
          button (Class "zoomButtonStyle") (fun _ -> zoom true model |> dispatch) "+" ] 

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    [ viewWaveSimButtonsBar model.WaveSim dispatch
      viewWaveformViewer model.WaveSim dispatch
      viewZoomDiv model.WaveSim dispatch ]
