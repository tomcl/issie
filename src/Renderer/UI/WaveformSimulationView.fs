(*
    WaveformSimulationView.fs

    View for waveform simulator in tab
*)

module WaveformSimulationView

open Fulma
open Fable.React
open Fable.React.Props

open DiagramMessageType
open DiagramStyle
open CommonTypes

let clkLineWidth = 0.0125
let transLen = 0.1
let vPos = 0.0
let zoomFactor = 1.2
let maxBusValGap = 3
let busLabelTextSize = 0.6 // multiplied by signal height
 
//type functions

let initModel: WaveSimModel =
    { waveData =
          //modify these two signals to change trial data
          let nbits1 = uint32 1
          let nbits2 = uint32 4
          let s1 = [| 0; 0; 0; 0; 1; 0; 1; 1; 1; 1 |]
          let s2 = [| 1; 1; 1; 1; 14; 14; 14; 14; 2; 8 |]
          let s3 = [| 
            [|"state1"|]; [|"state1"|]; [|"state2"; "state1"|]; 
            [|"state2"|]; [|"state1"|]; [|"state2"|]; [|"state1"|]; 
            [|"state2"|]; [|"state1"|]; [|"state2"|] 
          |]

          let makeTrialData (nBits1: uint32) (signal1: int []) (nBits2: uint32) signal2 signal3 : SimTime [] =
              let makeTimePointData (s1: int, s2: int, s3) : SimTime =
                  [| Wire { nBits = nBits1; bitData = bigint s1 }
                     Wire { nBits = nBits2; bitData = bigint s2 }
                     StateSample s3 |]
              Array.zip signal2 signal3
              |> Array.zip signal1
              |> Array.map ((fun (a,(b,c)) -> (a,b,c)) >> makeTimePointData)
          makeTrialData nbits1 s1 nbits2 s2 s3

      waveNames = [| 
        "try single Bit"; 
        "try bus"; 
        "try states" 
      |]

      posParams =
          { sigHeight = 0.5
            hPos = uint 0
            clkWidth = 1.0
            labelWidth = uint 2
            sigThick = 0.02
            boxWidth = uint 8
            boxHeight = uint 15
            spacing = 0.5
            clkThick = 0.025 }

      cursor = uint32 0
    
      radix = Bin 
    
      viewIndexes = (uint 0, uint 9) }

// SVG functions

let makeLine style attr = line (List.append [style] attr) []
let makeRect style attr = rect (List.append [style] attr) []
let makeText style attr t = text (List.append [style] attr) [str t]
let makeSvg style attr elements = svg (List.append [style] attr) elements
let makeLinePoints style (x1, y1) (x2, y2) = makeLine style [ X1 x1; Y1 y1; X2 x2; Y2 y2 ]

//radix change

let dec2bin (n: bigint) (nBits: uint32) : string = //unsigned
    let folder (state: bigint*char list) (digit: int) = 
        if fst state / bigint digit = bigint 1
            then (fst state - bigint digit, List.append (snd state) ['1'])
            else (fst state , List.append (snd state) ['0'])
    [float nBits-1.0..(-1.0)..0.0]
    |> List.map ((fun exp -> 2.0**exp) >> (fun f -> int f))
    |> List.fold folder (n, [])
    |> snd |> List.toSeq |> Seq.map string |> String.concat ""

let dec2hex (n: bigint) (nBits: uint32) : string = //2s complement
    let seqPad = [1..(4 - int nBits % 4) % 4] |> List.map (fun _ -> '0')
    let paddedBin = dec2bin n nBits |> Seq.toList |> List.append seqPad 
    let fourBitToHexDig fourBit = 
        match fourBit with
        | ['0';'0';'0';'0'] -> '0'
        | ['0';'0';'0';'1'] -> '1'
        | ['0';'0';'1';'0'] -> '2'
        | ['0';'0';'1';'1'] -> '3'
        | ['0';'1';'0';'0'] -> '4'
        | ['0';'1';'0';'1'] -> '5'
        | ['0';'1';'1';'0'] -> '6'
        | ['0';'1';'1';'1'] -> '7'
        | ['1';'0';'0';'0'] -> '8'
        | ['1';'0';'0';'1'] -> '9'
        | ['1';'0';'1';'0'] -> 'A'
        | ['1';'0';'1';'1'] -> 'B'
        | ['1';'1';'0';'0'] -> 'C'
        | ['1';'1';'0';'1'] -> 'D'
        | ['1';'1';'1';'0'] -> 'E'
        | ['1';'1';'1';'1'] -> 'F'
        | _ -> 'N' // maybe should deal with exception differently
    [0..4..int nBits - 1] 
    |> List.map ((fun i -> paddedBin.[i..i + 3]) >> fourBitToHexDig)
    |> List.toSeq |> Seq.map string |> String.concat ""

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits).[0] = '1'
        then n - bigint (2.0**(float nBits))
        else n
    |> string

let radixChange (n: bigint) (nBits: uint32) (rad: NumberBase) = 
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits

//auxiliary functions to the viewer function

(*let makeLabel (p: PosParamsType) label =
    let attr: IProp list = [ Y (p.spacing + p.sigHeight)
                             SVGAttr.FontSize (p.sigHeight * 0.6) ] 
    makeText waveLblStyle attr label*)
let makeLabels model =
    Array.map (fun l -> label [] [str l]) model.waveNames 

let makeSegment (p: PosParamsType) (xInd: int)  ((data: Sample), (trans: int * int)) =
    let bot = p.spacing + p.sigHeight
    let top = bot - p.sigHeight
    let left = float xInd * p.clkWidth
    let right = left + float p.clkWidth

    let makeSigLine = makeLinePoints (Class "sigLineStyle")

    match data with
    | Wire w when w.nBits = uint 1 ->
        let y =
            match w.bitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        // TODO: define DU so that you can't have values other than 0 or 1
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | 1 -> [| makeSigLine (right, bot + p.sigThick / 2.0) (right, top - p.sigThick / 2.0) |]
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
        let topL =     makeSigLine (leftInner, top) (rightInner, top)
        let botL =     makeSigLine (leftInner, bot) (rightInner, bot)
        let topLeft =  makeSigLine (left, cen) (leftInner, top)
        let botLeft =  makeSigLine (left, cen) (leftInner, bot)
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

let model2WaveList model : Waveform [] =
    let folder state (simT: SimTime) : Waveform [] =
        Array.zip state simT
        |> Array.map (fun (arr,sample) -> Array.append arr [| sample |])
    let initState = Array.map (fun _ -> [||]) model.waveNames
    Array.fold folder initState model.waveData

let transitions (model: WaveSimModel) = //relies on number of names being correct (= length of elements in waveData)
    let isDiff (ws1,ws2) =
        let folder state (e1,e2) =
            match state, e1 = e2 with
            | 0, true -> 0
            | _ -> 1
        match ws1, ws2 with 
        | Wire a, Wire b -> if a.bitData = b.bitData then 0 else 1
        | StateSample a, StateSample b when Array.length a = Array.length b -> 
            Array.zip a b |> Array.fold folder 0
        | _ -> 1
    let trans (wave: Waveform) = 
        Array.pairwise wave |> Array.map isDiff
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
                    {| GapLen=Array.max times - Array.min times + 1; GapStart=Array.min times|})

let busLabels model = 
    let gaps2pos (wave:Waveform,gaps) = 
        let nSpaces (g:{|GapLen:int; GapStart:int|}) = (g.GapLen / (maxBusValGap + 1) + 2)
        let gapAndInd2Pos (g:{|GapLen:int; GapStart:int|}) i = 
            float g.GapStart + float i * float g.GapLen / float (nSpaces g)
        gaps
        |> Array.map (fun (gap:{|GapLen:int; GapStart:int|}) -> wave.[gap.GapStart], Array.map (gapAndInd2Pos gap) [| 1..nSpaces gap - 1 |])
    (model2WaveList model, Array.map makeGaps (transitions model))
    ||> Array.zip
    |> Array.map gaps2pos

let makeCursVals model =
   let makeCursVal sample = 
       match sample with
       | Wire w when w.nBits > uint 1 -> [| radixChange w.bitData w.nBits model.radix |]
       | Wire w -> [| string w.bitData |]
       | StateSample s -> s
       |> Array.map (fun l -> label [] [str l])
   Array.map makeCursVal model.waveData.[int model.cursor]

let makeCursRect model =
    let p = model.posParams
    let attr: IProp list =
        [ X (p.clkWidth * float model.cursor + clkLineWidth / 2.0)
          SVGAttr.Width (p.clkWidth - clkLineWidth)
          SVGAttr.Height (p.spacing + p.sigHeight) ]
    [| makeRect (Class "cursorRectStyle") attr |]

let clkRulerSvg (model: WaveSimModel) = 
    let VBwidth = model.posParams.clkWidth * float (Array.length model.waveData) 
    let clkVB = "0 0 " + string VBwidth + " 0.25" |> ViewBox
    [| 0..(Array.length model.waveData - 1) |] 
    |> Array.map (fun x -> 
        makeText (Class "clkNumStyle")
                 [ X (model.posParams.clkWidth * (float x + 0.5)) 
                   Y 0.25 ] 
                 (string x))
    |> makeSvg (Class "clkRulerSvgStyle") 
               [ clkVB; Style [ Width ((string (VBwidth*10.0) ) + "%") ] ]

let displaySvg (model: WaveSimModel) =
    let p = model.posParams

    //container box and clock lines
    let backgroundSvg =
        let clkLine x = makeLinePoints (Class "clkLineStyle") (x, vPos) (x, vPos + float p.sigHeight + float p.spacing)
        let clkLines =
            [| 1 .. 1 .. Array.length model.waveData |] |> Array.map ((fun x -> float x * p.clkWidth) >> clkLine)
        clkLines

    // waveforms
    let VBwidth = p.clkWidth * float (Array.length model.waveData)
    let wavesVB =
        let appInv a b = b + a
        "0 0 " + string VBwidth
        |> appInv " "
        |> appInv (string (p.spacing + p.sigHeight))
        |> string
        |> ViewBox

    let waveSvg =
        let addLabel nLabels xInd (i: int) lbl = 
            let attr: IProp list = [
                X (xInd * p.clkWidth)
                Y (p.spacing + p.sigHeight - p.sigHeight * 0.3 + 0.3 * p.sigHeight * (float i - (float nLabels - 1.0) / 2.0))
                SVGAttr.FontSize (busLabelTextSize * p.sigHeight / float nLabels) 
            ]
            makeText (Class "busValueStyle") attr lbl

        let mapiAndCollect func = Array.mapi func >> Array.collect id

        let valueLabels = 
            let lblEl (sample, xIndArr) = 
                match sample with 
                | Wire w when w.nBits > uint 1 -> 
                                    Array.map (fun xInd -> 
                                                addLabel 1 xInd 0 (radixChange w.bitData w.nBits model.radix)) xIndArr
                | StateSample ss -> Array.collect (fun xInd -> Array.mapi (addLabel (Array.length ss) xInd) ss) xIndArr
                | _ -> [| |]
            busLabels model
            |> Array.map (fun row -> Array.collect lblEl row) 

        let makeWaveSvg = mapiAndCollect (makeSegment p)
        let padTrans (t: (int*int) []) = Array.append (Array.append [| 1, fst t.[0] |] t) [| snd t.[Array.length t - 1], 1 |]
        (model2WaveList model, transitions model)
        ||> Array.zip 
        |> Array.map (fun (wave, transRow) -> Array.zip wave (padTrans (Array.pairwise transRow)))
        |> Array.map makeWaveSvg
        |> Array.zip valueLabels
        |> Array.map (fun (a,b) -> Array.append a b)

    // name and cursor labels of the waveforms
    let labels = makeLabels model
    let cursLabs = makeCursVals model

    let labelCols =
        Array.zip labels cursLabs
        |> Array.map (fun (l, c) -> tr [Style [Height "5%"]] 
                                       [ td [Class "waveNamesCol"] [l]
                                         td [Class "cursValsCol"] c ] ) 

    let waveCol = 
        [| tr [Style [Height "100%"]] 
              [ td [Style [ Height "100%"] ] 
                   [makeSvg (Style [Height "100%"; Width (string (10.0 * VBwidth)+"%"); Display DisplayOptions.Block]) [wavesVB; PreserveAspectRatio "none"] backgroundSvg ] ] |]
        |> Array.append (Array.map (fun w -> 
                    tr [Style [Height "5%"]] 
                       [ td [ Style [Width ((string (max (VBwidth*10.0) 100.0))+"%"); Height "5%"] ] 
                            [makeSvg (Style [Width (string (10.0 * VBwidth)+"%"); Display DisplayOptions.Block; Height "5%"; Position PositionOptions.Absolute]) [wavesVB; PreserveAspectRatio "none"] (Array.append (Array.append backgroundSvg (makeCursRect model)) w) ] ] ) waveSvg)
        |> Array.append [| tr [Style [Height "5%"]] 
                              [ td [Style [Width ((string (max (VBwidth*10.0) 100.0))+"%")]] 
                                   [clkRulerSvg model] ] |]
    
    (table [Class "wavesColTableStyle"] waveCol), labelCols

// view function helpers

let zoom plus (*horizontal*) (m: WaveSimModel) =
    let multBy =
        if plus then zoomFactor else 1.0 / zoomFactor
    (*match horizontal with
    | false ->
        let newParams =
            { m.posParams with
                    sigHeight = m.posParams.sigHeight * multBy
                    spacing = m.posParams.spacing * multBy }
        { m with posParams = newParams }
    | true -> *)
    { m with posParams = { m.posParams with clkWidth = m.posParams.clkWidth * multBy } }
    |> StartWaveSim

let button style func label =
    Button.button (List.append [Button.Props [style]] [Button.OnClick func]) [ str label ]

let radixString rad =
    match rad with
    | Dec -> "Dec"
    | Bin -> "Bin"
    | Hex -> "Hex"
    | SDec -> "sDec"

let cursorMove increase model = 
    match increase, model.cursor, fst model.viewIndexes, snd model.viewIndexes with 
    | (true, c, _, fin) when c < fin -> {model with cursor = c + uint 1}
    | (false, c, start, _) when c > start -> {model with cursor = c - uint 1}
    | _ -> model
    |> StartWaveSim

//view function of the waveform simulator

let viewWaveSim (fullModel: DiagramModelType.Model) dispatch =   
    let model = fullModel.WaveSim
    let p = model.posParams
    let VBwidth = p.clkWidth * float (Array.length model.waveData)

    [
    div []
        [ button (Class "reloadButtonStyle") (fun _ -> ()) "Reload" 

          div [ Class "floatLeft" ]
              [ span [ Class "floatLeft" ] [ button (Class "cursorButtonStyle") (fun _ -> cursorMove false model |> dispatch) "-" ]
                input [ Class "floatLeft" ; Type "number"; Value "0"]
                span [ Class "floatLeft" ] [ button (Class "cursorButtonStyle") (fun _ -> cursorMove true model |> dispatch) "+" ] ] 

          Tabs.tabs [Tabs.IsBoxed; Tabs.IsToggle; Tabs.Props [ Style [FontSize "80%"; Float FloatOptions.Right]]] [
            let radTab rad =
                Tabs.tab
                    [ Tabs.Tab.IsActive (model.radix = rad) ]
                    [ a [ OnClick (fun _ -> StartWaveSim {model with radix = rad} |> dispatch ) ] [ str (radixString rad) ] ]
            radTab Bin
            radTab Hex
            radTab Dec
            radTab SDec ]  ]

    let tableWaves, tableBody = displaySvg model
    let tableTop = 
        [| col [ Style [BorderRight "2px solid black"]] 
           col [ Style [ Width ((string (max (VBwidth*10.0) 100.0))+"%")]; Class "wavesColStyle" ]
           col [ Style [Width "5%"; BorderLeft "2px solid black"] ] 
           tr []
              [ th [] [ //Checkbox.input [ Props [ Style [ Float FloatOptions.Left ] ] ] 
                       button (Class "cursorButtonStyle") (fun _ -> ()) "+" ]
                td [RowSpan (Array.length model.waveNames + 2)] 
                  [ div [Class "wavesColDivStyle"] [tableWaves] ] 
                th [] [ label [] [str ""] ] ] |]
    let tableBot = [| tr [Style [Height "100%"]] [td [Style [Stroke "white"]] [str "..."]; td [Style [Stroke "white"]] [str "..."] ] |]
    table [ Class "waveSimTableStyle" ]
          (Array.append (Array.append tableTop tableBody) tableBot)

    div [Class "zoomDiv"]
        [ button (Class "zoomButtonStyle") (fun _ -> zoom false model |> dispatch) "-"
          label [Style [Float FloatOptions.Left; Position PositionOptions.Relative]] [str "H Zoom"]
          button (Class "zoomButtonStyle") (fun _ -> zoom true model |> dispatch) "+" ] ]
