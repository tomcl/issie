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

//type functions

let initModel: WaveSimModel =
    { waveData =
          //modify these two signals to change trial data
          let nbits1 = uint32 1
          let nbits2 = uint32 5
          let s1 = [| 0; 0; 0; 0; 1; 0; 1; 1; 1; 1 |]
          let s2 = [| 1; 1; 1; 1; 14; 14; 14; 14; 2; 8 |]
          let s3 = [| [|"state1"|]; [|"state1"|]; [|"state2"; "state1"|]; [|"state2"|]; [|"state1"|]; [|"state2"|]; [|"state1"|]; [|"state2"|]; [|"state1"|]; [|"state2"|] |]

          let makeTrialData (nBits1: uint32) (signal1: int []) (nBits2: uint32) signal2 signal3 : SimTime [] =
              let makeTimePointData (s1: int, s2: int, s3) : SimTime =
                  [| Wire { nBits = nBits1; bitData = bigint s1 }
                     Wire { nBits = nBits2; bitData = bigint s2 }
                     StateSample s3 |]
              Array.zip signal2 signal3
              |> Array.zip signal1
              |> Array.map ((fun (a,(b,c)) -> (a,b,c)) >> makeTimePointData)
          makeTrialData nbits1 s1 nbits2 s2 s3

      waveNames = [| "try single Bit"; "try bus";"try states" |]

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

      cursor = uint32 0 }

// SVG functions

let makeLine style attr = line (List.append style attr) []
let makeRect style attr = rect (List.append style attr) []
let makeText style attr t = text (List.append style attr) [str t]
let makeSvg style attr elements = svg (List.append style attr) elements
let makeLinePoints style (x1, y1) (x2, y2) = makeLine style [ X1 x1; Y1 y1; X2 x2; Y2 y2 ]

//auxiliary functions to the viewer function

let makeLabel (p: PosParamsType) (ind: int) label =
    let attr: IProp list = [ Y ((p.spacing + p.sigHeight) * (float ind + 1.0))
                             SVGAttr.FontSize (p.sigHeight * 0.6) ] 
    makeText waveLblStyle attr label

let makeSegment (p: PosParamsType) (xInd: int) (yInd: int) ((data: Sample), (trans: bool * bool)) =
    let bot = (p.spacing + p.sigHeight) * (float yInd + 1.0)
    let top = bot - p.sigHeight
    let left = float xInd * p.clkWidth
    let right = left + float p.clkWidth
    
    let makeSigLine = makeLinePoints sigLineStyle

    match data with
    | Wire w when w.nBits = uint 1 ->
        let y =
            match w.bitData with
            | n when n = bigint 1 -> top
            | _ -> bot
        // TODO: define DU so that you can't have values other than 0 or 1
        let sigLine = makeSigLine (left, y) (right, y)
        match snd trans with
        | true -> [| makeSigLine (right, bot + p.sigThick / 2.0) (right, top - p.sigThick / 2.0) |]
        | false -> [||]
        |> Array.append [| sigLine |]
    | _ ->
        let leftInner =
            if fst trans then left + transLen else left
        let rightInner =
            if snd trans then right - transLen else right

        let cen = (top + bot) / 2.0

        //make lines
        let topL =     makeSigLine (leftInner, top) (rightInner, top)
        let botL =     makeSigLine (leftInner, bot) (rightInner, bot)
        let topLeft =  makeSigLine (left, cen) (leftInner, top)
        let botLeft =  makeSigLine (left, cen) (leftInner, bot)
        let topRight = makeSigLine (right, cen) (rightInner, top)
        let botRight = makeSigLine (right, cen) (rightInner, bot)

        (*let busValText =
            let attr: IProp list = [
                X((left + right) / 2.0)
                Y(bot - p.sigHeight * 0.1)
                SVGAttr.FontSize(0.8 * p.sigHeight) 
            ]
            makeText busValueStyle attr (string data.bitData)*)

        match trans with
        | true, true -> [| topLeft; botLeft; topRight; botRight |]
        | true, false -> [| topLeft; botLeft |]
        | false, true -> [| topRight; botRight |]
        | false, false -> [||]
        //|> Array.append [| busValText; topL; botL |]
        |> Array.append [| topL; botL |]
    //Probably should put other option for negative number which prints an error


let transitions (model: WaveSimModel) = //relies that the number of names is correct (= length of elements in waveData
    let waveLen = Array.length model.waveData
    let trueArr = [| Array.map (fun _ -> true) [| 1 .. Array.length model.waveNames |] |]
    let diffArray (arr1, arr2) = 
        Array.zip arr1 arr2 |> Array.map (fun (a, b) -> a <> b)
    let transArr =
        Array.zip model.waveData.[0..waveLen - 2] model.waveData.[1..waveLen - 1]
        |> Array.map diffArray
    Array.zip (Array.append trueArr transArr) (Array.append transArr trueArr)
    |> Array.map (fun (a, b) -> Array.zip a b)


// functions for bus labels

type gapMakeState = {| start: int; finish: int; value: string [] |}
type gapMakeInput = {| trans: bool; value: string [] |}

let isNotSingleBit sample =
    match sample with
    | Wire w -> w.nBits > uint 1
    | StateSample _ -> true

let busTransVals (model: WaveSimModel) (transitions: (bool*bool) [] []) =
    let outTransAndValue (sample: {| t: bool; wD: Sample |}) : {| trans: bool; value: string [] |}  =
        match sample.wD with
        | Wire s -> {| trans = sample.t; value = [| string s.bitData |]; |}
        | StateSample s ->  {| trans = sample.t; value = s; |}
    let zipAndRemSingleBit (data: {| t: (bool*bool) []; wD: SimTime |}) = 
        Array.zip data.t data.wD
        |> Array.mapi (fun i (t,wD) -> {| t = fst t; wD = wD |})
        |> Array.filter (fun r -> isNotSingleBit r.wD)
        |> Array.map outTransAndValue
    Array.zip transitions model.waveData
    |> Array.map ((fun (t,wD) -> {| t = t; wD = wD |}) >> zipAndRemSingleBit)

let gapMake (state: gapMakeState [] []) (tupArr: gapMakeInput []) : gapMakeState [] [] =
    let updateState ((sArr: gapMakeState []), (dataTup: gapMakeInput)) =
        let sArrLast = sArr.[Array.length sArr - 1]
        match sArr, dataTup.trans with
        | [||], _ ->
            [| {| start = 0; finish = 1; value = dataTup.value |} |]
        | _, true ->
            Array.append 
                sArr 
                [| {| start = sArrLast.finish; finish = sArrLast.finish + 1; value = dataTup.value |} |]
        | _, false ->
            Array.append 
                sArr.[0..Array.length sArr-2] 
                [| {| start = sArrLast.start; finish = sArrLast.finish + 1; value = dataTup.value |} |]
    Array.map updateState <| Array.zip state tupArr

let busLabels (model: WaveSimModel) (transitions: (bool*bool) [] []) = 
    match busTransVals model transitions with
    | [||] ->
        [||]
    | b ->      
        let busYind = 
            Array.zip model.waveData.[0] [| 0..Array.length model.waveNames - 1 |]
            |> Array.filter (fun (w,_) -> isNotSingleBit w)
            |> Array.map (fun (w,ind) -> ind)
        let initState = Array.map (fun _ -> [||]) b.[0] // relies on all elements having same length
        
        let fromGapsToPositions (constValSection: gapMakeState) = 
            let gap = constValSection.finish - constValSection.start
            let nSpaces = float (gap / (maxBusValGap + 1) + 2)
            Array.map (fun i -> float constValSection.start + float gap / nSpaces * float i, constValSection.value) [| 1.0..nSpaces - 1.0 |]
        let lblData = 
            Array.fold gapMake initState b
            |> Array.map (fun arr -> Array.collect fromGapsToPositions arr)
        Array.zip busYind lblData
        |> Array.map (fun (yInd, xAndStringArr) -> {| yInd = yInd; xIndAndLabels = xAndStringArr |})
 

let displaySvg (model: WaveSimModel) =
    let p = model.posParams

    //container box and clock lines
    let backgroundSvg =
        let top = vPos
        let bot = top + float p.boxHeight
        let waveLen = Array.length model.waveData |> float

        let clkLine x = makeLinePoints clkLineStyle (x, top) (x, bot)
        let clkLines =
            [| 1 .. 1 .. (int (waveLen / p.clkWidth) + 1) |] |> Array.map ((fun x -> float x * p.clkWidth) >> clkLine)
        let simBox = [| makeRect boxLineStyle [Y top; SVGAttr.Height (bot-top)] |]
        clkLines, simBox

    // waveforms
    let waveSvg =
        let addLabel nLabels yInd xInd i lbl = 
            let attr: IProp list = [
                X (xInd * p.clkWidth)
                Y ((p.spacing + p.sigHeight) * (float yInd + 1.0) - p.sigHeight * 0.3 + 0.3 * p.sigHeight * (float i - (float nLabels - 1.0) / 2.0))
                SVGAttr.FontSize (busLabelTextSize * p.sigHeight / float nLabels) 
            ]
            makeText busValueStyle attr lbl

        let valueLabels = 
            let lblEl (yInd: int) ((xInd: float), (lblArr: string [])) =
                Array.mapi (addLabel (Array.length lblArr) yInd xInd) lblArr
            busLabels model (transitions model)
            |> Array.collect (fun row -> Array.collect (lblEl row.yInd) row.xIndAndLabels) 

        let mapiAndCollect func = Array.mapi func >> Array.collect id
        let makeWaveSvgCol xInd = mapiAndCollect (makeSegment p xInd)
        Array.zip model.waveData (transitions model)
        |> Array.map (fun (arr1, arr2) -> Array.zip arr1 arr2)
        |> mapiAndCollect makeWaveSvgCol
        |> Array.append valueLabels

    // name labels of the waveforms
    let labelSvg = Array.mapi (makeLabel p) model.waveNames

    labelSvg, snd backgroundSvg, Array.append waveSvg (fst backgroundSvg)

// view function helpers

let zoom plus horizontal (m: WaveSimModel) =
    let multBy =
        if plus then zoomFactor else 1.0 / zoomFactor
    match horizontal with
    | false ->
        let newParams =
            { m.posParams with
                    sigHeight = m.posParams.sigHeight * multBy
                    spacing = m.posParams.spacing * multBy }
        { m with posParams = newParams }
    | true -> { m with posParams = { m.posParams with clkWidth = m.posParams.clkWidth * multBy } }
    |> StartWaveSim

let button color func label =
    Button.button
        [ Button.Color color
          Button.OnClick func ] [ str label ]

//view function of the waveform simulator

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    let startWaveSim() = StartWaveSim initModel |> dispatch

    match model.WaveSim with
    | None -> div [] [ button IsSuccess (fun _ -> startWaveSim()) "Start waveform simulator" ]
    | Some simModel ->
        let endWaveSim _ =
            dispatch CloseWaveSimNotification // Copied this, don't know if necessary + it's not doing anything now I think
            dispatch EndWaveSim         

        div []
            [ button IsDanger endWaveSim "Close waveform simulator"
              div []
                  [ Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ]
                        [ str "The simulator uses the diagram at the time of pressing the button" ] ]
              button IsGrey (fun _ -> zoom true true simModel |> dispatch) "H Zoom +"
              button IsDanger (fun _ -> zoom false true simModel |> dispatch) "H Zoom -"
              button IsGrey (fun _ -> zoom true false simModel |> dispatch) "V Zoom +"
              button IsDanger (fun _ -> zoom false false simModel |> dispatch) "V Zoom -"
              hr []

              let p = simModel.posParams
              let appInv a b = b + a
              let nSig = Array.length simModel.waveNames
              let VBwidth = p.clkWidth * float (Array.length simModel.waveData)
              let VBheight = float nSig * (p.sigHeight + p.spacing) + waveVBextraHeight

              let labelVB = "0 0 2 " + string VBheight |> string |> ViewBox
              let boxVB = "0 0 8 " + string VBheight |> ViewBox
              let wavesVB =
                  "0 0 " + string VBwidth
                  |> appInv " "
                  |> appInv (string VBheight)
                  |> string
                  |> ViewBox

              let VBwidthPercentage =
                  100.0 * VBwidth / 8.0
                  |> int
                  |> string
                  |> appInv "%"
                  |> string

              let (lblSvg, boxSvg, wfrmSvg) = displaySvg simModel

              div [ waveLblDivStyle ] [ makeSvg waveLblSvgStyle [labelVB] lblSvg ]
              div
                  [ waveContDivStyle ]
                  [ makeSvg boxSvgStyle [boxVB] boxSvg
                    div
                        [ waveRightSmallDivStyle ]
                        [ makeSvg [unbox ("width", VBwidthPercentage)] [wavesVB] wfrmSvg ] ] ]