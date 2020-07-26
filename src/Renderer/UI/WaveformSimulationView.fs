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

          let makeTrialData (nBits1: uint32) signal1 (nBits2: uint32) signal2 =
              let makeTimePointData (s1: int, s2: int) =
                  [| { nBits = nBits1; bitData = bigint s1 }
                     { nBits = nBits2; bitData = bigint s2 } |]
              Array.zip signal1 signal2
              |> Array.map makeTimePointData
          makeTrialData nbits1 s1 nbits2 s2

      waveNames = [| "try single Bit"; "try bus" |]

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

let displaySvg ((model: WaveSimModel), (trans: (bool * bool) [] []), (busLabels: int [] * (float * bigint) [] [])) =
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
    let makeSegment (xInd: int) (yInd: int) ((data: Wire), (trans: bool * bool)) =
        let bot = (p.spacing + p.sigHeight) * (float yInd + 1.0)
        let top = bot - p.sigHeight
        let left = float xInd * p.clkWidth
        let right = left + float p.clkWidth
        
        let makeSigLine = makeLinePoints sigLineStyle

        match data.nBits with
        | n when n = uint 1 ->
            let y =
                match data.bitData with
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
    let waveSvg =
        let mapiAndCollect func = Array.mapi func >> Array.collect id
        let makeWaveSvgCol xInd = mapiAndCollect (makeSegment xInd)
        let valueLabels = 
            let lblEl (yInd: int) ((xInd: float), (lbl: bigint)) =
                let attr: IProp list = [
                    X (xInd * p.clkWidth)
                    Y ((p.spacing + p.sigHeight) * (float yInd + 1.0) - p.sigHeight * 0.1)
                    SVGAttr.FontSize (0.8 * p.sigHeight) 
                ]
                makeText busValueStyle attr (string lbl)
            Array.zip (fst busLabels) (snd busLabels)
            |> Array.collect (fun (yInd, tup) -> Array.map (lblEl yInd) tup) 
        Array.zip model.waveData trans
        |> Array.map (fun (arr1, arr2) -> Array.zip arr1 arr2)
        |> mapiAndCollect makeWaveSvgCol
        |> Array.append valueLabels

    // name labels of the waveforms
    let makeLabel (ind: int) label =
        let attr: IProp list = [ Y ((p.spacing + p.sigHeight) * (float ind + 1.0))
                                 SVGAttr.FontSize (0.25 + p.sigHeight * 0.3) ] 
        makeText waveLblStyle attr label

    let labelSvg = Array.mapi makeLabel model.waveNames

    labelSvg, snd backgroundSvg, Array.append waveSvg (fst backgroundSvg)

//view function of the waveform simulator

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    let startWaveSim() = StartWaveSim initModel |> dispatch

    let zoom plus horizontal () =
        let multBy =
            if plus then zoomFactor else 1.0 / zoomFactor 
        match model.WaveSim with
        | Some m ->
            match horizontal with
            | false ->
                let newParams =
                    { m.posParams with
                          sigHeight = m.posParams.sigHeight * multBy
                          spacing = m.posParams.spacing * multBy }
                { m with posParams = newParams }
            | true -> { m with posParams = { m.posParams with clkWidth = m.posParams.clkWidth * multBy } }
            |> StartWaveSim
            |> dispatch
        | _ -> printf "What? Zoom function called when model.WaveSim is None"

    let button color func label =
        Button.button
            [ Button.Color color
              Button.OnClick func ] [ str label ]

    match model.WaveSim with
    | None -> div [] [ button IsSuccess (fun _ -> startWaveSim()) "Start waveform simulator" ]
    | Some simModel ->
        let endWaveSim _ =
            dispatch CloseWaveSimNotification // Copied this, don't know if necessary + it's not doing anything now I think
            dispatch EndWaveSim

        let waveLen = Array.length simModel.waveData
        let nWaves = Array.length simModel.waveNames

        let transitions = //relies that the number of names is correct (= length of elements in waveData
            let trueArr = [| Array.map (fun _ -> true) [| 1 .. nWaves |] |]
            let diffArray (arr1, arr2) = 
                Array.zip arr1 arr2 |> Array.map (fun (a, b) -> a <> b)
            let transArr =
                Array.zip simModel.waveData.[0..waveLen - 2] simModel.waveData.[1..waveLen - 1]
                |> Array.map diffArray
            Array.zip (Array.append trueArr transArr) (Array.append transArr trueArr)
            |> Array.map (fun (a, b) -> Array.zip a b)

        let busLabels = 
            //TODO this relies on the fact that the last transition of a signal is set to true 
            let busTransVals =
                let zipAndRemSingleBit (tup: (bool*bool) [] * SimTime) = 
                    Array.zip (fst tup) (snd tup)
                    |> Array.filter (fun t -> (snd t).nBits > uint 1)
                    |> Array.map (fun t -> snd (fst t), (snd t).bitData)
                Array.zip transitions simModel.waveData
                |> Array.map zipAndRemSingleBit

            match busTransVals with
            | [||] ->
                [||],[||]
            | _ ->                
                let busYind = 
                    Array.zip simModel.waveData.[0] [| 0..nWaves - 1 |]
                    |> Array.filter (fun (w,_) -> w.nBits > uint 1)
                    |> Array.map (fun (w,ind) -> ind)
                let initState = Array.map (fun (_,firstVal) -> [| 0, 1, firstVal |]) busTransVals.[0] // relies on all elements having same length
                let gapMake (state: (int*int*bigint) [] []) (tupArr: (bool*bigint) []) =
                    let updateState ((sLst: (int*int*bigint) []),(dataTup: bool*bigint)) =
                        let sLstLen = Array.length sLst
                        let sLstLast = sLst.[sLstLen - 1]
                        let tupSec (a,b,c) = b
                        let tupFst (a,b,c) = a
                        match fst dataTup with
                        | true ->
                            Array.append 
                                sLst 
                                [| tupSec sLstLast, tupSec sLstLast + 1, snd dataTup |]
                        | false ->
                            Array.append 
                                sLst.[0..sLstLen-2] 
                                [| tupFst sLstLast, tupSec sLstLast + 1, snd dataTup |]
                    Array.map updateState <| Array.zip state tupArr
                let fromGapsToPositions (start,fin,value) = 
                    let gap = fin - start
                    let nSpaces = float (gap / (maxBusValGap + 1) + 2)
                    Array.map (fun i -> float start + float gap / nSpaces * float i, value) [| 1.0..nSpaces - 1.0 |]
                let lblData = 
                    Array.fold gapMake initState busTransVals
                    |> Array.map (fun arr -> Array.collect fromGapsToPositions arr)
                busYind, lblData
 
            

        div []
            [ button IsDanger endWaveSim "Close waveform simulator"
              div []
                  [ Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ]
                        [ str "The simulator uses the diagram at the time of pressing the button" ] ]
              button IsGrey (fun _ -> zoom true true ()) "H Zoom +"
              button IsDanger (fun _ -> zoom false true ()) "H Zoom -"
              button IsGrey (fun _ -> zoom true false ()) "V Zoom +"
              button IsDanger (fun _ -> zoom false false ()) "V Zoom -"
              hr []

              let p = simModel.posParams
              let appInv a b = b + a
              let nSig = Array.length simModel.waveNames
              let VBwidth = p.clkWidth * float waveLen
              let VBheight = float nSig * (p.sigHeight + p.spacing) + waveVBextraHeight

              let labelVB =
                  "0 0 2 " + string VBheight
                  |> string
                  |> ViewBox

              let wavesVB =
                  "0 0 " + string VBwidth
                  |> appInv " "
                  |> appInv (string VBheight)
                  |> string
                  |> ViewBox

              let boxVB = 
                  "0 0 8 " + string VBheight |> ViewBox

              let VBwidthPercentage =
                  100.0 * VBwidth / 8.0
                  |> int
                  |> string
                  |> appInv "%"
                  |> string

              let (lblSvg, boxSvg, wfrmSvg) = displaySvg (simModel, transitions, busLabels)

              div [ waveLblDivStyle ] [ makeSvg waveLblSvgStyle [labelVB] lblSvg ]
              div
                  [ waveContDivStyle ]
                  [ makeSvg boxSvgStyle [boxVB] boxSvg
                    div
                        [ waveRightSmallDivStyle ]
                        [ makeSvg [unbox ("width", VBwidthPercentage)] [wavesVB] wfrmSvg ] ] ]