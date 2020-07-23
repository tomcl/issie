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
open SimulatorTypes //Simulator is below as a module, is that a problem?

//type functions

let signalLength (signal: Signal) = 
    match snd signal with
    | OneBitSig lst -> List.length lst
    | ManyBitSig lst -> List.length lst

//initial model

//use this initially for debugging
let stdWave : Wave = 
    {
        wIn = "Signal-example", OneBitSig [One]
        cursorPos = uint 0
    }

let initModel : WaveSimModel = //modify the position constants
    {
        waves = [
                { stdWave with wIn = "Signal1", OneBitSig [One;Zero;Zero;One;One;Zero;Zero;One;One;Zero;Zero;One;One;Zero;Zero;One]}
                { stdWave with wIn = "Signal2", ManyBitSig [uint 4;uint 8;uint 3;uint 5;uint 0]}
                ]
        viewParams = {
            vPos      = uint 0
            vSize     = 0.5
            hPos      = uint 0
            hSize     = 1.0
            hNameSize = uint 2
            hValSize  = uint 8
            sigThick  = 0.02
            hBoxSize = uint 8
            vBoxSize  = uint 15
            spacing = 0.5
        }
        sigLimits = uint 0, uint 10
    }

// SVG functions

let makeLine (arg: LineParams) =
    line 
        [
        X1 (fst arg.pointA)
        Y1 (snd arg.pointA)
        X2 (fst arg.pointB)
        Y2 (snd arg.pointB)
        SVGAttr.Stroke arg.colour
        SVGAttr.StrokeWidth (string arg.thickness)
        ]
        []

let makeBox (x1,y1) (x2,y2) =
    rect
        [
        X x1
        Y y1
        SVGAttr.Width (x2-x1)
        SVGAttr.Height (y2-y1)
        SVGAttr.Stroke "black"
        SVGAttr.Fill "white"
        SVGAttr.StrokeWidth 0.1
        ]
        []

let makeSigLineArg pointA pointB =
    { dfltSigLine with pointA = pointA; pointB = pointB }

let makeSigLine pointA pointB =
    makeSigLineArg pointA pointB
    |> makeLine

//auxiliary functions to the viewer function

let needTransitionIntermediateLst (lstSignalValues: SigVals) =
    match lstSignalValues with
    | OneBitSig lst -> 
        List.zip lst.[0..lst.Length-2] lst.[1..lst.Length-1]
        |> List.map (fun (a,b) -> a <> b)
    | ManyBitSig lst -> 
        List.zip lst.[0..lst.Length-2] lst.[1..lst.Length-1]
        |> List.map (fun (a,b) -> a <> b)

let needTransitionAfter lstSignalValues =
    List.append (needTransitionIntermediateLst lstSignalValues) [false]

let needTransitionBefore lstSignalValues =
    List.append [true] (needTransitionIntermediateLst lstSignalValues)

let displayWaveform parameters (ind: int) wave  =
    let waveform = wave.wIn

    let nameLeft = float parameters.hPos
    let sigLeft = 0.0 //nameLeft + float parameters.hNameSize
    let spaceBetweenWaves = parameters.spacing
    let sigHeight = parameters.vSize
    let sigTop = float parameters.vPos + float ind * (spaceBetweenWaves + sigHeight) + spaceBetweenWaves
    let sigBot = sigHeight + sigTop
    let sigCentre = (sigTop+sigBot) / 2.0
    let clkLen = float parameters.hSize
    let sigThick = parameters.sigThick
    let clkThick = 0.025
    let transLen = 0.1
    

    let makeBitSegment (hasTransition, (value: Bit, position: int)) = //choose better names
        let fPos = float position
        let fVal = 
            match value with
            | One  -> sigHeight 
            | Zero -> 0.0
        let left = fPos * clkLen + sigLeft
        let right = left + clkLen

        let signalLine = makeSigLine (left, sigBot-fVal) (right, sigBot-fVal) 

        match hasTransition with
        | true ->
            [makeSigLine (right, sigBot+sigThick/2.0) (right, sigTop-sigThick/2.0)]
            |> List.append [signalLine]
        | false ->  
            [signalLine]


    let makeBusSegment ((transitionBefore: bool, transitionAfter: bool), (value: uint, position: int)) = //choose better names
        let fPos = float position
        
        let leftLim = sigLeft + fPos * clkLen
        let rightLim = leftLim + clkLen
        let hCentre = (leftLim+rightLim) / 2.0
        let left =
            if transitionBefore then leftLim + transLen else leftLim
        let right = 
            if transitionAfter then rightLim - transLen else rightLim

        
        let top = makeSigLine (left, sigTop) (right, sigTop)
        let bottom = makeSigLine (left, sigBot) (right, sigBot)
        let topLeft = makeSigLine (leftLim, sigCentre) (left, sigTop)
        let bottomLeft = makeSigLine (leftLim, sigCentre) (left, sigBot)
        let topRight = makeSigLine (rightLim, sigCentre) (right, sigTop)
        let bottomRight = makeSigLine (rightLim, sigCentre) (right, sigBot)

        let text =
            text 
                [
                X hCentre
                Y (sigBot - sigHeight*0.1)
                SVGAttr.Fill "black"
                SVGAttr.FontSize (0.8 * sigHeight)
                SVGAttr.TextAnchor "middle"
                ]
                [ str <| string value ]

        match transitionBefore, transitionAfter with
        | true, true -> 
            [topLeft; bottomLeft; topRight; bottomRight]
        | true, false -> 
            [topLeft; bottomLeft]
        | false, true -> 
            [topRight; bottomRight]
        | false, false -> 
            []
        |> List.append [text; top; bottom]
        //should there be the impossible case???

    let label =
        text 
            [
            X nameLeft
            Y sigCentre
            SVGAttr.Fill "black"
            SVGAttr.FontSize (0.25 + sigHeight * 0.3)
            SVGAttr.TextAnchor "start"
            ]
            [ str <| fst waveform ]
    
    let waveSvgElements =
        let waveValues = snd waveform
        match waveValues with
        | OneBitSig lst ->
            List.zip lst [0..lst.Length-1]
            |> List.zip (needTransitionAfter waveValues)
            |> List.collect makeBitSegment 
        | ManyBitSig lst ->
            let transitionsTupleLst =
                List.zip (needTransitionBefore waveValues) (needTransitionAfter waveValues)
            List.zip lst [0..lst.Length-1]
            |> List.zip transitionsTupleLst
            |> List.collect makeBusSegment
    label, waveSvgElements

let makeBackground (model : WaveSimModel) = 
    let p = model.viewParams

    let width = float p.hBoxSize
    let height = float p.vBoxSize
    let top = float p.vPos
    let bot = top + height
    let left = 0.0 //float parameters.hPos + float parameters.hNameSize
    let clkThickness = 0.025 //TODO: change to variable
    let clkLen = float p.hSize

    let maxWaveformLen =
        List.map (fun x -> signalLength x.wIn) model.waves
        |> List.max
        |> float
        |> (*) clkLen

    let clkLine x = 
        { dfltSigLine with 
            pointA = x, top
            pointB = x, bot
            colour = "gray"
            thickness = clkThickness
        }
        |> makeLine

    let clkLines = 
        [(int (left / clkLen) + 1)..1..(int (maxWaveformLen / clkLen) + 1)]
        |> List.map ((fun x -> float x * clkLen) >> clkLine)
    let simBox =  [makeBox (left,top) (8.0,bot)]

    clkLines, simBox
    
//view function of the waveform simulator

let viewWaveSim (model: DiagramModelType.Model) dispatch =
    let startWaveSim () = 
        dispatch <| StartWaveSim initModel
    let vZoom up () =
        let multBy = if up then 2.0 else 0.5
        match model.WaveSim with
        | Some m ->
            StartWaveSim { m with viewParams = { m.viewParams with vSize = m.viewParams.vSize*multBy;
                                                                    spacing = m.viewParams.spacing*multBy}}
            |> dispatch 
        | _ ->
            printf "What? vZoom function called when model.WaveSim is None"
            
    let hZoom up () =
        let multBy = if up then 2.0 else 0.5 
        match model.WaveSim with
        | Some m ->
            StartWaveSim { m with viewParams = { m.viewParams with hSize = m.viewParams.hSize*multBy}}  
            |> dispatch
        | _ ->
            printf "What? hZoom function called when model.WaveSim is None"
        
    match model.WaveSim with
    | None ->
        div [] [
            Button.button
                [ Button.Color IsSuccess; Button.OnClick (fun _ -> startWaveSim()) ]
                [ str "Start waveform simulator" ]
        ]
    | Some simModel ->
        let endWaveSim _ =
            dispatch CloseWaveSimNotification // Copied this, don't know if necessary + it's not doing anything now I think
            dispatch EndWaveSim // End simulation.
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endWaveSim ]
                [ str "Close waveform simulator" ]
            div [] [
                Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "The simulator uses the diagram at the time of pressing the button" ]
            ]
            Button.button
                [ Button.Color IsGrey; Button.OnClick (fun _ -> hZoom true ()) ]
                [ str "H Zoom +" ]
            Button.button
                [ Button.Color IsDanger; Button.OnClick (fun _ -> hZoom false ()) ]
                [ str "H Zoom -" ]
            Button.button
                [ Button.Color IsGrey; Button.OnClick (fun _ -> vZoom true ()) ]
                [ str "V Zoom +" ]
            Button.button
                [ Button.Color IsDanger; Button.OnClick (fun _ -> vZoom false ()) ]
                [ str "V Zoom -" ]
            hr []
            let displayWaveWithParams = displayWaveform simModel.viewParams 
            let svgBg = makeBackground simModel
            let svgElements = List.mapi displayWaveWithParams simModel.waves 
            let svgWaveforms = 
                svgElements
                |> List.collect snd
                |> List.append (fst svgBg)
            let svgLabels =
                svgElements
                |> List.map fst

            let appInv a b = 
                b + a
            let nSig = List.length simModel.waves
            let scaleX = simModel.viewParams.hSize * 8.0
            let scaleY = float nSig * (simModel.viewParams.vSize + simModel.viewParams.spacing)
            let labelVBparams = 
                "0 0 2 " + string scaleY
                |> string
            let wavesVBparams = 
                "0 0 " + string scaleX
                |> appInv " "
                |> appInv (string (scaleY + 0.5))
                |> string
            let boxVBparams = 
                "0 0 8 " + string (scaleY + 0.5)
            let widthWave = 
                100.0 * scaleX / 8.0
                |> int
                |> string
                |> appInv "%"
                |> string


            div 
                [ Style [Float FloatOptions.Left; Width "20%"] ]
                [
                svg 
                    [ViewBox labelVBparams; unbox ("width", "100%")]
                    svgLabels
                ]
            div 
                [                      
                Style [Float FloatOptions.Right; 
                        Width "80%";
                        Position PositionOptions.Relative]            
                ]
                [
                svg 
                    [ 
                    ViewBox boxVBparams; 
                    Style [Position PositionOptions.Absolute];
                    unbox ("width", "100%") 
                    unbox ("y", "0") 
                    ]
                    (snd svgBg)
                div 
                    [
                    Style [Width "100%"; 
                            OverflowX OverflowOptions.Scroll; 
                            Position PositionOptions.Absolute]
                    ]
                    [
                    svg 
                        [ 
                        ViewBox wavesVBparams; 
                        unbox ("width", widthWave) 
                        ]
                        svgWaveforms 
                    ]             
                ]
            ] 