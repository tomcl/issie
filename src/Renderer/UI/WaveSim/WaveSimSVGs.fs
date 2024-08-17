/// Functions to make SVGs of waveforms from FastSimulation data
module WaveSimSVGs

//---------------------------------------------------------------------------------------//
//-------------------------------Waveform SVG Generation---------------------------------//
//---------------------------------------------------------------------------------------//


open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open ModelHelpers
open WaveSimStyle
open WaveSimHelpers
open TopMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open WaveSimNavigation
open WaveSimSelect
open DiagramStyle


module Constants =
    /// <summary>Config variable to choose whether to generate the full 1000 cycles of SVG.</summary>
    let generateVisibleOnly = true
    /// <summary>Config variable to choose whether to print performance analysis info to console.</summary>
    let showPerfLogs = false
    let inlineNoWrap = WhiteSpace WhiteSpaceOptions.Nowrap

open Constants

/// Get all simulatable waves from CanvasState. Includes top-level Input and Output ports.
/// Waves contain info which will be used later to create the SVGs for those waves actually
/// selected. Init value of these from this function is None.
let getWaves (ws: WaveSimModel) (fs: FastSimulation) : Map<WaveIndexT, Wave> =
    let start = TimeHelpers.getTimeMs ()
    //printfn $"{fs.WaveIndex.Length} possible waves"
    fs.WaveIndex
    |> TimeHelpers.instrumentInterval "getAllPorts" start
    |> Array.map (fun wi -> wi, makeWave ws fs wi)
    //|> fun x -> printfn $"Made waves!";x
    |> Map.ofArray
    |> TimeHelpers.instrumentInterval "makeWavePipeline" start





/// <summary>Generates SVG to display non-binary values on waveforms.</summary>
/// <remarks>Should be refactored together with <c>displayBigIntOnWave</c>.</remarks>
let displayUInt32OnWave 
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<uint32>)
    (transitions: array<NonBinaryTransition>)
    : list<ReactElement> =
    // find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> Array.indexed
        |> Array.filter (fun (_, x) -> x = Change)
        |> Array.map (fun (i, _) -> i)

    // find start and length of each gap between a Change transition
    let gaps: array<Gap> =
        if Constants.generateVisibleOnly
        then
            // add dummy change at visible end, but need account for difference in changes:
            // e.g. if we are showing 3 cycles, a wave with a change in each would be 0, 1, 2, 3 and would be fine when
            // 4 is added; however, a wave with no change at all would be 0, and would produce an errorneous gap length
            // of 4 when 4 is added - we therefore add 3
            if changeTransitions[Array.length changeTransitions-1] <> wsModel.ShownCycles
            then Array.append changeTransitions [|wsModel.ShownCycles|]
            else Array.append changeTransitions [|wsModel.ShownCycles+1|]
            |> Array.map (fun loc -> loc+wsModel.StartCycle) // shift cycle to start cycle
        else
            Array.append changeTransitions [|wsModel.StartCycle+transitions.Length-1|] // add dummry change length end
        |> Array.pairwise
        |> Array.map (fun (i1, i2) -> {Start = i1; Length = i2-i1}) // get start and length of gap
    
    // utility functions for SVG generation
    /// <summary>Function to make polygon fill for a gap.</summary>
    /// <param name="points">Array of polyline points to fill.</param>
    let makePolyfill (points: array<XYPos>) = 
        let points = points |> Array.distinct
        polyline (wavePolyfillStyle points) []

    /// <summary>Function to make text element for a gap.</summary>
    /// <param name="start">Starting X location of element.</param>
    let makeTextElement (start: float) (waveValue: string) = 
        text (singleValueOnWaveProps start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        // generate string
        let waveValue = UInt32ToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gap.Start]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints cycleWidth gap
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (singleWidth <= w && w < doubleWidth) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement (float gap.Start * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w when (doubleWidth <= w) -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                if cycleWidth < doubleWidth
                then (cycleWidth - singleWidth) / 2.
                else Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                if singleCycleCenterPadWidth < 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    then 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    else singleCycleCenterPadWidth
            let endPadWidth = (float gap.Length * cycleWidth - startPadWidth - singleWidth)
            let startText = makeTextElement (float gap.Start * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement (float gap.Start * cycleWidth + endPadWidth) waveValue
            [ startText; endText ] 
        | _ -> // catch-all
            failwithf "displayUInt32OnWave: impossible case"
    )
    |> List.concat

/// <summary>Generates SVG to display <c>bigint</c> values on waveforms.</summary>
/// <remarks>Should be refactored together with <c>displayUInt32OnWave</c>.</remarks>
let displayBigIntOnWave
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<bigint>)
    (transitions: array<NonBinaryTransition>)
    : list<ReactElement> =
    // find all clock cycles where there is a NonBinaryTransition.Change
    let changeTransitions =
        transitions
        |> Array.indexed
        |> Array.filter (fun (_, x) -> x = Change)
        |> Array.map (fun (i, _) -> i)

    // find start and length of each gap between a Change transition
    let gaps: array<Gap> =
        if Constants.generateVisibleOnly
        then
            // add dummy change at visible end, but need account for difference in changes:
            // e.g. if we are showing 3 cycles, a wave with a change in each would be 0, 1, 2, 3 and would be fine when
            // 4 is added; however, a wave with no change at all would be 0, and would produce an errorneous gap length
            // of 4 when 4 is added - we therefore add 3
            if changeTransitions[Array.length changeTransitions-1] <> wsModel.ShownCycles
            then Array.append changeTransitions [|wsModel.ShownCycles|]
            else Array.append changeTransitions [|wsModel.ShownCycles+1|]
            |> Array.map (fun loc -> loc+wsModel.StartCycle) // shift cycle to start cycle
        else
            Array.append changeTransitions [|wsModel.StartCycle+transitions.Length-1|] // add dummry change length end
        |> Array.pairwise
        |> Array.map (fun (i1, i2) -> {Start = i1; Length = i2-i1}) // get start and length of gap
    
    // utility functions for SVG generation
    /// <summary>Function to make polygon fill for a gap.</summary>
    /// <param name="points">Array of polyline points to fill.</param>
    let makePolyfill (points: array<XYPos>) = 
        let points = points |> Array.distinct
        polyline (wavePolyfillStyle points) []

    /// <summary>Function to make text element for a gap.</summary>
    /// <param name="start">Starting X location of element.</param>
    let makeTextElement (start: float) (waveValue: string) = 
        text (singleValueOnWaveProps start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        // generate string
        let waveValue = BigIntToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gap.Start]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints cycleWidth gap
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (singleWidth <= w && w < doubleWidth) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement (float gap.Start * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w when (doubleWidth <= w) -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                if cycleWidth < doubleWidth
                then (cycleWidth - singleWidth) / 2.
                else Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                if singleCycleCenterPadWidth < 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    then 0.1 * DrawHelpers.getTextWidthInPixels Constants.valueOnWaveText waveValue 
                    else singleCycleCenterPadWidth
            let endPadWidth = (float gap.Length * cycleWidth - startPadWidth - singleWidth)
            let startText = makeTextElement (float gap.Start * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement (float gap.Start * cycleWidth + endPadWidth) waveValue
            [ startText; endText ] 
        | _ -> // catch-all
            failwithf "displayUInt32OnWave: impossible case"
    )
    |> List.concat



/// <summary>Check if generated SVG is correct, based on existence, position, 
/// and zoom. Fast simulation data is assumed unchanged. Used to determine if 
/// <c>generateWaveform</c> is run.</summary>
let waveformIsUptodate (ws: WaveSimModel) (wave: Wave): bool =
    wave.SVG <> None &&
    wave.ShownCycles = ws.ShownCycles &&
    wave.StartCycle = ws.StartCycle &&
    wave.CycleWidth = singleWaveWidth ws &&
    wave.Radix = ws.Radix &&
    wave.Multiplier = ws.CycleMultiplier

/// <summary>Called when <c>InitiateWaveSimulation</c> message is dispatched and when wave
/// simulator is refreshed. Generates or updates the SVG for a specific waveform
/// whether needed or not. The SVG depends on cycle width as well as start/stop
/// clocks and design. Assumes that the fast simulation data has not changed and
/// has enough cycles.</summary>
let generateWaveform (ws: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    let makePolyline points = 
        let points = points |> Array.concat |> Array.distinct
        polyline (wavePolylineStyle points) []
    
    let waveform =
        match wave.Width with
        | 0 -> 
            failwithf "Cannot have wave of width 0"

        | 1 -> // binary waveform
            let transitions = calculateBinaryTransitionsUInt32 wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles ws.CycleMultiplier
            
            let wavePoints =
                let waveWidth = singleWaveWidth ws
                let startCycle = if Constants.generateVisibleOnly then ws.StartCycle else 0
                Array.mapi (binaryWavePoints waveWidth startCycle) transitions
                |> Array.concat
                |> Array.distinct

            svg (waveRowProps ws) [ polyline (wavePolylineStyle wavePoints) [] ]

        | w when w <= 32 -> // non-binary waveform
            let transitions = calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles ws.CycleMultiplier
            let fstPoints, sndPoints =
                let waveWidth = singleWaveWidth ws
                let startCycle = if Constants.generateVisibleOnly then ws.StartCycle else 0
                Array.mapi (nonBinaryWavePoints waveWidth startCycle) transitions |> Array.unzip
            
            let valuesSVG = displayUInt32OnWave ws wave.Width wave.WaveValues.UInt32Step transitions
            let polyLines = [makePolyline fstPoints; makePolyline sndPoints]

            svg (waveRowProps ws) (List.append polyLines valuesSVG)

        | _ -> // non-binary waveform with width greather than 32
            let transitions = calculateNonBinaryTransitions wave.WaveValues.UInt32Step ws.StartCycle ws.ShownCycles ws.CycleMultiplier

            let fstPoints, sndPoints =
                Array.mapi (nonBinaryWavePoints (singleWaveWidth ws) 0) transitions |> Array.unzip

            let valuesSVG = displayBigIntOnWave ws wave.Width wave.WaveValues.BigIntStep transitions

            svg (waveRowProps ws) (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)
    {wave with 
        Radix = ws.Radix
        ShownCycles = ws.ShownCycles
        StartCycle = ws.StartCycle
        Multiplier = ws.CycleMultiplier
        CycleWidth = singleWaveWidth ws
        SVG = Some waveform}


/// <summary>This function regenerates all the waveforms listed on <c> wavesToBeMade </c>. 
/// Generation is subject to timeout, so may not complete.</summary>
/// <remarks>This function has been augmented with performance monitoring function, turn <c>Constants.showPerfLogs</c>
/// to print performance information to console.</remarks>
/// <returns>An anonymous record with the following information:<br/>
/// a) <c>WSM</c> (WaveSimModel with updated waveforms),<br/>
/// b) <c>NumberDone</c> (no of waveforms made), and <br/>
/// c) <c>TimeTaken</c> (<c>Some timeTaken</c> when greater than <c>timeOut</c> or <c>None</c>
/// if completed with no time out).</returns>
let makeWaveformsWithTimeOut
    (timeOut: option<float>)
    (ws: WaveSimModel)
    (wavesToBeMade: list<WaveIndexT>)
        : {| WSM: WaveSimModel ; NumberDone: int ; TimeTaken: option<float> |}=

    let start = TimeHelpers.getTimeMs()
    let allWaves, numberDone, timeTaken =
        ((ws.AllWaves, 0, None), wavesToBeMade)
        ||> List.fold (fun (all,n, _) wi ->
                match timeOut, TimeHelpers.getTimeMs() - start with
                | Some timeOut, timeSoFar when timeOut < timeSoFar ->
                    all, n, Some timeSoFar
                | _ ->
                    (Map.change wi (Option.map (generateWaveform ws wi)) all), n+1, None)
    printfn $"Making {numberDone} waves."
    let finish = TimeHelpers.getTimeMs()
    if Constants.showPerfLogs then
        let countWavesWithWidthRange lowerLim upperLim =
            wavesToBeMade
            |> List.map (fun wi -> (Map.find wi allWaves).Width)
            |> List.filter (fun width -> lowerLim <= width && width <= upperLim)
            |> List.length
        
        printfn "PERF:makeWaveformsWithTimeOut: generating visible only: %b" Constants.generateVisibleOnly
        printfn "PERF:makeWaveformsWithTimeOut: making %d/%d waveforms" (List.length wavesToBeMade) (Map.count allWaves)
        printfn "PERF:makeWaveformsWithTimeOut: binary = %d" (countWavesWithWidthRange 1 1)
        printfn "PERF:makeWaveformsWithTimeOut: int32 = %d" (countWavesWithWidthRange 2 32)
        printfn "PERF:makeWaveformsWithTimeOut: process took %.2fms" (finish-start)

    {| WSM={ws with AllWaves = allWaves}; NumberDone=numberDone; TimeTaken = timeTaken|}








