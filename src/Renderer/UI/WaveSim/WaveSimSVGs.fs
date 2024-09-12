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
open SimGraphTypes
open SimTypes
open NumberHelpers
open WaveSimSelect
open DiagramStyle


module Constants =
    /// <summary>Config variable to choose whether to generate the full 1000 cycles of SVG.</summary>
    let generateVisibleOnly = true
    /// <summary>Config variable to choose whether to print performance analysis info to console.</summary>
    let showPerfLogs = false
    let inlineNoWrap = WhiteSpace WhiteSpaceOptions.Nowrap



//------------------------------------------------------------------------------------------------------//
//---------------------------Calculate Waveform Transitions from data arrays----------------------------//
//------------------------------------------------------------------------------------------------------//

/// create a new array which samples the old one every mult cycles.
/// start: index of first cycle.
/// count: number of samples.
let subSamp (arr: 'T array) (start:int) (count: int) (mult:int)  =
    Array.init count (fun n -> arr[start*mult + n*mult])

/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
/// TODO: Remove this since it is unnecessary. Can use WaveValues instead.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne

/// Determines whether a non-binary waveform changes value at the beginning of that clock cycle.
type NonBinaryTransition =
    | Change
    | Const

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

/// Stores information about gaps between NonBinaryTransitions.
/// Used in displayValuesOnWave
type Gap = {
    // First cycle which is Change after a Const cycle
    Start: int
    // How many Const cycles there are immediately after this Change transition
    Length: int
}




/// If true, then show cross-hatch only for non-binary waves when wave is changing value very fast.
let highZoom clkCycleWidth = clkCycleWidth < 2. * Constants.nonBinaryTransLen

/// Left-shift non-binary waveforms by this much.
let xShift clkCycleWidth =
    if highZoom clkCycleWidth then
        clkCycleWidth / 2.
    else Constants.nonBinaryTransLen
        


/// Retrieve value of wave at given clock cycle as an int.
/// At extra (sampling) zoom this allows detail clock cycles within one sample
/// therefore clkCycleDetail IS NOT scaled the same as the sample numbers used
/// everywhere else.
let getWaveValue (clkCycleDetail: int) (wave: Wave) (width: int) : FastData =
    let waveData =
        match Array.tryItem wave.DriverIndex Simulator.simCacheWS.FastSim.Drivers with
        | Some (Some d) -> d
        | Some None ->
            failwithf $"No driver found for wave {wave.DisplayName}"
        | None ->
            printfn $"Error: index {wave.DriverIndex} not found in fast simulation array"
            failwithf $"Wave {wave.DisplayName} not found in fast simulation"

    match width with
    | w when w > 32 ->
        Array.tryItem clkCycleDetail waveData.DriverData.BigIntStep
        |> function
            | Some (fData) -> 
                { Dat = BigWord fData; Width = width}            
            | _ ->
                // TODO: Find better default value here
                // TODO: Should probably make it so that you can't call this function in the first place.
                printf "Trying to access index %A in wave %A. Default to 0." clkCycleDetail wave.DisplayName
                {Dat = Word 0u; Width = width}
    | _ ->      
        Array.tryItem clkCycleDetail waveData.DriverData.UInt32Step
        |> function
            | Some (fData) -> 
                { Dat = Word fData; Width = width}
            | _ ->
                printf "Trying to access index %A in wave %A. Default to 0." clkCycleDetail wave.DisplayName
                {Dat = Word 0u; Width = width}

/// Make left and right x-coordinates for a clock cycle.
let makeXCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) =
    match transition with
    | BinaryTransition _ ->
        float clkCycle * clkCycleWidth, float (clkCycle + 1) * clkCycleWidth
    | NonBinaryTransition _ ->
        // These are left-shifted by xShift: doing this means that for non-binary
        // waveforms, only the transition at the start of each cycle needs to be considered,
        // rather than the transition at both the start and end of each cycle.
        float clkCycle * clkCycleWidth - xShift clkCycleWidth,
        float (clkCycle + 1) * clkCycleWidth - xShift clkCycleWidth

/// Make top-left, top-right, bottom-left, bottom-right coordinates for a clock cycle.
let makeCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) : XYPos * XYPos * XYPos * XYPos =
    let xLeft, xRight = makeXCoords clkCycleWidth clkCycle transition

    let topL = {X = xLeft; Y = Constants.yTop}
    let topR = {X = xRight; Y = Constants.yTop}
    let botL = {X = xLeft; Y = Constants.yBot}
    let botR = {X = xRight; Y = Constants.yBot}

    topL, topR, botL, botR

/// Generate points for a binary waveform
let binaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: BinaryTransition)  : XYPos array =
    let topL, topR, botL, botR = makeCoords clkCycleWidth index (BinaryTransition transition)
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero | OneToZero ->
        [|botL; botR|]
    | ZeroToOne | OneToOne ->
        [|topL; topR|]

/// <summary>Generate polyline points for a non-binary waveform via transition info.</summary>
let nonBinaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: NonBinaryTransition)
    : array<XYPos>*array<XYPos> =
    let xLeft, _ = makeXCoords clkCycleWidth index (NonBinaryTransition transition)
    let _, topR, _, botR = makeCoords clkCycleWidth index (NonBinaryTransition transition)

    let crossHatchMid, crossHatchTop, crossHatchBot =
        {X = xLeft +      xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yTop},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yBot}

    match transition with
    | Change ->
        if highZoom clkCycleWidth then
            [|crossHatchMid; crossHatchTop|], [|crossHatchMid; crossHatchBot|]
        else
            [|crossHatchMid; crossHatchTop; topR|], [|crossHatchMid; crossHatchBot; botR|]
    | Const ->
        [|topR|], [|botR|]

/// <summary>Generate polyfill points for a non-binary gap via gap info.</summary>
let nonBinaryFillPoints (startCycle: int) (clkCycleWidth: float) (gap: Gap): array<XYPos> =
    let start = gap.Start - startCycle
    let xLeft, _ = makeXCoords clkCycleWidth start (NonBinaryTransition Change)
    let _, xRight = makeXCoords clkCycleWidth (start + gap.Length-1) (NonBinaryTransition Change)

    let crossHatchMidL, crossHatchTopL, crossHatchBotL =
        {X = xLeft + xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xLeft + 2.0 * xShift clkCycleWidth; Y = Constants.yTop},
        {X = xLeft + 2.0 * xShift clkCycleWidth; Y = Constants.yBot}
    
    let crossHatchMidR, crossHatchTopR, crossHatchBotR =
        {X = xRight + xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xRight; Y = Constants.yTop},
        {X = xRight; Y = Constants.yBot}

    [| crossHatchMidL; crossHatchTopL; crossHatchTopR; crossHatchMidR; crossHatchBotR; crossHatchBotL; crossHatchMidL |]


/// <summary>Find transitions for each clock cycle of a binary waveform.</summary>
let calculateBinaryTransitionsUInt32 (waveValues: array<uint32>) (startCycle: int) (shownCycles: int) (multiplier: int)
    : array<BinaryTransition> =
    let getBit bit = int32 bit
    match startCycle, startCycle + shownCycles - 1 with
    | startCyc, endCyc when startCyc = 0 && startCyc <= endCyc && endCyc*multiplier < Array.length waveValues ->
        // in this case, we need to add a 0 value to the start of the array
        // we start on the first sample, end one after the last sample.
        subSamp waveValues startCyc (endCyc-startCyc+1) multiplier
        |> Array.append [| waveValues[0] |]
    | startCyc, endCyc when 0 < startCyc && startCyc <= endCyc && endCyc*multiplier < Array.length waveValues ->
        // in this case we can start one before the first data sample, end one after the last sample, to work out the first transition
        subSamp waveValues (startCyc-1) (endCyc-startCyc+2) multiplier
    | _ ->
        printfn $"Before Bin failure: waveValues.Length {waveValues.Length} \
                e*m: {(startCycle+shownCycles-1)*multiplier}  start {startCycle} shown {shownCycles} mult: {multiplier}"
        failwithf $"Shown cycles is beyond array bounds: startCyc={startCycle}, shown={shownCycles}, mult={multiplier}"
    |> Array.pairwise
    |> Array.map (fun (x, y) ->
        match getBit x, getBit y with
        | 0, 0 -> ZeroToZero
        | 0, 1 -> ZeroToOne
        | 1, 0 -> OneToZero
        | 1, 1 -> OneToOne
        | _ -> failwithf $"Unrecognised transition {getBit x}, {getBit y}")


/// <summary>Find transitions for each clock cycle of a non-binary waveform.</summary>
let calculateNonBinaryTransitions (waveValues: array<'a>) (startCycle: int) (shownCycles: int) (multiplier: int)
    : array<NonBinaryTransition> * array<'a>=
    let sampledWaveValues =
        match startCycle, startCycle + shownCycles - 1 with
        | startCyc, endCyc when (0 <= startCyc && startCyc <= endCyc && endCyc*multiplier < Array.length waveValues) ->
            subSamp waveValues (startCyc) (endCyc-startCyc+1) multiplier 
        | _ ->
            printfn $"Before NonBinaryTransitions failure: waveValues.Length {waveValues.Length} \
                    e*m: {(startCycle+shownCycles-1)*multiplier}  start {startCycle} shown {shownCycles} mult: {multiplier}"
            failwithf $"Shown cycles is beyond array bounds: start={startCycle} shown={shownCycles} mult={multiplier} length = {waveValues.Length}"
    sampledWaveValues
    |> Array.pairwise
    |> Array.map (fun (x, y) -> if x = y then Const else Change)
    |> Array.append [| Change |]
    |> (fun trans -> trans, sampledWaveValues)


//------------------------------------------------------------------------------------------------------//
//-----------------------------Generate SVGs for Waveform Display---------------------------------------//
//------------------------------------------------------------------------------------------------------//


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
    let textFont = wsModel.WSConfig.FontSize
    let textWeight = wsModel.WSConfig.FontWeight
    let textSpec = {DrawHelpers.defaultText with FontSize = $"{textFont}px"; FontWeight = $"{textWeight}"; FontFamily = "Helvetica"}

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
    let makeTextElement (isStart) (start: float) (waveValue: string) =
        text (singleValueOnWaveProps isStart textFont textWeight start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        let gapCycle = gap.Start - wsModel.StartCycle
        // generate string
        let waveValue = UInt32ToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gapCycle]
        
        // calculate display widths
        let cycleWidth = 1.0 * singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1. * DrawHelpers.getTextWidthInPixels textSpec waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth * 1.05) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints wsModel.StartCycle cycleWidth gap 
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (w < doubleWidth * 1.1) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement true (float gapCycle * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w  -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                (*if cycleWidth < doubleWidth
                then (cycleWidth - singleWidth) / 2.
                else*) Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                (*if singleCycleCenterPadWidth < 0.1 * DrawHelpers.getTextWidthInPixels textSpec waveValue 
                    then 0.1 * DrawHelpers.getTextWidthInPixels textSpec waveValue 
                    else*) singleCycleCenterPadWidth
            let endPadWidth = (float gap.Length * cycleWidth - startPadWidth - singleWidth)
            let startText = makeTextElement true (float gapCycle * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement false (float (gapCycle + gap.Length) * cycleWidth - startPadWidth) waveValue
            [ startText; endText ] 

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
    let textFont = wsModel.WSConfig.FontSize
    let textWeight = wsModel.WSConfig.FontWeight
    let textSpec = {
        DrawHelpers.defaultText with
            FontSize = $"{textFont}px";
            FontWeight = $"{textWeight}";
            FontFamily = Constants.valueColumnFontFamily
            }
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
    let makeTextElement (isStart: bool) (start: float) (waveValue: string) = 
        text (singleValueOnWaveProps isStart textFont textWeight start) [ str waveValue ]
    
    // create text element for every gap
    gaps
    |> Array.map (fun gap ->
        let gapCycle = gap.Start - wsModel.StartCycle
        // generate string
        let waveValue = BigIntToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gap.Start]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1. * DrawHelpers.getTextWidthInPixels textSpec waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth * 1.05) -> // display filled polygon
            let fillPoints = nonBinaryFillPoints wsModel.StartCycle cycleWidth gap
            let fill = makePolyfill fillPoints
            [ fill ]
        | w when (w < doubleWidth*3.) -> // diplay 1 copy at centre
            let gapCenterPadWidth = (float gap.Length * cycleWidth - singleWidth) / 2.
            let singleText = makeTextElement true (float gapCycle * cycleWidth + gapCenterPadWidth) waveValue
            [ singleText ] 
        | w -> // display 2 copies at end of gaps
            let singleCycleCenterPadWidth = // if a single cycle gap can include 2 copies, set arbitrary padding
                Constants.valueOnWaveEdgePadding
            let startPadWidth = 
                    singleCycleCenterPadWidth
            let startText = makeTextElement true (float gapCycle * cycleWidth + startPadWidth) waveValue
            let endText = makeTextElement false (float (gapCycle + 1) * cycleWidth - startPadWidth) waveValue
            [ startText; endText ] 

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
    wave.Multiplier = ws.SamplingZoom

/// <summary>Called when <c>InitiateWaveSimulation</c> message is dispatched and when wave
/// simulator is refreshed. Generates or updates the SVG for a specific waveform
/// whether needed or not. The SVG depends on cycle width as well as start/stop
/// clocks and design. Assumes that the fast simulation data has not changed and
/// has enough cycles.</summary>
let generateWaveform (ws: WaveSimModel) (index: WaveIndexT) (wave: Wave): Wave =
    let makePolyline points = 
        let points = points |> Array.concat |> Array.distinct
        polyline (wavePolylineStyle points) []
    let waveData =
        match Simulator.simCacheWS.FastSim.Drivers[wave.DriverIndex] with
        | Some d  -> d
        | None -> failwith $"No driver fround for {wave.DisplayName}"
    let waveform =
        match wave.Width with
        | 0 -> 
            failwithf "Cannot have wave of width 0"

        | 1 -> // binary waveform
            
            let transitions = calculateBinaryTransitionsUInt32 waveData.DriverData.UInt32Step ws.StartCycle ws.ShownCycles ws.SamplingZoom
            let wavePoints =
                let waveWidth = singleWaveWidth ws
                Array.mapi (binaryWavePoints waveWidth ws.StartCycle) transitions
                |> Array.concat
                |> Array.distinct

            svg (waveRowProps ws) [ polyline (wavePolylineStyle wavePoints) [] ]

        | w when w <= 32 -> // non-binary waveform
            let transitions,waveValues = calculateNonBinaryTransitions waveData.DriverData.UInt32Step ws.StartCycle ws.ShownCycles ws.SamplingZoom
            let fstPoints, sndPoints =
                let waveWidth = singleWaveWidth ws
                let startCycle = if Constants.generateVisibleOnly then ws.StartCycle else 0
                Array.mapi (nonBinaryWavePoints waveWidth 0) transitions |> Array.unzip
            
            let valuesSVG = displayUInt32OnWave ws wave.Width waveValues transitions
            let polyLines = [makePolyline fstPoints; makePolyline sndPoints]

            svg (waveRowProps ws) (List.append polyLines valuesSVG)

        | _ -> // non-binary waveform with width greather than 32
            let transitions, sampledWaveValues = calculateNonBinaryTransitions waveData.DriverData.BigIntStep ws.StartCycle ws.ShownCycles ws.SamplingZoom

            let fstPoints, sndPoints =
                Array.mapi (nonBinaryWavePoints (singleWaveWidth ws) 0) transitions |> Array.unzip

            let valuesSVG = displayBigIntOnWave ws wave.Width sampledWaveValues transitions

            svg (waveRowProps ws) (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG)
    {wave with 
        Radix = ws.Radix
        ShownCycles = ws.ShownCycles
        StartCycle = ws.StartCycle
        Multiplier = ws.SamplingZoom
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
    try
        let start = TimeHelpers.getTimeMs()
        let allWaves, numberDone, timeTaken =
            ((ws.AllWaves, 0, None), wavesToBeMade)
            ||> List.fold (fun (all,n, _) wi ->
                    match timeOut, TimeHelpers.getTimeMs() - start with
                    | Some timeOut, timeSoFar when timeOut < timeSoFar ->
                        all, n, Some timeSoFar
                    | _ ->
                        (Map.change wi (Option.map (generateWaveform ws wi)) all), n+1, None)
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
    with
        | ex -> 
            printfn $"Error in makeWaveformsWithTimeOut: {ex.Message}"
            {| WSM=ws; NumberDone=0; TimeTaken= None |}








