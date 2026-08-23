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
open WaveSimTypes
open WaveSimStyle
open WaveSimHelpers
open SimGraphTypes
open SimTypes
open NumberHelpers
open WaveSimSelect
open DiagramStyle
open EvilHoverCache
open WaveSlice


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
/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
/// TODO: Remove this since it is unnecessary. Can use WaveValues instead.

/// Determines whether a non-binary waveform changes value at the beginning of that clock cycle.

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition





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


//------------------------------------------------------------------------------------------------------//
//-----------------------------Generate SVGs for Waveform Display---------------------------------------//
//------------------------------------------------------------------------------------------------------//


/// <summary>Generates SVG to display non-binary values on waveforms.</summary>
/// This function has side effect of recording hatched gaps in <c>gapCache</c>.
/// <remarks>Should be refactored together with <c>displayBigIntOnWave</c>.</remarks>
let displayUInt32OnWave 
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<uint32>)
    (transitions: array<NonBinaryTransition>)
    (gapCache: GapStore)
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
            EvilHoverCache.addGapToStore gapCache gap
            [ fill ]
        | w when (w < doubleWidth * 1.1) -> // display 1 copy at centre
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
/// This function has side effect of recording hatched gaps in <c>gapCache</c>.
/// <remarks>Should be refactored together with <c>displayUInt32OnWave</c>.</remarks>
let displayBigIntOnWave
    (wsModel: WaveSimModel)
    (width: int) 
    (waveValues: array<bigint>)
    (transitions: array<NonBinaryTransition>)
    (gapCache: GapStore)
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
        // waveValues is sampled from StartCycle, so it is indexed by gapCycle and not by the
        // absolute cycle gap.Start
        let waveValue = BigIntToPaddedString Constants.waveLegendMaxChars wsModel.Radix width waveValues[gapCycle]
        
        // calculate display widths
        let cycleWidth = singleWaveWidth wsModel
        let gapWidth = (float gap.Length * cycleWidth) - 2. * Constants.nonBinaryTransLen
        let singleWidth = 1. * DrawHelpers.getTextWidthInPixels textSpec waveValue
        let doubleWidth = 2. * singleWidth + Constants.valueOnWavePadding
        
        match gapWidth with
        | w when (w < singleWidth * 1.05) -> // display filled polygon
            EvilHoverCache.addGapToStore gapCache gap
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
            let endText = makeTextElement false (float (gapCycle + gap.Length) * cycleWidth - startPadWidth) waveValue
            [ startText; endText ] 

    )
    |> List.concat



/// Draw one waveform, or None where the data for that view is not here.
///
/// A pure function of the data and the spec - what it draws is decided entirely by its arguments,
/// and it decides nothing. Whether to call it, and what to do when it answers None, is `drawnFor`
/// below.
let private makeWaveform (ws: WaveSimModel) (wave: Wave) (spec: WaveDrawn.WaveSpec) =
    let makePolyline points =
        let points = points |> Array.concat |> Array.distinct
        polyline (wavePolylineStyle points) []

    match WaveData.slice (SignalHandle wave.DriverIndex) spec.Window with
    | None ->
        // the simulation has not reached these cycles, or the window this wave holds is not the one
        // being asked for - which where the sidecar simulates is every view until its fetch lands
        None
    | Some sliceOfWave ->

    let waveform, (gaps: GapStore) =
        match sliceOfWave, wave.Width with
        | _, 0 ->
            failwithf "Cannot have wave of width 0"


        | slice, 1 -> // binary waveform
            let transitions = WaveSlice.binaryTransitions slice
            let wavePoints =
                let waveWidth = singleWaveWidth ws
                Array.mapi (binaryWavePoints waveWidth ws.StartCycle) transitions
                |> Array.concat
                |> Array.distinct

            svg (waveRowProps ws) [ polyline (wavePolylineStyle wavePoints) [] ], initGapStore 0

        | slice, w when w <= 32 -> // non-binary waveform
            let transitions, waveValues = WaveSlice.nonBinaryTransitionsWords slice
            let fstPoints, sndPoints =
                let waveWidth = singleWaveWidth ws
                Array.mapi (nonBinaryWavePoints waveWidth 0) transitions |> Array.unzip
            let gapStore = EvilHoverCache.initGapStore (ws.ShownCycles/ 2 + 1)
            let valuesSVG = displayUInt32OnWave ws wave.Width waveValues transitions gapStore
            EvilHoverCache.finaliseStore gapStore
            let polyLines = [makePolyline fstPoints; makePolyline sndPoints]

            svg (waveRowProps ws) (List.append polyLines valuesSVG), gapStore

        | slice, _ -> // non-binary waveform with width greather than 32
            let transitions, sampledWaveValues = WaveSlice.nonBinaryTransitionsBigs slice

            let fstPoints, sndPoints =
                Array.mapi (nonBinaryWavePoints (singleWaveWidth ws) 0) transitions |> Array.unzip
            let gapStore = EvilHoverCache.initGapStore (ws.ShownCycles/ 2 + 1)
            let valuesSVG = displayBigIntOnWave ws wave.Width sampledWaveValues transitions gapStore
            EvilHoverCache.finaliseStore gapStore


            svg (waveRowProps ws) (List.append [makePolyline fstPoints; makePolyline sndPoints] valuesSVG), gapStore

    Some
        { WaveDrawn.Spec = spec
          WaveDrawn.Svg = waveform
          WaveDrawn.Gaps = gaps
          WaveDrawn.Samples = sliceOfWave }

/// The waveform to put on screen for one wave: the view the controls ask for where its data is
/// here, and otherwise the one it is already showing.
///
/// Called from the view, for every wave, on every render. Three outcomes, in the order they are
/// tried: the waveform asked for has already been made, so it is reused; it has not, and the data
/// is here, so it is made and kept; the data is not here, so what is on screen stays there. Only
/// the last is a judgement, and it is the one the user asked for - a viewer that empties itself
/// while it waits looks broken, one that shows the last view for a moment does not.
///
/// None only before anything has ever been drawn for this wave, which is the moment a wave is added
/// to the selection. An empty row is right there: there is nothing older to show.
let drawnFor (ws: WaveSimModel) (wave: Wave) : WaveDrawn.Drawn option =
    let spec = WaveDrawn.specOf ws wave

    let draw (ws: WaveSimModel) =
        let spec = WaveDrawn.specOf ws wave

        makeWaveform ws wave spec
        |> Option.map (fun drawn ->
            WaveDrawn.put drawn
            drawn)

    match WaveDrawn.tryDrawn wave.DriverIndex with
    | Some drawn when drawn.Spec = spec -> Some drawn
    | onScreen ->
        match draw ws with
        | Some drawn -> Some drawn
        | None ->
            // The data for the view asked for is not here. Draw whatever data IS here, which is
            // what the last fetch to land carried - a window somewhere between what is on screen
            // and what the controls now say.
            //
            // Keeping what is on screen instead is what a single missing fetch calls for, and it is
            // what this used to do always. Under a fast scroll it is wrong: fetch after fetch lands
            // and none of them is ever the exact view being asked for by the time it arrives, so
            // the waveforms freeze on the last window that happened to be current when its data
            // came - while newer data for the design goes into the cache and is never looked at.
            // Drawing what arrived keeps the picture moving, a window or so behind the numbers
            // above it, which is what a viewer over a wire should look like.
            match WaveData.heldWindow (SignalHandle wave.DriverIndex) with
            | Some held when onScreen |> Option.forall (fun d -> d.Spec.Window <> held) ->
                draw
                    { ws with
                        StartCycle = held.StartSample
                        ShownCycles = held.SampleCount
                        SamplingZoom = held.Multiplier }
                |> Option.orElse onScreen
            | _ -> onScreen

/// The value the cursor is on, read from the waveform that is DRAWN beside it.
///
/// Not from the cache and not by clock cycle. The cursor is a column of the picture, so the value
/// next to it is the sample that column was drawn from - which while the picture is a window behind
/// the controls is not the cycle the numbers above it say. Reading by cycle instead answered
/// nothing at all there, and a column of "?" beside perfectly good waveforms is a worse lie than a
/// value that matches what is on screen.
///
/// None where nothing is drawn for this wave, or the cursor is off the end of what is: an empty row
/// has no value, and neither has a column that is not there.
///
/// Goes through `drawnFor` rather than reading the memo, so that it does not matter whether the
/// value column or the waveform column is built first: whichever gets there does the drawing and
/// the other finds it done. Reading the memo instead was a render behind on any render that
/// changed the picture.
let getWaveValue (ws: WaveSimModel) (wave: Wave) : FastData option =
    drawnFor ws wave
    |> Option.bind (fun drawn -> WaveDrawn.valueAtSample drawn (ws.CursorDisplayCycle - ws.StartCycle))
