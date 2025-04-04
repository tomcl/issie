module EvilHoverCache

//---------------------------------------------------------------------------------------//
//----------Mutable Cache For Fast HTML Dynamic Tooltips in Waveform Simulator-----------//
//---------------------------------------------------------------------------------------//

(*
The code here is used to cache the data required to quickly determine if a wave is hatched at a given cycle.
This is used to display tooltips in the waveform simulator, which show the value of a wave at a given cycle.
The cache is mutable, as it is updated as gaps are added to the waveform.
It is evil because it is mutable, but this is necessary for performance reasons.
The cache should have minimal space impact (TODO - check this) because it consists of one small typed array
For every wave in the simulation.

*)

open ModelType
open WaveSimTypes
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop


/// A gap in the wave simulator, represented by a start cycle and a length.
/// the only gaps that are stored are relevant are those that correspond to hatched
/// parts of the waveform in which the wave value is not printed.
let initGapStore maxGaps =
    {Gaps = Array.zeroCreate maxGaps; NextGap = 0 ; GapStart = 0; GapEnd = 0}

/// Add a gap to the store, merging it with the previous gap if it is adjacent.
let addGapToStore (store:GapStore) (gap:Gap) =
    if store.GapEnd = gap.Start then
        store.GapEnd <- store.GapEnd + gap.Length
    else
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1
        store.GapStart <- gap.Start
        store.GapEnd <- gap.Start + gap.Length

/// Finalise the store by adding the last gap to the store.
let finaliseStore (store:GapStore) =
    if store.NextGap > 0 && store.GapStart = store.Gaps.[store.NextGap-1].Start + store.Gaps.[store.NextGap-1].Length then
        store.Gaps.[store.NextGap-1] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
    else
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1

/// Check if a wave is hatched at a given cycle.
/// This is done by checking if the cycle is within any of the gaps in a mutable store,
/// which is updated as gaps are added.
let checkIfHatched (store:GapStore) (cycle:int) =
    store.Gaps[0..store.NextGap-1]
    |> Array.exists (fun gap -> gap.Start <= cycle && gap.Length + gap.Start > cycle)

/// Use cached "gap" data to determine if a wave is hatched at a given cycle.
/// If so, return text for a tooltip based on the correct wave value for that cycle.
/// If not, return an empty string.
let getWaveToolTip (cycle:int) (waveNum: int) (ws:WaveSimModel) =
    match List.tryItem waveNum ws.SelectedWaves with
    | None -> ""
    | Some index ->
        let wave = ws.AllWaves[index]
        let start = wave.StartCycle
        let arrayIndex = cycle * ws.SamplingZoom
        //printfn $"Wave - Start: {start}, Cycle: {cycle}, {wave.ViewerDisplayName}, NextGap={wave.HatchedCycles.NextGap}"
        if checkIfHatched wave.HatchedCycles cycle
        then
            match Simulator.simCacheWS.FastSim.Drivers[wave.DriverIndex] with
            | Some {DriverData = data} ->
                if data.Width <= 32 then
                     NumberHelpers.UInt32ToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width data.UInt32Step[arrayIndex]
                else
                    NumberHelpers.BigIntToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width data.BigIntStep[arrayIndex]
            | None ->
                ""
        else

             ""

/// SVG group element for tooltip.
/// The props of the tooltip, as well as its text, are set in the function <c>changeToolTip</c>.
/// Initila props make it invisible.
let evilSvgToolTip (tipName: string) (ws: WaveSimModel) tipText (textProps: IProp list) : ReactElement =
    g [Id (tipName + "Group")] [
        rect [
            Id (tipName + "Rect2")
            SVGAttr.Width 50.0
            SVGAttr.Height 20.0
            SVGAttr.Fill "black"
            SVGAttr.Opacity Constants.tooltipShadowOpacity
            Style [ Visibility "hidden" ]
        ] []
        rect [
            Id (tipName + "Rect1")
            SVGAttr.Width 50.0
            SVGAttr.Height 20.0
            SVGAttr.Fill Constants.tooltipBackgroundColor
            SVGAttr.Opacity 1.0
            Style [ Visibility "hidden" ]
        ] []
        text (
            Id (tipName + "Text") ::
            SVGAttr.Fill Constants.tooltipTextColour ::
            SVGAttr.Opacity "1.0" ::
            textProps
        ) [str tipText]

    ]

/// <summary>Change the tooltip text and position.</summary>
/// <param name="tipText">Text to display in the tooltip.</param>
/// <param name="xPos">X-coordinate of the tooltip.</param>
/// <param name="yPos">Y-coordinate of the tooltip.</param>
/// <param name="ttXMaxEdge">Maximum X-coordinate of the tooltip right edge.</param>
/// <param name="isVisible">True if the tooltip is visible, false if it is hidden.</param>
let changeToolTip tipName tipText (xPos:float) (yPos:float) (ttXMaxEdge: float) (isVisible: bool)=
    let textSvgName = tipName + "Text"
    let svgText = Browser.Dom.document.getElementById textSvgName
    let changeShape shapeId w x y show =
        let shape = Browser.Dom.document.getElementById shapeId
        shape.setAttributeNS("", "width", string w)
        shape.setAttributeNS("", "x", string x)
        shape.setAttributeNS("", "y", string y)
        shape.setAttributeNS("", "style", if show then "visibility: visible" else "visibility: hidden")
    if svgText = null then
        printfn $"Can't find '{textSvgName}' element in DOM needed by changeToolTip with tipName='{tipName}'"
    else
        svgText.textContent <- tipText
        let w = svgText?getComputedTextLength()
        let adjXPos = if xPos + w + 10. > ttXMaxEdge then ttXMaxEdge - w - 10. else xPos
        changeShape (tipName + "Rect1") (w+10.) adjXPos yPos isVisible
        changeShape (tipName + "Rect2") (w+10.) (adjXPos + 2.) (yPos + 2.) isVisible
        changeShape textSvgName w (adjXPos + 5.) (yPos + 16.) isVisible
