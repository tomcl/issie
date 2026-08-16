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
open SimTypes
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

/// The comment written in a .ram file against the location a ROM is reading at a given simulation
/// step, if the ROM has any comments and this wave is its data output.
/// The location read is the address input one step earlier for a synchronous ROM and at the same
/// step for an asynchronous one, which is the distinction WaveSimRams.addReadWrite also makes.
let getRomCommentAtStep (fs: FastSimulation) (step: int) (wave: Wave) : string =
    match Map.tryFind wave.WaveId.Id fs.WaveComps with
    | Some fc when wave.WaveId.PortType = CommonTypes.PortType.Output ->
        let readStep =
            match fc.FType with
            | CommonTypes.AsyncROM1 _ -> Some step
            | CommonTypes.ROM1 _ -> if step > 0 then Some (step - 1) else None
            | _ -> None
        let comments : Map<bigint,string> =
            match fc.FType with
            | CommonTypes.AsyncROM1 mem
            | CommonTypes.ROM1 mem -> Option.defaultValue Map.empty mem.Comments
            | _ -> Map.empty
        match readStep with
        | Some readStep when not (Map.isEmpty comments) ->
            FastExtract.getFastComponentInput fc 0 readStep
            |> fun address -> Map.tryFind address comments
            |> Option.defaultValue ""
        | _ -> ""
    | _ -> ""

/// Text for the tooltip shown when hovering a waveform at a given cycle, or "" for no tooltip.
/// A value is given when the wave is too narrow there to have its number printed on it - the cached
/// "gap" data says when that is - and a ROM's data output also gives the comment written against
/// the location being read, so that both appear when the number is hidden as well.
///
/// The wave is passed in rather than counted off SelectedWaves by row number. The rows drawn are
/// the selected waves that AllWaves still HOLDS - see WaveSimStyle.selectedWaves, which the three
/// columns are all built from - so counting rows off SelectedWaves itself answered for a different
/// row wherever the two differ, and indexed AllWaves with a key it might not have.
let getWaveToolTip (cycle:int) (wave: Wave) (ws:WaveSimModel) =
    let arrayIndex = cycle * ws.SamplingZoom
    let hiddenValue =
        if checkIfHatched wave.HatchedCycles cycle then
            match Simulator.simCacheWS.FastSim.Drivers[wave.DriverIndex] with
            // The cursor can rest past the end of what has been simulated, so the step is asked
            // for rather than taken: a read off the end of a JS typed array is undefined, which
            // would be written into the tooltip as one.
            | Some {DriverData = data} when data.Width <= 32 ->
                Array.tryItem arrayIndex data.UInt32Step
                |> Option.map (NumberHelpers.UInt32ToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width)
                |> Option.defaultValue ""
            | Some {DriverData = data} ->
                Array.tryItem arrayIndex data.BigIntStep
                |> Option.map (NumberHelpers.BigIntToPaddedString Constants.waveLegendMaxChars ws.Radix data.Width)
                |> Option.defaultValue ""
            | None -> ""
        else ""
    match hiddenValue, getRomCommentAtStep Simulator.simCacheWS.FastSim arrayIndex wave with
    | "", "" -> ""
    | value, "" -> $"Value:{value}"
    | "", comment -> comment
    | value, comment -> $"Value:{value}. {comment}"

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
        Log.warnOnce "tooltip-missing-element" $"the DOM element '{textSvgName}' needed by changeToolTip is not there"
    else
        svgText.textContent <- tipText
        let w = svgText?getComputedTextLength()
        let adjXPos = if xPos + w + 10. > ttXMaxEdge then ttXMaxEdge - w - 10. else xPos
        changeShape (tipName + "Rect1") (w+10.) adjXPos yPos isVisible
        changeShape (tipName + "Rect2") (w+10.) (adjXPos + 2.) (yPos + 2.) isVisible
        changeShape textSvgName w (adjXPos + 5.) (yPos + 16.) isVisible
