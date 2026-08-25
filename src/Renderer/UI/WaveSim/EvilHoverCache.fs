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

open CommonTypes
open ModelType
open SimTypes
open WaveSimTypes
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open SimGraphTypes


/// A gap in the wave simulator, represented by a start cycle and a length.
/// the only gaps that are stored are relevant are those that correspond to hatched
/// parts of the waveform in which the wave value is not printed.
/// A store with room for `maxGaps` runs and no run in progress.
///
/// GapStart and GapEnd are -1 for "nothing pending", not 0. Zero was indistinguishable from a run
/// starting at cycle 0, so a view whose first hatched gap was anywhere else opened by storing a
/// zero-length run - one slot spent on nothing. With the store sized for the most runs a view can
/// hold, that one extra entry was one past the end: harmless in JavaScript, where writing past an
/// array extends it, and an exception anywhere else.
let initGapStore maxGaps =
    {Gaps = Array.zeroCreate maxGaps; NextGap = 0 ; GapStart = -1; GapEnd = -1}

/// Add a gap to the store, extending the run in progress if this one continues it.
///
/// Gaps arrive in order, so a run ends as soon as one arrives that does not continue it.
let addGapToStore (store:GapStore) (gap:Gap) =
    if store.GapEnd < 0 then
        // the first gap of the store begins a run rather than ending one
        store.GapStart <- gap.Start
        store.GapEnd <- gap.Start + gap.Length
    elif store.GapEnd = gap.Start then
        store.GapEnd <- store.GapEnd + gap.Length
    else
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1
        store.GapStart <- gap.Start
        store.GapEnd <- gap.Start + gap.Length

/// Store the run still in progress, if there is one.
///
/// There is nothing to merge with: a run is stored only when the gap that ended it started
/// somewhere else, so the run in progress never touches the one before it. The branch that tried to
/// merge them could not fire, and would have overwritten the stored run's start if it had.
let finaliseStore (store:GapStore) =
    if store.GapEnd >= 0 then
        store.Gaps[store.NextGap] <- {Start = store.GapStart; Length = store.GapEnd - store.GapStart}
        store.NextGap <- store.NextGap + 1
        store.GapStart <- -1
        store.GapEnd <- -1

/// Check if a wave is hatched at a given cycle.
/// This is done by checking if the cycle is within any of the gaps in a mutable store,
/// which is updated as gaps are added.
let checkIfHatched (store:GapStore) (cycle:int) =
    store.Gaps[0..store.NextGap-1]
    |> Array.exists (fun gap -> gap.Start <= cycle && gap.Length + gap.Start > cycle)

/// Whether a memory's contents can change as a simulation runs.
///
/// A ROM's cannot: what it holds is part of its type. Only the location it is READING moves, which
/// is why its rows are worth fetching once and marking again as the cursor moves, while a RAM's
/// have to be fetched afresh for every cycle they are shown at.
let isReadOnlyMemory (fs: FastSimulation) (ram: FComponentId) =
    match fs.Design.ComponentOfInstance ram |> Option.map (fun comp -> comp.Type) with
    | Some(CommonTypes.ROM1 _)
    | Some(CommonTypes.AsyncROM1 _) -> true
    | _ -> false

/// The driver of a memory's address input, in the instance it is in - which is the wave that says
/// what it is reading at each cycle.
///
/// A fact about the elaborated instance, so it comes from `PortView`, like every other driver the
/// wave simulator uses. Fetched with the waveforms being drawn rather than asked for on demand:
/// both the things that want it - the tooltip below, written into the DOM by a mouse handler, and
/// the read marker on a ROM's rows, drawn in a render - happen where nothing can wait.
let addressDriverOf (fs: FastSimulation) (ram: FComponentId) : int option =
    let compId, path = ram

    (PortView.ofInstanceCached fs (InstancePath path)).ViewPorts
    |> List.tryFind (fun p ->
        p.PortComp = compId && p.PortIs = CommonTypes.PortType.Input && p.PortNum = 0)
    |> Option.map (fun p -> p.PortArrayIndex)

/// The cycle at which a memory's address input says what it is reading, for a location shown at
/// `step`. A write lands one clock after the address that caused it, and a synchronous read
/// presents its data a clock late; an asynchronous one does not. RamView.readAndWritten draws the
/// same distinction on the other side of the wire.
let addressReadStep (fs: FastSimulation) (ram: FComponentId) (step: int) : int option =
    match fs.Design.ComponentOfInstance ram |> Option.map (fun comp -> comp.Type) with
    | Some(CommonTypes.AsyncROM1 _)
    | Some(CommonTypes.AsyncRAM1 _) -> Some step
    | Some(CommonTypes.ROM1 _)
    | Some(CommonTypes.RAM1 _) -> if step > 0 then Some(step - 1) else None
    | _ -> None

/// The location a memory is reading at `step`, from whichever simulator is running.
let addressReadAt (fs: FastSimulation) (ram: FComponentId) (step: int) : bigint option =
    match addressDriverOf fs ram, addressReadStep fs ram step with
    | Some driver, Some readStep ->
        WaveData.valueAt (SignalHandle driver) readStep
        |> Option.map (fun address -> address.GetBigInt)
    | _ -> None

/// The memory this wave is the data output of, when it has comments worth showing, together with
/// the driver of the address it reads.
///
/// Which memory it is and what its .ram file said are facts about the component as DRAWN, so they
/// come from the design. Where its address lies is a fact about the elaborated instance, so it
/// comes from `PortView` - like every other width and driver the wave simulator uses.
///
/// Returned as a pair because both callers want both: the tooltip below, to write the comment, and
/// WaveSimTop.missingForWaves, to fetch the address wave along with the ones being drawn. Fetched
/// with them rather than on hover because a tooltip is written into the DOM by a mouse handler,
/// which cannot wait for anything - so the address has to be there before the pointer arrives.
let romAddressOf (fs: FastSimulation) (wave: Wave) : (int * Map<bigint, string>) option =
    if wave.WaveId.PortType <> CommonTypes.PortType.Output then
        None
    else
        let comments =
            fs.Design.ComponentOfInstance wave.WaveId.Id
            |> Option.bind (fun comp ->
                match comp.Type with
                | CommonTypes.AsyncROM1 mem
                | CommonTypes.ROM1 mem -> mem.Comments
                | _ -> None)
            |> Option.filter (Map.isEmpty >> not)

        comments
        |> Option.bind (fun comments ->
            addressDriverOf fs wave.WaveId.Id |> Option.map (fun driver -> driver, comments))

/// The comment written in a .ram file against the location a ROM is reading at a given simulation
/// step, if the ROM has any comments and this wave is its data output.
///
/// The location read is the address input one step earlier for a synchronous ROM and at the same
/// step for an asynchronous one, which is the distinction RamView.readAndWritten also makes.
///
/// The address comes from `WaveData`, which answers from whichever simulator is running - the local
/// arrays where the renderer is simulating, and the window fetched from the sidecar where it is
/// not. It used to be read straight out of the renderer's own FastSimulation, which in .NET mode is
/// built for its structure and never run: every address would have been an unrun array's zero, so
/// the comment against location zero was shown on every hover, confidently. That was guarded by
/// refusing to answer at all, which is why these comments disappeared in that mode.
///
/// None where the address is not among the samples held - a synchronous ROM's address is one CYCLE
/// back, and at a zoom where a column is several cycles wide that is not a sample that was fetched.
/// No comment is better than the wrong one.
let getRomCommentAtStep (fs: FastSimulation) (step: int) (wave: Wave) : string =
    match romAddressOf fs wave with
    | None -> ""
    | Some(_, comments) ->
        addressReadAt fs wave.WaveId.Id step
        |> Option.bind (fun address -> Map.tryFind address comments)
        |> Option.defaultValue ""

/// Text for the tooltip shown when hovering a waveform at a given cycle, or "" for no tooltip.
/// A value is given when the wave is too narrow there to have its number printed on it - the cached
/// "gap" data says when that is - and a ROM's data output also gives the comment written against
/// the location being read, so that both appear when the number is hidden as well.
///
/// The wave is passed in rather than counted off SelectedWaves by row number. The rows drawn are
/// the selected waves that WaveDetails still HOLDS - see WaveSimStyle.selectedWaves, which the three
/// columns are all built from - so counting rows off SelectedWaves itself answered for a different
/// row wherever the two differ, and indexed WaveDetails with a key it might not have.
/// Everything is asked of the waveform AS DRAWN - the gaps in it, the samples it was drawn from,
/// and the cycle they are of. What is under the pointer is a column of a picture, and the picture
/// may be a window behind the controls while its data is still on its way; the tooltip describes
/// what is there rather than what has been asked for.
///
/// `sample` is therefore an index into that picture, counted from its left edge, and NOT a clock
/// cycle. It used to be given the absolute display cycle and to test it against gaps recorded from
/// the window's left edge, so a viewer scrolled anywhere but cycle 0 asked about the wrong column.
let getWaveToolTip (sample: int) (drawn: WaveDrawn.Drawn) (wave: Wave) (ws: WaveSimModel) =
    let window = drawn.Spec.Window
    let cycle = window.FirstCycle + sample * window.Multiplier

    let hiddenValue =
        if checkIfHatched drawn.Gaps sample then
            WaveDrawn.valueAtSample drawn sample
            |> Option.map (fun fd ->
                match fd.Dat with
                | Word v -> NumberHelpers.UInt32ToPaddedString Constants.waveLegendMaxChars ws.Radix fd.Width v
                | BigWord v -> NumberHelpers.BigIntToPaddedString Constants.waveLegendMaxChars ws.Radix fd.Width v)
            |> Option.defaultValue ""
        else ""
    match hiddenValue, getRomCommentAtStep Simulator.simCacheWS.FastSim cycle wave with
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
