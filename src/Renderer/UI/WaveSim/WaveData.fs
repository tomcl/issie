/// Where the waveform viewer gets its data, whichever simulator produced it.
///
/// Two sources answer the same two questions. `Local` reads the renderer's own
/// `FastSimulation` step arrays in place, which is what the viewer has always done. `Fetched`
/// answers from the window most recently pulled off the .NET sidecar, which holds only the
/// samples the current view draws - that being the whole point, since a sidecar simulation is
/// sized for the machine's memory rather than a browser heap and its step arrays are far too
/// large to ship.
///
/// **Both questions are answered with a slice**, even the point reads: a cursor column is one
/// sample of every selected wave, which is a window of one. That keeps one shape on the wire,
/// one shape in the cache and one shape for the drawing code - and it is the shape that will
/// carry run-length encoded waveforms when they arrive (see WaveSlice).
///
/// **Why the source is module state.** The functions here are called from view code -
/// `getWaveValue` while laying out the value column, the hover cache while building a tooltip -
/// which has no model to thread a flag through, and reaching for a global is what that code
/// already does for the simulation itself (`Simulator.simCacheWS`). This mirrors it
/// deliberately rather than inventing a second convention: set when the waveform simulator
/// refreshes, cleared when it ends, and read from view code. See docs/mutableState.md - this is
/// a cache of what the app is displaying, not model state.
module WaveData

open CommonTypes
open SimTypes
open SimGraphTypes
open WaveSlice

/// One fetched window: the samples of every wave the view is drawing, signal-major, exactly as
/// `SidecarClient.simRead` returns them and viewed with no copy.
type FetchedWindow =
    { Window: Window
      /// whether a sample before the window was fetched too, for the first drawn transition
      LeadIn: bool
      /// where each driver's row starts in `Data`, by driver index
      Rows: Map<int, int>
      /// bus width per driver index, since a slice needs to know which store it is
      Widths: Map<int, int>
      /// how many uint32 words each sample occupies - ceil(widest asked for / 32). A reply of
      /// ordinary buses has one, and one wide bus widens the samples of that reply alone
      WordsPerSample: int
      /// every driver this fetch was for
      Asked: Set<int>
      Data: uint32 array }

type Source =
    /// the renderer's own simulation - read its step arrays where they lie
    | Local
    /// a window fetched from the sidecar, plus the cursor column that goes with it
    | Fetched of window: FetchedWindow * cursor: FetchedWindow option

/// What the viewer is currently drawing from. Local until the waveform simulator says otherwise.
let mutable private source = Source.Local

/// How the Local source turns a handle into the data behind it.
///
/// A function rather than a FastSimulation, because this module must not know what one is: it is
/// the cache, and a simulation is the private business of whichever simulator filled it. Whoever
/// selects the Local source installs this, and the natural closure reaches for the CURRENT
/// simulation rather than capturing one - capturing would pin a simulation and its step arrays
/// for as long as the cache lives, which is the leak ModelHelpers.releaseWaveSimData exists to
/// prevent.
///
/// Returning None is not an error: it is a handle from a simulation that is no longer there, and
/// every caller already knows how to show nothing.
let mutable private localData: SignalHandle -> IOArray option = fun _ -> None

/// How far the renderer's own simulation has been run. Installed beside the lookup because a
/// slice of local data is only valid up to it, and reading it at call time rather than closing
/// over a number keeps the answer current as the simulation is extended.
let mutable private localClock: unit -> int = fun () -> 0

/// Read from the renderer's own simulation, through `lookup`. Nothing is copied - a local slice
/// names the step array where it lies - so this "fill" is only recording how to find it.
let setLocal (lookup: SignalHandle -> IOArray option) (clock: unit -> int) =
    localData <- lookup
    localClock <- clock
    source <- Source.Local

/// Make a fetched window what the viewer reads, checking that it is the shape it claims to be.
///
/// Both of these are silent when wrong, which is why they are checked rather than trusted. A row
/// whose handle was never asked for is a row indexed against a reply that does not contain it; and
/// a reply shorter than signals x samples gives every wave after the truncation point somebody
/// else's data, drawn as confidently as the rest. Neither shows up as an error anywhere else.
///
/// Said rather than thrown: what is drawn will be wrong, but refusing to draw is not better, and
/// the message names the invariant it broke.
let setFetched (window: FetchedWindow) (cursor: FetchedWindow option) =
    let rowsNotAsked =
        window.Rows |> Map.filter (fun handle _ -> not (Set.contains handle window.Asked)) |> Map.count

    if rowsNotAsked > 0 then
        Log.error
            $"waveform cache: {rowsNotAsked} rows of a fetched window were never asked for (invariant D1)"

    let samplesPerRow = window.Window.SampleCount + (if window.LeadIn then 1 else 0)
    let expected = Map.count window.Rows * samplesPerRow * window.WordsPerSample

    if window.Data.Length < expected then
        Log.error
            $"waveform cache: a fetched window holds {window.Data.Length} values where {Map.count window.Rows} signals x {samplesPerRow} samples needs {expected} (invariant D3)"

    source <- Source.Fetched(window, cursor)
let current () = source

/// Whether the fetched source already holds this view - the same window, the same cursor cycle,
/// and every wave now being drawn among those it was fetched for. A local source is never
/// "covered": it needs no fetch at all.
let coversFetched (window: Window) (handles: SignalHandle list) (cursorCycle: int) =
    match source with
    | Source.Local -> false
    | Source.Fetched(fetched, cursor) ->
        fetched.Window = window
        && (match cursor with
            | Some c -> c.Window.StartSample = cursorCycle
            | None -> true)
        && handles |> List.forall (fun (SignalHandle i) -> Set.contains i fetched.Asked)

/// A slice of one wave over `window`, or None where the data is not held - which for a fetched
/// source means the view has moved and the fetch for it has not landed yet, and for a local one
/// means the simulation has not run that far. Every caller already has a way of showing nothing.
let slice (SignalHandle handle as h) (window: Window) : WaveSlice option =
    match source with
    | Source.Local -> localData h |> Option.bind (fun io -> ofLocalDriver io (localClock ()) window)
    | Source.Fetched(fetched, _) ->
        if fetched.Window <> window then
            None
        else
            match Map.tryFind handle fetched.Rows, Map.tryFind handle fetched.Widths with
            | Some rowBase, Some width when width <= 32 ->
                Some(ofFetchedWords fetched.Data rowBase fetched.WordsPerSample width window fetched.LeadIn)
            | Some rowBase, Some width ->
                Some(ofFetchedBigs fetched.Data rowBase fetched.WordsPerSample width window fetched.LeadIn)
            | _ -> None

/// The value of one wave at one clock cycle, for the value column, the hover tooltip and the
/// schematic probe. None where it cannot be answered.
let valueAt (SignalHandle handle as h) (cycle: int) : FastData option =
    let asWidth width (v: bigint) =
        if width > 32 then
            { Dat = BigWord v; Width = width }
        else
            { Dat = Word(uint32 v); Width = width }

    match source with
    | Source.Local ->
        // the width is the signal's own, taken from the data rather than passed in: a caller that
        // could get it wrong is a caller that can print one signal as another's width
        localData h
        |> Option.filter (fun _ -> cycle <= localClock ())
        |> Option.bind (fun io ->
            if io.Width > 32 then
                io.TryBig cycle |> Option.map (fun v -> { Dat = BigWord v; Width = io.Width })
            else
                io.TryU32 cycle |> Option.map (fun v -> { Dat = Word v; Width = io.Width }))
    | Source.Fetched(fetched, cursor) ->
        // the cursor column first, since that is the cycle the value readouts almost always want,
        // then the drawn window, whose samples are only every Multiplier cycles
        let fromWindow (f: FetchedWindow) =
            match Map.tryFind handle f.Rows, Map.tryFind handle f.Widths with
            | Some rowBase, Some w ->
                let offset = cycle - f.Window.FirstCycle

                if offset < 0 || offset % f.Window.Multiplier <> 0 then
                    None
                else
                    let i = offset / f.Window.Multiplier

                    if i >= f.Window.SampleCount then
                        None
                    else
                        let s =
                            if w <= 32 then
                                ofFetchedWords f.Data rowBase f.WordsPerSample w f.Window f.LeadIn
                            else
                                ofFetchedBigs f.Data rowBase f.WordsPerSample w f.Window f.LeadIn

                        Some(asWidth w (sampleValue s i))
            | _ -> None

        match cursor |> Option.bind fromWindow with
        | Some v -> Some v
        | None -> fromWindow fetched
