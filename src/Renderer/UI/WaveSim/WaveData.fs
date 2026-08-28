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
/// already does for the simulation itself (`Simulator.simCache`). This mirrors it
/// deliberately rather than inventing a second convention: set when the waveform simulator
/// refreshes, cleared when it ends, and read from view code. See docs/mutableState.md - this is
/// a cache of what the app is displaying, not model state.
module WaveData

open CommonTypes
open SimTypes
open SimGraphTypes
open WaveSlice

/// One wave's samples, and the window they cover.
///
/// Per WAVE, not per view. What the viewer needs to know is "does this wave have the cycles it is
/// being drawn over", and that is a question about one wave: a wave just added to the selection is
/// missing while every other wave is fine, and a window that has moved leaves them all missing
/// together. Keying by wave says both without a special case, where one entry for the whole view
/// could only say "all" or "none".
///
/// Waves fetched together share one array - a reply is signal-major and copying it apart would be
/// the only copy in the path - so each entry carries where its own row starts.
type CachedWave =
    { Window: Window
      Width: int
      /// whether the row begins with the sample BEFORE the window, for the first drawn transition
      LeadIn: bool
      /// uint32 words per sample: one for an ordinary bus, more for a wide one
      WordsPerSample: int
      RowBase: int
      Data: uint32 array }

/// What the cache holds for one wave over one window: its samples, or the reason there are none.
///
/// "There are none, and asking again will not help" has to be sayable. A missing wave is exactly
/// what asks for a fetch, so a wave that can never be fetched - the simulation has no driver to
/// name it by - would be asked for again by every update for as long as it stayed selected. The
/// answer is recorded like any other, against the window it was asked over.
type Held =
    | Samples of CachedWave
    /// asked for over this window, and the simulation offers no way to name the signal. The row
    /// stays blank, and nothing asks again until the window changes.
    | NoDriver of Window

let private windowHeld =
    function
    | Samples c -> c.Window
    | NoDriver w -> w

type Source =
    /// the renderer's own simulation - read its step arrays where they lie
    | Local
    /// what has been fetched from the sidecar SESSION `epoch`, by driver index. A wave is here
    /// when it has been asked for, whatever window it was asked over; whether that window is the
    /// one being drawn is the caller's question and is answered by `hasData`.
    ///
    /// **The epoch is what makes invariant D4 true rather than intended.** A driver index names a
    /// different signal in the next build, and a fetch already in the air when one starts still
    /// lands - so without a session on the cache itself, the previous build's samples were written
    /// under indices the new build had reused and drawn under the new signal's name. The comment
    /// here used to point at a check in the update function; there was no such check, and there is
    /// nowhere for one to be, because this is written from inside a promise where the model is not
    /// reachable. Saying which session the cache is OF settles it where the writing happens.
    | Fetched of epoch: int * Map<DriverIndex, Held>

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

/// Read from the sidecar session `epoch`, holding nothing yet: every wave has to be asked for.
///
/// NOT the same as Local, which is what the renderer's own simulation is. In .NET mode the
/// renderer's step arrays exist but are never run, so reading through to them would draw a column
/// of zeros with the confidence of simulation output. This says "ask", where Local says "look".
let holdNothing (epoch: int) = source <- Source.Fetched(epoch, Map.empty)

/// Add what a fetch of session `epoch` carried, keeping any waves already held that it did not
/// carry - a fetch asks only for the waves that were missing, so the rest are still current.
///
/// **Refused unless the cache is of that session** (invariant D4). This is written from inside the
/// promise that fetched, where the model - and so which session is on screen - is not reachable;
/// the alternative was to write regardless and check somewhere later, and there is no later that
/// comes before the next render. A reply from a build that has been replaced would otherwise land
/// under driver indices the new build has reused, and be drawn under the new signal's name until
/// something moved.
///
/// A wave short of its own samples is reported AND kept out (invariant D3). A short reply is
/// silent: reading past the end of a typed array is `undefined` in JavaScript rather than a fault,
/// so every wave after the truncation point drew somebody else's samples - or NaNs - as
/// confidently as the rest. Reporting it and then storing it anyway made the check a description
/// of the failure instead of a stop to it. A wave with no entry draws nothing, which is what the
/// viewer already does for one whose data has not arrived.
let setFetched (epoch: int) (waves: (DriverIndex * CachedWave) list) =
    match source with
    | Source.Fetched(held, existing) when held = epoch ->
        /// how far into the reply this wave's row reaches: where it starts, plus a sample per
        /// drawn cycle and the lead-in, each of them WordsPerSample words wide
        let needsOf (cached: CachedWave) =
            cached.RowBase
            + (cached.Window.SampleCount + (if cached.LeadIn then 1 else 0)) * cached.WordsPerSample

        let sound, short = waves |> List.partition (fun (_, c) -> c.Data.Length >= needsOf c)

        short
        |> List.iter (fun (handle, cached) ->
            Log.error
                $"waveform cache: wave {handle} needs {needsOf cached} values and the reply holds {cached.Data.Length}; it is left blank (invariant D3)")

        source <- Source.Fetched(epoch, (existing, sound) ||> List.fold (fun m (h, c) -> Map.add h (Samples c) m))
    | _ ->
        Log.dbg
            Log.Wave
            $"a fetch of session {epoch} landed after the cache moved on; {List.length waves} waves dropped (invariant D4)"

/// Record that these waves were asked for over this window under session `epoch`, and the
/// simulation cannot name them.
///
/// Not an error and not a gap to be retried: a wave whose driver the simulation does not offer is
/// one this build has no way of fetching, so what is recorded is that answer. It goes in the cache
/// rather than in a list of exceptions because the question - "has this wave got the window it is
/// drawn over" - is the same one, and one answer is easier to keep true than two.
///
/// Session-checked exactly as `setFetched` is, and for the same reason: "no driver" is an answer
/// about one build, and the next build may well have one.
let setNoDriver (epoch: int) (handles: DriverIndex list) (window: Window) =
    match source with
    | Source.Fetched(held, existing) when held = epoch ->
        source <- Source.Fetched(epoch, (existing, handles) ||> List.fold (fun m h -> Map.add h (NoDriver window) m))
    | _ -> ()

let current () = source

/// Does this wave hold the window it is about to be drawn over?
///
/// The renderer's own simulation always does - it is read in place - so only a fetched source can
/// answer no, and a no is what makes a wave one that needs fetching.
let hasData (SignalHandle handle) (window: Window) =
    match source with
    | Source.Local -> true
    | Source.Fetched(_, waves) ->
        match Map.tryFind handle waves with
        | Some held -> windowHeld held = window
        | None -> false

/// The window this wave's samples cover, when it has some.
///
/// What the viewer can draw for it RIGHT NOW, which while a view is being scrolled is neither the
/// window on screen nor the one the controls ask for: it is whatever the last fetch to land carried.
/// Drawing that rather than keeping what is on screen is what makes a fast scroll move.
let heldWindow (SignalHandle handle) =
    match source with
    | Source.Local -> None
    | Source.Fetched(_, waves) ->
        match Map.tryFind handle waves with
        | Some(Samples c) -> Some c.Window
        | Some(NoDriver _)
        | None -> None

/// The waves, of those being drawn, that do not hold the window they are drawn over.
///
/// Derived, every time it is asked for, from the cache and the view. Nothing records which waves
/// are outstanding: a wave needs fetching exactly when it has not got the cycles it is being drawn
/// over, and that is a question with an answer at any moment.
let needFetching (handles: SignalHandle list) (window: Window) =
    handles |> List.filter (fun h -> not (hasData h window))

/// A slice of one wave over `window`, or None where the data is not held - which for a fetched
/// source means the view has moved and the fetch for it has not landed yet, and for a local one
/// means the simulation has not run that far. Every caller already has a way of showing nothing.
let slice (SignalHandle handle as h) (window: Window) : WaveSlice option =
    match source with
    | Source.Local -> localData h |> Option.bind (fun io -> ofLocalDriver io (localClock ()) window)
    | Source.Fetched(_, waves) ->
        match Map.tryFind handle waves with
        | Some(Samples c) when c.Window = window ->
            if c.Width <= 32 then
                Some(ofFetchedWords c.Data c.RowBase c.WordsPerSample c.Width window c.LeadIn)
            else
                Some(ofFetchedBigs c.Data c.RowBase c.WordsPerSample c.Width window c.LeadIn)
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
    | Source.Fetched(_, waves) ->
        // Answered from the window this wave HOLDS, which is the window it is drawn over - so the
        // value column and the tooltip say what the waveform beside them says, whether or not that
        // is the view the controls now ask for.
        match Map.tryFind handle waves with
        | None
        | Some(NoDriver _) -> None
        | Some(Samples c) ->
            let offset = cycle - c.Window.FirstCycle

            if offset < 0 || offset % c.Window.Multiplier <> 0 then
                None
            else
                let i = offset / c.Window.Multiplier

                if i >= c.Window.SampleCount then
                    None
                else
                    let s =
                        if c.Width <= 32 then
                            ofFetchedWords c.Data c.RowBase c.WordsPerSample c.Width c.Window c.LeadIn
                        else
                            ofFetchedBigs c.Data c.RowBase c.WordsPerSample c.Width c.Window c.LeadIn

                    Some(asWidth c.Width (sampleValue s i))
