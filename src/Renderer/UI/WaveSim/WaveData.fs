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
      Data: uint32 array }

type Source =
    /// the renderer's own simulation - read its step arrays where they lie
    | Local
    /// a window fetched from the sidecar, plus the cursor column that goes with it
    | Fetched of window: FetchedWindow * cursor: FetchedWindow option

/// What the viewer is currently drawing from. Local until the waveform simulator says otherwise.
let mutable private source = Source.Local

let setLocal () = source <- Source.Local
let setFetched (window: FetchedWindow) (cursor: FetchedWindow option) = source <- Source.Fetched(window, cursor)
let current () = source

/// A slice of one wave over `window`, or None where the data is not held - which for a fetched
/// source means the view has moved and the fetch for it has not landed yet, and for a local one
/// means the simulation has not run that far. Every caller already has a way of showing nothing.
let slice (driverData: IOArray) (driverIndex: int) (window: Window) : WaveSlice option =
    match source with
    | Source.Local -> ofLocalDriver driverData window
    | Source.Fetched(fetched, _) ->
        if fetched.Window <> window then
            None
        else
            match Map.tryFind driverIndex fetched.Rows, Map.tryFind driverIndex fetched.Widths with
            | Some rowBase, Some width -> Some(ofFetchedWords fetched.Data rowBase width window fetched.LeadIn)
            | _ -> None

/// The value of one wave at one clock cycle, for the value column, the hover tooltip and the
/// schematic probe. None where it cannot be answered.
let valueAt (driverData: IOArray) (driverIndex: int) (cycle: int) (width: int) : FastData option =
    let asData (v: bigint) =
        if width > 32 then
            { Dat = BigWord v; Width = width }
        else
            { Dat = Word(uint32 v); Width = width }

    match source with
    | Source.Local ->
        if width > 32 then
            driverData.TryBig cycle |> Option.map (fun v -> { Dat = BigWord v; Width = width })
        else
            driverData.TryU32 cycle |> Option.map (fun v -> { Dat = Word v; Width = width })
    | Source.Fetched(fetched, cursor) ->
        // the cursor column first, since that is the cycle the value readouts almost always want,
        // then the drawn window, whose samples are only every Multiplier cycles
        let fromWindow (f: FetchedWindow) =
            match Map.tryFind driverIndex f.Rows, Map.tryFind driverIndex f.Widths with
            | Some rowBase, Some w ->
                let offset = cycle - f.Window.FirstCycle

                if offset < 0 || offset % f.Window.Multiplier <> 0 then
                    None
                else
                    let i = offset / f.Window.Multiplier

                    if i >= f.Window.SampleCount then
                        None
                    else
                        let s = ofFetchedWords f.Data rowBase w f.Window f.LeadIn
                        Some(asData (sampleValue s i))
            | _ -> None

        match cursor |> Option.bind fromWindow with
        | Some v -> Some v
        | None -> fromWindow fetched
