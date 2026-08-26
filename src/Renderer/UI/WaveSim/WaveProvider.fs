/// Which simulator answers the waveform viewer, and - when it is the .NET sidecar - fetching
/// what the view draws.
///
/// The shape of it: the sidecar builds and runs the simulation, and the renderer asks it for one
/// window - the samples the current view shows, for the waves it is showing - every time that
/// view changes. `SidecarClient.simRead` takes exactly the viewer's own (StartCycle, SamplingZoom,
/// ShownCycles) triple, so a view at any zoom is ONE request rather than one per wave, and the
/// reply is read with no copy. What comes back becomes a `WaveData.Fetched` source, and the
/// drawing code is none the wiser (see WaveSlice).
///
/// Two requests, not one, and for a reason: the drawn window holds a sample every SamplingZoom
/// cycles, but the cursor sits on an exact cycle which at any zoom above 1 falls between those
/// samples. So the cursor column - every shown wave at that one cycle - is fetched alongside it.
/// Both are sub-millisecond: the measured round trip is 0.3-0.6 ms and a typical window is a few
/// hundred kB at 350-800 MB/s, against waveform generation that is budgeted at 50 ms a slice.
///
/// **Not yet here**: buses wider than 32 bits, which `simRead` refuses, so their waves come back
/// with no data and are drawn empty. That is the next thing to add, along with the RAM tables,
/// which need a command of their own.
module WaveProvider

open Fable.Core
open Fable.Core.JsInterop
open Fable.SimpleJson // Json.serialize, the renderer's wire encoder (an extension member)
open CommonTypes
open SimTypes
open WaveSlice

/// Forget what the sidecar holds, so the next refresh builds again. Called when the waveform
/// simulation ends or the design changes. The session itself is SidecarSession's, which the step
/// simulator shares: the sidecar holds one simulation at a time, so one module knows what it is.
let forget () = SidecarSession.forget ()

/// The width of a wave's signal, or None where there is no signal - an input port with nothing
/// driving it. A lookup in the instance's port list, which the viewer holds anyway.
///
/// This is all that is asked about a wave before fetching it. WHERE the data lies needs no lookup
/// at all: the wave's SimArrayIndex is the driver HANDLE this build issued (the port slice hands
/// them out, and an input port's index is already the array of the output driving it, resolved by
/// the simulator's own linker), and the fetch quotes it straight back. It used to be turned back
/// into a (component, port, path) name here for the simulator to resolve again on arrival - the
/// same fact, derived twice.
let private widthOf (fs: FastSimulation) (wi: WaveIndexT) : int option =
    let compId, ap = wi.Id

    (PortView.ofInstanceCached fs (InstancePath ap)).ViewPorts
    |> List.tryFind (fun p -> p.PortComp = compId && p.PortIs = wi.PortType && p.PortNum = wi.PortNumber)
    |> Option.map (fun p -> p.PortWidth)
    |> Option.filter (fun width -> width > 0)

/// Fetch some waves over the window they are about to be drawn over, and add them to the cache.
///
/// The waves asked for are the ones that have not got that window - not "all the waves in the
/// view" - so a fetch after a wave is added to the selection carries just that wave, while a fetch
/// after the window moves carries them all. Either way it is one request: `simRead` takes a list.
///
/// A binary waveform's first drawn cycle needs the value before it, so where the window does not
/// start at cycle 0 one extra sample is asked for and the slice is told it has a lead-in.
///
/// There is no separate cursor read. The value column and the tooltip are answered out of the
/// samples the waveform beside them is drawn from, so they cannot disagree with it, and at a zoom
/// where the cursor sits between drawn samples they say what the drawn sample says.
let private fetchWaves
    (epoch: int)
    (fs: FastSimulation)
    (waves: WaveIndexT list)
    (window: Window)
    : JS.Promise<Result<unit, string>> =
    // Every wave asked for, whatever its width - simRead carries a sample in as many words as it
    // needs. This used to drop anything over 32 bits while still recording it as asked for, so
    // coverage said yes, the wave came back with no row, and it kept whatever it had been showing
    // before, silently and for ever.
    //
    // By driver, because that is what the cache is keyed by: two ports sharing an array - a custom
    // component's output and the inner Output's - are one row, asked for once.
    let asked = List.distinctBy (fun (wi: WaveIndexT) -> wi.SimArrayIndex) waves

    let wanted =
        asked
        |> List.choose (fun wi -> widthOf fs wi |> Option.map (fun width -> wi.SimArrayIndex, width))

    // Waves this simulation offers no way to name: an input port with nothing driving it, which is
    // an unconnected input. They are recorded as having no driver rather than left missing - a
    // missing wave is what asks for a fetch, so leaving them would ask again on every update, for
    // ever. The rest of the view is fetched as usual, which is the point: refusing the whole
    // request over them left the viewer blank.
    let unnameable =
        let named = wanted |> List.map fst |> Set.ofList
        asked |> List.map (fun wi -> wi.SimArrayIndex) |> List.filter (fun i -> not (Set.contains i named))

    if not (List.isEmpty unnameable) then
        WaveData.setNoDriver unnameable window

        let plural = if List.length unnameable = 1 then "waveform" else "waveforms"

        Log.warnOnce
            $"no-driver-{fs.SimulatedTopSheet}-{List.length unnameable}"
            ($"{List.length unnameable} of {List.length asked} {plural} cannot be read from the .NET simulator:"
             + " nothing drives them, which is what an unconnected input port is."
             + " They are left blank; every other waveform is unaffected.")

    if List.isEmpty wanted then
        // nothing left to ask for once those are out
        Promise.lift (Ok())
    else
        let leadIn = window.StartSample > 0
        let firstCycle = if leadIn then window.FirstCycle - window.Multiplier else window.FirstCycle
        let samples = window.SampleCount + (if leadIn then 1 else 0)
        let requested = wanted |> List.map fst

        promise {
            let! frame = SidecarClient.simReadDrivers epoch firstCycle window.Multiplier samples requested
            let asText = SidecarClient.decodeText frame

            if asText.StartsWith "{" then
                return Error asText
            else
                // the reply states its own layout, so a signal whose width the renderer has stale
                // is still read the way the sender wrote it
                let wordsPerSample = SidecarClient.simReadWordsPerSample frame
                let data: uint32 array =
                    unbox (SidecarClient.viewSimReadData frame (List.length requested * samples * wordsPerSample))

                // Written unconditionally. Whether this answer is still of the simulation being
                // drawn is a question about the model - which session it believes is live - and it
                // is asked where the model is, when this promise's completion message lands
                // (Update.discardIfSessionMoved). A promise reaching for that belief through a
                // side channel is what put a fact the UI must draw somewhere the UI could not read.
                //
                // one array, shared: the reply is signal-major, and each wave records where its
                // own row starts rather than the rows being copied apart
                wanted
                |> List.mapi (fun row (i, width) ->
                    i,
                    { WaveData.Window = window
                      WaveData.Width = width
                      WaveData.LeadIn = leadIn
                      WaveData.WordsPerSample = wordsPerSample
                      WaveData.RowBase = row * samples * wordsPerSample
                      WaveData.Data = data })
                |> WaveData.setFetched

                return Ok()
        }

/// One signal's value at one cycle: what the schematic probe shows.
///
/// A read like any other - the same by-handle read, one signal and one sample of it - so the probe uses
/// the mechanism everything else uses rather than a transport of its own. It was briefly a
/// BLOCKING read over a second transport, so that the value could be had inside the render that
/// draws it. That cost 2.2ms against this path's 0.2ms, because a synchronous XMLHttpRequest is
/// the only thing that can block a renderer's thread and Chromium does not make it quick. The
/// label appearing one render later is not something a user can see.
let fetchProbeValue
    (epoch: int)
    (fs: FastSimulation)
    (wi: WaveIndexT)
    (cycle: int)
    : JS.Promise<bigint option> =
    match widthOf fs wi with
    | None -> Promise.lift None
    | Some _ ->
        promise {
            let! frame = SidecarClient.simReadDrivers epoch cycle 1 1 [ wi.SimArrayIndex ]
            let asText = SidecarClient.decodeText frame

            if asText.StartsWith "{" then
                Log.dbg Log.Wave $"reading the probe's wire at cycle {cycle}: {asText}"
                return None
            else
                let wordsPerSample = SidecarClient.simReadWordsPerSample frame
                let data: uint32 array = unbox (SidecarClient.viewSimReadData frame wordsPerSample)

                // least significant word first, whatever the width - see Protocol.SimRead
                return
                    (0I, [ wordsPerSample - 1 .. -1 .. 0 ])
                    ||> List.fold (fun acc w -> (acc <<< 32) + bigint data[w])
                    |> Some
        }

/// Read the waves asked for over that window, from a session that has already been run far enough.
///
/// Running it that far is a separate operation, or rather a sequence of them, sequenced by the
/// update function - see WaveSimTop.fetchWhatIsMissing. Nothing here decides anything about the
/// session: it is handed one that is ready and reads from it.
let private fetchForView
    (epoch: int)
    (fs: FastSimulation)
    (waves: WaveIndexT list)
    (window: Window)
    : JS.Promise<Result<unit, string>> =
    fetchWaves epoch fs waves window

/// Choose the simulator for this refresh, and say what the renderer's own one reads through.
///
/// Called once per refresh so that nothing below has to branch on which simulator is running.
/// A new simulation means the design or its shape changed: what the sidecar holds is then not it,
/// and any window already fetched was read from a simulation that no longer exists.
let selectSimulator
    (inRenderer: bool)
    (newSimulation: bool)
    (localLookup: SignalHandle -> IOArray option)
    (localClock: unit -> int)
    =
    match inRenderer, newSimulation, WaveData.current () with
    | true, _, _ -> WaveData.setLocal localLookup localClock
    | false, true, _ ->
        // the design or its shape changed: what the sidecar holds is not it, and every wave already
        // fetched was read from a simulation that no longer exists
        forget ()
        WaveData.holdNothing ()
    | false, false, WaveData.Source.Local ->
        // the sidecar is simulating and the cache is still reading the renderer's own step arrays -
        // which in this mode are never run. Ask, rather than draw a simulation nobody has run.
        WaveData.holdNothing ()
    | false, false, _ -> ()

/// How many cycles the simulation being shown has actually been run for.
///
/// Whichever simulator is running is the one that knows. Asking the renderer's FastSimulation
/// while the sidecar simulates gives zero however far the sidecar has gone, which is not a number
/// anything should act on: it put the cursor back to cycle 0 when a progress bar was cancelled.
let cyclesSimulated (inRenderer: bool) (sidecarClock: int) (fs: FastSimulation) =
    if inRenderer then fs.ClockTick else sidecarClock

/// The waves that are not holding the window they are about to be drawn over, and so have to be
/// asked for.
///
/// Derived, on every refresh, from what the cache holds and what the view asks for. Nothing records
/// which waves are outstanding, because nothing needs to: a wave needs fetching exactly when it has
/// not got the cycles it is being drawn over.
///
/// None of them, ever, when the renderer is simulating: its cache reads through to step arrays that
/// are already in memory, so there is nothing to fetch and nothing to wait for. That asymmetry is
/// the only thing that distinguishes the two simulators here, and it is why the caller needs no
/// flag of its own.
let wavesToFetch (inRenderer: bool) (handles: SignalHandle list) (window: Window) =
    if inRenderer then [] else WaveData.needFetching handles window

/// Fetch some waves over the window they are drawn over, and put them where the viewer reads them.
///
/// **One fetch at a time**, which the CALLER enforces with `WaveSimModel.FetchInProgress`. A fetch
/// is asked for whenever a wave is not holding its window - every checkbox tick and scroll step -
/// so without that a second chain starts while the first is still running, and the two interleave
/// build, run and read against one session: the second chain's build resets the simulation under
/// the first chain's read. The sidecar serves them in arrival order and cannot tell that they
/// belong to different views.
///
/// One request covers every wave asked for, so the number of round trips is one per view rather
/// than one per wave, and the waves that arrive together are drawable together.
let fetchWavesFor
    (epoch: int)
    (fs: FastSimulation)
    (waves: WaveIndexT list)
    (window: Window)
    : JS.Promise<Result<unit, string>> =
    fetchForView epoch fs waves window
    |> Promise.catch (fun e -> Error e.Message)
