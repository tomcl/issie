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

module Constants =
    /// How long one SimRun chunk may take. The renderer does nothing while the sidecar simulates,
    /// so this is what keeps the progress bar moving and Cancel answerable.
    let runChunkMs = 100

/// What the sidecar has been told to build: the top sheet, and the step-array size it was built
/// for. Process state about another process, not model state - the model cannot know what a
/// separate program is holding, and nothing in the model would be a better place to say so.
/// What the sidecar holds: the top sheet, the array size it was built for, and the epoch that
/// build issued.
///
/// The epoch is the part that makes the other two checkable. Without it this record is a belief -
/// the sidecar could have restarted, or been built over - and every command sent on the strength of
/// it would be answered as though the belief were true. With it, a command that names a session the
/// sidecar does not hold is refused by name.
let mutable private built: (string * int * int) option = None

/// How far the sidecar's simulation has been run, as it last reported.
///
/// This and `built` above are the only state this module keeps, and neither is a copy of anything
/// in the model: they are the renderer's picture of a process it cannot see inside. WHICH
/// simulator is running is a model fact - Model.SimulateInRenderer - and is passed in rather than
/// mirrored here, because a second copy of a model fact is a thing that can disagree with it.
///
/// The renderer keeps its own simulator's clock in the FastSimulation; the sidecar's is in the
/// sidecar, and this is the renderer's copy of it. Written only from the chunk replies of runTo,
/// which is where the sidecar says what it has reached.
let mutable private sidecarClockTick = 0

/// Whether a fetch is in progress. See fillFor, which is the only thing that reads it.
let mutable private fetching = false

/// Forget what the sidecar holds, so the next refresh builds again. Called when the waveform
/// simulation ends or the design changes.
let forget () =
    built <- None
    sidecarClockTick <- 0
    // whatever is in flight belongs to a simulation that no longer exists; its reply is discarded
    // when it lands, and the next view asks for itself
    fetching <- false

/// (component id, output port, access path) for each driver, taken from the wave index.
///
/// A driver is a component OUTPUT and `WaveIndexT.SimArrayIndex` says which driver a wave reads,
/// so an output-port entry names its own driver. An INPUT-port entry names the driven component
/// instead, and has to be followed back through `InputDrivers` to the output feeding it - most
/// drivers are only reachable that way. Measured on 3cpu: taking output entries alone found 167
/// of 1,094 drivers, and the waves a user actually picks were mostly among the other 927.
///
/// A ComponentId IS an integer here - the whole design is reduced to integer ids when a project
/// is opened (Helpers.RegenerateIds), which is what lets the sidecar name components at all - so
/// this is a rename rather than a conversion.
let private driverSignals (fs: FastSimulation) : Map<int, int * int * int list> =
    let ofFId ((ComponentId comp), path) port =
        comp, port, path |> List.map (fun (ComponentId p) -> p)

    let componentAt fId =
        match Map.tryFind fId fs.FComps with
        | Some fc -> Some fc
        | None -> Map.tryFind fId fs.FCustomComps

    (Map.empty, fs.WaveIndex)
    ||> Array.fold (fun found wi ->
        if Map.containsKey wi.SimArrayIndex found then
            found
        else
            let signal =
                match wi.PortType with
                | PortType.Output -> Some(ofFId wi.Id wi.PortNumber)
                | PortType.Input ->
                    componentAt wi.Id
                    |> Option.bind (fun fc -> Array.tryItem wi.PortNumber fc.InputDrivers)
                    |> Option.flatten
                    |> Option.map (fun (fId, OutputPortNumber port) -> ofFId fId port)

            match signal with
            | Some s -> Map.add wi.SimArrayIndex s found
            | None -> found)

/// The error text of a sidecar reply, or None when it is not an error. Every reply that can fail
/// answers with a JSON object whose only key is "error".
let private errorIn (reply: string) =
    if reply.StartsWith "{\"error\"" then Some reply else None

/// Build the design on the sidecar if it does not already hold it at a big enough array size.
let private ensureBuilt (design: SimpleDesign) (arraySize: int) : JS.Promise<Result<int, string>> =
    match built with
    | Some(top, size, epoch) when top = design.TopSheet && size >= arraySize -> Promise.lift (Ok epoch)
    | _ ->
        promise {
            do! SidecarClient.connect ()
            let sheetJsons = design.Sheets |> List.map Json.serialize<SimpleSheet>
            let! sent = SidecarClient.sendDesign design.TopSheet sheetJsons

            match errorIn sent with
            | Some e ->
                built <- None
                return Error e
            | None ->
                let! reply = SidecarClient.simBuild arraySize

                match errorIn reply with
                | Some e ->
                    built <- None
                    return Error e
                | None ->
                    let epoch = SidecarClient.epochOf reply

                    if epoch = 0 then
                        // a build that issued no epoch built nothing, whatever else the reply said
                        built <- None
                        return Error $"the sidecar's build reply named no session: {reply}"
                    else
                        built <- Some(design.TopSheet, arraySize, epoch)
                        return Ok epoch
        }

[<Emit("JSON.parse($0)")>]
let private parseJson (text: string) : obj = jsNative

/// Advance the sidecar's simulation to `cycle`, a chunk at a time so the renderer stays live.
/// `onProgress` is told the clock tick after each chunk.
///
/// **Only ever as far as the view needs.** A waveform simulation is run LAZILY and extended on
/// demand as the user scrolls or zooms out; it is never run to the end of its step arrays because
/// it was configured for a long one. That is deliberate UX - a design configured for four million
/// cycles must not make the user wait for four million cycles to look at the first ten - and it
/// is the reason `arraySize` above and the cycle here are different numbers with different jobs:
/// the first ALLOCATES for the configured length, this one RUNS for the shown length.
///
/// The renderer's own simulator does the same thing through `lastCycleNeeded` in WaveSimTop,
/// which works the bound out from the same view a slightly different way and lands one sample
/// further on. Both cover what is drawn. When building and running are eventually shared, the
/// two should become one expression rather than two that agree by inspection.
let private runTo (epoch: int) (cycle: int) (onProgress: int -> unit) : JS.Promise<Result<unit, string>> =
    let rec chunk () =
        promise {
            let! reply = SidecarClient.simRun epoch cycle Constants.runChunkMs

            match errorIn reply with
            | Some e -> return Error e
            | None ->
                let parsed = parseJson reply
                let tick: int = unbox parsed?clockTick
                let finished: bool = unbox parsed?``done``
                sidecarClockTick <- tick
                onProgress tick

                if finished then
                    return Ok()
                else
                    return! chunk ()
        }

    chunk ()

/// Fetch the window the view draws, plus the cursor column, and make them what the viewer reads.
///
/// A binary waveform's first drawn cycle needs the value before it, so where the window does not
/// start at cycle 0 one extra sample is asked for and the slice is told it has a lead-in.
let private fetchWindow
    (epoch: int)
    (fs: FastSimulation)
    (driverIndices: int list)
    (window: Window)
    (cursorCycle: int)
    : JS.Promise<Result<unit, string>> =
    let signals = driverSignals fs

    // Only what simRead will answer for: a wider bus has no wire format yet, and asking would
    // fail the whole request rather than just that wave.
    let wanted =
        driverIndices
        |> List.distinct
        |> List.choose (fun i ->
            match Map.tryFind i signals, Array.tryItem i fs.Drivers with
            | Some sig_, Some(Some driver) when driver.DriverWidth <= 32 -> Some(i, sig_, driver.DriverWidth)
            | _ -> None)

    let asked = Set.ofList (List.distinct driverIndices)

    if List.isEmpty wanted then
        // every wave shown is wider than simRead will answer for; say so rather than fall back
        // to local data, which in time will not be there to fall back to
        WaveData.setFetched
            { WaveData.Window = window
              WaveData.LeadIn = false
              WaveData.Rows = Map.empty
              WaveData.Widths = Map.empty
              WaveData.Asked = asked
              WaveData.Data = Array.empty }
            None

        Promise.lift (Ok())
    else
        let leadIn = window.StartSample > 0
        let firstCycle = if leadIn then window.FirstCycle - window.Multiplier else window.FirstCycle
        let samples = window.SampleCount + (if leadIn then 1 else 0)
        let requested = wanted |> List.map (fun (_, s, _) -> s)

        let rowsAndWidths (perSignal: int) =
            wanted
            |> List.mapi (fun row (i, _, width) -> i, (row * perSignal, width))
            |> List.fold (fun (rows, widths) (i, (b, w)) -> Map.add i b rows, Map.add i w widths) (Map.empty, Map.empty)

        promise {
            let! frame = SidecarClient.simRead epoch firstCycle window.Multiplier samples requested
            let asText = SidecarClient.decodeText frame

            if asText.StartsWith "{" then
                return Error asText
            else
                let data = SidecarClient.viewSimReadData frame (List.length requested * samples)
                let rows, widths = rowsAndWidths samples

                let drawn =
                    { WaveData.Window = window
                      WaveData.LeadIn = leadIn
                      WaveData.Rows = rows
                      WaveData.Widths = widths
                      WaveData.Asked = asked
                      WaveData.Data = unbox data }

                // the cursor sits on an exact cycle, which above zoom 1 is between drawn samples
                let! cursorFrame = SidecarClient.simRead epoch cursorCycle 1 1 requested
                let cursorText = SidecarClient.decodeText cursorFrame

                let cursor =
                    if cursorText.StartsWith "{" then
                        None
                    else
                        let cursorData = SidecarClient.viewSimReadData cursorFrame (List.length requested)
                        let cRows, cWidths = rowsAndWidths 1

                        Some
                            { WaveData.Window = { StartSample = cursorCycle; Multiplier = 1; SampleCount = 1 }
                              WaveData.LeadIn = false
                              WaveData.Rows = cRows
                              WaveData.Widths = cWidths
                              WaveData.Asked = asked
                              WaveData.Data = unbox cursorData }

                WaveData.setFetched drawn cursor
                return Ok()
        }

/// Everything the viewer needs for one view, in one promise: build if the sidecar does not hold
/// the design, run to the last cycle the view shows, then fetch that window.
let private fetchForView
    (design: SimpleDesign)
    (arraySize: int)
    (fs: FastSimulation)
    (driverIndices: int list)
    (window: Window)
    (cursorCycle: int)
    (onProgress: int -> unit)
    : JS.Promise<Result<unit, string>> =
    promise {
        match! ensureBuilt design arraySize with
        | Error e -> return Error e
        | Ok epoch ->
            let lastNeeded = max window.LastCycle cursorCycle

            match! runTo epoch (lastNeeded + 1) onProgress with
            | Error e -> return Error e
            | Ok() -> return! fetchWindow epoch fs driverIndices window cursorCycle
    }


/// Choose the simulator for this refresh, and say what the renderer's own one reads through.
///
/// Called once per refresh so that nothing below has to branch on which simulator is running.
/// A new simulation means the design or its shape changed: what the sidecar holds is then not it,
/// and any window already fetched was read from a simulation that no longer exists.
let selectSimulator (inRenderer: bool) (newSimulation: bool) (localLookup: SignalHandle -> IOArray option) =
    if inRenderer then
        WaveData.setLocal localLookup
    elif newSimulation then
        forget ()
        WaveData.setLocal localLookup

/// How many cycles the simulation being shown has actually been run for.
///
/// Whichever simulator is running is the one that knows. Asking the renderer's FastSimulation
/// while the sidecar simulates gives zero however far the sidecar has gone, which is not a number
/// anything should act on: it put the cursor back to cycle 0 when a progress bar was cancelled.
let cyclesSimulated (inRenderer: bool) (fs: FastSimulation) =
    if inRenderer then fs.ClockTick else sidecarClockTick

/// Is a fetch already running?
///
/// A refresh that finds one has nothing to do. It must NOT ask for another - two chains against one
/// session interleave build, run and read - and it must not pretend one was made either: the fetch
/// in progress refreshes when it lands, and that refresh asks for whatever view is current by then,
/// which is the one the user is looking at.
///
/// This used to be answered inside `fillFor`, which returned Ok when it dropped a request. The
/// caller took that for "the data is here", refreshed, found the view still not covered, asked
/// again, was dropped again - a refresh loop for as long as the real fetch took, which on a four
/// million cycle run is half a minute of the renderer doing nothing else.
let fetchInProgress () = fetching

/// Is the data this view draws already where the viewer can read it?
///
/// Always, when the renderer is simulating: its cache reads through to step arrays that are
/// already in memory, so there is nothing to fetch and nothing to wait for. That asymmetry is the
/// only thing that distinguishes the two simulators here, and it is why the caller needs no flag.
let covers (inRenderer: bool) (window: Window) (handles: SignalHandle list) (cursorCycle: int) =
    inRenderer || WaveData.coversFetched window handles cursorCycle

/// Put the data for this view where the viewer can read it, for a simulator that has to be asked.
/// Only ever called when `covers` says no, which for the renderer's own simulator is never.
///
/// **One at a time**, which the CALLER enforces by asking `fetchInProgress` first. A fetch is asked
/// for whenever the view is not held - every checkbox tick, scroll step and cursor move - so
/// without that a second chain starts while the first is still running, and the two interleave
/// build, run and read against one session: the second chain's build resets the simulation under
/// the first chain's read. The sidecar serves them in arrival order and cannot tell that they
/// belong to different views.
let fillFor
    (design: SimpleDesign)
    (arraySize: int)
    (fs: FastSimulation)
    (driverIndices: int list)
    (window: Window)
    (cursorCycle: int)
    (onProgress: int -> unit)
    : JS.Promise<Result<unit, string>> =
    fetching <- true

    fetchForView design arraySize fs driverIndices window cursorCycle onProgress
    |> Promise.map (fun result ->
        fetching <- false
        result)
    |> Promise.catch (fun e ->
        fetching <- false
        Error e.Message)
