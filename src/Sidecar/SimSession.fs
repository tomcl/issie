/// The sidecar's simulation session: the existing Issie simulator, run under .NET on a design
/// that arrived as SimpleSheets. This is the BASELINE - today's simulation code, unchanged,
/// reached through SimpleDesignShim - that rewrites and the Electron simulator are compared
/// against, via SimDigest (byte-identical observable-behaviour text) and SimLog (identical
/// per-invocation cost records in both runtimes).
///
/// One session at a time, held as process state and replaced by the next build - which also
/// suits FastCreate's non-reentrant build. Progress and cancellation are the CLIENT's:
/// SimRun takes a target cycle and a time budget and reports how far it got, exactly the
/// contract the renderer's own progress loop uses against local simulation, so the caller
/// chunks, shows progress from the replies, and cancels by not sending the next chunk.
module Issie.Sidecar.SimSession

open CommonTypes

let mutable private session: SimTypes.FastSimulation option = None

let private escape (text: string) =
    text.Replace("\\", "/").Replace("\"", "'").Replace("\n", " ").Replace("\r", " ")

let private errorReply (message: string) =
    sprintf """{"error":"%s"}""" (escape message)

/// Build a simulation of the design's top sheet. Replaces any previous session.
let build (design: SimpleDesign) (maxArraySize: int) : string =
    let ldcs = SimpleDesignShim.designToLoadedComponents design

    match ldcs |> List.tryFind (fun ldc -> ldc.Name = design.TopSheet) with
    | None -> errorReply $"no sheet called {design.TopSheet} in the design"
    | Some top ->
        let sw = System.Diagnostics.Stopwatch.StartNew()

        match Simulator.startCircuitSimulation maxArraySize design.TopSheet top.CanvasState ldcs with
        | Error e ->
            session <- None
            errorReply $"simulation build failed: %A{e.ErrType}"
        | Ok simData ->
            sw.Stop()
            let fs = simData.FastSim
            session <- Some fs

            sprintf
                """{"sheet":"%s","components":%d,"maxArraySize":%d,"buildMs":%.2f}"""
                (escape design.TopSheet)
                (fs.FComps.Count + fs.FCustomComps.Count)
                fs.MaxArraySize
                sw.Elapsed.TotalMilliseconds

/// Run the session's simulation towards `targetCycle`, giving up after `timeoutMs` (0 = no
/// budget). The reply says where the clock got to; the caller repeats until done - each call
/// is one SimLog record, mirroring the renderer's progress loop.
let run (targetCycle: int) (timeoutMs: int) : string =
    match session with
    | None -> errorReply "no simulation built - send SimBuild first"
    | Some fs ->
        let timeout = if timeoutMs <= 0 then None else Some(float timeoutMs)
        let sw = System.Diagnostics.Stopwatch.StartNew()
        FastRun.runFastSimulation timeout targetCycle fs |> ignore
        sw.Stop()

        sprintf
            """{"clockTick":%d,"done":%b,"ms":%.2f}"""
            fs.ClockTick
            (fs.ClockTick >= targetCycle)
            sw.Elapsed.TotalMilliseconds

/// The digest text for the design under the deterministic stimulus: builds its own simulation
/// (SimDigest fixes the array size so the text is identical wherever it is computed), so it
/// neither needs nor disturbs the session. The reply is the raw render text on success - the
/// caller distinguishes an error by the leading '{'.
let digest (design: SimpleDesign) (ticks: int) : string =
    let ldcs = SimpleDesignShim.designToLoadedComponents design

    match SimDigest.render ldcs design.TopSheet ticks with
    | Ok text -> text
    | Error e -> errorReply e

/// Drop the session so its (potentially large) step arrays can be collected.
let endSession () : string =
    session <- None
    """{"ended":true}"""
