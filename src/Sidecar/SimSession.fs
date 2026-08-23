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

open System
open CommonTypes

/// The built simulation and its top-level inputs (id, label, width) - inputs kept so that
/// SimSetInputs can look up widths without re-deriving them.
let mutable private session: (SimTypes.FastSimulation * (ComponentId * ComponentLabel * int) list) option = None

/// Which session this is: bumped by every successful build, and never reused.
///
/// The renderer cannot see inside this process, so everything it believes about the session here -
/// that one exists, that it is of the design it last sent, how far its clock has run - is a belief
/// with nothing to check it against. A build that failed after the renderer thought it succeeded, a
/// sidecar restarted underneath it, a reply from a design that has since been replaced: all of them
/// look exactly like agreement.
///
/// So every command that depends on a session says which session it means, and this is what it is
/// checked against. One integer turns three unverifiable beliefs into one exact test: there is no
/// case where the epochs match and the two sides do not.
///
/// Zero is "no session", and no build ever issues it, so a renderer that has not built cannot name
/// one by accident.
let mutable private epoch = 0

let private escape (text: string) =
    text.Replace("\\", "/").Replace("\"", "'").Replace("\n", " ").Replace("\r", " ")

let private errorReply (message: string) =
    sprintf """{"error":"%s"}""" (escape message)

/// The epoch a session-dependent command must carry, or 0 when there is no session.
let currentEpoch () = if session.IsSome then epoch else 0

/// The earliest cycle whose data is still correct.
///
/// The step arrays are a circular buffer - stepIndexOf is numStep % maxArraySize - so a simulation
/// run past its array length has overwritten its own beginning. The waveform simulator sizes its
/// array for the whole configured run and never reaches that; the step simulator is sized short and
/// does. The renderer cannot work this out for itself without knowing both numbers, and over a wire
/// "overwritten" and "not yet reached" would otherwise arrive as the same silence.
let private firstValidCycle (fs: SimTypes.FastSimulation) =
    max 0 (fs.ClockTick - fs.MaxArraySize + 1)

/// Ok when a command names the session that exists, an error reply when it does not.
///
/// The message says both numbers because the two failures need telling apart: 0 from a caller that
/// has not built, against a stale number from one whose session was replaced underneath it.
let private checkEpoch (asked: int) =
    if asked = currentEpoch () then
        Ok()
    else
        Error(errorReply $"stale session: command names epoch {asked}, this sidecar holds {currentEpoch ()}")

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
            session <- Some(fs, simData.Inputs)
            // only a build that produced a session takes the next number, so an epoch always names
            // a simulation that once existed
            epoch <- epoch + 1

            sprintf
                """{"epoch":%d,"sheet":"%s","components":%d,"maxArraySize":%d,"buildMs":%.2f}"""
                epoch
                (escape design.TopSheet)
                (fs.FComps.Count + fs.FCustomComps.Count)
                fs.MaxArraySize
                sw.Elapsed.TotalMilliseconds

/// Run the session's simulation towards `targetCycle`, giving up after `timeoutMs` (0 = no
/// budget). The reply says where the clock got to; the caller repeats until done - each call
/// is one SimLog record, mirroring the renderer's progress loop.
let run (askedEpoch: int) (targetCycle: int) (timeoutMs: int) : string =
    match checkEpoch askedEpoch, session with
    | Error reply, _ -> reply
    | _, None -> errorReply "no simulation built - send SimBuild first"
    | Ok(), Some(fs, _) ->
        let timeout = if timeoutMs <= 0 then None else Some(float timeoutMs)
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let outcome = FastRun.runFastSimulation timeout targetCycle fs
        sw.Stop()

        // The run says whether it finished; the clock is reported beside it rather than compared
        // against the target to work the same thing out a second way. A budget that ran out loses
        // nothing - the next call continues from where this one reached.
        sprintf
            """{"epoch":%d,"clockTick":%d,"firstValidCycle":%d,"done":%b,"ms":%.2f}"""
            epoch
            fs.ClockTick
            (firstValidCycle fs)
            (outcome = SimTypes.RunCompleted)
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

/// Drop the session because a new design is arriving, whatever session that is.
///
/// A design is only ever sent with every simulation closed - Start and Refresh both do it on a
/// closed one, Refresh by stopping first - so this cannot take a session out from under a caller
/// that is using it. It is here so that the sidecar's state says the same thing: between a design
/// arriving and the build that follows it there is no session, so a command left over from before
/// the design changed names an epoch that no longer exists and is refused, rather than being
/// answered from a simulation of a design that has been replaced.
///
/// The epoch counter is untouched, so the next build takes a number never used before.
let discardForNewDesign () = session <- None

/// Drop the session so its (potentially large) step arrays can be collected.
let endSession (askedEpoch: int) : string =
    match checkEpoch askedEpoch with
    | Error reply -> reply
    | Ok() ->
        session <- None
        """{"ended":true}"""

// ---------------------------------------------------------------------------------------------
// Binary step data. Payload layouts are in Protocol.fs; both functions take the raw command
// payload so the tests exercise the exact wire format.
// ---------------------------------------------------------------------------------------------

let private u32 (body: byte array) (offset: int) =
    if body.Length >= offset + 4 then int (BitConverter.ToUInt32(body, offset)) else 0

/// Set top-level input values at a cycle: uint32 cycle, uint32 count, then per input uint32
/// component id + value as two uint32 words. The value reaches the simulator through the same
/// changeInput call the renderer uses, masked to the input's width by FastData construction.
let setInputs (askedEpoch: int) (body: byte array) : string =
    match checkEpoch askedEpoch, session with
    | Error reply, _ -> reply
    | _, None -> errorReply "no simulation built - send SimBuild first"
    | Ok(), Some(fs, inputs) ->
        let cycle = u32 body 0
        let count = u32 body 4

        let widthOf compId =
            inputs
            |> List.tryPick (fun (ComponentId cid, _, width) -> if cid = compId then Some width else None)

        let rec apply i offset =
            if i >= count then
                sprintf """{"cycle":%d,"inputsSet":%d}""" cycle count
            else
                let compId = u32 body offset
                let value = bigint (u32 body (offset + 4)) + (bigint (u32 body (offset + 8)) <<< 32)

                match widthOf compId with
                | None -> errorReply $"component {compId} is not a top-level input of the simulation"
                | Some width ->
                    let fd = NumberHelpers.convertBigintToFastData width value
                    FastExtract.changeInput (ComponentId compId) (SimGraphTypes.IData fd) cycle fs
                    apply (i + 1) (offset + 12)

        apply 0 8

/// Read sampled output data: for each signal, `samples` values taken every `rep` cycles from
/// `start` - the (StartCycle, SamplingZoom, ShownCycles) triple the waveform viewer's own
/// generation runs on, so one request serves a view at any zoom, and a tooltip is the
/// one-signal one-sample special case. The reply payload is uint32 signal count, uint32 sample
/// count, then signal-major uint32 values - which the renderer views as a Uint32Array with no
/// copy, the reason the frame header is 8 bytes. Errors come back as JSON text instead.
let read (askedEpoch: int) (body: byte array) : Result<byte array, string> =
    match checkEpoch askedEpoch, session with
    | Error _, _ ->
        Error $"stale session: command names epoch {askedEpoch}, this sidecar holds {currentEpoch ()}"
    | _, None -> Error "no simulation built - send SimBuild first"
    | Ok(), Some(fs, _) ->
        let startCycle = u32 body 0
        let rep = u32 body 4
        let samples = u32 body 8
        let count = u32 body 12

        let signals =
            (16, List.init count id)
            ||> List.mapFold (fun offset _ ->
                let compId = u32 body offset
                let outPort = u32 body (offset + 4)
                let pathLen = u32 body (offset + 8)
                let path = List.init pathLen (fun i -> ComponentId(u32 body (offset + 12 + 4 * i)))
                (ComponentId compId, path, outPort), offset + 12 + 4 * pathLen)
            |> fst

        let lastWanted = startCycle + (samples - 1) * rep

        if rep < 1 || samples <= 0 || count <= 0 then
            Error "SimRead needs rep >= 1 and at least one sample and one signal"
        elif fs.ClockTick < lastWanted then
            Error $"simulation has only reached cycle {fs.ClockTick} - run to {lastWanted} first"
        elif lastWanted - startCycle >= fs.MaxArraySize || fs.ClockTick - startCycle >= fs.MaxArraySize then
            Error $"cycles {startCycle}..{lastWanted} are outside the {fs.MaxArraySize}-entry circular buffer"
        else
            let reply = Array.zeroCreate (8 + 4 * count * samples)
            BitConverter.GetBytes(uint32 count).CopyTo(reply, 0)
            BitConverter.GetBytes(uint32 samples).CopyTo(reply, 4)

            let mutable error = None

            signals
            |> List.iteri (fun signalIndex (cid, path, outPort) ->
                for j in 0 .. samples - 1 do
                    if error.IsNone then
                        let cycle = startCycle + j * rep

                        match FastExtract.extractFastSimulationOutput fs cycle (cid, path) (OutputPortNumber outPort) with
                        | SimGraphTypes.IData fd when fd.Width <= 32 ->
                            let offset = 8 + 4 * (signalIndex * samples + j)
                            BitConverter.GetBytes(uint32 fd.GetBigInt).CopyTo(reply, offset)
                        | SimGraphTypes.IData fd ->
                            error <- Some $"signal on component %A{cid} is {fd.Width} bits wide - only widths up to 32 are readable for now"
                        | SimGraphTypes.IAlg _ ->
                            error <- Some "algebraic values cannot be read")

            match error with
            | Some e -> Error e
            | None -> Ok reply
