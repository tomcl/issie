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
            session <- Some(fs, simData.Inputs)

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
    | Some(fs, _) ->
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

// ---------------------------------------------------------------------------------------------
// Binary step data. Payload layouts are in Protocol.fs; both functions take the raw command
// payload so the tests exercise the exact wire format.
// ---------------------------------------------------------------------------------------------

let private u32 (body: byte array) (offset: int) =
    if body.Length >= offset + 4 then int (BitConverter.ToUInt32(body, offset)) else 0

/// Set top-level input values at a cycle: uint32 cycle, uint32 count, then per input uint32
/// component id + value as two uint32 words. The value reaches the simulator through the same
/// changeInput call the renderer uses, masked to the input's width by FastData construction.
let setInputs (body: byte array) : string =
    match session with
    | None -> errorReply "no simulation built - send SimBuild first"
    | Some(fs, inputs) ->
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

/// Read a window of output step data: the reply payload is uint32 item count, uint32 cycle
/// count, then item-major uint32 values - which the renderer views as a Uint32Array with no
/// copy, the reason the frame header is 8 bytes. Errors come back as JSON text instead.
let read (body: byte array) : Result<byte array, string> =
    match session with
    | None -> Error "no simulation built - send SimBuild first"
    | Some(fs, _) ->
        let startCycle = u32 body 0
        let cycles = u32 body 4
        let count = u32 body 8

        let items =
            (12, List.init count id)
            ||> List.mapFold (fun offset _ ->
                let compId = u32 body offset
                let outPort = u32 body (offset + 4)
                let pathLen = u32 body (offset + 8)
                let path = List.init pathLen (fun i -> ComponentId(u32 body (offset + 12 + 4 * i)))
                (ComponentId compId, path, outPort), offset + 12 + 4 * pathLen)
            |> fst

        let lastWanted = startCycle + cycles - 1

        if cycles <= 0 || count <= 0 then
            Error "SimRead needs at least one cycle and one item"
        elif fs.ClockTick < lastWanted then
            Error $"simulation has only reached cycle {fs.ClockTick} - run to {lastWanted} first"
        elif lastWanted - startCycle >= fs.MaxArraySize || fs.ClockTick - startCycle >= fs.MaxArraySize then
            Error $"cycles {startCycle}..{lastWanted} are outside the {fs.MaxArraySize}-entry circular buffer"
        else
            let reply = Array.zeroCreate (8 + 4 * count * cycles)
            BitConverter.GetBytes(uint32 count).CopyTo(reply, 0)
            BitConverter.GetBytes(uint32 cycles).CopyTo(reply, 4)

            let mutable error = None

            items
            |> List.iteri (fun itemIndex (cid, path, outPort) ->
                for c in 0 .. cycles - 1 do
                    if error.IsNone then
                        match FastExtract.extractFastSimulationOutput fs (startCycle + c) (cid, path) (OutputPortNumber outPort) with
                        | SimGraphTypes.IData fd when fd.Width <= 32 ->
                            let offset = 8 + 4 * (itemIndex * cycles + c)
                            BitConverter.GetBytes(uint32 fd.GetBigInt).CopyTo(reply, offset)
                        | SimGraphTypes.IData fd ->
                            error <- Some $"signal on component %A{cid} is {fd.Width} bits wide - only widths up to 32 are readable for now"
                        | SimGraphTypes.IAlg _ ->
                            error <- Some "algebraic values cannot be read")

            match error with
            | Some e -> Error e
            | None -> Ok reply
