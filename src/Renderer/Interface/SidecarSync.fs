/// Asking the .NET simulator a question and waiting for the answer, inside a render.
///
/// **The synchronous half of the protocol.** Everything else the renderer asks of the sidecar is
/// an operation with an answer that arrives as a message. This is the other kind: a function that
/// returns a value, called from `view`, where nothing can await.
///
/// **Why it is a second transport.** The WebSocket cannot do this and never will: a browser socket
/// delivers through the event loop, so blocking the renderer's one thread to wait on it would stop
/// the very delivery that would unblock it. A blocking XMLHttpRequest does not go through the
/// event loop. It also cannot receive binary - `responseType` may not be set on a synchronous
/// request - so this path is text, which is the right shape for it anyway: small questions with
/// small answers. Bulk data stays where it is.
///
/// **When it may be used.** Only when nothing is in flight - `ModelHelpers.sidecarIsBusy` is
/// false. That is the whole of the arbitration, and it is enough: two synchronous calls cannot
/// overlap, because one runs to completion before anything else on this thread runs; and an
/// asynchronous operation is visible in the model. So a synchronous call never waits behind
/// anything, never interleaves with anything, and needs no priority, no queue and no lock at
/// either end. The caller must check, because the caller is the one holding the model.
///
/// **What it costs.** The renderer is stopped for the round trip. On loopback that is tens of
/// microseconds and is exactly what is wanted. It stops being what is wanted the moment the far
/// side is slow, and a caller cannot tell from inside - which is why the operations allowed here
/// are the ones bounded by construction (reading state that is already computed), never a build
/// and never a run.
module SidecarSync

open Fable.Core
open CommonTypes

/// A blocking GET, or None if it could not be made. The whole of the synchronous transport.
///
/// Emit rather than a binding because no Fable package wraps synchronous XHR - it is deprecated
/// on the main thread and browsers say so, which is fair warning for the general case and beside
/// the point for a loopback read of state already in memory.
[<Emit("""(function (url) {
    try {
        var x = new XMLHttpRequest();
        x.open('GET', url, false);
        x.send(null);
        return x.status === 200 ? x.responseText : null;
    } catch (e) {
        return null;
    }
})($0)""")>]
let private blockingGet (url: string) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private parseJson (text: string) : obj = jsNative

[<Emit("$0 == null")>]
let private isNullish (x: obj) : bool = jsNative

[<Emit("$0.values")>]
let private valuesOf (parsed: obj) : string array = jsNative

[<Emit("$0.error")>]
let private errorOf (parsed: obj) : string = jsNative

/// One signal to read: the component, the instance path it is in, and which of its output ports.
type SyncSignal =
    { SyncComp: ComponentId
      SyncPath: ComponentId list
      SyncPort: int }

/// The values of some signals at one cycle, or None.
///
/// None means the answer is not available - the sidecar is not there, the session has moved on, or
/// the cycle is outside what it holds. It is never a value: a caller showing "unknown" is right,
/// and a caller showing zero is wrong in a way nothing on screen would reveal.
let readAt (epoch: int) (cycle: int) (signals: SyncSignal list) : bigint list option =
    match SidecarClient.syncEndpoint () with
    | None -> None
    | Some(port, token) ->
        let query =
            signals
            |> List.map (fun s ->
                let (ComponentId comp) = s.SyncComp
                let path = s.SyncPath |> List.map (fun (ComponentId p) -> string p) |> String.concat "."
                // tilde and dot, not pipe: a pipe is a reserved character and Chromium refuses to
                // send a request whose query string holds one
                $"&s={comp}~{s.SyncPort}~{path}")
            |> String.concat ""

        match blockingGet $"http://127.0.0.1:{port}/read?token={token}&epoch={epoch}&cycle={cycle}{query}" with
        | null -> None
        | text ->
            let parsed = parseJson text

            if not (isNullish (errorOf parsed)) then
                Log.dbg Log.Sim $"a synchronous read of the .NET simulator: {errorOf parsed}"
                None
            else
                let values = valuesOf parsed

                if isNullish values then
                    None
                else
                    values |> Array.toList |> List.map (fun (v: string) -> bigint.Parse v) |> Some
