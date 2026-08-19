/// The renderer's half of the sidecar transport: connect, then correlated request -> response.
///
/// Wire protocol, shared with src/Sidecar/Protocol.fs - the two files change together:
///
///     byte 0        command; a response carries the request's command with bit 7 set
///     bytes 1..4    correlation id, uint32 little-endian, echoed back by the sidecar
///     bytes 5..     payload
///
/// The socket is the browser WebSocket global: the renderer is sandboxed and a browser API is
/// what it has - which is also why no npm package is involved. Where the socket points comes
/// from Bridge.sidecarEndpoint, polled at the moment of connecting.
///
/// Frames are Uint8Array over ArrayBuffer, reached only through the Emit helpers below: sizes
/// and correlation ids stay as plain floats (JS numbers), and nothing indexes a typed array
/// through an F# array type.
module SidecarClient

open Fable.Core
open Fable.Core.JsInterop

module Constants =
    [<Literal>]
    let echoCmd = 1

    [<Literal>]
    let uploadCmd = 2

    [<Literal>]
    let downloadCmd = 3

    /// Command byte plus the four bytes of correlation id.
    [<Literal>]
    let headerSize = 5

// ---- the WebSocket API, by Emit: this project has no Fable.Browser.WebSocket binding ----

[<Emit("new WebSocket($0)")>]
let private newWebSocket (url: string) : obj = jsNative

[<Emit("$0.binaryType = 'arraybuffer'")>]
let private setBinaryTypeArrayBuffer (ws: obj) : unit = jsNative

[<Emit("$1.onopen = ($0)")>]
let private setOnOpen (callback: obj -> unit) (ws: obj) : unit = jsNative

[<Emit("$1.onmessage = ($0)")>]
let private setOnMessage (callback: obj -> unit) (ws: obj) : unit = jsNative

[<Emit("$1.onerror = ($0)")>]
let private setOnError (callback: obj -> unit) (ws: obj) : unit = jsNative

[<Emit("$1.onclose = ($0)")>]
let private setOnClose (callback: obj -> unit) (ws: obj) : unit = jsNative

[<Emit("$0.send($1)")>]
let private wsSend (ws: obj) (frame: obj) : unit = jsNative

[<Emit("$0.close()")>]
let private wsClose (ws: obj) : unit = jsNative

// ---- frames as Uint8Array ----

[<Emit("new Uint8Array($0)")>]
let makeBytes (size: int) : obj = jsNative

[<Emit("$0.length")>]
let byteLength (bytes: obj) : float = jsNative

/// Write a uint32 little-endian, as the wire wants it, at a byte offset.
[<Emit("new DataView($0.buffer, $0.byteOffset).setUint32($1, $2, true)")>]
let writeUint32At (bytes: obj) (offset: int) (value: float) : unit = jsNative

[<Emit("new Uint8Array($0.data)")>]
let private eventBytes (messageEvent: obj) : obj = jsNative

[<Emit("$0[0] = $1")>]
let private setCommand (frame: obj) (cmd: int) : unit = jsNative

[<Emit("$0[0]")>]
let private commandOf (frame: obj) : int = jsNative

[<Emit("$0.set($1, 5)")>]
let private blitPayload (frame: obj) (payload: obj) : unit = jsNative

[<Emit("new DataView($0.buffer, $0.byteOffset).getUint32(1, true)")>]
let private readCorrId (frame: obj) : float = jsNative

// Connection state and in-flight requests. Live socket handles, not model state
// (docs/mutableState.md) - the Elmish model never sees the socket, only promises.
let mutable private socket: obj option = None
let private pending = System.Collections.Generic.Dictionary<float, obj -> unit>()
let mutable private nextCorrId = 0.0

/// Resolves once the socket is open; rejects when the sidecar is not up (yet). One socket per
/// renderer: a second connect while one is open resolves immediately.
let connect () : JS.Promise<unit> =
    Promise.create (fun resolve reject ->
        match socket, Bridge.sidecarEndpoint () with
        | Some _, _ -> resolve ()
        | None, None ->
            reject (System.Exception "sidecar is not running (or not listening yet) - try again")
        | None, Some (port, token) ->
            let ws = newWebSocket $"ws://127.0.0.1:{port}/?token={token}"
            setBinaryTypeArrayBuffer ws

            setOnOpen (fun _ -> socket <- Some ws; resolve ()) ws

            setOnError
                (fun _ ->
                    socket <- None
                    reject (System.Exception "sidecar websocket failed"))
                ws

            // in-flight requests can never complete now; forget them rather than leak them
            setOnClose (fun _ -> socket <- None; pending.Clear()) ws

            setOnMessage
                (fun ev ->
                    let frame = eventBytes ev
                    let corrId = readCorrId frame

                    match pending.TryGetValue corrId with
                    | true, resolveRequest ->
                        pending.Remove corrId |> ignore
                        resolveRequest frame
                    | false, _ -> Log.error $"sidecar: unmatched response, command {commandOf frame}")
                ws)

/// One request, resolved with the whole response frame - header included, so a caller can size
/// what came back. The payload is a Uint8Array from makeBytes.
let request (cmd: int) (payload: obj) : JS.Promise<obj> =
    Promise.create (fun resolve reject ->
        match socket with
        | None -> reject (System.Exception "sidecar is not connected - connect first")
        | Some ws ->
            nextCorrId <- nextCorrId + 1.0
            let frame = makeBytes (Constants.headerSize + int (byteLength payload))
            setCommand frame cmd
            writeUint32At frame 1 nextCorrId
            blitPayload frame payload
            pending[nextCorrId] <- resolve
            wsSend ws frame)

/// Drop the connection; the next connect () makes a fresh one.
let disconnect () =
    socket |> Option.iter wsClose
    socket <- None
