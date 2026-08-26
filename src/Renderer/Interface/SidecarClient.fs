/// The renderer's half of the sidecar transport: connect, then correlated request -> response.
///
/// Wire protocol, shared with src/Sidecar/Protocol.fs - the two files change together:
///
///     byte 0        command; a response carries the request's command with bit 7 set
///     bytes 1..4    correlation id, uint32 little-endian, echoed back by the sidecar
///     bytes 5..7    padding, always zero: an 8-byte header means a binary response payload
///                   starts 8-aligned, so Uint32Array/Float64Array views need no copy
///     bytes 8..     payload
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

    /// payload: UTF-8 JSON of a CommonTypes.SimpleDesign; response payload: a small JSON report
    [<Literal>]
    let sendDesignCmd = 4

    // the simulation session - argument layouts in src/Sidecar/Protocol.fs
    [<Literal>]
    let simBuildCmd = 5

    [<Literal>]
    let simRunCmd = 6

    [<Literal>]
    let simDigestCmd = 7

    [<Literal>]
    let simEndCmd = 8

    [<Literal>]
    let simLogCmd = 9

    [<Literal>]
    let simSetInputsCmd = 10

    [<Literal>]
    let simReadCmd = 11
    let simReadRamCmd = 12
    let simPortsCmd = 13
    let simReadDriversCmd = 14

    /// Command byte, uint32 correlation id, three bytes of padding - 8, so binary response
    /// payloads start 8-aligned for zero-copy typed-array views.
    [<Literal>]
    let headerSize = 8

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

[<Emit("$0.set($1, 8)")>]
let private blitPayload (frame: obj) (payload: obj) : unit = jsNative

[<Emit("new DataView($0.buffer, $0.byteOffset).getUint32(1, true)")>]
let private readCorrId (frame: obj) : float = jsNative

// Connection state and in-flight requests. Live socket handles, not model state
// (docs/mutableState.md) - the Elmish model never sees the socket, only promises.
let mutable private socket: obj option = None

/// A request that has been sent and not yet answered.
///
/// It carries how to fail it as well as how to resolve it, because a request that can never be
/// answered has to be FAILED rather than forgotten - see the close handler below. And it carries
/// when it was sent, because a reply that never comes is the one failure nothing else reports:
/// every other kind arrives as an error.
type private Pending =
    { Cmd: int
      SentAtMs: float
      /// said once, not once per event afterwards
      Warned: bool
      Resolve: obj -> unit
      Fail: exn -> unit }

let private pending = System.Collections.Generic.Dictionary<float, Pending>()

let mutable private nextCorrId = 0.0

/// How long a command may take before something is wrong, or None for one declared long.
///
/// Not a latency budget - a round trip is under a millisecond and a run chunk is a tenth of a
/// second. This is the line past which a command is not slow but stuck, so one generous number
/// serves every bounded command and the interesting part is the exemptions.
///
/// `SimBuild` and `SimDigest` are the two declared-long commands: a build has no cycle loop to
/// bound it, and a digest builds and runs a simulation of its own for the two runtimes to be
/// compared byte for byte. The three measurement commands can be asked for 64MB. See
/// docs/dev/sidecarInvariants.md, section E.
let private budgetMs (cmd: int) : float option =
    match cmd with
    | c when c = Constants.simBuildCmd -> None
    | c when c = Constants.simDigestCmd -> None
    | c when c = Constants.echoCmd || c = Constants.uploadCmd || c = Constants.downloadCmd -> None
    | _ -> Some 2000.0

/// What to call a command in a diagnostic. The numbers are in Protocol.fs and nobody reading a
/// warning has them by heart.
let private nameOf (cmd: int) =
    match cmd with
    | c when c = Constants.echoCmd -> "Echo"
    | c when c = Constants.uploadCmd -> "Upload"
    | c when c = Constants.downloadCmd -> "Download"
    | c when c = Constants.sendDesignCmd -> "SendDesign"
    | c when c = Constants.simBuildCmd -> "SimBuild"
    | c when c = Constants.simRunCmd -> "SimRun"
    | c when c = Constants.simDigestCmd -> "SimDigest"
    | c when c = Constants.simEndCmd -> "SimEnd"
    | c when c = Constants.simLogCmd -> "SimLog"
    | c when c = Constants.simSetInputsCmd -> "SimSetInputs"
    | c when c = Constants.simReadCmd -> "SimRead"
    | c when c = Constants.simReadRamCmd -> "SimReadRam"
    | c -> $"command {c}"

/// Say so about any request that has outstripped its command's budget.
///
/// Timestamps against the clock, checked when something happens - a request going out, a reply
/// coming in - rather than on a timer. Nothing is scheduled and nothing is counted: the table
/// already holds when each request was sent, so this is a comparison over at most a handful of
/// entries. An app with nothing happening reports nothing, which costs nothing, because there is
/// also nobody being misled by it.
let private reportOverdue () =
    let now = TimeHelpers.getTimeMs ()

    let late =
        pending
        |> Seq.filter (fun kv ->
            not kv.Value.Warned
            && (match budgetMs kv.Value.Cmd with
                | Some budget -> now - kv.Value.SentAtMs > budget
                | None -> false))
        |> Seq.map (fun kv -> kv.Key, kv.Value)
        |> List.ofSeq

    late
    |> List.iter (fun (corrId, entry) ->
        Log.warn
            $"sidecar {nameOf entry.Cmd} has not answered in %.0f{now - entry.SentAtMs}ms (invariant A6)"

        pending[corrId] <- { entry with Warned = true })

/// How long to wait for the sidecar to start listening before deciding it is not going to.
///
/// It is spawned with the app and prints its port when it is ready. Until then the endpoint is not
/// missing, it is NOT YET - and from a development build "not yet" is tens of seconds, because the
/// sidecar is built first. A transport that reports the difference as a failure makes every caller
/// above it responsible for a startup ordering problem it cannot see: the waveform viewer sat empty
/// and had to be told to ask again.
let private startupBudgetMs = 60000.

/// The sidecar's endpoint, waiting for it to appear if it has not yet.
///
/// Polled, because nothing pushes it: the main process learns the port from the sidecar's own
/// output and answers with it when asked. A hundred milliseconds is far below the time any of this
/// takes and far above the cost of asking.
let rec private endpoint (deadline: float) : JS.Promise<int * string> =
    match Bridge.sidecarEndpoint () with
    | Some ep -> Promise.lift ep
    | None when TimeHelpers.getTimeMs () > deadline ->
        Promise.create (fun _ reject ->
            reject (System.Exception $"the sidecar did not start listening within %.0f{startupBudgetMs / 1000.}s"))
    | None ->
        Promise.create (fun resolve _ -> JS.setTimeout (fun () -> resolve ()) 100 |> ignore)
        |> Promise.bind (fun () -> endpoint deadline)

/// Resolves once the socket is open, WAITING for the sidecar to be listening if it is still
/// starting. One socket per renderer: a second connect while one is open resolves immediately.
///
/// Rejects only when the sidecar is genuinely not there - it never started, or it has died - which
/// is a fault and is reported as one. That distinction is the point of the wait above: everything
/// over this is written as though the sidecar is simply there, because by the time it is called it
/// is.
let connect () : JS.Promise<unit> =
    match socket with
    | Some _ -> Promise.lift ()
    | None ->
        endpoint (TimeHelpers.getTimeMs () + startupBudgetMs)
        |> Promise.bind (fun (port, token) ->
            Promise.create (fun resolve reject ->
                let ws = newWebSocket $"ws://127.0.0.1:{port}/?token={token}"
                setBinaryTypeArrayBuffer ws

                setOnOpen (fun _ -> socket <- Some ws; resolve ()) ws

                setOnError
                    (fun _ ->
                        socket <- None
                        reject (System.Exception "sidecar websocket failed"))
                    ws

                // Every in-flight request now has no possible answer, so every caller waiting on one
                // is FAILED. Clearing the table instead dropped the resolvers without calling them,
                // and a promise that neither resolves nor rejects makes "still working" and "gone"
                // the same thing to everything above it - which is the shape of hang that is hardest
                // to find, because nothing anywhere reports it.
                setOnClose
                    (fun _ ->
                        socket <- None
                        let dropped = List.ofSeq pending.Values
                        pending.Clear()

                        if not (List.isEmpty dropped) then
                            Log.warn $"sidecar connection closed with {dropped.Length} requests unanswered"

                        dropped
                        |> List.iter (fun entry -> entry.Fail (System.Exception "the sidecar connection closed")))
                    ws

                setOnMessage
                    (fun ev ->
                        let frame = eventBytes ev
                        let corrId = readCorrId frame

                        match pending.TryGetValue corrId with
                        | true, entry ->
                            pending.Remove corrId |> ignore

                            // a reply that came, but too slowly to be one of the bounded commands the
                            // protocol says this is - reported as well as the ones that never come,
                            // because it is the same invariant and this is the half that is testable
                            let took = TimeHelpers.getTimeMs () - entry.SentAtMs

                            match budgetMs entry.Cmd with
                            | Some budget when took > budget && not entry.Warned ->
                                Log.warn
                                    $"sidecar {nameOf entry.Cmd} answered after %.0f{took}ms, past its %.0f{budget}ms budget (invariant A6)"
                            | _ -> ()

                            reportOverdue ()
                            entry.Resolve frame
                        | false, _ -> Log.error $"sidecar: unmatched response, command {commandOf frame}")
                    ws))

/// One request, resolved with the whole response frame - header included, so a caller can size
/// what came back. The payload is a Uint8Array from makeBytes.
let request (cmd: int) (payload: obj) : JS.Promise<obj> =
    Promise.create (fun resolve reject ->
        match socket with
        | None -> reject (System.Exception "sidecar is not connected - connect first")
        | Some ws ->
            nextCorrId <- nextCorrId + 1.0

            if pending.ContainsKey nextCorrId then
                // ids only ever increase, and are floats, so this cannot happen in any real
                // session - but reusing one in flight would deliver a reply to the wrong caller,
                // which is the kind of wrong that looks like a simulation bug
                Log.error $"sidecar: correlation id {nextCorrId} is already in flight"

            let frame = makeBytes (Constants.headerSize + int (byteLength payload))
            setCommand frame cmd
            writeUint32At frame 1 nextCorrId
            blitPayload frame payload

            pending[nextCorrId] <-
                { Cmd = cmd
                  SentAtMs = TimeHelpers.getTimeMs ()
                  Warned = false
                  Resolve = resolve
                  Fail = reject }

            reportOverdue ()
            wsSend ws frame)

/// Whether a socket is open, and how many requests are waiting on the sidecar.
///
/// For the development harness, which is the only thing that asks: "is the sidecar there yet" is
/// otherwise unanswerable from outside, and the honest answer is what tells a test that has just
/// started the app to wait rather than to conclude the simulator is broken.
let connectionState () = Option.isSome socket, pending.Count

/// Drop the connection; the next connect () makes a fresh one.
let disconnect () =
    socket |> Option.iter wsClose
    socket <- None

// ---- text payloads: UTF-8 strings over the same binary frames ----

[<Emit("new TextEncoder().encode($0)")>]
let private encodeText (text: string) : obj = jsNative

// the subarray skips the 8-byte header (Constants.headerSize, which an Emit cannot name)
[<Emit("new TextDecoder().decode($0.subarray(8))")>]
let private decodeTextPayload (frame: obj) : string = jsNative

[<Emit("$0.set($1, $2)")>]
let private blitAt (target: obj) (source: obj) (offset: int) : unit = jsNative

/// Strings packed as SendDesign wants them: for each, a uint32 LE byte length then its UTF-8
/// bytes. The sidecar's parsing half is DesignCache.parsePayload; the two change together.
/// Local mutation only - the offset cursor never escapes.
let private packStrings (strings: string list) : obj =
    let encoded = strings |> List.map encodeText
    let total = encoded |> List.sumBy (fun bytes -> 4 + int (byteLength bytes))
    let payload = makeBytes total
    let mutable offset = 0

    for bytes in encoded do
        writeUint32At payload offset (byteLength bytes)
        blitAt payload bytes (offset + 4)
        offset <- offset + 4 + int (byteLength bytes)

    payload

/// Send a design as ONE MESSAGE PER SHEET: which sheet of how many, then the top sheet's name
/// and that sheet's JSON.
///
/// Per sheet because decoding is the cost and it happens on the sidecar's serve loop, which
/// serves one message at a time - so a whole design in one message is one handler holding that
/// loop for ~300ms on 3cpu, against ~25ms for its largest single sheet. Per-sheet framing also
/// lets the sidecar reuse sheets it has already decoded, since an unchanged sheet serialises to
/// the identical string.
///
/// Sent in order, awaited one at a time. The sheets are a design only once the last has landed,
/// which is what the reply's `complete` says; sending them at once would arrive in any order and
/// give the sidecar no way to know when it had them all.
///
/// A design is only ever sent with every simulation closed - Start and Refresh both do it on a
/// closed one - so an upload never races a session. The sidecar drops whatever session it holds
/// when the first sheet arrives, so a command left over from before the design changed names an
/// epoch that no longer exists.
///
/// Resolves with the last reply, or stops at the first error and resolves with that.
let sendDesign (topSheet: string) (sheetJsons: string list) : JS.Promise<string> =
    let count = List.length sheetJsons

    let sendOne (index: int) (json: string) : JS.Promise<string> =
        let strings = packStrings [ topSheet; json ]
        let payload = makeBytes (8 + int (byteLength strings))
        writeUint32At payload 0 (float index)
        writeUint32At payload 4 (float count)
        blitAt payload strings 8
        request Constants.sendDesignCmd payload |> Promise.map decodeTextPayload

    let rec sendFrom (index: int) (remaining: string list) : JS.Promise<string> =
        match remaining with
        | [] -> Promise.lift (sprintf "{\"error\":\"a design with no sheets\"}")
        | [ last ] -> sendOne index last
        | json :: rest ->
            sendOne index json
            |> Promise.bind (fun reply ->
                if reply.StartsWith "{\"error" then
                    Promise.lift reply
                else
                    sendFrom (index + 1) rest)

    sendFrom 0 sheetJsons

/// A request whose payload is uint32 LE arguments and whose reply is text.
let private requestArgs (cmd: int) (args: int list) : JS.Promise<string> =
    let payload = makeBytes (4 * List.length args)
    args |> List.iteri (fun i value -> writeUint32At payload (4 * i) (float value))
    request cmd payload |> Promise.map decodeTextPayload

/// Build a simulation of the last-sent design's top sheet on the sidecar.
let simBuild (maxArraySize: int) = requestArgs Constants.simBuildCmd [ maxArraySize ]

/// The session epoch a build reply issued, or 0 if it issued none.
///
/// Zero for an error reply and for anything unparseable, which is the safe direction: a command
/// naming epoch 0 is refused by the sidecar unless there genuinely is no session, so a caller that
/// failed to build cannot go on to name one.
[<Emit("(function (t) { try { return JSON.parse(t).epoch || 0 } catch (e) { return 0 } })($0)")>]
let epochOf (buildReply: string) : int = jsNative

/// Advance the sidecar's simulation towards a cycle within a millisecond budget (0 = none);
/// the reply says where the clock got to. Chunk by repeating; cancel by stopping.
///
/// Every command that depends on a session names the session it means, and the sidecar refuses one
/// that names any other - see SimSession.checkEpoch. That is what stops a reply from a superseded
/// simulation being taken for a reply from this one.
let simRun (epoch: int) (targetCycle: int) (timeoutMs: int) =
    requestArgs Constants.simRunCmd [ epoch; targetCycle; timeoutMs ]

/// The sidecar's deterministic-stimulus digest text for the last-sent design (an error reply
/// starts with '{').
let simDigest (ticks: int) = requestArgs Constants.simDigestCmd [ ticks ]

let simEnd (epoch: int) = requestArgs Constants.simEndCmd [ epoch ]

/// The sidecar's SimLog ring as JSON - the .NET half of a cross-runtime cost comparison.
let simLog () = requestArgs Constants.simLogCmd []

// ---- binary step data: the point of the 8-byte header ----

/// Read a uint32 little-endian at a byte offset of a frame.
[<Emit("new DataView($0.buffer, $0.byteOffset).getUint32($1, true)")>]
let readUint32At (bytes: obj) (offset: int) : float = jsNative

/// How many uint32 words each sample of a SimRead reply occupies - ceil(widest signal / 32), so
/// one for a reply of ordinary buses. At byte 16 of the frame: 8 of frame header, then the signal
/// and sample counts.
[<Emit("new DataView($0.buffer, $0.byteOffset).getUint32(16, true)")>]
let simReadWordsPerSample (frame: obj) : int = jsNative

/// A zero-copy Uint32Array view over `count` words starting at byte 24 of a SimRead response
/// frame - 8 of frame header, three uint32 counts and four bytes of padding, which is what keeps
/// the values 8-aligned.
[<Emit("new Uint32Array($0.buffer, $0.byteOffset + 24, $1)")>]
let viewSimReadData (frame: obj) (count: int) : obj = jsNative

/// Read an element of a Uint32Array view.
[<Emit("$0[$1]")>]
let uint32At (view: obj) (index: int) : float = jsNative

/// Set top-level input values at a cycle: (component id, value) pairs, values up to 2^53
/// (split into low and high words on the wire). Reply is JSON.
let simSetInputs (epoch: int) (cycle: int) (values: (int * float) list) : JS.Promise<string> =
    let args =
        [ epoch; cycle; List.length values ]
        @ (values
           |> List.collect (fun (compId, value) ->
               let hi = System.Math.Floor(value / 4294967296.0)
               let lo = value - hi * 4294967296.0
               [ compId; int lo; int hi ]))

    requestArgs Constants.simSetInputsCmd args

/// THE waveform-data interface: for each signal - (component id, output port number, access
/// path root-first) - read `samples` values taken every `rep` cycles from `startCycle`. These
/// are the same (StartCycle, SamplingZoom, ShownCycles) parameters the waveform viewer's own
/// generation runs on, so a view at any zoom is one request. Resolves with the raw response
/// frame: on success the values are `viewSimReadData frame (signals * samples)`, signal-major
/// and zero-copy; an error response is JSON text (`decodeText frame` starts with '{').
let simRead
    (epoch: int)
    (startCycle: int)
    (rep: int)
    (samples: int)
    (signals: (int * int * int list) list)
    : JS.Promise<obj> =
    let args =
        [ epoch; startCycle; rep; samples; List.length signals ]
        @ (signals
           |> List.collect (fun (compId, outPort, path) -> [ compId; outPort; List.length path ] @ path))

    let payload = makeBytes (4 * List.length args)
    args |> List.iteri (fun i value -> writeUint32At payload (4 * i) (float value))
    request Constants.simReadCmd payload

/// SimRead by driver handle: the indices the port slice handed out, valid for this build.
/// Resolves with the raw response frame, exactly as `simRead` does - same reply layout, same
/// zero-copy view - so the two are interchangeable to everything downstream.
let simReadDrivers
    (epoch: int)
    (startCycle: int)
    (rep: int)
    (samples: int)
    (drivers: int list)
    : JS.Promise<obj> =
    let args = [ epoch; startCycle; rep; samples; List.length drivers ] @ drivers
    let payload = makeBytes (4 * List.length args)
    args |> List.iteri (fun i value -> writeUint32At payload (4 * i) (float value))
    request Constants.simReadDriversCmd payload

/// The response payload as text, for reading an error reply from a binary command.
let decodeText (frame: obj) : string = decodeTextPayload frame

/// One memory's contents at one clock, as a RAM table shows them.
///
/// `sparseUpTo` is the most non-zero locations worth listing; past that a window of `rows` from
/// `start` comes back instead, and zero asks for a window whatever the memory holds. Which of the
/// two arrives is the sidecar's decision - only it knows how much the memory holds - so the reply
/// says which it is and the caller draws accordingly.
let simReadRam
    (epoch: int)
    (cycle: int)
    (compId: int)
    (path: int list)
    (sparseUpTo: int)
    (start: bigint)
    (rows: int)
    : JS.Promise<Result<RamView.RamView, string>> =
    let lowWord (v: bigint) = int (v &&& 4294967295I)
    let highWord (v: bigint) = int ((v >>> 32) &&& 4294967295I)

    let args =
        [ epoch; cycle; compId; List.length path ]
        @ path
        @ [ sparseUpTo; lowWord start; highWord start; rows ]

    let payload = makeBytes (4 * List.length args)
    args |> List.iteri (fun i value -> writeUint32At payload (4 * i) (float value))

    request Constants.simReadRamCmd payload
    |> Promise.map (fun frame ->
        let asText = decodeTextPayload frame

        if asText.StartsWith "{" then
            Error asText
        else
            let isSparse = readUint32At frame 8 = 1.0
            let rowCount = int (readUint32At frame 12)
            let wordsPerValue = int (readUint32At frame 16)
            // 8 bytes of frame header, then the reply's own 16
            let rowBase i = 24 + (12 + 4 * wordsPerValue) * i

            let row i =
                let at = rowBase i
                let addr = bigint (readUint32At frame at) + (bigint (readUint32At frame (at + 4)) <<< 32)

                let value =
                    (0I, [ wordsPerValue - 1 .. -1 .. 0 ])
                    ||> List.fold (fun acc w -> (acc <<< 32) + bigint (readUint32At frame (at + 12 + 4 * w)))

                { RamView.Addr = addr
                  RamView.Value = value
                  RamView.Row =
                    match int (readUint32At frame (at + 8)) with
                    | 1 -> RamView.RAMRead
                    | 2 -> RamView.RAMWritten
                    | _ -> RamView.RAMNormal }

            let allRows = [ for i in 0 .. rowCount - 1 -> row i ]
            Ok(if isSparse then RamView.RamSparse allRows else RamView.RamWindow(start, allRows)))

/// Width and driver index of every port of every component on one instance's sheet - the wave
/// selector's read, made when its combo boxes pick an instance. Decoded into the SAME type the
/// renderer's own simulator answers with (PortView.sheetSliceOf), so everything downstream is
/// one code path and only the source of the bytes differs.
let simPorts (epoch: int) (path: int list) : JS.Promise<Result<PortView.ComponentSlots list, string>> =
    let args = [ epoch; List.length path ] @ path
    let payload = makeBytes (4 * List.length args)
    args |> List.iteri (fun i value -> writeUint32At payload (4 * i) (float value))

    request Constants.simPortsCmd payload
    |> Promise.map (fun frame ->
        let asText = decodeTextPayload frame

        if asText.StartsWith "{" then
            Error asText
        else
            // 8 bytes of frame header, then the layout Protocol.SimPorts states
            let compCount = int (readUint32At frame 8)
            let mutable at = 12

            let readU32 () =
                let v = int (readUint32At frame at)
                at <- at + 4
                v

            let readSlots n : PortView.PortSlot array =
                Array.init n (fun _ ->
                    let width = readU32 ()
                    let driver = readU32 ()
                    { PortView.SlotWidth = width; PortView.SlotDriver = driver })

            Ok
                [ for _ in 1 .. compCount ->
                      let cid = readU32 ()
                      let nIns = readU32 ()
                      let nOuts = readU32 ()

                      { PortView.SlotsComp = CommonTypes.ComponentId cid
                        PortView.SlotsIns = readSlots nIns
                        PortView.SlotsOuts = readSlots nOuts } ])

/// One signal at one clock - the tooltip case, which is simRead's degenerate form: one signal,
/// one sample, rep 1.
let simReadPoint
    (epoch: int)
    (compId: int)
    (outPort: int)
    (path: int list)
    (clock: int)
    : JS.Promise<Result<float, string>> =
    simRead epoch clock 1 1 [ compId, outPort, path ]
    |> Promise.map (fun frame ->
        let asText = decodeText frame
        if asText.StartsWith "{" then Error asText else Ok(readUint32At frame 24))
