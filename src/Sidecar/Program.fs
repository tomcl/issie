/// A dotnet process Issie's main process runs beside the app, serving a WebSocket on loopback.
///
/// The server lives here rather than in Electron because the renderer is sandboxed: it can open
/// a browser WebSocket to 127.0.0.1 but cannot host anything, and putting the server in main
/// would need an npm dependency this way needs nothing beyond the base class library.
///
/// Startup contract with src/Main/Bridge.fs, which spawns this process:
///   - the one line ever written to stdout is "SIDECAR_LISTENING <port>", once the socket is up;
///   - the auth token, if any, arrives in the ISSIE_SIDECAR_TOKEN environment variable, and a
///     client must present it as ?token=<value> on the connection URL;
///   - stdin is held open by main for the process's whole life. EOF there means main is gone -
///     however it went - and is the signal to exit rather than outlive the app.
///
/// This skeleton is deliberately single-client and synchronous at the accept loop: it exists to
/// measure the channel, not to multiplex it.
module Issie.Sidecar.Program

open System
open System.Net
open System.Net.Sockets
open System.Net.WebSockets
open System.Threading
open System.Threading.Tasks
open Issie.Sidecar

/// HttpListener cannot bind port 0, so ask the kernel for a free port with a throwaway
/// TcpListener and bind to that. Another process can take the port between the two steps;
/// that race is rare and retried rather than prevented.
let rec private startListener (attempts: int) =
    if attempts = 0 then
        failwith "sidecar: no free loopback port found"

    let probe = new TcpListener(IPAddress.Loopback, 0)
    probe.Start()
    let port = (probe.LocalEndpoint :?> IPEndPoint).Port
    probe.Stop()

    let listener = new HttpListener()
    listener.Prefixes.Add $"http://127.0.0.1:{port}/"

    try
        listener.Start()
        listener, port
    with :? HttpListenerException ->
        startListener (attempts - 1)

/// One whole message, read chunk by chunk through `buffer` - a small buffer reused for every
/// read, so that receiving costs nothing proportional to the message. The 5-byte header is
/// always kept; what happens to the payload depends on the command as soon as it is known:
/// Echo and Download need theirs back (collected), an Upload payload is DISCARDED as it
/// arrives. Upload exists to measure transport, and collecting 16MB on this side would time
/// the collection too - this is the measurement the numbers claim to be.
/// Returns None on a Close frame, otherwise (header bytes filled, header, payload).
let private receiveMessage (ws: WebSocket) (buffer: byte array) (ct: CancellationToken) =
    task {
        let header = Array.zeroCreate Protocol.HeaderSize
        let mutable headerGot = 0
        let mutable body: IO.MemoryStream = null
        let mutable discarded = 0L
        let mutable finished = false
        let mutable closed = false

        while not finished do
            let! r = ws.ReceiveAsync(ArraySegment buffer, ct)

            if r.MessageType = WebSocketMessageType.Close then
                finished <- true
                closed <- true
            else
                // the header first, from however the chunks happen to fall
                let toHeader = min r.Count (Protocol.HeaderSize - headerGot)
                Array.blit buffer 0 header headerGot toHeader
                headerGot <- headerGot + toHeader

                let rest = r.Count - toHeader

                if headerGot = Protocol.HeaderSize then
                    if isNull body && header[0] <> Protocol.Upload then
                        body <- new IO.MemoryStream()

                    if rest > 0 then
                        if isNull body then
                            discarded <- discarded + int64 rest
                        else
                            body.Write(buffer, toHeader, rest)

                let sofar = discarded + (if isNull body then 0L else body.Length)

                if sofar > int64 Protocol.MaxMessage then
                    failwith "sidecar: message over the size cap"

                finished <- r.EndOfMessage

        if closed then
            return None
        else
            return Some(headerGot, header, (if isNull body then Array.empty else body.ToArray()))
    }

let private send (ws: WebSocket) (frame: byte array) (ct: CancellationToken) =
    ws.SendAsync(ArraySegment frame, WebSocketMessageType.Binary, true, ct)

/// The response header for a request header: same correlation id, response bit set.
let private responseHeader (header: byte array) =
    let response = Array.copy header
    response[0] <- response[0] ||| Protocol.ResponseFlag
    response

/// Decoded sheets of the last design received, keyed by the exact JSON each came from - see
/// DesignCache. Process state rather than anything modelled, replaced wholesale per request, so
/// it never holds more than one design's worth.
let mutable private sheetCache: Map<string, CommonTypes.SimpleSheet> = Map.empty

/// The last design assembled from a SendDesign, which is what the Sim* commands operate on.
let mutable private lastDesign: CommonTypes.SimpleDesign option = None

/// A response frame carrying a binary payload.
let private bytesResponse (header: byte array) (payload: byte array) =
    let frame = Array.zeroCreate (Protocol.HeaderSize + payload.Length)
    Array.blit (responseHeader header) 0 frame 0 Protocol.HeaderSize
    Array.blit payload 0 frame Protocol.HeaderSize payload.Length
    frame

/// A response frame whose payload is UTF-8 text.
let private textResponse (header: byte array) (text: string) =
    bytesResponse header (Text.Encoding.UTF8.GetBytes(text: string))

/// The uint32 at a byte offset of a command payload, 0 when the payload is too short.
let private argAt (body: byte array) (offset: int) =
    if body.Length >= offset + 4 then int (BitConverter.ToUInt32(body, offset)) else 0

/// Serve one connection until it closes or misbehaves.
let private serve (ws: WebSocket) (ct: CancellationToken) =
    task {
        // every chunk of every message lands here; nothing else is allocated per chunk
        let buffer = Array.zeroCreate 1024
        let mutable running = true

        while running do
            match! receiveMessage ws buffer ct with
            | None ->
                do! ws.CloseAsync(WebSocketCloseStatus.NormalClosure, "closing", ct)
                running <- false
            | Some(headerGot, _, _) when headerGot < Protocol.HeaderSize ->
                do! ws.CloseAsync(WebSocketCloseStatus.ProtocolError, "short frame", ct)
                running <- false
            | Some(_, header, body) ->
                match header[0] with
                | Protocol.Echo ->
                    // header and payload as two frames of one message, rather than copied
                    // into a single contiguous response
                    let response = responseHeader header

                    if body.Length = 0 then
                        do! send ws response ct
                    else
                        do! ws.SendAsync(ArraySegment response, WebSocketMessageType.Binary, false, ct)
                        do! ws.SendAsync(ArraySegment body, WebSocketMessageType.Binary, true, ct)
                | Protocol.Upload ->
                    do! send ws (responseHeader header) ct
                | Protocol.Download ->
                    let requested = if body.Length >= 4 then BitConverter.ToInt32(body, 0) else 0
                    let n = max 0 (min requested (Protocol.MaxMessage - Protocol.HeaderSize))
                    let frame = Array.zeroCreate (Protocol.HeaderSize + n)
                    Array.blit (responseHeader header) 0 frame 0 Protocol.HeaderSize
                    Random.Shared.NextBytes(Span(frame, Protocol.HeaderSize, n))
                    do! send ws frame ct
                | Protocol.SendDesign ->
                    // per-sheet framing so an unchanged sheet costs a lookup, not a decode -
                    // see DesignCache; the reply says how much work was actually done, as a
                    // little JSON built by hand since nothing here warrants an encoder
                    let stopwatch = Diagnostics.Stopwatch.StartNew()

                    let outcome =
                        DesignCache.parsePayload body
                        |> Result.bind (fun (topSheet, sheetJsons) ->
                            DesignCache.decodeSheets sheetCache sheetJsons
                            |> Result.map (fun (sheets, decoded, newCache) ->
                                sheetCache <- newCache

                                let design: CommonTypes.SimpleDesign =
                                    { TopSheet = topSheet; Sheets = sheets }

                                design, decoded))

                    stopwatch.Stop()

                    let reply =
                        match outcome with
                        | Ok(design, decoded) ->
                            lastDesign <- Some design
                            let comps = design.Sheets |> List.sumBy (fun sheet -> sheet.Components.Length)
                            let conns = design.Sheets |> List.sumBy (fun sheet -> sheet.Connections.Length)

                            sprintf
                                """{"sheets":%d,"decoded":%d,"cached":%d,"components":%d,"connections":%d,"deserialiseMs":%.2f}"""
                                design.Sheets.Length
                                decoded
                                (design.Sheets.Length - decoded)
                                comps
                                conns
                                stopwatch.Elapsed.TotalMilliseconds
                        | Error e ->
                            let safe = e.Replace("\\", "/").Replace("\"", "'").Replace("\n", " ").Replace("\r", " ")
                            sprintf """{"error":"%s"}""" safe

                    do! send ws (textResponse header reply) ct
                | Protocol.SimBuild ->
                    let reply =
                        match lastDesign with
                        | None -> """{"error":"no design received - send SendDesign first"}"""
                        | Some design -> SimSession.build design (max 2 (argAt body 0))

                    do! send ws (textResponse header reply) ct
                | Protocol.SimRun ->
                    // epoch first, then target cycle and time budget - see Protocol.fs
                    do! send ws (textResponse header (SimSession.run (argAt body 0) (argAt body 4) (argAt body 8))) ct
                | Protocol.SimDigest ->
                    let reply =
                        match lastDesign with
                        | None -> """{"error":"no design received - send SendDesign first"}"""
                        | Some design -> SimSession.digest design (max 1 (argAt body 0))

                    do! send ws (textResponse header reply) ct
                | Protocol.SimEnd ->
                    do! send ws (textResponse header (SimSession.endSession (argAt body 0))) ct
                | Protocol.SimLog ->
                    do! send ws (textResponse header (SimLog.recentJson ())) ct
                | Protocol.SimSetInputs ->
                    do! send ws (textResponse header (SimSession.setInputs (argAt body 0) (body[4..]))) ct
                | Protocol.SimRead ->
                    let frame =
                        match SimSession.read (argAt body 0) (body[4..]) with
                        | Ok payload -> bytesResponse header payload
                        | Error e -> textResponse header (sprintf """{"error":"%s"}""" (e.Replace("\"", "'")))

                    do! send ws frame ct
                | other ->
                    do! ws.CloseAsync(WebSocketCloseStatus.ProtocolError, $"unknown command {other}", ct)
                    running <- false
    }

/// Windows schedules a process it considers background onto efficiency cores, and applies EcoQoS
/// power throttling on top. A console process with no window is background by that reckoning
/// however hard it is working - which for this one, measured on an i7-1265U (2 P cores, 8 E
/// cores), meant simulation running anywhere between 57% and 80% of its clock cycles on a P core
/// with the rate tracking that residency almost exactly, 154 to 293 cycles/ms. That was the whole
/// of the run-to-run variance the sidecar's numbers used to show; GC turned out to account for
/// none of it (SimLog records 0 collections over a 1.1M-cycle run).
///
/// So this process asks not to be power-throttled, by default, at startup: a polite request that
/// takes no core away from anyone and only says this is not background work. It is the right
/// default for what this process is - it sits blocked on a socket using nothing at all until the
/// app asks it to simulate, so there is no idle battery cost to trade away. With it, the same
/// four runs go to 97-99.7% P-core residency and 390-474 cycles/ms.
///
/// ISSIE_SIDECAR_CPU overrides, read once at startup:
///   unset            ask not to be power-throttled - the default described above
///   "eco"            leave the process alone, so whatever Windows decides stands. Here to
///                    measure the difference, and as the escape hatch if the default ever
///                    misbehaves on a machine
///   "pin"            confine the process to performance cores (affinity mask). Blunt, and it
///                    hurts a machine whose P cores are wanted elsewhere - for measuring the
///                    ceiling, not for shipping
module private CpuQos =
    open System.Runtime.InteropServices

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool private SetProcessInformation(nativeint proc, int informationClass, nativeint information, uint32 size)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool private GetSystemCpuSetInformation(nativeint information, uint32 bufferLength, uint32& returnedLength, nativeint proc, uint32 flags)

    /// ProcessPowerThrottling = 4; the struct is version, ControlMask, StateMask - setting the
    /// EXECUTION_SPEED bit (1) in the mask and clearing it in the state turns EcoQoS OFF.
    let private processPowerThrottling = 4
    let private currentVersion = 1u
    let private executionSpeed = 1u

    let private disableThrottling () =
        let size = 12
        let block = Marshal.AllocHGlobal size

        try
            Marshal.WriteInt32(block, 0, int currentVersion)
            Marshal.WriteInt32(block, 4, int executionSpeed) // ControlMask: we are setting this
            Marshal.WriteInt32(block, 8, 0) // StateMask: ... to off
            SetProcessInformation(Diagnostics.Process.GetCurrentProcess().Handle, processPowerThrottling, block, uint32 size)
        finally
            Marshal.FreeHGlobal block

    /// Affinity mask over every logical processor of the top efficiency class, 0 when the CPU is
    /// not hybrid or will not say.
    let private performanceMask () =
        try
            let self = Diagnostics.Process.GetCurrentProcess().Handle
            let mutable needed = 0u
            GetSystemCpuSetInformation(0n, 0u, &needed, self, 0u) |> ignore
            let buffer = Marshal.AllocHGlobal(int needed)

            try
                let mutable got = 0u

                if not (GetSystemCpuSetInformation(buffer, needed, &got, self, 0u)) then
                    0L
                else
                    let found = ResizeArray<int * int>()
                    let mutable offset = 0
                    let mutable running = true

                    while running && offset + 8 <= int got do
                        let size = Marshal.ReadInt32(buffer, offset)

                        if size <= 0 then
                            running <- false
                        else
                            if Marshal.ReadInt32(buffer, offset + 4) = 0 then
                                found.Add(int (Marshal.ReadByte(buffer, offset + 14)), int (Marshal.ReadByte(buffer, offset + 18)))

                            offset <- offset + size

                    let classes = found |> Seq.map snd |> Seq.toList

                    match classes with
                    | [] -> 0L
                    | _ when List.min classes = List.max classes -> 0L
                    | _ ->
                        let top = List.max classes

                        found
                        |> Seq.filter (fun (_, cls) -> cls = top)
                        |> Seq.fold (fun mask (index, _) -> mask ||| (1L <<< index)) 0L
            finally
                Marshal.FreeHGlobal buffer
        with _ ->
            0L

    /// Apply what ISSIE_SIDECAR_CPU asks for, and say what was done - the one line of startup
    /// diagnostics this process has room for, on stderr so stdout stays the port handshake.
    let apply () =
        if not (OperatingSystem.IsWindows()) then
            ()
        else
            match (Environment.GetEnvironmentVariable "ISSIE_SIDECAR_CPU" |> Option.ofObj |> Option.defaultValue "").ToLower() with
            | "eco" -> Console.Error.WriteLine "sidecar: leaving power throttling as Windows set it"
            | "pin" ->
                match performanceMask () with
                | 0L -> Console.Error.WriteLine "sidecar: no performance-core mask available"
                | mask ->
                    Diagnostics.Process.GetCurrentProcess().ProcessorAffinity <- nativeint mask
                    Console.Error.WriteLine $"sidecar: pinned to performance cores (mask 0x%x{mask})"
            | _ ->
                let ok = disableThrottling ()
                Console.Error.WriteLine $"sidecar: EcoQoS off = {ok}"

[<EntryPoint>]
let main _ =
    // absent when run by hand from a shell, and then any client is accepted
    let token = Environment.GetEnvironmentVariable "ISSIE_SIDECAR_TOKEN"

    CpuQos.apply ()

    // The simulation memory budgets ship with fallbacks for a process that cannot ask its
    // machine (the test suite); this process can ask. Both budgets scale with physical memory
    // here: .NET has no V8-style 4GB heap cage, so the wide-bus step arrays are bounded by the
    // machine exactly as the narrow ones are.
    let physicalBytes = float (GC.GetGCMemoryInfo().TotalAvailableMemoryBytes)
    SimTypes.SimulationBudget.setBudgetsFromMachine physicalBytes physicalBytes

    let listener, port = startListener 10

    // The handshake main is waiting for; the only line this process ever prints to stdout.
    Console.Out.WriteLine $"SIDECAR_LISTENING {port}"
    Console.Out.Flush()

    // Main holds our stdin pipe for as long as it lives, so EOF here means the app is gone -
    // including every way it can die without running its quit handlers. Exit rather than orphan.
    Task.Run(fun () ->
        (try
            Console.In.ReadToEnd() |> ignore
         with _ ->
            ())

        exit 0)
    |> ignore

    while true do
        let ctx = listener.GetContext()

        let authorised =
            isNull token || ctx.Request.QueryString["token"] = token

        if not (ctx.Request.IsWebSocketRequest && authorised) then
            ctx.Response.StatusCode <- 401
            ctx.Response.Close()
        else
            try
                let wsCtx = ctx.AcceptWebSocketAsync(subProtocol = null).GetAwaiter().GetResult()
                serve wsCtx.WebSocket CancellationToken.None |> fun t -> t.GetAwaiter().GetResult()
            with e ->
                // a dropped connection lands here; log it and go back to accepting
                Console.Error.WriteLine $"sidecar: connection ended: {e.Message}"

    0
