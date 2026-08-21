/// Every simulation invocation, logged the same way in both runtimes.
///
/// The hooks live inside the shared simulator code - one record per startCircuitSimulation
/// (kind "build") and one per runFastSimulation invocation that advances the clock (kind
/// "run") - so Electron and the dotnet sidecar cannot instrument differently. Because the
/// renderer's progress bar works by calling runFastSimulation repeatedly with a timeout, each
/// progress-bar update is exactly one record here, and the sidecar's chunked runs produce the
/// same shape: any user-driven session yields directly comparable per-chunk numbers from both
/// runtimes, pulled as JSON by the DevHarness `simLog` command on the Electron side and the
/// SimLog protocol command on the sidecar side.
///
/// A bounded ring of records rather than prints: process state, not model state
/// (docs/mutableState.md) - written from inside the simulator, which has no dispatch, at the
/// write frequency of a progress tick. A live line per record goes to Log.dbg Log.Sim, so the
/// usual category switches make it visible without a rebuild.
module SimLog

type SimLogKind =
    | SimBuild
    | SimRun

type SimLogRecord = {
    Seq: int
    Kind: SimLogKind
    /// the top sheet simulated
    Sheet: string
    /// components after expansion (fast components plus custom wrappers)
    Components: int
    /// run: the clock tick the invocation started from; build: 0
    FromCycle: int
    /// run: the clock tick when it returned; build: 0
    ToCycle: int
    /// milliseconds this invocation took
    Ms: float
    /// TimeHelpers.getTimeMs at the moment of recording, for aligning two runtimes' logs
    At: float
    /// percentage of in-run samples taken on a performance core, or -1 where that cannot be
    /// known: under Fable always, and under .NET on a CPU that is not hybrid or does not say
    PPct: float
    /// garbage collections of each generation DURING this invocation (.NET; 0 under Fable)
    Gen0: int
    Gen1: int
    Gen2: int
    /// milliseconds this invocation spent in GC pauses (.NET; 0 under Fable)
    PauseMs: float
    /// heap in use when the invocation ended - GC.GetTotalMemory under .NET,
    /// performance.memory.usedJSHeapSize under Fable, which counts neither runtime's
    /// off-heap step slabs
    HeapMb: float
}

// ---------------------------------------------------------------------------------------------
// What an invocation cost the machine, beside time.
//
// Three questions this exists to answer, none of which wall-clock could: whether a run was
// scheduled onto efficiency cores (Windows hybrid CPUs park background work there, and the same
// simulation measured 2x apart across processes), how much of a run was garbage collection, and
// how big the heap was while that happened. All of it is .NET-only except the heap - Fable has
// no processor number and no collector to ask - so a Fable record carries -1 for PPct and zeros
// for the GC fields, which is what "not measured here" looks like in the JSON.
//
// The counters below are module-level mutable state, which docs/mutableState.md allows for
// instrumentation that is not model state: they are written from inside the simulator's run
// loop, which has no dispatch and must not allocate.
// ---------------------------------------------------------------------------------------------

#if FABLE_COMPILER

open Fable.Core

/// Sampled from the simulator's run loop; nothing to sample under Fable.
let inline sampleCore () = ()

let private resetSamples () = ()
let private samplePPct () = -1.0
let private gcCounts () = struct (0, 0, 0)
let private gcPauseMs () = 0.0

[<Emit("(typeof performance !== 'undefined' && performance.memory) ? performance.memory.usedJSHeapSize / 1048576 : 0")>]
let private heapMb () : float = jsNative

#else

open System.Runtime.InteropServices

/// Which logical processors are performance cores.
///
/// Windows reports an "efficiency class" per logical processor through GetSystemCpuSetInformation,
/// higher meaning more performant; a CPU that is not hybrid gives every processor the same class.
/// Discovered once - the topology cannot change under a running process - and consulted by an
/// array index per sample, since this is read from the run loop.
module private CpuTopology =

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool private GetSystemCpuSetInformation(nativeint information, uint32 bufferLength, uint32& returnedLength, nativeint proc, uint32 flags)

    [<DllImport("kernel32.dll")>]
    extern uint32 GetCurrentProcessorNumber()

    /// SYSTEM_CPU_SET_INFORMATION field offsets, for walking the returned buffer. The records are
    /// variable-length by their own Size field, so the walk follows that rather than a stride.
    let private sizeOffset = 0
    let private typeOffset = 4
    let private logicalIndexOffset = 14
    let private efficiencyClassOffset = 18
    let private cpuSetInformationType = 0

    /// true at the index of every logical processor on the top efficiency class. Empty when the
    /// machine cannot be asked, or is not hybrid - in both cases there is nothing to report.
    let isPerformance: bool array =
        try
            let self = System.Diagnostics.Process.GetCurrentProcess().Handle
            let mutable needed = 0u
            GetSystemCpuSetInformation(0n, 0u, &needed, self, 0u) |> ignore

            if needed = 0u then
                [||]
            else
                let buffer = Marshal.AllocHGlobal(int needed)

                try
                    let mutable got = 0u

                    if not (GetSystemCpuSetInformation(buffer, needed, &got, self, 0u)) then
                        [||]
                    else
                        let found = ResizeArray<int * int>()
                        let mutable offset = 0
                        let mutable running = true

                        while running && offset + 8 <= int got do
                            let size = Marshal.ReadInt32(buffer, offset + sizeOffset)

                            if size <= 0 then
                                running <- false
                            else
                                if Marshal.ReadInt32(buffer, offset + typeOffset) = cpuSetInformationType then
                                    found.Add(
                                        int (Marshal.ReadByte(buffer, offset + logicalIndexOffset)),
                                        int (Marshal.ReadByte(buffer, offset + efficiencyClassOffset)))

                                offset <- offset + size

                        let classes = found |> Seq.map snd |> Seq.toList

                        match classes with
                        | [] -> [||]
                        | _ when List.min classes = List.max classes -> [||] // not a hybrid CPU
                        | _ ->
                            let top = List.max classes
                            let flags = Array.zeroCreate ((found |> Seq.map fst |> Seq.max) + 1)
                            found |> Seq.iter (fun (index, cls) -> flags[index] <- (cls = top))
                            flags
                finally
                    Marshal.FreeHGlobal buffer
        with _ ->
            // any of this failing means the machine will not say, which is not an error here
            [||]

let mutable private pSamples = 0
let mutable private eSamples = 0

/// Note which core this is running on. Called from the simulator's run loop at its existing
/// time-check point - every hundred clock steps, so a million cycles costs ten thousand of
/// these - and does nothing at all where the CPU is not hybrid.
let sampleCore () =
    if CpuTopology.isPerformance.Length > 0 then
        let n = int (CpuTopology.GetCurrentProcessorNumber())

        if n < CpuTopology.isPerformance.Length then
            if CpuTopology.isPerformance[n] then
                pSamples <- pSamples + 1
            else
                eSamples <- eSamples + 1

let private resetSamples () =
    pSamples <- 0
    eSamples <- 0

let private samplePPct () =
    match pSamples + eSamples with
    | 0 -> -1.0
    | total -> 100.0 * float pSamples / float total

let private gcCounts () =
    struct (System.GC.CollectionCount 0, System.GC.CollectionCount 1, System.GC.CollectionCount 2)

let private gcPauseMs () = System.GC.GetTotalPauseDuration().TotalMilliseconds

let private heapMb () = float (System.GC.GetTotalMemory false) / 1048576.0

#endif

/// The machine's state when the invocation being timed began, so a record can report what
/// happened DURING it rather than since the process started. Invocations do not nest - the
/// simulator is single-threaded - so one slot is enough.
let mutable private atStart = struct (0, 0, 0, 0.0)

/// Bracket an invocation: called where its timing starts, so that GC and core samples cover
/// the same span as its Ms.
let beginInvocation () =
    resetSamples ()
    let struct (g0, g1, g2) = gcCounts ()
    atStart <- struct (g0, g1, g2, gcPauseMs ())

module Constants =
    let ringSize = 1000

// the ring: written one slot at a time so recording allocates almost nothing
let private ring: SimLogRecord option array = Array.create Constants.ringSize None
let mutable private nextSeq = 0

let private kindName kind =
    match kind with
    | SimBuild -> "build"
    | SimRun -> "run"

/// Record one invocation. Called from inside the simulator; everything else reads.
let record (kind: SimLogKind) (sheet: string) (components: int) (fromCycle: int) (toCycle: int) (ms: float) =
    let struct (s0, s1, s2, sPause) = atStart
    let struct (g0, g1, g2) = gcCounts ()

    let entry =
        { Seq = nextSeq
          Kind = kind
          Sheet = sheet
          Components = components
          FromCycle = fromCycle
          ToCycle = toCycle
          Ms = ms
          At = TimeHelpers.getTimeMs ()
          PPct = samplePPct ()
          Gen0 = g0 - s0
          Gen1 = g1 - s1
          Gen2 = g2 - s2
          PauseMs = gcPauseMs () - sPause
          HeapMb = heapMb () }

    ring[nextSeq % Constants.ringSize] <- Some entry
    nextSeq <- nextSeq + 1

    Log.dbg
        Log.Sim
        ($"sim {kindName kind} {sheet}: {components} comps, cycles {fromCycle}->{toCycle}, %.2f{ms}ms, "
         + $"%.0f{entry.PPct}%% P-core, gc {entry.Gen0}/{entry.Gen1}/{entry.Gen2} in %.1f{entry.PauseMs}ms, "
         + $"heap %.0f{entry.HeapMb}MB")

/// The recorded invocations, oldest first.
let recent () : SimLogRecord list =
    let count = min nextSeq Constants.ringSize
    let first = nextSeq - count

    [ for i in first .. nextSeq - 1 do
        match ring[i % Constants.ringSize] with
        | Some entry -> entry
        | None -> () ]

let clear () =
    Array.fill ring 0 ring.Length None
    nextSeq <- 0

/// The recorded invocations as a JSON array, hand-built so both runtimes emit byte-identical
/// shapes with no serializer in the way.
let recentJson () : string =
    recent ()
    |> List.map (fun entry ->
        sprintf
            """{"seq":%d,"kind":"%s","sheet":"%s","components":%d,"fromCycle":%d,"toCycle":%d,"ms":%.3f,"at":%.1f,"pPct":%.1f,"gen0":%d,"gen1":%d,"gen2":%d,"pauseMs":%.2f,"heapMb":%.1f}"""
            entry.Seq
            (kindName entry.Kind)
            entry.Sheet
            entry.Components
            entry.FromCycle
            entry.ToCycle
            entry.Ms
            entry.At
            entry.PPct
            entry.Gen0
            entry.Gen1
            entry.Gen2
            entry.PauseMs
            entry.HeapMb)
    |> String.concat ","
    |> sprintf "[%s]"
