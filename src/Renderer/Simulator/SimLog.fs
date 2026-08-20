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
}

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
    let entry =
        { Seq = nextSeq
          Kind = kind
          Sheet = sheet
          Components = components
          FromCycle = fromCycle
          ToCycle = toCycle
          Ms = ms
          At = TimeHelpers.getTimeMs () }

    ring[nextSeq % Constants.ringSize] <- Some entry
    nextSeq <- nextSeq + 1

    Log.dbg
        Log.Sim
        $"sim {kindName kind} {sheet}: {components} comps, cycles {fromCycle}->{toCycle}, %.2f{ms}ms"

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
            """{"seq":%d,"kind":"%s","sheet":"%s","components":%d,"fromCycle":%d,"toCycle":%d,"ms":%.3f,"at":%.1f}"""
            entry.Seq
            (kindName entry.Kind)
            entry.Sheet
            entry.Components
            entry.FromCycle
            entry.ToCycle
            entry.Ms
            entry.At)
    |> String.concat ","
    |> sprintf "[%s]"
