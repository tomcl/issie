# Simulation and waveform generation in a .NET sidecar

The sidecar skeleton (`src/Sidecar`, `src/Renderer/Interface/SidecarClient.fs`, spawn logic in
`src/Main/Bridge.fs`) exists to answer one question: is a loopback WebSocket between the
renderer and a .NET process fast enough to move Issie's simulation out of JavaScript? The
numbers below were measured with it — Development > Play > Test Sidecar Latency runs the same
measurements in any debug build. The answer is yes, comfortably, and this note records the
strategy options and their rough costs.

All numbers are from one Windows development machine and are order-of-magnitude guides, not
promises.

## What the channel measures as

| Property | Measured |
|---|---|
| Round trip, small message | ~0.3–0.6 ms |
| Renderer → .NET (upload) | ~150 MB/s typical, 70–360 MB/s run-to-run spread |
| .NET → renderer (download) | ~350–800 MB/s, steady |
| `ws.send` behaviour | never blocks the page; whole message buffered instantly, drains at the above rate |

The asymmetry is structural, not fixable here: a WebSocket client must XOR-mask every byte it
sends (RFC 6455) and feed it through Chromium's renderer→network-service pipe, while the server
sends unmasked bytes straight to the socket. The .NET side consumes uploads in real time (its
reply arrives the same millisecond the client's buffer drains), so the client send pipeline is
the limit. The upload spread is exposure, not throttling: a 16MB send occupies the pipeline for
50–200 ms and anything landing in that window (GC, rendering, scheduling) stretches it; per-byte
cost does not rise with size.

Frames are binary end to end — `binaryType = "arraybuffer"`, `WebSocketMessageType.Binary` — so
payloads cross with no encoding, no JSON, no copies beyond the transport's own. The transport is
message-oriented: a "stream" is a sequence of binary messages, which also gives backpressure for
free (`SendAsync` awaits on the .NET side; `bufferedAmount` is the renderer's signal). For
happy-path traffic (commands up, results down) the fast direction carries the weight, which is
the right way round.

The frame header is 8 bytes (command, uint32 correlation id, 3 bytes of padding), so a binary
response payload starts 8-aligned and the renderer overlays `Uint32Array`/`Float64Array` views
on the received buffer directly — zero parse, zero copy. `SimRead` is the first user.

## Getting the design across

Measured on the 3cpu demo project (18 sheets, 378 components, 528 connections):

| Form | Size |
|---|---|
| Raw `.dgm` JSON, whole project | 712 KB |
| Stripped: no `SymbolInfo`, no wire `Vertices`, uuids → small ints | 186 KB |
| Also dropping component positions | 171 KB |
| Feasible floor (ports derived from `Type`, connections as 4 ints) | ~40–60 KB |

Even unstripped, the whole project is single-digit milliseconds in either direction, and JSON is
fine — no binary design format is warranted. Transfer is not the cost that matters, though:
JSON *decode* on the .NET side is. The compatibility decoder (`SimpleJsonDotNet`, a reflection
walker) measures ~300 ms warm for the whole design as `SimpleDesign` JSON (72 KB — the Simple
wire types beat every stripped-`.dgm` estimate above), ~25 ms for the largest single sheet, and
core type is irrelevant (P-cores vs E-cores measured within ~10%).

(Since these measurements, canvas ids became native integers throughout Issie -
`Helpers.IdAllocator` mints them, `RegenerateIds.admitDesign` keeps component ids design-unique
and dense at project open - so the "reduce before send" step has dissolved: SimpleDesign
conversion is now a straight projection costing ~3 ms for the whole 3cpu design.)

This is why the sidecar protocol sends a design as **one JSON string per sheet** and the sidecar
caches decoded sheets keyed by the exact JSON string (`src/Sidecar/DesignCache.fs`): an
unchanged sheet serialises to the identical string and costs a string comparison instead of a
decode. Measured in the app on 3cpu: first send 18 sheets decoded, ~1.3 s (cold JIT included);
every send after it 0 decoded, **0.26 ms** on the .NET side, ~77 ms total — nearly all of which
is now renderer-side conversion and serialisation. An edit re-decodes exactly the touched sheet.
A hand-written decoder for the Simple types (no reflection) is the known next step if the cold
first send ever matters.

## Simulation under .NET

`Renderer.fsproj` already compiles under plain .NET — `Tests/Issie.Tests` references it and
exercises the simulator that way daily. So the sidecar can reference it the same way and run the
existing FastSim, `CanvasExtractor`, and parameter resolution unchanged; the design-JSON decoder
already exists on that side. From there the options widen in a way Fable never allowed:

- the existing three-evaluator FastSim, as is, JIT-compiled;
- .NET-only algorithm work — real threads (`Parallel`, `Task`), structs and spans, native-sized
  integer arrays — none of which exist under Fable;
- entirely different engines (event-driven, compiled-to-IL) checked against `EvalReference`
  exactly as the current evaluators are.

The standing caveat from the simulator docs applies in reverse: .NET and JS performance differ
per algorithm, so measure candidates under .NET rather than assuming the app numbers carry over.

## Getting results back — three options

Per view change (scroll, zoom, radix, run), for a worst-ish case of 50 visible waveforms and the
~340 cycles a wide monitor shows at minimum zoom:

**A. Ship step values, render as today.** The sidecar sends the visible window of driver data as
binary typed arrays (~50 × 340 × 4 B ≈ 70 KB, sub-millisecond); the renderer's existing
`generateWaveform` builds the SVG from local arrays as it does now. Smallest change, smallest
payload; the renderer keeps all waveform CPU work.

**B. Ship finished SVG.** The sidecar generates per-wave SVG markup strings — the transition and
point maths in `WaveSimSVGs.fs` is dual-compiled already, only an element→string serialiser is
new — and the renderer injects them with `dangerouslySetInnerHTML`, so React diffs nothing
inside. Estimated 0.5–1.5 MB typical, ~4.5 MB pathological for 50 waves: 2–13 ms transfer, plus
a comparable browser parse on injection. Moves all waveform CPU to .NET; biggest payload.

**C. Ship polyline points as binary.** `Float32` point arrays (~3× smaller than markup, no parse)
plus a two-line VDOM wrapper per wave in the renderer. The middle ground: .NET does the maths,
the payload stays tens-of-KB, and the renderer work is trivial.

**Tooltips go lazy in every option.** `getWaveToolTip` already reads the simulator at hover
time; only the hatched-gap test (`EvilHoverCache`) is precomputed, and the sidecar computes gaps
during waveform generation anyway. So hover becomes a request — `(wave, cycle, radix)` → tooltip
string — at ~0.3–0.6 ms, imperceptible against a human hover. No `GapStore` ships, and the
renderer-side mutable cache disappears. The lookup turns asynchronous: discard stale replies
(the protocol's correlation ids do this) and hide on mouse-out without waiting.

## Rough interaction budget

| Interaction | Cost over the channel |
|---|---|
| Project open: full design sync | 1–10 ms, hidden behind project load |
| Edit: one sheet delta | < 1 ms |
| Run N cycles | command is tens of bytes; simulation time dominates |
| Scroll / zoom refresh, 50 waves | A: ~1 ms · B: ~5–20 ms incl. parse · C: ~2–5 ms |
| Hover tooltip | ~1 ms |
| Cursor column (50 values at one cycle) | tens of bytes, < 1 ms |

Per-message round-trip latency, not bandwidth, is the number that shapes a design here: anything
chatty should batch, and anything bulky is cheap if it flows .NET → renderer.

## The baseline: today's simulator under .NET (implemented)

The existing simulator now runs in the sidecar, unchanged, as the BASELINE that rewrites - and
the Electron simulator itself - are checked against:

- `SimpleDesignShim` rebuilds skeleton LoadedComponents from the wire-form design (ports
  synthesized from component arity; port ids never cross the wire), and
  `Simulator.startCircuitSimulation` takes it from there - the whole of FastSim, untouched.
- Protocol commands `SimBuild` / `SimRun` / `SimDigest` / `SimEnd` / `SimLog` drive a session
  on the last-sent design. `SimRun` takes a target cycle and a millisecond budget and reports
  where the clock got to - the same contract the renderer's own progress loop uses, so the
  client chunks, shows progress from replies, and cancels by not sending the next chunk.
- **Correctness**: `SimDigest.render` (the golden-model text, moved into the app so both
  runtimes produce it) rendered by each side and diffed by the DevHarness `simCompare` command.
  Measured on 3cpu: **byte-identical over 50 cycles** (30,821 chars of digest), electron vs
  dotnet, via `node scripts/drive.js send simCompare 50`.
- **Cost**: `SimLog` records every simulation invocation identically in both runtimes - one
  record per build, one per run chunk, which in the app means one per progress-bar update - so
  ANY user-driven session yields directly comparable per-chunk numbers, pulled with
  `drive.js send simLog` (electron) and `drive.js send sidecarSimLog` (dotnet). First numbers:
  3cpu digest runs at ~0.05 ms/cycle under Electron and ~0.02 ms/cycle under .NET in
  single-cycle chunks, both dominated by per-chunk fixed cost - the log exists precisely so
  that real workloads can be compared instead of extrapolating from this.

`SimSetInputs` sets top-level input values at a cycle (component id + 64-bit value pairs), and
`SimRead` is THE waveform-data interface: for each signal — component id, output port, access
path — it returns `samples` values taken every `rep` cycles from `start`, as binary the
renderer views zero-copy (`SidecarClient.viewSimReadData`). Those are the same (StartCycle,
SamplingZoom, ShownCycles) parameters the waveform viewer's own generation runs on, so a view
at any zoom is one request, and a tooltip is the degenerate one-signal one-sample case
(`SidecarClient.simReadPoint`). Both are pinned by a wire-payload parity test against a locally
driven simulation — dense, strided (rep 3) and single-sample — and live by the DevHarness
`sidecarProbe` command (measured word-identical on 3cpu). Signals wider than 32 bits are
refused for now. The waveform generation code itself is unchanged: the interface delivers the
data it already consumes.

The app's own progress bar drives chunked sidecar runs: `DevHarness.runOnSidecarWithProgress`
(Development > Play > Run Design On Sidecar, or `drive.js send sidecarRun <cycles>`) loops
100 ms `SimRun` chunks, updating `Model.SpinnerPayload` after each - the same popup, and the
same Cancel button, as a local long run. Cancellation is exactly the designed contract: Cancel
clears the payload, the loop notices on its next reply and simply sends no more chunks (then
frees the sidecar session). Measured: 3M cycles of 3cpu in 6.4 s over 52 chunks (~470
cycles/ms continuous - the first real sustained .NET rate); a 60M-cycle run cancelled within
one chunk of the click.

The algebraic (FData) path stays Electron-only, as agreed. Not yet done: wide-bus `SimRead`.

## Measured: the wave-sim workload, Electron against .NET

The comparison the backend switch waits on. `drive.js send localRun "<cycles> <arraySize>"` and
`sidecarRun` with the same arguments run the identical chunked workload - build at that array
size, then 100 ms `runFastSimulation` chunks to the target - on 3cpu (378 components,
1908 B/cycle). `arraySize = cycles + 1200` is the waveform simulator's non-circular shape;
`arraySize = 250` is the step simulator's circular one. Rates below are cycles/ms of pure
simulation time (SimLog chunk sums, excluding build); each backend was run cold and again warm
in the same process, several sessions apart.

| workload | Electron | .NET, workstation GC | .NET, server GC |
| --- | --- | --- | --- |
| 1.1M cycles, full arrays (~2 GB) | 113-163 | 64-83, decaying as arrays fill | 109-153 |
| 1.1M cycles, 250-entry circular | ~90 | ~253 | ~400 |
| 4M cycles, full arrays (~6.6 GB) | cannot build (over budget) | - | 153, flat 146-161 across the whole run |
| build, full arrays | ~0.4 s | ~1.7-2.2 s | ~1.7-2.7 s |

What the numbers say:

- **The GC strategy was the bottleneck, not the simulator - but WHY is not yet established.**
  Under the default workstation collector the big-array rate decayed from ~160 to ~55 cycles/ms
  as the arrays filled and a warm repeat started degraded; server GC
  (`Issie.Sidecar.fsproj`, `<ServerGarbageCollection>`) removes the decay entirely - the 4M run
  holds 146-161 cycles/ms from first decile to last with ~6.6 GB live, so the cost was never
  proportional to heap size as such. What it WAS proportional to is not yet measured. Note the
  step data is mostly not GC-visible by design: DFFs and registers keep no `State` at all
  (previous-cycle output IS their state - EvalReference's `putState` comment), and a RAM's
  store is `RamStore` - one mutable CSR structure shared by every step slot, allocating nothing
  per step (docs/dev/ramRepresentation.md). What the collector does have to look at: the
  per-RAM `State` reference array (MaxArraySize pointers, all to that one store), any
  `BigIntStep` arrays (`bigint` is a struct with a reference field, so the array is scanned),
  and whatever transient garbage the run loop makes. Which of those - or something else, such
  as card-table scanning of the big reference arrays - actually cost the workstation collector
  its throughput needs the GC counters below before it is believed.
- **Per-cycle compute is decisively faster under .NET.** Cache-resident (circular 250) it
  sustains 250-400 cycles/ms against Electron's ~90 - the earlier 470 cycles/ms measurement was
  this shape. The big-array workloads converge because both runtimes are then paying memory
  costs, not compute.
- **On the wave-sim shape the two are within each other's variance** (Electron 113-163, .NET
  109-153). Session-to-session swings of ~1.4x appear on BOTH sides across app/sidecar restarts
  - consistent with P/E-core scheduling on this machine - so single runs cannot rank them;
  overlapping ranges from repeated fresh-process runs is the honest result.
- **Only .NET reaches past Electron's budget.** 4M cycles is unbuildable in the renderer
  (V8-cage heap share refuses it); the sidecar builds it against machine RAM - it sizes both
  simulation budgets from `GC.GetGCMemoryInfo().TotalAvailableMemoryBytes` at startup, there
  being no V8 cage on that side - and runs it at full rate, undecayed. Capacity, not speed, is
  currently the .NET side's measurable win; the rewrite (flat word storage instead of per-step
  objects) is where the speed win is expected, and the baseline's GC profile above says exactly
  why.

Next instrumentation, so the GC account stops being conjecture: put
`GC.CollectionCount` per generation and `GC.GetGCMemoryInfo()` pause data into each sidecar
SimLog chunk record (and the running thread's processor number, for the P/E-core question) -
then one workstation run and one server run say exactly where the time went, and the
unexplained ~1.4x session-to-session variance seen on both backends gets an answer too.

Benchmark hygiene, learned the hard way: `sidecarRun` sends whatever design is open, so an
`openProject` must have finished before it fires - a 0-component build (visible in the SimLog
record) means it did not; and the wall-clock line the command logs includes build and transport,
so per-chunk SimLog sums are what to compare.

## What the skeleton does not yet do

Single client, no respawn on crash, no reconnect in `SidecarClient`; the header needs the 8-byte
padding above before binary typed-array payloads; macOS signing of the published binary is
expected to work (the entitlements already allow JIT) but has not been exercised. Dev runs
require a dotnet SDK (`dotnet run` spawns it); production ships a self-contained binary under
`resources/sidecar/` via `scripts/publish-sidecar.js`.
