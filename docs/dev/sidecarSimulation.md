# Simulation and waveform generation in a .NET sidecar

The sidecar skeleton (`src/Sidecar`, `src/Renderer/Interface/SidecarClient.fs`, spawn logic in
`src/Main/Bridge.fs`) exists to answer one question: is a loopback WebSocket between the
renderer and a .NET process fast enough to move Issie's simulation out of JavaScript? The
numbers below were measured with it — Development > Play > Test Sidecar Latency runs the same
measurements in any debug build. The answer is yes, comfortably, and this note records the
strategy options and their rough costs.

All numbers are from one Windows development machine and are order-of-magnitude guides, not
promises.

What must be TRUE for the two sides to agree about a simulation - as opposed to how fast they can
talk - is [sidecarInvariants.md](sidecarInvariants.md), which also says which of those things a
running Issie can check for itself.

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
`sidecarProbe` command (measured word-identical on 3cpu). Any width: `wordsPerSample` is
computed from the widest signal asked for, laid out least-significant word first. The waveform
generation code itself is unchanged: the interface delivers the data it already consumes.

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

**Step-slab layout (measured after the table above).** Flattening every uint32/bigint step
array into a few large shared slabs - IOArray regions named by `StepBase`/`StepLength` integer
offsets, the FastCreate arena made cross-runtime instead of Fable-only - moved the numbers
decisively. Pure-simulation rates (SimLog chunk sums, build excluded), same 3cpu workloads:

| workload | Electron | .NET, server GC |
| --- | --- | --- |
| 1.1M cycles, full arrays | 102-131 | 181-206 |
| 1.1M cycles, 250-entry circular | 116 | 321 |
| 4M cycles, full arrays, fresh process | - | **303, flat 250-327 across the run, 6.6 GB live** |

.NET's big-array rate doubled (the ~150 ceiling was substantially the runtime's own array
bookkeeping, not memory latency as first guessed); Electron stayed inside its usual range, as
the arena note predicted ("measured simulation speed unchanged") since under Fable the layout
was already slab-backed views. The .NET build also got cheaper (0.6-0.9s against 1.6-2.7s for
the 2 GB build). With the slabs, the wave-sim workload verdict flips: .NET is ~2x Electron at
1.1M and holds ~300 cycles/ms to 4M cycles, which Electron cannot build at all. One trap found
and shimmed on the way: reads inside the new IOArray members compiled to fable-library's
bounds-checked item() (the EvalCompiled getA/setA story again - `SimTypes.stepGet/stepSet` is
the same shim one layer down), which had silently tripled Electron's whole-simulation cost
until the emitted JS was checked. Correctness pinned by the full suite (652 tests, golden +
reducers-agree) and by SimDigest byte-identity between the runtimes in the app.

## The variance was the scheduler, and the answer is one call

The run-to-run spread that made every rate above a range rather than a number - the same 1.1M
workload measuring 154 and 293 cycles/ms in one process - is now measured rather than guessed,
and it was neither GC nor heap debris. SimLog gained three things to settle it: the percentage
of a run's clock cycles executed on a **performance core** (sampled from the run loop's existing
time-check point, every hundred steps, through GetCurrentProcessorNumber and the efficiency
classes GetSystemCpuSetInformation reports), the **GC collections and pause milliseconds** during
each invocation, and the heap in use. Windows and .NET only: Fable can ask for none of them, so
an Electron record carries -1 for P% and zeros for the GC fields.

The first run of that instrumentation answered it outright. Four consecutive 1.1M runs on 3cpu,
on an i7-1265U with 2 performance cores (4 threads) and 8 efficiency cores:

| P-core residency | rate | GC collections | GC pause |
| ---: | ---: | :---: | ---: |
| 56.8% | 154 c/ms | 0/0/0 | 0.0 ms |
| 69.5% | 229 c/ms | 0/0/0 | 1.2 ms |
| 74.9% | 273 c/ms | 0/0/0 | 0.0 ms |
| 80.5% | 293 c/ms | 0/0/0 | 9.9 ms |

**Garbage collection is not involved at all** - zero collections of any generation across a
1.1M-cycle run, which the step-slab layout makes sense of: the slabs are a handful of large
long-lived arrays and the run loop allocates about a byte a cycle. The rate tracks P-core
residency monotonically, and tracks it decile by decile *within* a run too, rate rising and
falling with the percentage as Windows moves the thread.

The cause is that Windows treats a windowless console process as background work, parks it on
efficiency cores and applies EcoQoS power throttling. The sidecar now asks not to be
power-throttled at startup (`CpuQos` in Program.fs, one SetProcessInformation call), which is
the right default for a process that sits blocked on a socket using nothing until the app asks
it to simulate - no idle battery is traded away. `ISSIE_SIDECAR_CPU=eco` restores the old
behaviour and `=pin` confines the process to performance cores, both for measurement.

With the default in place the same four runs sit at **97-99.7% P-core residency and 390-529
cycles/ms**, and the spread that motivated all of this is gone.

This also retired the sidecar's `ServerGarbageCollection` setting, which was added earlier on
the strength of numbers that had not controlled for any of the above. Measured again with the
scheduler settled and the step slabs in place, the default workstation collector gives 411, 490
and 488 cycles/ms against server GC's 405, 529 and 511 - the same, which is what zero
collections predicts. The override is gone rather than kept as a talisman; it costs per-core
heaps for no measured benefit, and it is one line to restore if a future execution layer
allocates in the run loop.

### What was left after that: tiered compilation

Removing the scheduler left a smaller spread - 405, 529, 511 over three runs - which the
per-chunk records placed exactly. The first run in a process spent its first several hundred
milliseconds at 57-87 cycles/ms *while at 94-99% P-core residency*, then jumped abruptly to
450+. Not scheduling, not GC (still zero collections), and not a gradual ramp: a step change.

That is tiered compilation. A method starts in unoptimised tier-0 code and is promoted once it
has been called enough times, but the call counting is delayed - by default 100ms, and the
delay RESTARTS whenever more new code is being compiled. Building a simulation compiles a great
deal of new code, so the hot reducers stayed in tier-0 until the build's compilation activity
finally stopped, and everything got promoted at once. Later runs in the same process pay none
of it, which is why only the first run looked slow.

`DOTNET_TC_CallCountingDelayMs=0`, set on the sidecar's environment where main spawns it
(Bridge.fs), promotes them as soon as they are hot. Zero rather than turning tiering off
altogether: tier 0 still gives the process a quick start, and dynamic PGO - which rides on
tiering - still gets to specialise the reducers. Three consecutive 1.1M-cycle runs through the
real spawn path then give **495, 500, 497 cycles/ms**, and the first run is no longer the slow
one. For comparison, turning tiering off entirely gave 527, 488, 486 - no better, and it costs
startup and PGO.

So the variance decomposes, in the order it was found and each measured rather than argued:

| | three-run spread |
| --- | --- |
| as first measured | 154 - 293 c/ms |
| EcoQoS off (scheduler) | 405 - 529 |
| plus call-counting delay 0 (JIT) | 495 - 500 |

What remains is chunk-to-chunk noise of about 5% around a settled ~505 cycles/ms, which is
where measurement of this workload should now start.

**Electron is subject to the same thing**, which matters because it means earlier comparisons
were between two throttled processes rather than two fair ones. The renderer cannot sample its
own core, so it was tested from outside: pinning the Electron processes to performance cores
took 1.1M-cycle runs from 101-182 cycles/ms (nine runs, no pattern) to **205, 214, 215** - a
tight cluster. Making the window foreground, by contrast, changed nothing measurable, so this
is the OS scheduler placing a long-running compute thread rather than Chromium's own
backgrounding. Issie does not currently opt its renderer out of throttling; that is a real
improvement available to the Electron simulator, and a separate change from this branch.

So the honest head-to-head, both runtimes on performance cores, 1.1M cycles of 3cpu on full
(wave-sim shaped) arrays, pure simulation time:

| | Electron | .NET sidecar |
| --- | ---: | ---: |
| pure simulation | 205-215 c/ms | 390-529 c/ms |
| build (2 GB of arrays) | ~0.4 s | ~0.4-0.6 s |

**.NET is about 2.2x faster**, on top of reaching cycle counts Electron cannot build at all.
That is the number the backend switch should be judged on; every earlier figure in this document
was taken before the scheduler was understood and should be read as a lower bound on both sides.

### The comparison, with core occupancy controlled

Every figure above this point was taken before the scheduler was understood, so the head-to-head
was redone with both runtimes held on performance cores - the sidecar by its own QoS request
(SimLog confirms 99-100%), Electron by pinning its processes, since it cannot report or control
its own placement. Two further things had to be handled. Runs are **interleaved**, Electron and
sidecar alternating on the same workload, because the machine drifts: on this 15W laptop part a
long benchmarking session sees both runtimes fall by a third or more, so only pairwise ratios
taken minutes apart mean anything. And **pure simulation time** is used throughout - SimLog
chunk sums - since the wall-clock line a run prints includes build and transport.

1.1M cycles of 3cpu, four interleaved pairs, wave-sim shaped (full) arrays:

| | Electron | .NET | ratio |
| --- | ---: | ---: | ---: |
| | 217 | 709 | 3.26 |
| | 211 | 620 | 2.95 |
| | 227 | 665 | 2.93 |
| | 202 | 594 | 2.94 |

**.NET is 2.9x Electron** on the workload the waveform simulator actually runs. On the
cache-resident 250-entry circular workload, three interleaved pairs give 4.4-4.7x. Both are
larger than the 2.2x recorded further up this document, which was measured before the JIT delay
was found.

**The slab change did not cost Electron anything measurable.** The pre-slab simulator - where a
step array was its own Uint32Array view rather than a slab plus an integer offset - was rebuilt
and measured under the same pinning. On full arrays it went 247 against the new 242 in one
block and 190 against 214 in another: the direction reverses, so the difference is drift rather
than the change. On the circular workload the old form was ahead in both blocks it was measured
(269 vs 251, then 247 vs 238), which is a plausible ~5% for the extra add per access showing up
where the work is compute-bound and cache-resident - but it was never measured interleaved, so
treat it as a hint rather than a result. Either way the "old simulator was faster" impression
came from unpinned runs, where scheduling noise is larger than any of this.

### The large designs

3cpu is 378 components. The designs that decide whether this is worth doing are the ones a
student's project actually reaches, and `largeTest` is the stress case: `main1`..`main6`, each
sheet instantiating several of the one below, so tiny files expand enormously - **main5 to
120,084 components and main6 to 480,342**, the design the step-array arena was built for. Both
cost megabytes of step storage per clock, so they are measured at few cycles and the interesting
numbers are the build and the per-cycle rate rather than any total.

Measured as shipped - the sidecar with its QoS request and server GC, Electron as it runs today:

| design | components | | Electron | .NET | .NET is |
| --- | ---: | --- | ---: | ---: | ---: |
| main5 | 120,084 | build | 5.1 s | 3.4 s | 1.5x faster |
| | | simulation | 105 cycles/s | 218 cycles/s | **2.1x** |
| main6 | 480,342 | build | 21.6 s | 15.6 s | 1.4x faster |
| | | simulation | 22.4 cycles/s | 54.5 cycles/s | **2.4x** |

Three things to take from this beyond the ratios.

**The advantage narrows as the design grows** - 2.9x on 3cpu, 2.4x on main6 - because a cycle of
main6 touches 480,000 components' worth of step storage and the work becomes memory-bound, where
neither runtime can do much. It does not disappear, and the build advantage appears only at this
scale.

**Pinning Electron is the wrong control here.** On 3cpu it was the fix; on these designs it makes
Electron *slower* (main5 105 -> 64 cycles/s, main6 22.4 -> 15.8), because confining every Electron
process to two physical cores starves V8's parallel GC threads, which have real work at a 5.6 GB
heap. The as-shipped comparison above is the meaningful one; the pinned-core methodology used
earlier does not transfer to workloads where the collector is busy.

**GC is a build cost, not a run cost, and only at scale.** The run loop still collects nothing -
0/0/0 over 400 cycles of a 480,000-component design, which is the slab layout doing its job. The
build is the opposite: 1,232 gen0 collections and 7.0 s of pause in a 22.8 s build under the
default collector. That is why `ServerGarbageCollection` is back in the sidecar's fsproj after
being removed a few commits ago: it was removed on evidence from 3cpu's run loop, where GC does
nothing and it made no difference, and that evidence simply did not cover the case where it
earns its place.

**Cycle count does not change the rate.** 800K against 1.1M, interleaved three times each to
cancel drift, gave medians of 116 and 139 cycles/ms - the larger workload nominally faster, and
both well inside the 96-180 spread of unpinned runs. A wider sweep (500K, 800K, 1.1M, 1.4M) put
1.4M between 800K and 1.1M rather than last. There is no size cliff between those working sets;
what looked like one was the scheduler.

Benchmark hygiene, learned the hard way: `sidecarRun` sends whatever design is open, so an
`openProject` must have finished before it fires - a 0-component build (visible in the SimLog
record) means it did not; and the wall-clock line the command logs includes build and transport,
so per-chunk SimLog sums are what to compare.

## What the skeleton does not yet do

Single client, and no respawn after a crash: `SidecarClient` waits for the sidecar to START
listening (a startup budget, so first contact after spawn is a wait rather than an error), but a
process that dies mid-session stays dead until the app restarts - the failure is bounded by the
paced-retry latching of sidecarInvariants.md section J and surfaced by the viewer's banner. The
8-byte header padding is in place; macOS signing of the published binary is expected to work
(the entitlements already allow JIT) but has not been exercised. Dev runs
require a dotnet SDK (`dotnet run` spawns it); production ships a self-contained binary under
`resources/sidecar/` via `scripts/publish-sidecar.js`.
