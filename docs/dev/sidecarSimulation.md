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

One design note before building binary formats on this: the skeleton's 5-byte header leaves the
payload misaligned. Pad the header to 8 bytes and the renderer can overlay
`Uint32Array`/`Float64Array` views on a received buffer directly — zero parse, zero copy.

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

## What the skeleton does not yet do

Single client, no respawn on crash, no reconnect in `SidecarClient`; the header needs the 8-byte
padding above before binary typed-array payloads; macOS signing of the published binary is
expected to work (the entitlements already allow JIT) but has not been exercised. Dev runs
require a dotnet SDK (`dotnet run` spawns it); production ships a self-contained binary under
`resources/sidecar/` via `scripts/publish-sidecar.js`.
