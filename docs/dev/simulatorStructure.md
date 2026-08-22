# How the simulator is put together

A map of `src/Renderer/Simulator`, what each layer is responsible for, and which of the seams are
deliberate. Written for someone about to change the fast simulator rather than someone using it.

## The two graphs, and the bridge

Issie has two representations of a circuit and the most common mistake is treating them as one.

- **Canvas state** — `Component list * Connection list`, what a `.dgm` holds, full of geometry.
- **The simulation graph** — the electrical structure, no layout.

`CanvasExtractor.fs` is the bridge, and `CanvasExtractor.signatureOfInstance` is the only place
that works out a custom component instance's port widths. A parameterised sheet has a *family* of
signatures, one per set of bindings, so a port width is a fact about the instance and not about
the sheet.

## Layers, in compile order

F# compile order is linear, so the order in `Renderer.fsproj` **is** the dependency layering: a
module can only use what is above it, and an illegal dependency is a compile error rather than a
convention. The order below is chosen for that reason.

| | |
|---|---|
| `SimGraphTypes.fs` | the simulation graph, value representations (`FastData`, `FData`), bit-extraction helpers |
| `SimTypes.fs` | `IOArray`, `StepIndex`, `FastComponent`, `FastSimulation` |
| `NumberHelpers.fs` | number formatting and conversion |
| `CanvasExtractor.fs` | canvas → graph, and instance signatures |
| `SynchronousUtils.fs`, `CanvasStateAnalyser.fs`, `GraphBuilder.fs`, `GraphMerger.fs`, `SimulationGraphAnalyser.fs` | building and checking the graph |
| `FastSim/…` | the fast simulator, below |
| `Simulator.fs` | entry point for starting a simulation |

### FastSim

```
FastCreate      allocate components and step arrays, link ports to driver arrays
EvalKernel      the primitives every evaluator is built from
EvalReference   the match on component type - the executable specification
EvalCompiled    per-component reducers, built once when the simulation is built
EvalAlgebraic   the FData backend, of which truth tables are one user
FastOrder       the order combinational components must be reduced in
FastValidate    checks on a built simulation
FastBuild       assemble: arrays, order, validate, install reducers
FastRun         the run loop
FastExtract     read results out
```

## The three evaluators

All three compute the same component semantics. They differ only in what a value is and when the
work is decided.

**`EvalReference`** is one `match` over component type and `UseBigInt`, evaluated per component
per clock step. It is the specification: when the other two disagree with it, they are wrong.
Nothing should be deleted from it as other evaluators grow — it is what they are checked against.

**`EvalCompiled`** builds a closure per component when the simulation is built, which has already
resolved everything that cannot change: the component's type, whether it is on the uint32 or
bigint path, its bus masks, and which step arrays its ports live in. Its body is only what differs
from step to step. `reducerFor` returns `None` for a type it does not handle, and that component
keeps `EvalReference`, so the file can be filled in a type at a time.

**`EvalAlgebraic`** is the `FData` backend, which carries algebraic values alongside data. It is a
near-copy of `EvalReference` for a different value representation, and the two must move together.
That duplication is the largest single piece of debt here — see below.

### Two rules `EvalCompiled` depends on

- **Reducers capture step arrays.** They must therefore be installed after every re-linking pass,
  including the one `addWavesToFastSimulation` does for custom components, and only for components
  that are actually reduced. `FastBuild.installReducers` runs last for this reason. Installing
  earlier would capture an array the simulation no longer uses, and the simulation would quietly
  compute the wrong signal.
- **The masking invariant.** Every value in a step array is already within its bus width. Readers
  never mask; a reducer masks its result exactly when its own operation can overflow, and not
  otherwise. On the uint32 path width exactly 32 needs care, since `1u <<< 32` is `1u`.

### The Fable-specific part

`EvalCompiled` indexes step arrays through `getA`/`setA`, which are raw JS indexing under Fable and
ordinary checked indexing under .NET. This is not a micro-optimisation: profiling the app found
fable-library's `item`/`setItem` taking **70%** of all simulation time, because an `arr[i]` on a
local binding compiles to a bounds-checked call. `EvalReference` escapes it by accident, writing
through a property chain, which Fable emits as a raw index.

Every index involved is either a step index, below `MaxArraySize` by construction, or a
multiplexer select, kept in range by the masking invariant. On a port to .NET the `#else` branch
is already plain indexing, so the shim disappears rather than needing to be unpicked.

## How a simulation is checked

Three layers, in increasing breadth:

- `ComponentSemantics.fs` — each component type against an independent reference, exhaustively at
  small widths; `Properties.fs` drives the >32-bit paths with FsCheck.
- `GoldenModel.fs` `golden …` — whole fixture projects, every output on every cycle, against a
  stored file.
- `GoldenModel.fs` `reducers agree …` — **two simulations of the same design, one driven through
  `EvalReference` and one through the installed reducers, compared output by output.** This is what
  makes converting another component type to a compiled reducer a safe change. Adding a reducer
  without it is not.

## What it runs at

The benchmark is the `5eratosthenes` demo — the EEP1 CPU, 349 component reductions per clock —
running the **large** `sieve` program for its first 100,000 cycles. Warmed past the optimising
tiers, median of three, `runFastSimulation` as the entry point. "Old" is the simulator before
compiled reducers (`e09aa9b17`); "new" is this design.

| cycles/ms | old | new | new/old |
|---|---:|---:|---:|
| **.NET** (`DOTNET_TieredCompilation=0`) | 82.9 | 211.3 | 2.5x |
| **V8** (Electron renderer) | 11.2 | 132.9 | 11.9x |
| **V8 / .NET** | 7.4x slower | **1.6x slower** | |

The bottom row is the point. Most of what the old simulator cost in V8 was overhead .NET never
paid: a 100kB dispatcher V8 could not inline, fable-library's bounds-checked `item`/`setItem` on
every port access, and heap `BigInt` for constants and bus compares. Removing those leaves the two
runtimes doing much the same work, and .NET keeps only the structural edge its real structs and
cheap non-virtual dispatch give it.

Note also how far apart the two *speedups* are: 2.5x against 11.9x for the same change. The .NET
figure is diluted by RAM, which is still on the general path in both designs and costs the same in
both. A change measured only under .NET would have looked five times less valuable than it is.

## Measuring it

Simulation speed must be measured **in the app**, not under .NET - see the two speedup columns
above. Errors in both directions are easy to make: .NET can make a change look like a 5x win that
is worth 1.2x in Chromium, and can understate a real 11.9x as 2.5x.

What works:

- Drive the app over the DevTools protocol (`scripts/inspect-canvas.js` and the CDP directly).
  `Profiler` gives self time per function; `HeapProfiler.startSampling` gives allocation by site.
- **`FilesIO.loadAllComponentFiles` reads a project headlessly**, so a benchmark or a script can
  open the demos with nothing running. `Helpers.jsonStringToState` tries `Common/SimpleJsonDotNet.fs`
  before Thoth: the app's writer is the vendored SimpleJson, which encodes a union as a single-key
  object where Thoth's `Decode.Auto` expects an array, so a Thoth-only .NET branch fails on every
  `.dgm` the app has ever written. Sheets the .NET side wrote itself are Thoth-encoded, which is
  why both are tried.
- **Check the design is actually computing.** The `5eratosthenes` demo's `sievesmall` program
  finishes in well under 25,000 cycles and then spins in a self-jump; timing it measures a halted
  CPU. Use the large `sieve` program, and confirm activity — RAM words written, distinct values
  taken by clocked components — rather than assuming.
- **Time the same work every repetition.** The sieve's cost varies by phase, so successive windows
  of one long run measure different things. Build a fresh simulation per measurement and time the
  first N cycles.
- **Median, not minimum.** The distribution is a tight cluster with occasional 2x-fast outliers.
- Steady state allocates about 1 byte per clock. If a change makes the loop allocate, that is a
  bug in the change.

## What building a simulation costs

Compiled reducers move work from the run loop to the build, so the build is where to look for a
regression. On `3cpu` (349 components; `CODEMEM`, a 64K x 16 `AsyncROM1`, and `DATAMEM`, a 64K x 16
`AsyncRAM1`), under .NET:

| | |
|---|---:|
| whole `startCircuitSimulation` | 16.0 ms |
| of which `installReducers` | 0.38 ms (2.4%) |

So the per-component reducers cost a low single-digit percentage of a build that is itself
dominated by gathering and linking. It scales with component count, and with ROM *size* rather
than ROM count: a ROM's lookup table is `2^AddressWidth` words, capped by
`maxRomTableAddressWidth`, above which the component keeps the general path.

One trap to keep clear of: `reducerFor` takes no clocked/combinational flag, and must not acquire
one it does not read. An unread flag makes `installReducers` build every reducer twice — which for
a ROM means building and retaining two copies of its lookup table. If a reducer is added that
genuinely depends on the pass (the hybrid asynchronous RAM is the only candidate), the flag comes
back and the caller goes to two calls *for that component only*.

## Known debt

Roughly in order of how much it costs.

**Every component lookup goes through a composite `Map` key, and that is the build's shape
problem.** `fs.FComps`, `fs.FCustomComps` and `fs.WaveComps` are all keyed by
`FComponentId = ComponentId * ComponentId list`. Measured on .NET, 200,000 `containsKey` lookups
into a 10,000-entry structure:

| key shape | time | allocated |
| --- | ---: | ---: |
| `Map<int,_>` | 8.4 ms | 0 MB |
| `Map<ComponentId,_>` (a reference DU over int) | 60.6 ms | 0 MB |
| `Map<int * int list,_>` | 93.7 ms | 122 MB |
| `Map<ComponentId * ComponentId list,_>` — what is used | 77.2 ms | 0 MB |
| `Dictionary<int,_>` | 0.4 ms | 0 MB |
| array index | 0.2 ms | 0 MB |

So a dense integer into an array is around **two hundred times** faster than the lookup a build
does constantly, and this is the first place to look when .NET simulation startup is the thing
being made fast. Two traps the table also records. Structural comparison of a composite key
compares its elements as `obj`, so any VALUE-type element is boxed on every comparison - which is
why the raw-int tuple both allocates and loses, and why the reference wrapper that looks wasteful
is in fact the best of those three. And `[<Struct>]` on an id makes it a value type, so it hits
exactly that trap: see the note above the id types in `CommonTypes.fs`, where the same effect is
measured end to end as +1.9% allocation across a whole 3cpu build.

The direction, when it is worth doing: **allocate contiguous integers as ids at build time**, and
let the id type carry that integer so everything else about a component is an array lookup from
it. That makes the id both a name and an index, which is what the design-time `ComponentId`
already is per design and what a simulation-time id could be per build. The condition is that any
such array is built **write-once**, or is encapsulated tightly enough that nothing outside can
mutate it - otherwise this trades a slow lookup for the class of bug `docs/mutableState.md`
exists to prevent. A build-scoped index also must not be persisted: anything saved or compared
across builds stays a structured name.

**`EvalAlgebraic` duplicates `EvalReference`.** ~2,500 lines expressing one set of component
semantics twice, kept in step only by discipline. The build path duplicates with it:
`orderCombinationalComponents`/`…FData`, `checkAndValidate`/`…FData`,
`buildFastSimulation`/`…FData` differ only in which reducer is called and which array is
initialised. Parameterising the build path on a small `Evaluator` record would delete the twins;
the indirect call would land on the build path only, and the run loop must keep calling
`fc.ReduceComb` directly or the dispatch this design removes comes straight back.

**`FastComponent` carries five unrelated concerns**: step data, the reducers, ordering scratch
(`Touched`, `NumMissingInputValues`, `DrivenComponents`), naming (`FullName`, `SimSheetName`,
`SimSheetNamePath`, `SheetName`, `FLabel`) and Verilog output names. Only the first two are needed
once the build is over. The ordering scratch is the easy one — it is build-only state on a hot
record and could live in a table inside `FastOrder`.

**The façade is notional.** `Simulator.fs` is nominally the entry point, but twelve modules
outside `Simulator/` reach into `FastRun`, `FastExtract`, `FastCreate` and the evaluators. Some of
that is legitimate: the waveform viewer needs bulk access to step arrays and routing it through a
narrow API would mean copying. The honest position is two supported entry points — `Simulator` for
run control, `FastExtract` for reading results — and nothing outside `Simulator/` touching
`FastCreate`, `FastOrder`, `FastBuild` or the evaluators.

**RAM is still on `EvalReference`.** It has no compiled reducer, so a design's memories keep the
general path while everything around them has been specialised. What used to be written here — a
`Map` of contents snapshotted into the step array every clock — is gone; `RamStore.fs` replaced it
and [ramRepresentation.md](ramRepresentation.md) records what that was worth and what was left
undone. What remains is the reducer itself, and one obstacle to it: the asynchronous RAM is the
hybrid component, reduced once as clocked and once as combinational, and `reducerFor` deliberately
takes no flag to tell those apart (see the trap above). A `RAM1` reducer needs no flag; an
`AsyncRAM1` one brings it back for that component alone.

**`GraphBuilder` defines its own `extractBit`/`packBit`,** duplicating `EvalKernel`'s.

**The per-component loop scaffolding** — `Array.iter` with a closure per component — is around a
fifth of run time on the sieve. It was left alone deliberately: the ways to remove it either spread
the unchecked-indexing shim out of `EvalCompiled`, or add a precomputed flat array of reducers that
duplicates `FClockedComps`/`FOrderedComps` and must be kept in step with them. Both are better
decided as part of a rewrite of the execution layer than retrofitted.
