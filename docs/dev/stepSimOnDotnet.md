# Run the step simulator under .NET

Done, except a RAM's contents - see the last section. Kept because it records why each piece is
where it is, and what is still missing.

## Why

The waveform simulator can be run by the .NET sidecar; the step simulator cannot, and nothing in
its path consults `Model.SimulateInRenderer`. `SimulationView.simulateWithProgressBar` calls
`FastRun.runFastSimulation` on the renderer's own `FastSimulation`, so "Run to clock N" is always
V8 — including the components/ms the progress bar quotes, which is why that number can move
against a change measured under .NET and disagree with it in sign.

On `largeTest/main5` (120,084 components, step-simulator array size 550) one clock of every
component costs, per `runFastSimulation`:

| | comp-steps/ms |
| --- | ---: |
| .NET | 27,000 |
| V8 | measured 1.8-2.2x worse on the same builds |

So the step simulator on a large design is the one place left where the runtime that is being
retired is the one doing the work.

## What already exists

Most of it. Nothing below needs a protocol change.

| | |
| --- | --- |
| `SimBuild`, `SimRun`, `SimEnd` | build, chunked run towards a target cycle with a time budget, teardown - the same contract the renderer's own progress loop uses |
| `SimSetInputs` | set top-level input values at a cycle |
| `SimRead` | sampled values for any list of (component id, access path, output port) at any cycle. **Any width**: it computes `wordsPerSample` from the simulation and lays the reply out least-significant word first. The note in `Protocol.fs` saying signals wider than 32 bits are refused is stale and should go with this work |
| `SidecarClient.fs` | the renderer half of all of the above |
| `SimSession.firstValidCycle` | the earliest cycle still in the circular buffer - written for this case: "the waveform simulator sizes its array for the whole configured run and never reaches that; the step simulator is sized short and does" |
| `ModelHelpers.Constants.rendererArraySizeWhenSidecarSimulates` | the renderer already builds a SHORT local simulation when the sidecar is simulating, for structure only |
| `SimInterface.ISimulator` | already declares `SetInput` and `ReadStepPanel`, and `StepPanelSnapshot` already has the four sections below. Declared, not implemented |

## Where each part of the panel comes from

`SimulationView.viewSimulationData` reads the simulation in exactly four places, and they map onto
`StepPanelSnapshot`:

| panel section | today | over the wire |
| --- | --- | --- |
| top-level inputs | `FastExtract.extractFastSimulationIOs simData.Inputs` | `SimRead` — `(cid, [], port 0)` |
| top-level outputs | `FastExtract.extractFastSimulationIOs simData.Outputs` | `SimRead` |
| viewers, anywhere in the hierarchy | `FastExtract.extractViewers` | `SimRead`, with the access path |
| stateful components | `FastExtract.extractStatefulComponents` | see below |

The stateful section splits three ways, and only one of them is missing:

- **Register, RegisterE, Counter\*, DFF, DFFE** — `extractStatefulComponents` reads these from
  `fc.Outputs[0]`, so they are ordinary signals and `SimRead` already covers them.
- **ROM1** — contents are part of the component's type and never change
  (`RamStore.fixedOf`), so they need no simulation at all.
- **RAM1, AsyncRAM1** — the memory store, which is the one thing not readable over the wire.
  `SimInterface` says so deliberately ("reading a RAM's contents, which needs a row type that is
  declared in the waveform UI and has to move first"), and the waveform simulator already has the
  answer: `WaveSimRams.fs` shows the memory's contents only when the renderer is simulating, and
  otherwise says plainly that they are not available rather than showing a memory as it was before
  the first clock edge. **Say the same thing here.** Wrong contents look exactly like right ones.

## What was done

`SimulationView` gained one section - "the step panel, from whichever simulator is running" - and
every site that ran or read the simulation goes through it:

- `advanceTo` replaces the five `FastRun.runFastSimulation` calls. Local: runs and calls back at
  once, as before. Sidecar: `ensureBuilt`, `runTo`, then one `SimRead` of the panel's signals.
- `StepPanelData` is the cache, in the shape of `WaveData`'s: one snapshot, of one cycle of one
  session, so a value can only ever be read back for the cycle and epoch it was fetched for.
- `ioValues`, `viewerValues` and `statefulValues` replace the four `FastExtract` calls.
- `setInput` replaces `changeInput`: `SimSetInputs` and then a re-read, because the values on
  screen were computed from the input that just changed.
- `clockNow` is the clock a run advances from. Locally the `FastSimulation`'s own tick; in sidecar
  mode the model's, because the local simulation is never run and the sidecar's clock only ever
  goes forwards while the panel can be stepped back.
- The session itself moved to `Interface/SidecarSession`, out of `WaveProvider`, which compiles
  after `SimulationView`. The sidecar holds one session and now one module knows what it is.

The sidecar's session is shared with the waveform simulator. That is mostly a saving - a session
built for a long waveform run is reused rather than rebuilt - but the two are stepping the same
simulation, so setting an input here moves what a waveform simulation of the same design would
show. They are different tabs and starting a waveform simulation runs it again from its own
inputs, so nothing stale is drawn.

## How big a design it will take

The waveform simulator has a configuration dialog that prices the design and refuses a last clock
that will not fit (`FastCreate.maxLastClockFor`, `UIPopups`). The step simulator has no dialog: it
picks one number, how many cycles of history to keep, and used to take `Constants.maxArraySize`
whatever the design cost - so a design too big for it was refused by the build's own memory check,
in words about a waveform configuration the user was not looking at.

`ModelHelpers.stepSimCycles` binds the same budget to that one number:

| the design can afford | what happens |
| --- | --- |
| the full 550 cycles | it gets them |
| fewer, but at least `minStepArraySize` (20) | shortened to what it can afford, logged under `sim` |
| fewer than 20 | refused, in words about the design |

Measured against `maxCyclesFor`, which is the budget WITHOUT the runtime headroom
`checkSimulationFits` allows itself - deliberately the stricter of the two, so a size chosen here
is certain to pass the build's own check and the refusal happens where it can be explained.

Nothing real is shortened: every sheet of `largeTest` fits at 550, main6 included, at 2.4 MB a
cycle against the 968 cycles its budget allows. A generated hierarchy of 2048-bit buses is
shortened (4 MB a cycle, 250 cycles); one of 65536-bit buses is refused (116 MB a cycle, 8).

## What it measured

`largeTest/main5`, 120,084 components, step simulator, Run to clock 1000, read off the progress
bar - the same thing, on the same design, as the number that started this:

| | component-clocks/ms |
| --- | ---: |
| renderer | ~6,000 |
| .NET | **25,900** |

Values agree: `3cpu/eep1` stepped to clock 5 gives `NZCV=8, PCV=x0002, R0=x0002` in both modes.

## Still to do

- **A RAM's contents.** Shown as unavailable, the way `WaveSimRams` shows them. A command that
  reads a memory store over the wire would give both back at once.
- **Input values above 2^53.** `SimSetInputs` carries a value as two 32-bit words. A wider input
  is refused by name rather than sent truncated.
- **`ISimulator` is still not implemented.** `advanceTo` and the three value functions are the
  same shape as `RunTo`, `SetInput` and `ReadStepPanel`, and should become them - along with the
  waveform simulator's own path - rather than a second way of saying it.

## The one real difficulty

The step simulator's update path is synchronous today: a click runs the simulation and the next
render shows the result. Over the wire it cannot be. The waveform simulator has already solved
this - view code reads a cache synchronously, only the update function talks to an `ISimulator`,
and a reply carries the epoch it belongs to so a superseded answer is discarded rather than shown
(`SimInterface.fs`, and `docs/dev/sidecarInvariants.md` section C). Copy that; do not invent a
second pattern.

## Verification

- `CI=true npm run test` for the suite, and `Issie.GoldenModel` in particular.
- Drive it: `node scripts/drive.js` with `simulateIn sidecar`, then the step simulator on
  `largeTest/main5` run to clock 1000, and the same in renderer mode - the values shown must agree
  and only the speed differ.
- The progress bar's components/ms on that run is the number this exists to change. Measure it in
  both modes and record both here.
