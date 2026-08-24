# Run the step simulator under .NET

Plan, not yet implemented.

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

## Order of work

Each step compiles and is separately testable.

1. **The run path.** `simulateWithProgressBar`, the step-forward and step-back buttons, and
   `simulationClockChangeAction` call `SidecarClient.simRun` when `not model.SimulateInRenderer`.
   This is the whole of the measured complaint, and it can go in before anything is read back.
2. **The build path.** The step simulator's build sends the design and issues `SimBuild`, keeping
   the renderer's own build for structure, sized by `rendererArraySizeWhenSidecarSimulates` as the
   waveform simulator's is.
3. **A step-panel cache**, in the shape of `WaveData.source`: the last snapshot, the epoch and the
   cycle it is of. Filled from the update function by one `SimRead` naming every signal the panel
   shows; read synchronously by the view. One request, not one per row - the panel is redrawn on
   every render.
4. **The four read sites** take their values from the cache in sidecar mode.
5. **`changeInput`** becomes `SimSetInputs`.
6. **RAM and AsyncRAM** get `WaveSimRams`'s message.

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
