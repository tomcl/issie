# Driving Issie from outside it

`scripts/inspect-canvas.js` reads what the draw block is showing. This is the other half: sending
Issie messages, asking what state it is in, and knowing when it has finished responding.

```bash
node scripts/drive.js state                 # what the app currently is, as JSON
node scripts/drive.js refs                  # what is holding a simulation, and heap use
node scripts/drive.js waves                 # what the waveform viewer shows, holds and awaits
node scripts/drive.js wait <expr> [secs]    # block until a condition of the app holds
node scripts/drive.js commands              # the commands send accepts
node scripts/drive.js send <name> [arg]     # send one, and wait for the render it causes
node scripts/drive.js script <file.txt>     # a command per line, each awaiting its render
```

Issie must be running a **debug build** — `npm run app -- -d`, or `npm run dev`, since the harness
is published only when `JSHelpers.debugLevel > 0`. It can start simulations and open projects,
which is not something a shipped build should offer a page.

## Why it exists

The alternative is synthesising DOM events and reading rendered text back, and that fails in three
ways that cost real time:

- **Waiting.** With nothing to wait *on*, every step becomes a guessed `sleep`. Too short and it
  races; too long and a five-step sequence takes a minute.
- **Clicking.** A click has to find the right element, and "the element whose text is X" is wrong
  often enough to matter — a tab that needs the event on its `<a>` and not its `<li>`, a menu left
  open over the canvas, a button whose text is `See Problems` because the design has an error.
- **Reading.** Text scraped from the DOM can be a frame out of date while looking authoritative.
  This is the one that causes wrong conclusions rather than slow ones.

## What `waves` is for

```json
{ "open_": true, "startCycle": 0, "shownCycles": 66, "samplingZoom": 1, "cursor": 0,
  "selected": 8, "detailed": 8, "missing": 0, "drawn": 8, "drawnStart": 0, "stale": 0,
  "fetchInProgress": false }
```

Three separate questions the viewer's bugs live between: what the controls ask for (`startCycle`,
`shownCycles`, `samplingZoom`), what the cache holds for the waves being drawn (`missing` is how
many of them have not got that window), and whether a fetch is on its way to close the gap
(`fetchInProgress`). `drawn` counts the waves with an SVG on screen, which may have been made for
an older window - that is the deliberate fallback, not a fault.

At rest, `missing` is 0 and `fetchInProgress` is false. `missing > 0` with no fetch in progress and
no update running is the fault the stale-waveform banner exists to make visible.

`drawnStart` is where the first drawn waveform actually starts. Under a fast scroll it runs behind
`startCycle` and should be seen to FOLLOW it: a picture that sticks while `startCycle` moves is data
arriving and not being drawn.

Driving it: `startWaveSim` (the viewer's own Start button - `startSimulation` is the STEP
simulator's), then `waveSelect <n>`, `ramSelect <n>`, `waveView "<start> [shown] [zoom]"`,
`waveCursor <n>` and `waveConfig <lastClock>` (the Configure dialog's cycle count, which takes
effect on the next start).

`sidecar.connected` says whether a socket is open, which it is once anything has fetched. Nothing
needs to wait for the sidecar to start any more - the transport does that - so a script can ask for
data as soon as the viewer is running and simply wait for it to arrive:
`wait "waves.missing === 0" 90`.

## Waiting for a condition rather than for a duration

`send` waits for the render its message causes, which settles most things. What it does not settle
is anything that continues afterwards: a build that runs on after the message that started it, a
fetch answered by another process, a sidecar that is still starting up. For those, `wait` blocks on
what is actually being waited for.

```bash
node scripts/drive.js wait "sidecar.connected"
node scripts/drive.js wait "waves.state === 'Success'"
node scripts/drive.js wait "waves.missing === 0 && !waves.fetchInProgress" 300
```

The expression is JavaScript with `state`, `waves`, `refs` and `sidecar` in scope - the four
reporters - and the optional last argument is a timeout in seconds (60 by default). A wait that
times out prints what the app actually was, which is the thing worth knowing when a sequence has
gone wrong. `wait` works as a line in a script too, so a whole setup runs with no sleeps in it:

```
openProject C:/Users/me/Desktop/myProject
wait state.openSheet === 'eep1'
simulateIn sidecar
rightTab Simulation
simSubTab WaveSim
startWaveSim
wait waves.state === 'Success'
waveSelect 8
wait sidecar.connected
wait waves.missing === 0
```

That sequence takes about two seconds. Written with sleeps long enough to be safe it took thirty,
and still needed a second `startWaveSim` when the first one raced the canvas.

## A script

One `<name> [arg]` per line; `#` comments and blank lines ignored. Each line waits for the render
its message caused before the next is sent, so no sleeps are needed anywhere.

```
openProject C:\Users\me\Desktop\myProject
openSheet main5
rightTab Simulation
simSubTab StepSim
startSimulation
```

## What `refs` is for

```json
{ "inModel": 120084, "stepCache": 120084, "waveCache": 0, "truthTable": 0,
  "waveSimSheets": ["main5"], "usedHeapMB": 450, "heapLimitMB": 4192 }
```

A `FastSimulation` is reachable from several places and only one of them is the model, so "is it
still in memory" cannot be answered from the model alone. It also cannot reliably be answered from
the heap: `usedJSHeapSize` counts typed arrays, does not shrink until a collection runs, and says
nothing about *which* reference is keeping something alive. Ending a simulation shows
`inModel` and `stepCache` drop to 0 immediately, while `usedHeapMB` stays where it was until the
next GC — the counts are the truthful signal.

This was added because finding one retained simulation took a heap snapshot with a retainer path
and an A/B build. `refs` answers the same question in one call.

## Measuring simulation speed: `benchmark` and `rerun`

```bash
node scripts/drive.js send benchmark "20 550"   # build the open sheet, then time 20 cycles
node scripts/drive.js send rerun 20             # time it again, without rebuilding
```

```json
{ "sheet": "main6", "comps": 480342, "syncComps": 24577, "ordered": 393217,
  "maxArraySize": 550, "typedArrayMB": 1135.4, "usedHeapMB": 3689,
  "steps": 20, "medianMs": 1900.2, "compStepPerMs": 5054,
  "seriesMs": [1909.2, 1767.8, 1707.7, 1558.4, 1900.2, ...] }
```

Simulation speed must be measured in the app rather than under .NET
([simulatorStructure.md](simulatorStructure.md)), and these are how. Four things they are shaped
around, each of which caused a wrong answer before it was:

- **The build is not the run.** On a 480,000-component design the build is ~33s and a run is
  ~1.9s, so a profiler wrapped around `benchmark` profiles the build. Wrap it around `rerun`,
  which reuses what `benchmark` built.
- **`seriesMs` is every repetition, in order.** The median alone cannot show whether the warm-up
  was long enough. A first repetition slower than the rest means the JIT is still tiering up; on a
  small design that is the first 1–2ms and nothing after, and on a large one it is invisible.
- **The second argument is the step array size**, overriding `SimulationView.Constants.maxArraySize`.
  It is the only thing that changes the distance between the words a clock cycle touches without
  changing the work, which is what makes "is this design memory-bound?" a question with an answer.
- **A benchmark retains its simulation** so that `rerun` can use it, and a heap left near its limit
  slows down everything measured afterwards. `endSimulation` drops it. Compare only measurements
  taken in comparable heap states, and read `usedHeapMB` in the reply to know which one you were in.

Run-to-run variance on a laptop with performance and efficiency cores is up to 2x, so repeat, and
do not believe a single number.

## Adding a command

`send` takes a **name from a fixed table**, not a serialised `Msg`. A `Msg` is an F# union carrying
models, canvases and functions; nothing useful survives a round trip through JSON, and a
dispatch-anything surface in a debug build is a hazard for the sake of messages nobody wanted to
send. Add a row to `commands` in [`src/Renderer/UI/DevHarness.fs`](../../src/Renderer/UI/DevHarness.fs)
— that is the intended way to extend it. Each row sends the message the corresponding UI element
sends, so driving the app from here and driving it by hand cannot diverge.

## How the render signal works

`Renderer.view'` calls `DevHarness.recordModel` before the view runs and `DevHarness.renderDone`
after it. Waiting callbacks are resolved on the **next animation frame** rather than immediately:
the view returning means React has been given the new elements, not that the DOM holds them, and a
caller waiting for a render wants the state it then reads to be the state it waited for.

A command that changes nothing renders nothing, so `send` bounds its wait rather than hanging, and
reports `(no render within the timeout)` if none arrived.

## A window nobody can see does not render

Chromium treats a covered window as hidden, and a hidden page runs no `requestAnimationFrame` —
which is how Issie renders, since Elmish batches the view into one. A dev window left behind the
terminal driving it therefore stops rendering altogether: every `send` reports no render, every
`Input.dispatchMouseEvent` takes five seconds to be acknowledged, and a CPU profile of a drag shows
the renderer idle throughout. Nothing in any of that says "the window was covered"; it reads as the
app being slow, which is the worst possible answer to give someone measuring why the app is slow.

`scripts/start.js` turns occlusion tracking off for this reason, so `npm run dev` renders whether
or not its window is visible. If you launch Electron yourself, pass the same switches:

```
--disable-features=CalculateNativeWinOcclusion --disable-backgrounding-occluded-windows
--disable-renderer-backgrounding
```

`document.visibilityState` is what to check when results stop making sense: `hidden` while the
window is plainly on screen means occlusion tracking has decided otherwise, and every timing taken
in that state is worthless.
