# Driving Issie from outside it

`scripts/inspect-canvas.js` reads what the draw block is showing. This is the other half: sending
Issie messages, asking what state it is in, and knowing when it has finished responding.

```bash
node scripts/drive.js state                 # what the app currently is, as JSON
node scripts/drive.js refs                  # what is holding a simulation, and heap use
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
