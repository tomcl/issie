# Global mutable state: policy and cleanup list

Issie's model code is immutable: state lives in the Elmish `Model`, updates go through
lens/prism composition, and there are no `for` loops or side effects. Module-level
`let mutable` is a deliberate exception, not a shortcut.

## When a global mutable is allowed

Only two reasons count:

1. **Performance.** Going through the model would cost something measurable — a cache on a hot
   path, or state written far more often than the model is rebuilt. Simulation caches and
   fast-simulation internals are the clear cases.
2. **It genuinely is not model state.** DOM references, scroll positions read inside browser
   event handlers, Electron main-process state, and debug flags set once at startup have no
   sensible place in an Elmish model.

Everything else belongs in `Model`. A global mutable used as "somewhere to put this value until
the next message arrives" is the pattern to avoid: it survives project close, it is invisible to
undo, and it makes the update function's behaviour depend on history that is not in the model.

**Before adding one, check the performance claim rather than assuming it.** `ModelHelpers.reduce`
and `reduceApprox` enumerate fields explicitly rather than comparing whole records, so adding a
`Model` field costs nothing for view memoisation, and an update function that already rebuilds the
model record pays nothing for one more field. Every mutable removed from this list so far turned
out to have no performance argument at all.

## Audit

Function-local `let mutable` inside a loop is an implementation detail and is not covered here —
this is about module-level state.

### Justified: caches and hot paths

| Where | What |
|---|---|
| `Simulator.fs` | `simCache`, `simCacheWS` |
| `UI/ModelHelpers.fs` | `waveSimCostMemo` — the waveform configuration dialog reads the design's per-cycle cost on every render |
| `FastSim/FastCreate.fs` | `stepArrayIndex`, `stepArena` — build-scoped allocation state, reset by every build; threading either through the build would put plumbing in a dozen signatures for two leaf call sites |
| `UI/TruthTable/TruthTableView.fs` | `selCache` |
| `Common/TimeHelpers.fs` | `executionStats`, `instrumentation` |

`Common/Helpers.fs` has a `lastKey` / `lastValue` pair inside `memoizeBy`. It is local to each
memoised function rather than module-level, so it is out of scope here — noted only because it
looks like the exception and is not one.

### Justified: not model state

| Where | What |
|---|---|
| `DrawBlock/SheetDisplay.fs` | `mountedCanvas`, `modelScrollPos` — DOM refs |
| `DrawBlock/Sheet.fs` | `recentProgrammaticScrollPos`, `scrollSequence`, `viewIsAfterUpdateScroll` — scroll bookkeeping driven by DOM events |
| `Main/Main.fs` | `mainWindow`, `splashWindow` — the two Electron windows; `closeAfterSave`; `appStarted` — process lifecycle, and there is no Elmish model in the main process at all |
| `UI/KeyBindings.fs` | `modelContext` — a cached projection of the model for DOM handlers, which cannot see it and must decide `preventDefault` synchronously; `ctrlHeld` and `spaceHeld` — physical modifier state, wanted on its edges and across focus loss, and a held key is not something the shortcut table can express; `keyLog` — debug-only log of what the dispatcher decided |
| `Interface/JSHelpers.fs` | `debugLevel`, `loggingMemory`, `memSize` |
| `UI/DevHarness.fs` | `latestModel`, `latestDispatch`, `waitingForRender` — the outside world's handle on a running Issie, in the same way `KeyBindings.modelContext` is the DOM's. Written by the view wrapper on every render and read only by the debug-build harness; there is no Elmish message that could carry them, since the point is to answer between renders |
| `Simulator/SimTypes.fs` | `SimulationBudget.maxTypedArrayBytes`, `SimulationBudget.maxHeapBytes` — how much memory a simulation may take, which is a fact about the machine rather than about the design. Read once from Electron at renderer startup (`setBudgetsFromMachine`) and never written again; the values compiled in are the fallback for a run with no Electron to ask, which is every run of the test suite |
| `Common/Log.fs` | `enabled` — which log categories are on, switched from a menu, a command-line flag or the console; `ring` / `ringNext` — a fixed buffer of the last few hundred lines, written one slot at a time so that logging allocates nothing, and read from outside the app the way `KeyBindings.keyLog` is; `warnedKeys`; the five counters (`msgCount`, `renderCount`, `updateMsTotal`, `slowestMs`, `slowestName`) with `lastSummary` and `summaryInterval`, updated on every message and every render, which is exactly the write frequency that rules out a model field |
| `Simulator/CanvasExtractor.fs` | `debugChangedConnections` — records what a change check saw, for tracing only |
| `Playground.fs` | `Memory.modelCopy` — memory-leak investigation; never read, and its only writer is commented out in `Renderer.fs` |
| `UI/Update.fs`, `UI/UpdateHelpers.fs` | `uiStartTime`, `lastMemoryUpdateCheck` — timing instrumentation |
| `UI/MainView.fs` | `lastDragModeOn`, `lastMemoryCheckTime` — view-time bookkeeping |
| `UI/ModelHelpers.fs` | `asyncJobs` — async job queue |

### To clean up: probably should be in `Model`

These look like UI state that leaked into globals. None has a stated performance reason, and each
would need the same judgement applied before moving: confirm the write frequency, then move it.

- **`UI/UpdateHelpers.fs` — `rightClickElement: RightClickElement`.** Which element was
  right-clicked, set in an event handler and read when the menu action fires. Cross-message
  bookkeeping of exactly the kind that belongs in `Model`. Check first whether it is written on
  every mouse event or only on right-click; if the latter, there is no performance argument.
- **`UI/MemoryEditorView.fs` — `dynamicMem: Memory1`.** Working state of the memory editor dialog.
  `PopupDialogData` already holds dialog state and is the obvious home. Note this one may be
  written per keystroke in a large memory table, so measure before moving.

Not everything in this position moves into `Model`. A mutable that exists because a **DOM handler
cannot read the model** — it must decide `preventDefault` synchronously, and the model is not
reachable from inside the handler — belongs in the "not model state" table above instead. That is
what `KeyBindings.modelContext` and `ctrlHeld` are.
