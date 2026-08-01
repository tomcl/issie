# CLAUDE.md

Guidance for Claude Code (claude.ai/code) working in this repository.

Issie (Interactive Schematic Simulator with Integrated Editor) is a digital circuit design and
simulation application written in F#, transpiled to JavaScript by Fable and run under Electron.

## Building, running, testing

```bash
build.cmd          # Windows: full setup - installs dependencies, builds, starts dev mode
build.sh           # Linux/Mac equivalent

npm run dev        # hot reload
npm run debug      # includes assertions - slower
npm run dist       # production binaries
npm run typecheck  # dotnet build of Renderer.fsproj: F# type check without Fable
```

There is no lint command; the F# compiler is the check.

**You can see what the running app is drawing.** `npm run dev` opens a DevTools-protocol port, and
`scripts/inspect-canvas.js` reads the canvas through it — symbol geometry and wire routing from the
model, the SVG as rendered, a screenshot, or arbitrary JavaScript evaluated in the renderer (which
can click through the UI). Use it instead of guessing when the drawing and the model might disagree.
See [docs/dev/inspectingTheCanvas.md](docs/dev/inspectingTheCanvas.md).

`dotnet test` exists but the tests are outdated and may not work. The JS simulator harness under
`simulator_tests/js` no longer compiles either: its fsproj references
`src/Renderer/Simulator/SimulatorTypes.fs`, which has since been split into `SimGraphTypes.fs` and
`SimTypes.fs`.

## Things the code will not tell you

**Canvas is not the simulation graph.** `CanvasExtractor.fs` bridges them, stripping visual layout
to produce the electrical graph. Treating the two as interchangeable is the most common mistake.

**Component creation flows across four files** in this order: `CatalogueView.fs` (user picks a
type) → `Sheet.fs` (mouse placement) → `Symbol.fs` (visual representation and ports) →
`CanvasExtractor.fs` (simulation node).

**Fable emits `.fs.js` and `.fs.js.map` next to every `.fs`.** When JS behaviour disagrees with the
F# you just wrote, read the emitted `.fs.js`.

**Paket manages F# dependencies and npm manages JavaScript ones**; the two must stay in sync.

**All file I/O goes through the Electron main process**, not the renderer.

**The Verilog grammar is Nearley**: `VerilogGrammar.ne`, compiled with `npx nearleyc`.

## File formats

- `.dgm` — one circuit diagram, JSON. Canvas state is `(Component list * Connection list)`.
- `.dprj` — project marker, empty.
- `.ram` — memory initialisation data.

Sheets are continuously auto-backed-up to a `backup/` subdirectory of the project.

## Conventions that differ from the defaults

These are enforced across the codebase, so following the surrounding code is not enough — the
defaults would teach the wrong pattern.

- **State updates go through lens/prism composition**, not record-copy syntax:
  `model |> Optic.set (sheet_ >-> symbols_ >-> label_) newLabel`. Lenses are defined in
  `ModelType.fs` for every record field.
- **Strictly immutable.** No `for` loops, no `mutable`, no side effects in model code. Use `map`,
  `fold`, `filter`, pipelines and recursion. Module-level `let mutable` is allowed only for a
  measured performance reason or for state that genuinely is not model state (DOM refs, Electron,
  debug flags) — never as somewhere to park a value until the next message. See
  [docs/mutableState.md](docs/mutableState.md) for the policy, the audit of existing ones, and the
  cleanup list.
- **No nulls** — `Option` and `Result` throughout.

## Common gotchas

- **FastSim masking invariant**: every value stored in a step array is already within its bus
  width. Readers (bus compare, mux selects, memory addressing, waveform extraction) never mask;
  a reducer in `FastReduce.fs`/`FastReduceTT.fs` must mask its result exactly when the operation
  can overflow the width (add, multiply, not, shift, constants) and not otherwise. Beware width
  exactly 32 on the uint32 path: `1u <<< 32` wraps to `1u`, so a `(1u <<< w) - 1u` mask is wrong
  there — either special-case it or rely on uint32 wrap-around.
- **Elmish timing**: some updates need `Cmd.OfAsyncImmediate` with a delay to stay in step with the
  UI.
- **Wire routing** is a complex state machine — exercise edge cases when changing it.
- **Memory components** need special handling for RAM/ROM initialisation from `.ram` files.
- **Debug tracing** is gated on `JSHelpers.debugTraceUI`; memory monitoring on the `CheckMemory`
  message.
