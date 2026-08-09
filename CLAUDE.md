# CLAUDE.md

Guidance for Claude Code (claude.ai/code) working in this repository.

Issie (Interactive Schematic Simulator with Integrated Editor) is a digital circuit design and
simulation application written in F#, transpiled to JavaScript by Fable and run under Electron.

## Building, running, testing

```bash
build.cmd          # Windows: full setup - installs dependencies, builds, starts dev mode
build.sh           # Linux/Mac equivalent

npm run app        # start the app in whichever mode is already compiled - no recompile to switch
npm run dev        # hot reload; Main and Renderer compile in parallel
npm run dev:once   # one-shot compile + app, no watcher; near-instant when nothing changed
npm run debug      # includes assertions - slower
npm run dist       # production binaries
npm run typecheck  # dotnet build of Renderer.fsproj: F# type check without Fable
npm version patch  # release (master only): sync Version.fs, commit, tag, push - CI publishes
```

There is no lint command; the F# compiler is the check.

Fable skips recompiling (or in watch mode, starts the app before the silent background
recompile) only when every `.fs.js` is strictly newer than its `.fs` — see
[docs/BUILD_OPTIMIZATION.md](docs/BUILD_OPTIMIZATION.md). A source touched without changing its
emitted JS used to keep a permanently stale `.fs.js`, since Fable rewrites an output only when its
content changes; `scripts/refresh-stale-output.js` now bumps those after every compile. The
remaining trap is that watch mode adds the `DEBUG` define, so alternating
`dev`/`dev:once`/`compile` recompiles on each switch — `npm run app` sidesteps it by going
wherever the tree already is. **When verifying that a change still compiles under Fable, use
`node scripts/dev.js --once --no-app`, not `npm run compile`**: the latter leaves the tree in
`PRODUCTION` and costs whoever runs the app next a full recompile.

**You can see what the running app is drawing.** `npm run dev` opens a DevTools-protocol port, and
`scripts/inspect-canvas.js` reads the canvas through it — the whole draw block model serialised with
the same library that writes `.dgm` files, a readable summary of it, the SVG as rendered, a
screenshot, or arbitrary JavaScript evaluated in the renderer (which can click through the UI). Use
it instead of guessing when the drawing and the model might disagree.
See [docs/dev/inspectingTheCanvas.md](docs/dev/inspectingTheCanvas.md).

**You can make sheets from a program.** A list of components and logical connections, written as
data, becomes a laid-out `.dgm`, project or `.ldgm` library component — from plain .NET, with
nothing running. Use it for test schematics rather than building canvases by hand.
See [docs/dev/sheetDescriptionDsl.md](docs/dev/sheetDescriptionDsl.md).

`npm run test` runs the Expecto suite under `Tests/Issie.Tests` (`dotnet run`, not `dotnet test`).
It reaches the whole of `Renderer.fsproj`, so simulation, parameter resolution, the draw block and
even UI-module helpers can all be tested. Use it: a fix to simulation or parameter behaviour can
be pinned by a test rather than argued about.

**Don't run the whole suite by default — it is ~60s, and one group is seconds.** Per-group
timings and what to run when are in [Tests/README.md](Tests/README.md); `--filter Issie.<Group>`
runs one group: `dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock`.
Two-thirds of the suite's time is `Issie.VerilogCompiler`, which spawns node per parse — run it
when touching `src/Renderer/VerilogComponent/`, and note it is skipped automatically when the
`CI` environment variable is set (so CI runners never pay for it, and `CI=true npm run test` is
the fast full-suite run locally, ~20s).

Adding a test file takes two edits, and missing either fails silently: list it in
`Tests/Issie.Tests/Issie.Tests.fsproj` (compile order matters) and add its `tests` value to the
list in `Main.fs`.

## Things the code will not tell you

**Canvas is not the simulation graph.** `CanvasExtractor.fs` bridges them, stripping visual layout
to produce the electrical graph. Treating the two as interchangeable is the most common mistake.

**The fast simulator has three evaluators of the same component semantics** — `EvalReference` (the
specification), `EvalCompiled` (per-component reducers built when the simulation is built) and
`EvalAlgebraic` (the `FData` backend) — over a shared `EvalKernel`. Before changing any of them,
read [docs/dev/simulatorStructure.md](docs/dev/simulatorStructure.md): it has the layering, the two
invariants a compiled reducer depends on, how the three are held to agree, and how to measure
simulation speed without being misled (measure in the app, not under .NET).

**Component creation flows across four files** in this order: `CatalogueView.fs` (user picks a
type) → `Sheet.fs` (mouse placement) → `Symbol.fs` (visual representation and ports) →
`CanvasExtractor.fs` (simulation node).

**A parameterised sheet has no single signature.** It has a family of them, one per set of
bindings, so a custom component instance's port widths are a fact about the INSTANCE, not the
sheet: two instances of one sheet are meant to differ. `CanvasExtractor.signatureOfInstance` is the
only place that works them out, and placement, the properties pane, the simulator's custom
component check and `CustomCompPorts` all go through it — they held separate copies before, and the
copies disagreed. "Instance out of date" means differs from what its OWN bindings give it; compared
against the sheet instead, every parameterised design reports as changed.

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
  `DrawBlockTests.fs` builds symbols and routes wires under plain .NET, so an edge case can be a
  test rather than something to click at.
- **Text is measured against a browser canvas**, which is why `DrawHelpers.getTextWidthInPixels`
  creates that canvas on first use and not while the module loads — doing the latter made the whole
  draw block throw `JS only` under .NET, since symbol sizing measures text. Under .NET it instead
  reconstructs the width from a table of per-character advances, which `DrawBlockTests.fs` holds to
  within 1% of widths recorded from a real browser. Measure through that function; do not reach for
  a canvas of your own. Prefer asserting draw block *structure* — ports, edges, overlap,
  orthogonality — since a pixel width is only ever as good as that table.
- **Memory components** need special handling for RAM/ROM initialisation from `.ram` files.
- **Nothing prints unconditionally.** `src/Renderer/Common/Log.fs` is the only way to the console:
  `Log.warn` and `Log.error` always show, `Log.dbg Log.Wire $"..."` (and the other categories)
  shows only when that category is on, and `Log.out` is for a Development-menu item whose output
  *is* the point. Categories are switched live, with no rebuild — from the Development > Log menu,
  from `--log=wire,sim` at launch, or from `window.issieLog.on "wire"` in a console. The last few
  hundred lines are kept in a ring buffer readable from outside the app:
  `node scripts/inspect-canvas.js log`. A new `printf` outside a short allowlist fails
  `Tests/Issie.Tests/SourceHygiene.fs`, which is what keeps this true.
- **Timing instrumentation is off by default.** `TimeHelpers.instrumentation` is `Off`; the
  Development > Play menu turns on either per-interval times or the 10-second aggregate table.
  Message and render counts are always kept (five numbers in `Log.fs`) and summarised by the
  `perf` category. Memory monitoring is on the `CheckMemory` message.
