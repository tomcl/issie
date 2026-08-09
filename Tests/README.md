# Issie tests

```bash
npm run test        # the whole suite: ~249 tests, ~55s (~18s with VerilogCompiler excluded)
```

That runs `dotnet run --project Tests/Issie.Tests -c Release`. It is `dotnet run`, not
`dotnet test`: the suite is an Expecto executable, and `dotnet test` will not find it.

**Run one group, not the suite** — a group answers most questions in a couple of seconds:

```bash
dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock
dotnet run --project Tests/Issie.Tests -c Release -- --filter-test-case Register --summary
```

## Runtimes, and what to run

Group runtimes (Release, warm build; add ~4s of startup per invocation):

| Group (`--filter Issie.<name>`) | Tests | Time |
|---|---:|---:|
| `VerilogCompiler` | 11 | **39s** |
| `ComponentSemantics` | 42 | 9s |
| `GoldenModel` | 3 | 3.4s |
| `Properties` | 23 | 1.0s |
| `VerilogOutput` | 45 | 1.1s |
| `SheetDescription` | 18 | 1.0s |
| everything else (`Algebra`, `DrawBlock`, `InstanceSignatures`, `KeyBindings`, `Library`, `ParameterScenarios`, `ParameterUI`, `Persistence`) | 107 | ≤ 1s each |

`VerilogCompiler` dominates: it spawns **node** for every parse (the real nearley parser, the
same one the editor runs), so it needs node on PATH and costs ~2/3 of the suite. Because of
that it is **skipped whenever the `CI` environment variable is set** (GitHub Actions sets
`CI=true`) — see `Main.fs`. To skip it locally the same way:

```bash
CI=true npm run test          # 238 tests, ~18s of test time
```

Run `Issie.VerilogCompiler` explicitly when touching anything under
`src/Renderer/VerilogComponent/`; otherwise the fast groups cover the change.

The suite references `Renderer.fsproj` directly, so it reaches all of the application code —
simulation, parameter resolution, persistence, the draw block and UI-module helpers — under plain
.NET, with no Electron, no browser and no Fable step.

## What is here

| File | Tests | Covers |
|---|---|---|
| `VerilogOutput.fs` | 45 | The Verilog text `Verilog.getVerilog` emits: structural invariants over every component, emitted expressions evaluated against the simulator's own reference, and constructs that were once emitted wrongly. |
| `ComponentSemantics.fs` | 42 | Every component type simulated in a minimal circuit and compared against an independent reference. Exhaustive over all inputs at width 3. |
| `AlgebraTests.fs` | 32 | Truth-table algebraic simulation: the `evalExp` simplifier, append handling, and symbolic simulation end to end. |
| `InstanceSignatures.fs` | 26 | What a custom component instance's ports are, and keeping instances in step with the sheet inside them: per-instance signatures, port add/delete/rename, slot identity across a rename, and the out-of-date flag for parameter-only edits. |
| `Properties.fs` | 23 | FsCheck properties: the parameter expression language against a reference evaluator and through render/parse, plus its name rule, unary minus and constraint checking; number conversions; the >32-bit bigint simulation paths. |
| `ParameterScenarios.fs` | 19 | Parameterised sheets instantiated at different bindings and simulated: per-instance resolution, propagation down a hierarchy, what is saved while computed values are displayed, and error reporting. |
| `SheetDescriptionTests.fs` | 18 | The sheet-description DSL and the layout that realises it — port resolution, error messages, placement, parameters, and a save/reload round trip. |
| `ParameterUI.fs` | 15 | The two gates that decide how much of the parameter feature the UI shows, and the totality of instance bindings, as pure functions of the loaded components. |
| `VerilogCompiler.fs` | 11 | The Verilog *input* compiler end to end: real nearley parse via node, semantic checks, synthesis to a sheet, and the simulated behaviour of that sheet. Needs `node`; skipped when `CI` is set. |
| `KeyBindingTests.fs` | 7 | The shortcut table: every `ShortcutId` bound, nothing shadowed, nothing that can never fire — all invisible at runtime otherwise. |
| `DrawBlockTests.fs` | 5 | Symbols built and wires routed with nothing running, plus the .NET text-width reconstruction checked against widths recorded from a real browser. |
| `GoldenModel.fs` | 3 | Whole fixture projects simulated for many cycles, every output and clocked value compared against a stored golden file. |
| `LibraryTests.fs` | 4 | What a library sheet shows of itself in the sheet trees, and every shipped `.ldgm` read and its sheet loaded. |
| `PersistenceTests.fs` | 10 | Project names and directories, the recent list, a canvas through the `.dgm` save path and back in through the load path, and every demo project loaded by `FilesIO`. |

Support files, which hold no tests: `TestFixtures.fs` loads fixture projects from disk, through
`FilesIO` exactly as the app does — so a fixture that loads is itself evidence the production
loader works headlessly; `CanvasBuilder.fs` builds canvases, loaded components and symbols
programmatically; `Main.fs` is the entry point and the list of test groups.

## Adding a test

Two edits, and missing either fails silently rather than loudly:

1. Add the file to `Tests/Issie.Tests/Issie.Tests.fsproj`. **Compile order matters** — F# compiles
   in listed order, so a file must come after anything it uses.
2. Add its `tests` value to the list in `Main.fs`.

Tests run `Sequenced`. That is deliberate: building a `FastSimulation` is not re-entrant, because
`FastCreate.stepArrayIndex` is a module-level mutable, so tests that simulate cannot run in
parallel.

Building a circuit by hand is rarely the best way to write a test. Either use `CanvasBuilder`, or
describe the sheet with the DSL in `SheetDescription`/`SheetLayout` and let it lay the sheet out —
see [../docs/dev/sheetDescriptionDsl.md](../docs/dev/sheetDescriptionDsl.md).

## Golden tests and their fixtures

`Tests/fixtures/` holds three whole Issie projects — `1fulladder`, `adder4` and `3cpu`, the last an
18-sheet CPU. `GoldenModel.fs` simulates each with a deterministic stimulus and writes out every
output, viewer and clocked value on every cycle, plus final memory contents, then compares that
against the `.golden` file beside the project.

```bash
ISSIE_UPDATE_GOLDEN=1 npm run test     # rewrites every golden file
```

**That rewrites the goldens wholesale, with no review.** A golden failure usually means a real
change in simulation behaviour, so read the diff the failure prints and understand it before
regenerating. Regenerating to make a failure go away destroys the only record of what the simulator
used to do.

## Draw-block tests and text measurement

Symbol geometry is sized from measured text widths, which in the browser comes from a canvas.
Outside the browser `DrawHelpers.getTextWidthInPixels` reconstructs the width from per-character
advance widths instead, so the draw block can be tested at all. `DrawBlockTests.fs` holds that
reconstruction to widths recorded from a real browser, currently within 1%.

Assert draw-block *structure* — port counts and edges, overlap, orthogonality, ordering — rather
than pixel widths, which are only ever as good as that table. Anything about how a sheet actually
*looks* needs the running app: see [../docs/dev/inspectingTheCanvas.md](../docs/dev/inspectingTheCanvas.md).

## What is not run

- **CI does not run this suite**, on any platform. `.github/workflows/tests.yml` runs a Fable
  compile on Windows and reports that. Run the tests locally before opening a PR.

The legacy `Tests/Tests.fsproj` and the `Tests/*.fs` beside it — a `netcoreapp3.1` project that
had not built since it was written, listing three source files that were never in the repository —
were deleted in August 2026. Nothing referenced them. Their `CanvasStates*.fs` and
`WidthInfererTests.fs` still hold hand-built canvases for cases this suite does not cover
(width-inference failures, partially connected components, non-inferrable loops) and are worth
mining from git history:

```bash
git show a1a8daba5:Tests/WidthInfererTests.fs
git show a1a8daba5:Tests/CanvasStates.fs
```

## Not covered

Worth knowing before you assume a change is safe. The Verilog subsystem has two suites —
`VerilogOutput.fs` (the emitted Verilog text) and `VerilogCompiler.fs` (the input compiler
end-to-end: real parse via node, semantic checks, synthesis, simulation) — but they pin specific
behaviours, not the 10,000-line subsystem broadly. There are no tests for the waveform
simulator, the truth-table UI, or the Elmish update loop. Wire routing is covered only for the
simple cases in `DrawBlockTests.fs`.
