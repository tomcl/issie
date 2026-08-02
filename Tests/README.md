# Issie tests

```bash
npm run test        # the whole suite: ~129 tests, ~20s
```

That runs `dotnet run --project Tests/Issie.Tests -c Release`. It is `dotnet run`, not
`dotnet test`: the suite is an Expecto executable, and `dotnet test` will not find it.

Running one group is much quicker than running everything, and is how to iterate:

```bash
dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock
dotnet run --project Tests/Issie.Tests -c Release -- --filter-test-case Register --summary
```

The suite references `Renderer.fsproj` directly, so it reaches all of the application code —
simulation, parameter resolution, persistence, the draw block and UI-module helpers — under plain
.NET, with no Electron, no browser and no Fable step.

## What is here

| File | Tests | Covers |
|---|---|---|
| `ComponentSemantics.fs` | 42 | Every component type simulated in a minimal circuit and compared against an independent reference. Exhaustive over all inputs at width 3. |
| `AlgebraTests.fs` | 32 | Truth-table algebraic simulation: the `evalExp` simplifier, append handling, and symbolic simulation end to end. |
| `SheetDescriptionTests.fs` | 18 | The sheet-description DSL and the layout that realises it — port resolution, error messages, placement, parameters, and a save/reload round trip. |
| `ParameterScenarios.fs` | 15 | Parameterised sheets instantiated at different bindings and simulated: per-instance resolution, propagation down a hierarchy, and error reporting. |
| `Properties.fs` | 11 | FsCheck properties: the parameter expression language against a reference evaluator and through render/parse, number conversions, and the >32-bit bigint simulation paths. |
| `DrawBlockTests.fs` | 5 | Symbols built and wires routed with nothing running, plus the .NET text-width reconstruction checked against widths recorded from a real browser. |
| `GoldenModel.fs` | 3 | Whole fixture projects simulated for many cycles, every output and clocked value compared against a stored golden file. |
| `LibraryTests.fs` | 2 | What a library sheet shows of itself in the sheet trees. |
| `PersistenceTests.fs` | 1 | A canvas through the `.dgm` save path and back in through the load path. |

Support files, which hold no tests: `TestFixtures.fs` loads fixture projects from disk (it
reimplements the few file primitives it needs, because `FilesIO` initialisation requires Electron);
`CanvasBuilder.fs` builds canvases, loaded components and symbols programmatically; `Main.fs` is the
entry point and the list of test groups.

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

- **`Tests/Tests.fsproj` and the `Tests/*.fs` beside it are dead.** The project targets
  `netcoreapp3.1` and lists three files that do not exist — `Tests/CommonTests.fs`,
  `Tests/DrawBlockTests.fs` and `Tests/VerilogTests.fs` — so it fails to build with `FS0225`.
  (`Tests/Issie.Tests/DrawBlockTests.fs`, in the live suite, is a different and unrelated file.)
  Nothing references the legacy project. Its `CanvasStates*.fs`
  and `WidthInfererTests.fs` do still hold hand-built canvases for cases the current suite does not
  cover — width-inference failures, partially connected components, non-inferrable loops — and are
  worth mining before the directory is removed.
- **`simulator_tests/js`** no longer compiles: its fsproj references `SimulatorTypes.fs`, which has
  since been split into `SimGraphTypes.fs` and `SimTypes.fs`.
- **CI does not run this suite**, on any platform. `.github/workflows/tests.yml` runs a Fable
  compile on Windows and reports that. Run the tests locally before opening a PR.

## Not covered

Worth knowing before you assume a change is safe. There are no tests for the Verilog subsystem
(`src/Renderer/VerilogComponent/`, over 10,000 lines), the waveform simulator, the truth-table UI,
or the Elmish update loop. Wire routing is covered only for the simple cases in `DrawBlockTests.fs`.
