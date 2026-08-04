# Repository Guidelines

Issie (Interactive Schematic Simulator with Integrated Editor) is a digital circuit design and
simulation application written in F#, transpiled to JavaScript by Fable and run under Electron.

**[CLAUDE.md](CLAUDE.md) is the fuller guide** — the coding conventions that differ from F#
defaults, the gotchas that cost time, and the tooling for inspecting a running canvas and for
generating sheets from a program. Read it before changing anything. Where the two disagree,
CLAUDE.md is right: this file is a summary, and a summary drifts.

## Project Structure & Module Organization
- `src/Main/`: Electron main process (F# via Fable).
- `src/Renderer/`: UI, draw block and simulator (F#, Elmish, small JS helpers).
- `Tests/Issie.Tests/`: the test suite — unit, property and golden tests (Expecto/FsCheck).
- `Tests/fixtures/`: whole Issie projects the golden tests simulate, with their `.golden` files.
- `Tests/*.fs` and `Tests/Tests.fsproj`: a legacy suite. **It does not build** — it targets
  `netcoreapp3.1` and lists three source files that are not in the repository. Nothing runs it. Its
  `CanvasStates*.fs` still hold hand-built canvases worth mining for cases.
- `scripts/`: build and dev helpers for Electron and Webpack, plus `inspect-canvas.js`.
- `public/`, `static/`: icons, HTML, demo projects, component libraries and other assets.
- `docs/`: user and developer documentation; `docs/dev/` is written for people changing the code.

## Build, Test, and Development Commands
- `npm run dev`: hot-reload development (parallel Fable watch + Electron).
- `npm run dev:once`: one-shot Fable compile (skipped entirely when sources are unchanged), then
  Electron — no watching, no hot reload.
- `npm run debug`: dev mode with extra renderer assertions, and slower.
- `npm run test`: run the test suite — see below.
- `npm run typecheck`: type-check `Renderer.fsproj` under .NET, without Fable. Fast.
- `npm run compile`: compile both F# projects to JS with Fable, in parallel, with the
  PRODUCTION define.
- `npm run build`: production bundle via Webpack, into `build/`.
- `npm run dist` / `npm run pack`: build distributables with electron-builder.
- `npm version patch|minor|major` (on master): release — syncs Version.fs with package.json,
  commits, tags `vX.Y.Z` and pushes; the tag push makes CI build and publish the GitHub release.
- `run-tests.cmd` (Windows) / `run-tests.sh` (Unix): restore, then run the suite and type-check.

`npm run typecheck` does not look inside `#if FABLE_COMPILER` branches. Code there is only checked
by an actual Fable run, so compile with Fable before trusting a change to one.

## Testing Guidelines
- Expecto, with FsCheck for property tests. Run with `dotnet run`, **not** `dotnet test`.
- `npm run test` runs about 209 tests in roughly a minute and reaches all of `Renderer.fsproj`:
  simulation, parameter resolution, the draw block, and UI-module helpers.
- **Run one group, not the suite** — seconds instead of a minute; timings per group are in
  `Tests/README.md`:
  `dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock`
- `Issie.VerilogCompiler` needs node and is ~2/3 of the suite's runtime; it is skipped when the
  `CI` env var is set, so `CI=true npm run test` is the fast everything-else run (~10s).
- Adding a test file takes two edits and missing either fails silently: list it in
  `Tests/Issie.Tests/Issie.Tests.fsproj` (compile order matters) and add its `tests` value to the
  list in `Main.fs`.
- Prefer pinning a fix with a test to arguing about it. Simulation, parameter and draw-block
  behaviour are all reachable from a plain `dotnet run`, with no Electron and no browser.
- Golden tests compare a whole simulated project against a stored file. `ISSIE_UPDATE_GOLDEN=1`
  rewrites those files wholesale — use it only once you have read the diff and meant it.
- Name a test for the scenario and its expectation, and group tests by feature.

## Coding Style & Naming Conventions
- F#: 4-space indentation, maximum line width 120. `.editorconfig` records that and the Fantomas
  settings, and editors honour it.
- Fantomas is available (`dotnet tool restore`, then `dotnet fantomas src/`) but is **not** enforced
  by anything, and the codebase is not uniformly formatted. Do not reformat files you are not
  otherwise changing: it buries the real diff.
- There is no linter, and no ESLint or Prettier configuration. The F# compiler is the check.
- Modules and types are `PascalCase`, values and functions `camelCase`. Test files end `Tests.fs`.
- CLAUDE.md has the conventions that differ from F# defaults, and following the surrounding code is
  not enough to infer them: optics rather than record-copy syntax for state updates, strict
  immutability, and `Option`/`Result` rather than nulls.

## Continuous Integration
Three workflows, in `.github/workflows/`:
- `tests.yml` — on every push. Despite its name it runs **no tests**: it runs `build.ps1 -t Build`
  on Windows, which is `npm run compile`, and reports whether the Fable compile succeeded. The step
  is `continue-on-error`, so it does not block either.
- `build.yml` — on a version tag. Builds and releases binaries for macOS, Windows and Linux.
- `docs.yml` — on push to `master`. Builds the documentation and deploys it to GitHub Pages.

The test suite is therefore **not run by CI on any platform**. Run it locally before a PR.

## Commit & Pull Request Guidelines
- Commit subjects are a short imperative sentence saying what the change does — "Write .dgm without
  a BOM, and add saveProject", "Refuse a parameter slot the component does not have". Two related
  changes may share one subject, joined by "and" or a semicolon. Conventional Commit prefixes
  (`feat:`, `fix:`) are **not** used here.
- Explain in the body why the change is right, not just what it does.
- PRs: give a summary and rationale, say how much you tested and why you think it will not break
  anything (see `CONTRIBUTING.md`), add screenshots or GIFs for UI changes, and link issues
  (`Fixes #123`). Note any migration steps.
- Check locally that `npm run test` and `npm run typecheck` pass, and that Fable still compiles if
  you touched a `#if FABLE_COMPILER` branch. Nothing in CI will do this for you.

## Security & Configuration Tips
- Do not commit secrets; use a local `.env` for development and GitHub secrets for CI.
- Electron: avoid enabling insecure renderer features and validate file I/O paths. All file I/O goes
  through the main process, not the renderer.
- Cross-platform: use the provided scripts and avoid hard-coded OS-specific paths.
