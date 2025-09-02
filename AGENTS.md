# Repository Guidelines

## Project Structure & Module Organization
- `src/Main/`: Electron main process (F# via Fable).
- `src/Renderer/`: UI + simulator logic (F#, Elmish, small JS helpers).
- `Tests/`: F# unit/property tests (Expecto/FsCheck).
- `scripts/`: build/dev helper scripts for Electron/Webpack.
- `public/`, `static/`: app icons, HTML, demos, HDL, and other assets.
- `docs/`: user docs, updates, and PDFs. Additional JS simulator tests in `simulator_tests/js`.

## Build, Test, and Development Commands
- `npm run dev`: Hot-reload development (Fable watch + Electron).
- `npm run debug`: Dev mode with extra renderer assertions.
- `npm run compile:parallel`: Compile F# projects to JS with Fable.
- `npm run build`: Production bundle via Webpack; outputs to `build/`.
- `npm run dist` / `npm run pack`: Build distributables (uses electron-builder).
- `run-tests.cmd` (Windows) / `run-tests.sh` (Unix): End-to-end local test runner.
- `dotnet run --project Tests/Tests.fsproj`: Run unit tests.
- `npm run typecheck`: Type-check F# renderer project.

## Coding Style & Naming Conventions
- F#: 4-space indentation, max line width ~120; enforced via `.editorconfig`.
- Formatter: `dotnet fantomas` (CI checks formatting). Run `dotnet tool restore` then `dotnet fantomas src/ Tests/`.
- JS: Use ESLint/Prettier defaults; run `npx eslint src/**/*.js`.
- Files: tests end with `*Tests.fs`; modules and types use `PascalCase`; values/functions use `camelCase`.

## Testing Guidelines
- Frameworks: Expecto + FsCheck (property tests). See `Tests/*.fs`.
- Conventions: Group by feature; name tests with clear scenario and expectation.
- Run: `run-tests.cmd` or `dotnet run --project Tests/Tests.fsproj`.
- CI: Matrix builds run tests, coverage, lint, and packaging on Windows/macOS/Linux.

## Commit & Pull Request Guidelines
- Commits: Prefer Conventional Commits (`feat:`, `fix:`, `docs:`, etc.). Keep changes scoped.
- PRs: Include summary, rationale, and screenshots/GIFs for UI changes. Link issues (`Fixes #123`). Note any migration steps.
- Checks: Ensure `npm run compile:parallel`, tests, and linters pass locally.

## Security & Configuration Tips
- Secrets: Do not commit secrets; use local `.env` (present for development) and GitHub secrets for CI.
- Electron: Avoid enabling insecure features in renderer. Validate file I/O paths.
- Cross-platform: Use provided scripts; avoid hard-coded OS-specific paths.

