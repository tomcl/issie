# Build Speed and Fable Caching

How the dev build gets fast startup, and what silently makes it slow again.

## The scripts

- `npm run dev` — [`scripts/dev.js`](../scripts/dev.js) runs `dotnet fable watch` for `src/Main`
  and `src/Renderer` **in parallel**, then starts webpack + Electron
  ([`scripts/start.js`](../scripts/start.js)) as soon as both projects' generated JS is safe to
  load. Hot reload of renderer changes while running.
- `npm run dev:once` — same launcher with a one-shot compile and no watcher. When nothing changed
  since the last compile, Fable skips compilation entirely and the app is up in a few seconds.
  Edits need a rerun.
- `npm run debug` — `dev` plus the `ASSERTS` define on the renderer.
- `npm run compile` — [`scripts/parallel-compile.js`](../scripts/parallel-compile.js): one-shot
  parallel compile of both projects with the `PRODUCTION` define. Used by `pack` and `dist`.
- `node scripts/dev.js --no-app` — either mode without launching Electron (compile check).

## How Fable decides it can start fast

Fable has no on-disk cache of typed ASTs: a cold compile type-checks every file of the Renderer
(~200 including dependencies) and takes on the order of a minute. What it does have:

1. **Project cracking cache** (`fable_modules/project_cracked.json`) — restoring project options
   takes ~200ms instead of several seconds.
2. **Up-to-date detection** — if every generated `.fs.js` is strictly newer than its `.fs`, a
   one-shot compile is skipped entirely, and `fable watch` runs its `--run` command *immediately*
   (recompiling silently in the background to build its watch graph). `dev.js` uses that `--run`
   hook (`scripts/fable-ready.js`) as the signal to start Electron, so an unchanged tree starts
   the app in seconds either way.

## What breaks it

- **A stale `.fs.js` timestamp.** Fable only rewrites an output file whose *content* changed. If a
  `.fs` file's mtime is refreshed without changing its emitted JS — a comment or warning-only
  edit, a `git checkout`/rebase touching the file — its output stays older than it forever, and
  that single file forces the full recompile on **every** startup. Find offenders (run in
  `src/Renderer` and `src/Main`) and touch the listed outputs' `.fs.js` files:

  ```bash
  find . -name "*.fs" -not -path "./obj/*" | while read f; do
    [ -f "$f.js" ] && [ ! "$f.js" -nt "$f" ] && echo "$f"
  done
  ```

- **Switching build modes.** `fable watch` implicitly adds the `DEBUG` define (this is what
  enables Elmish HMR), so `dev`, `dev:once` and `compile` are three different builds. Each switch
  between them invalidates the up-to-date state and costs one full recompile; staying in one mode
  stays fast.

- **Changed compiler options.** Any option change — defines, `--verbose`, source maps — is
  recorded in the cracking cache and defeats output reuse for the next run. In particular a
  `--verbose` diagnostic run makes the *following* plain run recompile.

If the cache seems wedged, `dotnet fable clean` in the project directory removes the generated
files and caches for a genuinely fresh start.
