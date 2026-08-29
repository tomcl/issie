# Build Speed and Fable Caching

How the dev build gets fast startup, and what silently makes it slow again.

## The scripts

- `npm run app` — [`scripts/app.js`](../scripts/app.js): starts the app in whichever of `dev` /
  `dev:once` the generated JS already belongs to, so there is no mode to keep track of. Use this
  unless you specifically want one of them. `npm run app -- --which` says what it would pick and
  why without starting anything; anything else after `--` goes through to Electron.
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

Every one of these reaches [`scripts/start.js`](../scripts/start.js), which frees the two ports it
needs before using them ([`scripts/free-port.js`](../scripts/free-port.js)): 8672 for the webpack
dev server and 9222 for the DevTools protocol that `scripts/inspect-canvas.js` talks to. A session
interrupted in any of the usual ways leaves both held, and the failure that follows is worse than
it sounds: the port is bound *after* a full Fable compile and after Electron has been told to open,
so the window appears, stays blank, and the log says `ready`. A stale Electron on 9222 is quieter
still — `inspect-canvas` connects to it and reports the previous run's canvas. Whoever holds the
port is killed, whatever it is; that is a more reliable test than matching command lines.

A `fable watch` holds no port, so nothing above reaches it — and it is the leftover that matters
most, because it is still watching: it recompiles on the next file change and can flip the tree's
build mode under whoever runs the app next. Every entry point above therefore starts by removing
the watchers an interrupted session left ([`scripts/free-watchers.js`](../scripts/free-watchers.js)).

That check has to be nearly free, since it runs before a `dev:once` that is otherwise instant on an
unchanged tree, and Windows cannot list processes without spawning PowerShell — 250ms before it has
done anything, 640ms for the query. So a watch session leaves a note in `build-fable` saying it
started and who owned it, and removes the note when it ends properly. No note, or a note whose
owner is still alive, means there is nothing to look for: about 10ms, and the usual answer. Only a
note left by a session that was killed is worth the listing, and then a leftover is identified by
its PARENT being gone rather than by its pid — the shell `dev.js` spawns it through dies with the
session while the Fable process under it does not, which is the whole problem.

[`scripts/clean-dev.js`](../scripts/clean-dev.js) is still the tool for sweeping a whole abandoned
session on request. It matches command lines rather than parentage, so it catches a watcher started
some other way — and it will kill a running app, which is why it is a command you run and not
something that happens at startup.

## How Fable decides it can start fast

Fable has no on-disk cache of typed ASTs: a cold compile type-checks every file of the Renderer
(~200 including dependencies) and takes on the order of a minute. What it does have:

1. **Project cracking cache** (`build-fable/<project>/fable_modules/project_cracked.json`) —
   restoring project options takes ~200ms instead of several seconds.
2. **Up-to-date detection** — if every generated `.fs.js` is strictly newer than its `.fs`, a
   one-shot compile is skipped entirely, and `fable watch` runs its `--run` command *immediately*
   (recompiling silently in the background to build its watch graph). `dev.js` uses that `--run`
   hook (`scripts/fable-ready.js`) as the signal to start Electron, so an unchanged tree starts
   the app in seconds either way.

## What breaks it

- **A stale `.fs.js` timestamp**, which [`scripts/refresh-stale-output.js`](../scripts/refresh-stale-output.js)
  exists to prevent. It pairs each generated file with its source through the `sources` entry of the
  source map beside it, since the output no longer sits next to the `.fs` — see
  [`scripts/fable-output.js`](../scripts/fable-output.js). Fable rewrites an output file only when
  its *content* changed, and the up-to-date check above is about timestamps, so the two can
  disagree. A `.fs` whose mtime moves
  without its emitted JS changing — a comment or warning-only edit, a `git checkout`, a rebase —
  leaves an output that is perfectly current and still fails the check. **A recompile does not fix
  it**: nothing changed, so nothing is written, so every subsequent startup pays the full ~1 minute
  again, indefinitely.

  So `dev.js` and `parallel-compile.js` both run `refresh-stale-output.js` after a successful
  compile, when every output is current by construction, and it prints what it touched. To run it
  by hand, or on a directory of your own:

  ```bash
  node scripts/refresh-stale-output.js [dir ...]
  ```

- **Switching build modes.** `fable watch` implicitly adds the `DEBUG` define (this is what
  enables Elmish HMR), so `dev`, `dev:once`, `debug` (`ASSERTS`) and `compile` (`PRODUCTION`) are
  four different builds. Each switch between them invalidates the up-to-date state and costs one
  full recompile; staying in one mode stays fast. `npm run app` avoids the question by going
  wherever the tree already is.

  Which mode the tree is in is recorded per project in `fable_modules`, and only one cache exists
  at a time: watch writes `project_cracked_debug.json`, every other mode writes
  `project_cracked.json` holding the defines it used. `npm run app -- --which` reads them out.

  Note for anyone verifying a change compiles: `npm run compile` leaves the tree in `PRODUCTION`,
  so the next `dev` or `dev:once` pays a full recompile. `node scripts/dev.js --once --no-app` is
  the same check without that cost.

- **Changed compiler options.** Any option change — defines, `--verbose`, source maps — is
  recorded in the cracking cache and defeats output reuse for the next run. In particular a
  `--verbose` diagnostic run makes the *following* plain run recompile.

If the cache seems wedged, `dotnet fable clean` in the project directory removes the generated
files and caches for a genuinely fresh start.
