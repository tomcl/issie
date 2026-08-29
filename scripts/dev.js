// Development launcher: compiles src/Main and src/Renderer with Fable in parallel, then
// starts Electron (webpack dev server + app) via scripts/start.js.
//
// Fable watchers orphaned by an earlier interrupted session are removed first, whichever of these
// is used - see scripts/free-watchers.js.
//
//   node scripts/dev.js            fable watch, hot reload         (npm run dev)
//   node scripts/dev.js --once     one-shot compile, no watching   (npm run dev:once)
//   node scripts/dev.js --asserts  watch with ASSERTS define       (npm run debug)
//   node scripts/dev.js --no-app   compile only, skip Electron
//
// The app starts as soon as both projects' generated JS is safe to load. In watch mode
// Fable signals that through the --run command (see fable-ready.js): immediately when all
// .fs.js files are up-to-date, otherwise after the first compilation. In --once mode a
// fully up-to-date project skips compilation in under a second.

const { spawn } = require('child_process');
const path = require('path');
const { reportRefreshStaleOutput } = require('./refresh-stale-output');
const { outDirOf, outPathOf } = require('./fable-output');
const { ensureRestored } = require('./fable-restore');
const { freeOrphanedWatchers, recordWatchers, forgetWatchers } = require('./free-watchers');

const root = path.join(__dirname, '..');
const once = process.argv.includes('--once');
const asserts = process.argv.includes('--asserts');
const noApp = process.argv.includes('--no-app');

const READY_MARKER = '__FABLE_READY__';
const reset = '\x1b[0m';

const projects = [
  { name: 'Main', dir: 'src/Main', color: '\x1b[36m', defines: [] },
  { name: 'Renderer', dir: 'src/Renderer', color: '\x1b[35m', defines: asserts ? ['ASSERTS'] : [] },
];

const fableProcs = [];
let appProc = null;
let shuttingDown = false;

function shutdown(code) {
  if (shuttingDown) return;
  shuttingDown = true;
  // Ours are about to die, so the record they are in must not outlive them: left behind, it would
  // cost the next session the process listing to work out that there is nothing to do.
  forgetWatchers();
  for (const proc of fableProcs) proc.kill();
  if (appProc) appProc.kill();
  process.exit(code);
}

process.on('SIGINT', () => {
  console.log('\nShutting down...');
  shutdown(0);
});

// Switches that are not this script's own are meant for Electron, and start.js passes them on:
// `npm run dev -- --log=wire` is how a log category is on before the app's first line runs.
// A single dash counts too, so that -d and -w survive the trip - filtering on '--' silently ate
// them, which is half of why `npm run dev -- -d` never turned the Development menu on.
const ours = ['--once', '--asserts', '--no-app'];
const forwarded = process.argv.slice(2).filter((a) => a.startsWith('-') && !ours.includes(a));

function startApp() {
  if (appProc || noApp) return;
  console.log(`\x1b[32m[App]${reset} Starting webpack dev server and Electron...`);
  appProc = spawn(process.execPath, [path.join(__dirname, 'start.js'), ...forwarded], {
    cwd: root,
    stdio: 'inherit',
    env: { ...process.env, NODE_ENV: 'development', ELECTRON_ENABLE_LOGGING: 'true' },
  });
  appProc.on('close', (code) => shutdown(code === null ? 0 : code));
}

let readyCount = 0;
function onReady(p) {
  if (p.ready) return;
  p.ready = true;
  readyCount += 1;
  console.log(`${p.color}[${p.name}]${reset} ready`);
  if (readyCount === projects.length) {
    // Ready means either nothing needed compiling - so nothing can be stale - or the compile
    // finished. Either way every generated file is current, and one that still looks older than
    // its source would cost a full recompile on every future startup. See refresh-stale-output.js.
    reportRefreshStaleOutput(projects.map((proj) => outPathOf(proj.dir)));
    startApp();
  }
}

// Before anything else: a `fable watch` left over from an interrupted session is still watching,
// so it recompiles on a file change and can flip the tree's build mode under the compile about to
// start here. Only the ones nothing owns any more - a session still running keeps its own. Every
// way into this script gets it, which is every way the app is started in development:
// `npm run dev`, `dev:once`, `debug`, and `npm run app`, which spawns this. See free-watchers.js.
freeOrphanedWatchers();

// Restore here, once, rather than letting the two Fable processes below each do it as a side
// effect of cracking their project: started together they race to write src/Shared's
// project.assets.json and one of them aborts. See scripts/fable-restore.js.
ensureRestored();

for (const p of projects) {
  const args = ['fable'];
  if (!once) args.push('watch');
  // --noRestore because that restore has just happened, once, above.
  args.push(p.dir, '-s', '--noRestore');
  // Each project compiles to its own tree, including its copy of the shared sources and of Fable's
  // library. Without this both projects write src/Shared/*.fs.js, and whichever finishes last
  // decides which library that file imports - which put two copies of it in the main bundle and
  // broke every F# Map that crossed the seam. See scripts/fable-output.js.
  args.push('-o', outDirOf(p.dir), '-e', '.fs.js');
  for (const d of p.defines) args.push('--define', d);
  // Relative, deliberately: this path is passed through two layers that join arguments back into
  // a command line - `shell: true` below, and Fable's own --run, which re-emits it as
  // `cmd /C node <path>` - and neither quotes. An absolute path therefore split at the first
  // space, and a checkout under "C:\My Projects\issie" never saw the ready signal, so the app
  // never started. A path relative to the repo root has no space wherever the repo lives, and
  // the root is this process's cwd (see the spawn below) and Fable's.
  if (!once) args.push('--run', 'node', 'scripts/fable-ready.js');

  const proc = spawn('dotnet', args, { cwd: root, shell: true });
  fableProcs.push(proc);
  // Noted as they start, because what the note has to survive is this process being KILLED - and a
  // kill leaves no chance to write anything on the way out. See free-watchers.js.
  if (!once) recordWatchers();

  const forward = (write) => (data) => {
    for (const line of data.toString().split('\n')) {
      if (!line.trim()) continue;
      write(`${p.color}[${p.name}]${reset} ${line}`);
      if (!once && line.includes(READY_MARKER)) onReady(p);
    }
  };
  proc.stdout.on('data', forward(console.log));
  proc.stderr.on('data', forward(console.error));

  proc.on('error', (err) => {
    console.error(`${p.color}[${p.name}]${reset} failed to start:`, err.message);
    shutdown(1);
  });

  proc.on('close', (code) => {
    if (shuttingDown) return;
    if (once && code === 0) {
      onReady(p);
    } else {
      // In watch mode fable should never exit on its own; in once mode a non-zero
      // exit is a compile failure. Either way, stop everything.
      console.error(`${p.color}[${p.name}]${reset} fable exited with code ${code}`);
      shutdown(code === 0 ? 1 : code);
    }
  });
}
