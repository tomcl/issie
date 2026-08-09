// Start the app in whichever build mode is already compiled, so there is nothing to think about.
//
//   npm run app                  whichever of dev / dev:once starts soonest
//   npm run app -- --which       say which it would pick, and why, without starting anything
//
// (--which, not --dry-run: npm claims --dry-run for itself even after the --, and swallowing it
// would silently start the app instead of reporting.)
//   npm run app -- --log=wire    anything else goes through to dev.js and on to Electron
//
// Why there is a choice to make at all: `dev` (fable watch), `dev:once`, `debug` (ASSERTS) and
// `compile` (PRODUCTION) compile with different defines, so they are four different builds of the
// same source, and the generated JS on disk belongs to exactly one of them. Switching costs a full
// ~1 minute recompile; staying put costs seconds. Since watch and one-shot produce the same app -
// DEBUG is used by nothing in Issie's own source, only by Fable.Elmish.HMR - which one you are in
// hardly matters, but changing your mind does.
//
// Fable records the mode in each project's fable_modules cracking cache, and only one exists at a
// time: watch writes project_cracked_debug.json, every other mode writes project_cracked.json with
// the defines it used. That is what is read here.
//
// When neither mode is ready - the tree was last built by `npm run compile`, or never built - a
// compile has to happen, so this starts the watcher: it pays that once and then recompiles later
// edits incrementally inside the live process, where a fresh one-shot would re-type-check all ~200
// files every time.

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');
const { findStaleOutput } = require('./refresh-stale-output');

const root = path.join(__dirname, '..');
const projectDirs = ['src/Main', 'src/Renderer'];

/// What the last Fable build of this project was: 'watch', 'once' (any non-watch mode) or 'none',
/// plus the defines it used beyond the FABLE_* ones Fable always sets.
function lastBuild(projectDir) {
  const modules = path.join(root, projectDir, 'fable_modules');
  const read = (name) => {
    const file = path.join(modules, name);
    if (!fs.existsSync(file)) return null;
    try {
      const options = JSON.parse(fs.readFileSync(file, 'utf8')).FableOptions;
      return (options.Define || []).filter((d) => !d.startsWith('FABLE_')).sort();
    } catch {
      return null; // a half-written cache is no cache
    }
  };
  const watch = read('project_cracked_debug.json');
  if (watch) return { mode: 'watch', defines: watch };
  const once = read('project_cracked.json');
  if (once) return { mode: 'once', defines: once };
  return { mode: 'none', defines: [] };
}

/// dev and dev:once both compile with no defines of their own, in both projects. `compile` adds
/// PRODUCTION and `debug` adds ASSERTS, so either leaves the tree ready for neither.
function readyFor(builds, mode) {
  return builds.every((b) => b.mode === mode && b.defines.length === 0);
}

function decide() {
  const builds = projectDirs.map(lastBuild);
  const describe = projectDirs
    .map((dir, i) => `${path.basename(dir)} ${builds[i].mode}` +
      (builds[i].defines.length ? ` [${builds[i].defines.join(',')}]` : ''))
    .join(', ');

  if (readyFor(builds, 'watch')) {
    return { once: false, why: `already built for watch (${describe})` };
  }
  if (readyFor(builds, 'once')) {
    return { once: true, why: `already built one-shot (${describe})` };
  }
  return {
    once: false,
    why: `built for neither (${describe}) - starting the watcher, which pays one compile now and ` +
      `keeps later edits incremental`,
  };
}

const args = process.argv.slice(2);
const dryRun = args.includes('--which');
const decision = decide();
const mode = decision.once ? 'dev:once' : 'dev';

console.log(`\x1b[32m[app]\x1b[0m ${mode}: ${decision.why}`);

// A generated file older than its source makes Fable recompile whatever mode it is in. Say so,
// rather than let an unexplained minute look like this script having chosen wrong. Do NOT refresh
// the timestamps here - before a compile they may mean the output really is out of date.
const stale = findStaleOutput(projectDirs.map((dir) => path.join(root, dir)));
if (stale.length) {
  const names = stale.map((f) => path.relative(root, f)).join(', ');
  console.log(`\x1b[32m[app]\x1b[0m ${stale.length} generated file(s) older than their source, so ` +
    `a compile is coming either way: ${names}`);
}

if (dryRun) process.exit(0);

const devArgs = [path.join(__dirname, 'dev.js')];
if (decision.once) devArgs.push('--once');
devArgs.push(...args.filter((a) => a !== '--which'));

const proc = spawn(process.execPath, devArgs, { cwd: root, stdio: 'inherit' });
proc.on('close', (code) => process.exit(code === null ? 0 : code));
proc.on('error', (err) => {
  console.error('[app] could not start dev.js:', err.message);
  process.exit(1);
});
