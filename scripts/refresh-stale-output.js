// Keeps Fable's fast-start check from being defeated by a correct file with an old timestamp.
//
// Fable writes a generated .fs.js only when its *content* changed. Its "can I skip compiling
// entirely?" check is a different thing: every .fs.js must be strictly newer than its .fs. So an
// output can be perfectly current and still fail that check - a source whose mtime moved without
// its emitted JS changing (a comment edit, a git checkout, a rebase) leaves one behind. The
// recompile that follows does not fix it: the content still has not changed, so nothing is
// written, and the same file forces the full ~1 minute compile again on the next startup, and
// every startup after that.
//
// Running this after a successful compile closes that loop. It only asserts what the compile just
// established - every output is current - so it cannot hide a real staleness: an output whose
// content was wrong would have been rewritten a moment ago.
//
// Output no longer sits beside its source: each project compiles to its own tree under build-fable
// (see fable-output.js), and one of those trees holds copies of sources from three different
// directories. So the pairing comes from the source map Fable writes next to every generated file,
// whose "sources" entry points back at the .fs it came from. That is Fable's own answer to the
// question and it holds whatever the layout is.

const fs = require('fs');
const path = require('path');

// Package sources under fable_modules come from NuGet and their timestamps do not move, so there
// is nothing here for them to fix - and there are thousands of them.
const skipDirs = new Set(['fable_modules', 'node_modules', '.git']);

function* sourceMaps(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (!skipDirs.has(entry.name)) yield* sourceMaps(full);
    } else if (entry.name.endsWith('.js.map')) {
      yield full;
    }
  }
}

/// The .fs a generated file was compiled from, or null if the map does not say.
function sourceOf(mapFile) {
  try {
    const map = JSON.parse(fs.readFileSync(mapFile, 'utf8'));
    const first = (map.sources || [])[0];
    if (!first) return null;
    const source = path.resolve(path.dirname(mapFile), map.sourceRoot || '', first);
    return source.endsWith('.fs') && fs.existsSync(source) ? source : null;
  } catch {
    return null; // a half-written map is no map
  }
}

/// Generated files that are not strictly newer than their source - the condition that makes Fable
/// recompile. Before a compile these are the files it will look at; after one they are all
/// false alarms. `dirs` are output directories.
function findStaleOutput(dirs) {
  const stale = [];
  for (const dir of dirs) {
    if (!fs.existsSync(dir)) continue;
    for (const mapFile of sourceMaps(dir)) {
      const output = mapFile.slice(0, -'.map'.length);
      if (!fs.existsSync(output)) continue;
      const source = sourceOf(mapFile);
      if (!source) continue;
      if (fs.statSync(output).mtime > fs.statSync(source).mtime) continue;
      stale.push(output);
    }
  }
  return stale;
}

/// Bump the mtime of every generated file that is not strictly newer than its source.
/// Returns the paths touched. ONLY safe straight after a successful compile - doing it before one
/// would tell Fable a genuinely out-of-date output is current, and the app would run stale code.
function refreshStaleOutput(dirs) {
  const now = new Date();
  const refreshed = findStaleOutput(dirs);
  for (const output of refreshed) fs.utimesSync(output, now, now);
  return refreshed.map((output) => path.relative(process.cwd(), output));
}

/// Do it and say so. Silence would make the next slow startup unexplainable.
function reportRefreshStaleOutput(dirs) {
  const refreshed = refreshStaleOutput(dirs);
  if (refreshed.length) {
    console.log(
      `Refreshed ${refreshed.length} up-to-date but old-looking generated file(s), which would ` +
        `otherwise force a full recompile on every startup: ${refreshed.join(', ')}`
    );
  }
  return refreshed;
}

module.exports = { findStaleOutput, refreshStaleOutput, reportRefreshStaleOutput };
