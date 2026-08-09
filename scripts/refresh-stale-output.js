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

const fs = require('fs');
const path = require('path');

const skipDirs = new Set(['obj', 'bin', 'fable_modules', 'node_modules', '.git']);

function* fsharpSources(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (!skipDirs.has(entry.name)) yield* fsharpSources(full);
    } else if (entry.name.endsWith('.fs')) {
      yield full;
    }
  }
}

/// Bump the mtime of every generated file that is not strictly newer than its source.
/// Returns the paths touched.
function refreshStaleOutput(dirs) {
  const now = new Date();
  const refreshed = [];
  for (const dir of dirs) {
    if (!fs.existsSync(dir)) continue;
    for (const source of fsharpSources(dir)) {
      const output = source + '.js';
      if (!fs.existsSync(output)) continue; // a module Fable emits nothing for
      if (fs.statSync(output).mtime > fs.statSync(source).mtime) continue;
      fs.utimesSync(output, now, now);
      refreshed.push(path.relative(process.cwd(), output));
    }
  }
  return refreshed;
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

module.exports = { refreshStaleOutput, reportRefreshStaleOutput };

// Usable on its own: node scripts/refresh-stale-output.js [dir ...]
if (require.main === module) {
  const dirs = process.argv.slice(2);
  const root = path.join(__dirname, '..');
  reportRefreshStaleOutput(
    dirs.length ? dirs : [path.join(root, 'src', 'Main'), path.join(root, 'src', 'Renderer')]
  );
}
