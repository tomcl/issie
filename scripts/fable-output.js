// Where each Fable project's generated JavaScript goes.
//
// One directory per project rather than beside the sources, because two projects compile the same
// sources: src/Shared is referenced by both src/Main and src/Renderer. Fable emits a referenced
// project's output next to the source, so with the default layout both compilers write
// src/Shared/ContextMenus.fs.js and the last one to finish wins.
//
// That is not benign. A generated file imports Fable's library by a path relative to the project
// that compiled it, so the shared file pointed at one project's copy while everything around it in
// the same bundle pointed at another. Two copies of Map.js are two class hierarchies: an F# Map
// built by one and read by the other matches its root key and misses every other one, silently.
// It is what left every context menu but one reporting "unknown menu".
//
// Separate trees also remove two quieter hazards. The projects compile with different defines
// (--define PRODUCTION for the renderer in parallel-compile.js, ASSERTS for `npm run debug`), so a
// shared file containing #if PRODUCTION would have wanted different contents from each - and got
// whichever ran last. And both compilers ran in parallel writing the same paths, so a torn file was
// possible whenever the timing lined up.
//
// The extension stays .fs.js: source-map-loader matches on it in both webpack configs, and
// WorkerInterface builds a worker URL from a sibling .fs.js name.

const path = require('path');

const root = path.join(__dirname, '..');

/// 'src/Main' -> 'build-fable/main'. Relative to the repo root, and with forward slashes, because
/// it is passed to Fable on a command line that goes through a shell.
function outDirOf(projectDir) {
  return `build-fable/${path.basename(projectDir).toLowerCase()}`;
}

/// Absolute path to the same place, for the scripts that read what Fable wrote.
function outPathOf(projectDir) {
  return path.join(root, outDirOf(projectDir));
}

module.exports = { outDirOf, outPathOf };
