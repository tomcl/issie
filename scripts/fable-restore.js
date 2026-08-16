// Restore NuGet packages once, before any Fable process starts.
//
// Fable does not read a .fsproj itself: it cracks the project by running
// `dotnet msbuild <proj> /restore /t:...DesignTime...`, which restores the whole reference graph
// as a side effect. src/Main and src/Renderer both reference src/Shared, so when the two
// compilers start together - which is the point of parallel-compile.js and dev.js - they restore
// that one shared project at the same moment and race to write its obj/project.assets.json. The
// loser gets
//
//     NuGet.targets(198,5): error : The file '.../src/Shared/obj/project.assets.json'
//         already exists.  [.../src/Main/Main.fsproj]
//
// and aborts with 134. It is pure timing, so it passes most of the time; it took out the Linux
// release build of v6.0.19, having passed the six before it. Nothing about it is Linux-specific.
//
// The fix is to stop Fable restoring at all - `--noRestore` drops the /restore, leaving the crack
// to read the assets file that is already there - and to make sure one is there first. build.fsx
// restores before it builds, but `npm run dist` and `npm run dev` reach Fable without going
// through it, so the guarantee has to live here.
//
// A second, quieter effect: the restore Fable asks for is not the one `dotnet restore` performs
// (Fable passes NuGetLockFilePath=Fable.lock and friends), so each invalidated the other's
// up-to-date check and a plain `dotnet restore issie.sln` re-restored three of four projects
// every time it was run. With no Fable restore in the picture, the second one is a no-op.

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const root = path.join(__dirname, '..');

// Every project a crack of src/Main or src/Renderer evaluates: the two compiled and the one they
// share. Tests/Issie.Tests is in the solution too, but no Fable compile reaches it.
const projects = ['src/Main', 'src/Renderer', 'src/Shared'];

/// Restore the solution unless every project Fable will crack already has its NuGet assets.
/// Exits the process if the restore fails - there is no point starting a compile without it.
function ensureRestored() {
  const missing = projects.filter(
    (p) => !fs.existsSync(path.join(root, p, 'obj', 'project.assets.json')));
  if (missing.length === 0) return;

  console.log(`Restoring NuGet packages (no assets for ${missing.join(', ')})...`);
  const res = spawnSync('dotnet', ['restore', 'issie.sln'], {
    cwd: root,
    stdio: 'inherit',
    shell: true,
  });
  if (res.status !== 0) {
    console.error('✗ dotnet restore failed; not starting Fable');
    process.exit(res.status === null ? 1 : res.status);
  }
}

module.exports = { ensureRestored };
