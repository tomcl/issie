// Drop a cached Fable project crack that names assemblies which no longer exist.
//
// Fable caches what cracking a project told it - source list, defines and the full `-r:` list of
// referenced assemblies - in build-fable/<project>/fable_modules/project_cracked*.json, and reuses
// it on the next start ("Retrieving project options from cache"). Those references are absolute
// paths into the .NET SDK's reference pack, e.g.
//
//     C:/Program Files/dotnet/packs/Microsoft.NETCore.App.Ref/10.0.11/ref/net10.0/System.Runtime.dll
//
// and an SDK update (Visual Studio and Windows Update both do this unasked) removes that folder.
// Fable does not report the missing files: its compiler agent dies quietly while loading them and
// the process sits idle forever, printing nothing after "Project and references ... parsed". In
// `npm run dev` that shows as Main reaching `ready` - it signals before loading references when
// its output is current - and Renderer never doing so, so the app never starts. Found 18 Oct 2026
// after the SDK went from 10.0.11 to 10.0.12.
//
// Deleting the cache makes Fable crack the project afresh against the SDK that is there. That
// costs one full recompile, which the new references would force anyway.

const fs = require('fs');
const path = require('path');
const { outPathOf } = require('./fable-output');

const projects = ['src/Main', 'src/Renderer'];

/// The cache files for one project: one per build mode (debug, release, ...).
function cacheFilesOf(projectDir) {
  const dir = path.join(outPathOf(projectDir), 'fable_modules');
  if (!fs.existsSync(dir)) return [];
  return fs
    .readdirSync(dir)
    .filter((f) => /^project_cracked.*\.json$/.test(f))
    .map((f) => path.join(dir, f));
}

/// The referenced assemblies a cache names that are not on disk, or null when it cannot be read -
/// which is also a reason to drop it, since Fable would not be able to use it either.
function missingReferences(cacheFile) {
  try {
    const cracked = JSON.parse(fs.readFileSync(cacheFile, 'utf8'));
    return (cracked.FSharpOptions || [])
      .filter((o) => o.startsWith('-r:'))
      .map((o) => o.slice(3))
      .filter((ref) => !fs.existsSync(ref));
  } catch {
    return null;
  }
}

/// Delete every project cache that refers to an assembly which is gone. Never fails the caller:
/// at worst Fable meets the stale cache, which is where things stood without this.
function dropStaleCrackCaches() {
  for (const file of projects.flatMap(cacheFilesOf)) {
    const missing = missingReferences(file);
    if (missing && missing.length === 0) continue;
    const why = missing
      ? `${missing.length} referenced assemblies are gone, e.g. ${missing[0]}`
      : 'it could not be read';
    try {
      fs.unlinkSync(file);
      console.log(`Dropped stale Fable project cache ${path.relative(process.cwd(), file)}: ${why}`);
    } catch (err) {
      console.warn(`Could not drop stale Fable project cache ${file}: ${err.message}`);
    }
  }
}

module.exports = { dropStaleCrackCaches };

if (require.main === module) dropStaleCrackCaches();
