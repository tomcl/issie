// Keeps src/Renderer/Interface/Version.fs in step with package.json's version.
//
// Wired into the npm version lifecycle (see the version scripts in package.json), so a
// whole release is one command on master:
//
//   npm version patch      (or minor / major / an explicit 6.1.0)
//
// npm refuses to run on a dirty tree, bumps package.json, runs this script to rewrite
// Version.fs, commits both files, tags vX.Y.Z, and postversion pushes commit + tag.
// The tag push triggers .github/workflows/build.yml, which builds the platform
// binaries and creates the GitHub release.
//
// --preflight (the preversion hook) only checks we are on master, before anything
// has been modified.

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const root = path.join(__dirname, '..');
const versionFsPath = path.join(root, 'src', 'Renderer', 'Interface', 'Version.fs');

if (process.argv.includes('--preflight')) {
  const branch = execSync('git rev-parse --abbrev-ref HEAD', { cwd: root }).toString().trim();
  if (branch !== 'master') {
    console.error(`Releases are tagged on master; current branch is '${branch}'.`);
    process.exit(1);
  }
  process.exit(0);
}

const { version } = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
const [major, minor, patch] = version.split('.');
const line = `let VERSION = [ "${major}" ; "${minor}" ; "${patch}" ]`;

const src = fs.readFileSync(versionFsPath, 'utf8');
if (!/^let VERSION = \[.*\][ \t]*$/m.test(src)) {
  console.error(`VERSION line not found in ${versionFsPath}`);
  process.exit(1);
}
fs.writeFileSync(versionFsPath, src.replace(/^let VERSION = \[.*\][ \t]*$/m, line));
console.log(`Version.fs synced to ${version}`);
