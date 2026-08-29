// Keeps src/Renderer/Interface/Version.fs in step with package.json's version.
//
// Wired into the npm version lifecycle (see the version scripts in package.json), so a
// whole release is one command on master:
//
//   npm version patch      (or minor / major / an explicit 6.1.0)
//
// npm refuses to run on a dirty tree, bumps package.json, runs this script to rewrite
// Version.fs and stage all three files, commits them, tags vX.Y.Z, and postversion
// verifies the commit before pushing it. The tag push triggers
// .github/workflows/build.yml, which builds the platform binaries and creates the
// GitHub release.
//
//   --preflight   the preversion hook: check we are on master, before anything is modified
//   --verify      the postversion hook: check the commit and tag actually say the new
//                 version, BEFORE any of it is pushed
//
// WHY --verify EXISTS. v6.2.2 was released with binaries called issie-6.2.1-*, because the
// commit it was tagged on did not contain package.json's bump - only package-lock.json and
// Version.fs. The bump was on disk; git did not believe it. "6.2.1" and "6.2.2" are the same
// number of bytes, so the file's size never changed, and `git add` skips a path whose stat
// still matches the index. electron-builder names its output from package.json, so a build
// that succeeded produced a release with the previous version's name on every file.
//
// Two things follow, and both are here. The staging below re-reads content instead of
// trusting stat, so `git add` cannot be fooled that way again. And --verify checks the
// result rather than assuming it: a release that did not record its own version stops on
// the machine that made it, where the fix is one amend, rather than on the release page.

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const root = path.join(__dirname, '..');
const versionFsPath = path.join(root, 'src', 'Renderer', 'Interface', 'Version.fs');
const versionFsRepoPath = 'src/Renderer/Interface/Version.fs';
const git = (cmd) => execSync(`git ${cmd}`, { cwd: root }).toString();

/// The version in a package.json, from disk or from a commit.
const versionOf = (json) => JSON.parse(json).version;

/// The version Version.fs declares, as "6.2.2".
const versionFsVersion = (src) => {
  const m = src.match(/^let VERSION = \[([^\]]*)\]/m);
  return m ? m[1].split(';').map((s) => s.trim().replace(/"/g, '')).join('.') : null;
};

if (process.argv.includes('--preflight')) {
  const branch = git('rev-parse --abbrev-ref HEAD').trim();
  if (branch !== 'master') {
    console.error(`Releases are tagged on master; current branch is '${branch}'.`);
    process.exit(1);
  }
  process.exit(0);
}

if (process.argv.includes('--verify')) {
  const version = versionOf(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
  const tag = `v${version}`;
  const problems = [];

  const committed = versionOf(git('show HEAD:package.json'));
  if (committed !== version) {
    problems.push(`the commit says package.json is ${committed}, not ${version}`);
  }

  const committedFs = versionFsVersion(git(`show HEAD:${versionFsRepoPath}`));
  if (committedFs !== version) {
    problems.push(`the commit says Version.fs is ${committedFs}, not ${version}`);
  }

  // The tag has to be on the commit that is about to be pushed, or the build runs on
  // something else entirely.
  let tagged = '';
  try {
    tagged = git(`rev-list -n 1 ${tag}`).trim();
  } catch {
    problems.push(`there is no tag ${tag}`);
  }
  const head = git('rev-parse HEAD').trim();
  if (tagged && tagged !== head) {
    problems.push(`${tag} is on ${tagged.slice(0, 9)}, not on HEAD (${head.slice(0, 9)})`);
  }

  if (problems.length) {
    console.error(`\nRelease ${tag} is NOT ready to push:\n`);
    for (const problem of problems) console.error(`  - ${problem}`);
    console.error(`
Nothing has been pushed. The binaries are named from package.json, so pushing this
would publish a release whose files carry the wrong version. To repair it here:

  git add package.json package-lock.json ${versionFsRepoPath}
  git commit --amend --no-edit
  git tag -f ${tag}
  git push origin master --follow-tags
`);
    process.exit(1);
  }

  console.log(`${tag} verified: commit and tag both say ${version}`);
  process.exit(0);
}

// The version hook: rewrite Version.fs from package.json, then stage everything the release
// commit must carry.
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

// Staged here rather than left to npm, and with the index re-read first. `git add` trusts the
// stat it has cached, and a version bump changes no file's size - so the add can decide there is
// nothing to do. --really-refresh compares CONTENT and is what makes the add below honest.
try {
  git('update-index --really-refresh -q');
} catch {
  // exits non-zero merely because it found something changed, which is the point
}
git(`add package.json package-lock.json ${versionFsRepoPath}`);
