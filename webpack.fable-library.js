const fs = require('fs');
const path = require('path');
const webpack = require('webpack');

// One copy of Fable's runtime library per bundle, rather than one per Fable project.
//
// Fable emits its library into <project>/fable_modules and points every generated file at it by a
// relative path. That is right while a file belongs to one project, and wrong for the files both
// projects compile - ElectronAPI.fs, Log.fs and ContextMenus.fs are linked into Main.fsproj as well
// as Renderer.fsproj. Those emit to ONE .fs.js each, beside the original source, so whichever
// project compiled last decides which fable_modules the shared file imports. The main bundle then
// carries that copy for the shared files and its own copy for everything else.
//
// Two copies of Map.js are two MapTree class hierarchies. A Map built by one and read by the other
// matches only its root: Map.tryFind answers for one key and misses the other twenty, and it does
// it silently and partially, which is the worst way for it to fail. That is what left every context
// menu but one reporting "unknown menu" - see the comment in ContextMenuBuilder.makeMenu.
//
// So every request for a fable-library file, from either project and from the F# packages under
// fable_modules too, is rewritten to the single copy in node_modules. It is the same code:
// @fable-org/fable-library-js 2.5.1 is byte-identical to what Fable 5.13 embeds, and Fable prints
// that version as its minimum on every run.
//
// This is a bundling fix, not a compiler one - Fable 5.13 has no --fableLib and installing the
// package does not redirect its imports, both checked. If a later Fable gains that option, setting
// it in scripts/dev.js and scripts/parallel-compile.js would replace this and be tidier.

const CANONICAL = '@fable-org/fable-library-js';

// The rewrite below sends every fable-library request to the npm copy whatever version Fable
// emitted, which is a trap the moment those two disagree: upgrading Fable changes the library it
// embeds, generated code starts using whatever that version added, and the imports would still
// resolve - to the old copy - leaving missing exports as undefined at runtime rather than as an
// error. So the versions are compared here, once per build, and a mismatch stops it.
function assertVersionsAgree() {
  const npmVersion = require(`${CANONICAL}/package.json`).version;

  const emitted = fs
    .readdirSync(path.resolve(__dirname, 'src'))
    .map((project) => path.resolve(__dirname, 'src', project, 'fable_modules'))
    .filter((dir) => fs.existsSync(dir))
    .flatMap((dir) =>
      fs
        .readdirSync(dir)
        .filter((name) => name.startsWith('fable-library-js'))
        .map((name) => ({
          dir: path.join(dir, name),
          version: require(path.join(dir, name, 'package.json')).version,
        })));

  const wrong = emitted.filter((e) => e.version !== npmVersion);
  if (wrong.length === 0) return;

  const detail = wrong.map((e) => `  ${e.version}  ${path.relative(__dirname, e.dir)}`).join('\n');
  throw new Error(
    `fable-library version mismatch.\n` +
    `  ${npmVersion}  node_modules/${CANONICAL}  (pinned in package.json; what every bundle uses)\n` +
    `${detail}  (what Fable now emits)\n` +
    `Fix: npm install --save-exact ${CANONICAL}@${wrong[0].version}\n` +
    `Without it the bundles would use ${npmVersion} against code generated for ${wrong[0].version}, ` +
    `and a missing export is undefined at runtime rather than an error at build time. ` +
    `See webpack.fable-library.js.`);
}

// Matches "./fable_modules/fable-library-js.5.13.0/Map.js" from a project's own output, and
// "../fable-library-js.5.13.0/Types.js" from an F# package that already sits inside fable_modules.
const EMITTED = /(?:^|[\\/])fable-library-js[^\\/]*[\\/](.+)$/;

module.exports = () => {
  assertVersionsAgree();

  return new webpack.NormalModuleReplacementPlugin(EMITTED, (resource) => {
    const match = EMITTED.exec(resource.request);
    if (!match) return;
    resource.request = `${CANONICAL}/${match[1].split(path.sep).join('/')}`;
  });
};
