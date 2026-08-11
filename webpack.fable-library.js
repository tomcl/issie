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

// Matches "./fable_modules/fable-library-js.5.13.0/Map.js" from a project's own output, and
// "../fable-library-js.5.13.0/Types.js" from an F# package that already sits inside fable_modules.
const EMITTED = /(?:^|[\\/])fable-library-js[^\\/]*[\\/](.+)$/;

module.exports = () =>
  new webpack.NormalModuleReplacementPlugin(EMITTED, (resource) => {
    const match = EMITTED.exec(resource.request);
    if (!match) return;
    resource.request = `${CANONICAL}/${match[1].split(path.sep).join('/')}`;
  });
