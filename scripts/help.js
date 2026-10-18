// `npm run help`: what each npm script is for, grouped by when you would reach for it.
//
// `npm run` on its own lists the scripts too, but only as the commands they run, which says
// nothing about which one to pick. The descriptions live here rather than in package.json because
// JSON has no comments. Every script in package.json must have an entry: one that has none is
// listed at the end under "Undocumented", so a script added without a line here shows up as such
// rather than silently going missing from the help.

const pkg = require('../package.json');

const groups = [
  {
    title: 'Running the app from source',
    scripts: {
      app: 'Start the app in whichever mode is already compiled (dev or dev:once), so nothing recompiles. `-- --which` says which it would pick.',
      dev: 'The normal dev loop: fable watch on Main and Renderer in parallel, then the app, with hot reload on save.',
      'dev:once': 'Compile once and start the app, with no watcher. Near-instant when nothing has changed.',
      debug: 'As dev, with runtime assertions (ASSERTS) compiled in. Noticeably slower.',
      start: 'Webpack dev server + Electron only, on JS that Fable has already produced. dev runs this for you.',
      'clean-dev': 'Kill fable watchers, dev servers, Electron and sidecars left by an interrupted session. `-- --list` only lists them.',
      'clean-dev-win': 'Alias for clean-dev.',
      'clean-dev-mac': 'Alias for clean-dev.',
    },
  },
  {
    title: 'Checking a change',
    scripts: {
      test: 'The Expecto suite under plain .NET. `-- --filter Issie.<Group>` runs one group; see Tests/README.md.',
      typecheck: 'dotnet build of Renderer.fsproj: the quickest compile check, with better errors than Fable.',
      audit: 'Fail if anything that ships (production deps, plus Electron) has a security advisory. Runs before dist.',
    },
  },
  {
    title: 'Building binaries',
    scripts: {
      compile: 'One-shot PRODUCTION Fable compile of both projects. Leaves the tree in production mode, so the next dev recompiles fully.',
      build: 'Webpack production bundles of the compiled JS into build/.',
      'sidecar:publish': 'Publish the .NET simulation sidecar for this OS into build-sidecar/.',
      pack: 'compile + sidecar + build + electron-builder --dir: an unpacked app in dist/, without the archive.',
      dist: 'compile + sidecar + build + electron-builder: distributable binaries for this platform in dist/.',
      buildonly: 'electron-builder alone, on whatever build/ and build-sidecar/ already hold.',
    },
  },
  {
    title: 'npm hooks (run for you, not by you)',
    scripts: {
      predist: 'Runs `audit` before every `npm run dist`.',
      preversion: 'Before `npm version`: checks a release can be made from here.',
      version: 'During `npm version`: copies the new version into Version.fs.',
      postversion: 'After `npm version`: verifies the sync and pushes master and the tag, which starts the CI release.',
    },
  },
  {
    title: 'This help',
    scripts: {
      help: 'This list.',
    },
  },
];

const color = process.stdout.isTTY ? (code, s) => `\x1b[${code}m${s}\x1b[0m` : (_code, s) => s;
const defined = Object.keys(pkg.scripts);
const width = Math.max(...defined.map((s) => s.length));

console.log(`Issie npm scripts - run as ${color(1, 'npm run <name>')}; pass arguments after ${color(1, '--')}.\n`);

for (const group of groups) {
  console.log(color(1, group.title));
  for (const [name, text] of Object.entries(group.scripts)) {
    if (!defined.includes(name)) continue; // documented here but since removed from package.json
    console.log(`  ${color(36, name.padEnd(width))}  ${text}`);
  }
  console.log();
}

console.log(color(1, 'Releases'));
console.log(`  ${color(36, 'npm version patch|minor|major'.padEnd(width))}  On master only: bump, sync, commit, tag and push; CI publishes.`);
console.log();

const documented = new Set(groups.flatMap((g) => Object.keys(g.scripts)));
const undocumented = defined.filter((s) => !documented.has(s));
if (undocumented.length) {
  console.log(color(33, 'Undocumented (add a line to scripts/help.js)'));
  for (const name of undocumented) console.log(`  ${color(36, name.padEnd(width))}  ${pkg.scripts[name]}`);
  console.log();
}

console.log('More: README.md "Day-to-day development", and docs/BUILD_OPTIMIZATION.md for why a start is fast or slow.');
