const webpack = require('webpack');
const configMain = require('../webpack.config.main');
const configPreload = require('../webpack.config.preload');
const configRenderer = require('../webpack.config.renderer');
const path = require('path');
const fsextra = require('fs-extra');

/**
 * Run one webpack config to completion, and fail the build if it did not succeed.
 *
 * The callbacks this replaces ignored both `err` and `stats`, so a bundle that failed to compile
 * produced a zero exit code and a build/ directory quietly missing a file. That was survivable while
 * there were two bundles that obviously either worked or did not; it is not survivable now that one
 * of them is the preload, whose absence turns into a renderer that loads and then cannot reach the
 * main process at all.
 */
const run = (config, label) => new Promise((resolve, reject) => {
  webpack(config).run((err, stats) => {
    if (err) return reject(err);
    if (stats.hasErrors()) {
      return reject(new Error(`${label} bundle failed:\n${stats.toString({ all: false, errors: true })}`));
    }
    console.log(`> Built ${label}`);
    return resolve();
  });
});

(async () => {
  /**
   * Delete build and dist dirs.
   *
   * Awaited: fsextra.remove returns a promise, and without the await webpack started writing into
   * the very directory that was being deleted. This is also the only thing that empties build/ -
   * none of the three configs sets `clean`, which would take the other two bundles with it.
   */
  await fsextra.remove(path.join(__dirname, '../build'));
  await fsextra.remove(path.join(__dirname, '../dist'));

  // Sequential rather than concurrent: three webpack instances competing for the machine is slower
  // than three in a row, and the ordering makes a partial build easy to read.
  await run(configMain, 'main');
  await run(configPreload, 'preload');
  await run(configRenderer, 'renderer');
})().catch((e) => {
  console.error(e.message || e);
  process.exit(1);
});
