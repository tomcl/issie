const webpack = require('webpack');
const configMain = require('../webpack.config.main');
const configRenderer = require('../webpack.config.renderer');
const path = require('path');
const fsextra = require('fs-extra');
const compilerMain = webpack(configMain);
const compilerRenderer = webpack(configRenderer);

 (async () => {
   /**
     * Delete build and dist dirs.
     *
     * Awaited: fsextra.remove returns a promise, and without the await webpack started writing
     * into the very directory that was being deleted. This is also the only thing that empties
     * build/ - webpack.config.main.js deliberately does not set `clean`, which would take the
     * renderer's output with it.
     */
     await fsextra.remove(path.join(__dirname, '../build'))
     await fsextra.remove(path.join(__dirname, '../dist'))
    /**
     * Build main
     */
    compilerMain.run((err, stats) => {
      console.log('> Building main');
    });

    /**
     * Build main
     */
     const buildRenderer = (stats) => {
      console.log('> Building renderer');
      compilerRenderer.run((err, stats) => {});
      return;
    }

    compilerMain.hooks.afterDone.tap('on-main-built', buildRenderer);
})();