const webpack = require('webpack');
const configMain = require('../webpack.config.main');
const configRenderer = require('../webpack.config.renderer');
const path = require('path');
const del = require('del');

const compilerMain = webpack(configMain);
const compilerRenderer = webpack(configRenderer);

 (async () => {
   /**
     * Delete build and dist dirs
     */
    await del([path.join(__dirname, '../build'), path.join(__dirname, '../dist')], { force: true });

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