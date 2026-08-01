const webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');
const configMain = require('../webpack.config.main');
const configRenderer = require('../webpack.config.renderer');
const { spawn } = require('child_process');
const path = require('path');
//const del = require('del');
const fsextra = require('fs-extra');

const { shell } = require('electron');
const { remove } = require('fs-extra');

const compilerMain = webpack(configMain);
const compilerRenderer = webpack(configRenderer);
const buildPath = path.join(__dirname, '../build');

let electronStarted = false;

 (async () => {
    /**
     * Delete build dir
     */
    //await del([buildPath], { force: true });
    fsextra.remove(buildPath)
    /**
     * Start renderer dev server
     */
    const renderSrvOpts = {
      hot: true,
      host: "localhost",
      port: 8672
    };

    const server = new WebpackDevServer(renderSrvOpts, compilerRenderer);
    await server.start();
    console.log(`> Dev server is listening on port ${renderSrvOpts.port}`);

    /**
     * Start Electron
     */
    const startElectron = () => {
      var electronPath = path.join(process.cwd(), 'node_modules', '.bin', process.platform === 'win32' ? 'electron.cmd' : 'electron');
      electronPath = '\"' + electronPath + '\"';

      var buildFile = path.join(buildPath, 'index.js');
      buildFile = '\"' + buildFile + '\"';

      // Expose the Chrome DevTools Protocol so that scripts/inspect-canvas.js can read the draw
      // block's geometry and screenshot the renderer. Development only: this script is not used
      // by `npm run build` or by a packaged app, so no released build ever listens on this port.
      const debugPort = process.env.ISSIE_DEBUG_PORT || '9222';

      const electron = spawn(electronPath, [`--remote-debugging-port=${debugPort}`, buildFile],{stdio: 'inherit', shell:true});

      electron.on('exit', function () {
          process.exit(0);
      });
    }

    /**
     * Start main
     */
     const startMain = (stats) => {
      console.log('> Renderer started');

      if(!electronStarted){
        electronStarted = true;
        compilerMain.run((err, stats) => {
            console.log('> Starting Electron (main)');
        });

        compilerMain.hooks.afterEmit.tap('on-main-build', startElectron);
      }
      
      return;
    }

    server.compiler.hooks.afterEmit.tap('on-renderer-start', startMain);
})();