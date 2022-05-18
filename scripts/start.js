const webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');
const configMain = require('../webpack.config.main');
const configRenderer = require('../webpack.config.renderer');
const { spawn } = require('child_process');
const path = require('path');
const del = require('del');

const compilerMain = webpack(configMain);
const compilerRenderer = webpack(configRenderer);
const buildPath = path.join(__dirname, '../build');

let electronStarted = false;

 (async () => {
    /**
     * Delete build dir
     */
    await del([buildPath], { force: true });

    /**
     * Start renderer dev server
     */
    const renderSrvOpts = {
      hot: true,
      host: "localhost",
      port: 9000
    };

    const server = new WebpackDevServer(renderSrvOpts, compilerRenderer);
    await server.start();
    console.log(`> Dev server is listening on port ${renderSrvOpts.port}`);

    /**
     * Start Electron
     */
    const startElectron = () => {
      const electronPath = path.join(process.cwd(), 'node_modules', '.bin', process.platform === 'win32' ? 'electron.cmd' : 'electron');
      const electron = spawn(electronPath, [path.join(buildPath, 'index.js')], {stdio: 'inherit'});

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