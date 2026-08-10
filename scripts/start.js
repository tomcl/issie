const webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');
const configMain = require('../webpack.config.main');
const configPreload = require('../webpack.config.preload');
const configRenderer = require('../webpack.config.renderer');
const { spawn } = require('child_process');
const path = require('path');
const fsextra = require('fs-extra');

const compilerMain = webpack(configMain);
const compilerRenderer = webpack(configRenderer);
const buildPath = path.join(__dirname, '../build');

let electronStarted = false;

 (async () => {
    /**
     * Delete build dir.
     *
     * Awaited: fsextra.remove returns a promise, and without the await the removal raced the
     * main-process bundle being written into the same directory.
     */
    await fsextra.remove(buildPath)
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

      // Switches this script was given are passed on to Electron, so that `npm run dev -- --log=wire`
      // reaches JSHelpers.setDebugLevel. It is the only way to have a log category on before the
      // first line of startup runs, which the menu and window.issieLog cannot be.
      const passedThrough = process.argv.slice(2).filter(a => a.startsWith('--'));

      const electron = spawn(
          electronPath,
          [`--remote-debugging-port=${debugPort}`, ...passedThrough, buildFile],
          {stdio: 'inherit', shell:true});

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
        // The preload has to be on disk before the first window is created, and it is not watched:
        // it is a few lines of marshalling that change once a stage rather than once an edit. Built
        // before main so that Electron never starts against a missing one.
        webpack(configPreload).run((err, stats) => {
            if (err || stats.hasErrors()) {
                console.error('> Preload bundle failed:',
                    err || stats.toString({ all: false, errors: true }));
                process.exit(1);
            }
            console.log('> Built preload');

            compilerMain.run((err, stats) => {
                console.log('> Starting Electron (main)');
            });

            compilerMain.hooks.afterEmit.tap('on-main-build', startElectron);
        });
      }
      
      return;
    }

    server.compiler.hooks.afterEmit.tap('on-renderer-start', startMain);
})();