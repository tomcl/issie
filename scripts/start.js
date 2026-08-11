const webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');
const configMain = require('../webpack.config.main');
const configPreload = require('../webpack.config.preload');
const configRenderer = require('../webpack.config.renderer');
const { spawn } = require('child_process');
const path = require('path');
const fsextra = require('fs-extra');
const { freePort } = require('./free-port');

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

    // An interrupted session leaves this port held, and binding it is the LAST thing that happens
    // here - after a full Fable compile and after Electron has been told to open. Failing then
    // gives a blank window and a log that says "ready", which reads as a broken app rather than as
    // a stale process. Clear it first.
    freePort(renderSrvOpts.port, 'dev server');

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

      // Freed for the same reason, and for one of its own: a stale Electron still holding this
      // port answers scripts/inspect-canvas.js, so the canvas being inspected is the old app's.
      freePort(debugPort, 'DevTools');

      // Switches this script was given are passed on, so that `npm run dev -- --log=wire` reaches
      // JSHelpers.setDebugLevel. It is the only way to have a log category on before the first line
      // of startup runs, which the Development menu and window.issieLog cannot be.
      //
      // Where they go matters. Electron reads everything BEFORE the app path as an instruction to
      // itself or to node, and `--debug` there is taken by node's own bootstrap, which rejects it
      // as the long-removed spelling of --inspect and stops the app from ever starting. Arguments
      // AFTER the app path are the application's, and reach it through process.argv - which is
      // where Bridge.fs and Main.hasDebugArgs both read them from.
      //
      // So Issie's own switches go after, and anything else stays before, since a genuine Chromium
      // flag such as --no-sandbox is no use to us and every use to the browser.
      const passedThrough = process.argv.slice(2).filter(a => a.startsWith('-'));
      const isIssieSwitch = a =>
          ['--debug', '-d', '--w', '-w'].includes(a.toLowerCase()) || a.toLowerCase().startsWith('--log=');

      // `--debug` is the spelling anyone reaches for, and it is also the long-removed node flag for
      // --inspect. Electron's embedded node rejects it wherever it appears - before the app path or
      // after it - and refuses to start, so no amount of reordering makes that exact string safe.
      // It is translated instead: -d means the same thing to Main.hasDebugArgs and to Bridge, and
      // node has no opinion about it. Users keep typing --debug; Electron never sees it.
      const toElectron = a => (a.toLowerCase() === '--debug' ? '-d' : a);

      const forApp = passedThrough.filter(isIssieSwitch).map(toElectron);
      const forChromium = passedThrough.filter(a => !isIssieSwitch(a));

      const electron = spawn(
          electronPath,
          [`--remote-debugging-port=${debugPort}`, ...forChromium, buildFile, ...forApp],
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