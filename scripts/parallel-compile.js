const { spawn } = require('child_process');
const path = require('path');
const { reportRefreshStaleOutput } = require('./refresh-stale-output');
const { outDirOf, outPathOf } = require('./fable-output');
const { ensureRestored } = require('./fable-restore');

const root = path.join(__dirname, '..');

// Restore here, once, rather than letting the two Fable processes below each do it as a side
// effect of cracking their project: started together they race to write src/Shared's
// project.assets.json and one of them aborts. See scripts/fable-restore.js.
ensureRestored();

console.log('Starting parallel compilation...');

// Create promises for each compilation process
const compileMain = new Promise((resolve, reject) => {
  // -o and -e: one output tree per project, so the sources both compile do not collide.
  // See scripts/fable-output.js.
  const proc = spawn('dotnet', ['fable', 'src/Main', '-s', '--noRestore', '-o', outDirOf('src/Main'), '-e', '.fs.js'], {
    shell: true,
    stdio: 'inherit'
  });
  
  proc.on('close', (code) => {
    if (code === 0) {
      console.log('✓ Main compilation completed');
      resolve();
    } else {
      reject(new Error(`Main compilation failed with code ${code}`));
    }
  });
  
  proc.on('error', reject);
});

const compileRenderer = new Promise((resolve, reject) => {
  const proc = spawn('dotnet', [
    'fable', 'src/Renderer', '-s', '--noRestore',
    '-o', outDirOf('src/Renderer'), '-e', '.fs.js',
    '--define', 'PRODUCTION',
  ], {
    shell: true,
    stdio: 'inherit'
  });
  
  proc.on('close', (code) => {
    if (code === 0) {
      console.log('✓ Renderer compilation completed');
      resolve();
    } else {
      reject(new Error(`Renderer compilation failed with code ${code}`));
    }
  });
  
  proc.on('error', reject);
});

// Wait for both compilations to complete
Promise.all([compileMain, compileRenderer])
  .then(() => {
    console.log('✓ All compilations completed successfully');
    // every output is current now, so anything that still looks old only has an old timestamp
    reportRefreshStaleOutput([outPathOf('src/Main'), outPathOf('src/Renderer')]);
    process.exit(0);
  })
  .catch((error) => {
    console.error('✗ Compilation failed:', error.message);
    process.exit(1);
  });