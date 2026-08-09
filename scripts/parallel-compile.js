const { spawn } = require('child_process');
const path = require('path');
const { reportRefreshStaleOutput } = require('./refresh-stale-output');

const root = path.join(__dirname, '..');

console.log('Starting parallel compilation...');

// Create promises for each compilation process
const compileMain = new Promise((resolve, reject) => {
  const proc = spawn('dotnet', ['fable', 'src/Main', '-s'], {
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
  const proc = spawn('dotnet', ['fable', 'src/Renderer', '-s', '--define', 'PRODUCTION'], {
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
    reportRefreshStaleOutput([path.join(root, 'src', 'Main'), path.join(root, 'src', 'Renderer')]);
    process.exit(0);
  })
  .catch((error) => {
    console.error('✗ Compilation failed:', error.message);
    process.exit(1);
  });