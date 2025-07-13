const { spawn } = require('child_process');
const path = require('path');

// Color codes for different processes
const colors = {
  main: '\x1b[36m',    // Cyan
  renderer: '\x1b[35m', // Magenta
  electron: '\x1b[32m', // Green
  reset: '\x1b[0m'
};

// Helper function to prefix output with process name
function prefixOutput(proc, name, color) {
  proc.stdout.on('data', (data) => {
    const lines = data.toString().split('\n').filter(line => line.trim());
    lines.forEach(line => {
      console.log(`${color}[${name}]${colors.reset} ${line}`);
    });
  });
  
  proc.stderr.on('data', (data) => {
    const lines = data.toString().split('\n').filter(line => line.trim());
    lines.forEach(line => {
      console.error(`${color}[${name}]${colors.reset} ${line}`);
    });
  });
}

// Start Main process compilation
const mainProcess = spawn('dotnet', ['fable', 'watch', 'src/Main', '-s'], {
  shell: true,
  cwd: process.cwd()
});

// Start Renderer process compilation
const rendererProcess = spawn('dotnet', ['fable', 'watch', 'src/Renderer', '-s'], {
  shell: true,
  cwd: process.cwd()
});

// Track if both compilations have completed initial build
let mainReady = false;
let rendererReady = false;
let electronProcess = null;

// Attach output handlers
prefixOutput(mainProcess, 'Main', colors.main);
prefixOutput(rendererProcess, 'Renderer', colors.renderer);

// Watch for initial compilation completion
mainProcess.stdout.on('data', (data) => {
  if (data.toString().includes('Watching') && !mainReady) {
    mainReady = true;
    console.log(`${colors.main}[Main] Initial compilation complete${colors.reset}`);
    checkAndStartElectron();
  }
});

rendererProcess.stdout.on('data', (data) => {
  if (data.toString().includes('Watching') && !rendererReady) {
    rendererReady = true;
    console.log(`${colors.renderer}[Renderer] Initial compilation complete${colors.reset}`);
    checkAndStartElectron();
  }
});

// Start Electron when both processes are ready
function checkAndStartElectron() {
  if (mainReady && rendererReady && !electronProcess) {
    console.log(`${colors.electron}[Electron] Starting Electron...${colors.reset}`);
    
    electronProcess = spawn('npm', ['run', 'start'], {
      shell: true,
      cwd: process.cwd(),
      env: { ...process.env, NODE_ENV: 'development' }
    });
    
    prefixOutput(electronProcess, 'Electron', colors.electron);
    
    electronProcess.on('close', (code) => {
      console.log(`${colors.electron}[Electron] Process exited with code ${code}${colors.reset}`);
      process.exit(code);
    });
  }
}

// Handle process termination
process.on('SIGINT', () => {
  console.log('\nShutting down all processes...');
  
  if (mainProcess) mainProcess.kill();
  if (rendererProcess) rendererProcess.kill();
  if (electronProcess) electronProcess.kill();
  
  process.exit(0);
});

// Handle process errors
mainProcess.on('error', (err) => {
  console.error(`${colors.main}[Main] Error:${colors.reset}`, err);
});

rendererProcess.on('error', (err) => {
  console.error(`${colors.renderer}[Renderer] Error:${colors.reset}`, err);
});