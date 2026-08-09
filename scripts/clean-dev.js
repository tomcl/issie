// Kill what an interrupted dev session leaves behind: fable watch, the webpack dev server, and
// Electron. Ctrl-C in the wrong window, or a killed terminal, orphans all three, and a stray
// `fable watch` is not harmless - it holds an F# compiler in memory and can still recompile on a
// file change, flipping the tree's build mode under whoever runs the app next.
//
//   npm run clean-dev            kill them
//   npm run clean-dev -- --list  show what would be killed, kill nothing
//
// This replaces two one-liners that could not work:
//
//   taskkill /f /im node.exe && taskkill /f /im dotnet.exe && taskkill /f /im issie.exe
//
// npm runs its scripts on node, so the first command killed npm's own process and took the shell
// with it - dotnet and issie were never reached. And when node happened not to be running,
// taskkill returned non-zero for "not found", so `&&` short-circuited and stopped there anyway.
// Either way the dotnet processes survived; eleven of them had accumulated over two days when
// this was found.
//
// So: match on what a process IS rather than on its executable name, kill node last, and never
// match this script or the npm that started it.

const os = require('os');
const { execFileSync } = require('child_process');

const windows = os.platform() === 'win32';

/// [{ pid, command }] for every process on the machine.
function allProcesses() {
  if (windows) {
    // -NoProfile: a user profile can print banners that corrupt the JSON
    const json = execFileSync(
      'powershell',
      ['-NoProfile', '-Command',
        'Get-CimInstance Win32_Process | Select-Object ProcessId,CommandLine | ConvertTo-Json -Compress'],
      { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 }
    );
    return JSON.parse(json)
      .filter((p) => p.CommandLine)
      .map((p) => ({ pid: p.ProcessId, command: p.CommandLine }));
  }
  return execFileSync('ps', ['-Ao', 'pid=,args='], { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 })
    .split('\n')
    .map((line) => line.trim().match(/^(\d+)\s+(.*)$/))
    .filter(Boolean)
    .map((m) => ({ pid: Number(m[1]), command: m[2] }));
}

/// The executable a command line runs, lowercased and without its directory.
function exeName(command) {
  const quoted = command.match(/^"([^"]+)"/);
  const first = quoted ? quoted[1] : command.split(/\s+/)[0];
  return first.split(/[\\/]/).pop().toLowerCase();
}

const is = (exe, ...names) => names.some((n) => exe === n || exe === n + '.exe');

// Every rule tests the executable as well as the arguments. Matching the word "issie" anywhere in
// a command line is not good enough - the repo directory is called that, so a first attempt at
// this listed a Visual Studio shell and the very PowerShell running the check.
//
// Ordered: the compilers and the app first, the node processes supervising them last, so killing
// a supervisor cannot orphan something we have not reached yet.
const groups = [
  { what: 'fable', matches: (c, exe) => is(exe, 'dotnet') && /\bfable\b/i.test(c) },
  { what: 'app', matches: (_c, exe) => is(exe, 'electron', 'issie') },
  {
    what: 'dev server',
    matches: (c, exe) =>
      is(exe, 'node') && /scripts[\\/](dev|start|app|parallel-compile)\.js|webpack/i.test(c),
  },
];

const listOnly = process.argv.includes('--list');
const mine = new Set([process.pid, process.ppid]);

let processes;
try {
  processes = allProcesses();
} catch (err) {
  console.error(`[clean-dev] could not list processes: ${err.message}`);
  process.exit(1);
}

const targets = [];
for (const group of groups) {
  for (const proc of processes) {
    if (mine.has(proc.pid)) continue; // this script, and the npm that ran it
    if (targets.some((t) => t.pid === proc.pid)) continue;
    if (group.matches(proc.command, exeName(proc.command))) {
      targets.push({ ...proc, what: group.what });
    }
  }
}

if (!targets.length) {
  console.log('[clean-dev] nothing left over');
  process.exit(0);
}

for (const target of targets) {
  const summary = target.command.length > 80 ? target.command.slice(0, 77) + '...' : target.command;
  if (listOnly) {
    console.log(`[clean-dev] would kill ${target.pid} (${target.what}): ${summary}`);
    continue;
  }
  try {
    process.kill(target.pid, 'SIGKILL');
    console.log(`[clean-dev] killed ${target.pid} (${target.what})`);
  } catch (err) {
    // ESRCH: a child that died when we killed its parent a moment ago. Not a failure.
    if (err.code !== 'ESRCH') console.error(`[clean-dev] could not kill ${target.pid}: ${err.message}`);
  }
}

console.log(`[clean-dev] ${listOnly ? 'listed' : 'killed'} ${targets.length} process(es)`);
