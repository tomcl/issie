// Kill Fable watchers that an earlier dev session left behind, at the start of the next one.
//
// A `fable watch` is a child of whatever ran it and dies with it in the ordinary case. It does not
// die when that parent is KILLED rather than asked to stop - Ctrl-C in the wrong window, a closed
// terminal, a taskkill on the Electron it was serving - and what is left is not harmless: it holds
// an F# compiler in memory, and it is still watching, so it recompiles on the next file change and
// can flip the tree's build mode under whoever runs the app next. They accumulate one per
// interrupted session until something goes wrong that looks like a build problem.
//
// WHAT THIS COSTS is why it is written the way it is. Windows has no way to list processes without
// spawning PowerShell, which is 250ms before it has done anything and 640ms for the query - on
// every `npm run dev`, `dev:once` and `npm run app`, against a `dev:once` that is otherwise
// near-instant on an unchanged tree. So the listing is the LAST resort. A watch session leaves a
// note behind saying that it started and who owned it, and that note - plus process.kill(pid, 0),
// which spawns nothing - answers "is there anything to look for" in a few milliseconds. A session
// that ends properly removes its own note, so the usual answer is no.
//
// The note deliberately does NOT name the watchers. dev.js spawns through a shell, so what it
// holds is the pid of the cmd.exe that runs dotnet - and that shell dies with the session while
// the Fable process under it does not, which is the whole problem. What identifies a leftover is
// therefore not its pid but its PARENT being gone, which is what the listing is for.
//
// A watcher started some other way - by hand, or by a session whose note was lost - is
// `npm run clean-dev`'s job. That sweeps a whole stale session by matching command lines and will
// happily kill a running app, which is exactly why it is a command the user runs and not something
// that happens at startup.

const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');

const windows = os.platform() === 'win32';

/// Where the pids are written. Beside the build output rather than in the repo: it describes a
/// process that was running, not the source, and it is as disposable as everything else in there.
const recordPath = path.join(__dirname, '..', 'build-fable', 'dev-watchers.json');

/// Whether a pid is a process that exists. Signal 0 checks without delivering anything, and costs
/// nothing - which is the whole reason the fast path here is possible.
function alive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    // EPERM: it exists but belongs to somebody else, which still means the pid is taken.
    return err.code === 'EPERM';
  }
}

/// Note that this session has watchers running, so that the next one knows to look if this one is
/// killed. `owner` is this process: a session that is still running keeps its own watchers, and
/// that is how a second `npm run dev` alongside a first one leaves the first alone.
function recordWatchers() {
  try {
    fs.mkdirSync(path.dirname(recordPath), { recursive: true });
    fs.writeFileSync(recordPath, JSON.stringify({ writtenMs: Date.now(), owner: process.pid }));
  } catch {
    // Not being able to write it costs the next session a stale watcher, which is where we were
    // before this existed. It is not a reason to fail to start.
  }
}

/// Forget the note: this session's watchers are gone, or were never ours to kill.
function forgetWatchers() {
  try {
    fs.rmSync(recordPath, { force: true });
  } catch {
    /* the next run re-reads it and finds nothing alive, which is the same answer */
  }
}

/// Every dotnet process, as { pid, ppid, command }. Only reached when there is something to kill.
function dotnetProcesses() {
  try {
    if (windows) {
      // Filtered to dotnet.exe in the query rather than here: asking for CommandLine means opening
      // each process, so the filter is most of the difference between 640ms and several seconds.
      // -NoProfile because a user profile can print banners that corrupt the JSON.
      const json = execFileSync(
        'powershell',
        ['-NoProfile', '-Command',
          "Get-CimInstance Win32_Process -Filter \"Name='dotnet.exe'\"" +
          ' | Select-Object ProcessId,ParentProcessId,CommandLine | ConvertTo-Json -Compress'],
        { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 }
      );
      // ConvertTo-Json writes a lone object rather than an array of one.
      const rows = JSON.parse(json);
      return (Array.isArray(rows) ? rows : [rows])
        .filter((p) => p && p.CommandLine)
        .map((p) => ({ pid: p.ProcessId, ppid: p.ParentProcessId, command: p.CommandLine }));
    }
    return execFileSync('ps', ['-Ao', 'pid=,ppid=,args='], { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 })
      .split('\n')
      .map((line) => line.trim().match(/^(\d+)\s+(\d+)\s+(.*)$/))
      .filter(Boolean)
      .map((m) => ({ pid: Number(m[1]), ppid: Number(m[2]), command: m[3] }))
      .filter((p) => /(^|[\\/])dotnet(\.exe)?\s/i.test(p.command));
  } catch {
    return [];
  }
}

/// Remove the Fable watchers left over from an earlier session, and say how many processes went.
/// Never throws: failing to tidy up is not a reason to refuse to start.
function freeOrphanedWatchers() {
  let record;
  try {
    record = JSON.parse(fs.readFileSync(recordPath, 'utf8'));
  } catch {
    return 0; // no record, or an unreadable one: nothing this script started is outstanding
  }

  // Nothing survives a restart, and after one the pids in here name whatever the machine started
  // instead - low numbers, handed out again from the beginning. A note older than the last boot is
  // not stale information, it is WRONG information, and acting on it would kill a stranger.
  const bootedMs = Date.now() - os.uptime() * 1000;
  if (!record.writtenMs || record.writtenMs < bootedMs) {
    forgetWatchers();
    return 0;
  }

  // The session that wrote this is still running, so its watchers are in use and not leftovers.
  // This is what makes a second dev session safe to start beside a first.
  if (record.owner && alive(record.owner)) return 0;

  // A session ended without tidying up, so it is worth what the listing costs to find what it
  // left. A leftover is a Fable process whose PARENT has gone: the shell dev.js spawned it
  // through died with the session, and nothing replaced it. One still in use has a living
  // parent, which is why this is safe to run while another session is up.
  //
  // Their children go too. `fable watch` runs the compiler as a separate dotnet process, which
  // would otherwise be left holding the same memory with nobody watching it at all.
  const processes = dotnetProcesses();
  const isFable = (p) => /\bfable\b/i.test(p.command);
  const roots = processes.filter((p) => isFable(p) && !alive(p.ppid));

  const doomed = [];
  const collect = (proc) => {
    for (const child of processes.filter((p) => p.ppid === proc.pid && p.pid !== proc.pid)) collect(child);
    if (!doomed.some((d) => d.pid === proc.pid)) doomed.push(proc);
  };
  for (const root of roots) collect(root);

  let killed = 0;
  for (const proc of doomed) {
    try {
      process.kill(proc.pid, 'SIGKILL');
      killed += 1;
    } catch (err) {
      // ESRCH: it died with its parent a moment ago, which is the outcome we wanted anyway.
      if (err.code !== 'ESRCH') {
        console.error(`> Could not remove leftover Fable process ${proc.pid}: ${err.message}`);
      }
    }
  }
  forgetWatchers();
  if (killed) {
    console.log(`> Removed ${killed} Fable process(es) left by an interrupted session`);
  }
  return killed;
}

module.exports = { freeOrphanedWatchers, recordWatchers, forgetWatchers };
