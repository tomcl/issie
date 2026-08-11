// Kill whatever is listening on a TCP port, so that starting the app never fails on a leftover.
//
// An interrupted dev session - Ctrl-C in the wrong window, a closed terminal, a killed debugger -
// leaves the webpack dev server holding 8672 and Electron holding the DevTools port. The next
// `npm run dev` then dies with EADDRINUSE *after* a full Fable compile, and the run before that
// looks like it worked: Electron opens, webpack never starts, and the window sits blank while the
// log says "ready".
//
// This matches on WHO HOLDS THE PORT rather than on what a process looks like, which is what makes
// it reliable. scripts/clean-dev.js matches command lines - the right tool for sweeping a whole
// stale session, including a fable watch that holds no port at all - but a command line is a guess
// about intent, and the guess is wrong for anything started by hand or by a debugger. The port is
// the thing that actually conflicts, so it is the thing to test.

const os = require('os');
const { execFileSync } = require('child_process');

const windows = os.platform() === 'win32';

/// The pids listening on `port`. Empty when nothing is, or when the OS refuses to say.
function listenersOn(port) {
  try {
    if (windows) {
      // -ano: numeric, all connections, with the owning pid. The pid is the last column, and only
      // LISTENING rows matter - an outgoing connection to the port is somebody using the app, not
      // somebody holding the socket we need.
      //
      // No `-p tcp`: that filters to IPv4, and webpack-dev-server binds "localhost", which resolves
      // to ::1 first on Windows. The row to find says TCPv6, so restricting to TCP found nothing
      // and this reported the port free while it was held.
      const out = execFileSync('netstat', ['-ano'], { encoding: 'utf8' });
      return out
        .split('\n')
        .filter((line) => /LISTENING/i.test(line))
        .map((line) => line.trim().split(/\s+/))
        .filter((cols) => cols.length >= 5 && new RegExp(`[:.]${port}$`).test(cols[1]))
        .map((cols) => Number(cols[cols.length - 1]))
        .filter((pid) => Number.isInteger(pid) && pid > 0);
    }
    const out = execFileSync('lsof', ['-nP', `-iTCP:${port}`, '-sTCP:LISTEN', '-t'], {
      encoding: 'utf8',
    });
    return out.split('\n').map(Number).filter((pid) => Number.isInteger(pid) && pid > 0);
  } catch {
    // netstat and lsof both exit non-zero when they find nothing, and lsof may not be installed.
    // Either way there is nothing to kill and nothing worth saying.
    return [];
  }
}

/// Free `port`, returning the number of processes killed. Never throws: failing to clear a port is
/// not a reason to refuse to start, because the port may be free by the time it is bound.
function freePort(port, what) {
  const mine = new Set([process.pid, process.ppid]);
  const pids = [...new Set(listenersOn(port))].filter((pid) => !mine.has(pid));
  for (const pid of pids) {
    try {
      process.kill(pid, 'SIGKILL');
      console.log(`> Freed ${what} port ${port}: killed leftover process ${pid}`);
    } catch (err) {
      if (err.code !== 'ESRCH') {
        console.error(`> Could not free ${what} port ${port} (process ${pid}): ${err.message}`);
      }
    }
  }
  // The kill is asynchronous: the socket is released when the process actually dies, which is
  // after this call returns. Wait for the port to go quiet rather than racing it - a bind that
  // fails here costs a whole recompile to retry.
  if (pids.length) {
    const deadline = Date.now() + 3000;
    while (Date.now() < deadline && listenersOn(port).some((pid) => !mine.has(pid))) {
      // execFileSync above is itself the pause; nothing to sleep on without adding a dependency
    }
  }
  return pids.length;
}

module.exports = { freePort };
