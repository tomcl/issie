// Issie's only route from the renderer to the operating system.
//
// This runs before the renderer's own code, in the preload context: it keeps Node access even once
// the renderer loses it, and contextBridge is the one-way valve between the two. Everything here is
// a named operation with a fixed shape. There is deliberately no general "run this in Node" call -
// that would hand straight back the capability context isolation exists to remove.
//
// sendSync rather than invoke: FilesIO's wrappers return values and are called from inside Elmish
// update, which is synchronous and cannot await. A round trip measures about 161us, against file
// operations of comparable cost and decode work an order of magnitude larger, so the synchronous
// shape survives the move. See docs/dev/contextIsolation.md for the measurements.

const { contextBridge, ipcRenderer } = require('electron');

const sync = (channel, payload) => ipcRenderer.sendSync(channel, payload);

// Main answers {ok, value, error} so that a refusal can be told from a legitimate result. Unwrapping
// it here means the F# side keeps the shape it already had: these operations return values and throw
// on failure, which is what every existing caller in FilesIO is written against.
const fsOp = (op, extra) => {
  const r = sync('issie:fs', Object.assign({ op }, extra));
  if (!r) throw new Error(`issie bridge: no response for ${op}`);
  if (!r.ok) throw new Error(r.error);
  return r.value;
};

const api = {
  // Constants, read once here rather than exposed as calls. platform, staticDir and the rest cannot
  // change while the process lives, so as data they cost no round trips at all - and __static in
  // particular *has* to arrive this way, being a webpack substitution that expands to an expression
  // over `path` and `process` which the renderer will no longer have.
  bootstrap: sync('issie:bootstrap', null),

  // Round-trip self-test. Used by the stage checks and by scripts/inspect-canvas.js to prove the
  // bridge is wired before anything depends on it.
  echo: (value) => sync('issie:echo', value),

  // The filesystem, as a fixed set of operations over paths main confines to directories it already
  // knows the user chose. Note what is not here: no open-by-handle, no recursive delete, no copy
  // that could be pointed anywhere, and above all nothing that takes the name of a thing to run.
  fs: {
    readFile: (path) => fsOp('readFile', { path }),
    writeFile: (path, data) => fsOp('writeFile', { path, data }),
    exists: (path) => fsOp('exists', { path }),
    isDirectory: (path) => fsOp('isDirectory', { path }),
    mkdir: (path) => fsOp('mkdir', { path }),
    readdir: (path) => fsOp('readdir', { path }),
    readdirDirectories: (path) => fsOp('readdirDirectories', { path }),
    unlink: (path) => fsOp('unlink', { path }),
    rename: (path, target) => fsOp('rename', { path, target }),
    modifiedTimeMs: (path) => fsOp('modifiedTimeMs', { path }),
  },

  // Dialogs. Main runs them, so a path the user picks is a path main watched them pick - which is
  // what lets it admit the containing directory as somewhere Issie may then read and write.
  // An empty array or empty string means the user cancelled.
  dialog: {
    open: (options) => sync('issie:dialog', { kind: 'open', options }),
    save: (options) => sync('issie:dialog', { kind: 'save', options }),
  },

  // Window controls that used to go through @electron/remote and reach the BrowserWindow directly.
  window: {
    getZoomLevel: () => sync('issie:window', { op: 'getZoomLevel' }),
    setZoomLevel: (value) => sync('issie:window', { op: 'setZoomLevel', value }),
    toggleFullScreen: () => sync('issie:window', { op: 'toggleFullScreen' }),
    // copy | cut | paste | selectAll | undo | redo, acting on whatever has focus
    edit: (op) => sync('issie:window', { op }),
  },

  openExternal: (url) => sync('issie:openExternal', url),
  clipboardWrite: (text) => sync('issie:clipboardWrite', text),

  // The one asynchronous call: shell.openPath reports failure by resolving with a message rather
  // than rejecting, so the caller wants the promise the F# side already handles with Promise.iter.
  revealInFileManager: (path) => ipcRenderer.invoke('issie:reveal', path),

  // Messages in both directions. These were ipcRenderer calls in the renderer itself, which is the
  // one thing contextIsolation takes away outright - a renderer cannot require('electron') at all.
  // The listeners are wrapped rather than passed through so that the renderer never receives the
  // IpcRendererEvent, which carries a sender it has no business holding.
  ipc: {
    quit: () => ipcRenderer.send('exit-the-app'),
    toggleDevTools: () => ipcRenderer.send('toggle-dev-tools'),
    showContextMenu: (menuType) => ipcRenderer.send('show-context-menu', menuType),
    setApplicationMenu: (template) => sync('issie:setApplicationMenu', template),

    onClosingWindow: (cb) => ipcRenderer.on('closingWindow', () => cb()),
    onWindowLostFocus: (cb) => ipcRenderer.on('windowLostFocus', () => cb()),
    onContextMenuCommand: (cb) => ipcRenderer.on('context-menu-command', (_e, arg) => cb(String(arg))),
    onApplicationMenuCommand: (cb) => ipcRenderer.on('application-menu-command', (_e, id) => cb(String(id))),
  },
};

// contextBridge is the correct call once contextIsolation is on. Until then the preload shares the
// renderer's context, where exposeInMainWorld is not available and a plain assignment is what lands.
// Keeping both means the flip in stage 8 does not have to touch this file.
if (process.contextIsolated) {
  contextBridge.exposeInMainWorld('issieBridge', api);
} else {
  window.issieBridge = api;
}
