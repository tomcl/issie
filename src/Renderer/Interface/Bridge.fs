/// The renderer's half of the bridge to the main process.
///
/// Partner to src/Preload/preload.js and src/Main/Bridge.fs. This module is the only place the
/// renderer reads anything the operating system knows, and it exists so that the rest of the code
/// asks for `Bridge.staticDir` rather than reaching for `__static`, `process.platform` or
/// `@electron/remote` - none of which survive contextIsolation.
///
/// Everything here is a value rather than a function. These are constants for the life of the
/// process, so the preload fetches them once and this module just reads the result: no round trip,
/// no ordering to get wrong, and five operations that never have to exist on the bridge at all.
module Bridge

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

/// The object src/Preload/preload.js attached before any renderer code ran. Reached through an Emit
/// rather than a binding because there is deliberately no type for it: the preload's surface and
/// Bridge.fs in the main process are the contract, and both are small enough to read.
[<Emit("window.issieBridge")>]
let private bridge: obj = jsNative

let private boot: obj = bridge?bootstrap

/// "win32" | "darwin" | "linux", as node reports it.
let platform: string = unbox boot?platform

/// Absolute path to the static asset directory, resolved by main. Was three different expressions
/// in the renderer, one of which (__static) is a webpack substitution over `path` and `process`.
let staticDir: string = unbox boot?staticDir

/// The per-user writable Issie directory - app.getPath "userData".
let userData: string = unbox boot?userData

/// The user's Documents directory, used as the default location in file dialogs.
let documents: string = unbox boot?documents

let cwd: string = unbox boot?cwd

let isDev: bool = unbox boot?isDev

/// Launch switches, read by JSHelpers.setDebugLevel. Sent as values so that app.commandLine itself
/// never needs to be reachable from here.
let hasDebugSwitch: bool = unbox boot?hasDebugSwitch
let hasWSwitch: bool = unbox boot?hasWSwitch
let logSwitch: string = unbox boot?logSwitch

// ---- filesystem ----
//
// Each of these is one round trip to main, which resolves the path against the directories the user
// actually chose before touching anything. They throw on failure - a missing file, an OS error, or a
// path main refused - which is the shape FilesIO's callers were already written against.
//
// Reached by Emit rather than a typed binding for the same reason as the bootstrap above: the
// contract is preload.js and Main's Bridge.fs, and inventing an F# interface for it would only add a
// third place to keep in step.

[<Emit("window.issieBridge.fs.readFile($0)")>]
let fsReadFile (filePath: string) : string = jsNative

[<Emit("window.issieBridge.fs.writeFile($0,$1)")>]
let fsWriteFile (filePath: string) (data: string) : unit = jsNative

[<Emit("window.issieBridge.fs.exists($0)")>]
let fsExists (filePath: string) : bool = jsNative

[<Emit("window.issieBridge.fs.isDirectory($0)")>]
let fsIsDirectory (filePath: string) : bool = jsNative

[<Emit("window.issieBridge.fs.mkdir($0)")>]
let fsMkdir (folderPath: string) : unit = jsNative

[<Emit("window.issieBridge.fs.readdir($0)")>]
let fsReaddir (folderPath: string) : string array = jsNative

[<Emit("window.issieBridge.fs.readdirDirectories($0)")>]
let fsReaddirDirectories (folderPath: string) : string array = jsNative

[<Emit("window.issieBridge.fs.unlink($0)")>]
let fsUnlink (filePath: string) : unit = jsNative

[<Emit("window.issieBridge.fs.rename($0,$1)")>]
let fsRename (oldPath: string) (newPath: string) : unit = jsNative

/// null when the path is not there, so the caller gets None rather than an exception.
[<Emit("window.issieBridge.fs.modifiedTimeMs($0)")>]
let fsModifiedTimeMs (filePath: string) : float option = jsNative

// ---- dialogs, shell and window ----
//
// Main displays the dialogs, which is not only where Electron wants them once contextIsolation is
// on: it is what makes a path the user chose distinguishable from a path the renderer invented, and
// so what lets main admit the chosen directory as somewhere Issie may read and write.

/// Empty when the user cancelled.
[<Emit("window.issieBridge.dialog.open($0)")>]
let dialogOpen (options: obj) : string array = jsNative

/// Empty when the user cancelled.
[<Emit("window.issieBridge.dialog.save($0)")>]
let dialogSave (options: obj) : string = jsNative

[<Emit("window.issieBridge.window.getZoomLevel()")>]
let getZoomLevel () : float = jsNative

[<Emit("window.issieBridge.window.setZoomLevel($0)")>]
let setZoomLevel (level: float) : unit = jsNative

[<Emit("window.issieBridge.window.toggleFullScreen()")>]
let toggleFullScreen () : unit = jsNative

/// "copy" | "cut" | "paste" | "selectAll" | "undo" | "redo", acting on whatever has focus. Used on
/// macOS, where these keys were delivered by the application menu Issie no longer installs.
[<Emit("window.issieBridge.window.edit($0)")>]
let editCommand (op: string) : unit = jsNative

/// http(s) only; main refuses anything else rather than handing an arbitrary protocol to the OS.
[<Emit("window.issieBridge.openExternal($0)")>]
let openExternal (url: string) : unit = jsNative

[<Emit("window.issieBridge.clipboardWrite($0)")>]
let clipboardWrite (text: string) : unit = jsNative

/// Resolves with the reason the folder could not be shown, or "" when it could.
[<Emit("window.issieBridge.revealInFileManager($0)")>]
let revealInFileManager (folderPath: string) : JS.Promise<string> = jsNative

// ---- the FPGA toolchain ----
//
// A stage, not a program: main owns which tool a stage means, so nothing here can name an
// executable. The job id stands in for the ChildProcess the draw block's model used to hold, since
// a live node object cannot cross a context boundary.

/// "" when the build could not be started; main logs the reason.
[<Emit("window.issieBridge.build.start($0,$1,$2,$3,$4)")>]
let buildStart (stage: string) (projectPath: string) (name: string) (device: string) (profile: string) : string = jsNative

/// -1 while running, -2 for an unknown job, otherwise the exit code.
[<Emit("window.issieBridge.build.status($0)")>]
let buildStatus (jobId: string) : int = jsNative

[<Emit("window.issieBridge.build.cancel($0)")>]
let buildCancel (jobId: string) : unit = jsNative

/// "iverilog" or "vvp" only - main runs nothing else. dst is where stdout is written, "" for
/// nowhere. Fire and forget, matching what the Verilog test harness always did.
[<Emit("window.issieBridge.build.runDevTool($0,$1,$2)")>]
let runDevTool (tool: string) (args: string array) (dst: string) : unit = jsNative

// ---- the debug UART ----

[<Emit("window.issieBridge.uart.connectAndRead($0)")>]
let uartConnectAndRead (viewers: int) : JS.Promise<string array> = jsNative

[<Emit("window.issieBridge.uart.simpleConnect()")>]
let uartSimpleConnect () : JS.Promise<unit> = jsNative

[<Emit("window.issieBridge.uart.disconnect()")>]
let uartDisconnect () : unit = jsNative

[<Emit("window.issieBridge.uart.step()")>]
let uartStep () : unit = jsNative

[<Emit("window.issieBridge.uart.pause()")>]
let uartPause () : unit = jsNative

[<Emit("window.issieBridge.uart.continue()")>]
let uartContinue () : unit = jsNative

[<Emit("window.issieBridge.uart.readAllViewers($0)")>]
let uartReadAllViewers (viewers: int) : JS.Promise<string array> = jsNative

[<Emit("window.issieBridge.uart.stepAndReadAllViewers($0)")>]
let uartStepAndReadAllViewers (viewers: int) : JS.Promise<string array> = jsNative

// ---- diagnostics ----
//
// Answered by the preload rather than main: webFrame and getProcessMemoryInfo describe the renderer
// process, so asking main would report the wrong one.

/// Per-resource {count, size, liveSize}, as webFrame.getResourceUsage gives it.
[<Emit("window.issieBridge.diagnostics.resourceUsage()")>]
let resourceUsage () : obj = jsNative

[<Emit("window.issieBridge.diagnostics.clearCache()")>]
let clearCache () : unit = jsNative

/// {private, residentSet, shared}, in kilobytes.
[<Emit("window.issieBridge.diagnostics.processMemory()")>]
let processMemory () : JS.Promise<obj> = jsNative

/// Channel name to listener count, for the preload's own listeners.
[<Emit("window.issieBridge.diagnostics.ipcListenerCounts()")>]
let ipcListenerCounts () : obj = jsNative

// ---- messages to and from main ----

[<Emit("window.issieBridge.ipc.quit()")>]
let quit () : unit = jsNative

[<Emit("window.issieBridge.ipc.toggleDevTools()")>]
let toggleDevTools () : unit = jsNative

[<Emit("window.issieBridge.ipc.showContextMenu($0)")>]
let showContextMenu (menuType: string) : unit = jsNative

/// The application menu as plain data. Closures cannot cross the bridge, so each item carries an id
/// and main sends that id back when it is clicked - see onApplicationMenuCommand.
[<Emit("window.issieBridge.ipc.setApplicationMenu($0)")>]
let setApplicationMenu (template: obj array) : unit = jsNative

[<Emit("window.issieBridge.ipc.onClosingWindow($0)")>]
let onClosingWindow (callback: unit -> unit) : unit = jsNative

[<Emit("window.issieBridge.ipc.onWindowLostFocus($0)")>]
let onWindowLostFocus (callback: unit -> unit) : unit = jsNative

/// The argument is "menuType,item", as ContextMenus.makeMenu spells it.
[<Emit("window.issieBridge.ipc.onContextMenuCommand($0)")>]
let onContextMenuCommand (callback: string -> unit) : unit = jsNative

[<Emit("window.issieBridge.ipc.onApplicationMenuCommand($0)")>]
let onApplicationMenuCommand (callback: string -> unit) : unit = jsNative

#else
// Under plain .NET - the test suite - there is no preload, no Electron and no window. These are the
// values that keep FilesIO and the modules above it loadable there. staticDir is empty for the same
// reason FilesIO.staticFileDirectory always was: tests are given the directory they work on.
let platform =
    if System.OperatingSystem.IsWindows() then "win32"
    elif System.OperatingSystem.IsMacOS() then "darwin"
    else "linux"

let staticDir = ""
let userData = System.IO.Path.GetTempPath()
let documents = System.Environment.GetFolderPath System.Environment.SpecialFolder.MyDocuments
let cwd = System.IO.Directory.GetCurrentDirectory()
let isDev = true
let hasDebugSwitch = false
let hasWSwitch = false
let logSwitch = ""

// The filesystem functions have no .NET counterparts here on purpose: FilesIO keeps its own
// System.IO implementations behind the same #if, and that is what the test suite exercises.
//
// What does need stubbing is everything below - dialogs, window controls, menus and messages - which
// is called from UI code that is not itself guarded, so it has to compile under .NET even though no
// test can reach it. There is no window, no Electron and no user, so these do nothing and return the
// answer a cancelled or absent version of themselves would give.
let dialogOpen (_options: obj) : string array = [||]
let dialogSave (_options: obj) : string = ""
let getZoomLevel () : float = 0.0
let setZoomLevel (_level: float) : unit = ()
let toggleFullScreen () : unit = ()
let editCommand (_op: string) : unit = ()
let openExternal (_url: string) : unit = ()
let clipboardWrite (_text: string) : unit = ()

/// Unreachable under .NET: it is only called from the draw block's "open containing folder", which
/// needs a window. Written as a throw rather than a lifted promise so that nothing has to construct
/// one outside a JavaScript runtime.
let revealInFileManager (_folderPath: string) : Fable.Core.JS.Promise<string> =
    failwith "revealInFileManager needs Electron: there is no file manager to open under .NET"

let buildStart (_stage: string) (_projectPath: string) (_name: string) (_device: string) (_profile: string) : string = ""
let buildStatus (_jobId: string) : int = -2
let buildCancel (_jobId: string) : unit = ()
let runDevTool (_tool: string) (_args: string array) (_dst: string) : unit = ()

let uartConnectAndRead (_viewers: int) : Fable.Core.JS.Promise<string array> =
    failwith "the debug UART needs Electron and a USB device"

let uartSimpleConnect () : Fable.Core.JS.Promise<unit> =
    failwith "the debug UART needs Electron and a USB device"

let uartDisconnect () : unit = ()
let uartStep () : unit = ()
let uartPause () : unit = ()
let uartContinue () : unit = ()

let uartReadAllViewers (_viewers: int) : Fable.Core.JS.Promise<string array> =
    failwith "the debug UART needs Electron and a USB device"

let uartStepAndReadAllViewers (_viewers: int) : Fable.Core.JS.Promise<string array> =
    failwith "the debug UART needs Electron and a USB device"

let resourceUsage () : obj = box null
let clearCache () : unit = ()

let processMemory () : Fable.Core.JS.Promise<obj> =
    failwith "process memory reporting needs Electron"

let ipcListenerCounts () : obj = box null

let quit () : unit = ()
let toggleDevTools () : unit = ()
let showContextMenu (_menuType: string) : unit = ()
let setApplicationMenu (_template: obj array) : unit = ()
let onClosingWindow (_callback: unit -> unit) : unit = ()
let onWindowLostFocus (_callback: unit -> unit) : unit = ()
let onContextMenuCommand (_callback: string -> unit) : unit = ()
let onApplicationMenuCommand (_callback: string -> unit) : unit = ()
#endif

/// Platform tests, so that the string literals live here and not at thirty call sites.
let isWindows = platform = "win32"
let isMac = platform = "darwin"
