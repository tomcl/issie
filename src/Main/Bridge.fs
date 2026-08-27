/// The main-process half of Issie's renderer bridge.
///
/// Every operation the renderer is allowed to ask the operating system for is registered here, as a
/// named channel with a fixed shape. The renderer half is src/Preload/preload.js, and the two are
/// meant to be read together: nothing should be exposed there that is not answered here, and nothing
/// answered here should take a request general enough to stand in for `require`.
///
/// Handlers use event.returnValue, the synchronous form, because the renderer's file wrappers return
/// values from inside Elmish update and cannot await. get-user-data in Main.fs already worked this
/// way before any of this existed.
module Bridge

open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open Node
open Node.ChildProcess

// process globals, reached by Emit rather than through a binding so that what is being read is
// visible at the point of use - these are the values the renderer will no longer be able to see.
[<Emit("process.cwd()")>]
let private cwd () : string = jsNative

[<Emit("process.platform")>]
let private platform () : string = jsNative

[<Emit("process.resourcesPath")>]
let private resourcesPath () : string = jsNative

/// True for a development run, by the same test the rest of Main.fs uses.
let private isDev () : bool = ``process``?defaultApp = true

/// __static is a webpack DefinePlugin substitution rather than a runtime value, and in a production
/// build it expands to a template literal over `path` and `process.resourcesPath`. That is exactly
/// why staticDir cannot stay in the renderer: after the flip there is no `path` there to expand
/// against. Main has both, so main resolves it once and sends the answer.
[<Emit("__static")>]
let private staticDefine () : string = jsNative

/// Absolute path to the static asset directory.
///
/// The renderer used to work this out three different ways - a bare "./resources/static" relative to
/// the working directory on Windows and Linux, and __dirname/../../static on macOS. Both land on the
/// same place as process.resourcesPath, which is what Main.fs already uses for the splash page, so
/// this collapses them into the one reliable form.
let staticDirectory () =
    if isDev () then
        // in development __static is the literal "static", relative to the working directory
        path.resolve (staticDefine (), ".")
    else
        path.join (resourcesPath (), "static")

/// Where the preload bundle is, for webPreferences.preload. webpack.config.preload.js writes it
/// beside the main bundle in both development and production, so no branch is needed here.
let preloadPath () = path.join (__dirname, "preload.js")

// Issie's own switches, read from process.argv rather than app.commandLine.
//
// They arrive after the app path (scripts/start.js explains why), and Electron hands those to the
// application rather than parsing them into Chromium's switch table - so hasSwitch cannot see them.
// argv can, and it is what Main.hasDebugArgs has always used.

let private rawArgs () : string list = ``process``.argv |> List.ofSeq

let private hasArg (names: string list) =
    let lowered = rawArgs () |> List.map (fun (s: string) -> s.ToLowerInvariant())
    names |> List.exists (fun n -> List.contains (n.ToLowerInvariant()) lowered)

/// The value of a `--name=value` switch, "" when it is absent. The name is matched without regard
/// to case; the value is left exactly as it was given.
let private argValue (name: string) =
    let prefix = (name + "=").ToLowerInvariant()

    rawArgs ()
    |> List.tryPick (fun (a: string) ->
        if a.ToLowerInvariant().StartsWith prefix then Some(a.Substring prefix.Length) else None)
    |> Option.defaultValue ""

/// app.getPath throws for a location the platform does not define, and every caller of this can
/// carry on without the path, so a failure becomes an empty string rather than a dead startup.
let private tryGetPath (which: AppGetPath) =
    try
        mainProcess.app.getPath which
    with _ ->
        ""

/// The constants the renderer needs, gathered once.
///
/// These are data rather than calls on purpose. None of them can change while the process lives, so
/// delivering them at preload time costs one round trip in total instead of one per use, and removes
/// five operations from the bridge that would otherwise have to exist.
let private bootstrap () =
    {| platform = platform ()
       staticDir = staticDirectory ()
       userData = tryGetPath AppGetPath.UserData
       documents = tryGetPath AppGetPath.Documents
       cwd = cwd ()
       isDev = isDev ()
       // the launch switches JSHelpers.setDebugLevel reads; sent as values so that the command line
       // itself never has to be reachable from the renderer
       hasDebugSwitch = hasArg [ "--debug"; "-d" ]
       hasWSwitch = hasArg [ "--w"; "-w" ]
       logSwitch = argValue "--log" |}

// ---------------------------------------------------------------------------------------------
// Root confinement
//
// This is the part that decides whether any of the rest is worth doing. A bridge that answers
// readFile for an arbitrary path leaves a compromised renderer able to read the user's home
// directory - the same outcome as nodeIntegration, reached one indirection later. So main resolves
// every requested path and requires it to sit inside a root it already knows about.
//
// Three kinds of root:
//   the static asset directory   read only  - shipped libraries and demos, inside the installation
//   the user data directory      read/write - Issie's own store: settings, demo copies, libraries
//   project directories          read/write - wherever the user keeps their work
//
// The first two are fixed and main computes them itself. The third cannot be, so a directory becomes
// a project root only when main has a reason of its own to believe the user chose it: either it came
// back from a dialog main displayed, or it is listed in the recents file - which main reads itself,
// from inside userData - and it actually contains a .dprj project marker.
//
// The residual gap, stated plainly because it is easy to talk yourself out of: the renderer writes
// IssieSettings.json, so it can nominate a directory it can already write to and then have it
// admitted here. Closing that means moving ownership of the settings file into main, which is a
// larger change than this one and is worth doing separately.
// ---------------------------------------------------------------------------------------------

let private isWin = platform () = "win32"
let private sep = if isWin then "\\" else "/"

/// Absolute, normalised, and case-folded on Windows where the filesystem is.
let private canonical (p: string) =
    let resolved = path.resolve (p, ".")
    if isWin then resolved.ToLowerInvariant() else resolved

let private isWithin (root: string) (candidate: string) =
    if root = "" then
        false
    else
        let r = (canonical root).TrimEnd(sep.[0])
        let c = canonical candidate
        // the separator matters: without it "C:\proj" would also admit "C:\projects-elsewhere"
        c = r || c.StartsWith(r + sep)

/// Project directories admitted so far. Session state of the main process rather than anything
/// modelled, which is what a mutable is for here (docs/mutableState.md).
let mutable private projectRoots: string list = []

let private hasProjectMarker (dir: string) =
    try
        fs.readdirSync (U2.Case1 dir)
        |> Seq.exists (fun (name: string) -> name.EndsWith ".dprj")
    with _ ->
        false

/// Admit a directory the user chose through a dialog main itself displayed. Called from the dialog
/// handlers, which is the only place a path can arrive having been picked by a human.
let mutable private cacheGeneration = 0

let allowProjectRoot (dir: string) =
    if dir <> "" && not (List.exists (fun r -> canonical r = canonical dir) projectRoots) then
        projectRoots <- dir :: projectRoots
        // the cache below is keyed on the settings file, which this does not touch
        cacheGeneration <- cacheGeneration + 1

let private settingsPath () = path.join (tryGetPath AppGetPath.UserData, "IssieSettings.json")

/// The recent projects, read by main from its own userData rather than taken from the renderer.
let private recentProjects () =
    try
        let settings = settingsPath ()
        if not (fs.existsSync (U2.Case1 settings)) then
            []
        else
            let parsed: obj = JS.JSON.parse (fs.readFileSync (settings, "utf8"))
            match parsed?RecentProjects with
            | null -> []
            | recents -> unbox<string array> recents |> List.ofArray
    with _ ->
        []

/// The Verilog test cases the Development > Verilog menu compiles, runs and writes output beside.
/// They are addressed by paths relative to the checkout, so in a development run this has to be
/// reachable; a packaged app has no equivalent and gets no such root.
///
/// Deliberately this directory and not the working directory. The whole checkout was the first
/// version, and it quietly made the static assets writable in development - they sit inside it -
/// so the read-only root above held only in a packaged app, which is the wrong way round for a
/// difference between what you test and what you ship. Every relative path these tools use is
/// under here.
let private verilogTestDirectory () =
    path.join (cwd (), "src", "Renderer", "VerilogComponent", "test")

let private computeRoots () =
    let fixedRoots =
        [ staticDirectory (), false
          tryGetPath AppGetPath.UserData, true
          if isDev () then verilogTestDirectory (), true ]

    let fromRecents =
        recentProjects ()
        |> List.filter hasProjectMarker
        |> List.map (fun dir -> dir, true)

    fixedRoots @ fromRecents @ (projectRoots |> List.map (fun dir -> dir, true))

/// Working out the roots means reading the settings file and looking for a .dprj in each project it
/// names, which is far too much to repeat per request: doing it every time cost 4ms a call, against
/// the 0.16ms the round trip itself takes. Keyed on the settings file's timestamp so that a project
/// added to the recents list is picked up on the next call without anything having to invalidate it.
let mutable private cachedRoots: (float * int * (string * bool) list) option = None

let private settingsStamp () =
    try
        let p = settingsPath ()
        if fs.existsSync (U2.Case1 p) then unbox<float> (fs.lstatSync (U2.Case1 p))?mtimeMs else 0.0
    with _ ->
        0.0

/// Every root currently in force, with whether it may be written to.
let private roots () =
    let stamp = settingsStamp ()

    match cachedRoots with
    | Some (cachedStamp, generation, cached) when cachedStamp = stamp && generation = cacheGeneration ->
        cached
    | _ ->
        let computed = computeRoots ()
        cachedRoots <- Some(stamp, cacheGeneration, computed)
        computed

/// Resolve a requested path against the roots. Error carries the reason, which is logged in main and
/// raised in the renderer, because a silent refusal here would surface as an unexplained empty file.
let private checkPath (needsWrite: bool) (p: string) : Result<string, string> =
    if p = "" then
        Error "empty path"
    else
        let containing = roots () |> List.filter (fun (root, _) -> isWithin root p)

        // Read-only wins over writable. Roots can nest - the static assets sit inside the checkout,
        // and a project could be put anywhere - so asking "is there SOME root that allows this"
        // lets an outer writable root quietly cancel an inner read-only one. Asking instead
        // "is there ANY root that forbids it" makes read-only mean what it says wherever it lands.
        let readOnlyHere = containing |> List.exists (fun (_, writable) -> not writable)
        let writableHere = containing |> List.exists (fun (_, writable) -> writable)

        match containing, needsWrite with
        | [], _ -> Error $"'{p}' is outside every directory Issie may use"
        | _, false -> Ok p
        | _, true when readOnlyHere -> Error $"'{p}' is inside a directory Issie may only read"
        | _, true when writableHere -> Ok p
        | _, true -> Error $"'{p}' is outside every directory Issie may write to"

// ---------------------------------------------------------------------------------------------
// The filesystem channel
// ---------------------------------------------------------------------------------------------

/// Answers are {ok, value, error} rather than bare values so that a refusal or an OS error can be
/// told apart from a legitimate result, and re-raised on the renderer side where the existing
/// callers already expect these operations to throw.
let private ok (value: obj) = {| ok = true; value = value; error = "" |}
let private err (message: string) = {| ok = false; value = null; error = message |}

let private isDirectorySync (p: string) =
    try
        fs.existsSync (U2.Case1 p) && fs.lstatSync(U2.Case1 p).isDirectory ()
    with _ ->
        false

[<Emit("$0.readdirSync($1, { withFileTypes: true }).filter(e => e.isDirectory()).map(e => e.name)")>]
let private subdirectoryNamesOf (fsModule: obj) (folderPath: string) : string array = jsNative

/// One request against the filesystem, already named by the renderer as one of a fixed set of
/// operations. There is no path here that reaches a program, a module, or an arbitrary node call.
let private handleFs (request: obj) =
    let op: string = unbox request?op
    let p: string = unbox request?path

    // Writing ops are checked against writable roots only; everything else needs read access.
    let needsWrite =
        match op with
        | "writeFile" | "mkdir" | "unlink" | "rename" -> true
        | _ -> false

    match checkPath needsWrite p with
    | Error reason ->
        Log.warn $"bridge refused {op}: {reason}"
        err reason
    | Ok _ ->
        try
            match op with
            | "readFile" -> ok (box (fs.readFileSync (p, "utf8")))
            | "exists" -> ok (box (fs.existsSync (U2.Case1 p)))
            | "isDirectory" ->
                ok (box (fs.existsSync (U2.Case1 p) && fs.lstatSync(U2.Case1 p).isDirectory ()))
            | "readdir" -> ok (box (fs.readdirSync (U2.Case1 p)))
            | "readdirDirectories" -> ok (box (subdirectoryNamesOf fs p))
            | "modifiedTimeMs" ->
                if fs.existsSync (U2.Case1 p) then ok (fs.lstatSync (U2.Case1 p))?mtimeMs
                else ok null
            | "writeFile" ->
                let data: string = unbox request?data
                let options = createObj [ "encoding" ==> "utf8" ] |> Some
                fs.writeFileSync (p, data, options)
                ok null
            | "mkdir" ->
                fs.mkdirSync p
                ok null
            | "unlink" ->
                fs.unlink (U2.Case1 p, ignore)
                ok null
            | "rename" ->
                // the destination is a write as much as the source is, so it is checked too
                let target: string = unbox request?target
                match checkPath true target with
                | Error reason ->
                    Log.warn $"bridge refused rename target: {reason}"
                    err reason
                | Ok _ ->
                    fs.renameSync (p, target)
                    ok null
            | unknown -> err $"unknown filesystem operation '{unknown}'"
        with e ->
            err (string e?message)

// ---------------------------------------------------------------------------------------------
// The browse channel
//
// The one channel above that is NOT confined to the roots, and the reason it can be: what crosses
// it is the names of directories and two counts of what is in them. No file is opened, nothing
// here reaches readFile, and no path learned this way becomes readable by anything else.
//
// It exists because confinement and a folder picker are otherwise contradictory. A picker may only
// list folders the user has already opened, but the whole point of opening it is to find one they
// have not - so Issie's own Open Project dialog, which starts in the folder holding the last
// project, was refused the moment it drew itself and reported "That folder does not exist" about
// the user's Documents. The native dialog does not have the problem because main runs it, and shows
// the user their whole filesystem regardless; drawing the list in the renderer instead is what
// makes projects distinguishable from ordinary folders, and should not cost the user the ability
// to go and look.
//
// So the capability granted here is knowing what a directory is called - which any file dialog
// discloses anyway - and not the readFile-anywhere that confinement exists to prevent.
// ---------------------------------------------------------------------------------------------

/// The file names directly inside a folder. Empty when it cannot be read, which the caller tells
/// apart from an empty folder by having already asked whether the folder is there.
let private fileNamesOf (folderPath: string) : string array =
    try
        fs.readdirSync (U2.Case1 folderPath) |> Seq.toArray
    with _ ->
        [||]

/// One subdirectory, as the two facts that tell an Issie project from an ordinary folder. Which
/// combination means what is the renderer's rule (FilesIO.ProjectDirectory), not settled here:
/// main reports what is on the disk and nothing about what it means.
let private browseEntry (parent: string) (name: string) =
    let full = path.join (parent, name)
    let files = fileNamesOf full

    {| path = full
       hasMarker = files |> Array.exists (fun (f: string) -> f.EndsWith ".dprj")
       sheetCount = files |> Array.filter (fun (f: string) -> f.EndsWith ".dgm") |> Array.length |}

/// Every immediate subdirectory of a folder, for the project browser to draw. `exists` says whether
/// the folder is there at all, so that a folder which is missing and one which cannot be read are
/// not reported to the user as the same thing.
let private handleBrowse (request: obj) =
    let folderPath: string = unbox request?path

    if not (isDirectorySync folderPath) then
        {| exists = false; entries = [||] |} |> box
    else
        let names =
            try
                subdirectoryNamesOf fs folderPath
            with e ->
                Log.warn $"browse could not list '{folderPath}': {e?message}"
                [||]

        {| exists = true
           entries = names |> Array.map (browseEntry folderPath) |}
        |> box

/// Admit a folder the user picked in Issie's own Open Project dialog, having checked here that it
/// is what the renderer says it is: a directory that actually holds an Issie project.
///
/// The check is what makes this safe to offer, and it is the same one the recents list already
/// passes through - so this grants nothing that was not reachable already. The comment on root
/// confinement above states the residual gap plainly: the renderer writes IssieSettings.json, so it
/// can already nominate any directory holding a .dprj and have it admitted on the next call. This
/// admits exactly that set, without the round trip through the settings file.
///
/// Sheets without a marker are included because the browser offers to open those too, and refusing
/// them here would make it offer something that then failed to load.
let private handleAdmitProject (request: obj) =
    let folderPath: string = unbox request?path

    if folderPath = "" || not (isDirectorySync folderPath) then
        false
    else
        let files = fileNamesOf folderPath
        let looksLikeProject =
            files |> Array.exists (fun (f: string) -> f.EndsWith ".dprj" || f.EndsWith ".dgm")

        if looksLikeProject then
            allowProjectRoot folderPath
            true
        else
            Log.warn $"not admitting '{folderPath}': it holds no Issie project"
            false

// ---------------------------------------------------------------------------------------------
// Dialogs, shell and window
// ---------------------------------------------------------------------------------------------

let private focusedWindow () = mainProcess.BrowserWindow.getFocusedWindow ()

/// A path the user picked in a dialog main itself displayed is, by construction, a path the user
/// chose - so it and the directory holding it become reachable. This is the main way a project
/// outside userData ever becomes readable, and the reason the dialogs had to move here first.
let private admitChosen (p: string) =
    if p <> "" then
        allowProjectRoot (if isDirectorySync p then p else path.dirname p)

/// Dialog options are already plain data on the renderer side - a title, a button label, a default
/// path, a list of property names - so they cross unchanged and are handed to Electron as they are.
let private handleDialog (request: obj) =
    let kind: string = unbox request?kind
    let options = request?options

    match kind with
    | "open" ->
        let chosen =
            match focusedWindow () with
            | Some w -> mainProcess.dialog.showOpenDialogSync (w, unbox options)
            | None -> mainProcess.dialog.showOpenDialogSync (unbox options)

        chosen |> Option.iter (Seq.iter admitChosen)
        chosen |> Option.map Seq.toArray |> Option.defaultValue [||] |> box
    | "save" ->
        let chosen = mainProcess.dialog.showSaveDialogSync (unbox options)
        chosen |> Option.iter admitChosen
        chosen |> Option.defaultValue "" |> box
    | unknown ->
        Log.warn $"bridge: unknown dialog kind '{unknown}'"
        box null

/// Show a directory in the platform's file manager.
///
/// On Windows this launches explorer.exe rather than calling shell.openPath, because the folder
/// window is created by the already-running explorer.exe, which has no right to take the foreground
/// away from Issie - so shell.openPath opens it *behind* the app. A process launched by the
/// foreground process does have that right and passes it on. This moved here wholesale from
/// FilesIO: it is the one spawn the renderer used to make, and the argument is a path main has
/// already confined rather than anything the renderer names.
///
/// Answers a promise of the reason it could not be shown, empty when it could - which is the shape
/// shell.openPath already has, and why this one channel is invoke/handle rather than synchronous.
let private revealInFileManager (target: string) : JS.Promise<string> =
    match checkPath false target with
    | Error reason -> Promise.lift reason
    | Ok _ ->
        if isWin then
            // explorer.exe exits non-zero even when it succeeds, and for a path that is not there
            // it silently opens a default folder rather than reporting anything - so there is no
            // result worth reading back, and the path is checked up front instead.
            if isDirectorySync target then
                let options = {| detached = true; stdio = "ignore"; shell = false |} |> toPlainJsObj
                childProcess.spawn ("explorer.exe", ResizeArray [ target ], options) |> ignore
                Promise.lift ""
            else
                Promise.lift "the directory is no longer there"
        else
            mainProcess.shell.openPath target

/// Open a link in the user's default browser, restricted to http(s) so that a link cannot be used
/// to invoke an arbitrary OS protocol handler. Main.fs applies the same rule to navigation.
let private openExternal (url: string) =
    if url.StartsWith "https://" || url.StartsWith "http://" then
        mainProcess.shell.openExternal url |> ignore
    else
        Log.warn $"bridge refused to open '{url}': only http and https are allowed"

let private handleWindow (request: obj) =
    let op: string = unbox request?op

    match focusedWindow () with
    | None -> box null
    | Some w ->
        let wc = w.webContents

        match op with
        | "getZoomLevel" -> box (wc.getZoomLevel ())
        | "setZoomLevel" ->
            wc.setZoomLevel (unbox request?value)
            box null
        | "isFullScreen" -> box (w.isFullScreen ())
        | "toggleFullScreen" ->
            w.setFullScreen (not (w.isFullScreen ()))
            box null
        // The text editing commands, needed on macOS where these keys used to be delivered by the
        // application menu Issie no longer installs. They act on whatever has focus in the window.
        | "copy" -> wc.copy (); box null
        | "cut" -> wc.cut (); box null
        | "paste" -> wc.paste (); box null
        | "selectAll" -> wc.selectAll (); box null
        | "undo" -> wc.undo (); box null
        | "redo" -> wc.redo (); box null
        | unknown ->
            Log.warn $"bridge: unknown window operation '{unknown}'"
            box null

// ---------------------------------------------------------------------------------------------
// The application menu
//
// An Electron menu item's action is a JavaScript function, and a function is the one thing that
// cannot cross a context boundary. So the renderer sends the menu as data - the same objects it used
// to build, minus the click handlers, each carrying an id - and main puts a handler on every item
// that does nothing but send that id back. The renderer looks the id up and runs the action.
//
// This is the pattern show-context-menu and context-menu-command have always used; the application
// menu is only now catching up with it.
// ---------------------------------------------------------------------------------------------

/// A field that is absent from a JavaScript object reads as `undefined`, which F#'s `null` pattern
/// does not match - so presence is asked in JavaScript's own terms. Getting this wrong threw inside
/// a sendSync handler, and a sendSync handler that throws never sets returnValue, which hangs the
/// renderer for good rather than failing.
[<Emit("($0) != null")>]
let private isPresent (value: obj) : bool = jsNative

/// Walk the template attaching a click to every item that has an id and is not a role. A role item
/// is handled by Electron itself, and giving it a click would take that behaviour away.
let rec private attachMenuClicks (send: string -> unit) (items: obj array) =
    items
    |> Array.iter (fun item ->
        if isPresent item?submenu then
            attachMenuClicks send (unbox item?submenu)
        elif isPresent item?id && not (isPresent item?role) then
            let id: string = unbox item?id
            item?click <- (fun _ _ _ -> send id))

/// Deliver a clicked item's id to whatever windows are alive at that moment. The window is
/// looked up per click rather than captured when the menu was registered: registration happens
/// while the splash is still the focused window, and a handler holding on to the splash throws
/// "Object has been destroyed" on every click for the rest of the session. Only the app window's
/// preload listens on this channel, so the send is a no-op for any other window.
let private sendMenuCommand (id: string) =
    mainProcess.BrowserWindow.getAllWindows ()
    |> Seq.iter (fun w ->
        if not (w.isDestroyed ()) then
            w.webContents.send ("application-menu-command", Some(box id)))

let private setApplicationMenu (template: obj array) =
    attachMenuClicks sendMenuCommand template

    match template with
    | [||] -> mainProcess.Menu.setApplicationMenu None
    | items ->
        items
        |> Array.map (unbox<MenuItemConstructorOptions> >> U2.Case1)
        |> mainProcess.Menu.buildFromTemplate
        |> Some
        |> mainProcess.Menu.setApplicationMenu

// ---------------------------------------------------------------------------------------------
// The FPGA toolchain
//
// The renderer used to build the command line itself and spawn it. A bridge method taking a program
// name and arguments would hand that capability straight back, so it does not exist: what crosses is
// which STAGE to run, and main turns that into a program. The renderer cannot name an executable.
//
// The process itself stays here too. It used to live in the draw block's model as a ChildProcess,
// which is a live node object and cannot cross a context boundary - so the model holds an opaque job
// id now and asks about it by name.
// ---------------------------------------------------------------------------------------------

/// Running and finished build jobs. Process handles belonging to the main process, not model state
/// (docs/mutableState.md); finished ones are kept so that a status asked for after the fact still
/// has an exit code to give.
let mutable private buildJobs: Map<string, ChildProcess> = Map.empty
let mutable private buildExitCodes: Map<string, int> = Map.empty
let mutable private nextBuildJob = 0

/// Still running.
let private buildRunning = -1
/// No such job - asked about an id main has never issued.
let private buildUnknown = -2

/// The Verilog include directory, from the static assets rather than from the working directory.
///
/// The renderer worked this out as cwd + "/static/hdl" or cwd + "/resources/static/hdl", choosing
/// between them on debugLevel - so a production build launched with --debug looked in the
/// development location and found nothing. staticDirectory is the same answer without that trap.
let private hdlDirectory () = path.join (staticDirectory (), "hdl")

/// Board-specific settings. Kept here rather than passed in, so that the renderer names a device
/// from a fixed list and main decides what that means.
let private deviceSettings (device: string) (profile: string) =
    let hdl = hdlDirectory ()
    let debug = profile = "debug"

    match device with
    | "IceStick" ->
        let pcf = if debug then "icestick_debug.pcf" else "icestick.pcf"
        Some(path.join (hdl, pcf), "--hx1k", "tq144", "i:0x0403:0x6010")
    | "IssieStick-v0.1" ->
        let pcf = if debug then "issiestick-0.1_debug.pcf" else "issiestick-0.1.pcf"
        Some(path.join (hdl, pcf), "--hx4k", "tq144", "i:0x0403:0xed1c")
    | "IssieStick-v1.0" ->
        let pcf = if debug then "issiestick-1.0_debug.pcf" else "issiestick-1.0.pcf"
        Some(path.join (hdl, pcf), "--hx8k", "bg121", "i:0x0403:0xed1c")
    | _ -> None

/// The one place a stage becomes a program. Adding a tool means adding a case here, which is the
/// point: there is no path by which the renderer can ask for anything not on this list.
let private toolFor (stage: string) (projectPath: string) (name: string) (device: string) (profile: string) =
    match deviceSettings device profile with
    | None -> None
    | Some (pcf, deviceType, devicePackage, usbDevice) ->
        let hdl = hdlDirectory ()
        let build = path.join (projectPath, "build")

        let verilog = path.join (projectPath, name + ".v")
        let json = path.join (build, name + ".json")
        let asc = path.join (build, name + ".asc")
        let bin = path.join (build, name + ".bin")

        match stage with
        | "synthesis" ->
            let script = $"read_verilog -I{hdl} {verilog}; synth_ice40 -flatten -json {json}"
            Some("yosys", [ "-p"; script ])
        | "placeAndRoute" ->
            Some(
                "nextpnr-ice40",
                [ "--package"; devicePackage
                  deviceType
                  "--pcf"; pcf
                  "--json"; json
                  "--asc"; asc ]
            )
        | "generate" -> Some("icepack", [ asc; bin ])
        | "upload" -> Some("iceprog", [ "-d"; usbDevice; bin ])
        | _ -> None

/// Start a stage. Answers the job id, or "" when it could not be started - the reason is logged
/// here, and the renderer stops the compilation.
let private startBuild (request: obj) =
    let stage: string = unbox request?stage
    let projectPath: string = unbox request?path
    let name: string = unbox request?name
    let device: string = unbox request?device
    let profile: string = unbox request?profile

    // the toolchain writes into <project>/build, so the project has to be somewhere Issie may write
    match checkPath true projectPath with
    | Error reason ->
        Log.error $"build refused: {reason}"
        ""
    | Ok _ ->
        match toolFor stage projectPath name device profile with
        | None ->
            Log.error $"build: no tool for stage '{stage}' on device '{device}'"
            ""
        | Some (prog, args) ->
            try
                let options = {| shell = false |} |> toPlainJsObj
                let child = childProcess.spawn (prog, ResizeArray args, options)

                nextBuildJob <- nextBuildJob + 1
                let jobId = $"build{nextBuildJob}"
                buildJobs <- Map.add jobId child buildJobs

                child.stdout.on ("data", fun _ -> ()) |> ignore
                child.stderr.on ("data", fun e -> Log.error $"compilation: {e}") |> ignore

                child.on (
                    "exit",
                    fun code ->
                        buildExitCodes <- Map.add jobId (unbox code) buildExitCodes
                        buildJobs <- Map.remove jobId buildJobs)
                |> ignore

                Log.dbg Log.Misc $"build {jobId}: {prog} for stage {stage}"
                jobId
            with e ->
                Log.error $"build: could not start '{prog}': {e.Message}"
                ""

/// -1 while running, the exit code once finished, -2 for an id main never issued. Polled rather than
/// pushed because the renderer already ticks the progress display once a second and has nothing to
/// do with an exit it hears about sooner.
let private buildStatus (jobId: string) =
    match Map.tryFind jobId buildExitCodes with
    | Some code -> code
    | None -> if Map.containsKey jobId buildJobs then buildRunning else buildUnknown

let private cancelBuild (jobId: string) =
    match Map.tryFind jobId buildJobs with
    | Some child ->
        child.kill ()
        buildJobs <- Map.remove jobId buildJobs
    | None -> ()

/// The Verilog developer tools behind the Development > Verilog menu, which compile and run the
/// test cases under src/Renderer/VerilogComponent/test with Icarus Verilog.
///
/// A closed pair, like the toolchain above: the renderer asks for "iverilog" or "vvp" and gets
/// nothing else. They are fire and forget, as they were before - the callers never waited - and the
/// stdout capture that used to happen in the renderer happens here, since main is where the file
/// write is confined.
let private runDevTool (request: obj) =
    let tool: string = unbox request?tool
    let args: string array = unbox request?args
    let dst: string = unbox request?dst

    let program =
        match tool with
        | "iverilog" -> Some "iverilog"
        | "vvp" -> Some "vvp"
        | _ -> None

    match program with
    | None -> Log.error $"dev tool: '{tool}' is not one Issie runs"
    | Some prog ->
        let destinationOk = dst = "" || (match checkPath true dst with Ok _ -> true | Error _ -> false)

        if not destinationOk then
            Log.error $"dev tool {prog}: refused to write to '{dst}'"
        else
            try
                let options = {| shell = false |} |> toPlainJsObj
                let child = childProcess.spawn (prog, ResizeArray args, options)

                child.stdout.on (
                    "data",
                    fun (d: string) ->
                        if dst <> "" then
                            let existing =
                                if fs.existsSync (U2.Case1 dst) then fs.readFileSync (dst, "utf8") else ""

                            let options = createObj [ "encoding" ==> "utf8" ] |> Some
                            fs.writeFileSync (dst, existing + d, options))
                |> ignore

                child.stderr.on ("data", fun e -> Log.error $"{prog}: {e}") |> ignore
                child.on ("exit", fun code -> Log.dbg Log.Misc $"{prog} exited with {code}") |> ignore
            with e ->
                Log.error $"dev tool: could not start '{prog}': {e.Message}"

// ---------------------------------------------------------------------------------------------
// The dotnet sidecar (skeleton)
//
// A .NET process serving a WebSocket on loopback, which the renderer talks to directly with the
// browser WebSocket API - src/Sidecar/Program.fs is the process's half of the contract. Main's
// part is the same shape as the toolchain above: it owns the executable path and the process,
// and the renderer only ever learns a loopback port number and a token.
// ---------------------------------------------------------------------------------------------

/// Process handle belonging to the main process, not model state (docs/mutableState.md).
let mutable private sidecarProc: ChildProcess option = None

/// 0 until the sidecar has printed its handshake; back to 0 if it exits.
let mutable private sidecarPort = 0

/// Set when the app is going down, so that the sidecar being killed on purpose is not mistaken
/// for it dying and started again in the middle of a shutdown.
let mutable private sidecarQuitting = false

/// Consecutive starts that never got as far as the handshake.
///
/// Reset by every start that does, which is what makes the two failures behave differently. A
/// sidecar that worked and then died - killed for memory by a build too large, or taking a fault
/// this side could not answer - is worth starting again however often it happens, because each
/// time it comes back. One that cannot start at all - no dotnet, a source tree that will not
/// build, a missing binary in a packaged app - would otherwise be started for ever, several times
/// a second, writing the same failure to the log each time.
let mutable private sidecarFailedStarts = 0

/// How many consecutive failures to start before giving up until the app is restarted.
let private sidecarStartAttempts = 5

/// How long to wait before starting it again. Long enough not to spin, short enough that a
/// simulation started straight after a crash finds the transport there.
let private sidecarRestartMs = 1000

/// Registered once rather than per process: the handler is about the sidecar in general, and a
/// restart that added a second copy would kill twice and, worse, keep the dead child alive in the
/// closure it captured.
let mutable private sidecarQuitHooked = false

[<Emit("require('crypto').randomBytes(16).toString('hex')")>]
let private randomHexToken () : string = jsNative

/// Fresh per app run. The sidecar rejects a connection that does not present it, so nothing else
/// on the machine can reach the socket just by scanning localhost ports.
let private sidecarToken = randomHexToken ()

/// The sidecar's environment: the token it authenticates connections with, plus one .NET tuning
/// knob it cannot set for itself from inside its own process.
///
/// TC_CallCountingDelayMs is how long tiered compilation waits before it starts counting calls
/// towards promoting a method out of tier 0, and the wait RESTARTS whenever more new code is
/// being compiled. Building a simulation compiles a great deal of new code, so on the default
/// 100ms the hot reducers stayed in unoptimised tier-0 code for the first several hundred
/// milliseconds of the first simulation of a session - measured on 3cpu at 57-87 cycles/ms
/// against a settled 500, with the jump to full speed arriving abruptly once promotion finally
/// happened. Setting it to zero promotes them as soon as they are hot, which took three
/// consecutive 1.1M-cycle runs from 405/529/511 cycles/ms to 529/525/526.
///
/// Zero rather than turning tiering off: tier 0 still gives the process a quick start, and
/// dynamic PGO - which needs the tiering it rides on - still gets to specialise the reducers.
/// An environment variable rather than a runtimeconfig entry because this knob is read from the
/// environment by the CLR and has no supported MSBuild property.
[<Emit("Object.assign({}, process.env, { ISSIE_SIDECAR_TOKEN: $0, DOTNET_TC_CallCountingDelayMs: '0' })")>]
let private envWithSidecarToken (token: string) : obj = jsNative

/// Start the sidecar, and start it again if it dies. In development that is `dotnet run`, which
/// builds first when it must - the port channel below answers null until the handshake lands, so a
/// slow first build costs nothing but waiting. In production it is the self-contained binary
/// electron-builder placed under resources/sidecar (scripts/publish-sidecar.js).
///
/// **What a restart does and does not recover.** The transport comes back on its own: the port
/// channel answers the new port, and the renderer's `request` connects when it has no socket, so
/// the next thing asked of the simulator reaches the new process. What does NOT come back is the
/// simulation - the new process holds no design and no session, so every command naming the old
/// session's epoch is refused by name, which is exactly what that number is for. The user's next
/// Start or Refresh builds again and works.
///
/// It is deliberately left there. Rebuilding from here would mean this side deciding to simulate,
/// and "nothing but a start path builds" is the rule that deleted the build-retry storm
/// (docs/dev/sidecarInvariants.md, section J). A restart that silently rebuilt would also be a
/// simulation the user did not ask for, of whatever design happened to be open.
let rec startSidecar () =
    let prog, args =
        if isDev () then
            "dotnet", [ "run"; "-c"; "Release"; "--project"; path.join (cwd (), "src", "Sidecar") ]
        else
            let exe = if platform () = "win32" then "issie-sidecar.exe" else "issie-sidecar"
            path.join (resourcesPath (), "sidecar", exe), []

    try
        let options =
            {| shell = false
               env = envWithSidecarToken sidecarToken |}
            |> toPlainJsObj

        let child = childProcess.spawn (prog, ResizeArray args, options)
        sidecarProc <- Some child

        // stdout arrives in arbitrary chunks; keep the partial line, parse only complete ones
        let mutable partialLine = ""

        child.stdout.on (
            "data",
            fun (data: obj) ->
                let lines = (partialLine + string data).Split '\n'
                partialLine <- Array.last lines

                for line in lines[.. lines.Length - 2] do
                    let line = line.Trim()

                    if line.StartsWith "SIDECAR_LISTENING " then
                        sidecarPort <- int (line.Substring "SIDECAR_LISTENING ".Length)
                        // it started, so whatever went wrong before it is not what is wrong now
                        sidecarFailedStarts <- 0
                        Log.dbg Log.Misc $"sidecar listening on port {sidecarPort}")
        |> ignore

        child.stderr.on ("data", fun e -> Log.error $"sidecar: {e}") |> ignore

        child.on (
            "exit",
            fun code ->
                sidecarPort <- 0
                sidecarProc <- None

                if sidecarQuitting then
                    Log.dbg Log.Misc $"sidecar exited with {code}"
                elif sidecarFailedStarts >= sidecarStartAttempts then
                    Log.error
                        $"the .NET simulator has failed to start {sidecarFailedStarts} times in a row and will not be started again. Restart Issie to simulate."
                else
                    sidecarFailedStarts <- sidecarFailedStarts + 1
                    Log.warn $"the .NET simulator exited with {code}; starting it again"
                    JS.setTimeout startSidecar sidecarRestartMs |> ignore)
        |> ignore

        if not sidecarQuitHooked then
            sidecarQuitHooked <- true

            // Said as early as the app says it, so that a sidecar dying during a shutdown - taken
            // down by the OS with everything else - is not started again on the way out.
            mainProcess.app.``on_before-quit`` (fun _ -> sidecarQuitting <- true) |> ignore

            // Belt: take it down with the app. The braces are the sidecar's own stdin-EOF watchdog,
            // which catches every exit path this handler cannot.
            mainProcess.app.``on_will-quit`` (fun _ ->
                sidecarQuitting <- true
                sidecarProc |> Option.iter (fun c -> c.kill ()))
            |> ignore
    with e ->
        Log.error $"sidecar: could not start '{prog}': {e.Message}"

        // A spawn that throws never reaches the exit handler above, so the retry is issued here or
        // a missing dotnet would end the sidecar for the session.
        if not sidecarQuitting && sidecarFailedStarts < sidecarStartAttempts then
            sidecarFailedStarts <- sidecarFailedStarts + 1
            JS.setTimeout startSidecar sidecarRestartMs |> ignore

// ---------------------------------------------------------------------------------------------
// The debug UART
//
// IS-uart.js requires the native `usb` module, which is exactly the kind of thing a renderer without
// node cannot load - so the file moved here wholesale and the renderer drives it by message. Its
// eight operations were already promise-returning with no shared objects, so they port one for one.
//
// Nothing here takes a path or a program name: the arguments are viewer counts. The device is found
// by vendor and product id inside IS-uart.js, as it always was.
// ---------------------------------------------------------------------------------------------

[<ImportAll("./IS-uart.js")>]
let private uart: obj = jsNative

let private uartCall (op: string) (n: int) : JS.Promise<obj> =
    match op with
    | "connectAndRead" -> uart?connectAndRead (n)
    | "simpleConnect" -> uart?simpleConnect ()
    | "disconnect" -> uart?disconnect ()
    | "step" -> uart?step ()
    | "pause" -> uart?pauseOp ()
    | "continue" -> uart?continuedOp ()
    | "readAllViewers" -> uart?readAllViewers (n)
    | "stepAndReadAllViewers" -> uart?stepAndReadAllViewers (n)
    | unknown -> failwithf $"unknown uart operation '{unknown}'"

/// Register a synchronous channel so that it always answers.
///
/// sendSync blocks the renderer until returnValue is assigned. A handler that throws first never
/// assigns it, and the renderer then waits for good - not a crash with a stack, but a frozen window.
/// That happened once during this migration and cost more to diagnose than this wrapper costs to
/// have, so no handler is registered without it.
let private onSync (channel: string) (handler: obj -> obj) =
    mainProcess.ipcMain.on (
        channel,
        fun (event: IpcMainEvent) args ->
            let answer =
                try
                    handler args
                with e ->
                    Log.error $"bridge handler '{channel}' threw: {e.Message}"
                    box null

            event.returnValue <- unbox answer)
    |> ignore

/// Register every bridge channel. Called once, before any window exists.
let register () =
    onSync "issie:setApplicationMenu" (fun args ->
        setApplicationMenu (unbox args)
        box null)

    // handleFs already answers {ok, error} for anything it can catch; onSync is the backstop for
    // anything it cannot, so that a bug here can never freeze the renderer.
    onSync "issie:fs" (handleFs >> box)

    // Listing folders for the Open Project dialog, and admitting the one chosen from it. Kept as
    // its own channel rather than two more filesystem operations, because these two are exactly
    // the ones the roots do not confine and that difference should be visible at the wiring.
    onSync "issie:browse" handleBrowse
    onSync "issie:admitProject" (handleAdmitProject >> box)

    onSync "issie:dialog" handleDialog

    onSync "issie:window" handleWindow

    onSync "issie:openExternal" (fun args ->
        openExternal (unbox args)
        box null)

    // Electron's clipboard module is a main-process one under contextIsolation. navigator.clipboard
    // would avoid a channel, but it is asynchronous and needs both a secure context and document
    // focus, neither of which is guaranteed for a file:// window - so one more channel it is.
    onSync "issie:clipboardWrite" (fun args ->
        mainProcess.clipboard.writeText (unbox args)
        box null)

    // The binding types a handler's promise as Promise<unit>; the value it actually resolves with is
    // whatever crosses the wire, so the string this resolves with reaches the renderer unchanged.
    mainProcess.ipcMain.handle (
        "issie:reveal",
        fun _ args ->
            revealInFileManager (unbox args)
            |> unbox<JS.Promise<unit>>
            |> U2.Case1)
    |> ignore

    onSync "issie:buildStart" (fun args -> box (startBuild args))
    onSync "issie:buildStatus" (fun args -> box (buildStatus (unbox args)))

    onSync "issie:buildCancel" (fun args ->
        cancelBuild (unbox args)
        box null)

    onSync "issie:runDevTool" (fun args ->
        runDevTool args
        box null)

    // The UART operations are asynchronous all the way down - a USB transfer is not something to
    // block the renderer on - so these are the one group that stays invoke/handle throughout.
    mainProcess.ipcMain.handle (
        "issie:uart",
        fun _ args ->
            let op: string = unbox args?op
            let n: int = unbox args?n

            uartCall op n
            |> unbox<JS.Promise<unit>>
            |> U2.Case1)
    |> ignore

    onSync "issie:bootstrap" (fun _ -> box (bootstrap ()))

    // The sidecar's endpoint: null until it has reported in (and again after it dies). Polled by
    // the renderer at the moment it wants to connect, for the same reason buildStatus is polled.
    onSync "issie:sidecarPort" (fun _ ->
        if sidecarPort = 0 then
            box null
        else
            box {| port = sidecarPort; token = sidecarToken |})

    // Round-trip self-test. Returns its argument, so a caller can prove the bridge is live and
    // measure what a crossing costs without needing a channel that does real work.
    onSync "issie:echo" box
