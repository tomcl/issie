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

let private computeRoots () =
    let fixedRoots =
        [ staticDirectory (), false
          tryGetPath AppGetPath.UserData, true ]

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
        let usable = roots () |> List.filter (fun (_, writable) -> writable || not needsWrite)

        match usable |> List.exists (fun (root, _) -> isWithin root p) with
        | true -> Ok p
        | false ->
            let verb = if needsWrite then "written" else "read"
            Error $"'{p}' is outside every directory Issie may have {verb}"

// ---------------------------------------------------------------------------------------------
// The filesystem channel
// ---------------------------------------------------------------------------------------------

/// Answers are {ok, value, error} rather than bare values so that a refusal or an OS error can be
/// told apart from a legitimate result, and re-raised on the renderer side where the existing
/// callers already expect these operations to throw.
let private ok (value: obj) = {| ok = true; value = value; error = "" |}
let private err (message: string) = {| ok = false; value = null; error = message |}

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
// Dialogs, shell and window
// ---------------------------------------------------------------------------------------------

let private focusedWindow () = mainProcess.BrowserWindow.getFocusedWindow ()

let private isDirectorySync (p: string) =
    try
        fs.existsSync (U2.Case1 p) && fs.lstatSync(U2.Case1 p).isDirectory ()
    with _ ->
        false

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

let private setApplicationMenu (template: obj array) =
    match focusedWindow () with
    | None -> Log.warn "bridge: no window to attach an application menu to"
    | Some w ->
        attachMenuClicks (fun id -> w.webContents.send ("application-menu-command", Some(box id))) template

        match template with
        | [||] -> mainProcess.Menu.setApplicationMenu None
        | items ->
            items
            |> Array.map (unbox<MenuItemConstructorOptions> >> U2.Case1)
            |> mainProcess.Menu.buildFromTemplate
            |> Some
            |> mainProcess.Menu.setApplicationMenu

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

    onSync "issie:bootstrap" (fun _ -> box (bootstrap ()))

    // Round-trip self-test. Returns its argument, so a caller can prove the bridge is live and
    // measure what a crossing costs without needing a channel that does real work.
    onSync "issie:echo" box
