module Main

open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open Node
open ContextMenus

(*
[<Emit("require('electron')")>]
let (contextBridge,ipcRenderer): (ContextBridge*IpcRenderer) = jsNative
*)

mainProcess.systemPreferences.setUserDefault?("NSDisabledDictationMenuItem","boolean", "true")
mainProcess.systemPreferences.setUserDefault?("NSDisabledCharacterPaletteMenu","boolean", "true")


(*
contextBridge.exposeInMainWorld("electronAPI", 
  Some {| listenForClicks = fun (callback: Event -> unit) ->  ipcRenderer.on("listenForClicks", callback) |})
*)

let args = 
    Api.``process``.argv
    |> Seq.toList
    |> List.map (fun s -> s.ToLower())

/// Returns true if any of flags are present as command line argument.    
let argFlagIsOn (flags:string list) = 
    let fl = List.map (fun (s:string) -> s.ToLower()) flags
    List.exists (fun flag -> List.contains flag args) fl

let hasDebugArgs() = argFlagIsOn ["--debug";"-d"]

//[<Emit("require(@electron/remote/main).initialize()")>]
//let initRemote():Unit = jsNative


/// Fix to access the deprecated @electron.remote module.
/// This must be enabled from main.fs
/// NB the interface used here is not precisely correct, because it
/// exposes the original electron-remote API. The @electron.remote API is
/// a bit reduced, but with some extra code to control access.
/// electronRemote replaces electron.remote and renderer.remote in old interface
[<ImportAll("@electron/remote/main")>]
let electronRemote : RemoteMainInterface = jsNative
electronRemote?initialize() // one-off initialization (see also electronRemote.enable below)



//initRemote()


let debug = false

let isMacos = Api.``process``.platform = Base.Darwin
let isWin = Api.``process``.platform = Base.Win32

        
// app.commandLine applies to every process Electron spawns - the GPU process, utility processes
// and every renderer - so this list is the whole app's, not the simulator's. The heap sizes are
// what Issie's simulations need. GC tracing writes a line per collection per process to stdout,
// which is a thing to ask for rather than have; it is not measurably slow, just noise nobody
// reads. --enable-logging was in this list and is not a V8 flag at all - it is a Chromium switch,
// so V8 only ever rejected it, and Chromium logging comes from ELECTRON_ENABLE_LOGGING anyway.
let jsFlags =
    [   "--expose-gc"
        "--max-old-space-size=3600"
        "--min-semi-space-size=64"
        if hasDebugArgs() then
            "--trace-gc"
            "--trace-gc-ignore-scavenger" ]
    |> String.concat " "

mainProcess.app.commandLine.appendSwitch("js-flags", jsFlags)


mainProcess.app.name <- "Issie"



// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

/// Splash shown while the renderer loads. Held here for the same reason as mainWindow.
let mutable splashWindow: BrowserWindow option = Option.None



let printListeners() =
    let listeners = mainProcess.ipcMain.listenerCount
    mainProcess.ipcMain.eventNames()
    |> Array.iter (fun name ->
        let lsNum = listeners name
        printfn $"{name} -> {lsNum}")

[<Emit("__static")>]
let staticDir() :string = jsNative

/// Seconds since this process was launched. Startup has to be measured from here and not from
/// the first line of this module: everything before that is Electron booting, and it is a phase
/// like any other.
[<Emit("process.uptime()")>]
let processUptime() : float = jsNative

/// Stamp a startup phase on stdout. The only useful question about a slow start is which phase
/// costs the seconds - Electron's boot, the splash's first paint, or the renderer's bundle - and
/// that is unanswerable unless each one says when it happened.
let stampStartup (phase: string) =
    printfn "[startup %6.3fs] %s" (processUptime()) phase

let mutable closeAfterSave = false

let wait n cont =
    Async.StartImmediate(async {
    try
        for i in [1..n] do
            printf "%i before" i
            do! Async.Sleep 1000
            printfn "%i after" i
    finally
        cont ()
        })

let dispatchToRenderer ((menuType, s): string*string) =
    match mainWindow with
    | Some win ->
        let args: obj option [] = [|Some $"{menuType},{s}"|]
        win.webContents.send("context-menu-command", args)
    | None -> ()

/// URL of the webpack dev server used in development mode.
/// Must match renderSrvOpts {port:} in scripts/start.js
let devServerUrl = "http://localhost:8672"

/// True only for Issie's own content: the dev server in development, the
/// packaged bundle otherwise. Everything else counts as external.
let isAppUrl (url: string) =
    url.StartsWith "file://" || url.StartsWith devServerUrl

/// Open a link in the user's default browser. Restricted to http(s) so a link
/// cannot be used to invoke an arbitrary OS protocol handler.
let openExternally (url: string) =
    if url.StartsWith "https://" || url.StartsWith "http://" then
        mainProcess.shell.openExternal url |> ignore

/// The renderer runs with nodeIntegration enabled, so any page loaded into it
/// gets full Node access. Refuse to navigate the app window away from Issie's
/// own content, and refuse to open popup windows at all: external links are
/// handed to the OS browser, which has no such privileges.
let hardenWebContents (webContents: WebContents) =
    webContents.setWindowOpenHandler (fun details ->
        openExternally details.url
        U2.Case1 {| action = "deny" |})

    webContents.``on_will-navigate`` (fun ev url ->
        if not (isAppUrl url) then
            ev.preventDefault()
            openExternally url)
    |> ignore

/// Where splash.html lives. __static is a compile-time substitution, and the production one
/// expands to a template literal over a bare `path` identifier that webpack's bundle does not
/// bind - so reading it unconditionally throws before any window exists. Every other consumer
/// (the icon below, FilesIO) branches around it for exactly this reason.
let splashPagePath () =
    let isDev = (``process``?defaultApp = true)
    if isDev then
        // relative in development ("static"), so resolve it against the working directory
        path.resolve (staticDir(), "splash.html")
    else
        // electron-builder copies static/ to resources/static (see package.json extraFiles)
        path.join (``process``?resourcesPath, "static", "splash.html")

/// Issie's renderer takes a noticeable time to start, and until it does the app window has
/// nothing in it. This frameless window covers that gap: it is the first thing on screen, and
/// the main window stays hidden behind it until its content has finished loading.
///
/// It stays hidden until it has painted. Chromium fills a new window with white until its web
/// contents produce a frame - backgroundColor does not change that - and a renderer process takes
/// long enough to spawn that showing the window on creation put a white rectangle on screen for
/// most of the start. onPainted then runs, and is where the app's own load is started from.
let createSplashWindow (onPainted: unit -> unit) =
    let page = splashPagePath()
    if not (fs.existsSync (U2.Case1 page)) then
        // No splash beats a broken one, and finding out by asking the filesystem costs a
        // millisecond where finding out by failing to load costs the ~2s a renderer takes to
        // start before it can report the error.
        JS.console.error ("Issie splash page not found: " + page)
        onPainted()
    else

    let options = jsOptions<BrowserWindowConstructorOptions> <| fun options ->
        options.width <- Some 640.
        options.height <- Some 360.
        options.frame <- Some false
        // Chromium paints a new window white until its web contents produce a frame, whatever
        // backgroundColor says. Staying hidden until ready-to-show is the only way to not put
        // an empty white rectangle on screen for as long as the page takes to paint.
        options.show <- Some false
        options.paintWhenInitiallyHidden <- Some true
        options.resizable <- Some false
        options.movable <- Some false
        options.minimizable <- Some false
        options.maximizable <- Some false
        options.center <- Some true
        options.alwaysOnTop <- Some true
        options.skipTaskbar <- Some true
        // matches the page's own background, so the window edge is not a lighter line around it
        options.backgroundColor <- Some "#071a24"
        options.title <- Some "Issie"
        options.webPreferences <- Some (
            jsOptions<WebPreferences> <| fun o ->
                // the splash is static markup: it needs neither Node nor dev tools
                o.nodeIntegration <- Some false
                o.contextIsolation <- Some true
                o.devTools <- Some false)

    let window = mainProcess.BrowserWindow.Create options
    stampStartup "splash window created"
    hardenWebContents window.webContents
    window.``once_ready-to-show`` (Function(fun _ ->
        stampStartup "splash painted"
        window.show()
        onPainted()))
    |> ignore
    window.loadFile page
    |> Promise.catch (fun err ->
        // Chromium renders its own error page when a load fails, and that page paints - so
        // without taking the window away here, ready-to-show would put "file not found" on
        // screen. Failure then goes down the same path as success: Issie carries on starting.
        JS.console.error ("Issie splash page failed to load: ", err)
        if not (window.isDestroyed()) then window.destroy()
        splashWindow <- Option.None
        onPainted())
    |> ignore
    splashWindow <- Some window

/// Take the splash down and put the app window on screen. Called when the renderer finishes
/// loading and again from a timeout, so a renderer that never loads cannot leave Issie with no
/// visible window at all - which means this must be safe to call more than once.
let revealMainWindow (window: BrowserWindow) =
    splashWindow
    |> Option.iter (fun splash -> if not (splash.isDestroyed()) then splash.destroy())
    splashWindow <- Option.None
    // Only the first call shows anything. The backstop fires 20s in whatever happened, by which
    // time the user may have unmaximised or moved the window, and maximising it again from under
    // them would be worse than the blank window this is all here to avoid.
    if not (window.isDestroyed()) && not (window.isVisible()) then
        window.setOpacity 1.0
        window.maximize()
        window.show()
        stampStartup "main window shown"

let createMainWindow () =
    let options = jsOptions<BrowserWindowConstructorOptions> <| fun options ->
        // hidden until the renderer has loaded: the splash is what the user looks at until then.
        // revealMainWindow is the only thing that shows it, and it always runs.
        options.show <- Some <| false
        options.autoHideMenuBar <- Some false
        options.backgroundColor <-  Some "#FFFFFF" // BUG - colors do not work
        options.opacity <- Some 0.8
        
        // fix for icons not working on linux
        // requires better solution for dist, maybe
        //if Api.``process``.platform = Base.Win32 then
        let isDev = (``process``?defaultApp = true)
        if isDev then
            options.icon <- Some (U2.Case2 (path.join(staticDir(), "icon-1.png")))
        else
            options.icon <- Some (U2.Case2 ("/static/icon-1.png"))

        //elif Api.``process``.platform = Base.Darwin then
            //options.icon <- (U2.Case2 (path.join(staticDir(), "icon.icns")))   (the icns icon does not work)
        options.title <- Some "issie"
        options.webPreferences <- Some (
            jsOptions<WebPreferences> <| fun o ->
                o.nodeIntegration <- Some true
                o.contextIsolation <- Some false
                o.devTools <- Some true) // allow dev tools to be opened

    let window = mainProcess.BrowserWindow.Create options

    let webContents = window.webContents
    // enable electronRemote for the renderer window
    electronRemote?enable webContents
    hardenWebContents webContents
    mainWindow <- Some window
    window

/// Set once the app's own start-up has been let go. Three things race to do that - the splash
/// painting, the splash failing to load, and a backstop timer - and exactly one must win. This
/// is process lifecycle rather than model state, which is what makes a mutable right here (see
/// docs/mutableState.md).
let mutable appStarted = false

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
let startRenderer (doAfterReady: BrowserWindow -> Unit) =
    mainProcess.app.on_ready(fun _ _ ->
        // Issie has no application menu: every key it cares about is resolved by KeyBindings
        // against the context the user is in, and an Electron menu registers its accelerators
        // globally and unconditionally. Clearing it HERE, before the first window exists, is what
        // makes that true - Electron installs its own File/Edit/View/Window/Help menu otherwise,
        // and a window created with autoHideMenuBar false shows it.
        //
        // The renderer used to be left to remove it, by assigning app.applicationMenu across the
        // @electron/remote bridge from inside an Elmish subscription. That is both late - the
        // standard menus are up for as long as the renderer takes to load - and conditional on a
        // remote property assignment landing at all. When it did not, they simply stayed. A debug
        // build still adds its Development menu from the renderer afterwards, which is where it
        // has to be built since its items dispatch into the app.
        stampStartup "app ready"
        mainProcess.Menu.setApplicationMenu None

        let startApp () =
            if not appStarted then
                appStarted <- true
                let window = createMainWindow()
                stampStartup "main window created"
                doAfterReady window

        // A window cannot show anything until Chromium has spawned a renderer process for it and
        // that process has painted, which measures at 1.5-2.5s here for a page of 6KB. Two windows
        // created together spawn two of them at once and both go slower: the splash used to take
        // ~2.9s to paint that way against ~2.0s when it has the machine to itself. So the app's
        // own window - the one that then fetches megabytes of bundle - waits for the splash.
        // The cost is ~0.7s on the total start, bought so that the splash is up for it.
        createSplashWindow startApp
        // Backstop: whatever the splash does, it must not be able to stop Issie starting. Well
        // clear of the ~2s a renderer process takes to spawn and paint, since firing early costs
        // the wait without buying the splash - it just puts the two renderers back in contention.
        JS.setTimeout startApp 5000 |> ignore) |> ignore


let loadAppIntoWidowWhenReady (window: BrowserWindow) =
    let loadWindowContent (window: BrowserWindow) =
        if window.isMinimized() then window.show()

        // Load the index.html of the app.    

        let isDev = (``process``?defaultApp = true)

        if isDev then
            // run the dev tools
            if debug then window.webContents.openDevTools() // default open in this case

            devServerUrl
            |> window.loadURL
            |> ignore

            ``process``.on("uncaughtException", fun err -> JS.console.error(err))
            |> ignore  
        else
            // run the app
            let url =
                path.join ( __dirname,  "index.html")
                |> sprintf "file:///%s" 
                |> Api.URL.Create

            Api.URL.format(url, createEmpty<Url.IFormatOptions>)
            |> window.loadURL
            |> ignore
    loadWindowContent window
    window.webContents.on("did-finish-load", ( fun () ->
        stampStartup "renderer loaded"
        revealMainWindow window))
    |> ignore
    // Backstop: whatever happens to the load, the splash comes down and the window appears.
    JS.setTimeout (fun () -> revealMainWindow window) 20000 |> ignore

  
   
let rec addListeners (window: BrowserWindow) =    
        // Emitted when the window is closed.
    window.on_closed <| (new Function(fun _ ->
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow <- Option.None))
    |> ignore


    window.on_blur <| (new Function (fun _ ->
        window.webContents.send("windowLostFocus")))
    |> ignore

    (window.on: (string * (obj -> unit)) -> Events.EventEmitter)(
            // called when attempt is made to close the window
            // send this to renderer to let it decide what to do
            // unless closeAfterSave is true.
            "close", ( fun e ->
                if not closeAfterSave then
                    // prevent default closure
                    ((unbox e):Event).preventDefault() |> ignore
                    window.webContents.send("closingWindow")))
            |> ignore      

    // quit programmatically from renderer
    mainProcess.ipcMain.on ("exit-the-app", fun _ -> 
        closeAfterSave <- true
        mainWindow
        |> Option.iter (fun win -> win.close()))
        |> ignore

    // return user data directory
    mainProcess.ipcMain.on ("get-user-data", fun (event: IpcMainEvent) args -> 
        let userAppDirOpt =
            try
                Some <| mainProcess.app.getPath AppGetPath.UserData
            with 
                | _ -> None
        event.returnValue <- unbox (Option.defaultValue "" userAppDirOpt)) |> ignore


    mainProcess.ipcMain.on ("toggle-dev-tools", fun _ _ -> 
        mainWindow
        |> Option.iter (fun win -> win.webContents.toggleDevTools()))
        |> ignore

    mainProcess.ipcMain.on("show-context-menu", fun (event:IpcMainEvent) args ->
        makeMenu window dispatchToRenderer args event
        )|> ignore
       
        


    // Quit when all windows are closed.
    mainProcess.app.``on_window-all-closed`` <| Function(fun _ ->
        // On Macos it is common for applications and their menu bar
        // to stay active until the user quits explicitly with Cmd + Q
        // currently this feature can't be properly implemented
        // if Api.``process``.platform <> Base.Darwin then
        mainProcess.app.quit()) |> ignore

    mainProcess.app.on_activate (fun _ _ ->
        // On OS X it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if mainWindow.IsNone then
            window = createMainWindow() |> ignore
            mainWindow <- Some window
            addListeners window
            |> loadAppIntoWidowWhenReady |> ignore
            mainWindow <- Some window) |> ignore
    window

stampStartup "main bundle loaded"

let rec startup() =
    startRenderer( fun win ->
        win
        |> addListeners
        |> loadAppIntoWidowWhenReady
        |> ignore)

startup()



                




