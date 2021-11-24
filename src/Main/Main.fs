module Main

open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open Node


mainProcess.systemPreferences.setUserDefault?("NSDisabledDictationMenuItem","boolean", "true")
mainProcess.systemPreferences.setUserDefault?("NSDisabledCharacterPaletteMenu","boolean", "true")

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

        



mainProcess.app.name <- "Issie"



// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

[<Emit("__static")>]
let staticDir() :string = jsNative

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

let createMainWindow () =
    let options = jsOptions<BrowserWindowConstructorOptions> <| fun options ->
        options.show <- Some <| true
        options.autoHideMenuBar <- Some false
        options.backgroundColor <-  Some "#FFFFFF" // BUG - colors do not work
        options.opacity <- Some 0.8
        
        // fix for icons not working on linux
        // requires better solution for dist, maybe
        //if Api.``process``.platform = Base.Win32 then
        options.icon <- Some (U2.Case2 (path.join(staticDir(), "icon-1.png")))
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
    mainWindow <- Some window
    window

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
let startRenderer (doAfterReady: BrowserWindow -> Unit) =
    mainProcess.app.on_ready(fun _ _ -> 
        let window = createMainWindow()
        //printfn "window created"
        window
        |> doAfterReady) |> ignore


let loadAppIntoWidowWhenReady (window: BrowserWindow) =
    //printfn "setting up load when ready..."
    let loadWindowContent (window: BrowserWindow) =
        //printfn "starting load..."
        if window.isMinimized() then window.show()

        // Load the index.html of the app.    

        let isDev = (``process``?defaultApp = true)

        if isDev then
            // run the dev tools
            if debug then window.webContents.openDevTools() // default open in this case

            sprintf $"http://localhost:{``process``.env?ELECTRON_WEBPACK_WDS_PORT}"
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
        //printfn "done load"
    loadWindowContent window
    window.webContents.on("did-finish-load", ( fun () -> 
        window.setOpacity 1.0
        window.maximize()))
    
   
let rec addListeners (window: BrowserWindow) =    
        // Emitted when the window is closed.
    //printfn "adding Main process listeners"
    window.on_closed <| (new Function(fun _ ->
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow <- Option.None))
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
        //printfn "Closing Issie..."
        mainWindow
        |> Option.iter (fun win -> win.close()))
        |> ignore

    mainProcess.ipcMain.on ("toggle-dev-tools", fun _ _ -> 
        mainWindow
        |> Option.iter (fun win -> win.webContents.toggleDevTools()))
        |> ignore

    // Quit when all windows are closed.
    mainProcess.app.``on_window-all-closed`` <| Function(fun _ ->
        // On OS X it is common for applications and their menu bar
        // to stay active until the user quits explicitly with Cmd + Q
        if Api.``process``.platform <> Base.Darwin then
            mainProcess.app.quit()) |> ignore

    mainProcess.app.on_activate (fun _ _ ->
        // On OS X it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if mainWindow.IsNone then
            //printfn "recreating window..."
            window = createMainWindow() |> ignore
            mainWindow <- Some window
            addListeners window
            |> loadAppIntoWidowWhenReady |> ignore
            mainWindow <- Some window) |> ignore
    window

let rec startup() =
    startRenderer( fun win ->
        win
        |> addListeners
        |> loadAppIntoWidowWhenReady
        |> ignore)

startup()



                




