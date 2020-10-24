module Main

open Fable.Core
open Fable.Core.JsInterop
open Electron
open Node

let args = 
    Api.``process``.argv
    |> Seq.toList
    |> List.map (fun s -> s.ToLower())

/// Returns true if any of flags are present as command line argument.    
let argFlagIsOn (flags:string list) = 
    let fl = List.map (fun (s:string) -> s.ToLower()) flags
    List.exists (fun flag -> List.contains flag args) fl

let hasDebugArgs() = argFlagIsOn ["--debug";"-d"]

let debug = true

module DevTools =
    let private installDevTools (extensionRef: obj) (forceDownload: bool): JS.Promise<string> =
        importDefault "electron-devtools-installer"
    let private REACT_DEVELOPER_TOOLS: obj = import "REACT_DEVELOPER_TOOLS" "electron-devtools-installer"
    let private REDUX_DEVTOOLS: obj = import "REDUX_DEVTOOLS" "electron-devtools-installer"

    let private installDevTool extensionRef =
        promise {
            try
                let! name = installDevTools extensionRef false
                JS.console.log ("Added extension", name)
            with err -> JS.console.log ("An error occurred adding extension:", err)
        }
        |> Promise.start

    let installAllDevTools (win: BrowserWindow) =
        installDevTool REACT_DEVELOPER_TOOLS
        installDevTool REDUX_DEVTOOLS
        win.webContents.executeJavaScript ("require('devtron').install()")
        |> ignore

    let uninstallAllDevTools (win: BrowserWindow) =
        main.Session.defaultSession.removeExtension("React Developer Tools")
        main.Session.defaultSession.removeExtension("Redux DevTools")
        win.webContents.executeJavaScript ("require('devtron').uninstall()")
        

    let connectRemoteDevViaExtension: unit -> unit = import "connectViaExtension" "remotedev"


electron.app.name <- "Issie"



// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

[<Emit("__static")>]
let staticDir() :string = jsNative

let createMainWindow () =
    let options = jsOptions<BrowserWindowOptions> <| fun options ->
        options.width <- 1200
        options.height <- 800
        options.show <- false
        options.autoHideMenuBar <- false
        options.frame <- true
        options.hasShadow <- true
        options.backgroundColor <-  "#5F9EA0"
        // fix for icons not working on linux
        // requires better solution for dist, maybe
        if Api.``process``.platform = Base.Win32 then
            options.icon <- (U2.Case2 (path.join(staticDir(), "icon.ico")))
        //elif Api.``process``.platform = Base.Darwin then
            //options.icon <- (U2.Case2 (path.join(staticDir(), "icon.icns")))   (the icns icon does not work)
        options.title <- "ISSIE"
        options.webPreferences <-
            jsOptions<WebPreferences> <| fun o ->
                o.nodeIntegration <- true
                o.enableRemoteModule <- true

    let window = electron.BrowserWindow.Create(options)

    window.onceReadyToShow <| fun _ ->
        if window.isMinimized() then window.show()
        options.backgroundColor <- "#F0F0F0"
        window.focus()
    |> ignore

    // Load the index.html of the app.    

#if DEBUG
    DevTools.installAllDevTools window
    DevTools.connectRemoteDevViaExtension()

    if debug then
        window.webContents.openDevTools()

    sprintf "http://localhost:%s" ``process``.env?ELECTRON_WEBPACK_WDS_PORT
    |> window.loadURL
    |> ignore

    ``process``.on("uncaughtException", fun err -> JS.console.error(err))
    |> ignore

    
#else
    let url =
        path.join ( __dirname,  "index.html")
        |> sprintf "file:%s" 
        |> Api.URL.Create

    Api.URL.format(url, createEmpty<Url.IFormatOptions>)
    |> window.loadURL
    |> ignore

#endif    
    
    // Emitted when the window is closed.
    window.onClosed <| fun _ ->
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow <- Option.None
    |> ignore

    // Maximize the window
    window.maximize()

    mainWindow <- Some window

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
electron.app.onReady(fun _ _ -> createMainWindow()) |> ignore

// Quit when all windows are closed.
electron.app.onWindowAllClosed <| fun _ ->
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    if Api.``process``.platform <> Base.Darwin then
        electron.app.quit()

|> ignore

electron.app.onActivate <| fun _ _ ->
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if mainWindow.IsNone then
        createMainWindow()
|> ignore

// quit programmatically from renderer
electron.ipcMain.on ("exit-the-app", fun _ -> 
    electron.app.quit()) |> ignore
