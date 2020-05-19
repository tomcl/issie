module Main

open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node.Exports

electron.app.setName "DEflow"

let args = 
    Fable.Import.Node.Globals.``process``.argv 
    |> Seq.toList
    |> List.map (fun s -> s.ToLower())

/// Returns true if any of flags are present as command line argument.    
let argFlagIsOn (flags:string list) = 
    let fl = List.map (fun (s:string) -> s.ToLower()) flags
    List.exists (fun flag -> List.contains flag args) fl

let hasDebugArgs() = argFlagIsOn ["--debug";"-d"]

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow: BrowserWindow option = Option.None

let createMainWindow () =
    let options = createEmpty<BrowserWindowOptions>
    options.width <- Some 1200.0
    options.height <- Some 800.0
    options.autoHideMenuBar <- Some true
    options.icon <- Some (Fable.Core.U2.Case2 "app/icon.ico")

    let window = electron.BrowserWindow.Create(options)

    // Load the index.html of the app.
    let opts = createEmpty<Node.Url.Url<obj>>
    opts.pathname <- Some <| path.join(Node.Globals.__dirname, "index.html")
    opts.protocol <- Some "file:"
    window.loadURL(url.format(opts))

    if hasDebugArgs() then window.webContents.openDevTools()

    // Emitted when the window is closed.
    window.on("closed", unbox(fun () ->
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow <- Option.None
    )) |> ignore

    // Maximize the window
    window.maximize()

    mainWindow <- Some window

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
electron.app.on("ready", unbox createMainWindow) |> ignore

// Quit when all windows are closed.
electron.app.on("window-all-closed", unbox(fun () ->
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    if Node.Globals.``process``.platform <> Node.Base.NodeJS.Darwin then
        electron.app.quit()
)) |> ignore

electron.app.on("activate", unbox(fun () ->
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if mainWindow.IsNone then
        createMainWindow()
)) |> ignore
