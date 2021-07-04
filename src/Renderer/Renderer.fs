module Renderer

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Electron
open Electron.Helpers
open ModelType

open Fable.SimpleJson
open Fable.React
open Fable.React.Props


let isMac = Node.Api.``process``.platform = Node.Base.Darwin

let testMaps() =
    let modMap = 
        [0..1000] 
        |> List.map (fun n -> n, (n*256+1) % 1001)
        |> Map.ofList


    let iterMap count =
        let mutable x: int = 1
        let mutable i:int = 0
        while i < count do
            x <- modMap.[x]
            i <- i + 1
        
    let count = 1000000
    let start = Helpers.getTimeMs()
    let result = iterMap count
    let interval = Helpers.getTimeMs() - start
    printfn "%d iterations of iterMap took %.1fms" count interval


(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)

let menuSeparator =
   let sep = createEmpty<MenuItemOptions>
   sep.``type`` <- MenuItemType.Separator
   sep

// Set up window close interlock using IPC from/to main process
let attachExitHandler dispatch =
    // set up callback called when attempt is made to close main window
    electron.ipcRenderer.on ("closingWindow", (fun (event: Event)->
        printfn "dispatching ShowExitDialog"
        // send a message which will process the request to exit
        dispatch <| ShowExitDialog
        )) |> ignore
    

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let handlerCaster f = System.Action<MenuItem, BrowserWindow, KeyboardEvent> f 
   let item = createEmpty<MenuItemOptions>
   item.label <- label
   match accelerator with | Some a -> item.accelerator <- a | _ -> ()
   item.click <- handlerCaster (fun _ _ keyEvent-> iAction keyEvent)
   item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item.visible <- cond
   item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- cond
   item


let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- label
        item.accelerator <- accelerator
        item.click <- fun _ _ _ -> action()


/// Make a new menu from a a list of menu items
let makeMenu (topLevel: bool) (name : string) (table : MenuItemOptions list) =
   let subMenu = createEmpty<MenuItemOptions>
   subMenu.``type`` <- if topLevel then MenuItemType.Normal else MenuItemType.SubMenu
   subMenu.label <- name
   subMenu.submenu <- U2.Case1 (table |> Array.ofList)
   subMenu    

let displayPerformance n m = Helpers.checkPerformance n m JSHelpers.startTimer JSHelpers.stopAndLogTimer







let fileMenu (dispatch) =
    makeMenu false "Sheet" [
        makeItem "New Sheet" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save Sheet" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))
        makeItem "Write design as Verilog" None (fun ev -> dispatch (MenuAction(MenuVerilogOutput,dispatch)))
        makeItem "Exit Issie" None (fun ev -> dispatch Msg.ShowExitDialog)
        makeItem ("About Issie " + Version.VersionString) None (fun ev -> PopupView.viewInfoPopup dispatch)
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Restart app" None (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.reload())
        makeCondRoleItem (JSHelpers.debugLevel <> 0 && not isMac) "Hard Restart app" None MenuItemRole.ForceReload
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace all" None (fun _ -> 
            JSHelpers.debugTraceUI <- Set.ofList ["update";"view"])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace off" None (fun _ -> 
            JSHelpers.debugTraceUI <- Set.ofList [])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Run performance check" None (fun _ -> 
            testMaps()
            
            displayPerformance 100 4000000)
     ]


let viewMenu dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let dispatch = Sheet.KeyPress >> sheetDispatch
    
    let devToolsKey = if isMac then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu false "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.ToggleFullScreen
        menuSeparator
        makeRoleItem "Zoom  In" (Some "CmdOrCtrl+Shift+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom  Out" (Some "CmdOrCtrl+Shift+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "Shift+Plus") (fun ev -> dispatch Sheet.KeyboardMsg.ZoomIn)
        makeItem "Diagram Zoom Out" (Some "Shift+-") (fun ev -> dispatch Sheet.KeyboardMsg.ZoomOut)
        makeItem "Diagram Zoom to Fit" (Some "CmdOrCtrl+W") (fun ev -> dispatch Sheet.KeyboardMsg.CtrlW)
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.toggleDevTools())
    ]


// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let dispatch = Sheet.KeyPress >> sheetDispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- true
        invisibleMenu.submenu <-
            [| // makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> ())
               makeElmItem "Copy" "CmdOrCtrl+C" (fun () -> dispatch Sheet.KeyboardMsg.CtrlC)
               makeElmItem "Paste" "CmdOrCtrl+V" (fun () -> dispatch Sheet.KeyboardMsg.CtrlV)
               makeElmItem "Select All" "CmdOrCtrl+A" (fun () -> dispatch Sheet.KeyboardMsg.CtrlA)
               makeElmItem "Delete"  (if isMac then "Backspace" else "delete") (fun () -> dispatch Sheet.KeyboardMsg.DEL)
               makeElmItem "Undo" "CmdOrCtrl+Z" (fun () -> dispatch Sheet.KeyboardMsg.CtrlZ)
               makeElmItem "Redo" "CmdOrCtrl+Y" (fun () -> dispatch Sheet.KeyboardMsg.CtrlY)
               makeElmItem "Cancel" "ESC" (fun () -> dispatch Sheet.KeyboardMsg.ESC)|]
            |> U2.Case1

let attachMenusAndKeyShortcuts dispatch =
    //setupExitInterlock dispatch
    let sub dispatch =
        let menu = 
            [|

                fileMenu dispatch

                editMenu dispatch 

                viewMenu dispatch
            |]          
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some true
        electron.remote.app.applicationMenu <- Some menu
        attachExitHandler dispatch

    Cmd.ofSub sub    

// This setup is useful to add other pages, in case they are needed.

type Model = ModelType.Model

type Messages = ModelType.Msg

// -- Init Model

let init() = 
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none


// -- Create View

let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model = Update.update msg model

printfn "Starting renderer..."

let view' model dispatch =
    if JSHelpers.debugLevel > 0 then
        printfn "Rendering with %s cursor" (model.Sheet.CursorType.Text())
    let start = Helpers.getTimeMs()
    view model dispatch
    |> Helpers.instrumentInterval "View" start
   
let mutable firstPress = true

///Used to listen for pressing down of Ctrl for selection toggle
let keyPressListener initial = 
    let subDown dispatch =
        Browser.Dom.document.addEventListener("keydown", fun e ->
                                                let ke: KeyboardEvent = downcast e
                                                if (ke.ctrlKey || ke.metaKey) && firstPress then 
                                                    firstPress <- false 
                                                    dispatch <| Sheet(Sheet.ToggleSelectionOpen)
                                                else 
                                                    ())
    let subUp dispatch = 
        Browser.Dom.document.addEventListener("keyup", fun e -> 
                                                    firstPress <- true
                                                    dispatch <| Sheet(Sheet.ToggleSelectionClose))
    Cmd.batch [Cmd.ofSub subDown; Cmd.ofSub subUp] 



Program.mkProgram init update view'
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.withSubscription keyPressListener
|> Program.run
