module Renderer

open Browser.Types
open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open Fable.Core
open Fable.Core.JsInterop
open Electron
open Electron.Helpers
open DiagramMessageType



(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)



let menuSeparator =
   let sep = createEmpty<MenuItemOptions>
   sep.``type`` <- MenuItemType.Separator
   sep

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
let makeMenu (name : string) (table : MenuItemOptions list) =
   let subMenu = createEmpty<MenuItemOptions>
   subMenu.``type`` <- MenuItemType.SubMenu
   subMenu.label <- name
   subMenu.submenu <- U2.Case1 (table |> Array.ofList)
   subMenu




let fileMenu (dispatch:Dispatch<DiagramMessageType.Msg>) =
    makeMenu "File" [
        makeItem "New" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        makeItem "Print" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))

    ]

let viewMenu dispatch =
    JSHelpers.setDebugLevel()
    let devToolsKey = if Node.Api.``process``.platform = Node.Base.Darwin then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.ToggleFullScreen
        menuSeparator
        makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "CmdOrCtrl+z") (fun ev -> dispatch <| MenuAction(MenuZoom 1.25, dispatch))
        makeItem "Diagram Zoom Out" (Some "CmdOrCtrl+y") (fun ev -> dispatch <| MenuAction(MenuZoom (1. / 1.25), dispatch))
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
    let dispatch = DiagramMessageType.KeyboardShortcutMsg >> dispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- false
        invisibleMenu.submenu <-
            [| makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> dispatch DiagramMessageType.CtrlS)
               makeElmItem "Copy" "Alt+C" (fun () -> dispatch DiagramMessageType.AltC)
               makeElmItem "Paste" "Alt+V" (fun () -> dispatch DiagramMessageType.AltV)
               makeElmItem "Undo" "Alt+Z" (fun () -> dispatch DiagramMessageType.AltZ)
               makeElmItem "Redo" "Alt+Shift+Z" (fun () -> dispatch DiagramMessageType.AltShiftZ) |]
            |> U2.Case1

let attachMenusAndKeyShortcuts dispatch =
    let sub dispatch =
        let menu =
            [|
                editMenu dispatch
                fileMenu dispatch
                viewMenu dispatch
            |]
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some false
        electron.remote.app.applicationMenu <- Some menu

    Cmd.ofSub sub    

// This setup is useful to add other pages, in case they are needed.

type Model = DiagramModelType.Model

type Messages = DiagramMessageType.Msg

// -- Init Model

let init() = DiagramMainView.init()

// -- Create View

let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model = DiagramMainView.update msg model

printfn "Starting renderer..."

Program.mkSimple init update view
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.run
