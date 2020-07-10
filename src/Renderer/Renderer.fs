module Renderer

open Browser.Types
open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Core
open Fable.Core.JsInterop
open Electron

// Keybindings.
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu:
// https://www.electronjs.org/docs/tutorial/keyboard-shortcuts#keyboard-shortcuts 
// In our case the menu is invisible.
// I also tried setting up Electron global shortcuts, but that way the
// app was also capturing combinations of key pressed in other Electron windows
// open at the same time, such as VSCode (on Linux).

let makeItem (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- accelerator
        item.accelerator <- accelerator
        item.visible <- false
        // These menu items will be invisible and only accessible via shortcuts.
        // Thanks to VisUAL2 code for lending this function to DEflow.
        item.click <- fun _ _ _ -> action()

let invisibleMenu dispatch =
    let dispatch = DiagramMessageType.KeyboardShortcutMsg >> dispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "InvisibleMenu"
        invisibleMenu.visible <- false
        invisibleMenu.submenu <-
            [| makeItem "CmdOrCtrl+S" (fun () -> dispatch DiagramMessageType.CtrlS)
               makeItem "Alt+C" (fun () -> dispatch DiagramMessageType.AltC)
               makeItem "Alt+V" (fun () -> dispatch DiagramMessageType.AltV)
               makeItem "Alt+Z" (fun () -> dispatch DiagramMessageType.AltZ)
               makeItem "Alt+Shift+Z" (fun () -> dispatch DiagramMessageType.AltShiftZ) |]
            |> U2.Case1

/// Create an invisible menu and attach keybindings to actions. 
/// Design decision: use Alt for actions that trigger equivalent to the buttons
/// on the diagram.
/// - copy diagram components: Alt+C
/// - paste diagram components: Alt+V
/// - undo diagram action: Alt+Z
/// - redo diagram action: Alt+Shift+Z
/// - save open file (not diagram action): Ctrl+S
let attachKeyShortcuts _ =
    let sub dispatch =
        let menu =
            [| U2.Case1(invisibleMenu dispatch) |]
            |> electron.remote.Menu.buildFromTemplate
        
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

Program.mkSimple init update view
|> Program.withReactBatched "app"
|> Program.withSubscription attachKeyShortcuts
|> Program.run
