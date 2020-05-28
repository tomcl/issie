module Renderer

open Elmish
open Elmish.HMR
open Elmish.React
open Elmish.Debug
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron

importSideEffects "./../../app/scss/main.scss" 

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
    let handlerCaster f = System.Func<MenuItem, BrowserWindow, unit> f |> Some
    let item = createEmpty<MenuItemOptions>
    item.accelerator <- Some accelerator
    // These menu items will be invisible and only accessible via shortcuts.
    // Thanks to VisUAL2 code for lending this function to DEflow.
    item.click <- handlerCaster (fun _ _ -> action())
    item

let invisibleMenu dispatch =
    let invisibleMenu = createEmpty<MenuItemOptions>
    let dispatch = DiagramMessageType.KeyboardShortcutMsg >> dispatch
    invisibleMenu.submenu <-
        [ makeItem "CmdOrCtrl+S" (fun () -> dispatch DiagramMessageType.CtrlS)
          makeItem "Alt+C" (fun () -> dispatch DiagramMessageType.AltC)
          makeItem "Alt+V" (fun () -> dispatch DiagramMessageType.AltV)
          makeItem "Alt+Z" (fun () -> dispatch DiagramMessageType.AltZ)
          makeItem "Alt+Shift+Z" (fun () -> dispatch DiagramMessageType.AltShiftZ) ]
        |> ResizeArray<MenuItemOptions> |> U2.Case2 |> Some
    invisibleMenu

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
        let template = ResizeArray<MenuItemOptions> [invisibleMenu dispatch]
        template
        |> electron.remote.Menu.buildFromTemplate
        |> electron.remote.Menu.setApplicationMenu
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
|> Program.withReact "electron-app"
|> Program.withSubscription attachKeyShortcuts
|> Program.run
