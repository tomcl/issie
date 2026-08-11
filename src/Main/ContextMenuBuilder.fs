/// Building and popping up the right-click menus, for the main process only.
///
/// The menu DATA - which menus exist and what is on them - stays in ContextMenus.fs, which both
/// processes compile: the renderer needs it to ask for a menu by name and to recognise the item
/// that comes back. Only the part that builds an Electron menu is here, because it needs the
/// electron module, and requiring that is what a renderer without node integration cannot do.
module ContextMenuBuilder

open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open ContextMenus

/// function used to implement main process 
/// context menu items. It should not be changed.
let makeClickableReturner
        (dispatchToRenderer: (string * string) -> unit)
        (ev: IpcMainEvent)
        ((menuType,s): string*string)
            : MenuItemConstructorOptions =
    [|
        "click", unbox (Some (fun _  ->
                    ev.preventDefault()
                    dispatchToRenderer (menuType,s)
                    ev))
        "label", unbox Some s
    |]
    |> createObj
    |> unbox

/// The menu name out of what ipcMain hands us.
///
/// Electron invokes a listener as (event, ...args), so the second parameter is the first argument
/// the sender passed - a string here - even though the binding types it as a collection. This was
/// `unbox args`, which worked by that type simply being wrong in a direction that happened not to
/// matter. Saying it in JavaScript instead is honest about the shape, and tolerates the sender
/// passing a one-element array, which is a distinction no log line can show: String(["Canvas"]) is
/// "Canvas".
[<Emit("Array.isArray($0) ? String($0[0] ?? '') : String($0 ?? '')")>]
let private menuNameOf (args: obj) : string = jsNative

/// Function implements main process context menus
/// it is called in main.fs from the renderer contextmenu event.
/// to change which menu is called where alter UpdateHelpers.chooseContextMenu
let makeMenu
    (window: BrowserWindow)
    (dispatchToRenderer)
    (args: ResizeArray<obj option>) =
    let menuType = menuNameOf args

    // menuItemsFor and isMenuName rather than Map.tryFind on the map itself: the map is built by
    // the renderer's copy of fable-library and this process has its own, so its tree nodes are not
    // instances of the classes this process's Map functions test for. Every lookup here saw the
    // root and nothing else, so every menu but one failed to open. See ContextMenus.
    let cases =
        match isMenuName menuType with
        | true -> menuItemsFor menuType
        | false ->
            let known =
                menuNames
                |> Array.filter (fun k -> k <> "")
                |> String.concat ", "
            Log.warn $"context menu '{menuType}' is not a menu name: it must be one of {known}"
            [|"unknown_menu"|]
    fun ev ->
        if menuType <> "" then
            let template =
                cases
                |> Array.map (fun s -> makeClickableReturner dispatchToRenderer ev (menuType, s))
                |> Array.map U2.Case1
            let (menu:Menu) = mainProcess.Menu.buildFromTemplate template
            let popupOptions = Some {| window = window |}
            menu.popup (unbox popupOptions)|> ignore


    

