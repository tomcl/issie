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

/// Function implements main process context menus
/// it is called in main.fs from the renderer contextmenu event.
/// to change which menu is called where alter UpdateHelpers.chooseContextMenu
let makeMenu
    (window: BrowserWindow)
    (dispatchToRenderer)
    (args: ResizeArray<obj option>) =
    let menuType:string = unbox args

    let cases =
        Map.tryFind menuType menuMap
        |> function
            | None ->
                Log.warn $"'{menuType}' is not a menu name: it must be one of {menuMap |> Map.keys |> Seq.toList}"
                ["unknown_menu"]
            | Some cases ->
                cases
            |> List.toArray
    fun ev ->
        if menuType <> "" then
            let template =
                cases
                |> Array.map (fun s -> makeClickableReturner dispatchToRenderer ev (menuType, s))
                |> Array.map U2.Case1
            let (menu:Menu) = mainProcess.Menu.buildFromTemplate template
            let popupOptions = Some {| window = window |}
            menu.popup (unbox popupOptions)|> ignore


    

