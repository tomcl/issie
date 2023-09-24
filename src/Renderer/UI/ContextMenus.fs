module ContextMenus
open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
//
// **** DO NOT open or use renderer module code ****
//

//---------------------------------------------------------------------------------------//
//-------Menus for context-dependent right-click actions - mainly used in draw block-----//
//---------------------------------------------------------------------------------------//

(*
    NB - this file is linked into Main project as well as Renderer - so it cannot reference
    Renderer modules which are not compiled with Main.fs (that is nearly all of the renderer).
*)


/// The context menu info is a map of menu name -> list of menu items
/// menu and item names can be arbitrary strings
/// add menus as here
let contextMenus = [
        "SheetMenuBreadcrumbDev", ["Rename"; "Delete"; "Lock"; "Unlock"; "Lock Subtree"; "Unlock Subtree"]
        "SheetMenuBreadcrumb", ["Rename"; "Delete"]
        "CustomComponent", ["Go to sheet" ; "Properties"]
        "ScalingBox", ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)"; "Delete Box (DEL)"; "Copy Box (Ctrl+C)"; "Move Box (Drag any component)"]
        "Component", ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)" ; "Delete (DEL)"; "Copy (Ctrl+C)"; "Properties"]
        "Canvas", ["Zoom-in (Alt+Up) and centre" ; "Zoom-out (Alt+Down)" ; "Fit to window (Ctrl+W)" ; "Paste (Ctrl+V)"; "Reroute all wires"; "Properties"]
        "Wire", ["Unfix Wire"]
        "WaveSimHelp", ["Waveform and RAM selection"; "Waveform Operations"; "Miscellaneous"]
        "", [] // Empty string for no context menu.
    ]

let menuMap = Map.ofList contextMenus


/// function used to implement main process 
/// context menu items. It should not be changed.
let makeClickableReturner
        (dispatchToRenderer: (string * string) -> unit)
        (ev: IpcMainEvent)
        ((menuType,s): string*string)
            : MenuItemConstructorOptions =
    [|
        "click", unbox (Some (fun _  ->
                    //printfn "dispatching: %s, %s" menuType s
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

    //printf "%A" menuType
    let cases =
        Map.tryFind menuType menuMap
        |> function
            | None ->
                printfn "%s" $"Error: '{menuType}' must be a valid menu name: one of {menuMap |> Map.keys |> List.ofSeq}"
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


    

