module ContextMenus
open Fable.Core
open Fable.Core.JsInterop
open Microsoft.FSharp.Reflection
open ElectronAPI





/// The context menu info is a map of menu name -> list of menu items
/// menu and item names can be arbitrary strings
/// add menus as here
let menus = [
        "Menu1", ["Item1"; "Item with spaces"]
        "MenuB", ["Averylongitemwill still work"; "tiny"; "medium"]
    ]

let menuMap = Map.ofList menus




/// function used to implement main process 
/// context menu items. It should not be changed.
let makeClickableReturner
        (dispatchToRenderer: (string * string) -> unit)
        (ev: IpcMainEvent)
        ((menuType,s): string*string)
            : MenuItemConstructorOptions =
    [|
        "click", unbox (Some (fun _  ->
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
                printfn "%s" $"Error: '{menuType}' must be a valid menu name: one of {menuMap |> Map.keys |> List.ofSeq}"
                ["unknown_menu"]
            | Some cases ->
                cases
            |> List.toArray
    fun ev ->
        let template =
            cases
            |> Array.map (fun s -> makeClickableReturner dispatchToRenderer ev (menuType, s))
            |> Array.map U2.Case1
        let (menu:Menu) = mainProcess.Menu.buildFromTemplate template
        let popupOptions = Some {| window = window |}
        menu.popup (unbox popupOptions)|> ignore


    

