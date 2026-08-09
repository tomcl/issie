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


/// Item offered on a schematic component while a wave simulation is running, which adds waveforms
/// from that component to the viewer. Which menu the renderer asks for decides whether it appears -
/// this file cannot see the model. See UpdateHelpers.getContextMenu.
let addWavesItem = "Add waveforms to viewer"

/// Items offered on an instance of a library component, which take the place of "Go to sheet".
/// A library component is meant to be one thing rather than a sheet with innards, so its sheet is
/// normally unreachable; these open it read-only, and put it away again. Which of the two is
/// offered depends on whether that sheet is already being viewed, so as with the waveform item
/// above the renderer decides by asking for a different menu.
let viewLibraryItem = "View library component"
let hideLibraryItem = "Hide library component"

let private componentItems =
    ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)" ; "Delete (DEL)"; "Copy (Ctrl+C)"; "Properties"]

let private customComponentItems =
    ["Go to sheet" ; "Properties" ; "Move ports" ; "Resize symbol"]

/// As customComponentItems, for an instance of a library component: the instance itself sits on an
/// ordinary sheet and can be moved and resized like any other, so only the first item differs.
let private libraryInstanceItems (openItem: string) =
    [openItem ; "Properties" ; "Move ports" ; "Resize symbol"]

/// The context menu info is a map of menu name -> list of menu items
/// menu and item names can be arbitrary strings
/// add menus as here
let contextMenus = [
        "SheetMenuBreadcrumbDev", ["Rename"; "Duplicate"; "Delete"; "Set as top"; "Save as library component"; "Write design as Verilog"; "Lock"; "Unlock"; "Lock Subtree"; "Unlock Subtree"]
        "SheetMenuBreadcrumb", ["Rename"; "Duplicate"; "Delete"; "Set as top"; "Save as library component"; "Write design as Verilog"]
        "ProjectPath", ["Copy path"; "Open directory"]
        "CustomComponent", customComponentItems
        "CustomComponentWaveSim", customComponentItems @ [addWavesItem]
        "LibraryInstance", libraryInstanceItems viewLibraryItem
        "LibraryInstanceWaveSim", libraryInstanceItems viewLibraryItem @ [addWavesItem]
        "LibraryInstanceOpen", libraryInstanceItems hideLibraryItem
        "LibraryInstanceOpenWaveSim", libraryInstanceItems hideLibraryItem @ [addWavesItem]
        "ScalingBox", ["Rotate Clockwise (Ctrl+Right)"; "Rotate AntiClockwise (Ctrl+Left)" ; "Flip Vertical (Ctrl+Up)"; "Flip Horizontal (Ctrl+Down)"; "Delete Box (DEL)"; "Copy Box (Ctrl+C)"; "Move Box (Drag any component)"]
        "Component", componentItems
        "ComponentWaveSim", componentItems @ [addWavesItem]
        // These labels spell their own keys, so they have to be kept in step with KeyTypes by
        // hand - this file cannot see it, being compiled into the main process as well.
        "Canvas", ["Zoom-in (Ctrl+plus) and centre" ; "Zoom-out (Ctrl+minus)" ; "Fit to window (Ctrl+0)" ; "Paste (Ctrl+V)"; "Reroute all wires"; "Properties"]
        "Wire", ["Unfix Wire"]
        // Menus offered while a library component's sheet is being viewed. Every item that would
        // change the sheet is absent rather than disabled, because the sheet is held at what it
        // loaded with and the change would be silently undone. A wire and the scaling box have
        // nothing left at all, so they are given no menu. Fit to window stays: it moves the whole
        // circuit rather than editing it, and is allowed for that reason.
        "ComponentReadOnly", ["Properties"]
        "LibraryInstanceReadOnly", [viewLibraryItem; "Properties"]
        "LibraryInstanceOpenReadOnly", [hideLibraryItem; "Properties"]
        "CanvasReadOnly", ["Zoom-in (Ctrl+plus) and centre" ; "Zoom-out (Ctrl+minus)" ; "Fit to window (Ctrl+0)" ; "Properties"]
        "SheetMenuBreadcrumbLibrary", [hideLibraryItem]
        // Each of these is the name of a case in UIPopups.viewWaveInfoPopup, which is given the
        // item clicked on verbatim. A name here that has no case there reaches its catch-all, so
        // the two lists must be changed together. They cannot be one list: this file is compiled
        // into the main process, which has none of the renderer, and UIPopups is compiled long
        // before this file in the renderer itself.
        "WaveSimHelp", ["Waveform and RAM selection"; "Viewing Waveforms"; "Miscellaneous"]
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


    

