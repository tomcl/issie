(*
Top-level renderer that initialises the app and runs the elmish loop
The electron built-in menus, and key presses,have actions which are
are implemented here using elmish subscriptions
*)

module Renderer

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Fable.Core
open Fable.Core.JsInterop
open ElectronAPI
open ModelType
open Fable.SimpleJson
open JSHelpers
open Sheet.SheetInterface
open DrawModelType

importSideEffects "./scss/main.css"

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
            x <- modMap[x]
            i <- i + 1

    let count = 1000000
    let start = TimeHelpers.getTimeMs()
    let result = iterMap count
    let interval = TimeHelpers.getTimeMs() - start
    printfn "%d iterations of iterMap took %.1fms" count interval



(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)

let menuSeparator =
   let sep = createEmpty<MenuItemConstructorOptions>
   sep.``type`` <- Some MenuItemType.Separator
   sep

// Set up window close interlock using IPC from/to main process
let attachExitHandler dispatch =
    // set up callback called when attempt is made to close main window
    renderer.ipcRenderer.on ("closingWindow", (fun (event: Event)->
        // send a message which will process the request to exit
        dispatch <| MenuAction(MenuExit,dispatch)
        )) |> ignore
(*
// Set up window close interlock using IPC from/to main process
let attachGetAppHandler dispatch =
    // set up callback called when attempt is made to close main window
    renderer.ipcRenderer.on ("get-user-data", (fun (event: Event)->
        // send a message which will process the request to exit
        dispatch <| SetUserAppDir (unbox event. : string)
        )) |> ignore*)

let getUserAppDir () : string =
    unbox <| renderer.ipcRenderer.sendSync("get-user-data",None)

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let item = createEmpty<Electron.MenuItemConstructorOptions>
   item.label <- Some label
   item.accelerator <- accelerator
   item.click <- Some (fun _ _ keyEvent -> iAction keyEvent)
   item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- Some role
   item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- Some role
   item.visible <- Some cond
   item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- Some cond
   item


let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemConstructorOptions> <| fun item ->
        item.label <- Some label
        item.accelerator <- Some accelerator
        item.click <- Some (fun _ _ _ -> action())


/// Make a new menu from a a list of menu items
let makeMenu (topLevel: bool) (name : string) (table : MenuItemConstructorOptions list) =
   let subMenu = createEmpty<MenuItemConstructorOptions>
   subMenu.``type`` <- Some (if topLevel then MenuItemType.Normal else MenuItemType.Submenu)
   subMenu.label <-Some name
   subMenu.submenu <- Some (U2.Case1 (table |> ResizeArray))
   subMenu

let displayPerformance n m = TimeHelpers.checkPerformance n m JSHelpers.startTimer JSHelpers.stopAndLogTimer







let fileMenu (dispatch) =
    makeMenu false "Sheet" [
        makeItem "New Sheet" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save Sheet" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        //makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))
        makeItem "Write design as Verilog" None (fun ev -> dispatch (MenuAction(MenuVerilogOutput,dispatch)))
        makeItem "Exit Issie" None (fun ev -> dispatch (MenuAction(MenuExit,dispatch)))
        makeItem ("About Issie " + Version.VersionString) None (fun ev -> PopupView.viewInfoPopup dispatch)
        makeCondRoleItem (JSHelpers.debugLevel <> 0 && not isMac) "Hard Restart app" None MenuItemRole.ForceReload
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace all" None (fun _ ->
            JSHelpers.debugTraceUI <- Set.ofList ["update";"view"])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace off" None (fun _ ->
            JSHelpers.debugTraceUI <- Set.ofList [])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Run performance check" None (fun _ ->
            testMaps()
            displayPerformance 100 4000000)
        makeCondItem (JSHelpers.debugLevel <> 0) "Force Exception" None  (fun ev -> failwithf "User exception from menus")
        makeCondItem (JSHelpers.debugLevel <> 0) "Call update" None  (fun ev -> failwithf "User exception from menus")
     ]


let viewMenu dispatch =
    let maindispatch = dispatch
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let dispatch = SheetT.KeyPress >> sheetDispatch
    let wireTypeDispatch = SheetT.WireType >> sheetDispatch
    let interfaceDispatch = SheetT.IssieInterface >> sheetDispatch
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)
    
    
    
    let symbolDispatch msg = busWireDispatch (BusWireT.Msg.Symbol msg)

    let devToolsKey = if isMac then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu false "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.Togglefullscreen
        menuSeparator
        makeRoleItem "Zoom  In" (Some "CmdOrCtrl+Shift+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom  Out" (Some "CmdOrCtrl+Shift+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "Shift+Plus") (fun ev -> dispatch SheetT.KeyboardMsg.ZoomIn)
        makeItem "Diagram Zoom Out" (Some "Shift+-") (fun ev -> dispatch SheetT.KeyboardMsg.ZoomOut)
        makeItem "Diagram Zoom to Fit" (Some "CmdOrCtrl+W") (fun ev -> dispatch SheetT.KeyboardMsg.CtrlW)
        menuSeparator
        makeItem "Toggle Grid" None (fun ev -> sheetDispatch SheetT.Msg.ToggleGrid)
        makeMenu false "Theme" [
            makeItem "Grayscale" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.White
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.White)
            )
            makeItem "Light" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.Light
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.Light)
            )
            makeItem "Colourful" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.Colourful
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.Colourful)
            )
        ]
        makeItem "Toggle Wire Arrows" None (fun ev -> busWireDispatch (BusWireT.Msg.ToggleArrowDisplay))
        makeMenu false "Wire Type" [
            makeItem "Jump wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Jump)
            makeItem "Radiussed wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Radiussed)
            makeItem "Modern wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Modern)
        ]
        menuSeparator
        makeItem "Show/Hide Build Tab" None (fun ev -> maindispatch (ChangeBuildTabVisibility))
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ ->
            renderer.ipcRenderer.send("toggle-dev-tools", [||]) |> ignore)
    ]


// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch' =
    let sheetDispatch sMsg = dispatch' (Sheet sMsg)
    let dispatch = SheetT.KeyPress >> sheetDispatch
    let rotateDispatch = SheetT.Rotate >> sheetDispatch

    jsOptions<MenuItemConstructorOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- Some MenuItemType.Submenu
        invisibleMenu.label <- Some "Edit"
        invisibleMenu.visible <- Some true
        invisibleMenu.submenu <-
            [| // makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> ())
               makeElmItem "Copy" "CmdOrCtrl+C" (fun () -> dispatch SheetT.KeyboardMsg.CtrlC)
               makeElmItem "Paste" "CmdOrCtrl+V" (fun () -> dispatch SheetT.KeyboardMsg.CtrlV)
               menuSeparator
               makeElmItem "Rotate Anticlockwise" "CmdOrCtrl+Left" (fun () -> rotateDispatch SymbolT.RotateAntiClockwise)
               makeElmItem "Rotate Clockwise" "CmdOrCtrl+Right" (fun () -> rotateDispatch SymbolT.RotateClockwise)
               makeElmItem "Flip Vertically" "CmdOrCtrl+Up" (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipVertical)
               makeElmItem "Flip Horizontally" "CmdOrCtrl+Down" (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipHorizontal)
               makeItem "Move Component Ports" None (fun _ -> 
                    dispatch' <| ShowStaticInfoPopup("How to move component ports", SymbolUpdatePortHelpers.moveCustomPortsPopup(), dispatch'))
               menuSeparator
               makeElmItem "Align" "CmdOrCtrl+Shift+A"  (fun ev -> sheetDispatch <| SheetT.Arrangement SheetT.AlignSymbols)
               makeElmItem "Distribute" "CmdOrCtrl+Shift+D" (fun ev-> sheetDispatch <| SheetT.Arrangement SheetT.DistributeSymbols)
               makeElmItem "Rotate Label Clockwise" "CmdOrCtrl+Shift+Right" (fun ev-> sheetDispatch <| SheetT.RotateLabels)
               menuSeparator
               makeElmItem "Select All" "CmdOrCtrl+A" (fun () -> dispatch SheetT.KeyboardMsg.CtrlA)
               makeElmItem "Delete"  (if isMac then "Backspace" else "delete") (fun () -> dispatch SheetT.KeyboardMsg.DEL)
               makeElmItem "Undo" "CmdOrCtrl+Z" (fun () -> dispatch SheetT.KeyboardMsg.CtrlZ)
               makeElmItem "Redo" "CmdOrCtrl+Y" (fun () -> dispatch SheetT.KeyboardMsg.CtrlY)
               makeElmItem "Cancel" "ESC" (fun () -> dispatch SheetT.KeyboardMsg.ESC)
            |]
            |> ResizeArray
            |> U2.Case1
            |> Some

let attachMenusAndKeyShortcuts dispatch =
    //setupExitInterlock dispatch
    let sub dispatch =
        let menu:Menu =
            [|

                fileMenu dispatch

                editMenu dispatch

                viewMenu dispatch
            |]
            |> Array.map U2.Case1
            |> electronRemote.Menu.buildFromTemplate   //Help? How do we call buildfromtemplate
        menu.items[0].visible <- true
        dispatch <| Msg.ExecFuncInMessage((fun _ _ ->
            electronRemote.app.applicationMenu <- Some menu), dispatch)
        attachExitHandler dispatch
        let userAppDir = getUserAppDir()
        dispatch <| ReadUserData userAppDir


    Cmd.ofSub sub

// This setup is useful to add other pages, in case they are needed.

type Model = ModelType.Model

type Messages = ModelType.Msg

// -- Init Model

let init() =
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none


// -- Create View
let addDebug dispatch (msg:Msg) =
    let str = UpdateHelpers.getMessageTraceString msg
    if str <> "" then printfn ">>Dispatch %s" str else ()
    dispatch msg

let view model dispatch = DiagramMainView.displayView model (addDebug dispatch)

// -- Update Model

let update msg model = Update.update msg model

printfn "Starting renderer..."

let view' model dispatch =
    let start = TimeHelpers.getTimeMs()
    view model dispatch
    |> TimeHelpers.instrumentInterval "View" start

let mutable firstPress = true

///Used to listen for pressing down of Ctrl for selection toggle
let keyPressListener initial =
    let subDown dispatch =
        Browser.Dom.document.addEventListener("keydown", fun e ->
                                                let ke: KeyboardEvent = downcast e
                                                if (jsToBool ke.ctrlKey || jsToBool ke.metaKey) && firstPress then
                                                    firstPress <- false
                                                    dispatch <| Sheet(SheetT.PortMovementStart)
                                                else
                                                    ())
    let subUp dispatch =
        Browser.Dom.document.addEventListener("keyup", fun e ->
                                                    firstPress <- true
                                                    dispatch <| Sheet(SheetT.PortMovementEnd))
    Cmd.batch [Cmd.ofSub subDown; Cmd.ofSub subUp]



Program.mkProgram init update view'
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.withSubscription keyPressListener
|> Program.run