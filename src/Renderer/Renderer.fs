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
open Optics
open Optics.Operators
open TestParser
open ContextMenus




importSideEffects "./scss/main.css"

let isMac = Node.Api.``process``.platform = Node.Base.Darwin





// -- Init Model

let init() =
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none

let getUserAppDir () : string =
    unbox <| renderer.ipcRenderer.sendSync("get-user-data",None)

let softInitialise model dispatch =
    //Playground.Memory.modelCopy <- None
    //dispatch (UpdateModel(fun _ -> fst (init())))
    //let userAppDir = getUserAppDir()
    //dispatch <| ReadUserData userAppDir
    //printfn $"INIT: rpsc={Sheet.recentProgrammaticScrollPos.Length}, dm= {MemoryEditorView.dynamicMem.Data.Count}"
    //Sheet.recentProgrammaticScrollPos <- []
    //MemoryEditorView.dynamicMem <- {MemoryEditorView.dynamicMem with Data = Map.empty}
    ()


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
    renderer.ipcRenderer.on ("windowLostFocus", (fun (event: Event)->
        // send a message which will process the request to exit
        dispatch <| MenuAction(MenuLostFocus,dispatch)
        )) |> ignore
(*
// Set up window close interlock using IPC from/to main process
let attachGetAppHandler dispatch =
    // set up callback called when attempt is made to close main window
    renderer.ipcRenderer.on ("get-user-data", (fun (event: Event)->
        // send a message which will process the request to exit
        dispatch <| SetUserAppDir (unbox event. : string)
        )) |> ignore*)



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

/// make  a conditional menu item from a condition,
/// name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- Some cond
   item

/// A menu item which is visible only if in debug mode
/// (run dev or command line -D on binaries) and on windows.
let makeDebugItem label accelerator option =
    makeCondItem (JSHelpers.debugLevel <> 0) label accelerator option

/// A menu item which is visible only if in debug mode
/// (run dev or command line -D on binaries) and on windows.
let makeWinDebugItem label accelerator option =
    makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) label accelerator option

/// Make 
let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemConstructorOptions> <| fun item ->
        item.label <- Some label
        item.accelerator <- Some accelerator
        item.click <- Some (fun _ _ _ -> action())


/// Make a new menu from a list of menu items
let makeMenuGen (visible: bool) (topLevel: bool) (name : string) (table : MenuItemConstructorOptions list) =
   let subMenu = createEmpty<MenuItemConstructorOptions>
   subMenu.``type`` <- Some (if topLevel then MenuItemType.Normal else MenuItemType.Submenu)
   subMenu.label <-Some name
   subMenu.submenu <- Some (U2.Case1 (table |> ResizeArray))
   subMenu.visible <-  Some visible
   subMenu


/// Make a new menu from a list of menu items
let makeMenu (topLevel: bool) (name : string) (table : MenuItemConstructorOptions list) =
    makeMenuGen true topLevel name table

open JSHelpers

let reSeparateWires dispatch =
    dispatch <| UpdateModel (fun model ->
        model
        |> Optic.map (sheet_ >->  SheetT.wire_) (BusWireSeparate.reSeparateWiresFrom model.Sheet.SelectedComponents)
    )

let reRouteWires dispatch =
    dispatch <| UpdateModel (fun model ->
        model
        |> Optic.map (sheet_ >->  SheetT.wire_) (BusWireSeparate.reRouteWiresFrom model.Sheet.SelectedComponents)
    )

//-----------------------------------------------------------------------------------------------------------//
//-----------------------------------------------FILE MENU---------------------------------------------------//
//-----------------------------------------------------------------------------------------------------------//

let fileMenu (dispatch) =
    makeMenu false "Sheet" [
        makeItem "New Sheet" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save Sheet" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        makeItem "Save Project in New Format" None (fun ev -> dispatch (MenuAction(MenuSaveProjectInNewFormat,dispatch)))
        //makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))
        makeItem "Write design as Verilog" None (fun ev -> dispatch (MenuAction(MenuVerilogOutput,dispatch)))
        makeItem "Exit Issie" None (fun ev -> dispatch (MenuAction(MenuExit,dispatch)))
        makeItem ("About Issie " + Version.VersionString) None (fun ev -> UIPopups.viewInfoPopup dispatch)
        makeCondRoleItem (debugLevel <> 0 && not isMac) "Hard Restart app" None MenuItemRole.ForceReload
        makeWinDebugItem "Trace all" None (fun _ ->
            debugTraceUI <- Set.ofList ["update";"view"])
        makeWinDebugItem "Trace View function" None (fun _ ->
            debugTraceUI <- Set.ofList ["view"])
        makeWinDebugItem "Trace Update function" None (fun _ ->
            debugTraceUI <- Set.ofList ["update"])
        makeWinDebugItem "Trace Mouse Messages" None (fun _ ->
            debugTraceUI <- debugTraceUI + Set.ofList ["mouse"])
        makeWinDebugItem "Trace off" None (fun _ ->
            debugTraceUI <- Set.ofList [])
        makeMenuGen (debugLevel > 0) false "Play" [
            makeDebugItem "HEAP" None
                (fun _ ->
                    printfn $"\n******* USED heap size:{float(usedHeap()) / 1000000.} MB MAX: {float(maxHeap()) / 1000000.} MB********\n")
            makeDebugItem "INIT" None
                (fun _ ->
                    dispatch <| ExecFuncInMessage(softInitialise, dispatch))
            makeDebugItem "SCREEN RESET" None
                (fun _ ->
                    printfn $"USED heap\n size before screen reset:{float(usedHeap()) / 1000000.} MB\n"
                    dispatch (SetTopMenu TransientClosed))
            makeDebugItem "Set Scroll" None
                (fun _ -> SheetDisplay.writeCanvasScroll {X=1000.;Y=1000.} |> ignore)
            makeDebugItem "Trace all times" None
                (fun _ -> TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint( 0.1, 0.1)
                          if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update";"view"])
            makeDebugItem "Trace short, medium & long times" None
                (fun _ -> TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint( 1.5, 1.5)
                          if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update";"view"])
            makeDebugItem "Trace medium & long times" None
                (fun _ -> TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(3.,3.)
                          if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update";"view"])
            makeDebugItem "Trace long times" None
                (fun _ -> TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(20.,20.)
                          if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update";"view"])
            makeDebugItem "Print Misc Performance Info" None
                (fun _ ->
                    Playground.Memory.printListeners()
                    Playground.Memory.printProcessMemory()
                    dispatch SaveModel)
            makeDebugItem "Test Fonts" None
                (fun _ -> Playground.TestFonts.makeTextPopup dispatch)
            makeWinDebugItem  "Run performance check" None
                (fun _ -> Playground.MiscTests.testMaps())
            makeWinDebugItem  "Print names of static asset files" None 
                (fun _ -> Playground.MiscTests.testAssets())
            makeWinDebugItem  "Test Breadcrumbs" None 
                (fun _ -> dispatch <| Msg.ExecFuncInMessage(Playground.Breadcrumbs.testBreadcrumbs,dispatch))
            makeWinDebugItem  "Test All Hierarchies Breadcrumbs" None 
                (fun _ -> dispatch <| Msg.ExecFuncInMessage(Playground.Breadcrumbs.testAllHierarchiesBreadcrumbs,dispatch))

            makeDebugItem "Force Exception" None
                (fun ev -> failwithf "User exception from menus")

            makeDebugItem "Web worker performance test" None
                (fun _ -> Playground.WebWorker.testWorkers Playground.WebWorker.Constants.workerTestConfig)


        ]

        makeMenu false "Verilog" [
            makeDebugItem "Run Verilog tests" None  (fun _ ->
                runCompilerTests ()
                printfn "Compiler tests done")
            makeDebugItem "Run Verilog performance tests" None  (fun _ ->
                runPerformanceTests ()
                printfn "Performance tests done")
            makeDebugItem "Generate driver modules" None  (fun _ ->
                genDriverFiles ())
            makeDebugItem "Icarus compile testcases" None  (fun _ ->
                icarusCompileTestCases ())
            makeDebugItem "Icarus run testcases" None  (fun _ ->
                icarusRunTestCases ())
        ]
     ]

//-----------------------------------------------------------------------------------------------------------//
//-----------------------------------------------VIEW MENU---------------------------------------------------//
//-----------------------------------------------------------------------------------------------------------//


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
        makeRoleItem "Zoom In" (Some "CmdOrCtrl+Shift+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom Out" (Some "CmdOrCtrl+Shift+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "Alt+Up") (fun ev -> dispatch SheetT.KeyboardMsg.ZoomIn)
        makeItem "Diagram Zoom Out" (Some "Alt+Down") (fun ev -> dispatch SheetT.KeyboardMsg.ZoomOut)
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
        makeItem "Toggle app memory display" None (fun ev ->
                    JSHelpers.loggingMemory <- not JSHelpers.loggingMemory
                    let state = if loggingMemory then "on" else "off"
                    printfn $"Memory display is now {state}.")
        //makeItem "Benchmark" (Some "Ctrl+Shift+B") (fun ev -> maindispatch Benchmark) // does this work?
        makeItem "Show/Hide Build Tab" None (fun ev -> maindispatch (ChangeBuildTabVisibility))
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ ->
            renderer.ipcRenderer.send("toggle-dev-tools", [||]) |> ignore)
    ]

//-----------------------------------------------------------------------------------------------------------//
//-----------------------------------------------EDIT MENU---------------------------------------------------//
//-----------------------------------------------------------------------------------------------------------//

// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch' =
    let sheetDispatch sMsg = dispatch' (Sheet sMsg)
    let dispatch = SheetT.KeyPress >> sheetDispatch
    let rotateDispatch = SheetT.Rotate >> sheetDispatch
    let busWireDispatch (bMsg: BusWireT.Msg) = sheetDispatch (SheetT.Msg.Wire bMsg)

    jsOptions<MenuItemConstructorOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- Some MenuItemType.Submenu
        invisibleMenu.label <- Some "Edit"
        invisibleMenu.visible <- Some true
        invisibleMenu.submenu <-
            [| // makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> ())
               makeElmItem "Copy" "CmdOrCtrl+C" (fun () -> dispatch SheetT.KeyboardMsg.CtrlC)
               makeElmItem "Paste" "CmdOrCtrl+V" (fun () -> dispatch SheetT.KeyboardMsg.CtrlV)
               menuSeparator
               makeElmItem "Rotate Anticlockwise" "CmdOrCtrl+Left" (fun () -> rotateDispatch CommonTypes.Degree270)
               makeElmItem "Rotate Clockwise" "CmdOrCtrl+Right" (fun () -> rotateDispatch CommonTypes.Degree90)
               makeElmItem "Flip Vertically" "CmdOrCtrl+Up" (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipVertical)
               makeElmItem "Flip Horizontally" "CmdOrCtrl+Down" (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipHorizontal)
               makeItem "Move Component Ports" None (fun _ -> 
                    dispatch' <| ShowStaticInfoPopup("How to move component ports", SymbolPortHelpers.moveCustomPortsPopup(), dispatch'))
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
               menuSeparator
               makeItem "Separate Wires from Selected Components" None (fun _ -> reSeparateWires dispatch')
               makeItem "Reroute Wires from Selected Components" None  (fun _ -> reRouteWires dispatch')
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




// -- Create View
let addDebug dispatch (msg:Msg) =
    let str = UpdateHelpers.getMessageTraceString msg
    //if str <> "" then printfn ">>Dispatch %s" str else ()
    dispatch msg

let view model dispatch = DiagramMainView.displayView model (addDebug dispatch)

// -- Update Model

let update msg model = Update.update msg model

//printfn "Starting renderer..."

let view' model dispatch =
    let start = TimeHelpers.getTimeMs()
    view model dispatch
    |> (fun view ->
        if Set.contains "view" JSHelpers.debugTraceUI then
            TimeHelpers.instrumentInterval ">>>View" start view
        else
            view)

let mutable firstPress = true

///Used to listen for pressing down of Ctrl for selection toggle
let keyPressListener initial =
    let subDown dispatch =
        Browser.Dom.document.addEventListener("keydown", fun e ->
                                                let ke: KeyboardEvent = downcast e
                                                if (jsToBool ke.ctrlKey || jsToBool ke.metaKey) && firstPress then
                                                    firstPress <- false
                                                    //printf "Ctrl-Meta Key down (old method)"
                                                    dispatch <| Sheet(SheetT.PortMovementStart)
                                                else
                                                    ())
    let subUp dispatch =
        Browser.Dom.document.addEventListener("keyup", fun e ->
                                                    firstPress <- true
                                                    //printf "Any Key up (old method)"
                                                    dispatch <| Sheet(SheetT.PortMovementEnd))
    /// unfinished code
    /// add hook in main function to display a context menu
    /// create menu as shown in main.fs
    let subRightClick dispatch =
        Browser.Dom.document.addEventListener("contextmenu", unbox (fun (e:Browser.Types.MouseEvent) ->
            e.preventDefault()
            //printfn "Context Menu listener sending to main..."
            dispatch (ContextMenuAction e)))
            

    let subContextMenuCommand dispatch =
        renderer.ipcRenderer.on("context-menu-command", fun ev args ->
            let arg:string = unbox args |> Array.map string |> String.concat ""
            match arg.Split [|','|] |> Array.toList with
            | [ menuType ; item ] ->
                //printfn "%A" $"Renderer context menu callback: {menuType} --> {item}"
                dispatch <| ContextMenuItemClick(menuType,item,dispatch)
            | _ -> printfn "Unexpected callback argument sent from main.") |> ignore

    /// Why does this not work in production?
    let periodicMemoryCheckCommand dispatch =
        JSHelpers.periodicDispatch dispatch UpdateHelpers.Constants.memoryUpdateCheckTime CheckMemory |> ignore


    Cmd.batch [
        Cmd.ofSub subDown
        Cmd.ofSub subUp
        Cmd.ofSub subRightClick
        Cmd.ofSub subContextMenuCommand
        //Cmd.ofSub periodicMemoryCheckCommand 
        ]




    



    

Program.mkProgram init update view'
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.withSubscription keyPressListener
|> Program.run
