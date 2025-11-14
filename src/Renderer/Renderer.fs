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
importSideEffects "./scss/extra.css"

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
    let newSheetKeyOp = Some (if isMac then "Cmd+n" else "CmdOrCtrl+n")
    let saveSheetKeyOp = Some (if isMac then "Cmd+s" else "CmdOrCtrl+s")
    let saveProjectKeyOp = if isMac then Some "Cmd+Shift+s" else None
    let aboutIssieKeyOp = if isMac then Some "Cmd+h" else None
    let quitIssieKeyOp = if isMac then Some "Cmd+q" else None
    let testEditorKeyOp = Some (if isMac then "Cmd+e" else "CmdOrCtrl+q")
    makeMenu false "Sheet" [
        makeItem "New Sheet" newSheetKeyOp (fun ev -> dispatch (MenuAction(MenuNewFile, dispatch)))
        makeItem "Save Sheet" saveSheetKeyOp (fun ev -> dispatch (MenuAction(MenuSaveFile, dispatch)))
        makeItem "Save Project in New Format" saveProjectKeyOp 
            (fun ev -> dispatch (MenuAction(MenuSaveProjectInNewFormat, dispatch)))
        //makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint, dispatch)))
        menuSeparator
        makeItem "Write Design as Verilog" None (fun ev -> dispatch (MenuAction(MenuVerilogOutput, dispatch)))
        menuSeparator
        makeItem ("About Issie "+Version.VersionString) aboutIssieKeyOp (fun ev -> UIPopups.viewInfoPopup dispatch)
        makeItem "Quit Issie" quitIssieKeyOp (fun ev -> dispatch (MenuAction(MenuExit, dispatch)))
        menuSeparator
        makeCondRoleItem (debugLevel <> 0 && not isMac) "Hard Restart Issie" None MenuItemRole.ForceReload
        makeWinDebugItem "Trace All" None (fun _ -> debugTraceUI <- Set.ofList ["update"; "view"])
        makeWinDebugItem "Trace View Function" None (fun _ -> debugTraceUI <- Set.ofList ["view"])
        makeWinDebugItem "Trace Update Function" None (fun _ -> debugTraceUI <- Set.ofList ["update"])
        makeWinDebugItem "Trace Mouse Messages" None (fun _ -> debugTraceUI <- debugTraceUI + Set.ofList ["mouse"])
        makeWinDebugItem "Trace Off" None (fun _ -> debugTraceUI <- Set.ofList [])
        makeMenuGen (debugLevel > 0) false "Play" [
            makeDebugItem "Heap" None
                (fun _ ->
                    let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
                    let maxHeapSize = () |> maxHeap |> float |> (fun v -> v / 1000000.)
                    let heapUsage = usedHeapSize / maxHeapSize * 100.
                    printfn $"Used Heap:%.2f{usedHeapSize}MB; Max Heap:%.2f{maxHeapSize}MB; Usage:%.2f{heapUsage}%%\n")
            makeDebugItem "Initialise" None
                (fun _ -> dispatch <| ExecFuncInMessage(softInitialise, dispatch))
            makeDebugItem "Screen Reset" None
                (fun _ ->
                    let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
                    printfn $"Used Heap; Heap size before screen reset:%.2f{usedHeapSize}MB\n"
                    dispatch (SetTopMenu TransientClosed))
            makeDebugItem "Set Scroll" None
                (fun _ -> SheetDisplay.writeCanvasScroll {X=1000.; Y=1000.} |> ignore)
            makeDebugItem "Trace All Times" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(0.1, 0.1)
                    if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update"; "view"])
            makeDebugItem "Trace Short, Medium & Long Times" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(1.5, 1.5)
                    if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update"; "view"])
            makeDebugItem "Trace Medium & Long Times" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(3.0, 3.0)
                    if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update"; "view"])
            makeDebugItem "Trace Long Times" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(20.0, 20.0)
                    if debugTraceUI = Set.ofList [] then debugTraceUI <- Set.ofList ["update"; "view"])
            makeDebugItem "Print Misc Performance Info" None
                (fun _ ->
                    Playground.Memory.printListeners()
                    Playground.Memory.printProcessMemory()
                    dispatch SaveModel)
            makeDebugItem "Test Fonts" None (fun _ -> Playground.TestFonts.makeTextPopup dispatch)
            makeDebugItem "Test Editor" testEditorKeyOp (fun _ -> Playground.Misc.makeEditorPopup dispatch)
            makeWinDebugItem "Run Performance Check" None (fun _ -> Playground.MiscTests.testMaps())
            makeWinDebugItem "Print Names of Static Asset Files" None (fun _ -> Playground.MiscTests.testAssets())
            makeWinDebugItem "Test Breadcrumbs" None
                (fun _ -> dispatch <| Msg.ExecFuncInMessage(Playground.Breadcrumbs.testBreadcrumbs,dispatch))
            makeWinDebugItem  "Test All Hierarchies Breadcrumbs" None 
                (fun _ ->
                    dispatch <| Msg.ExecFuncInMessage(Playground.Breadcrumbs.testAllHierarchiesBreadcrumbs,dispatch))
            makeDebugItem "Force Exception" None
                (fun ev -> failwithf "User exception from menus")
            makeDebugItem "Test Web Sorker Performance" None
                (fun _ -> Playground.WebWorker.testWorkers Playground.WebWorker.Constants.workerTestConfig)

        ]
        makeMenu false "Verilog" [
            makeDebugItem "Run Verilog Tests" None (fun _ ->
                runCompilerTests ()
                printfn "Compiler tests done")
            makeDebugItem "Run Verilog Performance Tests" None (fun _ ->
                runPerformanceTests ()
                printfn "Performance tests done")
            makeDebugItem "Generate Driver Modules" None (fun _ -> genDriverFiles ())
            makeDebugItem "Icarus Compile Testcases" None (fun _ -> icarusCompileTestCases ())
            makeDebugItem "Icarus Run testcases" None (fun _ -> icarusRunTestCases ())
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

    let fullscreenKeyOp = Some (if isMac then "Cmd+Ctrl+F" else "F11")
    let webZoomInKeyOp = Some (if isMac then "Cmd+Plus" else "CmdOrCtrl+Plus")
    let webZoomOutKeyOp = Some (if isMac then "Cmd+-" else "CmdOrCtrl+-")
    let webZoomResetKeyOp = Some (if isMac then "Cmd+0" else "CmdOrCtrl+0")
    let diagramZoomInKeyOp = Some (if isMac then "Cmd+Option+Plus" else "Alt+Up")
    let diagramZoomOutKeyOp = Some (if isMac then "Cmd+Option+-" else "Alt+Down")
    let diagramZoomResetKeyOp = Some (if isMac then "Cmd+Option+0" else "CmdOrCtrl+w")
    let toggleGridKeyOp = if isMac then Some "Cmd+Option+g" else None
    let toggleWireKeyOp = if isMac then Some "Cmd+Option+w" else None
    let devtoolsKeyOp = Some (if isMac then "Cmd+Option+i" else "Ctrl+Shift+i")
    makeMenu false "View" [
        makeRoleItem "Enter/Exit Fullscreen" fullscreenKeyOp MenuItemRole.Togglefullscreen
        menuSeparator
        makeRoleItem "Zoom In" webZoomInKeyOp MenuItemRole.ZoomIn
        makeRoleItem "Zoom Out" webZoomOutKeyOp MenuItemRole.ZoomOut
        makeRoleItem "Zoom Reset" webZoomResetKeyOp MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" diagramZoomInKeyOp (fun ev -> dispatch SheetT.KeyboardMsg.ZoomIn)
        makeItem "Diagram Zoom Out" diagramZoomOutKeyOp (fun ev -> dispatch SheetT.KeyboardMsg.ZoomOut)
        makeItem "Diagram Zoom to Fit" diagramZoomResetKeyOp (fun ev -> dispatch SheetT.KeyboardMsg.CtrlW)
        menuSeparator
        makeItem "Show/Hide Grid" toggleGridKeyOp (fun ev -> sheetDispatch SheetT.Msg.ToggleGrid)
        makeItem "Show/Hide Wire Arrows" toggleWireKeyOp (fun ev -> busWireDispatch (BusWireT.Msg.ToggleArrowDisplay))
        makeMenu false "Wire Type" [
            makeItem "Jump Wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Jump)
            makeItem "Radiussed Wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Radiussed)
            makeItem "Modern Wires" None (fun ev -> wireTypeDispatch SheetT.WireTypeMsg.Modern)
        ]
        makeMenu false "Theme" [
            makeItem "Default" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.Colourful
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.Colourful))
            makeItem "Light" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.Light
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.Light))
            makeItem "Grayscale" None (fun ev -> 
                maindispatch <| SetThemeUserData SymbolT.ThemeType.White
                symbolDispatch (SymbolT.Msg.SetTheme SymbolT.ThemeType.White))
        ]
        menuSeparator
        makeItem "Show/Hide Build Tab" None (fun ev -> maindispatch (ChangeBuildTabVisibility))
        makeItem "Show/Hide App Memory Display" None (fun ev ->
                    JSHelpers.loggingMemory <- not JSHelpers.loggingMemory
                    let state = if loggingMemory then "on" else "off"
                    printfn $"Memory display is now {state}.")
        //makeItem "Benchmark" (Some "Ctrl+Shift+B") (fun ev -> maindispatch Benchmark) // does this work?
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Show/Hide Developer Tools" devtoolsKeyOp (fun _ ->
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

    let copyKey = if isMac then "Cmd+c" else "CmdOrCtrl+c"
    let pasteKey = if isMac then "Cmd+v" else "CmdOrCtrl+v"
    let selectAllKey = if isMac then "Cmd+a" else "CmdOrCtrl+a"
    let delteKey = if isMac then "Backspace" else "Delete"
    let rotAnticlockKey = if isMac then "Cmd+Option+Left" else "CmdOrCtrl+Left"
    let rotClockKey = if isMac then "Cmd+Option+Right" else "CmdOrCtrl+Right"
    let flipVertKey = if isMac then "Cmd+Option+Up" else "CmdOrCtrl+Up"
    let flipHoriKey = if isMac then "Cmd+Option+Down" else "CmdOrCtrl+Down"
    let alignKey = if isMac then "Cmd+Option+a" else "CmdOrCtrl+Shift+a"
    let distKey = if isMac then "Cmd+Option+d" else "CmdOrCtrl+Shift+d"
    let rotLabelClockKey = if isMac then "Cmd+Option+r" else "CmdOrCtrl+Shift+Right"
    let undoKey = if isMac then "Cmd+z" else "CmdOrCtrl+z"
    let redoKey = if isMac then "Cmd+Shift+z" else "CmdOrCtrl+y"
    let cancelKey = if isMac then "Esc" else "Esc"
    jsOptions<MenuItemConstructorOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- Some MenuItemType.Submenu
        invisibleMenu.label <- Some "Edit"
        invisibleMenu.visible <- Some true
        invisibleMenu.submenu <-
            [|
                makeElmItem "Copy" copyKey (fun () -> dispatch SheetT.KeyboardMsg.CtrlC)
                makeElmItem "Paste" pasteKey (fun () -> dispatch SheetT.KeyboardMsg.CtrlV)
                makeElmItem "Select All" selectAllKey (fun () -> dispatch SheetT.KeyboardMsg.CtrlA)
                makeElmItem "Delete" delteKey (fun () -> dispatch SheetT.KeyboardMsg.DEL)
                menuSeparator
                makeElmItem "Rotate Anticlockwise" rotAnticlockKey (fun () -> rotateDispatch CommonTypes.Degree270)
                makeElmItem "Rotate Clockwise" rotClockKey (fun () -> rotateDispatch CommonTypes.Degree90)
                makeElmItem "Flip Vertically" flipVertKey (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipVertical)
                makeElmItem "Flip Horizontally" flipHoriKey (fun () -> sheetDispatch <| SheetT.Flip SymbolT.FlipHorizontal)
                makeElmItem "Align Components" alignKey (fun ev -> sheetDispatch <| SheetT.Arrangement SheetT.AlignSymbols)
                makeElmItem "Distribute Components" distKey (fun ev -> sheetDispatch <| SheetT.Arrangement SheetT.DistributeSymbols)
                menuSeparator
                makeElmItem "Rotate Label Clockwise" rotLabelClockKey (fun ev -> sheetDispatch <| SheetT.RotateLabels)
                makeItem "Move Component Ports" None
                    (fun _ -> dispatch' <| ShowStaticInfoPopup("How to move component ports", 
                                                               SymbolPortHelpers.moveCustomPortsPopup(), dispatch'))
                menuSeparator
                makeElmItem "Undo Action" undoKey (fun () -> dispatch SheetT.KeyboardMsg.CtrlZ)
                makeElmItem "Redo Action" redoKey (fun () -> dispatch SheetT.KeyboardMsg.CtrlY)
                makeElmItem "Cancel" cancelKey (fun () -> dispatch SheetT.KeyboardMsg.ESC)
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
//let view (model:Model) (dispatch: Msg -> unit) = Playground.Misc.displayEditor () 
// -- Update Model

let update msg model =
    let model', cmd = Update.update msg model
    if Option.isSome model'.CodeEditorState && Option.isSome model'.PopupViewFunc then
        Update.evilUIState <- Update.EvilCodeEditor 
    elif Option.isSome model'.PopupViewFunc then
        Update.evilUIState <- Update.EvilUIPopup
    else
        Update.evilUIState <- Update.EvilNoState
    model',cmd

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

/// Used to listen for pressing down of Ctrl for selection toggle.
/// Also for the code editor keys.
/// TODO: use this for global key press info throughout Issie
let keyPressListener initial =
    let subDown dispatch =
        Browser.Dom.document.addEventListener("keydown", fun e ->
            let ke: KeyboardEvent = downcast e
            if (jsToBool ke.ctrlKey || jsToBool ke.metaKey) && firstPress then
                firstPress <- false
                //printf "Ctrl-Meta Key down (old method)"
                dispatch <| Sheet(SheetT.PortMovementStart)
            else
                ()
            dispatch <| AnyKeyPress {
                                KeyString = ke?key
                                AltKey = ke?altKey
                                ControlKey = ke?ctrlKey
                                MetaKey = ke?metaKey
                                ShiftKey = ke?shiftKey
                            }
            // prevent unwanted default processing of " " key as scrolling
            // NB - input boxes in popups require default processing to work
            // it seems that input boxes in properties pane do not require " ".
            // TODO: sort out key processing consistently allowing " " for input boxes
            // TODO: See also OTHER hacks doing key processing ManualKeyUp etc. Unify these.
            // NB - electron menus need to be able to process many control keys.
            match Update.evilUIState with
            | Update.EvilCodeEditor | Update.EvilNoState when ke?key = " "  ->
                e.preventDefault()
            | _ -> () //e.preventDefault()
            )
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
