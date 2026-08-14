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

let isMac = Bridge.isMac





// -- Init Model

let init() =
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none

/// Already in the bootstrap record, which main fills from the same app.getPath call the old
/// get-user-data channel made - so this is now a lookup rather than a round trip.
let getUserAppDir () : string = Bridge.userData

let softInitialise model dispatch =
    //Playground.Memory.modelCopy <- None
    //dispatch (UpdateModel(fun _ -> fst (init())))
    //let userAppDir = getUserAppDir()
    //dispatch <| ReadUserData userAppDir
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
    Bridge.onClosingWindow (fun () ->
        // send a message which will process the request to exit
        dispatch <| MenuAction(MenuExit, dispatch))
    Bridge.onWindowLostFocus (fun () ->
        dispatch <| MenuAction(MenuLostFocus, dispatch))
(*
// Set up window close interlock using IPC from/to main process
let attachGetAppHandler dispatch =
    // set up callback called when attempt is made to close main window
    renderer.ipcRenderer.on ("get-user-data", (fun (event: Event)->
        // send a message which will process the request to exit
        dispatch <| SetUserAppDir (unbox event. : string)
        )) |> ignore*)



/// What each application-menu item does, keyed by the id main sends back when it is clicked.
///
/// The menu lives in the main process now, and a main-process menu item cannot hold an F# closure -
/// so the closure stays here and only its id makes the trip. Not model state: these belong to a menu
/// that is built once and lives as long as the app (docs/mutableState.md).
let mutable private menuActions: Map<string, KeyboardEvent -> unit> = Map.empty
let mutable private nextMenuId = 0

/// Look up and run whatever main says was clicked.
let runMenuAction (id: string) =
    match Map.tryFind id menuActions with
    | Some action -> action (unbox null)
    | None -> Log.warn $"application menu: no action registered for '{id}'"

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let item = createEmpty<Electron.MenuItemConstructorOptions>
   item.label <- Some label
   item.accelerator <- accelerator
   // an id rather than item.click: see menuActions above
   nextMenuId <- nextMenuId + 1
   let id = $"issieMenu{nextMenuId}"
   item.id <- Some id
   menuActions <- Map.add id iAction menuActions
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
//-------------------------------------------DEVELOPMENT MENU------------------------------------------------//
//-----------------------------------------------------------------------------------------------------------//

/// The only Electron menu Issie still has, and only in debug builds.
///
/// Nothing here carries an accelerator, and nothing here should. Electron registers a menu item's
/// accelerator globally and unconditionally, which is why the Sheet, Edit and View menus had to
/// go: they took the key before anything could ask whether it made sense in that context. A menu
/// with no accelerators takes nothing, so it can coexist with KeyBindings.
///
/// Everything users need from the old menus is on the renderer's own menu bar - Project, Sheets,
/// Edit and View in TopMenuView - or in a context menu.
let devMenu (dispatch) =
    makeMenuGen (debugLevel > 0) false "Development" [
        // The writable half of the component libraries, under userData - where "save as library
        // component" puts things. The libraries shipped with Issie are elsewhere, read-only under
        // the installation, so this is the one that changes as the app is used.
        makeDebugItem "Open User Library Directory" None (fun _ ->
            match ComponentLibraries.tryUserLibrariesDirectory () with
            | Ok path ->
                FilesIO.openFolderInFileManager path (fun reason ->
                    dispatch <| SetFilesNotification
                        (Notifications.errorFilesNotification $"Could not open {path}: {reason}"))
            | Error e ->
                dispatch <| SetFilesNotification
                    (Notifications.errorFilesNotification $"No user library directory: {e}"))
        makeCondRoleItem (debugLevel <> 0 && not isMac) "Hard Restart Issie" None MenuItemRole.ForceReload
        // One item per log category. The same switches are reachable as window.issieLog.on "wire"
        // from a console, and as --log=wire at launch, which is the only one of the three that is
        // on before a project loads.
        makeMenuGen (debugLevel > 0) false "Log" (
            [ makeDebugItem "Off" None (fun _ -> Log.setCategories 0)
              makeDebugItem "Everything" None (fun _ -> Log.setCategories Log.All) ]
            @ (Log.categoryNames
               |> List.filter (fun name -> name <> "all")
               |> List.map (fun name ->
                    makeDebugItem (name[0..0].ToUpper() + name[1..]) None
                        (fun _ -> Log.setCategories (Log.maskOfNames name)))))
        makeMenuGen (debugLevel > 0) false "Play" [
            makeDebugItem "Heap" None
                (fun _ ->
                    let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
                    let maxHeapSize = () |> maxHeap |> float |> (fun v -> v / 1000000.)
                    let heapUsage = usedHeapSize / maxHeapSize * 100.
                    Log.out $"used heap %.2f{usedHeapSize}MB, max heap %.2f{maxHeapSize}MB, \
                              usage %.2f{heapUsage}%%")
            makeDebugItem "Initialise" None
                (fun _ -> dispatch <| ExecFuncInMessage(softInitialise, dispatch))
            // for writing libraries: normally a component's sheets are not the user's business
            makeDebugItem "Toggle Showing Library Sheets" None
                (fun _ -> dispatch <| UpdateModel (fun m -> {m with ShowLibrarySheets = not m.ShowLibrarySheets}))
            makeDebugItem "Screen Reset" None
                (fun _ ->
                    let usedHeapSize = () |> usedHeap |> float |> (fun v -> v / 1000000.)
                    Log.out $"used heap before screen reset: %.2f{usedHeapSize}MB"
                    dispatch (SetTopMenu TransientClosed))
            makeDebugItem "Set Scroll" None
                (fun _ -> SheetDisplay.writeCanvasScroll {X=1000.; Y=1000.} |> ignore)
            // Two, not four thresholds - and neither turns message tracing on behind your back,
            // which the four used to do. The summary says whether anything is slow; the table
            // says what, and is the one to reach for second.
            makeDebugItem "Time Every Interval Over 1.5ms" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.ImmediatePrint(1.5, 1.5)
                    Log.setCategories (Log.enabled ||| Log.Perf))
            makeDebugItem "Summarise Times Every 10s" None
                (fun _ ->
                    TimeHelpers.instrumentation <- TimeHelpers.aggregate 10000.
                    Log.setCategories (Log.enabled ||| Log.Perf))
            makeDebugItem "Times Off" None
                (fun _ -> TimeHelpers.instrumentation <- TimeHelpers.Off)
            makeDebugItem "Print Misc Performance Info" None
                (fun _ ->
                    Playground.Memory.printListeners()
                    Playground.Memory.printProcessMemory()
                    dispatch SaveModel)
            makeDebugItem "Test Fonts" None (fun _ -> Playground.TestFonts.makeTextPopup dispatch)
            makeDebugItem "Test Editor" None (fun _ -> Playground.Misc.makeEditorPopup dispatch)
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
        makeMenuGen (debugLevel > 0) false "Verilog" [
            makeDebugItem "Run Verilog Tests" None (fun _ ->
                runCompilerTests ()
                Log.out "compiler tests done")
            makeDebugItem "Run Verilog Performance Tests" None (fun _ ->
                runPerformanceTests ()
                Log.out "performance tests done")
            makeDebugItem "Generate Driver Modules" None (fun _ -> genDriverFiles ())
            makeDebugItem "Icarus Compile Testcases" None (fun _ -> icarusCompileTestCases ())
            makeDebugItem "Icarus Run testcases" None (fun _ -> icarusRunTestCases ())
        ]
    ]

/// One-shot application setup, run as an Elmish subscription so it receives dispatch:
/// attach the Electron menu, the exit handler, and read the user data.
/// There is nothing to tear down - what it attaches lives as long as the app.

let attachMenusAndKeyShortcuts (dispatch: Msg -> unit) : System.IDisposable =
    let template =
        [ if isMac then
              // macOS keeps its application menu, which cannot be hidden and which the system
              // expects: it is where Cmd+Q, Cmd+H and Cmd+M come from. Those are OS-reserved
              // chords that Issie does not want for anything, so it takes no keys from us.
              yield makeRoleItem "Issie" None MenuItemRole.AppMenu
          if debugLevel > 0 then
              yield devMenu dispatch ]

    dispatch
    <| Msg.ExecFuncInMessage(
        (fun _ _ ->
            // Only ever ADDS a menu, and only a debug build has one to add. Removing the menu bar
            // is the main process's job, done before the first window exists (Main.startRenderer):
            // doing it from here left Electron's own File/Edit/View/Window/Help menu up until the
            // renderer had loaded, and left it up for good whenever this assignment - a property
            // set across the @electron/remote bridge - did not land. Every key those menus used to
            // register is resolved by KeyBindings against the context the user is in.
            match template with
            | [] -> ()
            | items ->
                // main attaches the click handlers, which send the item's id back to runMenuAction
                Bridge.onApplicationMenuCommand runMenuAction
                items |> List.map box |> Array.ofList |> Bridge.setApplicationMenu),
        dispatch)

    attachExitHandler dispatch
    KeyBindings.publishKeyLog()
    Log.publish()
    // How much memory a simulation may take is a fact about this machine, so it is settled here,
    // once, rather than built into the simulator as a number that suits no machine in particular.
    // The heap limit is read rather than assumed: Main.fs asks for one, and V8 grants what it will.
    SimTypes.SimulationBudget.setBudgetsFromMachine
        (Bridge.systemMemoryTotalKB() * 1024.0)
        (float (JSHelpers.heapLimit()))
    Log.dbg Log.Sim
        $"simulation memory budget: {SimTypes.SimulationBudget.formatBytes SimTypes.SimulationBudget.maxTypedArrayBytes} \
          of step arrays, {SimTypes.SimulationBudget.formatBytes SimTypes.SimulationBudget.maxHeapBytes} of heap"
    let userAppDir = getUserAppDir()
    dispatch <| ReadUserData userAppDir
    { new System.IDisposable with member _.Dispose() = () }

// This setup is useful to add other pages, in case they are needed.

type Model = ModelType.Model

type Messages = ModelType.Msg




// -- Create View
// addDebug, which wrapped this dispatch, is gone: it built a message trace string and then threw
// it away, on every dispatch the view made. What it was for - a line per message - is done at the
// update function, which is the only place every message passes through anyway.
let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model =
    let model', cmd = Update.update msg model
    // The keyboard context, derived here because a DOM handler cannot see the model and
    // preventDefault has to be decided synchronously inside the handler. This replaces
    // evilUIState, which held a three-case approximation of the same thing for the sole purpose
    // of deciding whether to swallow the space bar.
    KeyBindings.setContextFromModel model'
    model',cmd

let view' model dispatch =
    // Counted unconditionally - one increment - because a re-render storm is Issie's classic
    // performance failure and nothing used to count renders at all.
    Log.countRender()
    let start = TimeHelpers.getTimeMs()
    view model dispatch
    |> (fun view ->
        if Log.isOn Log.View then
            TimeHelpers.instrumentInterval ">>>View" start view
        else
            view)

/// A DOM event listener as an Elmish 4 subscription: attach on subscribe, detach on dispose.
let private domListenerSub (eventName: string) (makeHandler: (Msg -> unit) -> (Browser.Types.Event -> unit)) =
    fun (dispatch: Msg -> unit) ->
        let handler = makeHandler dispatch
        Browser.Dom.document.addEventListener(eventName, handler)
        { new System.IDisposable with
            member _.Dispose() = Browser.Dom.document.removeEventListener(eventName, handler) }

/// As domListenerSub, but on the window: focus events do not reach document.
let private windowListenerSub (eventName: string) (makeHandler: (Msg -> unit) -> (Browser.Types.Event -> unit)) =
    fun (dispatch: Msg -> unit) ->
        let handler = makeHandler dispatch
        Browser.Dom.window.addEventListener(eventName, handler)
        { new System.IDisposable with
            member _.Dispose() = Browser.Dom.window.removeEventListener(eventName, handler) }

/// The application's subscriptions, in Elmish 4 form: a constant set of identified
/// subscriptions, each returning its teardown. The set does not depend on the model, so each
/// subscription is started exactly once, as with Elmish 3's Cmd.ofSub.
let appSubscriptions (_model: ModelType.Model) : Sub<Msg> =
    /// Every key in Issie arrives here and nowhere else.
    let subDown = domListenerSub "keydown" KeyBindings.onKeyDown
    let subUp = domListenerSub "keyup" KeyBindings.onKeyUp
    /// Ctrl held while the window loses focus never produces a keyup, so without this the draw
    /// block would think Ctrl was still down - which is what the old decaying list of held keys
    /// existed to paper over.
    let subBlur = windowListenerSub "blur" KeyBindings.onWindowBlur
    /// unfinished code
    /// add hook in main function to display a context menu
    /// create menu as shown in main.fs
    let subRightClick =
        domListenerSub "contextmenu" (fun dispatch -> unbox (fun (e:Browser.Types.MouseEvent) ->
            e.preventDefault()
            dispatch (ContextMenuAction e)))

    let subContextMenuCommand (dispatch: Msg -> unit) =
        Bridge.onContextMenuCommand (fun arg ->
            match arg.Split [|','|] |> Array.toList with
            | [ menuType ; item ] ->
                dispatch <| ContextMenuItemClick(menuType,item,dispatch)
            | _ -> Log.warn "unexpected callback argument sent from the main process") |> ignore
        // the listener lives as long as the app: nothing worth tearing down
        { new System.IDisposable with member _.Dispose() = () }

    // Why does this not work in production?
    // let periodicMemoryCheckCommand dispatch =
    //     JSHelpers.periodicDispatch dispatch UpdateHelpers.Constants.memoryUpdateCheckTime CheckMemory |> ignore

    [
        ["menus"], attachMenusAndKeyShortcuts
        ["keydown"], subDown
        ["keyup"], subUp
        ["blur"], subBlur
        ["contextmenu"], subRightClick
        ["ipc"; "context-menu-command"], subContextMenuCommand
    ]

Program.mkProgram init update view'
|> Program.withReactBatched "app"
|> Program.withSubscription appSubscriptions
|> Program.run
