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

//-----------------------------------------------------------------------------------------------//
//------------------------------------- EXCEPTION BOUNDARY --------------------------------------//
//-----------------------------------------------------------------------------------------------//

// **No exception should ever reach any of this.** Issie raises exceptions in quantity - the
// simulator alone has hundreds of `failwithf`s about states it says cannot arise - but every one
// of them is raised somewhere that catches it and turns it into something the user can read:
// `Simulator.startCircuitSimulation` makes an `InternalError`, `FilesIO` makes an `Error`, the
// Verilog compiler makes a parse failure. An exception arriving here got past all of that, which
// means a bug, and by the time anybody hears about it the only evidence left is what was
// recorded when it happened. So it is recorded, and Info -> Bug Reports is where the user finds
// it.
//
// There are four ways out of Issie's own code and all four are covered:
//
//   update    the Elmish update function, caught here rather than by Elmish (see `update`)
//   view      building the React element tree
//   window    anything called by the browser - DOM handlers, timers, animation frames, and the
//             React render that `withReactBatched` does inside one of those
//   promise   a rejected promise nothing awaited: file I/O, the sidecar, the web workers
//
// Recording is all that happens. Nothing here tries to recover, and nothing dispatches: a
// boundary that repaired the model would be guessing at what the failed operation was half way
// through doing.

/// What a thrown value says about itself. Three shapes arrive here and they are not alike:
///
///   a JavaScript Error   what a real bug in the renderer throws. Name, message and stack.
///   an F# exception      NOT an Error. Fable's `Exception` is a plain class with a message and
///                        an inner exception, so `failwith` gives its own type name and what it
///                        said, and nothing else.
///   anything at all      a rejected promise carries whatever it was rejected with, which can be
///                        a string, a number, or nothing.
[<Emit("$0 instanceof Error ? String($0) \
        : ($0 != null && typeof $0.message === 'string' \
           ? ((($0.constructor && $0.constructor.name) || 'exception') + ': ' + $0.message) \
           : String($0))")>]
let private thrownMessage (thrown: obj) : string = jsNative

/// **An F# exception has no stack under Fable, and this returns "" for one.** Only a real
/// JavaScript Error carries one, which is the majority of what gets this far - a `failwith` that
/// escapes is a bug in Issie's own error handling, and a TypeError is a bug in Issie. Where the
/// stack is missing, what stands in for it is the source: `update` says which message was being
/// handled.
[<Emit("$0 && $0.stack ? String($0.stack) : ''")>]
let private thrownStack (thrown: obj) : string = jsNative

/// The last message `update` finished handling.
///
/// **This is what localises an F# exception, because nothing else does.** An exception raised in a
/// render or a callback says only what it was - `KeyNotFoundException: The given key was not
/// present in the dictionary` - and the message that produced the model being rendered is the
/// most useful thing anybody can be told about where to look.
///
/// The MESSAGE and not its name: naming one is not free. `shortDisplayMsg` reaches
/// `$"Sheet %10A{sMsg}"` for a draw block message and calls `Simulator.getFastSim()` for
/// `SetWSModel`, neither of which belongs on the path every message takes. This is one reference
/// assignment, and the name is built only if an exception is actually recorded.
///
/// It therefore holds one message's payload until the next message replaces it, which in a UI
/// that dispatches on every mouse move is not long - and what it holds is almost always what has
/// just gone into the model anyway. Not model state: it is a note about what the application was
/// doing, kept for the case where the model can no longer be trusted (docs/mutableState.md).
let mutable private lastHandledMsg: Msg option = None

/// The case name of an F# union, straight off the value. Fable emits a `cases()` on every union
/// class, so this is exact and costs nothing but the lookup - unlike `%A`, which would print the
/// message's whole payload and is the reason `shortDisplayMsg` exists at all.
[<Emit("($0 && typeof $0.cases === 'function' && typeof $0.tag === 'number') ? String($0.cases()[$0.tag]) : ''")>]
let private unionCaseName (value: obj) : string = jsNative

/// Naming a message must never be the thing that loses an exception: `shortDisplayMsg` is an
/// incomplete match by construction, so a case added without a line there raises.
let private nameOfMsg (msg: Msg) =
    try
        match UpdateHelpers.shortDisplayMsg msg with
        | Some name -> name
        | None ->
            // None means "not worth showing while tracing", and its caller then falls back to %A
            // over the message - which is the one thing that must not happen here. The union
            // knows its own case names, so ask it instead.
            match unionCaseName (box msg) with
            | "" -> "an unnamed message"
            | name -> name
    with _ ->
        "a message shortDisplayMsg does not name"

/// Everything that can be said about where an exception came from, best first.
///
/// A real JavaScript Error carries a stack. An F# exception does not - Fable's `Exception` is a
/// plain class - but V8 still knows where the throw was, and hands it to `window.onerror` as a
/// file and a line even when the thrown value carries nothing. So an uncaught F# exception is not
/// unlocatable; the location just arrives beside it rather than on it. Where Issie CAUGHT the
/// exception itself there is no site either, because catching is what stops V8 reporting one, and
/// the last message handled is all that is left.
let private whereFrom (stack: string) (site: string) =
    [ if stack <> "" then stack
      elif site <> "" then $"  thrown at {site}"
      else "  no stack: an F# exception carries none, and this one was caught before V8 reported it"
      match lastHandledMsg with
      | Some msg -> $"  last message handled: {nameOfMsg msg}"
      | None -> () ]
    |> String.concat "\n"

/// The one way into the buffer. A `box`ed `exn` and a value caught from `window.onerror` are not
/// the same shape - see above - so both go through the two readers rather than being read here.
/// `site` is the throw location when the browser gave us one, and "" when it did not.
let private recordThrownAt (source: string) (site: string) (thrown: obj) =
    Log.recordException source (thrownMessage thrown) (whereFrom (thrownStack thrown) site)

let private recordThrown (source: string) (thrown: obj) = recordThrownAt source "" thrown

/// Catch what the browser calls, which Elmish never sees.
let private installExceptionBoundary () =
    Browser.Dom.window.addEventListener("error", fun ev ->
        let ev: obj = box ev
        // V8 knows where the throw was even when the thrown value does not - see whereFrom
        let file: string = ev?filename
        let line: int = ev?lineno
        let col: int = ev?colno
        let site = if isNull (box file) then "" else $"{file}:{line}:{col}"
        let thrown: obj = ev?error

        if isNull thrown then
            // a script or resource that failed to load: a message, and nothing thrown
            let message: string = ev?message
            Log.recordException "window" message (whereFrom "" site)
        else
            recordThrownAt "window" site thrown)

    Browser.Dom.window.addEventListener("unhandledrejection", fun ev ->
        let ev: obj = box ev
        recordThrown "promise" ev?reason)

// Before anything else in the renderer runs, so that a failure while the application is starting
// up - which is the one a user can do least about - is recorded like any other.
installExceptionBoundary ()


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
        // Which simulator runs. The sidecar is the default and the intended one; the renderer's own
        // simulator is kept for development - see Model.SimulateInRenderer - and these two items are
        // the only way to reach it from the app.
        //
        // First in this menu, and not inside Play with the experiments, because it is the switch
        // somebody comes to this menu looking for. Which one is running is said in the log when it
        // changes (it always shows, whatever the log categories are set to), since a menu built once
        // at startup cannot carry a tick that stays true.
        makeDebugItem "Simulate In .NET Sidecar (default)" None
            (fun _ -> dispatch <| ExecFuncInMessage((fun _ d -> DevHarness.setSimulateInRenderer false d), dispatch))
        makeDebugItem "Simulate In Renderer (deprecated)" None
            (fun _ -> dispatch <| ExecFuncInMessage((fun _ d -> DevHarness.setSimulateInRenderer true d), dispatch))
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
            // One per way out of Issie's own code, because an exception boundary nobody can fire
            // is one nobody notices has stopped working. What each should do is recorded in the
            // buffer and shown on Info -> Bug Reports; the last one also replaces the screen with
            // the crash page, which is the only part that cannot be checked any other way.
            makeDebugItem "Force Exception In A Menu Action" None
                (fun ev -> failwithf "User exception from menus")
            makeDebugItem "Force Exception In Update" None
                (fun _ -> DevHarness.forceException "update" dispatch |> Log.out)
            makeDebugItem "Force Exception In View (until restart)" None
                (fun _ -> DevHarness.forceException "view" dispatch |> Log.out)
            makeDebugItem "Test Web Sorker Performance" None
                (fun _ -> Playground.WebWorker.testWorkers Playground.WebWorker.Constants.workerTestConfig)
            makeDebugItem "Test Sidecar Latency" None
                (fun _ -> Playground.Sidecar.testLatency Playground.Sidecar.Constants.latencyTestConfig)
            makeDebugItem "Send Design To Sidecar" None
                (fun _ -> dispatch <| Msg.ExecFuncInMessage(DevHarness.sendDesignToSidecar, dispatch))
            makeDebugItem "Run Design On Sidecar (1M cycles)" None
                (fun _ -> dispatch <| Msg.ExecFuncInMessage(DevHarness.runOnSidecarWithProgress 1_000_000 250 None, dispatch))

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

    // **Debug builds only, and here rather than beside the window handlers because it needs
    // dispatch and a debug level, neither of which exists when the module loads.** Recording is
    // for everybody; interrupting is for the person who can fix it. A user is told about the
    // things that concern them by a notification or an error pane, and a popup full of stack
    // traces on top of that would be noise they cannot act on.
    //
    // Dispatching from inside a render is safe here only because `withReactBatched` renders on an
    // animation frame: the message is queued, the update runs after the render that logged, and
    // React is never asked to update while it is rendering.
    //
    // **Once per distinct error, not once per occurrence.** An error on a path that runs every
    // frame - a waveform viewer with nothing to draw from, a handler that throws on every mouse
    // move - would otherwise make the debug build unusable, and the tempting fix is to log it as
    // something milder than it is. That would be a lie about what happened, and the buffer and
    // the bug report would carry the lie. Dedupe the interruption instead and let the severity
    // stay honest. The same shape, and the same reason, as `Log.warnedKeys`.
    if debugLevel > 0 then
        let mutable reported: Set<string> = Set.empty

        Log.onErrorLogged <-
            Some(fun text ->
                if not (reported.Contains text) then
                    reported <- reported.Add text
                    dispatch ProblemLogged)

    attachExitHandler dispatch
    KeyBindings.publishKeyLog()
    Log.publish()
    DevHarness.publish dispatch
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
    try
        let model', cmd = Update.update msg model
        // The keyboard context, derived here because a DOM handler cannot see the model and
        // preventDefault has to be decided synchronously inside the handler. This replaces
        // evilUIState, which held a three-case approximation of the same thing for the sole purpose
        // of deciding whether to swallow the space bar.
        KeyBindings.setContextFromModel model'
        // on the way out, so this names the message that produced the model the view is about to
        // draw - which is what an exception during that render needs to be told about
        lastHandledMsg <- Some msg
        model',cmd
    with e ->
        // **Caught here rather than left to Elmish, which would also catch it.** Elmish's handler
        // is reached through `sprintf "Unable to process the message: %A" msg`, and an Issie
        // message is not a small value - SetProject carries every loaded component of the project.
        // Formatting one with %A walks the lot, so the price of an exception in update used to be
        // an unbounded pause with nothing on screen to explain it. Catching first means that
        // string is never built.
        //
        // The model is returned unchanged. Whatever the update was doing did not finish, so the
        // model it would have produced does not exist; the one Issie already had is at least a
        // model the view has drawn before. Returning the same REFERENCE also means React does not
        // re-render, which is right: nothing changed.
        //
        // What is recorded is named with the message being handled, which is the nearest thing to
        // a stack that an F# exception has - and taken from `shortDisplayMsg`, which names every
        // case by hand precisely so that nothing has to print a message carrying a whole model.
        recordThrown $"update ({nameOfMsg msg})" (box e)
        model, Cmd.none

let view' model dispatch =
    // Counted unconditionally - one increment - because a re-render storm is Issie's classic
    // performance failure and nothing used to count renders at all.
    Log.countRender()
    // the model the render is about to draw, for anything driving Issie from outside it
    DevHarness.recordModel model
    let start = TimeHelpers.getTimeMs()

    let drawn =
        try
            if DevHarness.forceViewException then failwith "forced exception from the view"
            view model dispatch
            |> (fun view ->
                if Log.isOn Log.View then
                    TimeHelpers.instrumentInterval ">>>View" start view
                else
                    view)
        with e ->
            // The view is a pure function of the model, so a view that throws throws again on
            // every later render: there is no carrying on from here and no point pretending
            // otherwise. What there is a point in is the user being able to send us the reason,
            // which the Info window can no longer be opened to show - so the page IS the report.
            //
            // This catches what `view` itself raises. An exception raised inside a child
            // component's own render happens later, during React's reconciliation, and comes back
            // through the window handler instead.
            recordThrown "view" (box e)
            ExceptionReport.crashPage (thrownMessage (box e))

    // after the elements exist, so a caller waiting on a render is told once there is one
    DevHarness.renderDone ()
    drawn

/// A DOM event listener as an Elmish 4 subscription: attach on subscribe, detach on dispose.
let private domListenerSub (eventName: string) (makeHandler: (Msg -> unit) -> (Browser.Types.Event -> unit)) =
    fun (dispatch: Msg -> unit) ->
        let handler = makeHandler dispatch
        Browser.Dom.document.addEventListener(eventName, handler)
        { new System.IDisposable with
            member _.Dispose() = Browser.Dom.document.removeEventListener(eventName, handler) }

/// As domListenerSub, but registered with `passive: false`. Chromium makes document-level
/// `wheel` and touch listeners passive by default, and a passive listener cannot
/// preventDefault - which is the point of handling the canvas wheel natively at all: a zoom
/// gesture must suppress the scroll (or page zoom) that would otherwise accompany it.
let private domNonPassiveListenerSub (eventName: string) (makeHandler: (Msg -> unit) -> (Browser.Types.Event -> unit)) =
    fun (dispatch: Msg -> unit) ->
        let handler = makeHandler dispatch
        let options = jsOptions<Browser.Types.AddEventListenerOptions> (fun o -> o.passive <- false)
        Browser.Dom.document.addEventListener(eventName, handler, options)
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
    /// Every wheel over the canvas arrives here, natively and non-passive: React's own wheel
    /// listeners are passive, so only from here can a zoom gesture stop the accompanying scroll.
    let subWheel = domNonPassiveListenerSub "wheel" SheetDisplay.onCanvasWheel
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
        ["wheel"], subWheel
        ["contextmenu"], subRightClick
        ["ipc"; "context-menu-command"], subContextMenuCommand
    ]

Program.mkProgram init update view'
// What is left for this to catch, now that `update` catches its own, is a command that failed
// and a subscription that failed - the async and IPC edges of the application.
|> Program.withErrorHandler (fun (context, e) ->
    // The context says which message was in flight, which is worth having - but Elmish builds it
    // with %A over that message, so it can be enormous. Truncated, and folded into the source
    // rather than logged as a second error: one thing went wrong, so one line and one report.
    let context = if context.Length > 120 then context[..119] + "..." else context
    recordThrown $"elmish ({context})" (box e))
|> Program.withReactBatched "app"
|> Program.withSubscription appSubscriptions
|> Program.run
