/// Issie's logging.
///
/// It is compiled second, after EEExtensions and before everything else, so the simulator, the
/// draw block, file I/O and the UI can all reach it.
///
/// The rule it exists to enforce: **a run prints a warning or an error, and nothing else.**
/// Anything else belongs to a category that is off until someone turns it on - from the
/// Development menu, from --log on the command line, or from window.issieLog in a console. No
/// rebuild and no restart, because the run worth reading is always the one already happening.
///
/// This replaced 239 ad-hoc printfn calls, of which 219 fired unconditionally: under Fable every
/// printfn is a console.log, so a symbol drag used to narrate itself.
module Log

open Fable.Core
open Fable.Core.JsInterop

//---------------------------------------------------------------------------------------------//
//----------------------------------------- CATEGORIES ----------------------------------------//
//---------------------------------------------------------------------------------------------//

// Categories are subsystems, not severities, because that is how debugging actually goes: "show
// me what the wire separator is doing", never "show me everything at level 3". Severity is the
// choice between error, warn and dbg, and there is nothing else to say about it.
//
// One bit each, as literals, so several can be on at once and "is this on?" compiles to a single
// mask test against a constant folded into the call site. What this replaced - Set.contains on a
// string set - was a balanced-tree walk with string comparisons, run on every render and every
// message.

/// One line per Elmish message.
[<Literal>]
let Update = 1
/// Render counts and times.
[<Literal>]
let View = 2
/// Wire routing, separation and jumps.
[<Literal>]
let Wire = 4
/// Symbols, ports and their geometry.
[<Literal>]
let Symbol = 8
/// Sheet-level gestures: zoom, scroll, selection, clipboard.
[<Literal>]
let Sheet = 16
/// Graph build, fast simulation, width inference.
[<Literal>]
let Sim = 32
/// The waveform simulator.
[<Literal>]
let Wave = 64
/// Project, sheet and library file I/O.
[<Literal>]
let Files = 128
/// Verilog, parameters, and whatever else has too few lines to earn a category.
[<Literal>]
let Misc = 256
/// The periodic performance summary, and the interval times from TimeHelpers.
[<Literal>]
let Perf = 512
/// Not a subsystem but a modifier: it stops Update suppressing mouse drag and move messages,
/// which are the majority of all messages and would bury everything else.
[<Literal>]
let Mouse = 1024
[<Literal>]
let All = 2047

/// Which categories are on: 0 in an ordinary run. Public only because `isOn` is inline and F#
/// will not let an inline function read a private binding - set it with setCategories.
let mutable enabled = 0

/// Whether this category is on. Compiles to `(enabled & 4) !== 0`, so it is the guard to reach
/// for on a path that runs per frame, where even building the message would cost something:
/// `if Log.isOn Log.Wire then Log.dbg Log.Wire $"..."`.
let inline isOn (cat: int) = enabled &&& cat <> 0

let private names =
    [ "update", Update; "view", View; "wire", Wire; "symbol", Symbol; "sheet", Sheet
      "sim", Sim; "wave", Wave; "files", Files; "misc", Misc; "perf", Perf
      "mouse", Mouse; "all", All ]

/// The category names accepted by --log and window.issieLog.on, for error messages and menus.
let categoryNames = names |> List.map fst

//---------------------------------------------------------------------------------------------//
//-------------------------------------------- SINK -------------------------------------------//
//---------------------------------------------------------------------------------------------//

#if FABLE_COMPILER
/// Seconds since the process started, which is the clock Main.stampStartup prints against - so a
/// main-process line and a renderer line can be read against each other. performance.now() is the
/// fallback for anywhere `process` is not exposed; it measures from navigation rather than from
/// launch, so the two agree on intervals if not on the origin.
[<Emit("(typeof process !== 'undefined' && process.uptime ? process.uptime() : performance.now() / 1000)")>]
let private uptime () : float = jsNative

[<Emit("console.log($0)")>]
let private toLog (s: string) : unit = jsNative

[<Emit("console.warn($0)")>]
let private toWarn (s: string) : unit = jsNative

[<Emit("console.error($0)")>]
let private toError (s: string) : unit = jsNative
#else
// Renderer.fsproj is also compiled under plain .NET for the Expecto suite, where an Emit binding
// throws when called. Logging must never be the thing that brings the test run down.
let private startedAt = System.Diagnostics.Stopwatch.StartNew()
let private uptime () = startedAt.Elapsed.TotalSeconds
let private toLog (s: string) = System.Console.Out.WriteLine s
let private toWarn (s: string) = System.Console.Out.WriteLine s
let private toError (s: string) = System.Console.Error.WriteLine s
#endif

/// The last few hundred lines, so a run that misbehaved can be read afterwards and from outside
/// the app - see publish. A fixed array written in place: one slot store per line, with nothing
/// allocated and nothing copied.
[<Literal>]
let private ringSize = 400
let private ring = Array.create ringSize ""
let mutable private ringNext = 0

let private emit (tag: string) (write: string -> unit) (text: string) =
    let line = $"[%7.3f{uptime ()}] {tag} {text}"
    ring[ringNext % ringSize] <- line
    ringNext <- ringNext + 1
    write line

//---------------------------------------------------------------------------------------------//
//-------------------------------------------- API --------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// The result of something the user explicitly asked for from the Development menu - a benchmark,
/// a test run, a memory report. Always emitted: printing it is the point, and a category would
/// mean asking for the output twice.
let out (text: string) = emit "OUT" toLog text

/// Issie could not do what the user asked, or swallowed an exception. Always emitted.
let error (text: string) = emit "ERR" toError text

/// Issie found an inconsistency and carried on. Always emitted.
let warn (text: string) = emit "WRN" toWarn text

let mutable private warnedKeys: Set<string> = Set.empty

/// As warn, but only the first time for a given key. For a complaint on a path that can repeat
/// every frame of a drag, where an unconditional warn would itself be the bug.
let warnOnce (key: string) (text: string) =
    if not (warnedKeys.Contains key) then
        warnedKeys <- warnedKeys.Add key
        warn text

/// A categorised debug line, discarded unless that category is on.
///
/// The message is an ordinary string, built by the caller: a thunk would read better but would
/// rely on Fable reducing away the lambda, and every site this is called from fires at most a few
/// times per user action, where building a template literal costs nothing. Guard with isOn on the
/// rare site that runs per frame.
let dbg (cat: int) (text: string) =
    if isOn cat then emit "DBG" toLog text

//---------------------------------------------------------------------------------------------//
//------------------------------------------ CONTROL ------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// "wire,sim" -> the mask for those two. An unknown name is reported rather than silently
/// dropped: a typo that quietly logs nothing wastes more time than the log saves.
let maskOfNames (spec: string) =
    spec.Split(',')
    |> Array.fold (fun mask name ->
        let name = name.Trim().ToLower()
        match List.tryFind (fst >> (=) name) names with
        | Some (_, bit) -> mask ||| bit
        | None when name = "" -> mask
        | None ->
            let known = String.concat ", " categoryNames
            warn $"unknown log category '{name}' - known categories are {known}"
            mask) 0

/// Turn these categories on and all others off.
let setCategories (mask: int) =
    enabled <- mask
    // a category coming on is a fresh look at the problem, so the once-only warnings get another
    // chance to be seen rather than staying suppressed from before anyone was watching
    warnedKeys <- Set.empty

//---------------------------------------------------------------------------------------------//
//----------------------------------------- COUNTERS ------------------------------------------//
//---------------------------------------------------------------------------------------------//

// Five numbers, updated unconditionally. Nothing is allocated and there is nothing to switch off,
// and between them they answer the only question an ordinary run raises: is Issie busy, and with
// what. Renders in particular were counted nowhere, though a re-render storm is the classic Issie
// performance failure.

let mutable private msgCount = 0
let mutable private renderCount = 0
let mutable private updateMsTotal = 0.
let mutable private slowestMs = 0.
let mutable private slowestName = ""
let mutable private lastSummary = 0.

/// Seconds between performance summary lines, when Perf is on.
let mutable summaryInterval = 10.

let countRender () = renderCount <- renderCount + 1

/// Record one Elmish message and its duration. The name is only asked for when the message beat
/// the slowest so far, so the ordinary cost is an add and a compare.
let countMessage (ms: float) (name: unit -> string) =
    msgCount <- msgCount + 1
    updateMsTotal <- updateMsTotal + ms
    if ms > slowestMs then
        slowestMs <- ms
        slowestName <- name ()
    if isOn Perf then
        let now = uptime ()
        if now - lastSummary > summaryInterval then
            lastSummary <- now
            emit "PRF" toLog
                $"{msgCount} msgs %.0f{updateMsTotal}ms, {renderCount} renders, \
                  slowest {slowestName} %.1f{slowestMs}ms"
            msgCount <- 0
            renderCount <- 0
            updateMsTotal <- 0.
            slowestMs <- 0.
            slowestName <- ""

//---------------------------------------------------------------------------------------------//
//------------------------------------------ PUBLISH ------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// The log, and its switches, beside window.issie and window.issieKeys:
///
///     window.issieLog.lines()          the last few hundred lines, oldest first
///     window.issieLog.on("wire,sim")   turn categories on, live
///     window.issieLog.off()
///
/// This is what lets scripts/inspect-canvas.js read a run's log without DevTools, and what lets
/// a category be turned on in a build somebody else is running. Published in every build for that
/// second reason: a debug-only hook is no use on the machine where the problem happens.
let publish () =
#if FABLE_COMPILER
    let lines () =
        // oldest first, and only the slots that have been written
        Array.init ringSize (fun i -> ring[(ringNext + i) % ringSize])
        |> Array.filter (fun line -> line <> "")
    Browser.Dom.window?issieLog <-
        {| lines = lines
           on = fun (spec: string) -> setCategories (maskOfNames spec); $"logging: {spec}"
           off = fun () -> setCategories 0; "logging off" |}
#endif
    ()
