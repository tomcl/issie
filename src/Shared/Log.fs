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

/// The same lines again, but only the ones that said something was wrong.
///
/// **Errors have to survive being debugged.** The ring above is everything, and everything is
/// mostly debug output: turn a category on and one drag writes several hundred lines, so 400
/// slots is a few seconds and the error that started the investigation is gone before anybody
/// reads it. That is exactly the run somebody is asked to send in a bug report. Only `error` and
/// `warn` write here, so nothing anyone switches on can push a problem out - and 100 slots is
/// hundreds of sessions' worth of a build that is behaving.
[<Literal>]
let private problemRingSize = 100
let private problemRing = Array.create problemRingSize ""
let mutable private problemNext = 0

/// Called after an error is logged, by whoever wants to know, with the text of it. Nothing here
/// decides what that means: `Log` is compiled by both processes and knows nothing about a model,
/// a popup or a debug level, so it offers the fact and the renderer decides what to do with it -
/// see the exception boundary in `Renderer.fs`, which uses it to put the buffer on screen in a
/// debug build.
///
/// **The text, not the formatted line.** The line carries a timestamp, so every one is unique and
/// nothing downstream could tell two occurrences of one error from two different errors. Telling
/// them apart is what lets the renderer interrupt once per distinct error rather than once per
/// occurrence - which is the whole reason a per-frame error does not have to be logged as
/// something milder than it is.
///
/// Errors only, not warnings. A warning is something that went wrong and was recovered from, and
/// there is nothing for anybody to do about one.
///
/// Not a list of subscribers: there is one, it is installed once at startup, and a second would
/// mean two things had opinions about what an error means.
let mutable onErrorLogged: (string -> unit) option = None

/// `isProblem` says whether the line also goes to the problem ring - true for error and warn,
/// false for everything else. Severity is the only thing that decides it: a category is a
/// subsystem, and a bug report wants the errors from every subsystem.
let private emitAs (isProblem: bool) (tag: string) (write: string -> unit) (text: string) =
    let line = $"[%7.3f{uptime ()}] {tag} {text}"
    ring[ringNext % ringSize] <- line
    ringNext <- ringNext + 1

    if isProblem then
        problemRing[problemNext % problemRingSize] <- line
        problemNext <- problemNext + 1

    write line

let private emit (tag: string) (write: string -> unit) (text: string) = emitAs false tag write text

//---------------------------------------------------------------------------------------------//
//-------------------------------------------- API --------------------------------------------//
//---------------------------------------------------------------------------------------------//

/// The result of something the user explicitly asked for from the Development menu - a benchmark,
/// a test run, a memory report. Always emitted: printing it is the point, and a category would
/// mean asking for the output twice.
let out (text: string) = emit "OUT" toLog text

/// **Something was asked for and did not happen.** Always emitted, kept in the problem ring for
/// the bug report, and in a debug build it puts the buffer on screen.
///
/// The line between this and `warn` is whether the outcome is still the right one, and NOT whose
/// fault it was. An OS command that failed and succeeded on retry is a `warn`: the file got
/// written, and there is nothing for anybody to do. The same command failing with no retry left
/// is an error even though the fault is the operating system's, because Issie did not do what it
/// was asked and a developer needs to know that - it is what decides whether Issie should retry,
/// fall back, or tell the user.
///
/// So a failure is a `warn` only when it was recovered from, or when Issie has given the user a
/// real answer instead: "this project will not load, here is why" is a defined outcome and a
/// perfectly good one. A failure that leaves a button doing nothing, a typed value not taking, or
/// a stale screen the user believes is live is an error whoever is to blame.
///
/// **Do not reach for `warn` to stop something repeating.** The renderer interrupts once per
/// distinct error, not once per occurrence - see `onErrorLogged`. Severity says what happened;
/// it is not a rate limit.
///

/// The line is written BEFORE `onError` runs, so that whatever the hook does - which in the
/// renderer is to open a popup showing the buffer - is looking at a buffer this error is already
/// in. A hook that throws is on its own: this is the error path, and an error while reporting an
/// error has nowhere left to go.
let error (text: string) =
    emitAs true "ERR" toError text
    match onErrorLogged with
    | Some f -> f text
    | None -> ()

/// Something went wrong and the outcome is still right: a retry succeeded, a documented fallback
/// took over, or Issie gave the user a real answer about why it could not do what they asked.
/// Always emitted, and kept in the problem ring for the bug report, since a warning is often the
/// first sign of whatever is being reported. Unlike `error` it interrupts nobody, because there
/// is nothing for anybody to act on. See `error` for exactly where the line falls.
let warn (text: string) = emitAs true "WRN" toWarn text

let mutable private warnedKeys: Set<string> = Set.empty

/// As warn, but only the first time for a given key. For a complaint on a path that can repeat
/// every frame of a drag, where an unconditional warn would itself be the bug.
let warnOnce (key: string) (text: string) =
    if not (warnedKeys.Contains key) then
        warnedKeys <- warnedKeys.Add key
        warn text

/// The ring buffer's contents, oldest first. This is what publish hands to a script, and what
/// lets a test assert that a run did - or did not - complain about something. A log line is
/// sometimes the only externally visible symptom of a bug, and one nothing can read is one no
/// test can hold on to.
let recentLines () =
    Array.init ringSize (fun i -> ring[(ringNext + i) % ringSize])
    |> Array.filter (fun line -> line <> "")

/// Just the errors and warnings, oldest first. What the Bug Reports tab shows.
let recentProblems () =
    Array.init problemRingSize (fun i -> problemRing[(problemNext + i) % problemRingSize])
    |> Array.filter (fun line -> line <> "")

//---------------------------------------------------------------------------------------------//
//------------------------------------ UNCAUGHT EXCEPTIONS ------------------------------------//
//---------------------------------------------------------------------------------------------//

/// One exception that got out of Issie's own code.
///
/// **There should never be any.** An exception Issie means to raise is raised at a place that
/// catches it, and the simulator is the worked example: it is full of `failwithf`s about states
/// that cannot arise, every one of them reachable from a design the user drew, and every one
/// caught by `Simulator.startCircuitSimulation` and turned into an `InternalError` the user is
/// asked to send us. Anything recorded here escaped instead, which means a bug, and the only
/// thing anybody can do about it afterwards is read it - so it is kept.
type UncaughtException =
    { /// Seconds since startup - the same clock the log lines carry, so the two can be read
      /// against each other. The time of the LAST occurrence when Repeats > 1.
      At: float
      /// Where it escaped from: which of the boundary's handlers caught it.
      Source: string
      Message: string
      /// The JavaScript stack, or "" for a thrown value that carries none.
      Stack: string
      /// How many times in a row this same exception has been seen.
      ///
      /// A render that throws throws again on the next frame, and a mouse handler that throws
      /// throws on every mouse move: without this, ten slots hold ten copies of one bad frame
      /// and the exception that caused it is gone. Counted rather than stored.
      Repeats: int }

/// How many distinct exceptions are kept. Small on purpose: what is worth having is the first
/// one, which is usually the cause, and the last few, which are usually consequences of it.
[<Literal>]
let private maxExceptions = 10

/// Newest first, at most maxExceptions long. A list rather than a ring because it is ten items
/// long, is rebuilt only when something has gone wrong, and is read in the order it is stored.
let mutable private uncaught: UncaughtException list = []

/// Record an exception that escaped. Called by the renderer's exception boundary and by nothing
/// else - the arguments are strings so that this stays free of any knowledge of what a thrown
/// JavaScript value looks like, which is the boundary's business.
///
/// A repeat of the exception already at the head bumps its count and is not logged again: the
/// case this exists for is one that fires every frame, where logging each one would itself be
/// the thing that made the app unusable.
let recordException (source: string) (message: string) (stack: string) =
    match uncaught with
    | last :: rest when last.Source = source && last.Message = message ->
        uncaught <- { last with At = uptime (); Repeats = last.Repeats + 1 } :: rest
    | _ ->
        uncaught <-
            { At = uptime (); Source = source; Message = message; Stack = stack; Repeats = 1 }
            :: List.truncate (maxExceptions - 1) uncaught
        // one line, without the stack: the stack is in the record above, and this line is here so
        // that the problem ring says when the exception happened relative to everything else
        error $"uncaught exception in {source}: {message}"

/// The exceptions that have escaped this session, newest first.
let recentExceptions () = uncaught

/// Everything that has gone wrong this session, as the text of a bug report.
///
/// One blob rather than two, because it is written to be pasted somewhere by somebody who is
/// already annoyed: the exceptions first, since they are the part that should not exist at all,
/// and the errors and warnings after them as the run-up to whatever happened.
let problemReport () =
    let exceptionText =
        match recentExceptions () with
        | [] -> [ "(no uncaught exceptions - good)" ]
        | list ->
            list
            |> List.map (fun e ->
                let repeats = if e.Repeats > 1 then $" (x{e.Repeats})" else ""
                let stack = if e.Stack = "" then "  (no stack)" else e.Stack
                $"[%7.3f{e.At}] {e.Source}{repeats}: {e.Message}\n{stack}")

    let problemText =
        match recentProblems () with
        | [||] -> [ "(nothing)" ]
        | lines -> List.ofArray lines

    [ "=== uncaught exceptions, newest first ==="
      yield! exceptionText
      ""
      "=== errors and warnings, oldest first ==="
      yield! problemText ]
    |> String.concat "\n"

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

/// The names of the categories currently on, or "off". The inverse of maskOfNames, for a bug
/// report: what was being logged decides what the lines in it can be expected to say.
let namesOfMask (mask: int) =
    match names |> List.filter (fun (n, bit) -> n <> "all" && mask &&& bit <> 0) with
    | [] -> "off"
    | on -> on |> List.map fst |> String.concat ","

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
///     window.issieLog.problems()       just the errors and warnings, oldest first
///     window.issieLog.report()         those and the uncaught exceptions, as bug report text
///     window.issieLog.on("wire,sim")   turn categories on, live
///     window.issieLog.off()
///
/// This is what lets scripts/inspect-canvas.js read a run's log without DevTools, and what lets
/// a category be turned on in a build somebody else is running. Published in every build for that
/// second reason: a debug-only hook is no use on the machine where the problem happens.
let publish () =
#if FABLE_COMPILER
    Browser.Dom.window?issieLog <-
        {| lines = recentLines
           problems = recentProblems
           report = problemReport
           on = fun (spec: string) -> setCategories (maskOfNames spec); $"logging: {spec}"
           off = fun () -> setCategories 0; "logging off" |}
#endif
    ()
