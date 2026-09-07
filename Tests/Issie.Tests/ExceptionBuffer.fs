/// The buffers behind Info -> Bug Reports: the problem ring and the record of uncaught exceptions.
///
/// These are worth pinning precisely because they are the code that only runs when something has
/// already gone wrong. Nothing exercises them in an ordinary session, so a change that broke them
/// would be found by the next person to ask a user for a bug report - which is the worst possible
/// time to find out that the report is empty.
///
/// The whole of `Log` is process-global mutable state, so these tests share one set of buffers
/// with each other and with anything else in the run that logs. They are written not to care:
/// each one uses its own message text and asks only about its own lines.
module ExceptionBuffer

open Expecto

/// Every line these tests put in the buffers carries this, so that a line from somewhere else in
/// the test run - or from a previous test here - cannot be mistaken for one of them.
///
/// Public because `Main` allows it through the check that fails a run which logged an error:
/// these are the only errors the suite is supposed to produce, and one definition of what they
/// look like is better than the same string written in two places.
let tag = "excbuftest"

let private linesMentioning (what: string) =
    Log.recentProblems () |> Array.filter (fun line -> line.Contains what)

let private exceptionsMentioning (what: string) =
    Log.recentExceptions () |> List.filter (fun e -> e.Message.Contains what)

let tests =
    testList "ExceptionBuffer" [

        // The reason the problem ring exists at all. The main log ring is 400 lines and a single
        // drag with a category on fills it, so an error kept only there is gone by the time
        // anybody looks.
        test "errors and warnings are kept apart from debug output" {
            let mine = $"{tag}-severity"
            // the suite is Sequenced, so switching categories here cannot disturb another test -
            // but put them back, since what the run logs after this is not this test's business
            let wasEnabled = Log.enabled
            Log.setCategories Log.All
            Log.error $"{mine} an error"
            Log.warn $"{mine} a warning"
            Log.out $"{mine} a development-menu result"
            Log.dbg Log.Wire $"{mine} a debug line"
            Log.setCategories wasEnabled

            let problems = linesMentioning mine

            Expect.equal problems.Length 2 "only the error and the warning are problems"
            Expect.isTrue (problems |> Array.exists (fun l -> l.Contains "ERR")) "the error is there"
            Expect.isTrue (problems |> Array.exists (fun l -> l.Contains "WRN")) "and the warning"
            Expect.isTrue
                (Log.recentLines () |> Array.exists (fun l -> l.Contains $"{mine} a debug line"))
                "the debug line is still in the main ring - it is only the problem ring it stays out of"
        }

        // A render that throws throws again on the next frame, and a mouse handler that throws
        // throws on every mouse move. Without collapsing, ten slots hold ten copies of one bad
        // frame and whatever caused it has been pushed out.
        test "the same exception repeated fills one slot and is counted" {
            let mine = $"{tag}-repeat"
            for _ in 1..5 do
                Log.recordException "view" mine "a stack"

            let mineOnly = exceptionsMentioning mine

            Expect.equal mineOnly.Length 1 "five throws, one slot"
            Expect.equal mineOnly.Head.Repeats 5 "and the count says how many"
            Expect.equal (linesMentioning mine).Length 1 "logged once, not five times"
        }

        // Collapsing is only for a repeat of the exception at the HEAD. Two different failures
        // alternating are two failures, and both are worth keeping.
        test "a different exception in between is not collapsed into it" {
            let mine = $"{tag}-alternating"
            Log.recordException "view" $"{mine} one" ""
            Log.recordException "view" $"{mine} two" ""
            Log.recordException "view" $"{mine} one" ""

            let mineOnly = exceptionsMentioning mine

            Expect.equal mineOnly.Length 3 "three entries, because none repeats the one before it"
            Expect.isTrue (mineOnly |> List.forall (fun e -> e.Repeats = 1)) "none of them collapsed"
        }

        // The same message from two different places is two different problems - a failure in the
        // view and a failure in an update that happen to say the same thing are not one event.
        test "the source is part of what makes an exception the same one" {
            let mine = $"{tag}-source"
            Log.recordException "view" mine ""
            Log.recordException "update" mine ""

            Expect.equal (exceptionsMentioning mine).Length 2 "same message, different source, two entries"
        }

        test "only the last ten are kept, newest first" {
            let mine = $"{tag}-cap"
            for i in 1..14 do
                Log.recordException "window" $"{mine} {i}" ""

            let all = Log.recentExceptions ()

            Expect.equal all.Length 10 "the buffer holds ten"
            Expect.stringContains all.Head.Message $"{mine} 14" "newest first"
            Expect.isFalse
                (all |> List.exists (fun e -> e.Message.Contains $"{mine} 4 "))
                "and the oldest have been dropped"
        }

        // **The one real stack trace in Issie has to survive the wire to be worth having.**
        // The sidecar is .NET, so its exceptions carry a trace where a renderer one does not;
        // it is sent inside the single-key JSON object an error reply carries, and the escaper
        // that puts it there is hand-rolled. A trace is precisely the input that breaks a
        // careless one: backslashes in every Windows path, and a newline between every frame.
        test "a stack trace survives the sidecar error envelope" {
            // a verbatim string: a real trace is full of Windows paths, which is the point
            let trace =
                @"System.Exception: what? expected a RAM
   at Issie.SimSession.readRam(Int32 cid) in C:\GitHub\issie\src\Sidecar\SimSession.fs:line 42
   at Issie.Program.serve@300.MoveNext()"

            let payload = $"""{{"error":"{Issie.Sidecar.Protocol.jsonSafe trace}"}}"""

            // parsed the way the renderer parses it, so this asks the real question: is what
            // comes out the other side the trace that went in?
            let recovered =
                System.Text.Json.JsonDocument.Parse(payload).RootElement.GetProperty("error").GetString()

            Expect.equal recovered trace "the trace arrives exactly as it was raised"
            Expect.stringContains recovered "SimSession.fs:line 42" "with its paths unmangled"
            Expect.isTrue (recovered.Split('\n').Length >= 3) "and still on separate lines"
        }

        // What the Copy button puts on the clipboard. The exceptions come first because they are
        // the part that should not exist at all.
        test "the report carries both halves" {
            let mine = $"{tag}-report"
            Log.warn $"{mine} a warning"
            Log.recordException "promise" $"{mine} an exception" "a stack"

            let report = Log.problemReport ()
            let exceptionsAt = report.IndexOf "=== uncaught exceptions"
            let problemsAt = report.IndexOf "=== errors and warnings"

            Expect.isGreaterThan exceptionsAt -1 "the exceptions are in it"
            Expect.isGreaterThan problemsAt exceptionsAt "and come before the errors and warnings"
            Expect.stringContains report $"{mine} an exception" "the exception's message is there"
            Expect.stringContains report "a stack" "with its stack"
            Expect.stringContains report $"{mine} a warning" "and so is the warning"
        }
    ]
