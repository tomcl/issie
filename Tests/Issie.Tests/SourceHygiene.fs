/// Source hygiene: what the F# compiler cannot check about the source itself.
///
/// This exists because 239 ad-hoc printf calls accumulated over the years - 219 of them
/// unconditional, so a released Issie narrated a symbol drag to a console nobody was reading -
/// alongside 297 commented-out ones whose presence is what made the live ones look normal. That
/// was cleaned up once. Without something that fails, it comes back: there is no lint here, and
/// the compiler is happy either way.
module SourceHygiene

open Expecto
open System.IO

/// The repo's src directory, reached the way VerilogCompiler.fs reaches its grammar.
let private srcDir =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src"))

/// Files where printing IS the feature: they run only when a developer picks them from the
/// Development menu, and their output is what was asked for. Everything else logs.
let private allowed =
    [ "Playground.fs"                       // the Play menu's own experiments
      "TestParser.fs"                       // the Verilog test runner's report
      "TimeHelpers.fs"                      // checkPerformance and printStats, both menu-invoked
      "Log.fs"                              // the sink itself
      "Main.fs" ]                           // the main process: a separate fsproj that cannot see Log

/// A print, ignoring sprintf (which builds a string and prints nothing) and failwithf/kprintf
/// (which raise or format).
let private printPattern =
    System.Text.RegularExpressions.Regex(@"(^|[^a-zA-Z_.])(printfn|printf)\s*[""$@(]")

/// The code half of a line and its comment half. Splitting on the first "//" would be wrong
/// inside a string literal, but the only cost of that here is a print in such a string going
/// unnoticed - never a false alarm, which is what would make this test worth deleting.
let private code (line: string) =
    match line.IndexOf "//" with
    | -1 -> line
    | i -> line[.. i - 1]

let private comment (line: string) =
    match line.IndexOf "//" with
    | -1 -> ""
    | i -> line[i ..]

/// A live print: one in the code, not in the comment beside it.
let private livePrint (line: string) = printPattern.IsMatch(code line)

/// A commented-out print - a statement, not a mention of one. These are the seed corn: one of
/// them in a file makes the next live one look like it belongs.
///
/// The comment has to *start* with the print, once its slashes and any pipe are taken off, so
/// that "printf %x does not work on bignums" in a doc comment is prose rather than an offence.
/// That leaves out a print buried mid-sentence in a comment, which is a price worth paying: a
/// check with false alarms is one people learn to override.
let private commentedPrint (line: string) =
    System.Text.RegularExpressions.Regex.IsMatch(
        comment line, @"^[/\s]*(\|>\s*)?(printfn|printf|console\.log)\s*[""$@(]")

let private sourceFiles () =
    Directory.GetFiles(srcDir, "*.fs", SearchOption.AllDirectories)
    |> Array.filter (fun path ->
        let parts = path.Split([| '/'; '\\' |])
        not (Array.exists (fun (p: string) -> p = "fable_modules" || p = "obj" || p = "bin") parts)
        // the vendored copy of Fable.SimpleJson is upstream code, patched only where it had to be
        && not (Array.contains "SimpleJson" parts))

/// Offending lines as "file:line: text", so a failure says where to look.
let private offenders (isOffending: string -> bool) =
    sourceFiles ()
    |> Array.filter (fun path -> not (List.contains (Path.GetFileName path) allowed))
    |> Array.collect (fun path ->
        File.ReadAllLines path
        |> Array.mapi (fun i line -> i + 1, line)
        |> Array.filter (snd >> isOffending)
        |> Array.map (fun (n, line) ->
            $"{Path.GetRelativePath(srcDir, path)}:{n}: {line.Trim()}"))

let tests =
    testList "SourceHygiene" [

        test "no printf outside the dev tools that exist to print" {
            let found = offenders livePrint
            Expect.isEmpty found
                ($"Use Log.warn / Log.error for something that must always be seen, Log.dbg with a "
                 + $"category for anything else, and Log.out for output a Development menu item was "
                 + $"asked to produce. See src/Renderer/Common/Log.fs.\n"
                 + String.concat "\n" found)
        }

        test "no commented-out prints" {
            let found = offenders commentedPrint
            Expect.isEmpty found
                ($"A commented-out print is in git already - delete it. If the line documents "
                 + $"something, say it in words.\n"
                 + String.concat "\n" found)
        }

        // The allowlist is a liability if it names files that have gone: the next file with that
        // name silently inherits permission to print.
        test "every allowlisted file exists" {
            let names = sourceFiles () |> Array.map Path.GetFileName |> Set.ofArray
            let missing = allowed |> List.filter (fun name -> not (Set.contains name names))
            Expect.isEmpty missing $"allowlisted files that are no longer in src: {missing}"
        }
    ]
