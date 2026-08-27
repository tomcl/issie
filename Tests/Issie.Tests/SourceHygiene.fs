/// Source hygiene: what the F# compiler cannot check about the source itself.
///
/// This exists because 239 ad-hoc printf calls accumulated over the years - 219 of them
/// unconditional, so a released Issie narrated a symbol drag to a console nobody was reading -
/// alongside 297 commented-out ones whose presence is what made the live ones look normal. That
/// was cleaned up once. Without something that fails, it comes back: there is no lint here, and
/// the compiler is happy either way.
///
/// It also holds the one packaging invariant nothing else can state: which node_modules the
/// packaged app carries. That is spread across a webpack config and package.json, in two
/// different languages, and it only breaks in a packaged build.
module SourceHygiene

open Expecto
open System.IO

/// The repo root, and the src directory below it, reached the way VerilogCompiler.fs reaches its
/// grammar.
let private repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let private srcDir = Path.Combine(repoRoot, "src")

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

// ---------------------------------------------------------------------------------------------
// Packaging: what the main process requires at run time, against what the package ships.
//
// The main bundle is the only one with externals. A module named there is deliberately NOT
// compiled in - it is `require`d from node_modules while the app runs, which is the only way to
// load a native module. package.json's `build.files` excludes node_modules wholesale and
// re-includes just those, because webpack has already bundled everything else and shipping it
// again cost 1171 files.
//
// The trap is that the two halves disagree silently and asymmetrically: in dev the whole
// node_modules tree is on disk, so a missing re-include changes nothing. It is absent only from a
// packaged app, and only when someone reaches that code path. Neither compiler sees both files.

/// The body of `externals: { ... }`, found by counting braces from the first one so that a nested
/// object would not end the block early.
let private externalsBlock (source: string) =
    let opening = System.Text.RegularExpressions.Regex.Match(source, @"externals\s*:\s*\{")
    if not opening.Success then
        None
    else
        let start = opening.Index + opening.Length
        let rec scan i depth =
            if i >= source.Length then None
            elif source[i] = '{' then scan (i + 1) (depth + 1)
            elif source[i] = '}' then (if depth = 0 then Some i else scan (i + 1) (depth - 1))
            else scan (i + 1) depth
        scan start 0 |> Option.map (fun stop -> source[start .. stop - 1])

/// The names in that block: the left of each `name:`, quoted or bare. Comments are dropped first,
/// so the prose explaining why `usb` is there does not read as another external.
let private externalNames (source: string) =
    match externalsBlock source with
    | None -> []
    | Some block ->
        block.Split '\n'
        |> Array.choose (fun line ->
            let m =
                System.Text.RegularExpressions.Regex.Match(
                    code line, @"^\s*[""']?([@\w./-]+)[""']?\s*:")
            if m.Success then Some m.Groups[1].Value else None)
        |> List.ofArray

let private mainExternals () =
    externalNames (File.ReadAllText(Path.Combine(repoRoot, "webpack.config.main.js")))

let private packagedFiles () =
    use doc = System.Text.Json.JsonDocument.Parse(File.ReadAllText(Path.Combine(repoRoot, "package.json")))
    doc.RootElement.GetProperty("build").GetProperty("files").EnumerateArray()
    |> Seq.map (fun entry -> entry.GetString())
    |> List.ofSeq

/// Shipped if some include - not an exclusion - reaches into that package. Deliberately looser
/// than one exact pattern: `node_modules/usb/**` and `node_modules/usb/**/*` both ship it, and a
/// check that failed on the difference is one people would learn to override.
let private shipped (files: string list) (name: string) =
    files
    |> List.exists (fun pattern ->
        not (pattern.StartsWith "!") && pattern.StartsWith $"node_modules/{name}/")

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

        test "every main-process external is shipped in the package" {
            let externals = mainExternals ()
            // A regex that quietly stopped matching would leave nothing to check, and the
            // assertion below would pass for the wrong reason for as long as anyone looked.
            Expect.isNonEmpty externals
                ("no externals found in webpack.config.main.js - if the block moved or changed "
                 + "shape, this test is no longer reading it and is checking nothing")
            let files = packagedFiles ()
            let missing = externals |> List.filter (shipped files >> not)
            Expect.isEmpty missing
                ("webpack.config.main.js requires these from node_modules while the app runs, but "
                 + "package.json's build.files does not ship them - so they are there in dev, "
                 + "where the whole tree is on disk, and gone from a packaged app. Add "
                 + "\"node_modules/<name>/**/*\" to build.files for each, and for any runtime "
                 + "dependency of theirs, which this test cannot work out: "
                 + String.concat ", " missing)
        }

        // Without the exclusion the includes beside it are decoration - electron-builder ships
        // every production dependency anyway, the test above passes on a package that never
        // needed it, and the only symptom is 13.6MB nobody looks at.
        test "the package still excludes node_modules wholesale" {
            Expect.contains (packagedFiles ()) "!node_modules/**/*"
                ("build.files must keep excluding node_modules and re-include only what the main "
                 + "process requires at run time; webpack bundles the rest")
        }
    ]
