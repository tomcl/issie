/// Persistence tests: the .dgm save path must produce JSON that the load path can
/// read back, and a serialisation failure must surface as an Error rather than as
/// placeholder text that would overwrite the sheet file.
module PersistenceTests

open Expecto
open CommonTypes
open Helpers.JsonHelpers
open CanvasBuilder

let private sheetInfo: SheetInfo =
    { Form = Some User; Description = None; ParameterDefinitions = None; IsTopSheet = Some false }

/// Run `body` against a fresh empty directory, which is removed afterwards.
let private withTempDir (body: string -> unit) =
    let folder =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-proj-{System.Guid.NewGuid()}")
    System.IO.Directory.CreateDirectory folder |> ignore
    try body folder
    finally try System.IO.Directory.Delete(folder, true) with _ -> ()

let private touch (folder: string) (name: string) =
    System.IO.File.WriteAllText(System.IO.Path.Combine(folder, name), "")

let tests =
    testList "Persistence" [

        // Whether the open sheet differs from the saved one, which is what decides that it needs
        // saving or backing up. Rerouting a wire changes how many segments it has, so comparing
        // vertex lists of different lengths is the ordinary case.
        test "a rerouted wire compares as a change rather than raising" {
            let a = makeComp 1 0 1 (Input1(1, None)) "A"
            let b = makeComp 2 1 0 (Output 1) "B"
            let routedAs (vs: (float * float * bool) list) : CanvasState =
                [ a; b ], [ { conn a 0 b 0 with Vertices = vs } ]
            let threeSegments = routedAs [ 0., 0., false; 10., 0., false; 10., 10., false ]
            let fourSegments = routedAs [ 0., 0., false; 5., 0., false; 5., 10., false; 10., 10., false ]
            Expect.isFalse (CanvasExtractor.compareCanvas 100. threeSegments fourSegments)
                "a wire routed with a different number of segments is a change"
            Expect.isTrue (CanvasExtractor.compareCanvas 100. threeSegments threeSegments)
                "and an unchanged sheet is not"
            // a connection carrying no vertices at all has no position to measure the whole-sheet
            // offset from, which is not a reason to fail
            let noVertices = routedAs []
            Expect.isFalse (CanvasExtractor.compareCanvas 100. noVertices threeSegments)
                "a wire with no vertices differs from a routed one"
            Expect.isTrue (CanvasExtractor.compareCanvas 100. noVertices noVertices)
                "and two of them are the same"
        }

        // What the New Project form asks of every keystroke, so that a name is refused while the
        // user is still typing it rather than by an error box after the fact.
        test "a project name is refused for exactly the characters it may not hold" {
            let refused name =
                Expect.isSome (FilesIO.projectNameError name) $"'{name}' should not be a project name"
            let accepted name =
                Expect.isNone (FilesIO.projectNameError name) $"'{name}' should be a project name"
            accepted "adder"
            accepted "Adder_4bit"
            accepted "cpu2"
            refused ""            // nothing typed is not a name, and Create must stay disabled
            refused "my adder"    // spaces
            refused "adder-4bit"  // hyphens: the message calls both out by name
            refused "adder.dgm"
            refused "../escape"
        }

        // A project is a directory, so opening one means judging a directory. Both the open path
        // and the "would this new project be inside an existing one?" check read this.
        test "a directory is told to be a project, sheets without a marker, or neither" {
            withTempDir (fun folder ->
                Expect.equal (FilesIO.inspectProjectDirectory folder) FilesIO.NotAProject
                    "an empty folder holds no project"

                touch folder "main.dgm"
                Expect.equal (FilesIO.inspectProjectDirectory folder) FilesIO.SheetsButNoMarker
                    "sheets alone are loadable, but nothing says the folder is a project"

                touch folder (System.IO.Path.GetFileName folder + ".dprj")
                Expect.equal (FilesIO.inspectProjectDirectory folder) FilesIO.IsProject
                    "the marker is what makes it a project")
        }

        test "a folder holding only the marker still counts as a project" {
            // an emptied project is still a project: it has a marker, and Issie must not offer to
            // create another one inside it
            withTempDir (fun folder ->
                touch folder "anything.dprj"
                Expect.equal (FilesIO.inspectProjectDirectory folder) FilesIO.IsProject
                    "the marker alone is enough")
        }

        // What the project browser draws. A native folder picker cannot say which folders are
        // projects, which is the whole reason Issie lists them itself.
        test "a folder is listed with what each thing in it is, projects first" {
            withTempDir (fun folder ->
                let sub name =
                    let p = System.IO.Path.Combine(folder, name)
                    System.IO.Directory.CreateDirectory p |> ignore
                    p
                let marked = sub "zebra"                // named last, to prove projects sort first
                touch marked "zebra.dprj"
                touch marked "main.dgm"
                touch marked "alu.dgm"
                let unmarked = sub "orphan"
                touch unmarked "main.dgm"
                sub "notes" |> ignore                   // an ordinary folder: navigable, so listed
                sub ".hidden" |> ignore                 // hidden: nobody keeps projects there
                touch folder "loose.dgm"                // a file, so not a folder in the listing

                let listed =
                    FilesIO.browseFolderForOpening folder
                    |> function
                        | Ok entries -> entries
                        | Error msg -> failtest $"a folder that is there should list: {msg}"
                Expect.equal (listed |> List.map (fun e -> System.IO.Path.GetFileName e.Path))
                    ["orphan"; "zebra"; "notes"]
                    "openable folders first in name order, then ordinary ones; hidden and files omitted"
                Expect.equal (listed |> List.map (fun e -> e.Kind))
                    [FilesIO.SheetsButNoMarker; FilesIO.IsProject; FilesIO.NotAProject]
                    "each says what it is, so the browser can mark it"
                Expect.equal (listed |> List.map (fun e -> e.SheetCount)) [1; 2; 0]
                    "and how many sheets it holds, which is what tells two projects apart")
        }

        test "a folder holding nothing lists nothing, and says so as an empty listing" {
            // An empty folder and a folder that cannot be listed are different things to be told:
            // the browser draws "Nothing in this folder" for one and the reason for the other, and
            // reporting a folder Issie could not read as empty is what this distinguishes.
            withTempDir (fun folder ->
                match FilesIO.browseFolderForOpening folder with
                | Ok entries -> Expect.isEmpty entries "an empty folder lists nothing"
                | Error msg -> failtest $"an empty folder is still a folder: {msg}")
        }

        test "a path that is not a folder is an error rather than an empty listing" {
            withTempDir (fun folder ->
                touch folder "a.dgm"
                let notAFolder = System.IO.Path.Combine(folder, "a.dgm")
                Expect.isError (FilesIO.browseFolderForOpening notAFolder)
                    "a file is not somewhere to browse"
                Expect.isError (FilesIO.browseFolderForOpening (System.IO.Path.Combine(folder, "nope")))
                    "nor is a path that is not there")
        }

        // The recent projects list is user data that survives restarts, so a list one too long, or
        // one holding the same project twice, is a fault that outlives the session that caused it.
        test "opening a project puts it at the top of the recents, once, within the limit" {
            let limit = MenuHelpers.Constants.numberOfRecentProjects
            let paths = [1..limit + 3] |> List.map (fun i -> $"/projects/p{i}")
            let recents = paths |> List.fold (fun acc path -> MenuHelpers.addToRecents path acc) None
            match recents with
            | None -> failtest "opening a project must produce a list"
            | Some recents ->
                Expect.equal (List.length recents) limit
                    "the list is capped: it used to keep one more than the limit, since it made \
                     room before adding rather than after"
                Expect.equal (List.head recents) (List.last paths) "newest first"
                Expect.equal recents (List.distinct recents) "no project appears twice"
                // reopening the oldest one still listed moves it up rather than duplicating it
                let reopened = MenuHelpers.addToRecents (List.last recents) (Some recents)
                Expect.equal (Option.map List.length reopened) (Some limit) "still capped"
                Expect.equal (Option.map List.head reopened) (Some (List.last recents))
                    "the reopened project is now the newest"
        }

        test "a project's marker is named after its folder" {
            let path = FilesIO.projectMarkerPath (FilesIO.pathJoin [| "some"; "where"; "adder" |])
            Expect.equal (FilesIO.baseName path) "adder.dprj" "the marker takes the folder's name"
        }

        // Every .dgm that exists was written by the app, that is by Fable.SimpleJson, whose
        // encoding of unions and options Thoth cannot read - so for a long time no sheet at all
        // could be opened outside Electron. This is what holds that open: the shipped demos, read
        // by the production loader, with nothing running.
        test "FilesIO reads every demo project's sheets under .NET" {
            let demosDir =
                System.IO.Path.GetFullPath(
                    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "demos"))
            let projects = System.IO.Directory.GetDirectories demosDir |> Array.sort
            Expect.isGreaterThan projects.Length 0 $"no demo projects under {demosDir}"
            projects
            |> Array.iter (fun projectPath ->
                let project = System.IO.Path.GetFileName projectPath
                match FilesIO.loadAllComponentFiles projectPath with
                | Error msg -> failtest $"{project}: {msg}"
                | Ok statuses ->
                    Expect.isGreaterThan statuses.Length 0 $"{project} loaded no sheets"
                    statuses
                    |> List.iter (fun status ->
                        let ldc =
                            match status with
                            | FilesIO.OkComp ldc
                            | FilesIO.OkAuto ldc
                            | FilesIO.Resolve(ldc, _) -> ldc
                        // a decoder that quietly produced an empty canvas would pass a mere
                        // "it did not throw" check
                        Expect.isNonEmpty (fst ldc.CanvasState)
                            $"{project}/{ldc.Name} loaded with no components"))
        }

        test "stateToJsonString round-trips a canvas through jsonStringToState" {
            let inComp = makeComp 1 0 1 (Input1(3, None)) "I0"
            let dut = makeComp 2 1 1 (NbitsNot 3) "N1"
            let outComp = makeComp 3 1 0 (Output 3) "O0"
            let canvas: CanvasState =
                [ inComp; dut; outComp ], [ conn inComp 0 dut 0; conn dut 0 outComp 0 ]
            match stateToJsonString (canvas, None, Some sheetInfo) with
            | Error e -> failtest $"serialisation failed: {e}"
            | Ok json ->
                match jsonStringToState json with
                | Error e -> failtest $"parse of freshly saved JSON failed: {e}"
                | Ok saved ->
                    let jsonComps, conns = saved.getCanvas
                    // the freshly saved ids are decimal strings of the int ids, so plain
                    // parsing is the whole mapping back
                    let comps = List.map (convertFromJSONComponent int int) jsonComps
                    Expect.equal (List.length conns) 2 "connection count"
                    Expect.equal
                        (comps |> List.map (fun c -> c.Id, c.Type) |> List.sort)
                        ([ 1, Input1(3, None); 2, NbitsNot 3; 3, Output 3 ] |> List.sort)
                        "component ids and types survive the round trip"
        }
    ]
