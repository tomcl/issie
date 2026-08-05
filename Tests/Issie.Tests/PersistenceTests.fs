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

        // A folder picker draws every folder alike, so the folder somebody chooses is as likely to
        // be the one their projects live in as a project. This is what turns that near miss into
        // the list of projects to pick from, rather than a refusal.
        test "the projects inside a folder are found, and nothing else is" {
            withTempDir (fun folder ->
                let sub name =
                    let p = System.IO.Path.Combine(folder, name)
                    System.IO.Directory.CreateDirectory p |> ignore
                    p
                let marked = sub "adder"
                touch marked "adder.dprj"
                touch marked "main.dgm"
                let unmarked = sub "orphan"
                touch unmarked "main.dgm"
                sub "notes" |> ignore                   // an ordinary folder
                touch (sub "empty") ".keep"             // a folder with nothing Issie wants
                touch folder "loose.dgm"                // a file, not a folder, and not recursed into

                let found = FilesIO.projectsWithin folder
                Expect.equal (found |> List.map (fst >> System.IO.Path.GetFileName)) ["adder"; "orphan"]
                    "both openable folders are offered, in name order, and nothing else is"
                Expect.equal (found |> List.map snd) [FilesIO.IsProject; FilesIO.SheetsButNoMarker]
                    "and each is labelled with what it is, since one of them lacks its marker")
        }

        test "a folder with nothing openable inside offers nothing" {
            withTempDir (fun folder ->
                System.IO.Directory.CreateDirectory(System.IO.Path.Combine(folder, "notes")) |> ignore
                Expect.isEmpty (FilesIO.projectsWithin folder) "an ordinary folder is not offered")
        }

        test "a project's marker is named after its folder" {
            let path = FilesIO.projectMarkerPath (FilesIO.pathJoin [| "some"; "where"; "adder" |])
            Expect.equal (FilesIO.baseName path) "adder.dprj" "the marker takes the folder's name"
        }

        test "stateToJsonString round-trips a canvas through jsonStringToState" {
            let inComp = makeComp "in0" 0 1 (Input1(3, None)) "I0"
            let dut = makeComp "not" 1 1 (NbitsNot 3) "N1"
            let outComp = makeComp "out0" 1 0 (Output 3) "O0"
            let canvas: CanvasState =
                [ inComp; dut; outComp ], [ conn inComp 0 dut 0; conn dut 0 outComp 0 ]
            match stateToJsonString (canvas, None, Some sheetInfo) with
            | Error e -> failtest $"serialisation failed: {e}"
            | Ok json ->
                match jsonStringToState json with
                | Error e -> failtest $"parse of freshly saved JSON failed: {e}"
                | Ok saved ->
                    let jsonComps, conns = saved.getCanvas
                    let comps = List.map convertFromJSONComponent jsonComps
                    Expect.equal (List.length conns) 2 "connection count"
                    Expect.equal
                        (comps |> List.map (fun c -> c.Id, c.Type) |> List.sort)
                        ([ "in0", Input1(3, None); "not", NbitsNot 3; "out0", Output 3 ] |> List.sort)
                        "component ids and types survive the round trip"
        }
    ]
