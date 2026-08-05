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

                let listed = FilesIO.listFolderForOpening folder
                Expect.equal (listed |> List.map (fun e -> System.IO.Path.GetFileName e.Path))
                    ["orphan"; "zebra"; "notes"]
                    "openable folders first in name order, then ordinary ones; hidden and files omitted"
                Expect.equal (listed |> List.map (fun e -> e.Kind))
                    [FilesIO.SheetsButNoMarker; FilesIO.IsProject; FilesIO.NotAProject]
                    "each says what it is, so the browser can mark it"
                Expect.equal (listed |> List.map (fun e -> e.SheetCount)) [1; 2; 0]
                    "and how many sheets it holds, which is what tells two projects apart")
        }

        test "a folder holding nothing lists nothing" {
            withTempDir (fun folder ->
                Expect.isEmpty (FilesIO.listFolderForOpening folder) "an empty folder lists nothing")
        }

        test "a path that is not a folder lists nothing rather than failing" {
            withTempDir (fun folder ->
                touch folder "a.dgm"
                let notAFolder = System.IO.Path.Combine(folder, "a.dgm")
                Expect.isEmpty (FilesIO.listFolderForOpening notAFolder) "a file has nothing inside it"
                Expect.isEmpty (FilesIO.listFolderForOpening (System.IO.Path.Combine(folder, "nope")))
                    "nor does a path that is not there")
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
