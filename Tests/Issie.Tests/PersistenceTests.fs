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

/// A two-level design: TOP holds an input, an instance of MID labelled M1, and a RAM labelled R1;
/// MID holds a NOT gate labelled N1. So TOP/M1/N1 is a two-label path and TOP/R1 a one-label one.
///
/// Component ids deliberately COLLIDE between the sheets - both use 1 and 2 - which is what makes
/// an id-named selection ambiguous and a label-named one not.
let private nested () =
    let inner = makeComp 1 1 1 (NbitsNot 3) "N1"
    let mid = makeLdc "MID" None ([ inner ], [])
    let topIn = makeComp 1 0 1 (Input1(3, None)) "I0"
    let instance = makeComp 2 1 1 (customOf mid [ "N1", 3 ] [ "N1", 3 ] None) "M1"
    let ram =
        makeComp
            3 1 1
            (RAM1 { Init = FromData; AddressWidth = 2; WordWidth = 3; Data = Map.empty; Comments = None })
            "R1"
    let top = makeLdc "TOP" None ([ topIn; instance; ram ], [])
    [ top; mid ]

//---------------------------------------------------------------------------------------------//
// Whether an id survives being written and read back
//
// The whole point of ids being integers written into the file: a project saved and loaded with no
// edit in between must come back with the ids it had. That is what lets anything - a wave
// selection, a RAM the user picked, an error highlighted on the canvas - name a component by id
// and still mean it after a reload. It failed for the uuid form, which is why WavePath names
// signals by label instead.
//---------------------------------------------------------------------------------------------//

let private demo name =
    System.IO.Path.GetFullPath(
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "demos", name))

let private loadFrom (folder: string) =
    match FilesIO.loadAllComponentFiles folder with
    | Error msg -> failtest msg
    | Ok statuses ->
        statuses
        |> List.map (function
            | FilesIO.OkComp ldc
            | FilesIO.OkAuto ldc
            | FilesIO.Resolve(ldc, _) -> ldc)
        |> Helpers.RegenerateIds.admitDesign

/// every id a sheet holds, in one comparable shape
let private idsOf (ldcs: LoadedComponent list) =
    ldcs
    |> List.map (fun ldc ->
        let comps, conns = ldc.CanvasState
        ldc.Name,
        (comps |> List.map (fun c -> cToInt c.Id) |> List.sort),
        (comps
         |> List.collect (fun c -> c.InputPorts @ c.OutputPorts)
         |> List.map (fun p -> pToInt p.Id)
         |> List.sort),
        (conns |> List.map (fun (c: Connection) -> let (ConnectionId n) = c.Id in n) |> List.sort))
    |> List.sortBy (fun (n, _, _, _) -> n)

/// save every sheet the way the app saves one
let private saveAll (folder: string) (ldcs: LoadedComponent list) =
    ldcs
    |> List.iter (fun ldc ->
        let sheetInfo: SheetInfo =
            { Form = ldc.Form
              Description = ldc.Description
              ParameterDefinitions = ldc.LCParameterSlots
              IsTopSheet = Some ldc.IsTopSheet }

        FilesIO.saveStateToFile folder ldc.Name (ldc.CanvasState, ldc.WaveInfo, Some sheetInfo)
        |> function
            | Ok() -> ()
            | Error e -> failtest e)

let tests =
    testList "Persistence" [

        test "a new-form project keeps every id through save and load" {
            let folder =
                System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-ids-{System.Guid.NewGuid()}")
            System.IO.Directory.CreateDirectory folder |> ignore

            try
                System.IO.Directory.GetFiles(demo "3cpu", "*.dgm")
                |> Array.iter (fun p ->
                    System.IO.File.Copy(p, System.IO.Path.Combine(folder, System.IO.Path.GetFileName p)))

                let first, changedOnFirst = loadFrom folder
                printfn "  first load renumbered: %A" changedOnFirst

                saveAll folder first
                let second, changedOnSecond = loadFrom folder
                printfn "  second load renumbered: %A" changedOnSecond

                Expect.equal (idsOf second) (idsOf first)
                    "save then load leaves every component, port and connection id as it was"

                saveAll folder second
                let third, changedOnThird = loadFrom folder
                printfn "  third load renumbered: %A" changedOnThird

                Expect.equal (idsOf third) (idsOf first) "and a second round changes nothing either"
                Expect.isEmpty changedOnSecond "no sheet needs renumbering on reload"
                Expect.isEmpty changedOnThird "nor on the round after that"
            finally
                try System.IO.Directory.Delete(folder, true) with _ -> ()
        }

        test "a legacy project is renumbered once, and is stable from then on" {
            let folder =
                System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-legacy-{System.Guid.NewGuid()}")
            System.IO.Directory.CreateDirectory folder |> ignore

            try
                // the fixture copy of 3cpu, still in the uuid form
                let legacy =
                    System.IO.Path.GetFullPath(
                        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "3cpu"))

                System.IO.Directory.GetFiles(legacy, "*.dgm")
                |> Array.iter (fun p ->
                    System.IO.File.Copy(p, System.IO.Path.Combine(folder, System.IO.Path.GetFileName p)))

                let first, _ = loadFrom folder
                Expect.all first (fun ldc -> ldc.LoadedComponentIsOutOfDate)
                    "every sheet came out of an old-form file"

                saveAll folder first
                let second, changedOnSecond = loadFrom folder

                Expect.all second (fun ldc -> not ldc.LoadedComponentIsOutOfDate)
                    "and is written in the new form"
                Expect.equal (idsOf second) (idsOf first)
                    "the ids the conversion chose are the ids that come back"
                Expect.isEmpty changedOnSecond "with nothing left to renumber"

                saveAll folder second
                let third, _ = loadFrom folder
                Expect.equal (idsOf third) (idsOf first) "and they stay put"
            finally
                try System.IO.Directory.Delete(folder, true) with _ -> ()
        }

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
            // written the way the list stores them, which is this platform's separator: the list
            // normalises so that one folder is one entry however its path was spelled
            let paths =
                [1..limit + 3] |> List.map (fun i -> FilesIO.normalisePath $"/projects/p{i}")
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
                    let comps =
                        List.map (convertFromJSONComponent (int >> ComponentId) (int >> PortId)) jsonComps
                    Expect.equal (List.length conns) 2 "connection count"
                    Expect.equal
                        (comps |> List.map (fun c -> c.Id, c.Type) |> List.sort)
                        ([ ComponentId 1, Input1(3, None); ComponentId 2, NbitsNot 3; ComponentId 3, Output 3 ]
                         |> List.sort)
                        "component ids and types survive the round trip"
        }

        test "a wave selection saved as a label path resolves back to the same signal" {
            let ldcs = nested ()
            // the NOT gate's output, inside the MID instance labelled M1
            let signal: WaveIndexT =
                { SimArrayIndex = DriverIndex 17   // a build number, which must not survive
                  Id = ComponentId 1, [ ComponentId 2 ]
                  PortType = PortType.Output
                  PortNumber = 0 }

            match WavePath.pathOfSignal ldcs "TOP" signal with
            | None -> failtest "a signal of the design did not name a path"
            | Some path ->
                Expect.equal path.WPLabels [ "M1"; "N1" ] "the instance's label, then the component's"
                match WavePath.signalOfPath ldcs "TOP" path with
                | None -> failtest "the path did not resolve back"
                | Some back ->
                    Expect.equal back.Id signal.Id "component and access path come back"
                    Expect.equal back.PortType signal.PortType "port type comes back"
                    Expect.equal back.PortNumber signal.PortNumber "port number comes back"
                    Expect.equal back.SimArrayIndex (DriverIndex -1)
                        "the build's array index must NOT come back: there is no build here"
        }

        test "a component on the simulated sheet has a one-label path" {
            let ldcs = nested ()
            let ramId: FComponentId = ComponentId 3, []
            Expect.equal (WavePath.pathOfComponent ldcs "TOP" ramId) (Some [ "R1" ])
                "a top-level component is named by its own label alone"
            Expect.equal (WavePath.componentOfPath ldcs "TOP" [ "R1" ]) (Some ramId)
                "and resolves back to itself"
        }

        // The reason the selection is saved this way: it has to survive the design being edited
        // between saves, and be dropped rather than silently point somewhere else when it cannot.
        test "a renamed or deleted component drops its saved wave instead of resolving elsewhere" {
            let ldcs = nested ()
            let renamed =
                ldcs
                |> List.map (fun ldc ->
                    if ldc.Name <> "MID" then ldc
                    else
                        let comps, conns = ldc.CanvasState
                        { ldc with
                            CanvasState = comps |> List.map (fun c -> { c with Label = "N2" }), conns })

            let path: WavePath =
                { WPLabels = [ "M1"; "N1" ]; WPPortType = PortType.Output; WPPortNumber = 0 }
            Expect.isSome (WavePath.signalOfPath ldcs "TOP" path) "resolves against the design it names"
            Expect.isNone (WavePath.signalOfPath renamed "TOP" path)
                "and against a design where the component was renamed, resolves to nothing"

            // a path that leaves the design part way down, and one that never enters it
            Expect.isNone (WavePath.signalOfPath ldcs "TOP" { path with WPLabels = [ "M9"; "N1" ] })
                "an instance the design no longer holds"
            Expect.isNone (WavePath.signalOfPath ldcs "TOP" { path with WPLabels = [] })
                "and a path naming no component at all"
            // I0 is a component, not a custom one, so nothing lies inside it
            Expect.isNone (WavePath.signalOfPath ldcs "TOP" { path with WPLabels = [ "I0"; "N1" ] })
                "a path descending through a component that is not a sheet instance"
        }

        // The saved selection changed shape with no legacy reader, which is only safe because a
        // file this version cannot parse loses its SELECTION and keeps its SHEET. This is the
        // real legacy data: alu.dgm holds a selection of uuid-named waves written years ago.
        //
        // The demo-loading test above covers the same fallback across every shipped project; this
        // one names the case, so that the day it starts failing says what broke.
        test "a sheet holding a legacy wave selection loads its canvas without it" {
            // The fixture copy, not the shipped demo: the demos are written in the current form
            // now, so the old shape survives only where it is KEPT on purpose, which is here.
            let alu =
                System.IO.Path.GetFullPath(
                    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "3cpu", "alu.dgm"))
            let json = System.IO.File.ReadAllText alu
            Expect.stringContains json "\"SimArrayIndex\""
                "alu.dgm is meant to hold a selection in the old shape - pick another sheet if it no longer does"
            match jsonStringToState json with
            | Error e -> failtest $"a sheet with unreadable wave info failed to load: {e}"
            | Ok saved ->
                Expect.isNone saved.getWaveInfo "the unreadable selection is dropped"
                Expect.isNonEmpty (fst saved.getCanvas) "and the canvas is kept"
        }

        //-----------------------------------------------------------------------------------//
        // Putting a project's files into the current id form
        //-----------------------------------------------------------------------------------//

        // The fixture projects are deliberately in the OLD form - uuids, as written before ids
        // were integers - which is what makes them the corpus for this. They are copied out
        // before being converted, so the corpus survives the test.
        test "a project written before ids were integers is converted when it is opened" {
            withTempDir (fun folder ->
                let source =
                    System.IO.Path.GetFullPath(
                        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "1fulladder"))

                System.IO.Directory.GetFiles(source, "*.dgm")
                |> Array.iter (fun path ->
                    System.IO.File.Copy(path, System.IO.Path.Combine(folder, System.IO.Path.GetFileName path)))

                let load () =
                    match FilesIO.loadAllComponentFiles folder with
                    | Error msg -> failtest msg
                    | Ok statuses ->
                        statuses
                        |> List.map (function
                            | FilesIO.OkComp ldc | FilesIO.OkAuto ldc | FilesIO.Resolve(ldc, _) -> ldc)
                        |> Helpers.RegenerateIds.admitDesign
                        |> fst

                let loaded = load ()
                Expect.isNonEmpty loaded "the fixture project has sheets"
                Expect.all loaded (fun ldc -> ldc.LoadedComponentIsOutOfDate)
                    "every sheet came out of a file whose ids are in the old form"

                let converted = MenuHelpers.convertProjectIdsOnDisk loaded
                Expect.all converted (fun ldc -> not ldc.LoadedComponentIsOutOfDate)
                    "and once written none of them is waiting to be written"

                let reloaded = load ()

                Expect.all reloaded (fun ldc -> not ldc.LoadedComponentIsOutOfDate)
                    "reading the files back finds ids that need no converting"

                // the point of the whole thing: what the file says is now what the app holds
                let idsOf (ldcs: LoadedComponent list) =
                    ldcs
                    |> List.map (fun ldc -> ldc.Name, fst ldc.CanvasState |> List.map (fun comp -> comp.Id))
                    |> List.sortBy fst

                Expect.equal (idsOf reloaded) (idsOf converted)
                    "the ids on disk are the ones the design was using"

                // Which sheet is newest is what decides the sheet a project opens on, so a
                // rewrite that is not the user's edit must not touch the ranking. The ORDER
                // rather than the stamps themselves: the .NET writer these tests run against
                // normalises the time zone the app's writer keeps, which moves every stamp by
                // the same offset and so is not what this is about.
                let ranked (ldcs: LoadedComponent list) =
                    ldcs |> List.sortBy (fun ldc -> ldc.TimeStamp) |> List.map (fun ldc -> ldc.Name)

                Expect.equal (ranked reloaded) (ranked loaded)
                    "the sheets still rank by when they were last saved as they did before"

                Expect.all reloaded
                    (fun ldc -> ldc.TimeStamp < System.DateTime.Now.AddMinutes -1.0)
                    "and none of them is stamped with the moment of the conversion"

                Expect.equal
                    (reloaded |> List.map (fun ldc -> ldc.Name, ldc.Form) |> List.sortBy fst)
                    (loaded |> List.map (fun ldc -> ldc.Name, ldc.Form) |> List.sortBy fst)
                    "the sheet info is written back as it was read")
        }

        test "a project that cannot be written keeps its old ids" {
            // Nothing is asked in advance about whether a directory can be written - the refusal
            // is the answer - so a project Issie may only read is left exactly as it was, rather
            // than half converted.
            withTempDir (fun folder ->
                let source =
                    System.IO.Path.GetFullPath(
                        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "1fulladder"))

                System.IO.Directory.GetFiles(source, "*.dgm")
                |> Array.iter (fun path ->
                    System.IO.File.Copy(path, System.IO.Path.Combine(folder, System.IO.Path.GetFileName path)))

                let loaded =
                    match FilesIO.loadAllComponentFiles folder with
                    | Error msg -> failtest msg
                    | Ok statuses ->
                        statuses
                        |> List.map (function
                            | FilesIO.OkComp ldc | FilesIO.OkAuto ldc | FilesIO.Resolve(ldc, _) -> ldc)
                    // a directory that is not there stands in for one that may not be written
                    |> List.map (fun ldc ->
                        { ldc with
                            FilePath =
                                System.IO.Path.Combine(folder, "not-a-folder", ldc.Name + ".dgm") })

                let before =
                    System.IO.Directory.GetFiles(folder, "*.dgm")
                    |> Array.map System.IO.File.ReadAllText

                let after = MenuHelpers.convertProjectIdsOnDisk loaded

                Expect.equal
                    (System.IO.Directory.GetFiles(folder, "*.dgm") |> Array.map System.IO.File.ReadAllText)
                    before
                    "the files are exactly as they were - not one of them half converted"
                Expect.isFalse
                    (System.IO.Directory.Exists(System.IO.Path.Combine(folder, "not-a-folder")))
                    "and nothing was created to write into"
                // Not left waiting to be written either: a project that cannot be written would
                // otherwise say it had unsaved changes at every close, about something the user
                // never did and cannot act on.
                Expect.all after
                    (fun ldc -> not ldc.LoadedComponentIsOutOfDate)
                    "and the sheets are not left claiming to be unsaved")
        }
    ]
