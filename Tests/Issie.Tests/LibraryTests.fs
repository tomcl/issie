/// The component library layer: what a library sheet shows of itself.
module LibraryTests

open Expecto
open CommonTypes
open CanvasBuilder
open MenuHelpers

let private ldc (name: string) (form: CCForm) (canvas: CanvasState) : LoadedComponent =
    { Name = name
      LoadedComponentIsOutOfDate = false
      WaveInfo = None
      TimeStamp = System.DateTime.Now
      FilePath = name + ".dgm"
      CanvasState = canvas
      InputLabels = []
      OutputLabels = []
      Form = Some form
      Description = None
      LCParameterSlots = None
      IsTopSheet = false }

/// an instance of sheet `name`, as a Custom component
let private instanceOf (id: int) (name: string) (label: string) =
    { makeComp id 0 0 (Input1(1, None)) label with
        Type =
            Custom
                { Name = name; InputLabels = []; OutputLabels = []
                  Form = None; Description = None; ParameterBindings = None } }

/// top uses a library component, which uses a second library sheet of its own
let private project : Project =
    { ProjectPath = "."
      OpenFileName = "top"
      WorkingFileName = Some "top"
      LoadedComponents = [
          ldc "top" User ([ instanceOf 1 "L1_fullAdd" "FA1" ], [])
          ldc "L1_fullAdd" (Library ("arithmetic", "fullAdd")) ([ instanceOf 1 "L1_halfAdd" "HA1" ], [])
          ldc "L1_halfAdd" (Library ("arithmetic", "halfAdd")) ([], [])
      ] }

let private namesIn (trees: Map<string, SheetTree>) =
    let rec walk (t: SheetTree) = t.SheetName :: List.collect walk t.SubSheets
    trees |> Map.toList |> List.collect (fun (name, tree) -> name :: walk tree) |> Set.ofList


/// Run `body` against a fresh empty directory standing in for a library, removed afterwards.
let private withTempLibrary (body: string -> unit) =
    let folder =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"issie-libproj-{System.Guid.NewGuid()}")
    System.IO.Directory.CreateDirectory folder |> ignore
    try body folder
    finally try System.IO.Directory.Delete(folder, true) with _ -> ()

let private header (name: string) (offered: bool) (requires: string list) : ComponentLibraries.LibraryHeader =
    { FormatVersion = ComponentLibraries.Constants.currentFormatVersion
      Name = name
      Description = $"test component {name}"
      Section = ComponentLibraries.Constants.defaultSection
      OfferedInCatalogue = offered
      Requires = requires }

/// The text of a .dgm holding these components - what an .ldgm carries as its body.
let private bodyOf (name: string) (comps: Component list) =
    let sheetInfo: SheetInfo =
        {Form = Some User; Description = Some $"test component {name}"
         ParameterDefinitions = None; IsTopSheet = Some false}
    match Helpers.JsonHelpers.stateToJsonString ((comps, []), None, Some sheetInfo) with
    | Ok json -> json
    | Error msg -> failwithf "%s" msg

let private writeComponent (libPath: string) (h: ComponentLibraries.LibraryHeader) (body: string) =
    match ComponentLibraries.writeComponentFile libPath h body with
    | Ok () -> ()
    | Error msg -> failwithf "%s" msg

let tests =
    testList "Library" [
        test "hidden library sheets leave no trace in the sheet trees" {
            // The instances have to be skipped during the walk rather than the sheets removed
            // beforehand: dropping a sheet from LoadedComponents still leaves its instance making
            // a node, and that node is a stub with no name and no contents.
            let hidden = getSheetTreesFiltered (fun _ -> false) false project |> namesIn
            Expect.equal hidden (Set.ofList [ "top" ]) "only the user's own sheet survives"
        }

        test "shown library sheets appear as roots and as subsheets" {
            let shown = getSheetTreesFiltered (fun _ -> true) false project |> namesIn
            Expect.equal shown (Set.ofList [ "top"; "L1_fullAdd"; "L1_halfAdd" ])
                "the component and the sheet it uses are both there"
            Expect.equal (getSheetTrees false project |> namesIn) shown
                "getSheetTrees, which everything else uses, shows them"
        }

        // Materialising a library component is "write the body out as a .dgm, then load it with
        // the ordinary loader". Both halves now run with nothing running, which they did not while
        // the .dgm reader was Fable-only - so a shipped component can be checked here rather than
        // by clicking through the catalogue.
        test "every shipped library component's sheet loads under .NET" {
            let librariesDir =
                System.IO.Path.GetFullPath(
                    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "libraries"))
            let components =
                System.IO.Directory.GetFiles(librariesDir, "*.ldgm", System.IO.SearchOption.AllDirectories)
                |> Array.sort
            Expect.isGreaterThan components.Length 0 $"no .ldgm files under {librariesDir}"
            components
            |> Array.iter (fun path ->
                match ComponentLibraries.tryReadComponentFile path with
                | Error msg -> failtest msg
                | Ok(header, body) ->
                    let folder =
                        System.IO.Path.Combine(
                            System.IO.Path.GetTempPath(), $"issie-lib-{System.Guid.NewGuid()}")
                    System.IO.Directory.CreateDirectory folder |> ignore
                    try
                        let dgm = System.IO.Path.Combine(folder, header.Name + ".dgm")
                        System.IO.File.WriteAllText(dgm, body)
                        match FilesIO.tryLoadComponentFromPath dgm with
                        | Error msg -> failtest $"{header.Name}: {msg}"
                        | Ok ldc ->
                            Expect.isNonEmpty (fst ldc.CanvasState) $"{header.Name} has no components"
                    finally
                        try System.IO.Directory.Delete(folder, true) with _ -> ())
        }

        // The catalogue draws a library component being dragged from this, before anything has
        // been written into the project - so what is carried is only the component that lands if
        // the two are worked out from the same sheets. They are checked against the loader here
        // rather than against a recorded answer, since the loader is what the placement uses.
        test "the shape a library component is dragged as is the one it lands with" {
            let librariesDir =
                System.IO.Path.GetFullPath(
                    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "libraries"))
            let components =
                System.IO.Directory.GetFiles(librariesDir, "*.ldgm", System.IO.SearchOption.AllDirectories)
                |> Array.sort
            Expect.isGreaterThan components.Length 0 $"no .ldgm files under {librariesDir}"
            components
            |> Array.iter (fun path ->
                let libPath = System.IO.Path.GetDirectoryName path
                let name = System.IO.Path.GetFileNameWithoutExtension path
                let shape =
                    match ComponentLibraries.tryReadComponentShape libPath name with
                    | Error msg -> failtest $"{name}: {msg}"
                    | Ok shape -> shape
                // materialise the component and everything it uses, as placing it does, and ask
                // the loaded sheets the same two questions
                let folder =
                    System.IO.Path.Combine(
                        System.IO.Path.GetTempPath(), $"issie-shape-{System.Guid.NewGuid()}")
                System.IO.Directory.CreateDirectory folder |> ignore
                try
                    let loaded =
                        match ComponentLibraries.readComponentAndDependencies libPath name with
                        | Error msg -> failtest $"{name}: {msg}"
                        | Ok files ->
                            files
                            |> List.map (fun (header, body) ->
                                let dgm = System.IO.Path.Combine(folder, header.Name + ".dgm")
                                System.IO.File.WriteAllText(dgm, body)
                                match FilesIO.tryLoadComponentFromPath dgm with
                                | Error msg -> failtest $"{header.Name}: {msg}"
                                | Ok ldc -> ldc)
                    let placed = List.last loaded
                    Expect.equal shape.InputLabels placed.InputLabels $"{name} inputs"
                    Expect.equal shape.OutputLabels placed.OutputLabels $"{name} outputs"
                    // the question createNewSymbol asks of the project once the sheet is in it
                    let instance =
                        { makeComp 1 0 0 (Input1(1, None)) "I1" with
                            Type =
                                Custom
                                    { Name = placed.Name
                                      InputLabels = placed.InputLabels
                                      OutputLabels = placed.OutputLabels
                                      Form = placed.Form; Description = None; ParameterBindings = None } }
                    Expect.equal shape.IsClocked (isClocked [] loaded instance) $"{name} clocked"
                finally
                    try System.IO.Directory.Delete(folder, true) with _ -> ())
        }

        test "a library component opened for viewing appears without its own innards" {
            // What the right-click "View library component" item asks for: this sheet and no
            // more. A component built from other library components keeps them shut, each
            // needing the same deliberate click of its own.
            let viewed = getSheetTreesFiltered (fun name -> name = "L1_fullAdd") false project |> namesIn
            Expect.equal viewed (Set.ofList [ "top"; "L1_fullAdd" ])
                "the component being looked at is there; the sheet it is built from is not"
        }
    

        //------------------------------------------------------------------------------------//
        // A library opened as a project.
        //
        // The library IS the project and its .ldgm files ARE its sheets, so a multi-sheet
        // component opens as the several sheets it was authored as, and saving one writes it back
        // into the library it came from rather than producing a copy somewhere else.
        //------------------------------------------------------------------------------------//

        test "a library opens as a project of its components, dependencies included" {
            withTempLibrary (fun libPath ->
                writeComponent libPath (header "adder" true [ "carry" ]) (bodyOf "adder" [ instanceOf 1 "carry" "C1" ])
                writeComponent libPath (header "carry" false []) (bodyOf "carry" [])

                match ComponentLibraries.tryLoadLibraryProject libPath with
                | Error msg -> failtest msg
                | Ok statuses ->
                    let ldcs = statuses |> List.map (function | FilesIO.OkComp ldc -> ldc | _ -> failtest "resolve")
                    Expect.equal (ldcs |> List.map (fun ldc -> ldc.Name) |> List.sort) [ "adder"; "carry" ]
                        "the component and the helper it requires are both sheets"
                    Expect.all ldcs (fun ldc -> ldc.Form = Some User)
                        "every sheet is the user's to edit - a Library form would open it read-only"
                    Expect.all ldcs (fun ldc -> not ldc.IsTopSheet)
                        "a library does not say which of its components is the design"
                    Expect.all ldcs (fun ldc -> FilesIO.hasExtn ".ldgm" ldc.FilePath)
                        "each sheet is saved back to the component file it came from")
        }

        test "a library project is told from an ordinary one by its sheets' files" {
            let libraryProject =
                { project with
                    LoadedComponents =
                        project.LoadedComponents
                        |> List.map (fun ldc -> {ldc with FilePath = ldc.Name + ".ldgm"}) }
            Expect.isTrue (ComponentLibraries.isLibraryProject libraryProject) "its sheets are .ldgm"
            Expect.isFalse (ComponentLibraries.isLibraryProject project) "and an ordinary project's are not"
            Expect.equal (ComponentLibraries.sheetExtension libraryProject) ".ldgm"
                "a sheet added to a library becomes another component of it"
            Expect.equal (ComponentLibraries.sheetExtension project) ".dgm" "and not otherwise"
        }

        test "saving a component keeps what the author declared and refreshes what the canvas says" {
            withTempLibrary (fun libPath ->
                let path = ComponentLibraries.componentPath libPath "adder"
                writeComponent libPath
                    {header "adder" true [ "carry" ] with Section = "Arithmetic"; Description = "ripple-carry adder"}
                    (bodyOf "adder" [ instanceOf 1 "carry" "C1" ])

                // saved with a different sub-sheet on the canvas, and no description of its own
                let canvas: CanvasState = [ instanceOf 1 "lookahead" "L1" ], []
                let sheetInfo: SheetInfo =
                    {Form = Some User; Description = None; ParameterDefinitions = None; IsTopSheet = Some false}
                match ComponentLibraries.writeSheetFile path (canvas, None, Some sheetInfo) with
                | Error msg -> failtest msg
                | Ok () ->
                    match ComponentLibraries.tryReadHeader path with
                    | Error msg -> failtest msg
                    | Ok saved ->
                        Expect.equal saved.Section "Arithmetic" "the catalogue section is the author's"
                        Expect.isTrue saved.OfferedInCatalogue "and so is being offered at all"
                        Expect.equal saved.Description "ripple-carry adder"
                            "a sheet with no description of its own keeps the one in the file"
                        Expect.equal saved.Requires [ "lookahead" ]
                            "Requires is what the canvas now instantiates, or the component is \
                             placed missing a sheet")
        }

        test "a component edited and saved comes back as what was saved" {
            withTempLibrary (fun libPath ->
                let path = ComponentLibraries.componentPath libPath "adder"
                writeComponent libPath (header "adder" true []) (bodyOf "adder" [])
                let canvas: CanvasState = [ instanceOf 7 "carry" "C1" ], []
                let sheetInfo: SheetInfo =
                    {Form = Some User; Description = Some "now with carry"; ParameterDefinitions = None; IsTopSheet = Some false}
                ComponentLibraries.writeSheetFile path (canvas, None, Some sheetInfo) |> Result.isOk
                |> fun ok -> Expect.isTrue ok "the save succeeded"

                match ComponentLibraries.tryLoadLibraryProject libPath with
                | Error msg -> failtest msg
                | Ok [ FilesIO.OkComp ldc ] ->
                    Expect.equal (fst ldc.CanvasState |> List.map (fun c -> c.Label)) [ "C1" ]
                        "the canvas written is the canvas read back"
                    Expect.equal ldc.Description (Some "now with carry")
                        "the sheet's description is the header's, in both directions"
                | Ok _ -> failtest "expected exactly one component")
        }

        test "a sheet added to a library becomes a helper, not a catalogue entry" {
            // What the catalogue offers is something the author declares, and nothing in the
            // editor asks for it - so a new sheet is a helper until "Save as library component"
            // says otherwise.
            withTempLibrary (fun libPath ->
                let libProject: Project =
                    { ProjectPath = libPath
                      OpenFileName = "adder"
                      WorkingFileName = Some "adder"
                      LoadedComponents = [ {ldc "adder" User ([], []) with FilePath = ComponentLibraries.componentPath libPath "adder"} ] }
                match ComponentLibraries.createEmptySheetFile libProject "helper" with
                | Error msg -> failtest msg
                | Ok () ->
                    match ComponentLibraries.tryReadHeader (ComponentLibraries.componentPath libPath "helper") with
                    | Error msg -> failtest msg
                    | Ok created ->
                        Expect.isFalse created.OfferedInCatalogue "not offered until it is declared to be"
                        Expect.equal created.Name "helper" "named after its file"
                        Expect.equal created.Requires [] "an empty sheet instantiates nothing")
        }
    

        // Which libraries the user may edit in place.
        //
        // Writing a library component is: draw a sheet, save it into a library, place it and try
        // it, change it. The last step must not need a second non-library copy of the sheet kept in
        // step by hand - so a library IS editable. What is not is a library that arrived from
        // somewhere else in the form it arrived in: the ones shipped with Issie, and the ones in
        // the user library directory, which is the store a saved or imported library lands in.
        test "a library in a folder of the user's own is theirs to edit" {
            withTempLibrary (fun libPath ->
                Expect.isFalse (ComponentLibraries.isManagedLibrary libPath)
                    "a folder of their own is not one of Issie's"
                Expect.isTrue (ComponentLibraries.libraryPathIsEditable libPath)
                    "so it opens as a project and saves back into itself")
        }

        test "the libraries in Issie's own directories are not edited in place" {
            match ComponentLibraries.tryUserLibrariesDirectory () with
            | Error msg -> failtest msg
            | Ok userRoot ->
                let stored = FilesIO.pathJoin [| userRoot; "someLibrary" |]
                Expect.isTrue (ComponentLibraries.isManagedLibrary stored)
                    "the user library directory is where a saved or imported library ARRIVES"
                Expect.isTrue (ComponentLibraries.isManagedLibrary userRoot)
                    "the directory itself counts, not only what is under it"

            // The shipped directory is empty under .NET - Bridge.staticDir has no Electron to ask -
            // so what is checked here is that an empty root claims nothing, which is what stops
            // every path in the world reading as a shipped library.
            Expect.isFalse (ComponentLibraries.isManagedLibrary "/somewhere/else/adders")
                "an unrelated path is not inside an empty shipped root"
        }
    ]
