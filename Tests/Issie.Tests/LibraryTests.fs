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
let private instanceOf (name: string) (label: string) =
    { makeComp label 0 0 (Input1(1, None)) label with
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
          ldc "top" User ([ instanceOf "L1_fullAdd" "FA1" ], [])
          ldc "L1_fullAdd" (Library ("arithmetic", "fullAdd")) ([ instanceOf "L1_halfAdd" "HA1" ], [])
          ldc "L1_halfAdd" (Library ("arithmetic", "halfAdd")) ([], [])
      ] }

let private namesIn (trees: Map<string, SheetTree>) =
    let rec walk (t: SheetTree) = t.SheetName :: List.collect walk t.SubSheets
    trees |> Map.toList |> List.collect (fun (name, tree) -> name :: walk tree) |> Set.ofList

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

        test "a library component opened for viewing appears without its own innards" {
            // What the right-click "View library component" item asks for: this sheet and no
            // more. A component built from other library components keeps them shut, each
            // needing the same deliberate click of its own.
            let viewed = getSheetTreesFiltered (fun name -> name = "L1_fullAdd") false project |> namesIn
            Expect.equal viewed (Set.ofList [ "top"; "L1_fullAdd" ])
                "the component being looked at is there; the sheet it is built from is not"
        }
    ]
