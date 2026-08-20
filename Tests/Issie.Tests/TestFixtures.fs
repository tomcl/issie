/// Loads Issie project fixtures from disk for tests running under plain dotnet.
/// The loading itself is FilesIO's, exactly as the app does it - which is the point: a fixture
/// that loads here proves the production loader works headlessly. It used to be reimplemented
/// here, because FilesIO reached for Electron as it initialised and could not read the app's
/// JSON on .NET; neither is true now.
module TestFixtures

open System.IO
open CommonTypes

/// Directory holding the test fixture projects, located relative to this source file
let fixturesDir =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures"))

/// Load one .dgm file as a LoadedComponent, memory contents and all
let loadLoadedComponent (filePath: string) : LoadedComponent =
    match FilesIO.tryLoadComponentFromPath filePath with
    | Ok ldc -> ldc
    | Error msg -> failwith msg

/// Load every sheet of a fixture project directory, admitted exactly as the app admits a
/// design at project open: allocators seeded, component ids made design-unique.
let loadProject (projectName: string) : LoadedComponent list =
    match FilesIO.loadAllComponentFiles (Path.Combine(fixturesDir, projectName)) with
    | Error msg -> failwith msg
    | Ok statuses ->
        statuses
        |> List.map (function
            | FilesIO.OkComp ldc
            | FilesIO.OkAuto ldc
            | FilesIO.Resolve(ldc, _) -> ldc)
        |> Helpers.RegenerateIds.admitDesign
        |> fst
