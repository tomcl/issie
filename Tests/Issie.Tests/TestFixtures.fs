/// Loads Issie project fixtures from disk for tests running under plain dotnet.
/// The loading itself is FilesIO's, exactly as the app does it - which is the point: a fixture
/// that loads here proves the production loader works headlessly. It used to be reimplemented
/// here, because FilesIO reached for Electron as it initialised and could not read the app's
/// JSON on .NET; neither is true now.
module TestFixtures

open System.IO
open CommonTypes
open SimTypes
open ModelType

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

/// Every wave the simulation offers, described.
///
/// The wave selector used to hold exactly this in its model - one Wave record per viewable port of
/// every instance of every sheet. It no longer builds it: the selector describes the waves of the
/// handful of sheet instances it is drawing, and the model keeps records only for the waves
/// SELECTED. Tests that want to talk about all of them at once build it here.
let allWavesOf (ws: WaveSimModel) (fs: FastSimulation) : Map<WaveIndexT, Wave> =
    fs.WaveIndex
    |> Array.toList
    |> List.filter (fun wi -> not (WaveSimHelpers.isInsideLibraryComponent fs fs.WaveComps[wi.Id]))
    |> List.map (fun wi -> wi, WaveSimHelpers.makeWave ws fs wi)
    |> WaveSimHelpers.makeWaveMap
