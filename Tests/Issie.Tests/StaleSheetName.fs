/// Asking to simulate a sheet that is not in the project.
///
/// This is not a hypothetical. Model.WaveSimSheet names the sheet the waveform viewer is
/// simulating, which is deliberately allowed to differ from the sheet on screen - so nothing about
/// it is invalidated by an ordinary sheet change, and it survived a change of PROJECT too. The
/// viewer's buttons asked, on every render, whether the sheet named there still builds; against the
/// newly opened project's sheets that question has no answer, and the answer used to be an
/// exception. Thrown from inside a React render it unmounted the whole UI, and since no message had
/// changed the model, every later render threw in the same place: the application was unusable
/// until it was reloaded.
///
/// Three things now stand between that and the user, and the first two are asserted here. A missing
/// sheet is an ordinary SimulationError, which is what every caller is typed to carry anyway; the
/// check that asks the question runs from the update function rather than from a render, and
/// catches (ModelHelpers.runCircuitCheck); and opening a project ends any running waveform
/// simulation, so the name does not go stale in the first place - that last one is in
/// MenuHelpers.setupProjectFromComponents and is not testable outside the Elmish loop.
module StaleSheetName

open Expecto
open CommonTypes
open SimGraphTypes
open CanvasBuilder

/// IN -> NOT -> OUT, under whatever name is asked for. Two projects' worth of these stand in for
/// the real thing: what matters is only that a name from one is absent from the other.
let private sheet (name: string) =
    let i = makeComp $"{name}-in" 0 1 (Input1(1, None)) "IN"
    let n = makeComp $"{name}-not" 1 1 Not "N"
    let o = makeComp $"{name}-out" 1 0 (Output 1) "OUT"
    makeLdc name None ([ i; n; o ], [ conn i 0 n 0; conn n 0 o 0 ])

/// The project being left, and the one opened over it. No name is shared.
let private oldProject = [ sheet "eep1" ]
let private newProject = [ sheet "largeTest"; sheet "aluN" ]

let private errorText (e: SimulationError) =
    match e.ErrType with
    | GenericSimError msg -> msg
    | other -> $"%A{other}"

/// Does this sheet of this design build? Exactly the composition ModelHelpers.runCircuitCheck and
/// Simulator.prepareSimulationMemoized both use - look the sheet up among the design's sheets, then
/// check what that gives - which is the question the waveform viewer's buttons have an answer to.
let private validateSheet (sheetName: string) (ldcs: LoadedComponent list) =
    CanvasExtractor.getStateAndDependencies sheetName ldcs
    |> Result.mapError Simulator.makeDummySimulationError
    |> Result.bind (fun (_, state, deps) -> Simulator.validateCircuitSimulation sheetName state deps)

let tests =
    testList "StaleSheetName" [

        test "a sheet that is in the design is returned with the others as its dependencies" {
            match CanvasExtractor.getStateAndDependencies "largeTest" newProject with
            | Error msg -> failtest $"largeTest is in this design: {msg}"
            | Ok(name, state, deps) ->
                Expect.equal name "largeTest" "the sheet asked for is the one named back"
                Expect.equal state (List.head newProject).CanvasState "with its own canvas"
                Expect.equal (deps |> List.map (fun ldc -> ldc.Name)) [ "aluN" ]
                    "and the dependency list excludes the sheet itself"
        }

        test "a sheet that is not in the design is an error, not an exception" {
            match CanvasExtractor.getStateAndDependencies "eep1" newProject with
            | Ok _ -> failtest "eep1 is not a sheet of this design"
            | Error msg ->
                Expect.stringContains msg "eep1" "the message names the sheet that is missing"
        }

        test "validating a sheet from a closed project returns an error" {
            // The crash itself: WaveSimSheet still says eep1, the project is now largeTest.
            match validateSheet "eep1" newProject with
            | Ok _ -> failtest "a sheet outside the project cannot validate"
            | Error e ->
                Expect.stringContains (errorText e) "eep1" "and the error says which sheet it was"
        }

        test "validating a sheet that is present still works" {
            // The error path must not have swallowed the ordinary one.
            Expect.isOk (validateSheet (List.head newProject).Name newProject)
                "a sheet of the design is simulable"
        }

        test "a stale sheet name does not wedge the simulation cache" {
            // prepareSimulationMemoized stores what it built. Storing nothing usable, and then
            // reporting the design unchanged, would leave the error stuck to a project that is
            // perfectly simulable - so the good simulation that follows must still be built.
            let openSheet = List.head newProject
            let deps = List.tail newProject
            let stale, _ =
                Simulator.prepareSimulationMemoized
                    true 100 openSheet.Name "eep1" openSheet.CanvasState deps
            Expect.isError stale "the stale name is refused"

            let good, _ =
                Simulator.prepareSimulationMemoized
                    true 100 openSheet.Name openSheet.Name openSheet.CanvasState deps
            Expect.isOk good "and the next simulation of a real sheet is built rather than cached away"

            // this suite's other groups share these globals
            Simulator.simCacheWS <- Simulator.simCacheInit ()
            Simulator.simCache <- Simulator.simCacheInit ()
        }

        test "the old project's own sheet still simulates against the old project" {
            // Nothing above is a fact about eep1: it is a fact about eep1 and the wrong sheet list.
            match CanvasExtractor.getStateAndDependencies "eep1" oldProject with
            | Error msg -> failtest $"eep1 is a sheet of the project it came from: {msg}"
            | Ok(_, _, deps) -> Expect.isEmpty deps "and it is the only one there"
        }
    ]
