/// Golden-model tests: simulate whole fixture projects and compare every output on
/// every clock cycle (plus the final state of clocked components) against a stored
/// golden file. Regenerate goldens by running with ISSIE_UPDATE_GOLDEN=1.
module GoldenModel

open System
open System.IO
open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open TestFixtures

/// The digest render moved into the app (SimDigest.render) so the dotnet sidecar can produce
/// the identical text; these are kept as the test suite's names for it.
let stimulus = SimDigest.stimulus

/// As runGolden, but on an already-loaded (or synthesised) design, so that a test can compare
/// two designs that must behave identically - e.g. the SimpleDesign shim against its original.
let runGoldenLdcs (ldcs: LoadedComponent list) (topSheet: string) (ticks: int) : string =
    match SimDigest.render ldcs topSheet ticks with
    | Ok text -> text
    | Error e -> failwith e

/// Simulate `ticks` clock cycles of `topSheet`, driving all top-level inputs with the
/// deterministic stimulus, and render the observable behaviour as text
let runGolden (projectName: string) (topSheet: string) (ticks: int) : string =
    runGoldenLdcs (loadProject projectName) topSheet ticks

let private goldenPath (projectName: string) (topSheet: string) =
    Path.Combine(fixturesDir, projectName, topSheet + ".golden")

let goldenTest (projectName: string) (topSheet: string) (ticks: int) =
    test $"golden {projectName}/{topSheet}" {
        let actual = runGolden projectName topSheet ticks
        let path = goldenPath projectName topSheet
        match Environment.GetEnvironmentVariable "ISSIE_UPDATE_GOLDEN" with
        | "1" -> File.WriteAllText(path, actual)
        | _ ->
            if not (File.Exists path) then
                failtest $"Golden file missing: {path}. Run with ISSIE_UPDATE_GOLDEN=1 to create it."
            let expected = File.ReadAllText(path).Replace("\r\n", "\n")
            if actual <> expected then
                let actualLines = actual.Split '\n'
                let expectedLines = expected.Split '\n'
                let pad (lines: string array) n =
                    Array.append lines (Array.create (max 0 (n - lines.Length)) "<missing>")
                let n = max actualLines.Length expectedLines.Length
                let diffs =
                    Array.zip (pad expectedLines n) (pad actualLines n)
                    |> Array.indexed
                    |> Array.filter (fun (_, (e, a)) -> e <> a)
                    |> Array.truncate 20
                    |> Array.map (fun (i, (e, a)) -> $"line {i}: expected '{e}' got '{a}'")
                    |> String.concat "\n"
                failtest $"Golden mismatch for {projectName}/{topSheet}:\n{diffs}"
    }

/// Every component has two reducers: the one EvalCompiled builds for its type, which the
/// simulation actually runs, and the general fastReduce, which is the definition of what that
/// reducer must do. This drives two independent simulations of the same design, one through
/// each, and requires that every output of every component agrees on every step held in the
/// circular buffer. It is what makes converting another component type to a specialised
/// reducer a safe change: get it wrong and this fails, whatever the goldens say.
let reducerAgreementTest (projectName: string) (topSheet: string) (ticks: int) =
    test $"reducers agree {projectName}/{topSheet}" {
        let ldcs = loadProject projectName
        let top = ldcs |> List.find (fun ldc -> ldc.Name = topSheet)
        let build () =
            match Simulator.startCircuitSimulation SimDigest.Constants.maxArraySize topSheet top.CanvasState ldcs with
            | Error e -> failwith $"Simulation of {projectName}/{topSheet} failed: %A{e}"
            | Ok simData -> simData.FastSim

        let viaGeneral = build ()
        let viaInstalled = build ()

        for tick in 1..ticks do
            let step = stepIndexOf viaGeneral.MaxArraySize tick
            Array.iter (EvalReference.fastReduce step true) viaGeneral.FClockedComps
            Array.iter (EvalReference.fastReduce step false) viaGeneral.FOrderedComps
            viaGeneral.ClockTick <- tick
            Array.iter (fun (fc: FastComponent) -> fc.ReduceClocked step) viaInstalled.FClockedComps
            Array.iter (fun (fc: FastComponent) -> fc.ReduceComb step) viaInstalled.FOrderedComps
            viaInstalled.ClockTick <- tick

        let outputsOf (fs: FastSimulation) =
            Array.append fs.FClockedComps fs.FOrderedComps
            |> Array.collect (fun fc ->
                fc.Outputs
                |> Array.mapi (fun i o -> $"{fc.FullName}:{i}", o.UInt32Step, o.BigIntStep))

        let disagreeing =
            Array.zip (outputsOf viaGeneral) (outputsOf viaInstalled)
            |> Array.filter (fun ((_, u1, b1), (_, u2, b2)) -> u1 <> u2 || b1 <> b2)
            |> Array.map (fun ((name, _, _), _) -> name)

        Expect.isEmpty disagreeing
            $"after {ticks} ticks these outputs differ between the installed reducer and fastReduce"
    }

let tests =
    testList "GoldenModel" [
        goldenTest "1fulladder" "fulladd" 8
        goldenTest "adder4" "fa4" 8
        goldenTest "3cpu" "eep1" 50
        reducerAgreementTest "1fulladder" "fulladd" 200
        reducerAgreementTest "adder4" "fa4" 200
        reducerAgreementTest "3cpu" "eep1" 500
    ]
