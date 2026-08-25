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
                |> Array.mapi (fun i o -> $"{fc.FullName}:{i}", o.U32Contents, o.BigContents))

        let disagreeing =
            Array.zip (outputsOf viaGeneral) (outputsOf viaInstalled)
            |> Array.filter (fun ((_, u1, b1), (_, u2, b2)) -> u1 <> u2 || b1 <> b2)
            |> Array.map (fun ((name, _, _), _) -> name)

        Expect.isEmpty disagreeing
            $"after {ticks} ticks these outputs differ between the installed reducer and fastReduce"
    }

/// The claim the sidecar's build rests on: leaving out the structures only a wave VIEWER reads
/// changes nothing a simulation DOES.
///
/// Compared as the golden digest, which is every output, viewer and clocked component on every
/// cycle plus the final memory contents - so a divergence anywhere in the run shows up as a line
/// number rather than as a summary. The tables themselves are then asserted absent, because a
/// build that quietly kept them would pass the first half.
let noWaveTablesTest (projectName: string) (topSheet: string) (ticks: int) =
    test $"a build without wave tables behaves identically {projectName}/{topSheet}" {
        let ldcs = loadProject projectName

        let digestOf waveTables =
            match SimDigest.renderWith waveTables ldcs topSheet ticks with
            | Ok text -> text
            | Error e -> failwith e

        let withTables = digestOf WithWaveTables
        let without = digestOf NoWaveTables
        Expect.isGreaterThan withTables.Length 0 "the digest must not be empty"

        if withTables <> without then
            let a = withTables.Split '\n'
            let b = without.Split '\n'
            let firstDiff =
                Seq.zip a b |> Seq.tryFindIndex (fun (x, y) -> x <> y) |> Option.defaultValue (min a.Length b.Length)
            failtest
                $"the two builds diverge at line {firstDiff}: \
                  with tables '{Array.tryItem firstDiff a}', without '{Array.tryItem firstDiff b}'"

        let top = ldcs |> List.find (fun ldc -> ldc.Name = topSheet)

        let build waveTables =
            match
                Simulator.startCircuitSimulationWith
                    waveTables SimDigest.Constants.maxArraySize topSheet top.CanvasState ldcs
            with
            | Error e -> failwith $"Simulation of {projectName}/{topSheet} failed: %A{e}"
            | Ok simData -> simData.FastSim

        let full = build WithWaveTables
        let lean = build NoWaveTables

        // the same simulation either way...
        Expect.equal lean.FComps.Count full.FComps.Count "the same components"
        Expect.equal lean.FClockedComps.Length full.FClockedComps.Length "the same clocked components"
        Expect.equal lean.FOrderedComps.Length full.FOrderedComps.Length "the same evaluation order"
        Expect.equal lean.NumStepArrays full.NumStepArrays "the same step arrays"

        // ...without the three things only a viewer reads
        Expect.isNonEmpty full.WaveComps "the ordinary build has a wave component map"
        Expect.isEmpty lean.WaveComps "and the lean one does not"
        Expect.isGreaterThan full.Drivers.Length 0 "the ordinary build has drivers"
        Expect.equal lean.Drivers.Length 0 "and the lean one does not"
        Expect.isGreaterThan full.WaveIndex.Length 0 "the ordinary build has a wave index"
        Expect.equal lean.WaveIndex.Length 0 "and the lean one does not"

        // A custom component's ports are linked either way - that pass is not part of the tables.
        // Without it a custom port reads the dummy array it was created with, which is how this
        // would break silently: every waveform of a subsheet boundary reading as nothing.
        // A custom component's ports are linked either way - that pass is NOT part of the tables.
        // Without it each port still has an array, of the right width, so nothing looks wrong:
        // it is the DUMMY the component was created with rather than the array of the Input or
        // Output inside the subsheet, and every waveform of a subsheet boundary reads as nothing.
        // Comparing the array indices is what tells the two apart.
        let customPortArrays (fs: FastSimulation) =
            fs.FCustomComps
            |> Map.toList
            |> List.collect (fun (fid, fc) ->
                [ for i, io in Array.indexed fc.Outputs -> $"{fid}.out{i}={io.Index}"
                  for i, io in Array.indexed fc.InputLinks -> $"{fid}.in{i}={io.Index}" ])

        Expect.equal (customPortArrays lean) (customPortArrays full)
            "custom component ports must point at the same arrays in both builds"
    }

let tests =
    testList "GoldenModel" [
        goldenTest "1fulladder" "fulladd" 8
        goldenTest "adder4" "fa4" 8
        goldenTest "3cpu" "eep1" 50
        reducerAgreementTest "1fulladder" "fulladd" 200
        reducerAgreementTest "adder4" "fa4" 200
        reducerAgreementTest "3cpu" "eep1" 500
        // eep1 is the one with custom components nested several deep, memories and viewers, so it
        // is where leaving a structure out would show
        noWaveTablesTest "1fulladder" "fulladd" 8
        noWaveTablesTest "3cpu" "eep1" 50
    ]
