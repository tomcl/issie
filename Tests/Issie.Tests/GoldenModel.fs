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
        Expect.equal lean.FCompsByIndex.Length full.FCompsByIndex.Length "the same components"
        Expect.equal lean.FClockedComps.Length full.FClockedComps.Length "the same clocked components"
        Expect.equal lean.FOrderedComps.Length full.FOrderedComps.Length "the same evaluation order"
        Expect.equal lean.NumStepArrays full.NumStepArrays "the same step arrays"

        // ...without the three things only a viewer reads
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
            fs.FCompsByIndex
            |> Array.toList
            |> List.filter (fun fc -> match fc.FType with Custom _ -> true | _ -> false)
            |> List.collect (fun fc ->
                [ for i, io in Array.indexed fc.Outputs -> $"{fc.fId}.out{i}={io.Index}"
                  for i, io in Array.indexed fc.InputLinks -> $"{fc.fId}.in{i}={io.Index}" ])

        Expect.equal (customPortArrays lean) (customPortArrays full)
            "custom component ports must point at the same arrays in both builds"
    }

/// Every instance of every sheet, however deep - the argument PortView is asked about.
let rec private allInstances (fs: FastSimulation) (InstancePath ap as inst) sheet =
    inst
    :: (fs.Design.SubSheetsOf sheet
        |> List.collect (fun (cid, child) -> allInstances fs (InstancePath(ap @ [ cid ])) child))

/// What the ports of an instance are, as a comparable string - everything a waveform takes from
/// PortView, so that two builds agreeing on this agree on every wave they offer.
let private portsAsText (fs: FastSimulation) (inst: InstancePath) =
    (PortView.ofInstance fs inst).ViewPorts
    |> List.map (fun p ->
        $"{p.PortComp} {p.PortIs} {p.PortNum} idx={p.PortArrayIndex} w={p.PortWidth} \
          '{p.PortDisplayName}' '{p.PortLabel}' '{p.PortCompLabel}'")

/// The waveform viewer asks PortView which ports an instance offers, how wide each is, and where
/// its data lies. That is the ONE thing about a build the viewer cannot get from the design - and
/// it is what a remote simulator would have to answer, from this same code, out of a build made
/// without the wave tables.
///
/// So it must not need them. It used to: the component came from WaveComps, the union of the two
/// component maps built by folding one into the other, and the width from the Drivers table. The
/// component is now looked up in the two maps directly and the width is the step array's own,
/// which is the same number - a driver's width is set from the width of the output whose array it
/// is, and an input link IS that array once linked.
let portViewWithoutTablesTest (projectName: string) (topSheet: string) =
    test $"PortView answers the same without the wave tables {projectName}/{topSheet}" {
        let ldcs = loadProject projectName
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

        Expect.isEmpty lean.Drivers "the lean build really has no driver table"

        let instances = allInstances full (InstancePath []) topSheet
        Expect.isGreaterThan (List.length instances) 1 "the design must have subsheets to be worth asking"

        let ports fs = instances |> List.collect (portsAsText fs)
        let fromFull = ports full
        let fromLean = ports lean

        Expect.isGreaterThan (List.length fromFull) 0 "the design must offer some ports"
        Expect.equal (List.length fromLean) (List.length fromFull) "the same number of ports"

        match List.zip fromFull fromLean |> List.tryFind (fun (a, b) -> a <> b) with
        | Some(a, b) -> failtest $"a port differs:\n  with tables: {a}\n  without:     {b}"
        | None -> ()

        // And the width really is the one the driver table would have given, which is the step
        // this rests on: every port, both numbers, on a build that still has the table to ask.
        let widthPairs =
            instances
            |> List.collect (fun inst ->
                (PortView.ofInstance full inst).ViewPorts
                |> List.map (fun p ->
                    let viaDriver =
                        match Array.tryItem (driverIndexValue p.PortArrayIndex) full.Drivers with
                        | Some(Some d) -> d.DriverWidth
                        | _ -> 0

                    p.PortComp, p.PortIs, p.PortNum, p.PortWidth, viaDriver))

        match widthPairs |> List.tryFind (fun (_, _, _, own, viaDriver) -> own <> viaDriver) with
        | Some(comp, portType, pn, own, viaDriver) ->
            failtest
                $"{comp} {portType} port {pn}: the array says {own} bits, the driver table {viaDriver}"
        | None -> ()
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
        portViewWithoutTablesTest "1fulladder" "fulladd"
        portViewWithoutTablesTest "adder4" "fa4"
        portViewWithoutTablesTest "3cpu" "eep1"
    ]
