/// What building and running a real design costs under .NET.
///
/// The suite had no standing measurement of the BUILD, which is where the simulator spends its
/// allocation (the run loop allocates nothing) and where every id in the design is created,
/// compared and looked up. That makes this the gate for work that changes how ids are
/// represented: wrapping them is worth having for the type checking it buys, and not worth
/// having if it costs the .NET simulator more than a few percent.
///
/// The design is `3cpu`/`eep1`, the shipped 16-bit CPU across 18 sheets - large by the standards
/// of what Issie is used for, and the one the rest of the suite already prices.
///
/// .NET speed is not the app's speed: simulatorStructure.md records the same change measured as
/// 2.5x under .NET and 11.9x in V8. What this measures honestly is the sidecar, which IS .NET,
/// and space, which is a real number either way.
///
/// Compare the FASTEST figure, not the median. The noise here is the machine, and it is all in
/// one direction - across processes the fastest slice repeats to within 2% while the median moves
/// by 10%, which is no use for a gate of 5%. Run with tiered compilation off, or tier 0 is most
/// of what is timed: it makes the run loop look 4x slower than it is.
///
/// Nothing here runs in the ordinary suite. Set ISSIE_BENCH=1 to run it:
///     DOTNET_TieredCompilation=0 DOTNET_TieredPGO=0 ISSIE_BENCH=1 ///         dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.SimBenchmark
module SimBenchmark

open System
open System.IO
open Expecto
open CommonTypes
open SimTypes

let private benchEnabled =
    not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable "ISSIE_BENCH"))

/// Cycles per run of the simulation. Enough to be a real measurement rather than a JIT trace.
let private cycles =
    match Environment.GetEnvironmentVariable "ISSIE_BENCH_CYCLES" with
    | null
    | "" -> 100_000
    | s -> int s

/// Median rather than minimum, as simulatorStructure.md asks: the distribution is a tight cluster
/// with occasional fast outliers.
let private median (xs: float list) = xs |> List.sort |> List.item (List.length xs / 2)

/// The fastest repetition too. A gate of a few percent needs to know how much of the difference
/// between two runs of this is the machine rather than the code: if the minimum moves with the
/// median, the change is real.
let private fastest (xs: float list) = List.min xs

/// Which design to price. 3cpu/eep1 by default - the shipped 16-bit CPU, and the one the rest of
/// the suite already prices - but a design where one sheet is placed many times is the only kind
/// where design-sized and EXPANSION-sized structures differ, and 3cpu is not one. Point it at such
/// a design with ISSIE_BENCH_PROJECT and ISSIE_BENCH_TOP:
///
///     ISSIE_BENCH_PROJECT=C:/Users/me/Desktop/largeTest/largeTest ISSIE_BENCH_TOP=main6
///
/// ISSIE_BENCH_ARRAY sets the step array size, which has to be small enough that the step arrays
/// are not what is being measured - a design that expands needs it smaller than 3cpu does.
let private envOr (name: string) (fallback: string) =
    match Environment.GetEnvironmentVariable name with
    | null
    | "" -> fallback
    | s -> s

let private projectPath () =
    envOr
        "ISSIE_BENCH_PROJECT"
        (Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "demos", "3cpu")))

let private topSheet () = envOr "ISSIE_BENCH_TOP" "eep1"
let private arraySizeOf () = int (envOr "ISSIE_BENCH_ARRAY" "1000")

let private designUnderTest () =
    let path = projectPath ()

    match FilesIO.loadAllComponentFiles path with
    | Error msg -> failtestf "could not load the design at %s: %s" (projectPath ()) msg
    | Ok statuses ->
        statuses
        |> List.map (function
            | FilesIO.OkComp ldc
            | FilesIO.OkAuto ldc
            | FilesIO.Resolve(ldc, _) -> ldc)
        |> Helpers.RegenerateIds.admitDesign
        |> fst

/// Build and run, timed apart: the build is what id representation touches most, and the run is
/// what a change there must not have broken.
let private measure () =
    let ldcs = designUnderTest ()
    let topName = topSheet ()

    let top =
        match ldcs |> List.tryFind (fun ldc -> ldc.Name = topName) with
        | Some ldc -> ldc
        | None -> failtestf "no sheet called %s in %s" topName (projectPath ())
    // Small enough that the step arrays are not what is being measured: a build sized for the
    // whole run allocates 113 MB of them, which swamps the structural work - the graph, the ids
    // and the lookups - that id representation actually touches. The run wraps the buffer.
    let arraySize = arraySizeOf ()

    let buildOnce () =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let before = GC.GetTotalMemory true
        let sw = Diagnostics.Stopwatch.StartNew()

        let fs =
            match Simulator.startCircuitSimulation arraySize topName top.CanvasState ldcs with
            | Error e -> failtestf "%s failed to build: %A" topName e
            | Ok simData -> simData.FastSim

        sw.Stop()
        let after = GC.GetTotalMemory true
        sw.Elapsed.TotalMilliseconds, float (after - before) / 1.0e6, fs

    /// One simulation, advanced a slice at a time: each repetition runs the next `cycles` clocks
    /// of the same design, so the work is the same each time while the state moves on.
    ///
    /// Not a fresh simulation per repetition, which is what RamBenchmark does: building one costs
    /// a garbage collection's worth of allocation, and doing that inside the loop put ±20% of GC
    /// noise on a measurement that is meant to resolve 5%. Not a clock reset either - that leaves
    /// the state behind and re-runs different work.
    let runSlices (fs: FastSimulation) (n: int) =
        [ for i in 1..n ->
            let target = i * cycles
            let sw = Diagnostics.Stopwatch.StartNew()
            FastRun.runFastSimulation None target fs |> ignore
            sw.Stop()
            sw.Elapsed.TotalMilliseconds ]

    // discarded: the first build and the first slices in a process pay for JIT of the whole path
    let _, _, warm = buildOnce ()
    runSlices warm 2 |> ignore

    let builds = [ for _ in 1..9 -> buildOnce () ]
    let buildMs = median (builds |> List.map (fun (ms, _, _) -> ms))
    let buildMb = median (builds |> List.map (fun (_, mb, _) -> mb))
    let _, _, fs = List.head builds

    let runTimes = runSlices fs 9
    let runMs = median runTimes

    printfn "  %s build: %.1f ms (fastest %.1f), %.1f MB retained (%d fast components, %d reduced per clock)"
        topName buildMs (fastest (builds |> List.map (fun (ms, _, _) -> ms))) buildMb
        fs.FCompsByIndex.Length
        (fs.FClockedComps.Length + fs.FOrderedComps.Length)
    printfn "  %s run:   %d cycles in %.0f ms (fastest %.0f), %.1f cycles/ms"
        topName cycles runMs (fastest runTimes) (float cycles / runMs)

let tests =
    testList "SimBenchmark" [
        testCase "build and run"
        <| fun () ->
            if not benchEnabled then
                skiptest "set ISSIE_BENCH=1 to run the build and run benchmark"

            measure ()
    ]
