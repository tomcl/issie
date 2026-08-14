/// Phase 0 of the RAM representation work ([docs/dev/ramRepresentation.md]): a RAM-at-scale
/// benchmark, because the suite had none. The only fixture with a read/write memory is
/// `3cpu`/`eep1`, whose `DATAMEM` is a 64K x 16 `AsyncRAM1` - the exact case the design note
/// argues about - but it is driven by a program and so writes at whatever rate that program
/// happens to write. These sheets drive a RAM at a known rate instead.
///
/// Speed here is .NET speed, which is NOT the number that decides anything: simulatorStructure.md
/// records the same change measured as 2.5x under .NET and 11.9x in V8. What .NET does measure
/// honestly is *space*, since `GC.GetTotalMemory true` is a real number, and space is what this
/// work is mostly about.
///
/// Nothing here runs in the ordinary suite. Set ISSIE_BENCH=1 to run it:
///     ISSIE_BENCH=1 dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.RamBenchmark
module RamBenchmark

open System
open System.IO
open Expecto
open CommonTypes
open SimGraphTypes
open SimTypes
open SheetDescription

/// 2^16 words of 16 bits: 64K x 16, matching `3cpu`'s DATAMEM.
let private mem64k =
    { Init = FromData
      AddressWidth = 16
      WordWidth = 16
      Data = Map.empty
      Comments = None }

/// A self-driving RAM exerciser.
///
/// `DIV` is a free-running 3-bit counter and `WE` fires when it reads 7, so there is **one write
/// every 8 clocks**. `ADDRC` counts only on a write, so successive writes walk the address space
/// and a long enough run touches all 65536 words rather than the eighth of them a single counter
/// would reach. `FREE` runs every clock and is XORed into the data, so the second write to an
/// address carries a different value from the first - otherwise suppressing writes that change
/// nothing would flatter the benchmark rather than being measured by it.
///
/// `oneAddress` sends every write to address 0 instead, which is the pattern that makes a
/// per-address history long and the tree it replaces one node deep.
let private benchSheet (name: string) (ram: Memory1 -> ComponentType) (oneAddress: bool) =
    let comps =
        [ comp "DIV" (CounterNoEnableLoad 3)
          comp "WE" (BusCompare1(3, 7I, "7"))
          comp "ADDRC" (CounterNoLoad 16)
          comp "FREE" (CounterNoEnableLoad 16)
          comp "XOR" (NbitsXor(16, None))
          comp "MEM" (ram mem64k)
          comp "OUT" (Output 16) ]
        @ (if oneAddress then [ comp "ZERO" (Constant1(16, 0I, "0")) ] else [])
    let addrDriver = if oneAddress then "ZERO" else "ADDRC/Q"
    describeSheet name comps [
        connect "DIV/Q" "WE"
        connect "WE" "ADDRC/EN"
        connect "ADDRC/Q" "XOR/P"
        connect "FREE/Q" "XOR/Q"
        connect addrDriver "MEM/ADDR"
        connect "XOR/OUT" "MEM/DIN"
        connect "WE" "MEM/WEN"
        connect "MEM/DOUT" "OUT"
    ]

/// A 64K x 16 RAM written on **every** clock, which is the hardest a single memory can be driven.
///
/// `ADDRC` advances every cycle, so the address sweeps the whole space every 65536 clocks. The
/// data is the address XORed with the top half of a 32-bit free-running counter, which is the
/// pass number: an address gets a different value on each of its visits, so no write is
/// suppressed. The pass number alone would be zero for the whole first sweep and every write of
/// it would be suppressed; a second 16-bit counter alone would equal the address every pass and
/// every write after the first sweep would be suppressed. Both were tried, and both silently
/// measured a memory that was never written.
let private alwaysWriteSheet (name: string) (ram: Memory1 -> ComponentType) =
    describeSheet name [
        comp "ADDRC" (CounterNoEnableLoad 16)
        comp "FREE32" (CounterNoEnableLoad 32)
        comp "PASS" (BusSelection(16, 16))
        comp "WEN" (Constant1(1, 1I, "1"))
        comp "XOR" (NbitsXor(16, None))
        comp "MEM" (ram mem64k)
        comp "OUT" (Output 16)
    ] [
        connect "ADDRC/Q" "MEM/ADDR"
        connect "FREE32/Q" "PASS"
        connect "ADDRC/Q" "XOR/P"
        connect "PASS" "XOR/Q"
        connect "XOR/OUT" "MEM/DIN"
        connect "WEN" "MEM/WEN"
        connect "MEM/DOUT" "OUT"
    ]

/// 256 words of 1 bit: 32 bytes of actual data, which is the size a lot of real RAMs in teaching
/// designs are. A design with a hundred of them has as many *addresses* as one 64K RAM has in a
/// quarter of its space, so it is the case that shows what per-address overhead costs.
let private mem256x1 =
    { Init = FromData
      AddressWidth = 8
      WordWidth = 1
      Data = Map.empty
      Comments = None }

/// `count` small RAMs sharing one address, data and write-enable driver, each with its own output.
///
/// The data bit is FREE bit 11. A pass over the 256 addresses takes 256 writes at one write every
/// eight clocks, which is 2048 clocks, and bit 11 toggles exactly that often - so every address
/// really changes value on every pass. Any faster-toggling bit reads the *same* at every write to
/// a given address, suppression then eats all but the first pass, and the benchmark measures
/// nothing. Getting this wrong is easy and silent, which is what the live-word assertions are for.
let private manySmallSheet (name: string) (count: int) =
    let comps =
        [ comp "DIV" (CounterNoEnableLoad 3)
          comp "WE" (BusCompare1(3, 7I, "7"))
          comp "ADDRC" (CounterNoLoad 8)
          comp "FREE" (CounterNoEnableLoad 16)
          comp "BIT" (BusSelection(1, 11)) ]
        @ [ for i in 1..count do
                comp $"MEM{i}" (AsyncRAM1 mem256x1)
                comp $"OUT{i}" (Output 1) ]
    let conns =
        [ connect "DIV/Q" "WE"
          connect "WE" "ADDRC/EN"
          connect "FREE/Q" "BIT" ]
        @ [ for i in 1..count do
                connect "ADDRC/Q" $"MEM{i}/ADDR"
                connect "BIT" $"MEM{i}/DIN"
                connect "WE" $"MEM{i}/WEN"
                connect $"MEM{i}/DOUT" $"OUT{i}" ]
    describeSheet name comps conns

/// Build a simulation of one of the sheets above, sized so that the step arrays never wrap -
/// which is what the waveform simulator does, and the case where retained history is the cost.
let private buildSim (name: string) (ram: Memory1 -> ComponentType) (oneAddress: bool) (cycles: int) =
    let canvas =
        match SheetLayout.toCanvasState (benchSheet name ram oneAddress) with
        | Ok c -> c
        | Error e -> failtestf "could not build the benchmark sheet: %s" e
    let ldc = CanvasBuilder.makeLdc name None canvas
    match Simulator.startCircuitSimulation (cycles + 3) name canvas [ ldc ] with
    | Error e -> failtestf "benchmark simulation failed to build: %A" e
    | Ok simData -> simData.FastSim

/// Words of the RAM that are non-zero at the given step: the "confirm activity" check
/// simulatorStructure.md asks for, so that a benchmark of a stalled circuit is not mistaken for
/// a fast one.
let private liveWords (fs: FastSimulation) (step: int) =
    fs.FClockedComps
    |> Array.tryPick (fun fc ->
        match fc.FType with
        | RAM1 _
        | AsyncRAM1 _ ->
            match FastExtract.extractFastSimulationState fs step (fst fc.fId, snd fc.fId) with
            | RamState ram -> Some(RamStore.liveCountAt ram step)
            | _ -> None
        | _ -> None)
    |> Option.defaultValue 0

/// Which measurements to run: "sieve" for the whole-design one alone, "ram" for the synthetic
/// sheets alone, anything else for all of them. At a million cycles the synthetic ones take
/// several minutes, which is a long wait for the one at the end.
let private benchOnly =
    match Environment.GetEnvironmentVariable "ISSIE_BENCH_ONLY" with
    | null -> ""
    | s -> s

let private benchEnabled =
    not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable "ISSIE_BENCH"))

/// Cycles to run. Long enough that the address counter laps the whole 64K address space at one
/// write in 8 (that needs 8 x 65536 = 524288), unless overridden.
let private cycles =
    match Environment.GetEnvironmentVariable "ISSIE_BENCH_CYCLES" with
    | null
    | "" -> 100_000
    | s -> int s

/// Median of three, as simulatorStructure.md asks: the distribution is a tight cluster with
/// occasional fast outliers, so the minimum is not representative. A fresh simulation per
/// repetition, because the in-app benchmark's trick of resetting ClockTick leaves RAM state
/// behind and makes the second repetition simulate something else.
let private median (xs: float list) =
    xs |> List.sort |> List.item (List.length xs / 2)

let private measureOne (title: string) (ram: Memory1 -> ComponentType) (oneAddress: bool) =
    let run () =
        let fs = buildSim "rambench" ram oneAddress cycles
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let heapBefore = GC.GetTotalMemory true
        let sw = Diagnostics.Stopwatch.StartNew()
        FastRun.runFastSimulation None cycles fs |> ignore
        sw.Stop()
        let heapAfter = GC.GetTotalMemory true
        let live = liveWords fs (cycles - 1)
        float cycles / sw.Elapsed.TotalMilliseconds, float (heapAfter - heapBefore) / 1.0e6, live
    // one discarded run first: the first simulation in a process pays for JIT of the whole
    // reducer path, which at these sizes is a large fraction of a measurement
    run () |> ignore
    let results = [ for _ in 1..5 -> run () ]
    let speed = median (results |> List.map (fun (s, _, _) -> s))
    let heap = median (results |> List.map (fun (_, h, _) -> h))
    let live = results |> List.map (fun (_, _, l) -> l) |> List.head
    printfn "%-34s %10.1f cycles/ms %10.1f MB retained %8d live words" title speed heap live
    speed, heap, live

let private measureAlways (title: string) (ram: Memory1 -> ComponentType) (arraySize: int) =
    let build () =
        let canvas =
            match SheetLayout.toCanvasState (alwaysWriteSheet "always" ram) with
            | Ok c -> c
            | Error e -> failtestf "could not build the always-write sheet: %s" e
        let ldc = CanvasBuilder.makeLdc "always" None canvas
        match Simulator.startCircuitSimulation arraySize "always" canvas [ ldc ] with
        | Error e -> failtestf "always-write simulation failed to build: %A" e
        | Ok simData -> simData.FastSim
    let run () =
        let fs = build ()
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let heapBefore = GC.GetTotalMemory true
        let sw = Diagnostics.Stopwatch.StartNew()
        FastRun.runFastSimulation None cycles fs |> ignore
        sw.Stop()
        let heapAfter = GC.GetTotalMemory true
        let live = liveWords fs (cycles - 1)
        float cycles / sw.Elapsed.TotalMilliseconds, float (heapAfter - heapBefore) / 1.0e6, live
    run () |> ignore
    let results = [ for _ in 1..3 -> run () ]
    printfn "%-38s %9.1f cycles/ms %9.1f MB retained %8d live words"
        title
        (median (results |> List.map (fun (s, _, _) -> s)))
        (median (results |> List.map (fun (_, h, _) -> h)))
        (results |> List.map (fun (_, _, l) -> l) |> List.head)

/// The many-small-RAMs case. Reported separately because what matters here is bytes held against
/// bytes of memory simulated: a hundred 256 x 1 RAMs are 3.2 kB of data between them.
let private measureMany (title: string) (count: int) =
    let build () =
        let canvas =
            match SheetLayout.toCanvasState (manySmallSheet "smallrams" count) with
            | Ok c -> c
            | Error e -> failtestf "could not build the small-RAM sheet: %s" e
        let ldc = CanvasBuilder.makeLdc "smallrams" None canvas
        match Simulator.startCircuitSimulation (cycles + 3) "smallrams" canvas [ ldc ] with
        | Error e -> failtestf "small-RAM simulation failed to build: %A" e
        | Ok simData -> simData.FastSim
    let run () =
        let fs = build ()
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let heapBefore = GC.GetTotalMemory true
        let sw = Diagnostics.Stopwatch.StartNew()
        FastRun.runFastSimulation None cycles fs |> ignore
        sw.Stop()
        let heapAfter = GC.GetTotalMemory true
        float cycles / sw.Elapsed.TotalMilliseconds, float (heapAfter - heapBefore) / 1.0e6
    run () |> ignore
    let results = [ for _ in 1..3 -> run () ]
    let speed = median (results |> List.map fst)
    let heap = median (results |> List.map snd)
    printfn "%-34s %10.1f cycles/ms %10.1f MB retained  (%d words of data)" title speed heap (count * 256)

/// The whole-design measurement: the `5eratosthenes` demo, which is the EEP1 CPU, running
/// Eratosthenes's sieve. This is the benchmark `simulatorStructure.md` names, and unlike the
/// synthetic sheets above it is a real design where the RAM is one component among hundreds.
///
/// The demo ships with its ROM linked to `sievesmall`, which finishes in well under 25 000 cycles
/// and then spins in a self-jump - timing that measures a halted CPU. `sieve.txt` says the large
/// program needs about 800 000 clocks, so it is relinked here.
let private loadSieve () =
    // static/demos is the tracked copy; the demos/ directory at the repo root is a build artefact
    let path = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "static", "demos", "5eratosthenes"))
    match FilesIO.loadAllComponentFiles path with
    | Error msg -> failtestf "could not load the eratosthenes demo: %s" msg
    | Ok statuses ->
        statuses
        |> List.map (function
            | FilesIO.OkComp ldc
            | FilesIO.OkAuto ldc
            | FilesIO.Resolve(ldc, _) -> ldc)
        |> List.map (fun ldc ->
            if ldc.Name <> "eep1" then
                ldc
            else
                let comps, conns = ldc.CanvasState
                let comps =
                    comps
                    |> List.map (fun c ->
                        match c.Type with
                        | AsyncROM1 mem ->
                            match FilesIO.initialiseMem { mem with Init = FromFile "sieve" } path with
                            | Ok m -> { c with Type = AsyncROM1 m }
                            | Error e -> failtestf "could not read sieve.ram: %s" e
                        | _ -> c)
                { ldc with CanvasState = (comps, conns) })

/// How much of the RAM the sieve has filled in, which is what says the CPU is computing rather
/// than spinning.
let private sieveActivity (fs: FastSimulation) (step: int) =
    fs.FClockedComps
    |> Array.sumBy (fun fc ->
        match fc.FType with
        | RAM1 _
        | AsyncRAM1 _ ->
            match FastExtract.extractFastSimulationState fs step (fst fc.fId, snd fc.fId) with
            | RamState ram -> RamStore.liveCountAt ram step
            | _ -> 0
        | _ -> 0)

let private measureSieve (arraySize: int) =
    let ldcs = loadSieve ()
    let top = ldcs |> List.find (fun ldc -> ldc.Name = "eep1")
    let run () =
        let fs =
            match Simulator.startCircuitSimulation arraySize "eep1" top.CanvasState ldcs with
            | Error e -> failtestf "eratosthenes simulation failed to build: %A" e
            | Ok simData -> simData.FastSim
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let before = GC.GetTotalMemory true
        let sw = Diagnostics.Stopwatch.StartNew()
        FastRun.runFastSimulation None cycles fs |> ignore
        sw.Stop()
        let after = GC.GetTotalMemory true
        // the last simulated cycle, not a buffer index: history is keyed by absolute step, and
        // asking at an index would report whatever the wrapping buffer happened to hold there
        let live = sieveActivity fs (cycles - 1)
        sw.Elapsed.TotalMilliseconds, float (after - before) / 1.0e6, live, fs
    let _, _, _, fs0 = run ()
    printfn "  design: %d components reduced per clock, %d bytes of step arrays per step (%d typed, %d heap)"
        (fs0.FClockedComps.Length + fs0.FOrderedComps.Length)
        fs0.StepCost.TotalBytes fs0.StepCost.TypedArrayBytes fs0.StepCost.HeapBytes
    let results = [ for _ in 1..3 -> run () ]
    let ms = median (results |> List.map (fun (m, _, _, _) -> m))
    let heap = median (results |> List.map (fun (_, h, _, _) -> h))
    let live = results |> List.map (fun (_, _, l, _) -> l) |> List.head
    printfn "  %d cycles in %.0f ms (%.1f cycles/ms), %.1f MB retained, %d RAM words written"
        cycles ms (float cycles / ms) heap live

let tests =
    testList "RamBenchmark" [
        test "the benchmark sheet actually writes its RAM" {
            // Cheap, and always run: a benchmark of a circuit that never writes measures nothing,
            // and this is the check that would have caught that.
            let shortRun = 2000
            let fs = buildSim "rambench" AsyncRAM1 false shortRun
            FastRun.runFastSimulation None shortRun fs |> ignore
            let live = liveWords fs (shortRun - 1)
            // one write every 8 clocks, each to a fresh address, minus any that wrote a zero
            Expect.isGreaterThan live 200 "the RAM exerciser should have written most of 2000/8 words"
            Expect.isLessThan live (shortRun / 8 + 1) "it cannot have written more words than it had writes"
        }

        test "the one-address sheet writes exactly one word" {
            let shortRun = 2000
            let fs = buildSim "rambench" AsyncRAM1 true shortRun
            FastRun.runFastSimulation None shortRun fs |> ignore
            Expect.equal (liveWords fs (shortRun - 1)) 1 "every write goes to address 0"
        }

        testCase "benchmark" (fun () ->
            if not benchEnabled then
                skiptest "set ISSIE_BENCH=1 to run the RAM benchmark"
            printfn ""
            printfn "RAM benchmark: %d cycles, one write every 8, 64K x 16 memory" cycles
            printfn "  .NET speed is indicative only - measure speed in the app (simulatorStructure.md)"
            printfn ""
            if benchOnly <> "sieve" && benchOnly <> "always" then
                measureOne "sync RAM, address sweep" RAM1 false |> ignore
                measureOne "async RAM, address sweep" AsyncRAM1 false |> ignore
                measureOne "async RAM, all writes to one addr" AsyncRAM1 true |> ignore
                measureMany "100 x (256 word x 1 bit) RAMs" 100
                printfn ""
            if benchOnly = "always" then
                printfn "64K x 16 RAM, written every cycle, %d cycles" cycles
                measureAlways "  waveform arrays (no wrap)" RAM1 (cycles + 3)
                measureAlways "  step arrays (550, wrapping)" RAM1 550
                printfn ""
            if benchOnly <> "ram" && benchOnly <> "always" then
                printfn "5eratosthenes, full sieve program, step-simulator array size (550, wrapping):"
                measureSieve 550
                printfn ""
                printfn "5eratosthenes, full sieve program, waveform array size (no wrap):"
                measureSieve (cycles + 3)
                printfn "")
    ]
