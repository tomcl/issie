/// Refusing a simulation that will not fit in memory, before any of it is allocated.
///
/// There are two ways a design exhausts memory and both are refused here. Its step arrays may be
/// too large, which is a question of how many cycles are asked for; or the design itself may expand
/// too far, since every sheet is copied into every place it is used and that multiplies down the
/// hierarchy. The second is the one that turns a small-looking project into millions of components.
///
/// The point of both checks is that they happen BEFORE the memory is taken: either used to allocate
/// until the machine gave out. So the tests here ask for absurd sizes and expect an ordinary
/// SimulationError back, promptly, rather than a crash or a wait - and a test that finishes at all
/// is a good part of what is being asserted.
///
/// The two step-array budgets are separate because the two memories are: buses of 32 bits and under
/// are held in Uint32Arrays outside the V8 heap, wider ones as arrays of BigInt inside it. A design
/// of wide buses can be refused while the same design in narrow buses is allowed, and the split
/// asserted below is what keeps that true.
module SimulationBudget

open Expecto
open CommonTypes
open SimTypes
open SimGraphTypes
open CanvasBuilder

/// A + B -> S, all three at the given bus width. Combinational, so nothing here allocates the
/// per-step state array and the heap cost is the bigint arrays alone.
let private adderSheet (name: string) (w: int) =
    let a = makeComp "a" 0 1 (Input1(w, None)) "A"
    let b = makeComp "b" 0 1 (Input1(w, None)) "B"
    let add = makeComp "add" 2 1 (NbitsAdderNoCinCout w) "ADD"
    let s = makeComp "s" 1 0 (Output w) "S"
    makeLdc name None ([ a; b; add; s ], [ conn a 0 add 0; conn b 0 add 1; conn add 0 s 0 ])

let private simulate (cycles: int) (ldc: LoadedComponent) =
    Simulator.startCircuitSimulation cycles ldc.Name ldc.CanvasState [ ldc ]

/// IN -> NOT -> OUT. Three components, and the bottom of every hierarchy below.
let private leaf =
    let i = makeComp "leaf-in" 0 1 (Input1(1, None)) "IN"
    let n = makeComp "leaf-not" 1 1 Not "N"
    let o = makeComp "leaf-out" 1 0 (Output 1) "OUT"
    makeLdc "leaf" None ([ i; n; o ], [ conn i 0 n 0; conn n 0 o 0 ])

/// A sheet holding `copies` instances of `child` in a chain from IN to OUT, so that every instance
/// is properly driven and the sheet passes the checks that run before the size is looked at.
let private chainSheet (name: string) (child: LoadedComponent) (copies: int) =
    let inp = makeComp $"{name}-in" 0 1 (Input1(1, None)) "IN"
    let out = makeComp $"{name}-out" 1 0 (Output 1) "OUT"
    let instances =
        [ for i in 1..copies ->
            makeComp $"{name}-c{i}" 1 1 (customOf child [ "IN", 1 ] [ "OUT", 1 ] None) $"C{i}" ]
    let chain = (inp :: instances) @ [ out ]
    let conns = chain |> List.pairwise |> List.map (fun (a, b) -> conn a 0 b 0)
    makeLdc name None (chain, conns)

/// `levels` sheets stacked on the leaf, each instantiating the one below it `copies` times.
/// Small on disk however deep it goes - which is the whole point.
let private hierarchy (levels: int) (copies: int) : LoadedComponent * LoadedComponent list =
    ((leaf, [ leaf ]), [ 1..levels ])
    ||> List.fold (fun (child, all) k ->
        let sheet = chainSheet $"lvl{k}" child copies
        sheet, sheet :: all)

let private expandedCount (levels: int) (copies: int) =
    let top, sheets = hierarchy levels copies
    GraphMerger.expandedComponentCount top.Name top.CanvasState sheets

let private costOf (w: int) =
    match simulate 10 (adderSheet "budget" w) with
    | Ok simData -> simData.FastSim.StepCost
    | Error e -> failtest $"Simulation failed: %A{e}"

let tests =
    testList "SimulationBudget" [
        test "a design of narrow buses costs typed array memory and no heap" {
            let cost = costOf 8
            Expect.isGreaterThan cost.TypedArrayBytes 0 "8-bit buses are held in Uint32Arrays"
            Expect.equal cost.HeapBytes 0
                "nothing here is wider than 32 bits or clocked, so nothing is held in the heap"
        }

        test "a design of wide buses costs heap memory and no typed array memory" {
            let cost = costOf 64
            Expect.equal cost.TypedArrayBytes 0 "64-bit buses are not held in Uint32Arrays"
            Expect.isGreaterThan cost.HeapBytes 0 "they are held as arrays of BigInt, in the heap"
        }

        test "widening a bus past 32 bits moves its cost from one memory to the other" {
            // the same four ports either side of the boundary: what changes is which budget pays
            let narrow = costOf 32
            let wide = costOf 33
            Expect.equal narrow.HeapBytes 0 "32 bits is still a Uint32Array"
            Expect.equal wide.TypedArrayBytes 0 "33 bits is not"
            Expect.isGreaterThan wide.HeapBytes narrow.TypedArrayBytes
                "a bigint step costs more than a uint32 one: a reference and the object it points at"
        }

        test "a wider bus costs more per cycle than a narrower one" {
            Expect.isGreaterThan (costOf 128).HeapBytes (costOf 64).HeapBytes
                "128 bits needs two 64-bit digits where 64 bits needs one"
        }

        test "an ordinary length of simulation is allowed" {
            match simulate 3000 (adderSheet "ok" 16) with
            | Ok _ -> ()
            | Error e -> failtest $"a 3000 cycle simulation of four ports should fit: %A{e}"
        }

        test "a simulation too large to fit is refused rather than attempted" {
            // ~16 bytes a cycle, so this asks for tens of GB. It must come back as an error, and it
            // must come back at once - if it allocated first there would be nothing to come back to.
            match simulate 2_000_000_000 (adderSheet "toobig" 16) with
            | Error _ -> ()
            | Ok _ -> failtest "a simulation needing tens of GB of step arrays should be refused"
        }

        test "the number of cycles the refusal offers is one that is actually allowed" {
            // the message names a number, and the waveform simulator's configuration repeats it, so
            // it has to be a number the check itself accepts - and the next one up must not be
            let cost = costOf 16
            let fits = FastCreate.maxCyclesFor cost
            Expect.isOk
                (FastCreate.checkSimulationFits fits cost)
                "the largest number of cycles reported must itself be accepted"
            Expect.isError
                (FastCreate.checkSimulationFits (fits + 1) cost)
                "and one more must not be"
        }

        test "each budget is applied to its own memory" {
            // a cost that is large in one memory and nothing in the other is measured against that
            // memory's budget alone, so the two cannot mask each other
            let typedOnly = { TypedArrayBytes = 1000; HeapBytes = 0 }
            let heapOnly = { TypedArrayBytes = 0; HeapBytes = 1000 }
            let cycles = int (SimulationBudget.maxHeapBytes / 1000.0) + 1
            Expect.isError (FastCreate.checkSimulationFits cycles heapOnly)
                "past the heap budget, with no typed array use at all"
            Expect.isOk (FastCreate.checkSimulationFits cycles typedOnly)
                "the same size in typed arrays is still within the larger budget for them"
        }

        test "a real design at the waveform simulator's default length is nowhere near the limit" {
            // The budget exists to stop a design exhausting memory, not to stop ordinary work. This
            // is a 16-bit CPU across 18 sheets - a large design by the standards of what Issie is
            // used for - and the default wave simulation must not come close to being refused.
            let ldcs = TestFixtures.loadProject "3cpu"
            let top = ldcs |> List.find (fun ldc -> ldc.Name = "eep1")
            let defaultLength = 3003 // WSConfig.LastClock of 2000, plus the overflow ModelHelpers adds
            match Simulator.startCircuitSimulation defaultLength "eep1" top.CanvasState ldcs with
            | Error e -> failtest $"the default wave simulation of a real CPU was refused: %A{e}"
            | Ok simData ->
                let cost = simData.FastSim.StepCost
                let allowed = FastCreate.maxCyclesFor cost
                printfn "  3cpu/eep1: %d bytes per cycle (%d typed, %d heap), %d cycles allowed"
                    cost.TotalBytes cost.TypedArrayBytes cost.HeapBytes allowed
                Expect.isGreaterThan allowed (defaultLength * 10)
                    "a real design must have room for far more than the default simulation length"
        }

        test "a design with nothing to store is not refused" {
            // guards the division: a cost of zero bytes a step must not be a division by zero
            Expect.isOk
                (FastCreate.checkSimulationFits 1_000_000 { TypedArrayBytes = 0; HeapBytes = 0 })
                "no step arrays means no limit on how long it may run"
        }

        //--------------------------------------------------------------------------------------//
        // The expanded design, which is the other thing that exhausts memory                    //
        //--------------------------------------------------------------------------------------//

        test "the expanded size of a hierarchy is counted, not the sheets on disk" {
            // leaf is 3 components. Each level is IN + OUT + `copies` instances, and holds a whole
            // expanded copy of the level below per instance, so the count multiplies.
            Expect.equal (expandedCount 0 2) 3.0 "the leaf alone"
            Expect.equal (expandedCount 1 2) (4.0 + 2.0 * 3.0) "one level of two instances"
            Expect.equal (expandedCount 2 2) (4.0 + 2.0 * 10.0) "two levels: 24, not 3 + 4 + 4"
            Expect.equal (expandedCount 3 8) (10.0 + 8.0 * (10.0 + 8.0 * (10.0 + 8.0 * 3.0)))
                "eight-way, three deep"
        }

        test "a hierarchy that fits is simulated" {
            let top, sheets = hierarchy 3 8
            match Simulator.startCircuitSimulation 10 top.Name top.CanvasState sheets with
            | Ok _ -> ()
            | Error e -> failtest $"a design expanding to ~2000 components should simulate: %A{e}"
        }

        test "a hierarchy too large to expand is refused, without expanding it" {
            // Eight sheets, none of them big, expanding to millions of components. The test
            // finishing at all is most of the assertion: were the design built before being
            // measured, this would take the machine down rather than fail.
            let top, sheets = hierarchy 7 8
            let predicted = GraphMerger.expandedComponentCount top.Name top.CanvasState sheets
            Expect.isGreaterThan predicted 5_000_000.0 "this hierarchy expands to millions"
            match Simulator.startCircuitSimulation 10 top.Name top.CanvasState sheets with
            | Ok _ -> failtest "a design expanding to millions of components should be refused"
            | Error e ->
                match e.ErrType with
                | GenericSimError msg ->
                    Expect.stringContains msg "components" "the refusal says what the size is in"
                    Expect.stringContains msg "lvl" "and names the sheets the size comes from"
                | other -> failtest $"expected a plain simulation error, got %A{other}"
        }

        test "the expansion is measured in ports as well as components" {
            // What a component costs depends on how many ports it has - the FastComponent holds a
            // link, a driver and an output array per port - so a sheet of wide gates must be
            // charged more than a sheet of the same number of narrow ones.
            let gateSheet (name: string) (fanIn: int) =
                let inp = makeComp $"{name}-in" 0 1 (Input1(1, None)) "IN"
                let g = makeComp $"{name}-g" fanIn 1 (GateN(And, fanIn)) "G"
                let out = makeComp $"{name}-out" 1 0 (Output 1) "OUT"
                let feed = [ for i in 0 .. fanIn - 1 -> conn inp 0 g i ]
                makeLdc name None ([ inp; g; out ], feed @ [ conn g 0 out 0 ])

            let narrow = gateSheet "narrow" 2
            let wide = gateSheet "wide" 8
            let compsN, portsN = GraphMerger.expandedSize "narrow" narrow.CanvasState [ narrow ]
            let compsW, portsW = GraphMerger.expandedSize "wide" wide.CanvasState [ wide ]
            Expect.equal compsN compsW "both sheets have the same three components"
            Expect.isGreaterThan portsW portsN "but the wide one has six more ports"
            Expect.isGreaterThan
                (SimulationBudget.heapBytesPerComponent (portsW / compsW))
                (SimulationBudget.heapBytesPerComponent (portsN / compsN))
                "so a component of the wide sheet is charged more than one of the narrow sheet"
        }

        test "counting a hierarchy is linear in its sheets, not in its expansion" {
            // 20 levels of 8 is 8^20 components - more than there are atoms to store them in. The
            // count must still come back, and come back as a number rather than an overflow.
            let top, sheets = hierarchy 20 8
            let predicted = GraphMerger.expandedComponentCount top.Name top.CanvasState sheets
            Expect.isGreaterThan predicted 1.0e18 "8^20 is a very large number of components"
            Expect.isTrue (System.Double.IsFinite predicted) "and it must be a number, not infinity"
        }

        //--------------------------------------------------------------------------------------//
        // Where a waveform simulation starts.
        //
        // Fitting in memory is not the same as being worth waiting for. A design can pass every
        // check above and still spend minutes building step arrays and wave records before showing
        // anything, so a big one starts at a clock count it can be started in and says so.
        //--------------------------------------------------------------------------------------//

        test "a big design starts short and a small one starts where it was asked to" {
            let budget = SimulationBudget.maxHeapBytes
            let atShare share = ModelHelpers.startingLastClock 2000 (share * budget)
            Expect.equal (atShare 0.5) ModelHelpers.Constants.shortStartClock
                "half the heap budget is a design to start short"
            Expect.equal (atShare 0.001) 2000 "a design of no size starts at what it was configured for"
            Expect.equal (ModelHelpers.startingLastClock 2000 0.0) 2000
                "and so does one whose size is not known"
        }

        test "below the threshold the configuration is the only limit on the start" {
            // This used to taper as 40/share below the threshold, which read as a hard limit
            // nobody had asked for: any design past a few dozen components was quietly started
            // below its configured cycle count. The cap is now the threshold alone - the
            // configuration dialog already bounds what may be asked for by what fits in memory.
            let budget = SimulationBudget.maxHeapBytes
            let atShare share = ModelHelpers.startingLastClock 1_000_000 (share * budget)
            Expect.equal (atShare 0.19) 1_000_000
                "just under the threshold the configured value stands, however large"
            Expect.equal (atShare 0.21) ModelHelpers.Constants.shortStartClock
                "and just over it the design starts short"
        }

        test "a real CPU asked for a million cycles is started at a million cycles" {
            // the regression this guards: 3cpu/eep1 - about 350 expanded components - was being
            // started at ~75,000 of a configured 1,000,000 cycles by the 40/share taper
            let ldcs = TestFixtures.loadProject "3cpu"
            let top = ldcs |> List.find (fun ldc -> ldc.Name = "eep1")
            let estimate = GraphMerger.expandedHeapEstimate "eep1" top.CanvasState ldcs
            Expect.isGreaterThan estimate 0.0 "the design must price as something"
            Expect.equal (ModelHelpers.startingLastClock 1_000_000 estimate) 1_000_000
                "the start honours the configuration; memory alone is what may refuse it"
        }

        test "a design is never started at more cycles than it asked for" {
            let budget = SimulationBudget.maxHeapBytes
            Expect.equal (ModelHelpers.startingLastClock 50 (0.5 * budget)) 50
                "a configuration below the short start is left alone"
            Expect.equal (ModelHelpers.startingLastClock 50 (0.0001 * budget)) 50
                "and so is one on a design of no size"
        }

        test "the step arrays carry a zoom margin the simulation could use, not the largest there is" {
            let ws lastClock zoom =
                { ModelHelpers.initWSModel with
                    WSConfig = { ModelHelpers.initWSModel.WSConfig with LastClock = lastClock }
                    SamplingZoom = zoom }
            // The margin covers reading one sample past the end at the multiplier in use.
            Expect.equal (ModelHelpers.Constants.waveSimRequiredArraySize (ws 2000 1)) (2000 + 3 + 1000)
                "a long simulation can be zoomed out the whole way, so it keeps the whole margin"
            Expect.equal (ModelHelpers.Constants.waveSimRequiredArraySize (ws 200 1)) (200 + 3 + 200)
                "a 200-cycle simulation used to allocate 1203 cycles of arrays for zooms it cannot use"
            Expect.equal (ModelHelpers.Constants.waveSimRequiredArraySize (ws 200 500)) (200 + 3 + 500)
                "the multiplier actually in use is covered whatever the configuration has shrunk to"
        }
    ]
