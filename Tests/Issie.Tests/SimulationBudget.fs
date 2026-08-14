/// Refusing a simulation whose step arrays would not fit, before any of them are allocated.
///
/// The point of the check is that it happens BEFORE the memory is taken: a design asked to run for
/// more cycles than there is memory for used to allocate until the machine gave out. So the tests
/// that matter here ask for absurd numbers of cycles and expect an ordinary SimulationError back,
/// promptly, rather than a crash or a wait.
///
/// The two budgets are separate because the two memories are: buses of 32 bits and under are held
/// in Uint32Arrays outside the V8 heap, wider ones as arrays of BigInt inside it. A design of wide
/// buses can be refused while the same design in narrow buses is allowed, and the split asserted
/// below is what keeps that true.
module SimulationBudget

open Expecto
open CommonTypes
open SimTypes
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
            let cycles = int (FastCreate.Constants.maxHeapArrayBytes / 1000.0) + 1
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
    ]
