/// `RamStore` against an independent model: a plain `Map` of the contents, snapshotted at every
/// step. The store replaced exactly such a Map (see [docs/dev/ramRepresentation.md]), so the Map
/// is the specification and this checks the store against it - currently, historically, and after
/// a restart.
///
/// The three ways an address reaches its history are covered separately, because they share no
/// code: a flat array up to 16 address bits, a path-compressed 16-way trie up to 32, and a Map
/// beyond that. Only the last had any coverage before, through `ComponentSemantics`'s 33-bit RAM.
module RamStoreTests

open Expecto
open CommonTypes
open RamStore

let private memOf aw ww data =
    { Init = FromData
      AddressWidth = aw
      WordWidth = ww
      Data = data
      Comments = None }

/// One configuration per addressing strategy, on each of the two value paths.
let private configs =
    [ 8, 16, "dense addressing, uint32 words"
      20, 16, "trie addressing, uint32 words"
      36, 16, "wide addressing, uint32 words"
      8, 40, "dense addressing, bigint words"
      20, 40, "trie addressing, bigint words"
      36, 40, "wide addressing, bigint words" ]

/// Deterministic, so a failure is reproducible. Values stay inside the word width because
/// everything in a step array does - the store is entitled to assume it.
let private lcg (seed: int) =
    let mutable s = uint64 seed
    fun () ->
        s <- (s * 6364136223846793005UL + 1442695040888963407UL)
        int ((s >>> 33) &&& 0x7FFFFFFFUL)

/// Addresses chosen to make the trie work: a cluster that shares every high nibble and so forces
/// leaves to be split apart level by level, plus a few far away that do not.
let private addressesFor (aw: int) =
    let top = if aw >= 20 then (1I <<< (aw - 4)) else 0I
    [ for i in 0..11 -> bigint i ]
    @ [ for i in 0..5 -> top + bigint i ]
    @ [ (1I <<< (aw - 1)) - 1I; (1I <<< aw) - 1I ]

let private readCurrent (ram: Ram) (addr: bigint) =
    if ram.WordWidth > 32 then
        readAddrBigIntDataBigInt ram addr
    else
        bigint (readAddrBigIntDataUInt32 ram addr)

let private write (ram: Ram) (step: int) (addr: bigint) (value: bigint) =
    if ram.WordWidth > 32 then
        writeAddrBigIntDataBigInt ram step addr value
    else
        writeAddrBigIntDataUInt32 ram step addr (uint32 value)

/// Drive a store and a Map of the same contents through the same writes, keeping a snapshot of
/// the Map after every step.
let private driveBoth (aw: int) (ww: int) (initial: Map<bigint, bigint>) (steps: int) (window: int) =
    let ram = ofMemory window (memOf aw ww initial)
    let addrs = addressesFor aw |> List.toArray
    let rand = lcg 20260812
    let mutable model = initial |> Map.filter (fun _ v -> v <> 0I)
    let snapshots = ResizeArray<Map<bigint, bigint>>()
    for step in 0 .. steps - 1 do
        // roughly one step in three writes, so that plenty of steps change nothing
        if rand () % 3 = 0 then
            let addr = addrs[rand () % addrs.Length]
            // a value of 0 sometimes, to exercise the live-word count going down as well as up,
            // and a repeat of the current value sometimes, to exercise write suppression
            let value =
                match rand () % 8 with
                | 0 -> 0I
                | 1 -> Map.tryFind addr model |> Option.defaultValue 0I
                | _ -> bigint (rand ()) % (1I <<< min ww 30)
            write ram step addr value
            model <- if value = 0I then Map.remove addr model else Map.add addr value model
        snapshots.Add model
    ram, addrs, snapshots

let private modelValue (model: Map<bigint, bigint>) addr =
    Map.tryFind addr model |> Option.defaultValue 0I

let tests =
    testList "RamStore" [
        for aw, ww, name in configs do
            test $"contents match a Map of the same writes ({name})" {
                let steps = 300
                let ram, addrs, snapshots = driveBoth aw ww Map.empty steps (steps + 1)
                let final = snapshots[steps - 1]
                for addr in addrs do
                    Expect.equal (readCurrent ram addr) (modelValue final addr)
                        $"current contents of address {addr}"
            }

            test $"the value at a past step is the value the Map held then ({name})" {
                let steps = 300
                let ram, addrs, snapshots = driveBoth aw ww Map.empty steps (steps + 1)
                // every step, not a sample: a binary search is easy to get wrong at the ends
                for step in 0 .. steps - 1 do
                    for addr in addrs do
                        Expect.equal (wordAt ram step addr) (modelValue snapshots[step] addr)
                            $"address {addr} at step {step}"
            }

            test $"the live word count at a past step matches ({name})" {
                let steps = 300
                let ram, _, snapshots = driveBoth aw ww Map.empty steps (steps + 1)
                for step in 0 .. steps - 1 do
                    Expect.equal (liveCountAt ram step) (Map.count snapshots[step])
                        $"live words at step {step}"
            }

            test $"toMemory rebuilds the Map exactly ({name})" {
                let steps = 200
                let ram, _, snapshots = driveBoth aw ww Map.empty steps (steps + 1)
                for step in [ 0; 1; steps / 3; steps / 2; steps - 1 ] do
                    Expect.equal (toMemory ram step).Data snapshots[step] $"whole memory at step {step}"
            }

            test $"initial contents are readable and survive a reset ({name})" {
                let initial = Map [ 0I, 7I; 3I, 9I; (1I <<< (aw - 1)) - 1I, 5I ]
                let ram = ofMemory 100 (memOf aw ww initial)
                for addr, value in Map.toList initial do
                    Expect.equal (readCurrent ram addr) value $"initial contents of {addr}"
                write ram 1 3I 42I
                Expect.equal (readCurrent ram 3I) 42I "overwritten"
                reset ram
                for addr, value in Map.toList initial do
                    Expect.equal (readCurrent ram addr) value $"{addr} back to its initial value"
                Expect.equal ram.Writes 0 "a reset store has recorded no writes"
                Expect.equal (liveCountAt ram 0) (Map.count initial) "live count back to the initial one"
            }

            test $"a write that changes nothing is not recorded ({name})" {
                let ram = ofMemory 100 (memOf aw ww Map.empty)
                write ram 1 5I 11I
                write ram 2 5I 11I
                write ram 3 5I 11I
                Expect.equal ram.Writes 1 "the two rewrites of the same value cost nothing"
                Expect.equal (readCurrent ram 5I) 11I "and the contents are still right"
                // and the value is right at a step between the suppressed writes
                Expect.equal (wordAt ram 2 5I) 11I "history is unaffected by suppression"
                write ram 4 5I 12I
                Expect.equal ram.Writes 2 "a real change is recorded"
                Expect.equal (wordAt ram 3 5I) 11I "the step before the change"
                Expect.equal (wordAt ram 4 5I) 12I "the step of the change"
            }

        // Pruning applies only when the step arrays wrap, which is the step simulator. The rule
        // that matters is keeping the last out-of-window entry: without it an address written
        // once long ago and never since loses its value entirely.
        test "pruning keeps the value of an address written before the window" {
            let window = 10
            let ram = ofMemory window (memOf 8 16 Map.empty)
            writeAddrBigIntDataUInt32 ram 1 1I 55u
            // hammer a different address so that time passes and pruning is triggered
            for step in 2..400 do
                writeAddrBigIntDataUInt32 ram step 2I (uint32 step)
            Expect.equal (readAddrBigIntDataUInt32 ram 1I) 55u
                "the one write to address 1 is still its current value"
            Expect.equal (wordAt ram 399 1I) 55I "and is still what it held at a recent step"
        }

        test "pruning bounds a hammered address's history to its window" {
            let window = 10
            let ram = ofMemory window (memOf 8 16 Map.empty)
            for step in 1..400 do
                writeAddrBigIntDataUInt32 ram step 2I (uint32 step)
            // ask a question first, so the tail is folded into the indexed runs
            wordAt ram 399 2I |> ignore
            let len =
                match ram.Addressing with
                | Dense slots when slots[2] >= 0 -> ram.Start[slots[2] + 1] - ram.Start[slots[2]]
                | Dense _ -> failtest "address 2 should have been written"
                | _ -> failtest "an 8-bit address width should be densely addressed"
            Expect.isLessThan len 100 $"history should be pruned to near the window, not {len}"
            Expect.equal (wordAt ram 399 2I) 399I "the recent value is still exact"
            Expect.equal (wordAt ram 395 2I) 395I "and so is one a few steps back"
        }

        test "a waveform-sized window never prunes" {
            let ram = ofMemory 1000 (memOf 8 16 Map.empty)
            for step in 1..400 do
                writeAddrBigIntDataUInt32 ram step 2I (uint32 step)
            for step in 1..400 do
                Expect.equal (wordAt ram step 2I) (bigint step) $"every step is still exact at {step}"
        }

        test "liveCountExceeds agrees with the exact count, and short-circuits" {
            let ram = ofMemory 10000 (memOf 8 16 Map.empty)
            for a in 0..49 do
                writeAddrBigIntDataUInt32 ram 1 (bigint a) 7u
            Expect.isFalse (liveCountExceeds ram 1 100) "50 live words does not exceed 100"
            Expect.isFalse (liveCountExceeds ram 1 50) "50 does not exceed 50"
            Expect.isTrue (liveCountExceeds ram 1 49) "50 does exceed 49"
            for a in 50..199 do
                writeAddrBigIntDataUInt32 ram 2 (bigint a) 7u
            Expect.isTrue (liveCountExceeds ram 2 100) "200 live words exceeds 100"
            Expect.isFalse (liveCountExceeds ram 1 100) "but only 50 of them at step 1"
            // zeroing words takes it back under
            for a in 0..199 do
                writeAddrBigIntDataUInt32 ram 3 (bigint a) 0u
            Expect.isFalse (liveCountExceeds ram 3 0) "nothing is live once every word is zero"
            Expect.isTrue (liveCountExceeds ram 2 100) "and step 2 is unaffected"
        }

        test "initial contents outside the address range are ignored, not written" {
            // Contents come from a .dgm, which is not guaranteed to agree with the address width it
            // declares. Such a location can never be addressed by the simulation, and seeding it
            // would index off the end of the slot array and fail the whole simulation.
            let ram = ofMemory 100 (memOf 8 16 (Map [ 3I, 9I; 999I, 5I ]))
            Expect.equal (readAddrBigIntDataUInt32 ram 3I) 9u "the location that does fit is there"
            Expect.equal (liveCountAt ram 0) 1 "and the one that does not was dropped"
        }

        test "a read-only store serves its fixed contents at any step" {
            let rom = fixedOf (memOf 8 16 (Map [ 1I, 11I; 2I, 22I ]))
            Expect.equal (wordAt rom 0 1I) 11I "step 0"
            Expect.equal (wordAt rom 999 2I) 22I "and any later step"
            Expect.equal (wordAt rom 999 3I) 0I "an address it has no entry for reads as zero"
            Expect.equal (liveCountAt rom 5) 2 "live count of a fixed memory"
            Expect.isTrue (liveCountExceeds rom 5 1) "two live words exceed one"
            Expect.isFalse (liveCountExceeds rom 5 2) "and do not exceed two"
            Expect.isFalse (liveCountExceeds rom 5 99) "nor a limit far above them"
        }
    ]
