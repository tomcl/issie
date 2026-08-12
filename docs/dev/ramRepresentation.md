# How a RAM is represented in the fast simulator

Phase 1 of this is **implemented**: `Simulator/RamStore.fs` holds a memory's contents, and the
`Map<bigint,bigint>` snapshot per clock step is gone. What follows sets out what the old
representation cost, what replaced it, what that measured, and what is deliberately left for
phase 2.

## Measured

`Tests/Issie.Tests/RamBenchmark.fs` drives a 64K x 16 RAM with one write every eight clocks, the
address advancing per write so a long run touches all 65536 words. Median of five under .NET after
a discarded warm-up run, against the same harness on the commit before the change. **.NET speed is
indicative only** - `simulatorStructure.md` records one change measuring 2.5x under .NET and 11.9x
in V8 - but the space figures are real.

| 64K x 16, 1 write in 8 | cycles | before | after |
|---|---:|---:|---:|
| sync RAM, address sweep | 524 288 | 339 cycles/ms, 97.5 MB | **1.3 MB** |
| async RAM, address sweep | 524 288 | 337 cycles/ms, 97.5 MB | **1.3 MB** |
| all writes to one address | 524 288 | 2465 cycles/ms, 26.2 MB | **0.9 MB** |
| sync RAM, address sweep | 1 048 576 | 396 cycles/ms, 190.3 MB | **1.3 MB** |
| async RAM, address sweep | 1 048 576 | 360 cycles/ms, 190.3 MB | **1.3 MB** |
| all writes to one address | 1 048 576 | 1423 cycles/ms, 52.4 MB | **1.6 MB** |

Speed measured 3000-16000 cycles/ms across these, against 340-2500 before, but the .NET spread
between repetitions of the *same* configuration is as much as 5x, so the only safe reading is
"an order of magnitude, direction certain, figure not". Space is stable to the tenth of a megabyte.

**Retained space no longer tracks run length**: 1.3 MB at 524 288 cycles and at 1 048 576 alike,
where the old representation doubled with the run. That is 65536 addresses at 8 bytes an entry
plus a 256 kB offset table - against 190 MB. What is left scales with *entries actually written*,
and the one-address rows show it doing exactly that: 8 bytes a write, nothing else.

### The whole design: 5eratosthenes

The two above isolate a RAM. This one does not: the `5eratosthenes` demo is the EEP1 CPU, 349
component reductions a clock, and the RAM is one of them. It is the benchmark
`simulatorStructure.md` names. Run for **1 000 000 cycles** of the full `sieve` program - the demo
ships linked to `sievesmall`, which halts under 25 000 cycles and then spins in a self-jump, so
`RamBenchmark` relinks the ROM - and reaching 51 246 words of the sieve array written, which is
what says the CPU is computing rather than idling.

| 1 000 000 cycles | before | after |
|---|---:|---:|
| step simulator (550 steps, wrapping) | 176-282 cycles/ms, 3.5 MB | 232-432 cycles/ms, **1.9 MB** |
| waveform (1 000 003 steps, no wrap) | 144 cycles/ms, **153.5 MB** | 161 cycles/ms, **2.0 MB** |

Two honest readings of that.

**Space, in the mode this work is about: 153.5 MB down to 2.0 MB, 77x.** The waveform simulator
sizes its arrays for the whole run and never wraps, so the old representation kept every version
of the memory it had ever made. The step simulator wraps at 550, which bounded the old cost to
that window, and there the difference is only 3.5 MB against 1.9 MB - the change matters most
exactly where the old design was worst.

**Speed: about 1.1x, and that is what it should be.** RAM is ~11% of this design's simulation time
(`simulatorStructure.md`), so removing all of it entirely would be 1.12x - Amdahl, not a
disappointment. The synthetic sheets above show 10x and more because there the RAM is most of the
work. Individual paired runs here ranged from 0.8x to 2.7x, so only the aggregate of several says
anything at all; single .NET runs of this benchmark should not be quoted.

**What it does not fix.** The step arrays for this design are 1488 bytes a step, so a genuine
1 000 000-cycle waveform run needs ~1.5 GB for those alone, whatever the RAM does. That is the
phase 2 argument in one number: RAM was the largest single term and is now a rounding error, and
the per-step retention of everything else is what stands between this design and a long waveform
simulation.

### Many small memories

The case that gains most is not the big RAM. A design of **a hundred 256-word by 1-bit RAMs** -
3.2 kB of memory between them, and a size real designs use - run for 100 000 cycles at one write
every eight clocks, the data bit toggling once per pass over the address space so that every write
is a real change:

| | before | after |
|---|---:|---:|
| 100 x (256 x 1) RAMs, 100 000 cycles | 7.5 cycles/ms, **1055 MB** | **11.8 MB** |

The old figure is over a gigabyte for 3.2 kB of memory at a twentieth of the wave simulator's
cycle limit, against a 3.6 GB heap (`Main.fs:63`) - such a design could not have been
waveform-simulated far at all. The per-step cost was paid *per RAM*: a hundred `RamState` boxes
and a hundred `Memory1` records every clock.

This is also what settled two design questions. The "packed" representation this note originally
proposed for small memories was never built: packing addresses the *current contents*, 1 kB per
small RAM, where the cost is history. And a growable list per address - the first implementation -
cost a record and two list objects for what is often a single 8-byte write, which measured 31.5 MB
here where CSR measures 11.8 MB.

What is left is close to irreducible: this design does 1.25 million real writes, and 11.8 MB is
about 8 bytes for each of them. Stretch it to 1 048 576 cycles and 13.1 million writes and it
holds 131 MB, which is the same 8 bytes a write. Keeping every write addressable from the
waveform viewer costs that; the only lever left is not keeping it.

## What it used to be

`Memory1.Data` is `Map<bigint,bigint>` (`CommonTypes.fs:206`) — an FSharp.Core AVL tree with heap
`BigInt` keys *and* values, sparse, absent addresses reading as zero. A `RamState (Memory1)` is
written into the component's step array on **every clock step**, whether or not anything changed
(`EvalReference.fs:1097`, `:1118`, `:1127`, `:1136`, `:1160`, and the `AsyncRAM1` cases; `putState`
at `:96-100`).

Per read (`EvalKernel.fs:57` → `Helpers.fs:293`): allocate a `BigInt` from the address,
`Map.tryFind`, convert back. Per write (`EvalKernel.fs:90`): a read *as well*, since `RAM1` returns
the old contents (`EvalReference.fs:1095`), plus two more `BigInt`s from `twosComp`, the AVL spine
path-copied, a `{ mem with … }` record copy and a `RamState` wrapper. `AsyncRAM1` is a hybrid,
reduced twice per step (`EvalReference.fs:1139-1167`), so it pays two `getRamStateMemory` lookups
per step for one `putState`.

Sizes in V8 with pointer compression: `MapTreeNode$2 {k,v,left,right,h}` ≈ 32 B, `MapTreeLeaf$2`
≈ 24 B, a one-digit `BigInt` 16 B, `Memory1` ≈ 32 B, `RamState` ≈ 24 B. That is **~62 B of heap per
live word**, and **~540 B retained per write**.

### The retention is unbounded in the mode that matters

The step simulator wraps at `MaxArraySize = 550` (`SimulationView.fs:38`), so its history is
bounded. **The waveform simulator does not wrap.** Its arrays are sized for the whole run,
`LastClock + 3` — 2003 by default (`ModelHelpers.fs:26-33`) and up to
`maxSimulationSize = 4_000_000` (`:35`). This is stated at `WaveSimSelect.fs:577-579` and enforced
at `WaveSimTop.fs:125`. Every snapshot ever written stays reachable.

Retained heap for one 64K × 16 RAM read every cycle:

| wave-sim run | 1 write/10 | 1 write/5 |
|---:|---:|---:|
| 2 000 cycles (default) | ~230 kB | ~350 kB |
| 100 000 cycles | ~11.6 MB | ~17.6 MB |
| 4 000 000 cycles (the limit) | **~440 MB** | **~660 MB** |

Against V8's 3.6 GB old-space cap (`Main.fs:63`) — and
`FastValidate.calculateTotalSimArraySizePerStep` (`:20-31`) sums only `fc.Outputs`, so the
"estimated GB" warning the user sees (`UIPopups.fs:423-453`) omits this term entirely.

Allocation rate is ~95–190 B per clock for one RAM, against a documented steady state of about
1 byte per clock for the whole rest of the simulator (`simulatorStructure.md:152`). On the sieve
the RAM is ~11% of CPU — 7.5 µs per clock over 349 components is 21.5 ns for an average component,
so ~0.8 µs for the one RAM, roughly 38× an average component.

## What replaced it

`Simulator/RamStore.fs`. Current contents are mutable, and every address that is ever written is
given a **slot** number whose writes are one contiguous run of shared flat arrays - compressed
sparse row. There is no snapshot per step, and no global write log either: indexing history by
address rather than by time is what makes reading a past value a search whose cost does not depend
on how far the cursor moved.

```fsharp
/// How an address is turned into its slot number. The *only* place the strategies differ:
/// everything after it works on slot numbers.
type Addressing =
    | Dense of int array                      // slot per address, AddressWidth <= 16
    | Sparse of NodeOrLeaf option array * int // a path-compressed 16-way trie, 17..32
    | Wide of Map<bigint, int> ref            // over 32 bits: addresses arrive as bigints
    | Fixed                                   // a ROM: contents are part of the component type

// slot s owns IStep/IVal[Start[s] .. Start[s+1]), ascending by step; recent writes wait in a
// tail that `compact` folds in
Start: int array
IStep: int array
IVal:  int array   // or IBig, chosen once by word width, as IOArray splits UInt32Step/BigIntStep
```

- **Read** — `Words[addr]` in the dense case, which is every RAM a design can really simulate. No
  BigInt, no comparison, no `Option`. The address is within `AddressWidth` by the masking
  invariant, so it always indexes the array. The other cases read `CurVal[slot]`.
- **Write** — read the old value, and if it differs append `(slot, step, value)` to the tail.
  **8 bytes** once compacted, no GC object. The read was needed anyway: a RAM outputs the contents
  it replaced.
- **Per step — nothing at all.** The state array still exists and is still written every step, but
  what it holds is the *same* `RamState` object every time, so the write is one array store and
  allocates nothing. That was not free: constructing `RamState store` per step is a union-case
  allocation, and leaving it in cost 24 bytes a clock and made retained space grow with run length
  exactly as the old code did. Reducers now fetch the state object and put the same one back.
- **Value at a past step** — binary search of that slot's run. Independent of the cursor's history.
- **`Memory1` is materialised only when something asks for one**, so `WaveSimRams`,
  `MemoryEditorView` and the golden tests keep the type they had.

### Why CSR and not a list per address

The obvious layout - a growable list of writes hanging off each address - was the first
implementation and is 2 to 12 times worse. A record plus two list objects plus an option wrapper
is around 230 bytes, paid for every address ever written, whatever it holds; a single 8-byte write
costs 230 bytes to record. Measured: a fully written 64K RAM held 15.1 MB that way and holds
1.3 MB as CSR; a hundred small RAMs held 31.5 MB and hold 11.8 MB.

The cost of CSR is that a slot's run has to stay contiguous, so writes cannot simply be appended
to it. They go to a small tail of `(slot, step, value)` triples and `compact` folds the tail into
the runs with a counting sort, once the tail is both 4096 long and at least as long as what is
already indexed - doubling, so the copying is amortised O(1) a write. Queries fold in whatever is
outstanding first, which happens at most once per render since writes and renders do not interleave.

**Pruning happens during compaction rather than per write**, which is the other thing CSR forces
and an improvement: dropping an entry from the middle of a run would mean shifting everything
after it, whereas a compaction is rebuilding the runs anyway.

### The live-word count is a question, not a number

The RAM table chooses between listing every non-zero location and a windowed display. It never
shows the count, so it never needs one - `liveCountExceeds ram step limit` is enough, and it is
much cheaper than the number:

- if fewer addresses have *ever* been written than the limit, the answer is no, with no work at all;
- otherwise the walk stops as soon as the limit is passed, which for a memory with anything in it
  is after about `limit` slots.

Keeping an exact count meant journalling every crossing of zero, which for a 1-bit memory is every
single write - 10 MB of the 31.5 MB the hundred small RAMs used to hold. `liveCountAt` survives for
tests and diagnostics, where an exact number is what is being checked.

### What was planned and is not there

- **No packed representation.** It was in the plan for memories under 20 bytes, holding the whole
  RAM in at most four int32s copied on write. Once the step slot became a reference to a mutable
  store rather than a snapshot, packing stopped being an improvement and became a regression: a
  copy on write retains one array per *writing step*, so a long run costs tens of megabytes where
  an entry costs 8 bytes. It also aims at the wrong thing - the current contents of a 256 x 1 RAM
  are 1 kB and the history is megabytes.
- **No undo log, and no backward replay.** Entries hold the value *after* each write. An undo log
  is the natural form when there is a single global replay order; indexed by address there is no
  such order, so the last entry is the current value and value-at-step-N is a binary search with
  no replay at all. Materialising a whole `Memory1` walks the slots and searches each, which is
  fine because it is rare.

### Random access to history is the hard part

The requirement is that a render needs of the order of **1000 word lookups at an arbitrary step**,
and the cursor may have moved anywhere since the last one: `SamplingZoom` runs to 1000
(`ModelHelpers.fs:25`), so consecutive displayed cycles are far apart and locality buys little.

The `Map`'s O(1) is the *handle* — `extractFastSimulationState` indexes the step array once. Every
lookup after that is a `Map.tryFind` whose cost depends on tree depth. Two mechanisms can replace
it, and **both are needed, because they fail on opposite patterns**:

- **The per-address index** — binary search the address's history for the latest entry at or before
  N. O(log w_a), no seek, and a render does one search per *distinct written address* rather than
  one per lookup. This is the mechanism for reading words.
- **Backward replay** — restore `LogOld` while `LogStep > N`, four typed-array operations per
  intervening write. This is kept only for materialising a *whole* `Memory1`, where searching every
  live address separately would cost O(live × log w) — 64K addresses at ~4 probes is a millisecond
  or two, against a replay that is usually far shorter.

Estimates for a 4M-cycle run of a 64K × 16 RAM, 400 000 writes:

| | writes spread over 64K addresses | all writes to *one* address |
|---|---|---|
| tree depth / bucket length | deep tree, ~6-entry buckets | **one-node tree**, one 400 000-entry bucket |
| `Map`, one lookup | ~250–650 ns | ~15 ns |
| replay, one lookup at a random cycle | ~130–270 µs | ~130–270 µs (worst ~0.8 ms) |
| index, one lookup | ~30 ns | ~100–200 ns |
| `Map`, 1000-lookup render | ~0.25–0.65 ms | ~15 µs |
| index, 1000-lookup render | ~10–20 µs | ~2 µs |
| retained | ~440 MB | ~130 MB |

Concentrated writes make the `Map` *fast* — its tree is one node — while maximising the journal, so
replay loses by four orders of magnitude there. Spread writes do the reverse. Nothing tells you in
advance which a design will produce, so the index is not optional. Replay remains the mechanism for
the whole-memory query (`extractStatefulComponents`, and any materialised `Memory1`).

**Hold the index as a compacted CSR, not as per-address growable arrays.** A plain SMI-packed JS
array costs ~16 B for the `JSArray`, ~8 B of `FixedArray` header and ~25% capacity slack, and two
are needed per written address — roughly 4 MB of pure overhead at 64K addresses, against 3.2 MB of
actual history. Flat arrays with an offset table have none of it. To keep appends O(1), leave the
recent journal tail out of the CSR and scan it **once per render** rather than once per lookup,
recompacting when it grows past a fraction of the total.

Two cheap refinements: store steps in the bucket rather than journal indices (8 B per write instead
of 4 B, but one load per probe rather than two dependent ones); and keep a per-address cursor hint
and gallop outward from the last hit, which makes a nearby cycle ~2–4 probes instead of ~19.

## Which of three representations, chosen by size

A flat array of one slot per address is sized `2^AddressWidth`, and **nothing caps a RAM's address
width**: `CatalogueView.fs:1128` limits only multipliers to 16 bits, the general dialog rejects
only `addressWidth < 1` (`:1139`), and `Helpers.getMemData`'s assertion contemplates
`AddressWidth > 63`. So the way an address reaches its history is chosen when the simulation is
built:

| address width | representation |
|---|---|
| ≤ 16 | **dense**: a mutable `Words` array of current contents, and a slot per address |
| 17..32 | **sparse**: a path-compressed 16-way trie, keyed on the address as a `uint32` |
| over 32 | **wide**: a `Map<bigint, History>`, since the addresses arrive as bigints |

The last is a guard rather than a design - a memory of more than 4G words cannot be meaningfully
simulated - but it means the `Map<bigint,bigint>` of contents is gone from the simulator
altogether, rather than surviving as a fallback for one case.

A 64K-word dense array is 256 kB, which is always affordable; beyond that the address space
outruns what can be allocated, so above 16 address bits the memory is assumed to be occupied
thinly and the address→history step becomes a trie instead of a flat offset table. Occupancy is
irrelevant *below* the threshold: a `Uint32Array(65536)` is 256 kB whether 10 words are live or all
of them, already better than ~62 B per live word.

All three are one mechanism — an address's writes held as a step-ordered history and binary
searched — differing only in how an address reaches its history. Only the dense case also keeps a
`Words` array, so that a read during simulation is a single indexed load rather than a walk to the
end of a history.

That is enforced by the layering rather than left to discipline, in three steps:

| | |
|---|---|
| `historyOf` / `historyFor` / `historyOfBig` / `historyForBig` | the *only* place the addressing strategies are told apart |
| `lastU` / `lastBig`, `appendU` / `appendBig` | what a read and a write do to a history once it is found — pruning, appending, live-word accounting |
| the eight `read`/`write` entry points | thin: pick the lookup, pick the value form |

So a change to how writes are recorded — pruning, suppression, the count journal, and the CSR
compaction when it comes — is one edit, not one per strategy. The only split that survives is
uint32 values against bigint values, which is the split `IOArray` already makes between
`UInt32Step` and `BigIntStep` and which cannot be merged without boxing the fast path.

There is precedent for the dense form: `EvalCompiled.romTable` (`:84-97`) already flattens ROMs
into a `uint32 array`, capped at `maxRomTableAddressWidth = 16`.

### Histories hold new values, not old ones

An undo log - storing the value each write replaced - is the natural form when there is a single
global replay order. Indexed **by address** there is no such order, so a history stores the value
*after* each write instead. The last entry is then the current value and value-at-step-N is a
binary search with no replay at all, which is what makes the cost independent of where the cursor
was. Materialising a whole `Memory1` walks the histories rather than replaying anything.

### The sparse case: a write-once mutable 16-way trie

Above 16 address bits, replace `IndexStart` with a trie on the address, 4 bits per level:

```fsharp
type NodeOrLeaf =
    /// 16 children, indexed by the next 4 bits of the address. A slot is filled once and
    /// never repointed, so there is no path copying and no version to keep.
    | Node of NodeOrLeaf option array
    /// One address's write history: steps ascending, values alongside. Addr is the full
    /// address, checked on arrival because a leaf may sit above its natural depth.
    | Leaf of addr: int64 * steps: ResizeArray<int> * values: ResizeArray<uint32>
```

Under Fable an `option` of a DU erases to `child | undefined`, so a `Node` is a plain 16-element JS
array with no wrapper per slot. Nodes are allocated once and mutated only by filling an empty slot,
so the structure grows in place — this is what removes the per-write path copy that the `Map` pays.

- **Simulation read** — descend, then take the last entry of the leaf. A few dependent loads
  against the `Map`'s comparisons on boxed `bigint`.
- **Write** — descend, append `(step, value)`. Amortised O(1), ~8 B, and a new node only the first
  time a subtree is touched.
- **Historical read** — binary search the leaf. O(log w_a), independent of the cursor, exactly as
  in the dense case.
- **No per-step snapshot**, as everywhere else here.

**Path compression is what makes it sparse, and is the only reason the union is needed.** Depth is
`ceil(AddressWidth / 4)` and known when the simulation is built, so without compression you would
know leaf from node by level and `NodeOrLeaf` would be dead weight. Worse, a fixed depth-8 trie for
a 32-bit address costs on the order of 550 kB of skeleton for 1000 live words. Hoisting a `Leaf`
whose subtree holds a single address — hence `addr` in the leaf, checked on arrival, and a split
when a second address collides — collapses the depth to about `log16(live addresses)` and the
skeleton to ~115 kB.

Worth knowing and not adopted: bitmap-compressed nodes, HAMT style, with a 16-bit occupancy mask
and a packed child array, cut an almost-empty node from ~88 B to ~20 B. Inserting a child then
reallocates that array, which is not literally write-once, though it still introduces no path
copying. Take it only if the skeleton is measured to matter.

For a 32-bit-address RAM with 1000 live words over a 4M-cycle run at 400 000 writes:

| | `Map` | trie, phase 1 | trie, phase 2 |
|---|---:|---:|---:|
| base structure | 62 kB | ~115 kB skeleton | ~115 kB |
| writes | 216 MB | 3.2 MB | 3.2 MB |
| per step | 96 MB | 96 MB | 0 |
| **total** | **~312 MB** | **~99 MB** | **~3.5 MB** |

The skeleton is larger per live address than the `Map`'s tree and it does not matter: the terms
that dominate are per-write and per-step. Note how much of what is left in phase 1 is the per-step
box, and see the phase 2 note below for why removing it is not a job for this change alone.

### Pruning, which the step simulator needs and the obvious rule gets wrong

Steps recorded in a history are absolute, but the step simulator wraps at `MaxArraySize = 550`, so
without pruning the histories grow forever while only 550 steps remain reachable — a leak relative
to today, where the circular buffer bounds it. Prune on append, dropping entries older than
`ClockTick - MaxArraySize`, **but always keep the most recent out-of-window entry**: an address
written once at step 5 and read at step 10 000 000 has its only record there, and dropping it loses
the value. So each history holds one base entry plus everything inside the window, and pruning is
amortised O(1). In the waveform simulator, where the arrays are sized for the whole run and never
wrap, nothing is ever out of window and nothing is pruned.

### Three details the trie changes

- **Initial contents** are loaded into the trie when the simulation is built, one base entry per
  initialised address, so every read is a trie read. Falling back to the initial `Memory1` for
  addresses never written would put a `Map` lookup back on the hot path of exactly the ROM-like
  memories that need it least.
- **`LogCount`** has no flat journal to sit in, so the live-word count gets its own small
  `(step, count)` pair of arrays, appended only when the count changes.
- **Restart** truncates every history rather than resetting one length, so it is O(live addresses)
  instead of O(1). Restarts are rare, and in the waveform simulator absent.

## A write that changes nothing is not a write

This applies to **all four representations, including the one there is now**, and it is the
cheapest thing on this page: before recording a write, compare the incoming data with what is
already at that address, and if they are equal do nothing at all.

The read is already being done — `RAM1` returns the old contents as its output
(`EvalReference.fs:1095`), so the comparison is one extra test on a value the reducer holds. What
it saves per suppressed write:

| | saved |
|---|---|
| present `Map` | the `Map.add` path copy, the `Memory1` record, two `BigInt`s — ~540 B |
| packed | the array copy |
| dense | the journal entry, and so a shorter history for every later search and replay |
| sparse trie | the history append, and possibly a whole subtree if the address is new |

Take it one step further: on a step with no *effective* write, store the **previous step's state
object** rather than a fresh one — `arr.Step[simStep] <- arr.Step[simStepOld]` — which allocates
nothing. That removes the unconditional `RamState` box (`EvalReference.fs:1097`) that costs 24 B on
every step of every RAM today, and it is worth doing to the current implementation on its own,
ahead of any of the rest of this.

Nothing observable changes. The contents are identical by definition; `LogCount` cannot move,
since a value equal to the old one cannot alter whether the word is non-zero; and the RAM table's
read/write highlighting is computed from the *input* step arrays, not from the state
(`WaveSimRams.fs:129-138` tests `getFastComponentInput fc 2 (step-1)`), so a location written with
the value it already held is still shown as written.

How much it is worth depends entirely on the design, and should be measured rather than assumed —
but the benchmark is a favourable case: a sieve marks composites repeatedly, writing the same flag
to a location that already holds it, so on `5eratosthenes` a large fraction of writes may be
suppressible.

## What it saves

Measured, at the top of this note. The projections that used to sit here were replaced by the
figures from `RamBenchmark.fs` once the change was built; they were close on space and, as
`simulatorStructure.md` warns about .NET measurement, wrong about speed in the conservative
direction.

## Alternatives, and why not

- **A *persistent* HAMT or radix trie keyed by `int`** — not to be confused with the write-once
  mutable trie adopted above for wide addresses. Read drops to four indexed loads and it keeps a
  genuine immutable per-step snapshot, but a write still path-copies ~4 nodes of 32 slots, ~500 B.
  It fixes speed and not space, which is the wrong half. The mutable trie keeps the read
  characteristics and drops the copying, at the price of moving history out of the structure and
  into per-address arrays.
- **`Map<int,uint32>` instead of `Map<bigint,bigint>`.** Removes three BigInt allocations per access
  and makes comparisons primitive: perhaps 2–3× on RAM cost, ~1.03× overall, no space win. A
  stepping stone at best.
- **Copy-on-write pages, 256 × 256 words.** Random writes touch a fresh page nearly every time,
  ~1 kB copied per write — worse than the AVL for this access pattern.
- **A mutable read cache beside the immutable map.** Desynchronises on restart and on replay
  through the circular buffer (`simulatorStructure.md:191`).
- **Periodic checkpoints of `Words` instead of an index.** Bounds the seek at
  `(writes / K) × arraySize` bytes, but the index is smaller and removes the seek entirely.

## What it touches

| file | change |
|---|---|
| `Simulator/RamStore.fs` | **new.** The whole of it: `History`, `NodeOrLeaf`, `Addressing`, `Ram`, and every operation on them |
| `Simulator/SimGraphTypes.fs` | `RamState of Memory1` becomes `RamState of RamStore.Ram` |
| `Simulator/GraphBuilder.fs:147` | the old simulation graph's initial RAM state builds a store |
| `FastSim/EvalKernel.fs` | `getRamStateMemory` becomes `getRamState`/`ramStoreOf`/`getRamStore`; the four `writeMemory*` on `Memory1` become eight `readRam*`/`writeRam*` on a store. The `readMemory*` used by ROMs are untouched |
| `FastSim/EvalReference.fs` | `RAM1`/`AsyncRAM1`, both width paths, all four bigint combinations, plus the `ramStateForStep` helper |
| `FastSim/EvalAlgebraic.fs` | the `FData` twin, which must move with `EvalReference` |
| `FastSim/FastOrder.fs` | the step-0 seed builds the store, once, for the whole run |
| `FastSim/FastExtract.fs` | ROM extraction wraps its `Memory1` in a read-only store |
| `UI/WaveSim/WaveSimRams.fs` | reads word by word out of the store; `.Count` comes from `liveCountAt` |
| `UI/SimulationView.fs` | the memory-diff button materialises a `Memory1` at the step it is showing |
| `Tests/…/RamStoreTests.fs` | **new.** The store against a `Map` model, per addressing strategy |
| `Tests/…/RamBenchmark.fs` | **new.** The measurements at the top of this note |
| `Tests/…/ComponentSemantics.fs` | 20-bit-address RAMs, so the trie is exercised through the evaluators |
| `Tests/…/GoldenModel.fs` | reads RAM contents out of the store |

`Simulator/RamStore.fs` sits **before** `SimGraphTypes.fs` in `Renderer.fsproj`, not in the
`FastSim` block with the rest of the simulator. F# compile order is the dependency layering and
`SimulationComponentState` names the type, so it has to come first; it depends on nothing but
`CommonTypes`, which is what lets it.

## Traps, and what happened to them

- **`restartSimulation` did not reset RAM.** `FastRun.fs:71-78` reduces at `numStep = 0`, where
  `getRamStateMemory 0 (MaxArraySize-1)` read the slot left by the wrapped-out end of the
  *previous* run rather than the initial contents seeded by `FastOrder.initClockedOuts`. **Fixed**:
  step 0 now calls `RamStore.reset`, which puts the contents back to what the memory was built
  with and clears every history. `RamStoreTests` pins it.
- **`AsyncRAM1` is reduced twice per step** — clocked, then combinational, the latter reading the
  current step's state to see the write its own clocked pass just made
  (`EvalReference.fs:1164`). With a mutable store the write is simply already visible, and the
  read-during-write case is covered by `ComponentSemantics`'s `asyncRamStimuli`, now at three
  address widths. It is also the one component `EvalCompiled.fs:169-173` warns about: a compiled
  reducer that depends on the clocked flag brings the flag back.
- **`getRamStateMemory`'s `| _, 1 -> memory` branch** existed for `MaxArraySize = 2` builds —
  Verilog output and truth tables (`SimulationView.fs:66`). It is gone: `FastOrder` seeds slot 0
  before anything reduces, so the slot is always there to be read.
- **Immutability convention.** [mutableState.md](../mutableState.md) allows mutation for a measured
  performance reason, and the step arrays already are mutable; the store belongs in the same layer
  and does not escape the simulator. `Memory1` stays the immutable type everywhere else, and
  `toMemory` builds one on demand.
- **The typed entry points are width-safe.** A store holds values in one of two forms, chosen by
  word width. Asking for the other would read an array that was never filled, so the bigint-value
  functions redirect to the uint32 ones on a narrow memory rather than trusting the caller.
- **The in-app benchmark is still unsound for this.** `Update.fs:316` sets `FastSim.ClockTick <- 0`
  directly instead of calling `restartSimulation`, so RAM state carries between repetitions and the
  second is not simulating the first. `RamBenchmark.fs` builds a fresh simulation per repetition
  for that reason.

## Order of work

The split is chosen for blast radius, not for how much each part is worth. Phase 1 changes what a
RAM's contents *are* and leaves every surrounding mechanism alone; phase 2 removes a mechanism, and
should wait for company.

### Phase 0 — a baseline

Every number on this page is calculated rather than measured. `Tests/fixtures/3cpu`'s `eep1` sheet
already has `DATAMEM`, an `AsyncRAM1` of AddressWidth 16 and WordWidth 16 — the 64K × 16 case
exactly — alongside `CODEMEM`, an `AsyncROM1` of the same size. Drive it, and build a second design
with all writes going to **one** address, since that is the pattern the two access mechanisms
disagree about.

### Phase 1 — change what a RAM's state *is*, not where it lives — **done**

All three representations, the `WaveSimRams` per-word path and the no-op write rule, with **the
step array of `SimulationComponentState` left exactly as it was**: still allocated per RAM, still
written every step, still read by `FastExtract` at `Step[step % MaxArraySize]`.

What changed is what a slot holds. It was a whole `Memory1` and therefore a whole `Map`; it is now

```fsharp
| RamState of RamStore.Ram
```

That payload change is the one thing in the model that could not be avoided. Keeping
`RamState of Memory1` would mean building a `Memory1` — and therefore a `Map` — for every step,
which is the cost the exercise exists to remove; and `Memory1` cannot hold the store instead,
because it is the saved-file type (`CommonTypes.fs:197`) and its `Data` field is serialised into
every `.dgm`.

There is no mark alongside the store, which the design expected there to be. The step being read
is known to whoever is asking — `extractFastSimulationState` takes it, and so does the RAM table —
so the slot only has to say *which* memory this is. That is why the same object goes into every
slot and the per-step allocation vanished rather than merely shrinking.

The reducer no longer consults the slot for contents: the store is mutable and steps run in order,
so a read is `Words[addr]`. `AsyncRAM1`'s combinational pass, which read
`getRamStateMemory (numStep + 1) simStep` to see the write its own clocked pass had just made
(`EvalReference.fs:1164`), simply reads the store.

Two things came out of building it that the plan had wrong, both recorded above: the packed
representation is a regression once the slot holds a reference rather than a snapshot, and
`RamState store` per step is itself an allocation, which had to be removed by putting the fetched
object back rather than constructing a new one.

Three things came out of building it that the plan had wrong, all recorded above: the packed
representation is a regression once the slot holds a reference rather than a snapshot;
`RamState store` per step is itself an allocation, which had to be removed by putting the fetched
object back rather than constructing a new one; and a growable list per address costs far more
than the writes it holds, which is what CSR fixed.

Still open inside phase 1, and optional:

- **An `EvalCompiled` reducer for RAM**, gated by the `reducers agree ...` harness. Now unblocked -
  it was the state representation that stood in the way (`EvalCompiled.fs:506-509`) - but nothing
  above depends on it, and it is the one component `EvalCompiled.fs:169-173` warns about.

### Phase 2 — stop keeping RAM state per step, but not on its own

Drop the `StepArray<SimulationComponentState>` for RAMs, keep marks in a plain `int array` or
recompute them, and the last ~96 MB goes. It takes the space from 115 MB to 19.5 MB — a further
5.9× — and it is **not worth doing by itself**, because it touches `FastCreate`, `FastOrder`,
`FastExtract`, `SimulationComponentState` and everything that matches on it, for a term that is not
the one that hurts.

The reason is proportion. A waveform simulation retains per-step data for *every* component, not
just RAMs: `calculateTotalSimArraySizePerStep` (`FastValidate.fs:20-31`) sums 4 or 16 B per output,
so a 349-component design is of the order of 1.6 kB a step before any RAM is counted. Against that,
one RAM's 24 B a step is about 1.5%. Removing it changes no user's experience while the other
98.5% stands.

So phase 2 belongs with a general attack on per-step retention — output-array width, sampling or
windowing the retained history, or bounding the wave-sim buffer the way the step simulator's is
bounded. Note the two related defects to fix at the same time: `calculateTotalSimArraySizePerStep`
omits state arrays entirely, so the "estimated GB" warning (`UIPopups.fs:423-453`) under-reports;
and the waveform simulator's arrays are sized for the whole run with no wrap
(`WaveSimSelect.fs:577-579`), which is what makes `maxSimulationSize = 4_000_000` a hard ceiling
rather than a soft one.

### Also deferred to phase 2

- **`FastValidate.calculateTotalSimArraySizePerStep`** counting RAM state — pointless while phase 1
  keeps that state per step anyway, and it is part of the same accounting fix.

## How it is checked

Green: 496 tests, `CI=true npm run test`.

1. **`Issie.RamStore`** — 41 tests, new. The store against a `Map` of the same writes: current
   contents, the value at *every* past step (a binary search is easy to get wrong at the ends),
   the live count at every past step, `toMemory`, initial contents surviving a reset, and writes
   that change nothing not being recorded. Run for each of the three addressing strategies on each
   of the two value paths, plus pruning, which only the step simulator's wrapping window reaches.
2. **`Issie.ComponentSemantics`** — memory semantics against the independent reference, now at
   three address widths: 4 bits (dense), 20 bits (trie, including two addresses that share every
   high nibble so leaves have to be split apart), and 33 bits (wide).
3. **`Issie.GoldenModel`** — whole fixture projects compared against a stored file, RAM contents
   included. `3cpu`/`eep1` has a 64K x 16 `AsyncRAM1`, and `reducerAgreementTest "3cpu" "eep1" 500`
   at `maxArraySize = 250` runs it across two buffer wraps. These passed unchanged, which is the
   strongest evidence that the store computes what the `Map` did.
4. **Fable** — `node scripts/dev.js --once --no-app`, clean.

Not yet done, and the honest gap:

- **Measured in the app.** Everything above is .NET. `simulatorStructure.md:129` is explicit that
  simulation speed has to be measured in V8, where the same change has measured 5x different. The
  numbers at the top of this note are the space claim, which .NET does measure honestly, and a
  speed *direction*, not a speed figure.
- **The waveform RAM table scrubbed by hand**, including the pathological cursor jump (last cycle
  → 0 → last cycle) on a full 64K RAM and a jump at `SamplingZoom = 1000`. `ramTables` is already
  wrapped in `TimeHelpers.instrumentInterval "ramTables"` (`WaveSimRams.fs:285`), so
  Development > Play gives a figure for it directly.
