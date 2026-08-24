# Reading a RAM from either simulator

Plan, not yet implemented.

## Why

Both RAM viewers read the renderer's own `FastSimulation` directly, so with the .NET sidecar
simulating - the mode large designs are moving to - neither can show anything:

- the waveform simulator's RAM table says the contents are not available (`WaveSimRams.fs`);
- the step simulator's *Stateful components* section says the same (`SimulationView.fs`), and its
  **View** button builds the whole memory and opens the diff viewer.

`SimInterface.ISimulator` names this as the piece deliberately left out - "reading a RAM's
contents, which needs a row type that is declared in the waveform UI and has to move first".

There is a second reason, which applies to the renderer's own simulator too. **The sparse display
costs a whole-memory read on every render.** `WaveSimRams` chooses the sparse listing when at most
`maxRamLocsWithSparseDisplay` (100) locations are non-zero - which is cheap to ask - and then
builds it with `RamStore.toMemory`, which walks every slot the memory has ever had. A memory with
65,536 addresses written but only fifty non-zero *now* takes the sparse branch and pays 80 ms for
it, per render of the table.

## Measured

`RamStore.toMemory` against the number of distinct addresses ever written, .NET, median of five
after a warm-up, 16-bit words, four writes per address so each slot has history to search:

| addresses written | `toMemory` | `liveCountExceeds` at 100 | 100 single-word reads |
| ---: | ---: | ---: | ---: |
| 10 | 0.005 ms | 0.000 ms | 0.002 ms |
| 100 | 0.088 ms | 0.000 ms | 0.008 ms |
| 1 000 | 1.256 ms | 0.004 ms | 0.007 ms |
| 10 000 | 13.594 ms | 0.002 ms | 0.004 ms |
| 65 536 | 80.269 ms | 0.001 ms | 0.004 ms |

**Reading the whole memory passes 5 ms at about 4 000 written addresses**, and is linear in them
above that. Everything else on the table is flat and free: `liveCountExceeds` stops as soon as it
has found `limit + 1` non-zero words, and a hundred individual `wordAt` reads cost less than the
hundred-address whole read does.

The conclusion the rest of this rests on: **no viewer ever needs a whole-memory read.** The two
things a viewer shows - at most a hundred non-zero locations, or a window of fifty - are both
bounded, and both are already cheap to produce one word at a time.

## The set of written locations already exists

`Ram.SlotAddr[0 .. SlotCount-1]` is exactly it. A slot is created the first time an address is
written (`RamStore.newSlot`), in all four addressing modes, and `reset` seeds the memory's initial
contents as writes at step -1 - so the slot table covers initial data as well as simulated writes.
It is a by-product of storage and costs nothing to keep.

So of the three requirements:

- **(a) keep a set of written locations** - already kept, no new state.
- **(b) when the count passes the maximum, stop updating it** - nothing is being paid to update, so
  nothing stops. What the threshold governs is *reading*: past `maxSparse` distinct written
  addresses the slot walk is no longer worth doing, and the viewer takes a window instead.
- **(c) below the maximum, use the set to return sparse data** - walk the slots, evaluate each at
  the step, emit the non-zero ones. `SlotCount <= maxSparse` decides it without any walk at all,
  and the walk that follows is bounded by `maxSparse`.

That is a strictly cheaper rule than the one in the code today, which asks the right question
(`liveCountExceeds`) and then answers it the expensive way (`toMemory`).

## 1. The view a RAM table needs

One type, below the UI so that both simulators can produce it. `RamRowType` moves out of
`WaveSimStyle` with it - that is the move `SimInterface` says has to happen first.

```fsharp
/// One row of a RAM table: an address, what it held at the clock asked about, and whether the
/// design read or wrote it at that clock.
type RamRow = { Addr: bigint; Value: bigint; Row: RamRowType }

/// What a RAM viewer gets back, and which of the two displays it is.
type RamView =
    /// every location that is non-zero at this clock - there were few enough to list
    | RamSparse of RamRow list
    /// `rows` locations from `Start`, zeros included, because there were too many to list
    | RamWindow of start: bigint * rows: RamRow list
```

`RamRowType` (`RAMWritten | RAMRead | RAMNormal`) is decided from the memory's own input ports at
that clock - `WaveSimRams.addReadWrite` does it today - so it belongs on the far side of the wire
rather than being worked out again by the caller. The sidecar has the `FastComponent`; it can
answer exactly as the local simulator does, from one place.

## 2. What a viewer asks for

```fsharp
/// The RAM at one clock, as one of the two displays. `sparseUpTo` is the most non-zero locations
/// worth listing; past that the window from `start` is returned instead, so a caller that wants
/// a window whatever the size asks for `sparseUpTo = 0`.
abstract ReadRam:
    comp: ComponentId * path: InstancePath * cycle: int *
    sparseUpTo: int * start: bigint * rows: int -> JS.Promise<Result<RamView, string>>
```

The **implementation** chooses between the two, not the caller: only it knows the count, and
asking first would be a round trip to learn something the answer already implies. The reply says
which it gave, and the table draws accordingly - which is what it does today, from the same
decision made locally.

The two numbers stay the UI's (`maxRamLocsWithSparseDisplay`, `maxRamRowsDisplayed`) and travel
with the request. The sidecar must not hold a copy of a display constant.

## 3. In `RamStore`

One new function, which is the whole of requirement (3):

```fsharp
/// The non-zero locations at the end of `step`, or None when there are more than `limit` of them.
///
/// Bounded work in every case. `SlotCount <= limit` settles it with no walk; otherwise the walk
/// stops at the first `limit + 1` non-zero words. Never a whole-memory read - see
/// docs/dev/ramOverTheWire.md for what one of those costs.
val sparseUpTo: ram: Ram -> step: int -> limit: int -> (bigint * bigint) list option
```

`liveCountExceeds` becomes a caller of it (or goes: `(sparseUpTo …).IsNone` is the same question).
`toMemory` stays for the two things that genuinely want all of it - a memory diff and a golden
file - with its doc comment strengthened to say what it costs and that no viewer may call it.

## 4. Over the wire

One command, `SimReadRam`. Request: epoch, cycle, component id, access-path length and ids,
`sparseUpTo`, `start` (two words), `rows`. Reply: JSON on error; otherwise a small binary frame -
a tag byte for which view, then per row the address, the value and the row type, with the same
words-per-value layout `SimRead` uses so that widths above 32 bits work from the start.

Bounded by construction: at most `max sparseUpTo rows` rows, so a few hundred bytes. It is a
per-render request, so it needs a cache in the renderer keyed by (epoch, cycle, ram, start) -
`StepPanelData` is the shape, and the same rule applies: one snapshot, and a value is only ever
read back for the key it was fetched for.

## 5. The two viewers

**The waveform table** (`WaveSimRams.fs`) stops calling `liveCountExceeds`, `toMemory` and
`generatewindowlocations`, and draws whichever `RamView` the cache holds. Its gap-collapsing rows
("`0x0100 ... 0x01FF   0x0000`") are a property of the sparse display and stay in the view.

**The step simulator** (`SimulationView.viewStatefulComponents`) currently has a **View** button
that builds the whole memory and opens the *diff* viewer against the component's initial contents.
A diff needs all of both memories, which is exactly the read this plan is removing.

Decide this before writing it, because it is the one place the answer is not obvious:

- *Recommended*: the diff viewer keeps working when the memory is small enough to list -
  `RamSparse` is the whole of the changed contents when the initial data is small - and for a
  memory too big to list, the button opens the same windowed table the waveform simulator shows,
  which is a display of the memory rather than of the diff. That gives the large case something
  true instead of nothing.
- The alternative, a `changed since the start` mode in the store, is one more walk of the slots
  with the same bound and could be added later if the diff turns out to be what people want.

## 6. Order of work

1. `RamRowType` and `RamRow`/`RamView` move below the UI; `WaveSimStyle` keeps its styling
   function and aliases the type. No behaviour change.
2. `RamStore.sparseUpTo`, with tests: fewer than the limit, exactly the limit, more than the
   limit, a memory whose slots outnumber its non-zero words (the 80 ms case above), initial data
   with no writes, and a wide-addressed memory.
3. The **local** viewers move onto it - both tables, still reading the renderer's simulation. This
   is where the 80 ms sparse read is fixed, and it is worth having on its own.
4. `SimReadRam` in the sidecar, `SidecarClient` and the cache.
5. The viewers take whichever simulator is running, and the two "not available" messages go.

Steps 1-3 are worth doing whether or not 4-5 follow.

## Verification

- `CI=true npm run test`, and a `RamStore` group for `sparseUpTo`.
- The measurement above, repeated: no viewer path may be linear in the memory's size. The check
  that matters is a memory with tens of thousands of written addresses and a handful of non-zero
  ones - the case that takes the sparse branch and used to pay for a whole read.
- Drive both viewers in both modes on `3cpu` (a 64K x 16 RAM and a ROM) and confirm the tables
  agree row for row, the read and write highlights included.
