# Reading a RAM from either simulator

**Done.** Both RAM viewers work under either simulator. What is left is listed at the end.

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

## Bounded means bounded by BOTH counts

The first version of this got it wrong and it is worth writing down. A walk that stops at the
first `limit + 1` non-zero words is bounded only when there ARE that many: a memory with 65,536
addresses written and fifty non-zero now runs out of slots before it runs out of limit, and
walking all of them is the same 80 ms read. The live count cannot be the precondition for the
walk, because the walk is how you learn it.

So there are two bounds, and the one that decides is the count of addresses **ever written**:

- more than `Constants.maxSlotsForWholeRead` (4 000, the 5 ms line above) - answer None without
  walking at all, and the caller shows a window;
- otherwise walk, stopping at the first `limit + 1` non-zero words.

Cost is then the same whatever the memory holds. The price is a display change in one case: a
memory written in more than 4 000 places but holding few words now used to get a sparse listing
and now gets a window. It got that listing by reading the whole memory on every render.

## The set of written locations already exists

`Ram.SlotAddr[0 .. SlotCount-1]` is exactly it. A slot is created the first time an address is
written (`RamStore.newSlot`), in all four addressing modes, and `reset` seeds the memory's initial
contents as writes at step -1 - so the slot table covers initial data as well as simulated writes.
It is a by-product of storage and costs nothing to keep.

So of the three requirements:

- **(a) keep a set of written locations** - already kept, no new state.
- **(b) when the count passes the maximum, stop updating it** - nothing is being paid to update, so
  nothing stops. What the maximum governs is *reading*: past `Constants.maxSlotsForWholeRead`
  written addresses the walk is not worth doing, and the viewer takes a window instead.
- **(c) below the maximum, use the set to return sparse data** - walk the slots, evaluate each at
  the step, emit the non-zero ones. The walk is bounded by the same number, so the cost is the
  same whatever the memory holds.

That is a strictly cheaper rule than the one it replaced, which asked the right question
(`liveCountExceeds`) and then answered it the expensive way (`toMemory`).

## What was done

- `RamStore.sparseUpTo ram step limit` - the non-zero locations in address order, or None when
  there are too many or too many slots to look through. Bounded as above.
- `RamStore.toMemoryIfSmall ram step` - the whole memory when reading it is affordable, None when
  it is not. For the one caller that genuinely needs all of it.
- `WaveSimRams` asks `sparseUpTo` once instead of asking `liveCountExceeds` and then answering it
  with `toMemory`. It does not ask at all when the user has typed a start address, since that is
  a request for a window whatever the memory holds.
- The step simulator's **View** button keeps the memory diff when the memory is small enough to
  read, and is disabled with a tooltip when it is not - decided from the slot count while the row
  is drawn, which is not a read.
- `toMemory` remains for a memory diff and the golden files, and now says it must not be called
  per render.

Six tests in `Issie.RamStore`, over all six addressing/word-width configurations: the listing
against the Map model at limits either side of the live count, the many-slots-few-live case, and
the refusal past the budget.

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

## There is no cache, and there did not need to be

The plan above said the wire would need one, and it does not. The rows are small - at most a
hundred a table - so they live in the model, in `WaveSimModel.RamRows`, and the view reads them
the way it reads everything else.

That is not tidiness. **The waveform pane is memoised on the model**, so a reply landing in a
module of its own changes nothing the renderer can see: the first version held the rows in
`RamData` and dispatched `UpdateModel id` to provoke a redraw, and the table stayed empty because
an unchanged model is not redrawn. Held in the model, arriving IS the redraw.

The waveform data proper is the case that genuinely cannot do this and stays outside (`WaveData`):
megabytes of typed arrays, read per render, per wave. That is the distinction - not "cache versus
model" but how much there is and how often it is read.

What is left in `RamData` is the asking, which belongs in the update function: a fetch started
from a render happens on every render and cannot be told whether it is still wanted.

Two things had to be got right for that, and neither is obvious:

- **A command that always resolves is a loop.** `RamData.needed` decides whether to issue one at
  all; a command that resolved to "nothing changed" would still dispatch, and dispatching redraws,
  and redrawing asks again. That is necessary but not sufficient on its own - see the next section
  for why "not held yet" is not the same question as "not asked for yet".
- **The key must be computed once.** `RamData.keyOf` builds it for both the request and the read,
  through `waveSimModel_` in both cases. `getWSModel` indexes the WaveSim map by `WaveSimSheet`
  and the lens by `WaveSimOrCurrentSheet`; where those disagree the rows are written to one entry
  and looked for in the other, which reads exactly like a reply that never came - and then asks
  again, on every render.

## One asker, not two managed ones

The first version gave the RAM tables a command of their own, alongside the waveform viewer's.
Both call `SidecarSession.ensureBuilt`, and a design is uploaded one sheet per message with index
0 beginning an upload and discarding any abandoned one - so the two interleaved and left the
sidecar holding half of each: `{"error":"no sheet called eep1 in the design"}`, and the waveform
fetch broken with it. Silent, total, and nothing about it looks like a race from the UI.

There were two routes to it, and the second is the one worth remembering:

- several RAMs in one `Cmd.batch`, each starting its promise before any await resolves;
- and no in-flight guard at all on the RAM command, so *every message* arriving while a read was
  outstanding started another one - because what decides whether to ask is whether the rows are
  held, and they are not held until the reply lands. The waveform path has `FetchInProgress` for
  exactly this; the RAM path had nothing.

The fix is not to arbitrate between two askers but to have one. `fetchWhatIsMissing` now issues a
single command that fetches whatever this update wants **in order**: the waves, which build the
session and run it to the view, and then at most one RAM's rows, by which time the session exists
and its own `ensureBuilt` returns at once. One command, under the one `FetchInProgress` bit that
already stops the next message asking again. A round trip is sub-millisecond, so one RAM per
update is not a delay anyone can see; the next update takes the next.

`ensureBuilt` keeps its in-flight guard, but it is no longer what makes this correct: it is there
because the step simulator builds too (`SimulationView.advanceTo`) and is not sequenced with a
live waveform simulation.

A RAM table has to be able to build at all, because it can be the only thing on screen: with no
waves selected, nothing else would ever have done it.

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

1. ~~`RamStore.sparseUpTo` and `toMemoryIfSmall`, with tests.~~ Done.
2. ~~The local viewers move onto them.~~ Done - this is where the 80 ms sparse read was fixed.
3. ~~`RamRowType` and `RamRow`/`RamView` move below the UI.~~ Done, as `Simulator/FastSim/RamView.fs`,
   which is where both simulators build their rows - the sidecar answers `SimReadRam` from exactly
   the same function the renderer calls.
4. ~~`SimReadRam`, `SidecarClient.simReadRam`, and the rows in the model.~~ Done.
5. ~~The viewers take whichever simulator is running.~~ Done, and both "not available" messages
   are gone.

### Still to do

- **A window is fetched at 50 rows whatever is on screen.** `maxRamRowsDisplayed` is the display's
  number and travels with the request, so this is right - but a viewer that grew a taller table
  would need the request to follow it.
- **`SimInterface.ISimulator` still has no `ReadRam`.** `RamView.ofFastSim` and
  `SidecarClient.simReadRam` are the two implementations of it in all but name.

## Verification

- `CI=true npm run test`, and a `RamStore` group for `sparseUpTo`.
- The measurement above, repeated: no viewer path may be linear in the memory's size. The check
  that matters is a memory with tens of thousands of written addresses and a handful of non-zero
  ones - the case that takes the sparse branch and used to pay for a whole read.
- Drive both viewers in both modes on `3cpu` (a 64K x 16 RAM and a ROM) and confirm the tables
  agree row for row, the read and write highlights included.
