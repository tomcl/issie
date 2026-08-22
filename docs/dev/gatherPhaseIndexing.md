# Index the gather phase: replace `GatherData`'s Maps with a LookupArray

Plan, not yet implemented. It attacks the first entry in the "Known debt" section of
[simulatorStructure.md](simulatorStructure.md).

## Context

`simulatorStructure.md` names this as the build's shape problem and prices it: 200,000
`containsKey` lookups into a 10,000-entry structure cost 77.2 ms as
`Map<ComponentId * ComponentId list, _>` and 0.2 ms as an array index. `FastCreate.fs:892-898`
already records the same effect measured in the app — a structurally-keyed map in the link loop
was *a measured fifth of a 480,000-component build* — and works around it with a local
`Dictionary<int,_>`.

An audit of every Map the simulator holds says the obstacle is not correctness. Every one is
write-once except `FIOActive`, which is a single guarded insert (`FastCreate.fs:924-928`). None
is ever compared, serialised, or held as an older snapshot; `FastComponent` is
`[<ReferenceEquality>]` and `Helpers.memoizeByIdentity` exists precisely because the codebase
already refuses to compare these structurally. What has been missing is a structure to move to.

This plan builds that structure — a growable, index-addressed store — and applies it to the
**gather phase only**: `GatherData`'s four Maps and the `getLinks` recursion that reads them.
That is where the hot lookups are, it is entirely inside `FastCreate`, and nothing outside the
simulator sees it. `FComps`, `FCustomComps`, `WaveComps` and `FCustomOutputCompLookup` stay
`Map`s, built from the store at the end of `createInitFastCompPhase`; they are the door, and the
~85 call sites in WaveSim, Verilog, `FastExtract` and the tests are untouched.

A second effect worth having: `WaveSimSVGs.fs:180-192` documents that a Fable `Map` carries a
comparer closure created at its construction site, and V8 gives every closure of a function one
shared context, so the map pins whatever was in scope there. `FComps` is built inside
`createInitFastCompPhase`, which has `GatherData` in scope — so the claim at `SimTypes.fs:521-526`
that `GatherData` is not retained may not survive a heap snapshot. Deleting `AllComps` removes
the largest thing that could be pinned.

## Decisions taken

- **The index stays internal.** `FComponentId` remains the simulation-time identity everywhere
  outside the build. One `Map` is built at the end of the gather as the boundary.
- **Ordering follows creation order**, not key order. Goldens are re-pinned if they move — see
  "What actually moves" below; the expectation is that nothing does.
- **No `Dictionary`.** The store below is the only new structure.

## 1. The store — `src/Renderer/Common/LookupArray.fs`

New file. It depends on nothing, so it can go anywhere before
`Simulator\SimGraphTypes.fs` (line 69) in `src/Renderer/Renderer.fsproj`; put it beside
`Common\Optics.fs` (line 31).

```fsharp
type LookupArray<'T> =
    { mutable Items: 'T array      // backing store; entries at or past Count are undefined
      mutable Count: int
      GetIndex: 'T -> int          // read an item's own record of its index
      AddIndexStamp: 'T -> int -> 'T   // write it, returning the stamped item
      MaxIncrement: int }          // growth cap, so a large array extends rather than doubles
```

Operations:

- `create (getIndex: 'T -> int) (addIndexStamp: 'T -> int -> 'T) (capacity: int)
  (maxIncrement: int) : LookupArray<'T>`
- `addItem (item: 'T) (store: LookupArray<'T>) : 'T` — stamps through `AddIndexStamp`,
  stores the **stamped** value, returns it.
- `updateItem (i: int) (item: 'T) (store: LookupArray<'T>) : unit`
- `item (i: int) (store: LookupArray<'T>) : 'T` — the read, and the hot one; `inline`.
- `count`, `toArray` (a copy truncated to `Count`), `iteri`.

**Two plain functions rather than a `Lens<'T,int>`**, so the caller chooses what stamping costs.
A lens setter is a record copy, and `FastComponent` has 24 fields and is created 480,000 times on
a large design — while `FastComponent` already carries `mutable` fields and is
`[<ReferenceEquality>]`, so a mutable `Index` written in place costs nothing and preserves the
object identity the simulator relies on. Both shapes fit:

```fsharp
// in place - no copy, same object back
LookupArray.create (fun fc -> fc.Index) (fun fc i -> fc.Index <- i; fc) n maxInc
// through a lens, where the type is immutable
LookupArray.create (Optic.get index_) (fun t i -> Optic.set index_ i t) n maxInc
```

It also drops the `Optics` dependency from the store, which is why it can sit anywhere in
compile order.

Four things the sketch leaves open, decided here:

1. **`addItem` returns the stamped item; use the return value, never the argument.** With the
   in-place stamp they are the same object and with a lens they are not, so `addItem fc store |>
   ignore` is silently wrong in one flavour and fine in the other. Mark the module
   `[<RequireQualifiedAccess>]` and say this in the doc comment; the F# compiler will not catch it.
2. **Growth must not need a sentinel.** `Array.zeroCreate<'T>` yields nulls for reference types
   and the codebase forbids nulls. Keeping `Count` beside an oversized backing array means
   nothing past `Count` is ever read, so no `option` per slot — which is what
   `Drivers: Driver option array` currently pays.
3. **Capacity where it is known.** `stepCostOfDesign` already walks the whole design before
   anything is allocated, so the component count is available; pass it and doubling never fires.
   Growth is the fallback for the flattening pass, where the count is not known in advance.
4. **This is mutable state in the build.** [../mutableState.md](../mutableState.md) allows it for
   a measured performance reason *and* asks that it be "built write-once, or encapsulated tightly
   enough that nothing outside can mutate it". Satisfy the second clause: the store is
   build-scoped and never reaches a `FastSimulation` field in this stage. Add a row to the audit
   table in `mutableState.md` in the same commit.

Test file `Tests/Issie.Tests/LookupArrayTests.fs` — growth across the `MaxIncrement` boundary,
`addItem` stamping, `updateItem`, `Count` never exposing an unwritten slot. **Two edits, or it
fails silently:** list it in `Tests/Issie.Tests/Issie.Tests.fsproj` (compile order matters) and
add its `tests` value to the list in `Main.fs`.

## 2. `Labels` becomes a plain array — independent, do it first

`GatherData.Labels : Map<ComponentId, string>` (`SimTypes.fs:536`) is keyed by *design*
`ComponentId`, and `CommonTypes.fs:1022-1024` guarantees those are "allocated densely from 1 by
`Helpers.IdAllocator` so a design's components can index arrays directly". So this is a
`string array` indexed by id, with no store and no key step at all.

It is read by `GatherData.getFullSimName` and `getFullSimPath` (`SimTypes.fs:545-565`), which do
one `Map.tryFind` **per element of the access path, per component** — called from
`FastCreate.fs:792-793` for every FastComponent created. Smallest change in the plan and it
stands alone.

Note `createFlattenedSimulation` appends the same `labels` list once per custom instance
(`FastCreate.fs:617`), so `Map.ofList` is deduplicating today. Writing into an array by id has
the same last-write-wins effect and drops the duplicates entirely.

## 3. The flattening pass creates, stamps and stores the FastComponents

`createFlattenedSimulation` (`FastCreate.fs:547-631`) currently returns four lists concatenated
with `@` up the recursion, which `gatherSimulation` turns into four Maps
(`FastCreate.fs:641-644`); the FastComponents are then made in a second walk, by
`createInitFastCompPhase`'s `Map.fold` over `AllComps` (`FastCreate.fs:796-802`).

Collapse the two. The flatten calls `createFastComponent` as it visits each component and
`LookupArray.addItem`s the result, so the store holds `FastComponent` directly and there is no
intermediate gathered type. `FastComponent` gains one field:

```fsharp
/// This component's position in the build's one index space. Written once by
/// LookupArray.addItem during the flatten and never again.
mutable Index: int
```

`mutable` and stamped in place, so no 24-field record copy per component — see the two-function
argument in section 1. The recursion visits every instance exactly once, so the index falls out
of the walk.

This subsumes `createInitFastCompPhase`'s creation loop; what is left of that function is the
door in section 5. It also removes one full traversal of the design and the `AllComps` Map that
existed only to be traversed.

### The budget check has to move with it

**This is the constraint that shapes the change.** `createFastComponent` allocates the step
arrays, and `checkSimulationFits` currently runs *before* any of them exist
(`FastBuild.fs:104-112`) — deliberately: `FastCreate.fs:308-313` says "Before rather than after,
because the arrays ARE what exhausts memory: a check that had to build them first would be the
thing it is meant to prevent", and `Tests/Issie.Tests/SimulationBudget.fs` asserts that an absurd
design comes back as an ordinary `SimulationError`, promptly, rather than crashing or hanging.
Creating FastComponents inside the flatten puts allocation ahead of the check, so the check must
become part of the flatten:

- `stepCostOfDesign` (`FastCreate.fs:274-284`) folds `AllComps` reading only
  `sComp.OutputWidths` and `couldBeSynchronousComponent sComp.Type` — both in hand at the moment
  the flatten visits a component. Accumulate the running `StepCost` in the store instead, and
  test it against `maxCyclesFor` per component (or per N components).
- The flatten then refuses **earlier** than today rather than later: it aborts at the component
  that crosses the budget instead of after expanding the whole design. That is a strictly
  stronger guarantee, and `SimulationBudget.fs` is what proves it still holds.
- `startStepArena` / `finishStepArena` (`FastBuild.fs:112-136`) must move out to wrap the
  flatten, keeping the `try`/`finally` — the comment there ("so that a build that raises cannot
  leave its arena open for an unrelated later build to draw from") applies to an aborted flatten
  exactly as it does today.
- `stepCostOfDesign` as a standalone function still has a caller: `ModelHelpers.waveSimCostMemo`
  reads a design's per-cycle cost for the waveform configuration dialog without building
  anything. Keep it, and have the flatten's running total agree with it — a test that the two
  give the same `StepCost` for a fixture design is cheap and pins the duplication.

**If the incremental check proves awkward, fall back to two passes:** flatten and stamp a
lightweight gathered record (no arrays), cost it, check, then create FastComponents in store
order into a parallel store. That keeps `checkSimulationFits` exactly where it is and loses only
the second-traversal saving.

### Links resolved during the walk

Two resolutions have to happen inside the recursion, and both can. They live on the stored
component; `FastComponent` already carries `InputDrivers`, so this is the same kind of field:

```fsharp
/// this instance's outgoing links, already resolved to store indices, by output port number
mutable OutLinks: (int * InputPortNumber) array array
```

- **Sibling links.** `getLinks` follows `sComp.Outputs : Map<OutputPortNumber, (ComponentId *
  InputPortNumber) list>`, which names design ids of siblings in the *same* instance. Resolve
  them while flattening, using a scratch `int array` indexed by design `ComponentId` (dense, so
  this is one array reused per instance) mapping sibling id → store index. `OutLinks` is then
  written once and `getLinks` never touches a `Map` again. This also deletes
  `FastCreate.fs:882-888`, where `Map.toArray |> Array.filter (fun (opn',_) -> opn' = opn)` is a
  fresh array per driver port doing the work of `Map.tryFind`.
- **Custom in/out links.** `getCustomNameIdsOf` (`FastCreate.fs:580-589`) already matches inner
  Input/Output components by label within the level, so the index is in hand where the link is
  built. `CustomInputCompLinks` becomes an `int` on the custom component's entry per input port;
  `CustomOutputCompLinks` becomes an `int * OutputPortNumber` on the inner Output's entry. The
  custom component is added at its own level *before* the recursion into its graph, so its index
  exists when the inner links are formed.

`GatherTemp` and the `@` concatenations go with this; `GatherData` shrinks to the store plus the
`Labels` array.

## 4. `getLinks` and `linkFastComponents` on indices

`linkFastComponents` (`FastCreate.fs:851-949`):

- `getSComp` / `apOf` (`:856-863`) become `LookupArray.item i store`, reading `.SimComponent` and
  `.AccessPath` off the FastComponent the store already holds.
- `getLinks` (`:867-890`) recurses on `int`, reading `OutLinks`, the custom-input index and the
  custom-output pair off the entry. All four `Map` reads in it disappear.
- `f.FComps[fDrivenId]` (`:907`) becomes `LookupArray.item`. There is now **one store and one
  index space** for the build, holding the FastComponents themselves, so nothing has to be kept
  in step with anything. Custom vs ordinary stays a *predicate* over the store, never a second
  index space — state that in a comment, because a second space is the obvious "tidy-up" and it
  is what would break this.
- The local `linkCheck` `Dictionary<int,_>` (`:898`) can become a `bool`/index array sized by
  `NumStepArrays`, since its key is already a dense step-array index. Optional, but it removes
  the one `Dictionary` in the file and the comment explaining why it is there.
- `FIOActive` stays a `Map`. Its key is `(ComponentLabel, ComponentId list)` — a label set within
  an instance, which has no dense index — and it holds one entry per distinct label name per
  sheet instance, touched once per link. Leave it alone.

## 5. The door

`createInitFastCompPhase` (`FastCreate.fs:784-818`) no longer creates anything — section 3 took
that over — so what is left of it is the boundary. Build from the store:

- `FComps` — entries where `not (isCustom …)`, keyed `(cId, AccessPath)`
- `FCustomComps` — the custom ones
- `FCustomOutputCompLookup` — the stored custom-output links, indices mapped back to
  `FComponentId` through the store

All three stay `Map<FComponentId, _>`. `WaveComps` (`FastCreate.fs:767-769`) is unchanged.
Every reader outside `FastCreate` — `FastExtract`, `WaveSim*`, `Verilog.fs`,
`SimulationView.fs`, `SimDigest.fs`, the tests — compiles untouched.

Building these Maps costs O(n log n) once, against millions of lookups saved. They are the thing
a later stage deletes, when `WaveIndexT.Id` carries the index instead.

## What actually moves

Creation order replaces key order in one place that matters: `createInitFastCompPhase` folds
`AllComps` today (`FastCreate.fs:797`) and that fold's order assigns `stepArrayIndex` values,
hence `IOArray.Index`, `Driver.Index` and `WaveIndexT.SimArrayIndex`. With creation moved into
the flatten these follow the design walk instead. The numbers change; they are internal and no
test asserts them.

Order that stays put, because it is still taken from a `Map`: `WaveIndex` ordering
(`FastCreate.fs:713` reads `waveComps |> Map.toArray`), `createFastArrays`
(`FastBuild.fs:45-63`), `Verilog.fs:266` and `:625`, `DevHarness.fs:675`.

So the goldens are expected to be **unchanged**. There are only three
(`Tests/fixtures/{1fulladder,3cpu,adder4}/*.golden`) and `SimDigest.render` sorts everything it
prints (`SimDigest.fs:43,51,56`). `VerilogOutput.fs` checks structure and semantics, not stored
text. If a golden does move, re-record with `ISSIE_UPDATE_GOLDEN=1` and **read the diff before
accepting it** — a change there means something other than ordering moved.

## Order of work

Each step compiles and is separately testable.

1. `LookupArray` + its tests + the `mutableState.md` audit row.
2. `Labels` → `string array`. Independent of 1; smallest measurable win.
3. **Budget check first, before anything else moves.** Accumulate `StepCost` during the flatten,
   check incrementally, move the arena's `try`/`finally` out to wrap the flatten — with
   `createInitFastCompPhase` still doing the creating. `Issie.SimulationBudget` must stay green
   across this step on its own, because after step 4 it is no longer possible to tell a budget
   regression from a creation-order one.
4. Flatten creates and stamps the FastComponents into the store; resolves sibling and custom
   links as indices. `AllComps`, `GatherTemp` and the `@` concatenations deleted, along with
   `createInitFastCompPhase`'s creation loop.
5. `getLinks` / `linkFastComponents` on indices; the door built from the store.
6. Optional: `linkCheck` array; retire the `stepArrayIndex` module global into the store.

## Verification

**Compiles under both toolchains** — `npm run typecheck` (dotnet, fast) and
`node scripts/dev.js --once --no-app` for Fable. Do *not* use `npm run compile`: it leaves the
tree in `PRODUCTION` and costs the next `npm run app` a full recompile.

**Behaviour** — `CI=true npm run test` is the full local run (629 tests, ~20 s). The groups that
bear on this change, runnable alone with
`dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.<Group>`:
`GoldenModel`, `CustomOutputExtraction`, `WaveSelection`, `SimpleDesignTests`,
`SimulationBudget`, `TruthTableSimTests`, `VerilogOutput`, `ComponentSemantics`.

**`Issie.SimulationBudget` is the one that matters most here**, and it is the reason step 3 is
its own step. Its tests ask for absurd sizes and assert an ordinary `SimulationError` comes back
promptly — "a test that finishes at all is a good part of what is being asserted"
(`SimulationBudget.fs:9-11`). Once creation moves into the flatten, that suite is the only thing
standing between a mis-sequenced check and a build that allocates until the machine gives out, so
run it *before* step 4 as well as after. Add to it the cheap agreement test named in section 3:
the flatten's running `StepCost` and standalone `stepCostOfDesign` must give the same answer for
a fixture design, or `waveSimCostMemo` and the build will disagree about what a design costs.

**Speed and allocation** — the numbers to quote, before and after, on `3cpu`:

- `FastBuild.fs:84-98` already marks every build phase; turn on the `perf` log category
  (Development > Log, or `--log=perf`) and read the `build gather / createInit / link / …` lines,
  which give ms and MB per phase. `gather` and `link` are the two this changes.
- `SimLog`'s `AllocMb` is exact and has none of the run-to-run noise of a timing
  (`SimLog.fs:46-50`). Pull it with `node scripts/inspect-canvas.js` / the DevHarness `simLog`
  command. `CommonTypes.fs:1004-1010` quotes 270.26 MB per `3cpu` build, repeatable to 0.1% —
  that is the baseline to beat and the method to use.
- Measure **in the app**, not under .NET: `simulatorStructure.md` records the same change
  measured as 2.5x under .NET and 11.9x in V8.

**The retention question** — take a heap snapshot after a `3cpu` build and confirm no
`GatherData`/`SimulationComponent` graph survives it. If the Fable comparer-closure effect was
real, this is where it shows up as a drop in retained size.

**Update `simulatorStructure.md`** — the "Known debt" section's first entry is the problem this
attacks. Amend it with what was done, what it measured, and what is left (the component tables
and `WaveIndexT.Id` still being structural).
