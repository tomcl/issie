# Review findings: parameter, library and simulator code

Recorded 2026-08-01 against the `parameter-display-values` branch (`11ff44b70`), from an objective
read of the parameter system
(`ParameterTypes`, `ParameterAnalysis`, `ParameterView`), the component library layer
(`ComponentLibraries`, `CatalogueView`, `MenuHelpers`), and the simulator's parameter path
(`GraphMerger`), plus the display-value plumbing in `SymbolUpdate`/`DrawModelType`.

**All three High findings, and the Medium that shared a cause with one of them, are now fixed**,
each pinned by a test in `Tests/Issie.Tests/ParameterScenarios.fs` that was checked to fail without
the fix. Finding 5 and two of the Low items have since been fixed as well, and are marked. The
rest stand. Line numbers are as of the original commit and will have drifted; the named functions
will not.

A third review, of the parameter system against the code that keeps custom component instances in
step with their sheets, is recorded at the end of this file.

## High

### 1. Instance port widths are evaluated in the wrong environment, and silently become 0

`ParameterView.updateComponentSlots` (Custom branch, ~`ParameterView.fs:366-383`) and
`ParameterView.editParameterBindingPopup` (~`:1378-1397`) build `labelToEval` by evaluating the
CHILD sheet's `IO` slot expressions against `newBindings` — the instance's stored
`ParameterBindings` overridden by whichever slots are being written. Two ways that is wrong:

- A parameter the instance does not bind is absent from `newBindings`, so the child's expression
  fails to evaluate. `Error _ -> 0` then makes `updateCustomComponent` write a port width of **0**.
- A binding produced by "bind to parent" is `PParameter name`, an expression in the PARENT's
  namespace. In the common same-name case, evaluating the child's `PParameter w` against
  `{w -> PParameter w}` trips the self-reference guard in `evaluateParamExpression`
  (`ParameterTypes.fs:176`) — `Parameter 'w' is defined in terms of itself` — and again gives 0.

Reachable whenever an instance has two or more parameters and one is edited
(`editParameterBindingPopup` writes a single slot), or when any binding has been dropped, e.g. by
`copyParamSlotsToPastedComponents`'s pruning.

The right environment is the child's declared defaults overridden by the instance bindings
evaluated in the parent. Two correct implementations already exist to copy:
`applyBindOffers`'s `childEffective` (~`ParameterView.fs:1524-1544`) and
`GraphMerger.effectiveBindings` (`GraphMerger.fs:329-332`).

**Fixed.** `ParameterView.portWidthsOfInstance` now builds that environment once and all three call
sites use it. A width that cannot be worked out leaves the port alone instead of setting it to 0.

### 2. A parameterised `BusSelection` LSB is ignored by the simulator

`SelectedComponentView.makeLsbBitNumberField` (~`:768`) creates an `IO comp.Label` slot for the
BusSelection LSB field, and the canvas honours it (`ParameterView.fs:396` -> `ChangeLSB`). But
`GraphMerger.applySlotValue` has only `IO _, Input1` and `IO _, Output`
(`GraphMerger.fs:433-434`); a BusSelection falls through to `| _ -> compType`. At any instance
whose binding differs from the sheet default, the simulation uses the LSB saved in the canvas
while the drawn sheet shows another, and `rebuildWidths` width-checks against the stale LSB.

Easy to miss because `resolveParametersForComponent` DOES handle it (`ioPortPrism`,
`ParameterView.fs:1245-1255`): the catalogue/port-label path is right and the simulation path is
not. The same gap applies to `BusCompare`/`BusCompare1` compare-values, which
`SelectedComponentView.fs:763` also puts in an `IO` slot.

Root cause is the overloading of `CompSlotName.IO`: documented as an input's width, but used for
three unrelated fields.

**Fixed.** `ComponentSlots.setSlotValue` handles all three, and elaboration calls it rather than
carrying its own copy. Finding 4 below was the same confusion on the canvas side and went with it.

### 3. `SavedComponent` discards non-parameter edits to a symbol's type

`SymbolUpdate.extractComponent` (`:908-911`) is the sole path to saved state and returns
`declaredComponent`, which takes `Type`/`H`/`W` from the stash and only
`Id`/`Label`/`X`/`Y`/ports/`SymbolInfo` from the live symbol (`:885-899`). Nothing ever updates or
clears `SavedComponent`: it is written only by `Symbol.fs:706`, `SymbolUpdate.fs:471` and
`ParameterView.stashDeclaredComponents` (`:646-652`).

So while a symbol displays a computed value, an edit to any OTHER field of its `ComponentType` is
lost on save — a `Constant1`'s value, a `BusCompare1`'s compare value, an `Input1`'s default when
only the width is parameterised, `Shift`'s type, memory contents. The edit lands in
`Symbol.Component`; the stash still holds the old type; the `.dgm` gets the old type. Re-running
`applyComputedDisplayValues` also reverts such an edit on screen, since `stashDeclaredComponents`
restores every symbol from the stash first.

**Fixed**, by making the stash per-slot. `Symbol.SavedComponent` is now
`Symbol.DeclaredSlots: Map<CompSlotName, int>`, and `extractComponent` puts just those slot values
back, so every other field is saved as it stands. This is what motivated extracting
`ComponentSlots`: `SymbolUpdate` is compiled long before `ParameterView` and `GraphMerger`, which
between them held three divergent copies of the slot-to-field mapping.

## Medium

### 4. `updateComponentSlots` applies an `IO` slot as a bus width for non-Input/Output types

`ParameterView.fs:397`: `| _, Buswidth | _, IO _ -> ChangeWidth`. Only `BusSelection` is
special-cased, on line 396. For a `BusCompare` — whose `IO` slot is the compare value, see
finding 2 — this writes the comparison value into the bus width. The
`ReloadSelectedComponent value` at line 408 likewise feeds an LSB or compare value into "most
recent bus width".

**Fixed** alongside finding 2: `ChangeLSB` already handled `BusCompare`, so routing it there was
two lines, and leaving it would have made the canvas disagree with the now-corrected simulator.
The `ReloadSelectedComponent` oddity stands.

### 5. `evaluateConstraints` dispatches from inside a render-time predicate

It calls `dispatch <| SetPopupDialogText ...` inside the `List.filter` predicate
(`ParameterView.fs:246`, `:253`, `:260`), and `editParameterBox`'s `isDisabled` calls it during
popup render (`:946`). Whenever a constraint expression fails to evaluate, every render dispatches
a model update that triggers another render.

**Fixed.** The `dispatch` parameter is gone and the function is pure: what is unmet comes back in
the returned `ParamConstraint list`, which is where both callers already looked for it. A bound
that cannot be evaluated returns a constraint whose message keeps the author's wording and says
why the limit could not be worked out. The removed dispatch was also `SetPopupDialogText`, which
is where some popups keep the text the user is typing. Being a function of its arguments is what
makes it testable at all, which `Tests/Issie.Tests/Properties.fs` now does.

### 6. Cross-sheet paste always loses parameterisation

`copyParamSlotsToPastedComponents` (`ParameterView.fs:690`) reads `paramSlotsOfModel_` — the
DESTINATION sheet's slots — then filters on `slot.CompId = sourceId`. The source components' slots
live on the source sheet, so nothing ever matches. `CopiedSymbols` survives a sheet change
(`SymbolUpdate.loadComponents`, `:527-535`, preserves it), so this is a real path. The doc comment
implies the declared-here check is the only restriction.

**Partly addressed.** A paste that drops a custom component instance's parameter bindings now says
so, in a warning over the canvas naming the instances and the parameters. That covers the case the
user can act on. The underlying loss is NOT fixed: a pasted non-custom component whose width was
parameterised still freezes silently at its resolved value, and nothing can report it, because
neither `Model.Clipboard` nor `SymbolT.Model.CopiedSymbols` records which sheet the copy came from.
Fixing it properly means recording that, or copying the slot expressions at copy time rather than
looking them up at paste time.

### 7. `makeLsbBitNumberField` can `failwithf` on a type its own first match handles

`SelectedComponentView.fs:754` reads `BusCompare1`, but the second match (`:757-769`) has no
`BusCompare1` case and falls to `failwithf`. Unreachable today only because `makeBusCompareDialog`
handles `BusCompare1` separately.

**Fixed.** Checked first: it is not reachable, as the only two callers match on `BusSelection` and
`BusCompare` and `BusCompare1` is routed to `makeBusCompareDialog`. The hazard was the two matches
listing different types, so a future caller trusting the first one would crash the properties pane.
They are now a single match over the two types that really are supported. The
`(1 <<< width) - 1` bound in the same expression also gained the width < 31 guard that
`makeDefaultValueField` already had.

## Low

- ~~`ParameterView.compSlot_` / `modelToSlot_` are dead and out of step.~~ **Deleted** as part of
  the `ComponentSlots` extraction: they were a third copy of the slot-to-field mapping, and the
  drift they had already accumulated is exactly what finding 2 was.
- `MiscMenuView.fs:299-301` computes `ComponentLibraries.reservedPrefixOf` twice and then uses
  `.Value`.
- ~~`ComponentLibraries.readLibraries` reads every entry of the libraries directory as though it
  were a directory, so the README logs an `ENOTDIR` warning on every startup.~~ **Fixed** with a
  `FilesIO.isDirectory` filter. The result was always correct; only the noise was the problem.
- Three different silent fallbacks for the same "cannot evaluate" condition:
  `ComponentLibraries.paramsOfSheet` (`:78`) and `ParameterView.childDefaultValue` (`:1686`)
  default to `1`; `ParameterAnalysis.displayValuesOfSheet` (`:242`) defaults to `0`.
- ~~`ParameterTypes.parseExpression` has no unary minus: `-5` tokenises to `["-"; "5"]` and reports
  `Unexpected characters at end of expression: 5`. Also `12abc` becomes a single token and is
  accepted as a parameter name.~~ **Both fixed.** Unary minus parses as subtraction from zero, so
  no AST case was added and no saved file changes; a negated literal is folded so it renders back
  as typed. The name rule and the tokenizer's name token are now one function,
  `isValidParamName` — which turned out to matter in the other direction too: `W2X` could be
  declared and then never written in an expression, and a name beginning with a digit was shown in
  red by the dialog and accepted by its OK button anyway.
- `ParameterAnalysis.chainActionsForInstance` calls `Set.minElement declarers` `rootDeclarer`
  (`:414`). That is the alphabetically first sheet, not the outermost, so the default and
  description copied onto intermediate sheets come from an arbitrary declarer.
- `MenuHelpers.sweepUnusedLibrarySheets` (`:441`) removes the `.dgm` but leaves the sheet's files
  under `backup/`.

## Checked and fine

- Removing the `NGateInputs` slot case and changing `CCForm.Library` to carry fields cannot break
  existing files: nothing in `master` ever constructed either value, and no demo `.dgm` contains
  one.
- Multi-sheet library components are documented as unsupported in `static/libraries/README.md`, so
  `materialiseLibrarySheet` copying a single `.dgm` is a known limitation, not a defect. The
  comment on `ComponentLibraries.unusedLibrarySheets` about a multi-sheet component "going in one
  piece" reads as if it were supported, and should say otherwise until it is.
- `FastReduce.fs` handles the width-32 `1u <<< 32` hazard consistently in the adders, `NbitsXor`
  multiply, `NbitsNot`, `NbitSpreader` and `Shift`.

---

# Second review: the branch's own code

Recorded 2026-08-02 against `parameter-display-values` at `01e45da91`, reading everything on the
branch that is not on `master` — 57 files, ~4400 added lines — with three questions: is it correct,
is it simple, and does it abstract rather than repeat. The first review above looked at code the
branch inherited; this one looks at what the branch wrote.

Nothing here is a High. The largest single theme is repetition of small idioms rather than any
structural problem.

**Findings 8 and 12 are fixed**, each pinned by a test checked to fail without the fix. Finding 10
was closed by the same change as 8. The rest stand.

## What holds up

- **`ComponentSlots` as the single source of truth** for slot-to-field mapping. This was the fix
  for findings 2 and 4 above and it did the job: `GraphMerger`, `SymbolUpdate.declaredComponent`
  and `SheetLayout.applySlotValues` all go through one 50-line match.
- **`ParameterTypes.bindingsOf`** as the one conversion from declarations to an evaluation
  environment. Every evaluation site goes through it, so the "descriptions dropped" step cannot be
  forgotten.
- **The `.ldgm` shape** — an authored header plus the sheet as opaque text. Listing a library
  parses no canvas; placing a component writes the text out and uses the ordinary loader. Nothing
  is derived, so nothing can go stale.
- **The `SheetDescription` / `SheetLayout` split** by dependency: the description has no draw block
  and no Fable, so a description can be built anywhere. Neither file has a single `#if`.
- **`DeclaredSlots` holding slot values rather than a whole component**, with the reasoning for
  both that and for keeping the computed value inside `Component` written down where the field is.

## Correctness

### 8. A parameter slot the component type has no such field for is accepted and does nothing — FIXED

`SheetLayout.paramDefsOf` checks that the slot's component exists and that every parameter it uses
is declared, but not that the slot means anything on that component.
`ComponentSlots.setSlotValue` ends in `| _ -> compType`, so an inapplicable slot silently returns
the type unchanged.

Verified: `withSlot "G" Buswidth "W"` where `G` is a `GateN(And, 2)` gives `Ok`, leaves `G` as
`GateN (And, 2)`, and records the slot in `ParamSlots`. The saved sheet then lists `G / Buswidth /
W` under "Parametrised Components" in the properties pane, and changing `W` does nothing to `G`.

The module documents the opposite intent — "never a slot quietly left alone" — and errors on an
expression that will not evaluate, so this is an inconsistency rather than a deliberate choice. The
UI cannot reach it, because the properties pane only offers slots a component actually has.

**Fixed.** The match now reads `trySetSlotValue : CompSlotName -> int -> ComponentType ->
ComponentType option`, with `slotApplies` asking it whether the slot exists and `setSlotValue`
keeping the old total behaviour for the paths that must not fail on an old file. There is still one
match. `SheetLayout.paramDefsOf` refuses a slot the component does not have, naming the component,
its type and the slot.

The rejected cases are exactly the ones that are a change of shape rather than a value: `GateN` and
`MergeN` have no slots at all, since their integer is an input count; a `SplitN`'s output count is
the same thing, while the width and bit position of a given output are values and keep their slots
for the outputs that exist.

### 9. `saveAsLibraryComponent` uses the typed library name as a directory name unchecked

`MiscMenuView.fs:771` joins the user's text onto the libraries directory with no validation beyond
non-empty (`isDisabled`). A name containing a path separator or `..` writes outside the intended
directory. Sheet names are validated by `maybeWarning`; library names are not.

### 10. `ComponentSlots` has no `IO`/`BusCompare1` case, and the canvas path does — FIXED by 8

`ParameterView.updateComponentSlots` falls through to `ChangeWidth` for an `IO` slot on any type it
does not name, while `setSlotValue` falls through to "unchanged". On a `BusCompare1` those
disagree: the canvas width would change and the simulation would not. Not reachable through the
properties pane — `makeLsbBitNumberField` deliberately excludes `BusCompare1`, which is finding 7
above — but reachable from the DSL and from a hand-edited file. Fixing 8 closes this too.

### 11. Dead branches that look like handled cases

- `CatalogueView.startPlacingLibraryComponent`'s `| Ok [] -> ()`: `materialiseLibraryComponent`
  already returns `Error` for the empty case, so this is unreachable.
- `SheetLayout.paramDefsOf`'s `| _ -> Error "unreachable"`: `ParamName` is single-case, so the two
  patterns above it are exhaustive. `List.tryHead` avoids the branch.

### Still open from the first review

`Set.minElement declarers` (Low, above) now chooses the parameter **description** copied onto
intermediate sheets as well as the default, so the arbitrary choice has more visible consequences
than when it was recorded.

## Duplication

### 12. Five hand-rolled "sequence a list of Results" — FIXED

`SheetLayout.allOk`, the fold in `paramDefsOf`, the fold in `saveLibraryComponent`, the fold in
`saveProject`, `ComponentLibraries.readComponentAndDependencies` and
`CatalogueView.materialiseLibraryComponent` all write out the same fold. Every one accumulates with
`got @ [x]`, which is quadratic.

**Fixed** by `Helpers.ResultList`: `fold` threads a state and stops at the first Error, with
`traverse`, `iter` and `sequence` on top of it. All six sites now name what they are doing. It also
absorbed `Helpers.tryFindError`, which predates the branch, was the same function again, and needed
two `failwith`s for cases its own types made impossible; its three callers use `sequence`.

### 13. `SheetLayout.saveSheet` and `sheetBody` are the same function twice

Both call `toCanvasState`, then `paramDefsOf`, then build an identical `SheetInfo`; they differ only
in calling `saveStateToFile` versus `stateToJsonString`. Extract the shared part and have both use
it. That also removes the double evaluation of `paramDefsOf`, which is computed once inside
`toCanvasState` and again by each caller.

### 14. Three copies of "the Custom components named in a canvas"

`ComponentLibraries.customSheetsUsedBy`, the same `List.choose` inline in
`ComponentLibraries.unusedLibrarySheets`, and `SheetLayout.saveLibraryComponent`'s `requiredBy`.
The first two operate on the same type and should not both exist.

### 15. `LibraryHeader` construction duplicated

`MiscMenuView.saveAsLibraryComponent` and `SheetLayout.saveLibraryComponent` each build a header
field by field with the same defaults. A `ComponentLibraries.makeHeader` would keep the two writers
from drifting, which matters because the format is versioned.

### 16. `GraphMerger.applySlotValue` is a one-line alias

`let applySlotValue compType slot value = ComponentSlots.setSlotValue slot value compType` — a
rename with a comment attached. Inline it and keep the comment at the call site.

### 17. The set of library sheet names is computed in two places

`WaveSimTop.refreshWaveSim` builds it inline; `WaveSimSelectHelpers` filters for the same predicate
separately. A `ComponentLibraries.librarySheetNames` would serve both.

## Simplicity

### 18. `SheetLayout.floorplan`'s axis choice

`match depthDiff > verticalDiff, verticalDiff > depthDiff with` is a three-way comparison encoded as
a pair of booleans whose fourth case cannot occur. `compare depthDiff verticalDiff` says the same
thing in one line. `depthSpread` on the line above nests `function [] -> 1. | ds -> List.max ds`
inside a `max`, where `List.fold max 1.` is the whole of it.

### 19. `MiscMenuView.maybeWarning` calls `reservedPrefixOf` twice and uses `.IsSome`/`.Value`

Recorded as Low in the first review; the branch added a second such pair rather than fixing the
first. A `match` binds the value once and matches the "no nulls, Option throughout" convention.

### 20. `SheetLayout.applySlotValues` rebuilds the component list once per slot

O(slots × components). Grouping the slots by `CompId` first and mapping once is both faster and
shorter.

## Dead code and stale comments

- **`FilesIO.modifiedTimeMs` has no callers.** It was written for the library-index scheme that was
  then dropped in favour of lazy reading. Delete it.
- **Three comments refer to `Tools/LibraryIndex`**, which does not exist: `FilesIO.fs:58`,
  `FilesIO.fs:100`, `JSHelpers.fs:64`. They should say "the tests".
- **Two entries under "Checked and fine" above are now out of date**: multi-sheet library
  components *are* supported (`readComponentAndDependencies` plus `Requires`), and
  `materialiseLibrarySheet` no longer exists.

---

# Third review: the parameter system against custom component port updating

Recorded 2026-08-04 against `master` at `605fa39cc`, asking two questions: is
`CustomCompPorts.fs` — which brings every instance of a sheet back into step when that sheet's
ports change — compatible with the parameter system, and is the parameter system itself correct.

**Everything below is fixed**, in `9e0f755ef`, each pinned by a test that was checked to fail
without the fix. Tests are in `Tests/Issie.Tests/InstanceSignatures.fs` (new),
`ParameterScenarios.fs` and `Properties.fs`.

## The incompatibility

`CustomCompPorts` was built on one axiom: **a sheet has one signature, and every instance of it
must equal that signature**. A parameterised sheet has a family of signatures, one per set of
bindings, and two instances of it are meant to differ. So:

1. **The dialog fired on every save of any parameterised design.** `getOutOfDateDependents`
   compared each instance against `parseDiagramSignature` of the sheet at its declared values.
   `optCurrentSheetDependentsPopup` runs from `FinishUICmd`, dispatched by
   `saveOpenFileActionWithModelUpdate` and `openFileInProject'`, so an instance bound to anything
   other than the sheet's default raised "you have changed the inputs or outputs" on every save
   and every sheet switch, when nothing had changed.
2. **Accepting it broke the design.** `changeInstance` rewrote `InputLabels`/`OutputLabels` to the
   declared widths and left `ParameterBindings` alone, so the instance claimed width 8 while
   binding `W=16`. The next simulation reported `BadInputs` from
   `checkCustomComponentForOkIOs` — a dialog whose stated purpose is consistency, destroying it.
3. **Instance-specific changes were inexpressible.** `updateInstance` took a single `newSig` for
   all instances and `getOutOfDateDependents` looked only at the head of the list.
4. **A renamed child IO unhooked its parameter slot.** The slot key is `IO of Label`, nothing
   rewrote it on rename, and `pruneDeadParamSlots` would not drop it because the component was
   still alive. Editing the renamed port then created a *second* slot for the same field, and
   `GraphMerger.processComponent` applied both, in `Map` key order.

### The reconciliation

The invariant that replaces "all instances match the sheet":

> an instance is out of date exactly when it differs from what **its own** bindings give it

`CanvasExtractor.signatureOfInstance` is the single calculation of that, and the four places that
need it — placement, the properties pane, the simulator's check, and `CustomCompPorts` — all call
it; they held three divergent copies before. `signatureOfInstanceWithCertainty` reports whether
the widths can be believed, so a canvas checked without its parent compares names only. The
`IO` slot label is no longer part of a slot's identity (`ParameterTypes.sameSlot`), and
`tidyParamSlots` repoints it on save.

Going through one function fixed a case none of the copies had right: placing an instance bound to
a same-named parameter of the sheet it is placed onto evaluated that binding in the *child's*
environment, where it looks self-referential, so the instance was placed at the child's default
widths.

## Also found in the parameter system

- **A parameter-only edit was never saved.** `UpdateHelpers.currentSheetIsOutOfDate` compares
  canvases and the `LoadedComponentIsOutOfDate` flag; a parameter declared, a description written,
  an unused parameter deleted, or a slot given an expression that works out to the width already
  shown all leave the canvas identical. The save button stayed dark, `openFileInProject` passes
  `SavedSheetIsOutOfDate` as its `saveCurrent`, and the work was dropped on leaving the sheet.
  Fixed by `ParameterView.markSheetParamsChanged` on every path that edits parameter data.
- **Saving while drawing at computed values wrote a self-contradicting instance.**
  `DeclaredSlots` restores a `CustomCompParam` binding, but the instance's ports follow from that
  binding by way of the child sheet and `ComponentSlots.setSlotValue` cannot reach it. The `.dgm`
  got computed ports and declared bindings. Fixed with `Symbol.DeclaredPortLabels`.
- **`guessAtRenamedPorts` ignored port direction**, keying rename candidates on width alone, so an
  input added at width 8 could be paired with an output deleted at width 8 and the instance left
  half-changed.
- **`reorderInstancePorts` judged its remaining work from the signature the instance started
  with**, not the one it had after `changeInstance` ran, so a change that both added a port and
  reordered the rest fell through to `printfn "What? Signatures do not match"` and returned the
  instance half-updated.
- Findings 5 and the two parser items in the first review's Low list (unary minus, `12abc` as a
  name) are fixed; see their entries above.

## Incidental

`JSHelpers.uuid` gained a `#if FABLE_COMPILER` .NET fallback. The npm package is absent under
plain .NET and a Fable import throws when called, so no test could reach any path that adds a
component or a port — including the add-a-port branch of `changeInstance`.

## Still open

- The three silent `1` / `0` fallbacks for "cannot evaluate" (first review, Low) are unchanged.
- `ParameterAnalysis.chainActionsForInstance`'s `Set.minElement declarers` is still the
  alphabetically first sheet rather than the outermost.
- Cross-sheet paste still loses parameterisation for non-custom components (finding 6).
- `deleteParameterBox` checks `ParamSlots` for uses of the parameter but not `DefaultBindings`, so
  a parameter defined in terms of another can be orphaned. Only reachable through a hand-edited
  file today, since the UI only ever writes `PInt` defaults — but `editParameterBox` explicitly
  handles expression-valued defaults, so the system claims to support them.
- `addParameterBox` does not reject a duplicate name: `Map.add` silently replaces the existing
  declaration and its description.
