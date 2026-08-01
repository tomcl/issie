# Review findings: parameter, library and simulator code

Recorded 2026-08-01 against the `parameter-display-values` branch (`11ff44b70`). Nothing here has
been fixed; this is the to-do list from an objective read of the parameter system
(`ParameterTypes`, `ParameterAnalysis`, `ParameterView`), the component library layer
(`ComponentLibraries`, `CatalogueView`, `MenuHelpers`), and the simulator's parameter path
(`GraphMerger`), plus the display-value plumbing in `SymbolUpdate`/`DrawModelType`.

Line numbers are as of that commit and will drift; the named functions will not.

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

Fixing this means either making the stash per-slot rather than a whole component, or refreshing it
on every symbol-changing message.

## Medium

### 4. `updateComponentSlots` applies an `IO` slot as a bus width for non-Input/Output types

`ParameterView.fs:397`: `| _, Buswidth | _, IO _ -> ChangeWidth`. Only `BusSelection` is
special-cased, on line 396. For a `BusCompare` — whose `IO` slot is the compare value, see
finding 2 — this writes the comparison value into the bus width. The
`ReloadSelectedComponent value` at line 408 likewise feeds an LSB or compare value into "most
recent bus width".

### 5. `evaluateConstraints` dispatches from inside a render-time predicate

It calls `dispatch <| SetPopupDialogText ...` inside the `List.filter` predicate
(`ParameterView.fs:246`, `:253`, `:260`), and `editParameterBox`'s `isDisabled` calls it during
popup render (`:946`). Whenever a constraint expression fails to evaluate, every render dispatches
a model update that triggers another render.

### 6. Cross-sheet paste always loses parameterisation

`copyParamSlotsToPastedComponents` (`ParameterView.fs:690`) reads `paramSlotsOfModel_` — the
DESTINATION sheet's slots — then filters on `slot.CompId = sourceId`. The source components' slots
live on the source sheet, so nothing ever matches. `CopiedSymbols` survives a sheet change
(`SymbolUpdate.loadComponents`, `:527-535`, preserves it), so this is a real path. The doc comment
implies the declared-here check is the only restriction.

### 7. `makeLsbBitNumberField` can `failwithf` on a type its own first match handles

`SelectedComponentView.fs:754` reads `BusCompare1`, but the second match (`:757-769`) has no
`BusCompare1` case and falls to `failwithf`. Unreachable today only because `makeBusCompareDialog`
handles `BusCompare1` separately.

## Low

- `ParameterView.compSlot_` / `modelToSlot_` (`:70-216`) are dead — nothing references them — and
  already out of step: `Buswidth` has no `Input1` case (unlike `buswidthPrism` and
  `GraphMerger.applySlotValue`), and the `CustomCompParam` getter `failwithf`s on any binding that
  is not a bare `PInt`. Delete or fix.
- `MiscMenuView.fs:299-301` computes `ComponentLibraries.reservedPrefixOf` twice and then uses
  `.Value`.
- Three different silent fallbacks for the same "cannot evaluate" condition:
  `ComponentLibraries.paramsOfSheet` (`:78`) and `ParameterView.childDefaultValue` (`:1686`)
  default to `1`; `ParameterAnalysis.displayValuesOfSheet` (`:242`) defaults to `0`.
- `ParameterTypes.parseExpression` has no unary minus: `-5` tokenises to `["-"; "5"]` and reports
  `Unexpected characters at end of expression: 5`. Also `12abc` becomes a single token and is
  accepted as a parameter name.
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
