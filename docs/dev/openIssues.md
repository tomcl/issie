# Known rough edges

Small defects and unfinished corners that are worth knowing about before working near them, and
that are too minor to have their own page. Each was checked against the code, not inherited from an
older list. Design-level limitations of the parameter system are in
[parameterSystem.md](../parameterSystem.md#known-limitations) instead.

Delete an entry when it is fixed. A list that keeps its history stops being read.

## Parameters

- **`ParameterAnalysis.chainActionsOnSheet` picks the wrong declarer.**
  `Set.minElement declarers` is the *alphabetically first* sheet that declares the parameter, not
  the outermost one. The default value and the description copied onto the intermediate sheets of a
  materialised bind-to-top chain therefore come from an arbitrary sheet. Worse, `BindOffer.BindsTo`
  prefers the *top* sheet where it declares the name, so the button can say "connect this to
  TOP.width" while copying the default and description from a different sheet entirely.
- **Deleting a parameter checks slots but not other parameters.** `ParameterView.deleteParameterBox`
  lists the `ParamSlots` that use the parameter and refuses while any remain, but does not look in
  `DefaultBindings`, so a parameter defined in terms of another can be orphaned. Only reachable
  through a hand-edited file today, since the UI writes only `PInt` defaults — but
  `editParameterBox` handles expression-valued defaults, so the system claims to support them.
- **`ReloadSelectedComponent` is fed the wrong number.** For a `BusSelection` LSB or a `BusCompare`
  value it receives that number as "most recent bus width", which is what the properties pane then
  offers as the default width for the next component.

## Wire routing and separation

How these two passes are meant to work is in [wireRouting.md](wireRouting.md).

- **Separation does not settle on the two array cases.** `WireQuality.fs` records it:
  `crossedArrays` needs five further passes before nothing moves, and `wrappedArrays` never
  settles at all — it is a period-2 limit cycle, alternating between two layouts differing by 1.2%
  of wire drawn, and the pass leaves the sheet on the worse of the two. Since separation runs after
  every drag, paste and rotate, what a user is left with on such a sheet depends on how many times
  it happened to run. Both sheets are an array of ports facing an array of ports, which is what
  most of the clustering complexity exists for.
- **A shift aims 0.0001 clear of a symbol; the check that accepts it demands 7.** The four shift
  sites use `smallOffset` while `findWireSymbolIntersections` expands every unconnected symbol's
  box by `minWireSeparation`, so a shift along the box it is avoiding can never satisfy the test.
  `wireSeparationFromSymbol` (7) — the constant named for this, and the one the comments say is
  used — appears only in comments. Changing the shifts to clear by 7 made no difference to any
  sweep, so this is latent rather than active.
- **The mux SEL exemption covers every mux on the sheet.** `findWireSymbolIntersections` skips
  checking a SEL wire's last segments against *all* muxes and demuxes, not just the one it
  connects to, so such a wire can cross a different mux undetected.
- **`string` on an `[<Erase>]` id means two different things.** `InputPortId`, `OutputPortId` and
  friends are erased by Fable, so `string portId` is the bare id in the app and
  `InputPortId "…"` under .NET. `BusWireRoute` used it for five `model.Symbol.Ports` lookups, which
  therefore threw in any .NET test that routed a wire; those are now `inputPortStr`/`outputPortStr`.
  Nothing checks for the pattern, and the same trap is open wherever an erased id meets `string`,
  `sprintf` or an interpolation.
- **`removeWireSpikes` and `removeModelSpikes` have no callers.** Spike removal is written, exported
  and never run: `separateAndOrderModelSegments` ends with `removeModelCorners` and nothing calls
  the spike pass. Either the artifact it removes no longer occurs, in which case delete it, or it
  does and the pass was dropped by accident.
- **`hasOverlap` and `hasNearOverlap` each have a clause that can only fire on exact equality.**
  Both write `b1.MinB` where the third argument should be `b1.MaxB`. Harmless — the other two
  clauses already decide overlap in every case — but it reads as if it were load-bearing.
- **`makeClusters` calls the head of a descending-sorted list `lowestLoc2Index`.** It is the
  highest index, so the test that follows it is not the "did the downward search fail to reach the
  starting segment" check it appears to be. It errs towards the branch that splits off a second
  cluster, which is the safe one.
- **Dead code kept alive.** `snapToNet` (and `copySegments`, `generateEndSegments`, which serve only
  it) is unreachable behind `match model.SnapToNet with | _ -> initialWire`. `expandCluster`
  computes `lowestDownwardsIndex` for a guard that is commented out. `adjustSegmentsInModel` binds
  `Option.get line.Seg1` and never uses it. The doc comment on `Constants.separateCaptureOverlap`
  describes `maxCornerSize`.

## Component libraries

- **A typed library name becomes a directory name unchecked.**
  `MiscMenuView.saveAsLibraryComponent` joins the user's text onto the libraries directory with no
  validation beyond non-empty, so a name containing a path separator or `..` writes outside the
  intended directory. Sheet names are validated by `maybeWarning`; library names are not.
- **Name collisions have no rule.** `CustomComponentType.Name` refers to sheets by name, so a
  library `Adder` and a user `Adder` cannot coexist in one project. Namespacing or rename-on-import
  is the missing piece.
- **Sweeping a library sheet leaves its backups.** `MenuHelpers.sweepUnusedLibrarySheets` removes
  the `.dgm` when the last instance goes, but the sheet's files under `backup/` stay.
- **A copied-in library sheet does not track the library.** Once materialised it is an ordinary
  project sheet and never sees a later version of the library component. This is probably what is
  wanted, but it is nowhere stated to the user.

## Refactoring worth doing

- `SheetLayout.saveSheet` and `sheetBody` are the same function twice, differing only in
  `saveStateToFile` versus `stateToJsonString`; both also evaluate `paramDefsOf` a second time
  after `toCanvasState` has already done it.
- "The custom components named in a canvas" is written three times:
  `ComponentLibraries.customSheetsUsedBy`, the same `List.choose` inline in
  `ComponentLibraries.unusedLibrarySheets`, and `SheetLayout.saveLibraryComponent`'s `requiredBy`.
- `LibraryHeader` is built field by field in both `MiscMenuView.saveAsLibraryComponent` and
  `SheetLayout.saveLibraryComponent`. The format is versioned, so the two writers drifting matters.
- `MiscMenuView.maybeWarning` calls `ComponentLibraries.reservedPrefixOf` twice and then uses
  `.IsSome`/`.Value`, against the `Option`-throughout convention.
- `FilesIO.modifiedTimeMs` has no callers. It was written for a library-index scheme that was
  dropped in favour of lazy reading.
- `VerilogComponent/TestParser.fs` (937 lines) is a hand-driven test runner that compiles into the
  shipped renderer, reads paths that exist only in a dev checkout, and keeps `printf` on the
  `SourceHygiene` allowlist. It should go once its corpus runner is in Expecto — see
  [verilogTesting.md](verilogTesting.md#route-a-automate-the-existing-icarus-differential-test).

## Documentation

- **The screenshots on the documentation site are a version behind.** `docs/img/homePage/*` and
  `docs/img/userGuide/*` show a **Sheets** menu and an Electron application menu bar that no longer
  exist — the bar is now `Project | Sheet | Edit | View`, drawn inside the app. The workflows are
  unchanged, so they are still usable. `scripts/inspect-canvas.js` can screenshot the running app,
  so a refresh can be scripted rather than done by hand.
