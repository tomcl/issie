# Known rough edges

Small defects and unfinished corners that are worth knowing about before working near them, and
that are too minor to have their own page. Each was checked against the code, not inherited from an
older list. Design-level limitations of the parameter system are in
[parameterSystem.md](../parameterSystem.md#known-limitations) instead.

Delete an entry when it is fixed. A list that keeps its history stops being read.

## Parameters

- **`ParameterAnalysis.chainActionsForInstance` picks the wrong declarer.**
  `Set.minElement declarers` is the *alphabetically first* sheet that declares the parameter, not
  the outermost one. The default value and the description copied onto the intermediate sheets of a
  materialised bind-to-top chain therefore come from an arbitrary sheet.
- **Deleting a parameter checks slots but not other parameters.** `ParameterView.deleteParameterBox`
  lists the `ParamSlots` that use the parameter and refuses while any remain, but does not look in
  `DefaultBindings`, so a parameter defined in terms of another can be orphaned. Only reachable
  through a hand-edited file today, since the UI writes only `PInt` defaults — but
  `editParameterBox` handles expression-valued defaults, so the system claims to support them.
- **`ReloadSelectedComponent` is fed the wrong number.** For a `BusSelection` LSB or a `BusCompare`
  value it receives that number as "most recent bus width", which is what the properties pane then
  offers as the default width for the next component.

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
- `GraphMerger.applySlotValue` is a one-line alias for `ComponentSlots.setSlotValue`.
- `SheetLayout.applySlotValues` rebuilds the whole component list once per slot.
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
