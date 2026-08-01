# Component libraries

Each subdirectory of this one is a library of ready-made components offered in Issie's catalogue
under **Library**.

```
static/libraries/<libname>/index.json      (optional, see below)
static/libraries/<libname>/<compname>.dgm  (one ordinary Issie sheet per component)
```

A component is just a sheet. Draw it in Issie like any other, give it a **description** (Properties
→ Sheet Description) because that becomes the catalogue tooltip, and copy its `.dgm` here. Sheets
that declare parameters are the point of the mechanism: placing one asks the user for a value for
each parameter, using the parameter descriptions, so write those to be read by someone who has
never seen the sheet.

## What happens on placement

The sheet is copied into the user's project as `L<n>_<compname>`, where `n` identifies this library
within that project. The prefix is short because the sheet name is what the user sees — on the
canvas as the stem of every instance label, in the sheet trees, and in the waveform simulator.
Components are copied with fresh component ids, so nothing is shared between projects.

Library sheets are hidden from the Sheets menu and cannot be opened. When the last instance of one
is deleted and the project is saved, the sheet is removed again.

## Multi-sheet components (not yet supported)

A sheet in a library that is instantiated by another sheet of the same library is treated as part
of that component rather than as a component in its own right, and is not offered separately in the
catalogue. Adding such a component will eventually bring all of its sheets into the project; for now
only single-sheet components are placed correctly.

## index.json

`index.json` lets the catalogue and the placement popup be shown without opening any `.dgm`, which
matters once a library has many components. It is **optional**: with no index the directory is
scanned instead, which is slower but produces the same result.

It is derived data, so do not hand-edit it — regenerate it with
`ComponentLibraries.writeLibraryIndex <path to the library directory>` whenever the sheets change.
A hand-maintained index would go stale, and a stale index is worse than none.
