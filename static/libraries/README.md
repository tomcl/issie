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

## Where libraries live at runtime

The libraries here are shipped inside the installation, which is **not writable**: on macOS the app
bundle is signed and notarised, so writing into it breaks the signature, and on Windows the
installation is normally under `Program Files`. So on first use each library directory is copied to
the user's own Issie directory (Electron's `userData`, e.g. `%APPDATA%/Issie/libraries`), and that
copy is what the catalogue reads. An imported library will go in the same place.

A library already in the user's directory is left alone, so a library changed by a new Issie release
does not overwrite one the user already has.

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

It is derived data, so do not hand-edit it. Issie regenerates it at startup when it is missing, or
when the library directory's modification time is later than the index's — which covers a sheet
being added, removed or renamed, so importing a library indexes it automatically.

**One case it cannot detect**: rewriting a sheet that is already there does not change the
directory's modification time. If you edit a library sheet in place, regenerate the index yourself
with `ComponentLibraries.writeLibraryIndex <path to the library directory>`. Watching every file for
that one case is not worth the complication; regenerating is one call.
