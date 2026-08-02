# Component libraries

Each subdirectory of this one is a library of ready-made components offered in Issie's catalogue
under **Library**.

```
static/libraries/<libname>/<compname>.ldgm
```

One `.ldgm` per component. There is no index file and nothing else to keep in step.

## The `.ldgm` format

An `.ldgm` is a serialised pair: a small **header** describing the component, and the component's
sheet as the exact text of a `.dgm`.

```fsharp
type LibraryHeader = {
    FormatVersion: int
    Name: string              // what the user sees, and the file's base name
    Description: string       // the catalogue tooltip
    Section: string           // catalogue section the component is grouped under
    OfferedInCatalogue: bool  // false for a sheet that only exists to serve another component
    Requires: string list     // other components of THIS library that this one instantiates
}
```

Two things follow from that shape, and they are the point of it.

**Listing a library reads headers only.** The body is a single JSON string token, so nothing builds
a canvas — no width inference, no id regeneration — which is what makes opening a library cheap
even when it holds many components.

**Placing a component writes the body straight out as a `.dgm`** and hands it to the ordinary sheet
loader. Issie has one canvas format, and the library layer does not understand it.

`FormatVersion` is checked on read: a file written by a later Issie is refused with a message
saying so, rather than a decoding error. `.dgm` has no version field, which is why loading one is
three decode attempts in sequence — this was the one chance not to repeat that.

## Making a component

Draw the sheet in Issie like any other, then **right-click it in the Sheets menu → "Save as library
component"**. You are asked which library to put it in and for a description for the catalogue.

The sheet you choose is offered in the catalogue. Every sheet it uses, transitively, is written
alongside it with `OfferedInCatalogue = false`, so a multi-sheet component arrives whole without
its internals cluttering the catalogue. `Requires` names the dependency rather than embedding it,
so two components sharing a sub-sheet do not each carry a copy.

What is written is what is on disk, so a sheet with unsaved changes is refused rather than quietly
saved in its last-saved state.

Components are written to a library under your own Issie directory
(`%APPDATA%/Issie/libraries` on Windows). To ship one with Issie, copy that directory here.
Nothing is written into this directory at runtime: it lives inside the installation, which is not
reliably writable — on macOS the app bundle is signed and notarised, so writing into it invalidates
the signature.

Sheets that declare parameters are the point of the mechanism: placing one asks the user for a
value for each parameter, using the parameter descriptions, so write those to be read by someone
who has never seen the sheet.

## What happens on placement

The sheet is copied into the user's project as `L<n>_<compname>`, where `n` identifies this library
within that project. The prefix is short because the sheet name is what the user sees — on the
canvas as the stem of every instance label, in the sheet trees, and in the waveform simulator.

Components are copied with fresh ids, so nothing is shared between projects, and any custom
component naming another component of the same library is repointed at that component's new name —
without which a multi-sheet component would arrive holding instances of sheets that are not there.

Library sheets are hidden from the Sheets menu, so a component looks like one thing rather than a
sheet with innards. A library author can see them: **Play → Toggle Showing Library Sheets** in the
developer menu (debug builds only, off at startup) puts them back, along with the subsheets under
them. They are never shown in the waveform simulator, whatever that is set to.

When the last instance of a library component is deleted and the project is saved, its sheets are
removed again.
