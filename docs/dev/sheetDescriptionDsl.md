# Making sheets from a program

A design sheet can be written as data — a list of components and a list of logical connections —
and turned into a `.dgm` (or an `.ldgm` library component) that Issie opens and a person can read.
No geometry is written by hand and no part of Issie has to be running.

Two modules, split by what they depend on:

| | |
|---|---|
| `src/Renderer/Common/SheetDescription.fs` | the description: plain data, no draw block, no Fable |
| `src/Renderer/DrawBlock/SheetLayout.fs` | realisation: port resolution, placement, output |

## A sheet

```fsharp
open SheetDescription
open SheetDescription.Operators      // see the warning about ==> below

describeSheet "adder" [
    comp "A"   (Input1(4, None))
    comp "B"   (Input1(4, None))
    comp "ADD" (NbitsAdderNoCinCout 4)
    comp "S"   (Output 4)
] [
    "A"       ==> "ADD/P"
    "B"       ==> "ADD/Q"
    "ADD/SUM" ==> "S"
]
```

A component's name is its label unless `compLabelled` gives a different one. A port is written
`COMPONENT/PORT`, where `PORT` is matched case- and space-insensitively against the names Issie
gives that component type and otherwise read as an index — the index is not a convenience, since
gates, inputs, outputs, wire labels, constants, bus selects and splitters have no port names at
all. `PORT` may be left off when the component has exactly one port in the direction wanted.
`From` is always the driver.

**Declaration order fixes the sheet's port order.** `CanvasExtractor.getOrderedCompLabels` sorts a
sheet's I/O by position, and that order becomes the signature every `Custom` instance is checked
against. The layout places Inputs and Outputs in declaration order for exactly this reason.

`==>` is opt-in because it is also `Fable.Core.JsInterop`'s object-literal operator and FsCheck's
implication. Open `SheetDescription.Operators` only in a file that opens neither; `connect` is the
same function and is always safe.

## Parameters

```fsharp
describeSheet "adder" comps conns
|> withParam "W" 6 "width of the two operands, in bits"
|> withSlot "A"   (IO "A")  "W"
|> withSlot "ADD" Buswidth  "W"
|> withSlot "S"   (IO "S")  "W-1"
```

Expressions are read by `ParameterTypes.parseExpression` — the properties-pane parser — so they
mean here what they mean typed into a properties box. The generated sheet carries both the
resolved integer in the component and the expression in `ParameterDefinitions`, which is what
Issie itself writes.

Four things are refused rather than silently skipped: a slot naming an undeclared parameter, an
expression that will not parse, an expression that will not evaluate, and a component that has no
such slot. That last one is `ComponentSlots.slotApplies`, and it is why a `GateN` or a `MergeN`
cannot be parameterised at all — their integer is an input count, and a parameter records a value,
not a change of shape. A `SplitN` is the mixed case: its number of outputs is a shape, but the
width and bit position of a given output are values, so `SplitNWidth i`/`SplitNLSB i` work for the
outputs that exist and are refused past the end.

## Layout

Inputs go in a left column and outputs in a right column, in declaration order. Everything else is
placed by recursive bisection: build the graph with edge weights counting connections, split into
roughly equal halves minimising cut weight, alternate the split axis, and place leaves a component
apart on the 30px grid Issie snaps to.

Wires are written with **no vertices**. `BusWireUpdate.LoadConnections` checks whether each saved
end vertex is near the actual port and routes from scratch when it is not, so opening the sheet
runs `smartAutoroute` on every wire and then the global separation pass. Wire creation is the draw
block's, deferred to load — there is nothing to route at build time and nothing that could route
it, since that needs a populated `SymbolT.Model`.

Component ids are `<sheet>-<name>` rather than uuids. Ids must be unique per project, which the
sheet prefix gives, and readable ids make a generated file possible to read and diff.

## Output, from .NET

```fsharp
SheetLayout.toCanvasState  sheet                              // Result<CanvasState, string>
SheetLayout.saveSheet      folder sheet                       // one .dgm
SheetLayout.saveProject    folder [sheet1; sheet2]            // .dgm per sheet, plus the .dprj
SheetLayout.saveLibraryComponent libPath description deps sheet   // .ldgm, plus its dependencies
```

All four run under plain .NET — `dotnet run`, a script, a test — with no Electron and no Issie
process. Three things had to be true for that, and now are:

- **File I/O.** `FilesIO` is cross-compiled; the `#if FABLE_COMPILER` branches use `System.IO`.
  Writing must not emit a byte-order mark (`UTF8Encoding false`): Issie's reader chokes on one.
- **A project needs its `.dprj`.** An empty marker file, without which Issie will not offer the
  directory and drops it from the recent list. `saveProject` writes it.
- **`.ldgm` encoding.** Fable writes it with the vendored SimpleJson and .NET with `Thoth.Json.Net`.
  The two disagree about discriminated unions — Thoth writes an array, SimpleJson a single-key
  object — but an `.ldgm` holds none: a record, a string and a list of strings, which both encode
  the same way. The `.dgm` body inside it does hold unions, and both directions work: SimpleJson's
  reader accepts either encoding, and on .NET `Common/SimpleJsonDotNet.fs` reads SimpleJson's.

Each side therefore reads what the other writes. It was one-way until August 2026 — .NET could
write a sheet Issie opened, but not open one Issie had written.

`SheetDescriptionTests.fs` writes an `.ldgm` this way and reads it back, with nothing running.

## Verifying a generated sheet

`npm run test` covers port resolution, error messages, non-overlap, I/O order, block ordering,
simulation and a save/reload round trip. For anything about how it *looks*, open it and use
`scripts/inspect-canvas.js` — see [inspectingTheCanvas.md](inspectingTheCanvas.md).
