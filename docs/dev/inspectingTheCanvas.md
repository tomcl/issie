# Inspecting the draw block from outside the app

Issie's canvas can be read, and screenshotted, while the app is running — without adding print
statements or clicking anything. This is for debugging symbol geometry, wire routing, and anything
where what is drawn might disagree with what the model says.

## Getting a connection

`npm run dev` starts Electron with `--remote-debugging-port` (`scripts/start.js`; default 9222,
override with `ISSIE_DEBUG_PORT`), which exposes the Chrome DevTools Protocol on the renderer.
Development only — that script is not used by `npm run build` or by a packaged app, so no released
build ever listens on the port.

`scripts/inspect-canvas.js` speaks that protocol. It has no dependencies: Node 22 provides a global
`fetch` and `WebSocket`, which is all CDP needs.

## Commands

```bash
node scripts/inspect-canvas.js raw              # the whole draw block model, faithfully
node scripts/inspect-canvas.js model            # a readable summary of the same thing
node scripts/inspect-canvas.js geometry         # what the SVG actually contains
node scripts/inspect-canvas.js shot out.png     # screenshot the renderer window
node scripts/inspect-canvas.js eval expr.js     # evaluate a file's contents in the page
```

`raw` and `model` need a debug build: `MainView.displayView` publishes `window.issie` only when
`JSHelpers.debugLevel > 0`, which `npm run dev` gives you. `geometry`, `shot` and `eval` work
against any build with the port open.

Both are published as functions rather than as data, so nothing is computed unless something asks.
A render costs one closure and one object.

### raw

The authoritative answer, and the one to reach for first. `ModelHelpers.canvasRaw` serialises
`model.Sheet.Wire` — the whole `BusWireT.Model` — with `Fable.SimpleJson`, the library that writes
`.dgm` files.

**SimpleJson round-trips F# maps.** It has a `TypeInfo.Map` case and writes maps two ways
(`Json.Converter.fs:786`): primitive or enum keys become a JSON object, anything else becomes an
array of `[key, value]` pairs. Both forms are read back (`:501`, `:567`). Sets are handled the same
way, and `bigint` becomes a quoted string. This is not theoretical — every `.dgm` save goes through
`Json.serialize<SavedInfo>`, and `SheetInfo.ParameterDefinitions` holds two maps keyed by a
single-case DU and by a record.

So nothing has to be reduced by hand. A `fulladder` dump is around 1500 lines and includes things a
summary would drop: `IntersectOrJumpList` on each segment, `PortMaps.Order` as well as
`Orientation`, `Appearance`, `LabelBoundingBox`, `SavedComponent`.

`BusWireT.Model` is the target because it is the largest part of the model containing **no functions
at all** — it and `SymbolT.Model` are pure data. Above it, `SheetT.Model` holds `PopupViewFunc` and
a `ChildProcess`, its `Action` transiently carries a lambda (`InitialisedCreateComponent`), and its
undo and redo lists are whole models, which would multiply the dump by the undo depth. The top-level
`Model` adds `Spinner`, `PopupViewFunc` and `Pending: Msg list`. Those few fields are the only
genuine obstacles; everything else is representable.

Serialisation failures are returned as `{"error": ...}` rather than thrown, so a type SimpleJson
cannot represent degrades to a message. Note that the dump is only ever *written* here — the
deserialisation direction is exercised by the `.dgm` and `.ram` load paths, not by this tool.

### model

`ModelHelpers.canvasInspection`: the same state cut down to one line per symbol, because 1500 lines
of faithful JSON is the wrong shape for most questions. Per symbol, position, size, scale, rotation,
flip, port edges, and `DisplaysComputedValues`/`DeclaredType` — the parameter display-value stash,
`SymbolT.Symbol.SavedComponent`. Per wire, its segments converted to **absolute** coordinates, which
`raw` does not do: the model stores relative lengths.

It is a convenience, not a workaround. If a field you want is missing, either add it here or use
`raw`.

### geometry

Reads the DOM instead, so it works against any build — but it describes rendered output rather than
the state behind it. That is exactly what you want when the two are suspected of disagreeing.

Two traps, both handled inside the script, both worth knowing if you write your own probe:

- **Do not read the bounding box of `#DrawBlockSVGTop`.** It sits outside the zoom transform, so its
  box comes back multiplied by the zoom. Use `drawnBBox`, which is taken inside the transform and is
  therefore in diagram units, the same ones the model uses.
- **Do not assume how deep the wrapping goes.** Today it is `svg#DrawBlockSVGTop > g[scale(zoom)] >
  g > items`, where each item is one symbol or one wire. The script descends through single-child
  `<g>`s until the node that actually branches, so an extra layer does not silently turn 16 items
  into 1.

### eval

Runs arbitrary JavaScript in the renderer and returns the result as JSON. Useful for a projection
narrower than `model` gives, for reading the DOM tree structure, and for driving the UI: React
handlers respond to `element.click()`, which is how you can open a project or a menu without touching
the mouse. Native Electron dialogs — the file-open dialog — cannot be driven this way; the demo
projects under **Open demo project** can, and they are copied into the gitignored `demos/` directory
rather than opened in place, so nothing in the repository is touched.

## Worked example

With the `fulladder` demo open:

```
$ node scripts/inspect-canvas.js model
  Sheet "fulladd", zoom 1.4946201411198472, 8 symbols, 8 wires
  HALFADD2  Custom  x=1751 y=1746  157.56614843750003 x 110  rot=Degree0

$ node scripts/inspect-canvas.js geometry
  zoomTransform matrix(1.49462, ...), 16 groups
  HALFADD2 group bbox w=157.57
```

The two agreeing to that many decimal places is the check that the canvas is drawing what the model
holds. When they disagree, the disagreement is the bug.

## Limits

- `raw` and `model` need a debug build; `geometry`, `shot` and `eval` do not.
- One renderer window only — the script takes the first `page` target.
- Native dialogs are outside the page and cannot be driven.
