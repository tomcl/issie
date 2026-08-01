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
node scripts/inspect-canvas.js model            # what the MODEL says is drawn
node scripts/inspect-canvas.js geometry         # what the SVG actually contains
node scripts/inspect-canvas.js shot out.png     # screenshot the renderer window
node scripts/inspect-canvas.js eval expr.js     # evaluate a file's contents in the page
```

### model

The authoritative answer, and the one to reach for first. Needs a debug build: `MainView.displayView`
publishes `window.issie` only when `JSHelpers.debugLevel > 0`, which `npm run dev` gives you.

The projection is `ModelHelpers.canvasInspection`. Per symbol: position, `W`/`H`, `HScale`/`VScale`,
rotation, flip, port edges, and `DisplaysComputedValues`/`DeclaredType` — the last two being the
parameter display-value stash, `SymbolT.Symbol.SavedComponent`. Per wire: its segments in **absolute**
coordinates via `BlockHelpers.getAbsSegments`, rather than the relative lengths the model stores.

`window.issie.canvas` is a function, not data, so nothing is computed unless something asks. A render
costs one closure and one object.

The Model itself cannot cross the interop boundary — it holds F# maps, closures and React elements,
none of which survive `JSON.stringify`. So the projection is arrays, numbers, strings and bools
throughout: F# lists are linked lists in JavaScript, and options are resolved to a value or a flag
rather than passed on. Extend `canvasInspection` when you need another field; do not try to expose
the model directly.

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

- `model` needs a debug build; `geometry`, `shot` and `eval` work against any build with the port open.
- One renderer window only — the script takes the first `page` target.
- Native dialogs are outside the page and cannot be driven.
