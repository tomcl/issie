# User-friendliness review

Recorded August 2026 against `master` at v6.0.11, from a read of the feature set as a user meets
it: the catalogue, the properties pane, the three simulators, the error paths, the keyboard and
menu system, and the documentation site.

Issie's stated principle is that a novice should need no manual, and that every error should say
what is wrong **and how to correct it**. Measured against that principle Issie does far better than
its competitors — the *Fix by …* buttons in
[`SimulationView.viewSimulationError`](../../src/Renderer/UI/SimulationView.fs), the generated
shortcut table, the per-component tooltips and the context menus are all genuinely unusual. What
follows is only where it falls short of its own standard.

Findings are ordered by (cost to fix) ÷ (benefit), best first.

**Status, August 2026:** sections A, B and C are all done — each entry says how. C was deferred
once and then done in one pass, which is what it wanted: remapping keys is a change users have to
absorb, so it is worth doing all at once. Of D, only **D2** remains open: the screenshots. D1, D3
and D4 are settled.

(An earlier version of this line claimed A, B and C were done while C4 had not been touched, and
listed D1–D2 as the open ones while saying nothing about D4. Both are now true.)

---

## A. Defects — places that contradict the principle

### A1. A help menu item that says "Feature not explained"

`ContextMenus.fs:45` offers `"Waveform Operations"` on the wave simulator's right-click help menu.
`UIPopups.viewWaveInfoPopup` (`UIPopups.fs:442`) has that panel under the name
`"Viewing Waveforms"`. The names do not match, so choosing it reaches the wildcard case and the
user is told *"Feature not explained"* — in the help system, which is the worst place for it.

**FIXED.** The menu item is now `"Viewing Waveforms"`, matching the popup. One list is not possible
— `ContextMenus.fs` is compiled into the main process, which has none of the renderer, and
`UIPopups` is compiled long before it — so both sites now carry a note saying they must change
together, and the catch-all case says what to do instead of *"Feature not explained"*.

### A2. The combinational-loop error offers no remedy

`SimulationGraphAnalyser.fs:131` reports a combinational loop as, in full:

> Cycle detected in combinatorial logic.

The offending components *are* highlighted, which is good. But a combinational loop is one of the
two or three classic first-year mistakes, and the message does not say what a loop is, why it
cannot be simulated, or what to do (break the path, or put a register in it). Compare the
asynchronous-RAM cycle message in `FastRun.fs:334`, which explains itself properly, or the net
label messages, which are exemplary.

**FIXED.** The message now names the loop (`A -> B -> C -> (back to the start)`, read from the
graph), says why such a circuit has no value and would oscillate in hardware, and gives the two
ways out: remove a connection, or put a flip-flop or register in the path.

### A3. Internal type names leak into user-facing errors

`SimGraphTypes.errMsg` renders several errors with `%A` over F# union values
(`SimGraphTypes.fs:110–115`):

> `PortType.Input port appears to have no port number`

and `InPortMismatch`/`OutPortMismatch` print raw F# list literals. `InternalError`
(`SimGraphTypes.fs:177`) shows the user an exception message and a JavaScript stack trace.

**FIXED.** `%A` over `PortType` is gone (a `portTypeName` helper writes "input"/"output", so the
message cannot change shape if the type ever gains a field), and those three now say the sheet file
is probably damaged and point at the `backup` folder — the user cannot have caused them by editing,
so telling them to correct it sent them looking for something that was not there. The port-mismatch
messages say the sheet has been edited since the instance was placed and to replace the instance.
`InternalError` is framed as a request to report, with the trace below it. One `Option.get` inside
the error renderer, which would have thrown while displaying an error, is gone too.

### A4. `OutputConnError` with a non-zero count renders as a bare number

`SimGraphTypes.fs:155`:

```fsharp
| OutputConnError (count, _, _) ->
    if count = 0 then "A component output port must have at least one connection. …"
    else sprintf "%d" count
```

The error is currently raised only for `count = 0` (`CanvasStateAnalyser.fs:497` passes `(<) 0`),
so the branch is unreachable — but it was a live landmine behind a one-character change to a
predicate.

**FIXED.** The branch now carries a real message saying an output may drive any number of inputs
and asking for a report, with a comment recording that it is currently unreachable.

### A5. The truth table refuses without saying what to do about it

On a sheet with any clocked component, `Generate Truth Table` is drawn as an ordinary enabled
button (`TruthTableView.fs:944` and `:975` — only `Button.IsLight` distinguishes it), and clicking
it raises the notification *"Truth Table generation only supported for Combinational Logic"*.

The user is not told **which** components are clocked, and is not pointed at the thing that would
actually work — *Truth Table for selected logic*, which is right there on the same panel and does
exactly what they want. This is a click that goes nowhere.

**FIXED.** The button now reads **"Why is there no table?"** and is `IsWarning`, like the
"See Problems" button beside it — it no longer looks like the one that makes a table. Clicking it
highlights the clocked components on the schematic, names them (a custom component counts only if
the sheet inside it is really clocked, which `couldBeSynchronousComponent` cannot tell on its own),
explains why a table of sequential logic has no meaning, and points at both *Truth Table for
selected logic* and the Wave Simulation tab.

### A6. Dead unsafe helpers in the error view

`SimulationView.fs:409` and `:425` defined `getComponentById` / `getConnectionById`, which
`failwith` when the id is missing. Both were unused — the `…ListOpt` variants beside them are what
the code calls — but they are the exact shape of a crash inside the error display.

**FIXED.** Both deleted, with a note beside the survivors recording why the throwing versions were
wrong: a component named by an error can genuinely be absent, having been deleted between the
simulation and the render.

---

## B. First contact — the biggest wins

### B1. The waveform simulator opens empty

`initWSModel` sets `SelectedWaves = List.empty` (`ModelHelpers.fs:78`). A user who presses
**Start** on a clocked design for the first time gets a viewer with nothing in it and a sentence
telling them to press another button.

The first thing a beginner sees should be waveforms of their own design, not an empty grid, and
the *Select Waves* dialog should be something they reach for to refine rather than something they
must find in order to begin.

**FIXED.** `WaveSimSelect.withDefaultSelectionIfEmpty` selects the simulated top sheet's own
inputs, outputs and Viewers — inputs first, then outputs, then Viewers, capped at 12 — and
`refreshWaveSim` applies it when a new simulation is created. It runs only when the user has
selected nothing at all, neither waves nor RAMs, so a selection saved with the sheet is never
overridden and a deliberately pared-down one is never added to. Viewers are included because a
Viewer exists for no other reason than to be looked at. Pinned by tests in
`Tests/Issie.Tests/WaveSelection.fs`.

**A top sheet with no ports of its own** — found while checking this against the demos, and now
also handled. The `3cpu` demo's `eep1` is a ROM, a RAM, two constants and two custom component
instances, with not one Input, Output or Viewer, so the rule above selects nothing. The fallback is
every **Viewer in the design**, at whatever depth: a Viewer is placed for exactly one reason, that
somebody wanted to watch that net, so wherever they are they are the signals the author thought
worth looking at. A design with no top-level ports and no Viewers anywhere still shows the "use
Select Waves" message, which at that point is the honest answer.

### B2. The Catalogue has no search

`CatalogueView.viewCatOfModel` draws nine collapsible sections, ~41 components, plus *This project*,
*Verilog* and *Library*. There is no filter box. A user who knows they want a shifter has to guess
whether it is under Buses or Arithmetic (it is Arithmetic), and a user who does not know the Issie
name for what they want has no way to look.

**FIXED.** A search box sits above the catalogue, filtering on component name **and** tooltip text
— so "subtract" finds the N bits XOR and "invert" finds Not, which is the point of searching the
tooltip as well as Issie's own name for the thing. Sections holding a match open themselves;
sections with nothing in them are not drawn at all, since a column of empty headings says nothing
about where the component is. Project sheets, Verilog components and libraries are filtered with
everything else. The search string lives in `Model.CatalogueSearch`.

### B3. The Properties pane explains nothing

Every Catalogue entry carries a written tooltip. `SelectedComponentView.fs` — the pane where a user
actually configures the component they just placed — contains **no tooltips at all**. So the
Catalogue tells you what an N-bit XOR is for, and then the pane offering to change its arithmetic
op, its width, its label and its port set says nothing.

**FIXED.** `UI/PropertiesHelp.fs` holds the explanations, keyed by the label each field displays,
and `fieldLabel` renders a label with its explanation as a tooltip. Keying on the label means a
field acquires help simply by being labelled: no call site passes anything, the same label is
explained the same way wherever it appears, and a label with no entry renders exactly as before.
Both `SelectedComponentView`'s form helpers and `ParameterView.paramInputField` go through it, so
the parameter boxes are covered as well. A label with something to say is marked with a dotted
underline and a help cursor — an invisible tooltip is one nobody hovers.

### B4. No way to see a signal's value on the schematic

Values can be read in the step simulator's panel, in the waveform cursor column, and as a tooltip
on a waveform (`EvilHoverCache.fs`) — but never on the schematic itself. Hovering a wire during a
simulation and seeing its current value is the thing users of every schematic simulator ask for
first, and Issie already has all the pieces: the wave simulator can map a canvas component to its
simulation driver in both directions (`WaveSimSelect.wavesOfComponent`,
`WaveSimSelect.compWavesToOffer`), and `EvilHoverCache` already formats a value for a tooltip.

**FIXED, for both simulators.** Rest the mouse on any wire of the schematic while a simulation is
running and the value it carries appears beside the pointer — at the waveform cursor's cycle, or at
the step simulator's current clock tick, in that simulator's radix.

The work splits along the seam it sits on. The draw block records which wire the mouse is resting
on (`SheetT.Model.HoveredWire`, set from the `mouseOn` result the idle mouse-move already computes
for the cursor shape) and knows nothing about simulation. `MainView.Probe` turns that into a value
and knows nothing about hit-testing. `SheetDisplay.view` gained an `overlay` parameter so the label
is drawn in draw block coordinates, panning and zooming with the schematic, without the draw block
having to know what is in it.

Which copy of the wire is answered by `WaveSimSelect.wavesOfComponent`, the same function the
schematic's right-click menu uses: a wire on a sheet the simulation holds twice shows nothing,
because there would be no single answer.

Nothing had to be built for the step simulator: **every** `FastSimulation` already carries
`WaveIndex`, `WaveComps` and `Drivers`, because `buildFastSimulation` ends in
`addWavesToFastSimulation` whichever simulator asked for it. So the probe is two map lookups and an
array read, and the two simulators differ only in which simulation, cycle and radix are handed to
`WaveSimSelect.probeLabelForWire`. The waveform simulator wins when both are running: its cursor is
where the user deliberately put it, whereas the step simulator's tick is just how far it has been
stepped.

One thing the shared path had to get right: the step simulator uses its data arrays as a **circular
buffer** (`step % MaxArraySize`, as `FastExtract` does), while the waveform simulator does not — its
array is sized for the whole run. Taking the modulo unconditionally is correct for both, since in
the waveform case the cycle is always below `MaxArraySize`.

### B5. Nothing happens on first run

`viewInfoPopup` is reachable only from the **Info** button (and `Cmd+H` on macOS). A brand-new user
sees `New project / Open project / Open demo project` and nothing else. The *Tips & Features* tab of
the Info popup is good content that most users will never open.

**FIXED.** Three parts, and the judgement behind them is that the in-app text is the primary path
and the website carries the depth. A page a beginner has to go and find is exactly the manual Issie
is designed not to need; but a popup is the wrong shape for a tutorial, and the documentation site
is already good and is kept in step with the code by the same pull request.

1. The startup menu — the one thing a first-time user is certainly looking at — now ends with
   **"New to Issie? Start here"**, which opens the Info window. It is offered rather than opened
   for them: a second window in front of the first, before they have asked for anything, is an
   interruption rather than help.
2. The *Introduction* tab is now **Getting Started**, and is first, so it is what the window opens
   on. It was three paragraphs about what Issie is; it is now the first five minutes in order
   (open a demo → place components → wire them → Properties → simulate), then what happens when
   something is wrong, then the two things that cannot be worked out by looking at the screen (the
   hierarchy, and the implicit clock). *About Issie* — version and acknowledgments, which nobody
   opened this window for — moves back to fourth.
3. It ends with **Where to read more**: five links to the documentation site, one line each saying
   what is on the page. Depth lives there, one click away and clearly secondary.

*Tips & Features* also gained rows for the things added since it was written: catalogue search,
Properties tooltips, dragging from the catalogue, probing a wire, and the two directions between
waveforms and the schematic.

---

## C. Consistency

### C1. Two zooms, and the obvious keys drive the wrong one

`Ctrl+=` / `Ctrl+-` scaled the **whole application** via Electron's `setZoomLevel`. The
**schematic** zoom was `Alt+Up` / `Alt+Down` on Windows and Linux, and `Cmd+Opt+=` / `Cmd+Opt+-` on
macOS — so the two platforms had no chord shape in common either.

`Ctrl+=`/`Ctrl+-` is what a user reaches for to zoom a drawing, in every drawing application there
is. In Issie it silently rescaled the entire UI instead, which looks like the canvas zooming until
the menu bar changes size too. Worse, Issie's own `Ctrl`+wheel already zoomed the diagram, so the
same modifier meant two different things depending on whether a wheel or a key carried it.

**FIXED, as one rule: Primary with `+` `-` `0` zooms whatever you are looking at, and adding `Alt`
zooms the whole application.** "Whatever you are looking at" is the schematic in the sheet
contexts and the **waveforms** in the wave simulator — which the in-app help already promised and
did not have: what those keys did there was zoom the application.

`Ctrl+W` is retired. It stays in the table bound to nothing, so that the host cannot read it as
close-window — which on an application with one window means quit. Not bound on macOS, where
`Cmd+W` closing a window is a real convention rather than a browser habit.

`Shift+=` is `+` on most layouts, so those two must always be the same action; that is what stops
application zoom living on `Primary+Shift` and puts it on `Primary+Alt`.

### C2. `Cmd+H` is taken from macOS

`ScAbout` was `macOnly [ Cmd+H ]`, and shortcuts `preventDefault` by default. On macOS `Cmd+H`
hides the front application in every program — one of the most ingrained system shortcuts there
is, and Issie quietly took it. Meanwhile Windows and Linux had **no key for help at all**, and
`F1` was unbound everywhere.

**FIXED.** `F1` on both platforms; `Cmd+H` released.

### C3. Grid and wire arrows have keys only on macOS

`ScToggleGrid` and `ScToggleWireArrows` were `macOnly`. Windows and Linux users got menu items
only, and the generated shortcut table showed them a row reading "(none)".

**FIXED.** `Primary+Alt+G` and `Primary+Alt+W` on both platforms.

### C4. Small naming mismatches

- The Simulations sub-tab reads **"Truth Tables"**; the panel heading inside it reads
  **"Truth Table"**; the documentation used to say `Simulations -> Truth Table`.
- The Catalogue section is **"Arithmetic"** but holds `N bits AND`/`OR`/`NOT`, which are not
  arithmetic; the section named **"Gates"** holds only the 1-bit versions. A user looking for a
  wide AND will look in Gates.

**FIXED.** The sub-tab is now **"Truth Table"**, singular — which is what the panel inside it says,
and what its two neighbours are ("Step Simulation", "Wave Simulation"); the two documentation pages
that named the tab follow. `N bits NOT`, `AND`, `OR` and `XOR` have moved to **Gates**, below the
1-bit gates they repeat. What is left under Arithmetic is what computes on the value of a bus
rather than bit by bit: the adder and the shifter.

### C5. The same action, different chords on the two platforms

Rotate and flip were `Ctrl`+arrows on Windows but `Cmd`+**`Opt`**+arrows on macOS; align and
distribute `Ctrl+Shift+A`/`D` against `Cmd+Opt+A`/`D`; rotate-label used a different *key*
altogether, `Ctrl+Shift+Right` against `Cmd+Opt+R`. `Mods.Primary` exists precisely so that
Ctrl-against-Cmd needs no thought, so none of these had a reason — they are accidents of the
migration from the Electron menus, whose accelerators the table was written to preserve.

**FIXED.** All three are one chord for both platforms. A test asserts the general rule: a
shortcut's Windows and macOS chords must be identical unless its id is on a short list of ones
that differ for a real platform convention (Backspace deletes on macOS, redo is `Cmd+Shift+Z`,
full screen, quit, dev tools, and the Windows-only `Ctrl+W` swallow).

### C6. Space did nothing but suppress scrolling

`Space` was bound only to stop the page scrolling under the canvas. In every drawing application
`Space`+drag pans; Issie panned with `Shift`+drag alone, which is the less-guessed of the two.

**FIXED.** `Space`+drag pans as well. Tracked as held state beside `ctrlHeld` in `KeyBindings`
rather than as a chord — the table resolves one press to one action and has nowhere to say "while
down". Only in the sheet contexts, so a space in a text box is still a space. The cursor shows the
mode, and pan mode outranks whatever is under the pointer, so it cannot offer to grab a wire when
the next drag is going to pan.

---

## D. Documentation and release information

### D1. `RELEASE_NOTES.md` stops at v0.5.0

The app is v6.0.11. The release notes file's most recent entry is *"v0.5.0 — Issie 1st release"*.
A user asking "what changed?" finds a file that answers for 2020. The `docs/updates/` blog is
better but stops at April 2025, and there has been a great deal since — parameters, libraries,
the in-app menu and keyboard system, the project browser, the Fable 5 / .NET 10 move.

**Fix:** either keep the file current (the git log is already written in a style that would
generate it) or delete it and point at the GitHub releases page, which *is* current.

**FIXED, by deleting it.** Nothing in the repository referenced it, and the releases page carries
notes for every release already. The README's *Getting Started* section, which is where a user is
sent to download a build, now says that is also where to read what changed, and why there is no
file — so the next person to look for one finds the answer rather than the question.

### D2. The screenshots on the documentation site are a version behind

`docs/img/homePage/*` and `docs/img/userGuide/*` show a **Sheets** menu and an Electron application
menu bar that no longer exist — the bar is now `Project | Sheet | Edit | View`, drawn inside the
app. They are still recognisably Issie and the workflows are unchanged, so they are usable, but a
refresh would be worth doing before the next release. `scripts/inspect-canvas.js` can screenshot
the running app, so this could be scripted rather than done by hand.

### D3. The licence is stated two ways

`LICENSE.md` and `LICENSE` are GPL v3. `package.json` says `"license": "LGPL-3.0-or-later"`. These
are different licences. One of them is wrong, and the documentation cannot state the licence
confidently until it is settled.

**FIXED — `package.json` now says `GPL-3.0-or-later`.** The licence text has been the GPL since
`89ad2aaec Add GPLv3 license`, and `LICENSE` and `LICENSE.md` are byte-identical copies of it. The
LGPL string was not a relicensing: it was written into an empty `"license": ""` field during a
routine version bump (`13182ba29 bump to v4.0.6`, August 2023) and never matched anything. The
"or later" is kept from what was there, which is also what the GPL's own recommended boilerplate
says. README now states the licence in prose so there is one place to read it.

If LGPL really was intended, this change is the wrong way round — but that is a relicensing
decision, and it would mean replacing `LICENSE`/`LICENSE.md`, not editing a JSON field.

### D4. `issiestick.md` says "More documentation needed here!"

The ISSIE-Stick page is nine lines and admits it is incomplete, while the feature is advertised on
the home page. Either write it or fold it into the Verilog Output page.

**FIXED, by saying what it is: older work that still exists but is not maintained.** Writing the
page properly would mean reviving the flow first — checking it against current yosys and nextpnr,
saying which versions work — and folding it into Verilog Output would put unmaintained work beside
the maintained kind. So the page now leads with the fact that it is unmaintained, records what was
built and where the code and the hardware repository are, and ends with what reviving it would
involve. It has stopped being advertised as a current feature on the home page, in Features and in
Verilog Output, each of which now sends the user to their own toolchain and mentions the build flow
as history; and it moves below Technical Reports in the nav.

---

## What is already right, and should not be disturbed

Worth recording, because these are the things a redesign would be tempted to "simplify":

- **The *Fix by …* buttons.** Applying the fix *and* restarting the simulation is what makes them
  feel like help rather than a suggestion.
- **Generating the shortcut table from the dispatch table.** It caught three real errors in the
  hand-written table it replaced.
- **Compulsory parameter descriptions**, and author-written constraint error text.
- **Tooltips written as sentences**, not as labels — the net label tooltip teaches the concept.
- **Materialising library components into the project** rather than linking them. A student can
  open one and read it.
- **Refusing a drop onto occupied space** instead of silently overlapping two symbols.
