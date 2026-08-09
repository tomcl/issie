# Parameter System: Planned Redesign

This document records the agreed design for the next stage of the Issie parameter system.

**Implementation status**: the design below is implemented (see
[parameterSystem.md](parameterSystem.md) for where the code lives), with these deviations and
deferrals:

- **Drawing at computed values is implemented**, as described in
  [Drawing at computed values](#drawing-at-computed-values) below — with one addition that section
  did not anticipate. A custom component instance's ports follow from a `CustomCompParam` binding
  by way of the child sheet, which `ComponentSlots.setSlotValue` cannot reach, so the declared
  ports are stashed alongside the declared slot values in `Symbol.DeclaredPortLabels`.
- **Binding is total**: every instance binds every parameter its sheet declares, rather than
  carrying no bindings until one is created. See the note below.
- **Deletion**: the existing behaviour (refusing deletion while slots on the sheet use the
  parameter, and dropping dead bindings from instances elsewhere) is kept unchanged; the
  pass-through-chain listing remains a possible extension.
- **Undo/redo** restores whole model snapshots, so re-doing a placement does not re-fire the
  component-added trigger. The next qualifying event re-checks.
- **A library sheet can now be opened after all**, though not edited. The plan below says they
  "cannot be opened", and that was the behaviour until it met the argument that Issie is a
  teaching tool, where wanting to see how a library component works is fair. An instance's
  right-click menu offers *View library component*, which opens that one sheet read-only and lists
  it in the Sheets menu until the project is closed. Nothing about the sheet can change, nothing is
  copied out of it, and nothing survives a reopen, so the sheet is still not part of the design the
  user works on - which is what the restriction was protecting.
- The **later extensions** (instance-path viewing, memory parametrisation) are not implemented.

Two behaviour changes beyond the plan text:

**Placement asks.** Placing a custom component instance no longer copies the parent sheet's
default bindings into the instance as frozen values (the old behaviour, which was precisely the
silent-stale-chain problem). Instead the placement popup asks for a value for every parameter the
child sheet declares, showing each one's description, with a button to bind to a same-named
parameter of the sheet being placed onto.

**Every instance binds every parameter.** The plan left an instance's parameter *unbound* until a
binding was created, and displayed that state as "(default; unbound)". That state is now designed
away rather than presented: an unbound parameter elaborates at the sheet's own declared value,
which is a fact about the sheet and not about the instance, and it makes "default" a concept the
user has to reason about. Placing an instance establishes the binding, and
`ParameterAnalysis.bindParamOnInstances` fills the hole a parameter added to a sheet that already
has instances would otherwise leave. This retargeted the bind-to-top offer, which fired on unbound
parameters and so could never fire again: it now fires on a parameter bound to a plain **number**,
which is the state the offer is actually useful in.

## The problem

Issie parameters today are **per-instance arguments**: a sheet declares parameters with default
values; slot expressions on its components use them; a custom component instance may explicitly
bind the child sheet's parameters to expressions in the parent's parameters, one level at a time.

This serves one of the two common use cases well, and fails the other:

1. **Library components with adjustable widths** (e.g. an N-bit adder defined as an Issie sheet).
   The parameter is set at the use site, per instance, and instances are supposed to differ. The
   current mechanism is exactly this, and it works.
2. **A design-wide constant** (e.g. the datapath width of a CPU). The constant has one value per
   elaborated design, set once, with every instance agreeing. The current mechanism forces this to
   be emulated with a chain of arguments: re-declare the parameter on every sheet in the
   hierarchy, re-bind it at every instance — O(instances × levels) of bookkeeping — and a missed
   link *silently* elaborates that subtree at default values: a working simulation of the wrong
   design.

A second problem is display. A parametrised sheet is a family of designs, and the editor can show
only one member. Parameters must have values for a sheet to be drawn and checked at all, but those
values cannot in general be the "real" values, because one sheet may be instantiated with several
different bindings.

## Requirements

1. **Zero-cost ignorance** — users who never touch parameters see no change anywhere.
2. **Use case 1 unchanged** — a per-instance argument set in the properties pane, like a built-in
   component's width.
3. **Use case 2 in one edit** — changing the constant at the top of a design changes it everywhere
   in that design.
4. **Declared dependence** — every parameter that can affect a sheet is visible on that sheet,
   with a default, so the sheet is viewable and simulatable standalone.
5. **No project-level semantics** — sheets remain the semantic unit; a project may contain several
   top-level sheets, each the root of its own design.
6. **No silent defaults** — a missing or stale parameter chain is flagged (with an offered
   repair), never quietly simulated at default values.
7. **Checking split** — elaboration-time checking is exact with informative errors; design-time
   checking is best effort with no false positives.

## The design

Elaboration semantics are **unchanged**: only explicit per-instance bindings exist. Everything
below is analysis plus UI. (A rejected alternative — auto-binding unbound parameters outward by
name, i.e. dynamic scoping along the instance path — reaches the same end state but implicitly; it
brings name capture, accidental unification of unrelated same-named parameters, the need for a
"local" opt-out marker, and new semantics to teach.)

### Open-time analysis

On opening a sheet, walk the instance tree under the current top sheet and compute, for each of
the sheet's parameters, the set of values it takes across all instances. (This reuses the binding
walk that simulation elaboration already performs, without building simulation graphs.) Display
rule per parameter:

- **Singleton set** — show the real value. It is exact, and design-time width inference runs on
  real values.
- **Empty set** (the sheet is not instantiated under the top) — show the declared default.
- **Multiple values** — show the default, with a note enumerating the values and the instance
  paths that produce them: *"W = 8 at CPU_TOP > FetchAdder, 16 at CPU_TOP > ALU; showing
  default 8."*

Multi-valuedness is not declared anywhere — it is *detected*, so a "library sheet" needs no
special kind: any sheet instantiated with differing bindings displays as one.

### The top sheet

- The model records a **current top sheet** per project: view state, persisted with the project's
  other non-semantic state. It changes what the editor displays, never what anything means.
- When the instance forest has a single root, the top is inferred silently and the user never
  meets the concept.
- A popup asking the user to choose a top fires only when multiple tops exist, they disagree
  about the sheet being opened, and no top is selected — roughly once per project. Cancelling
  still opens the sheet with defaults and a note; the popup never blocks opening.
- The sheet-menu pills colour the top distinctly, and distinguish sheets outside the top's tree
  (which necessarily display defaults). "Set as top" is on the pill right-click menu.

### Editing

Editing on a sheet always targets **definitions** — parameter defaults and slot expressions.
Contextual inherited values are read-only annotations naming their source
(*"W = 64, from CPU_TOP; default 32"*). Bindings are edited where they live: on the instance, in
the parent sheet.

### The bind-to-top offer

*As implemented the trigger is a parameter bound to a plain number, not an unbound one — see the
implementation status above.* Where a custom component instance's parameter qualifies, the UI may
offer:

> Bind to CPU_TOP:width? width parameters and bindings will be created in sheets X, Y, Z.

Accepting materialises the chain — ordinary persistent parameters and explicit bindings along
**all** instance paths from the top to this sheet — created once, with consent, and thereafter
edited like anything else.

- **Evidence gate**: the offer appears only if a same-named parameter already exists on an
  ancestor sheet along the instance path under the current top. An unbound parameter alone is not
  evidence of a design constant; a same name on an unrelated sheet is coincidence. Parameter-free
  projects never see the offer.
- The confirmation enumerates every sheet to be modified; modified sheets become dirty and are
  saved to disk like any other programmatic correction.
- Declining leaves the parameter at its default, noted in properties.

### Event-driven offers

Offers fire at the three events that can bring a qualifying chain into existence — a complete set
for in-session edits, so nothing goes silently stale:

1. **Custom component added** (place, paste, import, duplicate): check the new instance's
   parameters, and scan its whole subtree — unbound qualifying parameters in subsheets are
   offered up in the same interaction. This also covers hierarchy restructuring: interposing a
   new sheet between the top and an already-threaded sheet is just two component additions, and
   the subtree scan at the second offers the full rebuilt chain.
2. **Parameter added** to a sheet: unbound same-named parameters in its subsheets are offered.
3. **Top sheet changed**: the check re-runs under the new top; re-offering something previously
   declined is correct here because the ancestor context genuinely changed.

Parameters created by accepted offers persist and thereafter satisfy the evidence gate for future
placements. Declines need no persistent record: triggers 1 and 2 are naturally one-shot.

The open-time analysis remains as the display-value computation and as a safety net for projects
last edited outside these triggers (older Issie builds, hand-edited files).

### Deletion and cleanup

- Deleting a parameter lists the dependent pass-through chain, extending the existing behaviour
  of listing referencing slots before allowing deletion.
- Later, optionally: an offer to delete pass-through parameters no longer referenced below.

## Drawing at computed values

A sheet drawn at its declared defaults is, in many designs, obviously wrong: the widths on screen
are not the widths the sheet elaborates to under the top. The fix is to draw the open sheet at the
values its parameters actually take, **without changing what is saved**. The declaration stays the
declaration; the `.dgm` of a sheet that is only viewed is byte-identical to today.

For a given top sheet and a given displayed sheet, the parameters that have a definite value are
exactly the `ExactValue` cases of `ParameterAnalysis.displayValues` — every instance under the top
agrees. Those, and only those, are pushed onto the canvas.

### The computed component, and the declared values beside it

`Symbol.Component` holds the **computed** component; a new field `Symbol.DeclaredSlots :
Map<CompSlotName, int>` holds the DECLARED value of each parameterised slot the symbol is drawing
differently, and is empty for almost every symbol.

*As planned this was `Symbol.SavedComponent : Component option`, a whole declared component. That
was wrong and was changed while implementing: reverting a whole component on save also reverted
every edit made while computed values were on display — a constant's value, a memory's contents,
the label. Only the slot values are stashed, so everything that is not a parameterised slot is
saved as it stands.*

*Slot values alone turned out to be not quite enough, and a second field
`Symbol.DeclaredPortLabels` was added later. A custom component instance is the one symbol whose
slot value does not name a number in its own type: a `CustomCompParam` slot binds a parameter of
the sheet INSIDE the instance, and the instance's port widths follow from that binding by way of
the child sheet. `ComponentSlots.setSlotValue` can put the binding back but not the ports it
implies, having no access to the child sheet, so a sheet saved while showing computed values wrote
an instance whose ports contradicted its own bindings — which is what the simulator's custom
component check rejects. The declared ports are therefore remembered whole.*

The direction matters and is forced by React caching. `SymbolView.renderSymbol` is a
`FunctionComponent.Of(..., "Symbol", equalsButFunctions)` whose memo key is the whole `Symbol`
record. Holding the computed value anywhere outside the symbol — a model-level map consulted at
draw time — leaves every `Symbol` structurally unchanged when the top sheet changes, so
`equalsButFunctions` suppresses the re-render and the canvas silently goes stale. Putting the
computed value in `Symbol.Component` makes the memo correct by construction, and every existing
reader (`drawComponent`, port geometry, `H`/`W`, width inference, `GetComponentById`) gets real
values with no change.

Computed components are produced through the existing `ParameterView.updateComponents` path
(`ChangeWidth`, `ChangeSplitN`, …), not by patching `Component.Type`, so symbol size, ports and
port geometry are all recomputed properly. The declared value of each slot about to be displayed
differently is stashed in `DeclaredSlots` first, by `ParameterView.stashDeclaredSlots`.

### One funnel keeps the file unchanged

`SymbolUpdate.extractComponent` is the sole path from symbols to saved state: `extractComponents`
→ `Sheet.GetCanvasState()` → both save paths, the backup write, and `currentSheetIsOutOfDate`.
Putting the declared slot values back there (`SymbolUpdate.declaredComponent`) makes saving,
autosaving and the dirty flag all correct at once, with no normalise-on-save step and no invariant
to maintain at boundaries.

It is a merge, not a substitution: `storeLayoutInfoInComponent` writes `SymbolInfo`, `X` and `Y`
from the live symbol, and only the parameterised slots of `Type` are put back. `H` and `W` are left
as drawn — they are recomputed on load from the type, so a saved size derived from a computed value
does not survive to be wrong.

### Decisions

- **Input counts are not parameterisable.** The `NGateInputs` slot is removed from `CompSlotName`
  outright, not merely made uncreatable. An input count sets how many ports a component has, so a
  computed value would make `SymbolInfo.PortOrder` name ports the saved type does not have. The
  number of inputs of a gate or merge is edited as a plain number in Properties.
- **Properties pane** shows the symbol name and its viewed (computed) value, and the default as
  well when the two differ. The viewed value it already gets for free from `GetComponentById`; the
  default is an added annotation read from `DeclaredSlots`.
- **Copy and paste preserve both components**, and the parameter linkage with them: pasted
  components get fresh ids, so their slot expressions must be duplicated into `ParamSlots` under
  those new ids. More work than dropping to a literal, but semantically correct — a pasted
  parameterised component stays parameterised.
- Residue not worth engineering around: dragging a symbol whose displayed footprint differs from
  its saved one can produce overlap when the sheet is reloaded at defaults.

## Component libraries

A layer above sheets: reusable parameterised components shipped with Issie, placed like catalogue
components and materialised into the project on use.

**Implemented** in `src/Renderer/Interface/ComponentLibraries.fs` and the catalogue, with these
deviations from the plan text below:

- The **placement popup is not library-specific**: it was built in the previous phase for any sheet
  that declares parameters, and libraries reuse it unchanged.
- The **index is optional**. With no `index.json` the library directory is scanned instead, which
  is slower but gives the same result, so a library works with no tooling at all.
  `ComponentLibraries.writeLibraryIndex` regenerates one; there is no build step wiring it in.
- **No library ships yet.** `static/libraries/` holds only a README describing the layout, so
  `readLibraries ()` returns `[]` and no Library item appears in the catalogue. The mechanism is
  therefore untested against a real library.
- **Multi-sheet components** are recognised but not placed: a sheet instantiated by another sheet
  of the same library is correctly kept out of the catalogue, but only the component's own sheet is
  copied into the project, so such a component would arrive incomplete.
Library components are opaque in the waveform simulator as they are in the Sheets menu: the
instance's own ports appear like any custom component's (`sheet.L<n>_Comp1.port`), but nothing
inside it is offered as a wave and its sheet does not appear in the design hierarchy.
`WaveSimSVGs.getWaves` is the single choke point that filters them, testing each component's
`AccessPath` for an instance of a library sheet.

### UI

The catalogue gains a top-level **Library** item. Opening it lists the available libraries;
clicking one replaces the catalogue body with that library's components, arranged in sections just
as the catalogue is, under a header showing "*XXX* library" and a **Back** control returning to the
catalogue.

Dragging a library component onto a sheet places a custom component. If the library sheet declares
parameters, a popup requires a value for each before placement — and, uniformly, **creating a
custom component from any sheet that declares parameters raises the same popup**. The drag adds the
library sheet(s) to the project if not already present.

### Library sheets in a project

Library sheets are ordinary sheets with restrictions. They do not appear in the Sheets menu and
cannot be opened. When the last custom component referencing a library sheet is deleted, the sheet
is removed from the project.

> As implemented, one of those restrictions is looser: a sheet can be opened read-only by asking
> for it from an instance's right-click menu, and appears in the Sheets menu while it is. See the
> implementation status at the top.

`CCForm.Library` already exists in `CommonTypes` and is currently unused, so it is free to carry
exactly this meaning. Hiding is a UI property only: parameter analysis, width inference and
simulation must continue to see library sheets like any other.

### On disk and the index

Library sheets live in `static/libraries/<libname>/<compname>.dgm`. `FilesIO.staticDir()` already
resolves `static/` for both development and packaged builds, and the existing demo-project flow
(`openDemoProjectFromPath`) is the precedent for shipping `.dgm` files with the app; adding a sheet
to the project is what `FileImportSheet` already does.

Reading every sheet of every library at startup does not scale, so each library carries an
**index** — enough to render its catalogue entries and its parameter popups without opening a
single `.dgm`: component name, description, section, port summary, and each parameter's name,
default and constraints. Only the drag itself reads `.dgm` files.

### Multi-sheet library components (future)

A library component may be a multi-sheet design. All of its sheets are added to the project and
hidden. Within a library, a custom component instance marks its target as *part of* the enclosing
component rather than a separate catalogue entry — which is `instanceForestRoots` applied within
one library folder, so the index generator can compute the distinction with existing code.

### Open questions

- **Name collisions.** `CustomComponentType.Name` refers to sheets by name, so a library `Adder`
  and a user `Adder` cannot coexist. Needs a namespacing or rename-on-import rule before anything
  else here is built.
- **Deletion versus undo.** Removing the sheet when its last instance goes is a project-level side
  effect, but undo restores model snapshots — an eager delete makes undo unable to resurrect the
  sheet. Sweep unreferenced library sheets at save or close rather than on the delete itself.
- **Divergence.** Once copied in, a library sheet is a project sheet and does not track later
  library versions. Probably wanted; should be stated rather than discovered.
- **The placement popup versus the bind-to-top offer.** Both now fire when a parameterised instance
  is created. The popup should probably offer "bind to the top sheet's *X*" alongside a literal
  value, which would unify the two mechanisms rather than stacking them.
- **Keeping the index honest.** A hand-maintained index will rot; it needs a generator run as a
  build step or dev command.

## Later extensions

- **Instance-path viewing**: open a sheet via the breadcrumb as a particular instance
  (`CPU_TOP > FetchUnit > Adder(W=16)`) and see it with that instance's values — the strong
  answer for multi-valued sheets, mirroring how RTL tools pair module source with an elaborated
  hierarchy browser.
- **Memory parametrisation**: RAM/ROM address and word widths as parameter slots, with validity
  checking of `.ram` contents against the resolved widths.
