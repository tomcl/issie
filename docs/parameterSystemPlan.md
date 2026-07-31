# Parameter System: Planned Redesign

This document records the agreed design for the next stage of the Issie parameter system.

**Implementation status**: the design below is implemented (see
[parameterSystem.md](parameterSystem.md) for where the code lives), with these deviations and
deferrals:

- **Display of real values** happens in the properties pane (parameter table annotations); the
  canvas itself is still drawn and width-checked at default values. Drawing the canvas at the
  singleton real values is a possible later step.
- **Undo/redo** restores whole model snapshots, so re-doing a placement does not re-fire the
  component-added trigger. The unbound state remains visible as a "(default; unbound)" note in
  the instance's properties, and the next qualifying event re-checks.
- **Deletion**: the existing behaviour (refusing deletion while slots on the sheet use the
  parameter, and dropping dead bindings from instances elsewhere) is kept unchanged; the
  pass-through-chain listing remains a possible extension.
- The **later extensions** (instance-path viewing, memory parametrisation) are not implemented.

One behaviour change beyond the plan text: placing a custom component instance no longer copies
the parent sheet's default bindings into the instance as frozen values (the old behaviour, which
was precisely the silent-stale-chain problem). New instances carry no bindings and elaborate at
their sheet's defaults until a binding is created explicitly or through an accepted offer.

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

Where a custom component instance's parameter is unbound, the UI may offer:

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

## Later extensions

- **Instance-path viewing**: open a sheet via the breadcrumb as a particular instance
  (`CPU_TOP > FetchUnit > Adder(W=16)`) and see it with that instance's values — the strong
  answer for multi-valued sheets, mirroring how RTL tools pair module source with an elaborated
  hierarchy browser.
- **Memory parametrisation**: RAM/ROM address and word widths as parameter slots, with validity
  checking of `.ram` contents against the resolved widths.
