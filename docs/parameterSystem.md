---
title: Parameter System
category: Documentation
categoryindex: 1
index: 8
---

# Parameter System Documentation

## Overview

The parameter system in Issie allows users to define symbolic integer parameters on design sheets and use them in mathematical expressions to configure component properties. This enables parameterized component design where values can be dynamically calculated based on parameter bindings rather than being hardcoded. The system supports hierarchical parameter scoping and instance-specific parameter overrides.

## Key Features

- **Symbolic Parameters**: Define named integer parameters (e.g., `WIDTH`, `SIZE`) at the sheet level
- **Expression Language**: Use arithmetic expressions and the built-in functions `clog2`, `min` and
  `max` (`WIDTH + 1`, `SIZE * 8`, `max(clog2(N),1)`) to calculate values
- **Hierarchical Scoping**: Parameters defined at sheet level, overridable per component instance
- **Constraint System**: Define min/max constraints on parameter values with custom error messages
- **Custom Component Support**: Pass parameters between sheets through custom components
- **Real-time Evaluation**: Expressions evaluated dynamically as parameters change

## Architecture

The parameter system is implemented across several key modules with clear separation of concerns:

### Core Data Types (`ParameterTypes.fs`)

The parameter system's foundation is built on these core types:

#### Basic Types
- **`ParamInt`**: `bigint`. The fields a parameter feeds are bigint everywhere else in Issie — a
  constant's value, a bus comparison value, an input's default — and a bus may be up to
  `NumberHelpers.Constants.maxIssieBusWidth` bits wide. `ParameterTypes.tryIntOfParamInt` narrows
  back to `int` at the one place it must: `ComponentSlots.trySetSlotValue`, where a width, an index
  or a bit position is written into a component. A value too large to be an `int` is refused there
  rather than wrapped
- **`ParamName`**: Encapsulated string representing a parameter identifier

#### Expression AST
```fsharp
/// Named by an enumeration, so that adding a two-argument function is a case HERE
type ParamBinFunc =
    | PMin
    | PMax

type ParamExpression =
    | PInt of ParamInt                              // Integer constant, of any size
    | PParameter of ParamName                       // Parameter reference
    | PAdd of ParamExpression * ParamExpression     // Addition
    | PSubtract of ParamExpression * ParamExpression // Subtraction
    | PMultiply of ParamExpression * ParamExpression // Multiplication
    | PDivide of ParamExpression * ParamExpression  // Division
    | PRemainder of ParamExpression * ParamExpression // Modulo
    | PCLog2 of ParamExpression                     // clog2(x)
    | PBinFunc of ParamBinFunc * ParamExpression * ParamExpression // min(x,y), max(x,y)
```

Saved files key a DU case by its **name**, not its position (`SimpleJson/Json.Converter.fs`), so
appending a case leaves existing `.dgm` and `.ldgm` files readable. `PInt` now writes as a quoted
string (`{"PInt": "16"}`) because that is how bigint is encoded — the same form `Constant1` has
always used — and both the numeric and the string form are accepted when reading, so older files
load unchanged. Files written by this version cannot be opened by an older Issie.

#### Constraints
```fsharp
type ParamConstraint =
    | MinVal of ParamExpression * ParamError  // Minimum value constraint
    | MaxVal of ParamExpression * ParamError  // Maximum value constraint
```

#### Component Slots
```fsharp
type CompSlotName =
    | Buswidth              // Component bus width
    | IO of Label: string   // Input/Output port widths
    | CustomCompParam of ParamName: string // Custom component parameters
    | SplitNWidth of Index: int // SplitN output width
    | SplitNLSB of Index: int   // SplitN output LSB
    | InputDefault          // Value an Input1 takes when undriven
```

The number of inputs of a gate or merge is deliberately not a slot: an input count sets how many
ports a component has, and a parameter records a value, not a change of topology. It is edited as
a plain number in Properties.

The label in an `IO` slot is **not part of the slot's identity**. It records the component's label
as it was when the slot was created, and nothing rewrites it when the component is renamed.
`ParameterTypes.sameSlot` therefore compares `CompId` and the slot *kind*, and `tryFindSlot` /
`addSlot` / `removeSlot` are what every reader and writer uses. Treating the label as part of the
key let a rename orphan the slot and a second slot be created for the same field, with which of
the two applied decided by `Map` key order. `CanvasExtractor.tidyParamSlots` repoints the stored
label on save, so it stays worth displaying; nothing depends on it being right.

#### Sheet-Level Definitions
```fsharp
/// What an INSTANCE binds: no description, because the description belongs to the declaration
type ParamBindings = Map<ParamName, ParamExpression>

/// The DECLARATION of one parameter on a sheet
type ParamDefinition = {
    Expression: ParamExpression     // its default value
    Description: string             // compulsory: see below
}
type ParamDefinitions = Map<ParamName, ParamDefinition>

type ParameterDefs = {
    DefaultBindings: ParamDefinitions // the parameters this sheet declares
    ParamSlots: ComponentSlotExpr     // component slots bound to expressions
}
```

A parameter **must** carry a description. It is what the user reads when a custom component
instance of the sheet asks them for a value, so a parameter without one cannot be explained at the
point it has to be understood. `addParameterBox` and `editParameterBox` both refuse to commit an
empty description.

`ParameterTypes.bindingsOf : ParamDefinitions -> ParamBindings` drops the descriptions, and is how
every evaluation environment is derived from a sheet's declarations —
`ParameterAnalysis.declaredParams`, `ParameterView.getDefaultParams` and
`GraphMerger.defaultBindingsOfSheet` all go through it. Use `declaredParamDefs` /
`getDefaultParamDefs` where the description itself is wanted.

### UI Layer (`ParameterView.fs`)

Manages all parameter-related user interactions:

#### Sheet Parameter Management
- **`addParameterBox`**: Create new sheet parameters via popup dialog
- **`editParameterBox`**: Modify existing parameter values
- **`deleteParameterBox`**: Remove parameters from sheet
- **`makeParamsField`**: Display sheet parameters in properties panel

#### Component Parameter Binding
- **`paramInputField`**: Generic input field supporting parameter expressions
- **`makeParamBindingEntryBoxes`**: UI for custom component parameter bindings, one labelled box
  per parameter the child sheet declares, prompted with that parameter's description
- **`updateComponent` / `updateComponentSlots`**: Apply parameter changes to components via Sheet
  messages. All of one component's slots go together, because two of the messages replace a whole
  field (a `SplitN`'s width and LSB lists, a custom component's bindings and ports)

#### Parameter Evaluation
- **`evaluateConstraints`**: Validate expressions against constraints. A **pure function**: the
  constraints that are not met are returned, not dispatched. It must stay that way —
  `editParameterBox`'s `isDisabled` calls it *while rendering*, so anything it dispatched would
  re-render and dispatch again
- **`updateComponents`**: Batch update all parameterized components
- **`markSheetParamsChanged`**: Flag the open sheet as needing saving. An edit confined to what a
  sheet declares leaves the canvas identical, and `UpdateHelpers.currentSheetIsOutOfDate` compares
  canvases, so without this the save button stays dark and the work is dropped when the sheet is
  left. The work is `ParameterAnalysis.markSheetOutOfDate`, a function of the loaded components

### Simulation Integration (`GraphMerger.fs`)

Handles parameter resolution during simulation graph construction:

#### Stage 1 - Graph Merging
- Custom components replaced with internal graphs
- Parameter resolution intentionally deferred to avoid forward references
- Graphs stored in `CustomSimulationGraph` field

#### Stage 2 - Parameter Resolution (`resolveParametersInSimulationGraph`)

A single recursive walk (`resolveSheet`) resolves each sheet and then descends into the sheets of
the custom components it contains:

- The top sheet is resolved with its own default bindings.
- Each sheet below is resolved with its *effective bindings*: the instance's bindings override the
  sheet's defaults, and every declared parameter always has a value.
- An expression that cannot be evaluated fails the whole simulation with an informative
  `SimulationError` — the widths in a `SimulationGraph` must be concrete, and skipping one would
  simulate hardware that differs from the design.
- The walk is memoised on `(sheet name, diff of effective values from default values)`: every
  instance of a sheet whose bindings give the same diff resolves to the same graph, so a sheet
  tree recurring in several places is walked once, and the common all-defaults case adds no work.

### Design-Time Analysis (`ParameterAnalysis.fs`)

This whole layer is analysis and UI. **Elaboration semantics are untouched**: only explicit
per-instance bindings exist, and everything here is derived display state and consented repair.
(The alternative — auto-binding unbound parameters outward by name, i.e. dynamic scoping along the
instance path — reaches the same end state implicitly, and brings name capture, accidental
unification of unrelated same-named parameters, a "local" opt-out marker, and new semantics to
teach.)

- **`analyseUnderTop`** walks the instance tree under a top sheet over `LoadedComponent`s (the
  same binding walk `GraphMerger.resolveSheet` performs, without building graphs), recording for
  every sheet each instance's path and evaluated parameter values. Memoised on
  `(sheet, values)`; unevaluable bindings become *unknown* values, which are never reported as
  conflicts (design-time checking must not give false positives).
- **`displayValues`** turns this into a `ParamDisplayValue` per parameter. `NotUsed v` — nothing
  under the top instantiates the sheet, so the stored value is all there is; the properties pane
  shows it greyed and italic, as a placeholder rather than a fact, and this is the only case where
  editing it is offered. `Values vs` — distinct and descending, the head being the value the sheet
  is drawn at; several arise when one design reaches the sheet by paths that bind it differently,
  which is allowed, and the largest is taken so that the choice is definite and the recomputation
  idempotent. `ParameterView.makeParamsField` renders the rest after it — *"16 (also 8)"*,
  *"16 (also 8, 4, ...)"* — and says nothing about a declared value in either case, since naming a
  number that has been overwritten only asks the user to reason about one that no longer applies.
- **The top sheet** is per-project view state: an `IsTopSheet` flag on the chosen sheet,
  persisted in its `.dgm` `SheetInfo` and mirrored in `LoadedComponent`. `effectiveTopSheetFor`
  is **total** — it uses the flagged sheet, else the single instance-forest root containing the
  sheet asked about, else that sheet itself — which is what lets every other value be derived
  rather than guessed. "Set as top" is on
  the sheet-pill right-click menu (`MenuHelpers.setTopSheetState`); pills colour the top green
  and grey out sheets outside its tree (only when the project uses parameters at all). When
  several candidate tops disagree about the sheet being opened and none is chosen, a
  non-blocking choice popup fires once (`ParameterView.topSheetChoiceCheck`).
- **Placement** (`CatalogueView.startPlacingCustomComponent`): placing an instance of a sheet that
  declares parameters raises `ParameterView.customComponentParamPopup`, which asks for a value for
  each one, showing its description. Placing without asking would silently freeze the child
  sheet's defaults into the instance, which is the stale-chain problem; asking makes the choice
  explicit. Where the sheet being placed on declares a parameter of the same name, a button binds
  to it instead of taking a literal value. The instance's ports are sized at the chosen bindings
  before it is created, and `addParamComponents` records its slots once it has an id.
- **Every instance binds every parameter its sheet declares.** Placing one establishes that, and
  `ParameterAnalysis.bindParamOnInstances` fills the hole a parameter added to a sheet that
  already has instances would otherwise leave. An unbound parameter is a state the design
  deliberately does not have: it elaborates at the sheet's own declared value, which is a fact
  about the sheet rather than about the instance, and it makes "default" a concept the user has to
  reason about. `everyInstanceBindsEveryParam` is the invariant; it is false only for a project
  written by an older Issie or edited by hand.
- **Bind-to-top button** (`findBindOffers` / `ParameterView.applyBindOffers`): where an instance's
  parameter is bound to a **plain number** and a same-named parameter exists on an ancestor sheet
  along the instance path under the top (the evidence gate), a button in that instance's
  properties materialises the chain - ordinary parameters and explicit `PParameter` bindings along
  every instance path from that ancestor down. A literal is the right trigger: the offer exists to
  help follow an outer parameter of the same name, and typing that name in by hand fails whenever
  a sheet in between does not declare it, parameter scoping being single-level. The evidence gate
  matters as much as the trigger — a same name on an unrelated sheet is coincidence, and
  parameter-free projects never see the offer at all. It is a button rather than a popup so the user meets it when they
  look at the instance, and nothing has to guess when to interrupt them; it is hidden while a
  simulation is open, since accepting changes the design being simulated. Applying updates both
  stores of each binding (parent-sheet `CustomCompParam` slot and the instance's
  `ParameterBindings`), syncs open-sheet symbols via `ChangeCustom`, and writes modified sheets
  through to disk; the open sheet is only marked dirty so unrelated canvas edits are never
  silently committed.

### What an instance's ports are (`CanvasExtractor.fs`)

**A parameterised sheet has no single signature.** It has a family of them, one per set of
bindings, so the port widths of a custom component instance are a fact about the **instance**, not
about the sheet: two instances of one sheet are meant to differ.

`CanvasExtractor.signatureOfInstance` is the only place that works this out — the child sheet's
canvas resolved at the instance's bindings, with those bindings first evaluated in the sheet the
instance *sits on*, because an instance binding is an expression in the parent's parameters. Four
callers go through it, and they held three divergent copies before:

| caller | what it needs the signature for |
|---|---|
| `CatalogueView.placeCustomComponent` | sizing the ports of an instance being placed |
| `ParameterView.portWidthsOfInstance` | resizing them when a binding is edited |
| `CanvasStateAnalyser.checkCustomComponentForOkIOs` | checking an instance before simulation |
| `CustomCompPorts.getInstancesOfCurrentSheet` | bringing instances back into step |

`signatureOfInstanceWithCertainty` also reports whether the widths can be believed. A canvas is
checked without reference to whatever contains it, so `checkCustomComponentForOkIOs` has no parent
environment: a binding that is an expression in the parent's parameters cannot be evaluated there,
the signature comes back **inexact**, and only the port *names* are compared. The widths are left
to elaboration, which has the parent's bindings and is exact. Comparing them anyway would fail a
design that is perfectly correct.

### Keeping instances in step (`CustomCompPorts.fs`)

When a sheet's ports change, every instance of it elsewhere in the project must be updated. The
invariant is per instance:

> an instance is out of date exactly when it differs from what **its own** bindings give it

`getOutOfDateDependents` tests each instance against its `signatureOfInstance`, and
`updateInstance` brings each to its own signature. The dialog reports ports added, deleted and
renamed — facts about the sheet — and separately names the instances whose widths alone change.

The tempting simplification here — that every instance must equal the sheet — is wrong, and wrong
loudly: it raises "you have changed the inputs or outputs" on **every save** of any parameterised
design, and accepting that forces each instance to the sheet's *declared* widths while leaving its
bindings alone, which the simulator then rejects with `BadInputs`.

### Component Creation (`CatalogueView.fs`)

Integrates parameters during component instantiation:
- Raises `ParameterView.customComponentParamPopup` for a sheet that declares parameters, asking
  for a value for each — see [Design-Time Analysis](#design-time-analysis-parameteranalysisfs)
- Sizes the instance's ports with `signatureOfInstance` at the chosen bindings, before it is
  created
- Sets `ParameterBindings` on the instance, and `addParamComponents` records its slots once it
  has an id

## Data Flow

### 1. Parameter Definition Flow
```
User Input (Properties Panel): name, description, default value
    ↓
ParameterView.addParameterBox  (name checked by isValidParamName;
                                description compulsory)
    ↓
Update Model.LoadedComponent.LCParameterSlots.DefaultBindings
    ↓
bindParamOnInstances — every existing instance binds the new parameter
    ↓
markSheetParamsChanged — the canvas has not changed, so say so explicitly
    ↓
Persist to .dgm file on save
```

### 2. Component Parameterization Flow
```
User selects component property
    ↓
ParameterView.paramInputField
    ↓
ParameterTypes.parseExpression, then evaluateParamExpression
    ↓
evaluateConstraints (returns what is unmet; dispatches nothing)
    ↓
updateParamSlot — addSlot/removeSlot on ComponentSlotExpr,
                  and markSheetParamsChanged
    ↓
updateComponentSlots — apply to the component via Sheet messages,
                       all of that component's slots together
```

### 3. Simulation Resolution Flow
```
Simulation Start
    ↓
GraphMerger.mergeDependencies
    ↓
Stage 1: Merge graphs (defer parameters)
    ↓
Stage 2: resolveSheet — top sheet with defaults,
         each sheet below with its instance's bindings,
         memoised on (sheet, diff from defaults)
    ↓
FastSim with resolved values
```

### 4. Custom Component Flow
```
customComponentParamPopup — a value for each parameter the child sheet
                            declares, or "bind to the parent's <name>"
    ↓
CanvasExtractor.signatureOfInstance:
      instance bindings evaluated in the PARENT
    → merged over the child sheet's declared defaults
    → child canvas resolved at those values
    → ordered IO labels read off it
    ↓
Create the instance with those ports and those ParameterBindings
    ↓
addParamComponents records its slots once it has an id
```

## Expression Language

The parameter expression parser supports:

### Syntax Elements
- **Literals**: Decimal integer constants of any size (e.g., `8`, `32`, `1099511627776`).
  Hexadecimal is **not** supported
- **Variables**: Parameter names — a letter followed by letters and digits (e.g., `WIDTH`,
  `dataSize`, `W2X`), other than a built-in function name. See `isValidParamName` below
- **Operators** (with precedence, tightest first):
  - Unary minus: `-x`, being part of the operand it precedes
  - Multiplication, Division, Modulo: `*`, `/`, `%`
  - Addition, Subtraction: `+`, `-`
- **Functions**: `clog2(x)`, `min(x,y)`, `max(x,y)`. See below
- **Parentheses**: For grouping expressions

### Functions
Three built-ins, written as calls and so needing no precedence of their own:

| Written | Means |
|---|---|
| `clog2(x)` | bits needed to index `x` things: `ceil(log2 x)`. `clog2(8)` is 3 and `clog2(9)` is 4; 0 and 1 both give 0, as Verilog's `$clog2` does. A negative argument is an error |
| `min(x,y)` | the smaller of the two |
| `max(x,y)` | the larger of the two |

`clog2` is the one that makes a width follow a size: an address bus for an `N`-word memory, a
select input for an `N`-way mux, the shift amount for an `N`-bit shifter. `CommonTypes.shifterWidthFor`
computes the SHIFT input's width with the same function, so `clog2` written in a properties box
means exactly what Issie does internally. `min`/`max` are there because clamping is usually what
comes next: **`max(clog2(N),1)`** is the idiom, since a width must never be 0.

**Names are matched without regard to case**: `clog2`, `CLOG2` and `CLog2` are one function, as are
`min` and `MIN`. They are written back in lower case, so `MAX(1,2)` re-renders as `max(1,2)`.
Because the parser reads them as functions, they are reserved: a parameter may not be called
`clog2`, `min` or `max` in any case, and the "Add parameter" dialog says so.

### Example Expressions
```
WIDTH           // Simple parameter reference
WIDTH + 1       // Increment parameter
(n * 8) - 1     // Complex calculation
baseAddr + (offset * 4)  // Address calculation
WIDTH / 2       // Division
SIZE % 8        // Modulo operation
-1              // Negative literal
BIAS - -4       // Subtracting a negative
clog2(WORDS)    // Address bits for a memory of WORDS words
max(clog2(N),1) // ...clamped, since a width of 0 is not a width
min(WIDTH,32)   // Capping a width
```

### Parser Implementation
The parser uses recursive descent with separate functions for each precedence level:
- `parsePrimary`: Handles numbers, variables, function calls, unary minus, and parentheses
- `parseFactors`: Processes multiplication, division, modulo
- `parseExpressionTokens`: Handles addition and subtraction

A call is parsed in `parsePrimary` because it is atomic — its own parentheses delimit it — and its
arguments are whole expressions, `parseExpressionTokens` stopping at the `,` or `)` that ends each
one. The list of two-argument functions is derived from the `ParamBinFunc` DU by
`EEExtensions.Union.allCases`, so adding a case to that type reserves its name and reaches the
parser with no second edit; only `binFuncName` must then cover it, which the compiler requires.

**One name rule.** `ParameterTypes.isValidParamName` (`[a-zA-Z][a-zA-Z0-9]*`, and not a built-in
function name) is both what the "Add parameter" dialog accepts and what the tokenizer reads as a
name, because a name that cannot be written in an expression is of no use. Two rules diverging
breaks it in both directions: a name the dialog takes but the tokenizer will not read can be
declared and never referred to, and one the tokenizer reads but the dialog marks invalid is shown
in red and accepted anyway. That is also why a function name cannot be a parameter: the parser
reads `min` as the function, so a parameter of that name could never be referred to. A number run
directly into a name (`2W`) is reported as such, since it is either a missing `*` or a name from a
file written under a looser rule.

**Negation** is `PSubtract (PInt 0, e)`, not a new AST case: subtraction from zero is the same
expression, so every function over `ParamExpression` already handles it and no saved file changes.
A negated literal is folded to `PInt -n` so it renders back as the user typed it.

Notes and caveats:
- Tokenizer restricts inputs to digits/letters/operators/whitespace; unsupported characters are reported precisely.
- Division or modulo by zero is reported as an informative evaluation error, as is a parameter
  defined in terms of itself.

Code: `src/Renderer/Common/ParameterTypes.fs` (`parseExpression`, `isValidParamName`, tokenizer
regex, and helpers). `Tests/Issie.Tests/Properties.fs` holds a render/parse round-trip property
over generated expressions, negative literals included.

## Parameter Scoping & Precedence

### Scope Levels
1. **Sheet-level parameters**: declared in sheet properties, in scope for the slot expressions of
   components on that sheet, and for the bindings of instances placed on it
2. **Instance bindings**: what an instance supplies for the parameters of the sheet *inside* it.
   The expression is in the **parent's** parameters; the child sheet knows nothing of them

**Nothing is inherited.** Scoping is single-level and there is no implicit outward lookup: a
parameter of an enclosing sheet reaches a child only through an explicit binding written on each
instance along the way. That is a deliberate rejection of dynamic scoping by name, which brings
name capture, accidental unification of unrelated same-named parameters, and a "local" opt-out
marker to teach. The bind-to-top button exists to materialise such a chain on request.

### Precedence
An instance's binding wins over the child sheet's declared default, for every parameter the child
declares. That is the whole rule — `GraphMerger.effectiveBindings` and
`CanvasExtractor.effectiveInstanceBindings` are the two places that implement it, identically.

### Example Scenario
```
Sheet B declares:                 WIDTH = 16   (its default)
Sheet A declares:                 W = 8
Instance of B on A binds:         WIDTH = W * 4

Inside that instance, B resolves at WIDTH = 32.
Opened on its own, B still resolves at WIDTH = 16.
```

## Constraint System

Constraints keep a value within the range the field it is going into can hold:

### Constraint Definition
```fsharp
type ParamConstraint =
    | MinVal of ParamExpression * ParamError
    | MaxVal of ParamExpression * ParamError
```

The error text is author-written and is handed to the user unchanged — it should say what is wrong
with *this* field, not restate the bound.

### Derived from the slot, not stored on it

`ComponentSlots.constraintsFor : CompSlotName -> ComponentType -> ParamConstraint list` is the one
place that says what may go in a slot, and lives beside `trySetSlotValue`, which says where it goes.
Every box asks it rather than building a list of its own:

| slot | bounds |
|---|---|
| any width | `1 .. CommonTypes.Constants.maxIssieBusWidth` |
| `InputDefault`, and a `BusCompare` value | `0 .. 2^w - 1` at the component's **current** width |
| a `BusSelection` LSB | `>= 0` — a bit position, with no width to exceed |
| `CustomCompParam` | none *here*: see below |

Two things follow, and both used to be wrong. A bound computed from the component's width is
recomputed every time the pane is drawn, so widening or narrowing the component — which a property
can do without the box being touched — moves the bound with it; built inline at the box, it was
frozen at the width showing when the expression was typed, and an Input's *"must fit in 8 bits"*
outlived the 8. And a value arriving any other way is now bounded too: `maxIssieBusWidth` was
enforced only by those inline lists, so a width reached through an instance binding, or written by
the sheet-description DSL, had no upper limit at all.

The `Constraints` stored on a slot in the `.dgm` are still written as they always were, so files are
unchanged in both directions — but they are no longer what a value is checked against.

### An instance binding is checked against the sheet inside it

The bounds on a `CustomCompParam` value belong to the **child** sheet, and are expressions in the
*child's* parameters — which is why they cannot be handed to `paramInputField`, whose constraint
list is evaluated in the parameters of the sheet the instance sits on.
`ParameterView.instanceBindingProblem` therefore resolves the child sheet instead: it takes the
bindings that instance gives (`CanvasExtractor.effectiveInstanceBindings`), substitutes the
candidate value, and runs `evaluateConstraints` over the child's slots with their derived
constraints — the same call `editParameterBox` makes for the open sheet. Only the slots that *use*
the parameter are checked, so a complaint can never name a box the user is not editing.

## One evaluator, one slot mapping, one instance signature

There is **one** expression evaluator, `ParameterTypes.evaluateParamExpression`. What differs
between contexts is the environment it is given and what a failure means:

| context | environment | a failure is |
|---|---|---|
| properties pane | the open sheet's declared bindings | a message under the input box |
| simulation (`GraphMerger.resolveSheet`) | each sheet's effective bindings | a `SimulationError` naming the component and sheet |
| design-time analysis (`ParameterAnalysis`) | the values computed down the instance tree | an *unknown* value, never reported as a conflict |

Two other mappings are likewise single:

- **`ComponentSlots.trySetSlotValue`** — the only mapping from a `CompSlotName` to a field of a
  `ComponentType`.
- **`CanvasExtractor.signatureOfInstance`** — the only calculation of a custom component
  instance's ports.

Keep it that way. Each of these has been three or four copies at some point, and each time the
copies drifted apart and the drift was a bug: a slot applied by the canvas and ignored by the
simulator, an instance placed at the child's default widths, a renamed port silently losing its
parameterised width.

## Persistence

Parameter data is stored across multiple locations:

### File Storage
- **`.dgm` files**: Sheet parameter definitions and slot bindings
  - Stored in `LCParameterSlots` field of LoadedComponent
  - JSON serialization of ParameterDefs type
  - `CanvasExtractor.tidyParamSlots` runs on every save path: it drops slots naming components
    that have gone, and repoints the label an `IO` slot carries at its component's current label

### Runtime State
- **Model.LoadedComponent**: Current parameter values
- **Component.Type**: Resolved parameter values in components
- **CustomComponentType.ParameterBindings**: Instance overrides

Nothing else is kept. Where one design reaches a sheet by paths that bind it differently, the values
it is *not* drawn at are not stored anywhere: the properties pane asks `displayValues` for them as
it draws, which is the same question and one fewer copy of the answer to keep in step.

### Knowing when to save

`UpdateHelpers.currentSheetIsOutOfDate` decides whether the open sheet needs saving by comparing
its canvas against the saved one, plus the `LoadedComponentIsOutOfDate` flag. A change confined to
`LCParameterSlots` — a parameter declared, a description written, an unused one deleted, or a slot
given an expression that works out to the width already shown — leaves the canvas *identical*, so
it is invisible to that comparison. Every path that edits parameter data therefore sets the flag,
through `ParameterView.markSheetParamsChanged`. A new such path that forgets to call it leaves the
save button dark and the work is dropped on leaving the sheet, with nothing to say so.

### Bringing every sheet into line with what its design sets

A sheet is not drawn at its declared values and then adjusted for display: the values its design
settles are **written into it**, canvas and declarations alike. `PropagateParameters` is the message
that does it, and sending it twice or in the wrong order is harmless, because what it triggers is a
pure recomputation from the primary state rather than an incremental edit — each design's top-sheet
values, and the bindings on the instances below. That is what makes it safe to send after anything,
and it is sent after everything: a project load, a top-sheet choice, an edit to what a sheet
declares, and any draw-block message that changed an instance's bindings (`Update.fs`).

Three steps, and they are separate because no one of them can do the others' work:

1. **`ParameterAnalysis.propagateParameterValues`** — pure, over the `LoadedComponent`s. For every
   sheet it works out what its design sets each parameter to, writes the settled value into
   `DefaultBindings`, and rewrites the sheet's canvas at those values with
   `ComponentSlots.resolveCanvasAtBindings`. A parameter *nothing* sets is left exactly as it is:
   its stored value **is** the primary state for that sheet, and overwriting it would destroy the
   only copy. The instance tree is walked once per candidate top and the answers reused; asking
   `effectiveTopSheetFor` and then `displayValues` per sheet, as this did, cost
   `sheets × (roots + 1)` walks on every edit.
2. **`CanvasExtractor.syncInstancePorts`** — also pure, and separate because of what step 1 cannot
   reach. Resolving a `CustomCompParam` slot writes the value into the instance's
   `ParameterBindings`, which is as far as `ComponentSlots.setSlotValue` can see; the instance's
   **port widths** follow from that binding by way of the *child* sheet, and only
   `signatureOfInstance` knows how. Without this step a sheet came out of step 1 holding an instance
   whose bindings said one width and whose ports still said another — invisible on the open sheet,
   which is redrawn anyway, and written straight to file on every other, so that opening it raised
   the very instance-out-of-date error the per-instance signature exists to prevent. Widths only:
   the order of an instance's ports, and which ports it has, are left alone (see
   [Keeping instances in step](#keeping-instances-in-step-customcompportsfs)).
3. **`ParameterView.propagateParameters`** — the part that touches the world. A **closed** sheet
   whose values changed is written to its file at once, because only the open sheet is ever allowed
   to be unsaved; a sheet that cannot be written (a library component, which belongs to its library)
   is still brought into line in memory but its file is left alone. The **open** sheet's canvas is
   not in `LoadedComponents` but in the draw block, so its slots are pushed through the same
   symbol-change path the properties pane uses — symbol size, ports and geometry are recomputed
   rather than patched, and the change joins that sheet's undo history like any other edit.

Steps 1 and 2 are both idempotent, which is what the whole arrangement rests on: undo need only
restore the primary state and run it again, and no edit has to reason about which sheets a binding
might reach.

## Component Support

### Currently Parameterizable Components

#### Width-Configurable Components
- Registers (`Register`, `RegisterE`)
- Adders (`NbitsAdder`, `NbitsAdderNoCin`, etc.)
- Logic gates over a bus (`NbitsAnd`, `NbitsOr`, `NbitsNot`, `NbitsXor`), and `NbitSpreader`
- Bus components (`BusCompare`, `BusCompare1`, `BusSelection`, `SplitWire`, `SplitN`)
- Counters (all variants), `Shift`, `Viewer`

Multiplexers and demultiplexers are **not** parameterisable: they have no case in
`ComponentSlots.trySetSlotValue`, so `slotApplies` refuses a slot on one and the properties pane
offers none. `GateN` and `MergeN` are excluded deliberately, their integer being an input count.
`trySetSlotValue` is the list that decides this, and is worth reading rather than trusting this
one.

#### Custom Components
- All parameters from component definition
- Dynamic port width calculation
- Hierarchical parameter passing

#### I/O Components
- Input port widths (`Input`, `Input1`)
- Output port widths (`Output`)

#### Constants
- Bit width specification (`Constant`, `Constant1`)

### Adding Parameter Support to New Components

1. Update the component type in `CommonTypes.fs` if the field is not there yet
2. Add a case to `ComponentSlots.trySetSlotValue`, which is the only place that knows how a
   `CompSlotName` maps onto a field of a `ComponentType`. The properties pane, elaboration and the
   sheet-description DSL all go through it, and `slotApplies` — which decides whether a slot may be
   written at all — is derived from the same match, so there is nothing else to keep in step
3. Add a case to `ParameterView.updateComponentSlots` saying which sheet message writes the field
   on the canvas
4. Add UI support in the component's properties, via `ParameterView.paramInputField`

## Usage Examples

### Example 1: Define Sheet Parameter
```fsharp
// User adds parameter "WIDTH" with value 8
1. Open sheet properties panel
2. Click "Add Parameter"
3. Enter name: "WIDTH"          // a letter, then letters and digits
4. Enter description: "width of the data bus in bits"   // compulsory
5. Enter value: 8               // used when this sheet is simulated on its own
6. Parameter available for use in expressions
```

The description is compulsory because it is what an instance of this sheet shows the user when it
asks them for a value — the one place the parameter has to be understood.

### Example 2: Use Parameter in Component
```fsharp
// Configure Register with parameterized width
1. Add Register component to sheet
2. Select Register
3. In properties, enter bus width: "WIDTH"
4. System evaluates to 8
5. Change WIDTH parameter → Register updates automatically
```

### Example 3: Override in Custom Component
```fsharp
// Custom component with parameter override
1. Create custom component from sheet with WIDTH parameter
2. Place instance in parent sheet
3. Select instance
4. Edit parameter binding: WIDTH = "parentWidth * 2"
5. Instance uses calculated value
```

### Example 4: Complex Expression
```fsharp
// Address decoder with calculated ranges
1. Define parameters: baseAddr = 4096, blockSize = 256
   (decimal only — the expression language has no hex, and no underscore in a name)
2. Create comparator with expression: "baseAddr + (blockSize * 4)"
3. System evaluates to 5120
```

## Error Handling

The system provides comprehensive error handling at multiple levels:

The user-facing word for a sheet parameter is **property**, and every message below says so; only
the code calls them parameters. Quotes here are the messages themselves — `ParameterTypes` is where
they are written.

### Parse Errors
- **Invalid syntax**: "Contains unsupported characters: [']'"
- **Empty input**: "Type a number or expression" (`ParameterTypes.emptyInputError`). It says what to
  do rather than what is wrong: an empty box is not a mistake, it is how the user asks to be offered
  a property to follow
- **Unmatched parentheses**: "Mismatched parentheses"
- **A number run into a name**: "'2W' is neither a number nor a property name: a property name
  must start with a letter, and a multiplication must be written out as 2*W"

### Evaluation Errors
An unresolved name is nearly always one of two mistakes, and they need different advice, so the
message names the alternatives rather than only the failure:
- **Undefined property, where the sheet declares some**: "Property 'WITDH' is not defined.
  Properties of this sheet: DEPTH, WIDTH"
- **Undefined property, where the sheet declares none**: "This value must be numeric: to use a
  property this must first be added to the sheet"
- **Self-reference**: "Property 'W' is defined in terms of itself: W which uses W"
- **Division or remainder by zero**: "Division by zero: 4 cannot be divided by 0"

### Constraint Violations
- **Value too small / too large**: the author's message from the `MinVal` / `MaxVal` that the value
  failed, derived from the slot by `ComponentSlots.constraintsFor`
- **Limit not evaluable**: the author's message plus "- but that limit could not be worked out",
  because a bound that cannot be evaluated must not pass the value it guards
- **An instance binding out of range**: the message belonging to the slot of the **child** sheet
  that the value would break — see `ParameterView.instanceBindingProblem`

Only the first failure is shown. One bad value usually breaks the same bound on several components,
and a column of repeated sentences reads as noise.

## Implementation Details

### Optics/Lenses Pattern
The system uses functional lenses for immutable state updates:
```fsharp
let paramSlotsOfModel_ = 
    lcParameterInfoOfModel_ >?> paramSlots_

model |> set paramSlotsOfModel_ newSlots
```

### Message Dispatch
State changes flow through Elmish messages:
```fsharp
Sheet (SheetT.Wire (BusWireT.Symbol (SymbolT.ChangeWidth ...)))
```

### Functional Patterns
- Immutable data structures throughout
- Pattern matching for control flow
- Option/Result types for error handling
- Pipeline operators for composition

## Testing & Debugging

### Tests

`npm run test` reaches the whole parameter system under plain .NET — no Electron, no browser. The
groups, runnable individually with `--filter Issie.<name>`:

| group | covers |
|---|---|
| `InstanceSignatures` | what an instance's ports are, and keeping instances in step with the sheet inside them |
| `ParameterScenarios` | parameterised sheets instantiated at different bindings and simulated end to end |
| `ParameterUI` | the two gates deciding how much of the feature the UI shows, and binding totality |
| `Properties` | the expression language against a reference evaluator and through render/parse |

### Debug Helpers

Nothing prints unconditionally. `Common/Log.fs` is the only route to the console: `Log.warn` and
`Log.error` always show, and category logging shows only when its category is switched on — from
Development > Log, from `--log=sim` at launch, or from `window.issieLog.on "sim"` in a console.

```fsharp
Log.dbg Log.Sim $"Parameter evaluation: %A{expr} -> %A{value}"
```

A new `printf` outside a short allowlist fails `Tests/Issie.Tests/SourceHygiene.fs`.

### Common Issues
1. **Forward references**: Resolved by merging all graphs before resolving parameters
2. **Circular dependencies**: Detected and reported
3. **Constraint conflicts**: Validated before application
4. **Type mismatches**: Caught by F# type system

## API Reference

### Key Functions

#### Expression Parsing
```fsharp
parseExpression: string -> Result<ParamExpression, ParamError>
```

#### Expression Evaluation
```fsharp
evaluateParamExpression: ParamBindings -> ParamExpression -> Result<ParamInt, ParamError>
```

#### Expression Rendering
```fsharp
renderParamExpression: ParamExpression -> int -> string
```

#### Name validity (the parser's rule and the dialog's)
```fsharp
isValidParamName: string -> bool
```

#### Slot identity (the `IO` label is not part of it)
```fsharp
sameSlot:     ParamSlot -> ParamSlot -> bool
tryFindSlot:  ParamSlot -> ComponentSlotExpr -> ConstrainedExpr option
addSlot:      ParamSlot -> ConstrainedExpr -> ComponentSlotExpr -> ComponentSlotExpr
removeSlot:   ParamSlot -> ComponentSlotExpr -> ComponentSlotExpr
```

#### An instance's ports (`CanvasExtractor`)
```fsharp
signatureOfInstance:
    LoadedComponent list -> ParamBindings -> string -> ParamBindings -> Signature option
signatureOfInstanceWithCertainty:
    LoadedComponent list -> ParamBindings -> string -> ParamBindings -> (Signature * bool) option
```
The arguments are: the project's sheets, the bindings of the sheet the instance *sits on*, the
child sheet's name, and the instance's bindings.

#### Sizing every instance in a project at its own bindings (`CanvasExtractor`)
```fsharp
withPortWidths:    Map<string,int> -> CustomComponentType -> CustomComponentType
syncInstancePorts: LoadedComponent list -> LoadedComponent list
```
Widths only, matched by port label: the order of an instance's ports and which ports it has are
left alone. Run after `ParameterAnalysis.propagateParameterValues`, which cannot reach them.

#### Slot resolution (`ComponentSlots`)
```fsharp
trySetSlotValue: CompSlotName -> ParamInt -> ComponentType -> ComponentType option
setSlotValue:    CompSlotName -> ParamInt -> ComponentType -> ComponentType
slotApplies:     CompSlotName -> ComponentType -> bool
constraintsFor:  CompSlotName -> ComponentType -> ParamConstraint list
```

#### Constraint Checking
```fsharp
evaluateConstraints:    ParamBindings -> ConstrainedExpr list -> Result<Unit, ParamConstraint list>
instanceBindingProblem: LoadedComponent list -> string -> ParamBindings -> ParamName -> ParamInt
                            -> Result<unit, ParamError>
```

## Resolution Mechanics Deep-Dive

- UI evaluation: `ParameterTypes.evaluateParamExpression` performs recursive substitution and constant-folding with detailed errors. Used by `ParameterView` for validation and preview.
- Graph evaluation: `GraphMerger.resolveParametersInSimulationGraph` walks the sheet tree with `resolveSheet`, evaluating each slot with `evaluateParamExpression` and writing concrete values into `SimulationGraph` component types with `ComponentSlots.setSlotValue`; any failure is a `SimulationError`.
- Validation: `CanvasStateAnalyser.checkCustomComponentForOkIOs` asks `CanvasExtractor.signatureOfInstanceWithCertainty` what the instance's ports should be, and compares names only when the answer is inexact.
- Slot access: `ComponentSlots.trySetSlotValue : CompSlotName -> ParamInt -> ComponentType -> ComponentType option` is the single mapping from a slot to a field of `Component.Type`, returning `None` where the component has no such slot — and also where the value is too large to be the `int` that field holds, which is the one place a parameter value stops being a bigint. `setSlotValue` is the total version used by the paths that must not fail on an old file; `slotApplies` is the predicate used to refuse a bad slot where it is written; `constraintsFor` says what may go in the slot, and is kept beside the mapping that says where it goes.
- Instance ports: `CanvasExtractor.signatureOfInstance` resolves the child sheet's canvas at the instance's effective bindings and reads off the ordered IO labels. `effectiveInstanceBindings` is the same merge `GraphMerger.effectiveBindings` makes for elaboration, so what is drawn and what is simulated agree.

## Developer Notes (Files & Responsibilities)

- `src/Renderer/Common/ParameterTypes.fs`: Types (`ParamExpression`, `ParamConstraint`, `ParamSlot`, `ParameterDefs`), parser (`parseExpression`) and its name rule (`isValidParamName`), evaluator (`evaluateParamExpression`), renderer (`renderParamExpression`), slot identity (`sameSlot`, `tryFindSlot`, `addSlot`, `removeSlot`), and `bindingsOf`, which every evaluation environment is derived through.
- `src/Renderer/Simulator/CanvasExtractor.fs`: what a custom component instance's ports are (`signatureOfInstance`, `signatureOfInstanceWithCertainty`, `effectiveInstanceBindings`, `resolveCanvasAtBindings`), sizing every instance in a project at its own bindings (`syncInstancePorts`, `withPortWidths`), and `tidyParamSlots`, which puts a sheet's slots in order against its canvas on every save.
- `src/Renderer/UI/CustomCompPorts.fs`: keeping instances in step with the sheet inside them - `getOutOfDateDependents` (per instance, against its own bindings), `updateInstance`, and the confirmation dialog.
- `src/Renderer/Common/ComponentSlots.fs`: the one mapping from a `CompSlotName` to a field of a `ComponentType` (`trySetSlotValue`), the bounds that field imposes (`constraintsFor`), and resolving a whole canvas at a set of bindings (`resolveCanvasAtBindings`). Used by the properties pane, by elaboration and by the sheet-description DSL.
- `src/Renderer/Common/SheetDescription.fs`, `src/Renderer/DrawBlock/SheetLayout.fs`: sheets written as data - components, logical connections, parameters and slots - laid out and saved without Issie running. See [dev/sheetDescriptionDsl.md](dev/sheetDescriptionDsl.md).
- `src/Renderer/Common/ParameterAnalysis.fs`: Design-time instance-tree analysis under a top sheet (`analyseUnderTop`, `displayValues`), top-sheet inference (`effectiveTopSheetFor`, `instanceForestRoots`), bringing every sheet into line with what its design sets (`propagateParameterValues`), and bind-to-top chain computation (`findBindOffers`).
- `src/Renderer/UI/ParameterView.fs`: Sheet defaults and slot bindings CRUD, constraint checking (`evaluateConstraints`, `instanceBindingProblem`), component updates, parameter UI fields/popups, the propagation that touches the world (`propagateParameters`), the placement popup (`customComponentParamPopup`), the bind-to-top button action (`applyBindOffers`), and the top-choice popup (`topSheetChoiceCheck`).
- `src/Renderer/UI/CatalogueView.fs`: Raises the placement popup, sizes an instance's ports with `signatureOfInstance` at the chosen bindings, sets `ParameterBindings` on it.
- `src/Renderer/Simulator/GraphMerger.fs`: Two-stage resolution during merge; graphs merged first, then one recursive `resolveSheet` walk that applies each sheet's slots and descends with each instance's bindings, memoised on the diff from defaults.
- `src/Renderer/Simulator/CanvasStateAnalyser.fs`: Checks each custom component instance's ports against `signatureOfInstanceWithCertainty`, comparing names only where the widths cannot be known without the parent sheet.

## Known Limitations

- Parameter values are whole numbers only: there are no fractions and no strings.
- Parameter names are unqualified, and **scoping is single-level**: an instance binding is an
  expression in the parameters of the sheet the instance sits on, and nothing further out is in
  scope. Following a design-wide constant down a hierarchy means a parameter on every sheet in
  between, which is what the bind-to-top button materialises.
- A sheet reached by two paths that bind it differently is written at the **largest** of the values,
  with the others shown beside it in the properties pane. That is a choice made so that the value
  is definite and the recomputation idempotent, not a claim that the largest is the right one to
  look at; the strong answer is the last limitation below.
- `signatureOfInstanceWithCertainty` cannot evaluate a binding that is an expression in the parent
  sheet's parameters when it is asked about a canvas on its own, so
  `checkCustomComponentForOkIOs` compares port names but not widths in that case. This is
  deliberate — the alternative is failing correct designs — but it means a genuine width error of
  that shape is caught at simulation rather than at load.
- A pasted non-custom component whose width was parameterised freezes at its resolved value when
  pasted onto another sheet, and nothing reports it: neither `Model.Clipboard` nor
  `SymbolT.Model.CopiedSymbols` records which sheet the copy came from. A custom component instance
  that loses bindings the same way *does* warn.
- **Memories are not parameterisable**: RAM and ROM address and word widths are plain numbers, not
  parameter slots. An input count is likewise not a slot — the number of inputs of a gate or a
  merge sets how many ports a symbol has, so a computed value would make `SymbolInfo.PortOrder`
  name ports the saved type does not have.
- A sheet can be viewed only at one set of values. Opening it *as a particular instance*
  (`CPU_TOP > FetchUnit > Adder(W=16)`) is the strong answer for a multi-valued sheet and does not
  exist.

Smaller rough edges are in [dev/openIssues.md](dev/openIssues.md).

## Best Practices

1. **Use descriptive parameter names**: `dataWidth` instead of `W`. Names are letters and digits
   only — there is no underscore, so `DATA_WIDTH` is not a name Issie will accept — and may not be
   `clog2`, `min` or `max`, which are functions
2. **Write the description for the person choosing the value**, not for yourself: it is what an
   instance of the sheet shows where the value is entered
3. **Define constraints early**: Prevent invalid values at input time
4. **Test edge cases**: Min/max values, zero, negative numbers
5. **Keep expressions simple**: Complex logic in simulation, not parameters
6. **Use consistent naming**: Across sheets and components. A same-named parameter on an ancestor
   sheet is what the bind-to-top button looks for
7. **Validate before simulation**: Check all parameters resolve correctly

## Troubleshooting

### Parameter Not Found
- Check spelling of parameter name
- Verify parameter is defined in current scope
- Ensure proper capitalization (case-sensitive)

### Expression Parse Error
- Check for typos in operators
- Verify parentheses are balanced
- Use only supported operators (+, -, *, /, %)
- A built-in function must be written as a call: `clog2(x)`, `min(x,y)`, `max(x,y)`. A comma
  belongs only between the two arguments of `min` or `max`

### Constraint Violation
- Review constraint definitions
- Check calculated values against limits
- Adjust parameter values or constraints

### Simulation Failure
- Verify all parameters resolve to valid integers
- Check for circular parameter dependencies
- Ensure component types match parameter slots
