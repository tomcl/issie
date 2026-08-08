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
- **Expression Language**: Use arithmetic expressions (`WIDTH + 1`, `SIZE * 8`) to calculate values
- **Hierarchical Scoping**: Parameters defined at sheet level, overridable per component instance
- **Constraint System**: Define min/max constraints on parameter values with custom error messages
- **Custom Component Support**: Pass parameters between sheets through custom components
- **Real-time Evaluation**: Expressions evaluated dynamically as parameters change

## Architecture

The parameter system is implemented across several key modules with clear separation of concerns:

### Core Data Types (`ParameterTypes.fs`)

The parameter system's foundation is built on these core types:

#### Basic Types
- **`ParamInt`**: Currently `int`, will be extended to `bigint` for large bit constants
- **`ParamName`**: Encapsulated string representing a parameter identifier

#### Expression AST
```fsharp
type ParamExpression =
    | PInt of ParamInt                              // Integer constant
    | PParameter of ParamName                       // Parameter reference
    | PAdd of ParamExpression * ParamExpression     // Addition
    | PSubtract of ParamExpression * ParamExpression // Subtraction
    | PMultiply of ParamExpression * ParamExpression // Multiplication
    | PDivide of ParamExpression * ParamExpression  // Division
    | PRemainder of ParamExpression * ParamExpression // Modulo
```

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
  constraints that are not met are returned, not dispatched. It used to send a notification from
  inside a `List.filter`, and `editParameterBox`'s `isDisabled` calls it *while rendering* — so an
  unevaluable bound dispatched from a render, re-rendered, and dispatched again
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

Implements the analysis-plus-UI layer of [parameterSystemPlan.md](parameterSystemPlan.md).
Elaboration semantics are untouched: only explicit per-instance bindings exist, and everything
here is derived display state and consented repair.

- **`analyseUnderTop`** walks the instance tree under a top sheet over `LoadedComponent`s (the
  same binding walk `GraphMerger.resolveSheet` performs, without building graphs), recording for
  every sheet each instance's path and evaluated parameter values. Memoised on
  `(sheet, values)`; unevaluable bindings become *unknown* values, which are never reported as
  conflicts (design-time checking must not give false positives).
- **`displayValues`** turns this into per-parameter display rules: a singleton value set shows the
  real value with **nothing said about a default**, an empty set shows the declared value (which,
  the sheet having no instances, simply *is* the value and is not called a default either), and a
  multi-valued set shows the default with a note enumerating the values and example instance paths
  — *"8 at TOP > FetchAdder, 16 at TOP > ALU; showing default 8"*. Rendered in the sheet
  properties parameter table (`ParameterView.makeParamsField`). Naming the default in the first
  two cases only asked the user to reason about a number that is nearly always overwritten.
- **The top sheet** is per-project view state: an `IsTopSheet` flag on the chosen sheet,
  persisted in its `.dgm` `SheetInfo` and mirrored in `LoadedComponent`. `effectiveTopSheet`
  uses the flagged sheet, else infers a single instance-forest root silently. "Set as top" is on
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
  saved before it was required, or one edited by hand.
- **Bind-to-top button** (`findBindOffers` / `ParameterView.applyBindOffers`): where an instance's
  parameter is bound to a **plain number** and a same-named parameter exists on an ancestor sheet
  along the instance path under the top (the evidence gate), a button in that instance's
  properties materialises the chain - ordinary parameters and explicit `PParameter` bindings along
  every instance path from that ancestor down. (The trigger was an *unbound* parameter until
  binding became total, which made the offer unable to fire at all. A literal is the right trigger
  in its own right: the offer exists to help follow an outer parameter of the same name, and
  typing that name in by hand fails whenever a sheet in between does not declare it, parameter
  scoping being single-level.) It is a button rather than a popup so the user meets it when they
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

This module previously assumed the opposite: that every instance must equal the sheet. On any
parameterised design that raised "you have changed the inputs or outputs" on **every save**, and
accepting it forced each instance to the sheet's *declared* widths while leaving its bindings
alone — a design the simulator then rejects with `BadInputs`.

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
- **Literals**: Decimal integer constants (e.g., `8`, `32`). Hexadecimal is **not** supported
- **Variables**: Parameter names — a letter followed by letters and digits (e.g., `WIDTH`,
  `dataSize`, `W2X`). See `isValidParamName` below
- **Operators** (with precedence, tightest first):
  - Unary minus: `-x`, being part of the operand it precedes
  - Multiplication, Division, Modulo: `*`, `/`, `%`
  - Addition, Subtraction: `+`, `-`
- **Parentheses**: For grouping expressions

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
```

### Parser Implementation
The parser uses recursive descent with separate functions for each precedence level:
- `parsePrimary`: Handles numbers, variables, unary minus, and parentheses
- `parseFactors`: Processes multiplication, division, modulo
- `parseExpressionTokens`: Handles addition and subtraction

**One name rule.** `ParameterTypes.isValidParamName` (`[a-zA-Z][a-zA-Z0-9]*`) is both what the
"Add parameter" dialog accepts and what the tokenizer reads as a name, because a name that cannot
be written in an expression is of no use. They used to differ — names were accepted as
`[a-zA-Z0-9]+` while the tokenizer read letters-then-digits — so `W2X` could be declared and then
never referred to, and a name beginning with a digit was shown in red and accepted anyway. A
number run directly into a name (`2W`) is reported as such, since it is either a missing `*` or a
name from before the rule.

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

Constraints ensure parameter values remain within valid ranges:

### Constraint Definition
```fsharp
type ParamConstraint =
    | MinVal of ParamExpression * ParamError
    | MaxVal of ParamExpression * ParamError
```

### Features
- Evaluated during input validation
- Checked before component updates
- Display custom error messages when violated
- Support expressions in constraint definitions

### Example
```fsharp
let widthConstraints = [
    MinVal (PInt 1, "Width must be at least 1 bit")
    MaxVal (PInt 64, "Width cannot exceed 64 bits")
]
```

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

*This section used to describe a "three-tier evaluation architecture", the third tier being a
minimal `PInt`/`PParameter`-only evaluator inside `CanvasStateAnalyser`. It was not an
optimisation but a fourth copy of the resolution logic, and it had drifted: it required an `IO`
slot's stored label to still match its component's label, so a renamed port silently lost its
parameterised width. It is gone; `checkCustomComponentForOkIOs` calls
`signatureOfInstanceWithCertainty` like everything else.*

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
- **SymbolT.Symbol.DeclaredSlots / DeclaredPortLabels**: what a symbol is displaying differently
  from what it will be saved as — see below

### Knowing when to save

`UpdateHelpers.currentSheetIsOutOfDate` decides whether the open sheet needs saving by comparing
its canvas against the saved one, plus the `LoadedComponentIsOutOfDate` flag. A change confined to
`LCParameterSlots` — a parameter declared, a description written, an unused one deleted, or a slot
given an expression that works out to the width already shown — leaves the canvas *identical*, so
it is invisible to that comparison. Every path that edits parameter data therefore sets the flag,
through `ParameterView.markSheetParamsChanged`. Without it the save button stayed dark, switching
sheets did not save, and the work was silently dropped.

### Drawing at computed values, and saving what was declared

The open sheet is drawn at the values its parameters take under the current top sheet
(`ParameterView.applyComputedDisplayValues`), but what is written to the `.dgm` is unaffected.
`SymbolUpdate.extractComponent` is the sole path from symbols to saved state, and
`declaredComponent` puts back:

- **`DeclaredSlots`** — the declared value of each parameterised slot the symbol is displaying
  differently. Slot values rather than a whole declared component, so that an edit made to any
  *other* field while computed values were on display — a constant's value, a memory's contents,
  the label — is saved as it stands.
- **`DeclaredPortLabels`** — the declared ports of a custom component instance. This one cannot be
  derived from the slot value: a `CustomCompParam` slot binds a parameter of the sheet *inside*
  the instance, and the port widths follow from that binding by way of the child sheet, which
  `ComponentSlots.setSlotValue` cannot reach. Without it a sheet saved while showing computed
  values wrote an instance whose ports contradicted its own bindings — exactly what
  `checkCustomComponentForOkIOs` rejects.

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

### Parse Errors
- **Invalid syntax**: "Contains unsupported characters: [']'"
- **Empty input**: "Input Empty"
- **Unmatched parentheses**: "Mismatched parentheses"
- **A number run into a name**: "'2W' is neither a number nor a parameter name: a parameter name
  must start with a letter, and a multiplication must be written out as 2*W"

### Evaluation Errors
An unresolved name is nearly always one of two mistakes, and they need different advice, so the
message names the alternatives rather than only the failure:
- **Undefined parameter, where the sheet declares some**: "Parameter 'WITDH' is not defined.
  Parameters of this sheet: DEPTH, WIDTH"
- **Undefined parameter, where the sheet declares none**: "This value must be numeric: to use a
  parameter this must first be added to the sheet"
- **Self-reference**: "Parameter 'W' is defined in terms of itself: W which uses W"
- **Division or remainder by zero**: "Division by zero: 4 cannot be divided by 0"

### Constraint Violations
- **Value too small**: Custom message from MinVal constraint
- **Value too large**: Custom message from MaxVal constraint
- **Limit not evaluable**: the author's message plus "- but that limit could not be worked out",
  because a bound that cannot be evaluated must not pass the value it guards

### Type Errors
- **Invalid component**: "Invalid component [Type] for buswidth"
- **Wrong slot type**: "CustomCompParam can only be used with Custom components"

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

`JSHelpers.debugTraceUI` is a `string Set` of enabled trace codes, and `traceIf` prints when one
is present:

```fsharp
JSHelpers.traceIf "params" (fun () -> $"Parameter evaluation: %A{expr} -> %A{value}")
```

### Common Issues
1. **Forward references**: Resolved by merging all graphs before resolving parameters
2. **Circular dependencies**: Detected and reported
3. **Constraint conflicts**: Validated before application
4. **Type mismatches**: Caught by F# type system

## Future Extensions

Potential enhancements identified in the codebase:

### Type System
- **BigInt support**: For constants > 32 bits
- **Float parameters**: For analog simulations
- **String parameters**: For labels and identifiers

### Advanced Features
- **Parameter inheritance**: Across sheet hierarchy
- **Complex constraints**: Relationships between parameters
- **Expression optimization**: Caching and simplification
- **Parameter templates**: Reusable parameter sets

### UI Improvements
- **Visual expression builder**: Drag-drop interface
- **Parameter preview**: Real-time evaluation display
- **Batch parameter updates**: Apply to multiple components
- **Parameter search**: Find usage across project

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

#### Slot resolution (`ComponentSlots`)
```fsharp
trySetSlotValue: CompSlotName -> int -> ComponentType -> ComponentType option
setSlotValue:    CompSlotName -> int -> ComponentType -> ComponentType
slotApplies:     CompSlotName -> ComponentType -> bool
```

#### Constraint Checking
```fsharp
evaluateConstraints: ParamBindings -> ConstrainedExpr list -> Result<Unit, ParamConstraint list>
```

## Resolution Mechanics Deep-Dive

- UI evaluation: `ParameterTypes.evaluateParamExpression` performs recursive substitution and constant-folding with detailed errors. Used by `ParameterView` for validation and preview.
- Graph evaluation: `GraphMerger.resolveParametersInSimulationGraph` walks the sheet tree with `resolveSheet`, evaluating each slot with `evaluateParamExpression` and writing concrete values into `SimulationGraph` component types with `ComponentSlots.setSlotValue`; any failure is a `SimulationError`.
- Validation: `CanvasStateAnalyser.checkCustomComponentForOkIOs` asks `CanvasExtractor.signatureOfInstanceWithCertainty` what the instance's ports should be, and compares names only when the answer is inexact.
- Slot access: `ComponentSlots.trySetSlotValue : CompSlotName -> int -> ComponentType -> ComponentType option` is the single mapping from a slot to a field of `Component.Type`, returning `None` where the component has no such slot. `setSlotValue` is the total version used by the paths that must not fail on an old file; `slotApplies` is the predicate used to refuse a bad slot where it is written.
- Instance ports: `CanvasExtractor.signatureOfInstance` resolves the child sheet's canvas at the instance's effective bindings and reads off the ordered IO labels. `effectiveInstanceBindings` is the same merge `GraphMerger.effectiveBindings` makes for elaboration, so what is drawn and what is simulated agree.

## Developer Notes (Files & Responsibilities)

- `src/Renderer/Common/ParameterTypes.fs`: Types (`ParamExpression`, `ParamConstraint`, `ParamSlot`, `ParameterDefs`), parser (`parseExpression`) and its name rule (`isValidParamName`), evaluator (`evaluateParamExpression`), renderer (`renderParamExpression`), slot identity (`sameSlot`, `tryFindSlot`, `addSlot`, `removeSlot`), and `bindingsOf`, which every evaluation environment is derived through.
- `src/Renderer/Simulator/CanvasExtractor.fs`: what a custom component instance's ports are (`signatureOfInstance`, `signatureOfInstanceWithCertainty`, `effectiveInstanceBindings`, `resolveCanvasAtBindings`), and `tidyParamSlots`, which puts a sheet's slots in order against its canvas on every save.
- `src/Renderer/UI/CustomCompPorts.fs`: keeping instances in step with the sheet inside them - `getOutOfDateDependents` (per instance, against its own bindings), `updateInstance`, and the confirmation dialog.
- `src/Renderer/Common/ComponentSlots.fs`: the one mapping from a `CompSlotName` to a field of a `ComponentType`, used by the properties pane, by elaboration and by the sheet-description DSL. Three copies of this mapping used to exist and had drifted apart.
- `src/Renderer/Common/SheetDescription.fs`, `src/Renderer/DrawBlock/SheetLayout.fs`: sheets written as data - components, logical connections, parameters and slots - laid out and saved without Issie running. See [dev/sheetDescriptionDsl.md](dev/sheetDescriptionDsl.md).
- `src/Renderer/Common/ParameterAnalysis.fs`: Design-time instance-tree analysis under a top sheet (`analyseUnderTop`, `displayValues`), top-sheet inference (`effectiveTopSheet`, `instanceForestRoots`), and bind-to-top chain computation (`findBindOffers`).
- `src/Renderer/UI/ParameterView.fs`: Sheet defaults and slot bindings CRUD, constraint checking, component updates, parameter UI fields/popups, display-value annotations, the placement popup (`customComponentParamPopup`), the bind-to-top button action (`applyBindOffers`), and the top-choice popup (`topSheetChoiceCheck`).
- `src/Renderer/UI/CatalogueView.fs`: Raises the placement popup, sizes an instance's ports with `signatureOfInstance` at the chosen bindings, sets `ParameterBindings` on it.
- `src/Renderer/Simulator/GraphMerger.fs`: Two-stage resolution during merge; graphs merged first, then one recursive `resolveSheet` walk that applies each sheet's slots and descends with each instance's bindings, memoised on the diff from defaults.
- `src/Renderer/Simulator/CanvasStateAnalyser.fs`: Checks each custom component instance's ports against `signatureOfInstanceWithCertainty`, comparing names only where the widths cannot be known without the parent sheet.

## Early development history

The commits that first built the system. Later work — display values, the top sheet, component
libraries, and the instance-signature reconciliation described above — is in `git log` and is not
listed here.
- a67fa72f Fix parameter resolution in simulation graph creation
  - Passes `loadedDependencies` into merger; applies instance-specific `ParameterBindings` for custom components.
- 83bb0b0b Fix forward reference issue in parameter resolution
  - Introduces two-stage resolution: resolve custom component instance bindings first, then sheet-level defaults.
- b510fe4b Parameter System Redo
  - Reworks UI binding flow and simulation integration; clearer separation of concerns.
- edf61e87 Parameter System Support
  - Integrates `ParameterTypes.fs`, updates merger/validation, and adds comprehensive documentation.

## Known Limitations

- Integer-only parameters today (`ParamInt = int`); very large constants may require future `bigint`.
- Parameter names are unqualified, and **scoping is single-level**: an instance binding is an
  expression in the parameters of the sheet the instance sits on, and nothing further out is in
  scope. Following a design-wide constant down a hierarchy means a parameter on every sheet in
  between, which is what the bind-to-top button materialises.
- The open sheet is drawn at the values its parameters take under the current top sheet, but only
  where every instance agrees (`ExactValue`). Where they disagree, or the sheet is not
  instantiated under the top, it is drawn at its declared values. Design-time width inference runs
  on whatever is drawn; simulation elaboration performs the exact check.
- `signatureOfInstanceWithCertainty` cannot evaluate a binding that is an expression in the parent
  sheet's parameters when it is asked about a canvas on its own, so
  `checkCustomComponentForOkIOs` compares port names but not widths in that case. This is
  deliberate — the alternative is failing correct designs — but it means a genuine width error of
  that shape is caught at simulation rather than at load.
- A pasted non-custom component whose width was parameterised freezes at its resolved value when
  pasted onto another sheet, and nothing reports it: neither `Model.Clipboard` nor
  `SymbolT.Model.CopiedSymbols` records which sheet the copy came from. See finding 6 in
  [parameterReviewFindings.md](parameterReviewFindings.md).

## Best Practices

1. **Use descriptive parameter names**: `dataWidth` instead of `W`. Names are letters and digits
   only — there is no underscore, so `DATA_WIDTH` is not a name Issie will accept
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

### Constraint Violation
- Review constraint definitions
- Check calculated values against limits
- Adjust parameter values or constraints

### Simulation Failure
- Verify all parameters resolve to valid integers
- Check for circular parameter dependencies
- Ensure component types match parameter slots
