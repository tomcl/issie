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
    | NGateInputs           // Number of gate inputs
    | IO of Label: string   // Input/Output port widths
    | CustomCompParam of ParamName: string // Custom component parameters
```

#### Sheet-Level Definitions
```fsharp
type ParameterDefs = {
    DefaultBindings: ParamBindings  // Parameter name to expression mappings
    ParamSlots: ComponentSlotExpr   // Component slots bound to expressions
}
```

### UI Layer (`ParameterView.fs`)

Manages all parameter-related user interactions:

#### Sheet Parameter Management
- **`addParameterBox`**: Create new sheet parameters via popup dialog
- **`editParameterBox`**: Modify existing parameter values
- **`deleteParameterBox`**: Remove parameters from sheet
- **`makeParamsField`**: Display sheet parameters in properties panel

#### Component Parameter Binding
- **`paramInputField`**: Generic input field supporting parameter expressions
- **`makeParamBindingEntryBoxes`**: UI for custom component parameter bindings
- **`editParameterBindingPopup`**: Edit instance-specific parameter values
- **`updateComponent`**: Apply parameter changes to components via Sheet messages

#### Parameter Evaluation
- **`evaluateConstraints`**: Validate expressions against constraints
- **`updateComponents`**: Batch update all parameterized components
- **`resolveParametersForComponent`**: Resolve parameters for simulation

### Simulation Integration (`GraphMerger.fs`)

Handles parameter resolution during simulation graph construction using a two-stage process:

#### Stage 1 - Graph Merging
- Custom components replaced with internal graphs
- Parameter resolution intentionally deferred to avoid forward references
- Graphs stored in `CustomSimulationGraph` field

#### Stage 2 - Parameter Resolution

**2a. Instance-specific** (`resolveCustomComponentParameters`):
- Apply component instance parameter bindings
- Override default values from definitions
- Process nested custom components recursively

**2b. Sheet-level** (`resolveParametersInSimulationGraph`):
- Use default sheet parameter bindings
- Evaluate expressions for all parameterized slots
- Update component configurations with resolved values

This two-stage approach prevents forward reference issues and ensures proper parameter precedence.

### Validation Layer (`CanvasStateAnalyser.fs`)

Provides lightweight parameter resolution for component validation:
- Simplified evaluator supporting only `PInt` and `PParameter`
- Resolves I/O port widths for label extraction
- Optimized for validation performance

### Component Creation (`CatalogueView.fs`)

Integrates parameters during component instantiation:
- Merges parent sheet parameters with component defaults
- Resolves parameters before extracting port labels
- Passes parameter bindings to custom components
- Creates parameter input dialogs for component properties

## Data Flow

### 1. Parameter Definition Flow
```
User Input (Properties Panel)
    ↓
ParameterView.addParameterBox
    ↓
ParameterTypes.parseExpression (validate syntax)
    ↓
Update Model.LoadedComponent.LCParameterSlots
    ↓
Persist to .dgm file
```

### 2. Component Parameterization Flow
```
User selects component property
    ↓
ParameterView.paramInputField
    ↓
Parse & evaluate expression
    ↓
Check constraints
    ↓
Update ComponentSlotExpr map
    ↓
Apply to component via Sheet messages
```

### 3. Simulation Resolution Flow
```
Simulation Start
    ↓
GraphMerger.mergeDependencies
    ↓
Stage 1: Merge graphs (defer parameters)
    ↓
Stage 2a: Resolve instance parameters
    ↓
Stage 2b: Resolve sheet parameters
    ↓
FastSim with resolved values
```

### 4. Custom Component Flow
```
Parent Sheet Parameters
    ↓
Merge with Component Defaults
    ↓
Resolve Canvas State
    ↓
Extract Port Labels
    ↓
Create Custom Component Instance
```

## Expression Language

The parameter expression parser supports:

### Syntax Elements
- **Literals**: Integer constants (e.g., `8`, `32`)
- **Variables**: Alphanumeric parameter names (e.g., `WIDTH`, `dataSize`)
- **Operators** (with precedence):
  - Multiplication, Division, Modulo: `*`, `/`, `%` (higher precedence)
  - Addition, Subtraction: `+`, `-` (lower precedence)
- **Parentheses**: For grouping expressions

### Example Expressions
```
WIDTH           // Simple parameter reference
WIDTH + 1       // Increment parameter
(n * 8) - 1     // Complex calculation
baseAddr + (offset * 4)  // Address calculation
WIDTH / 2       // Division
SIZE % 8        // Modulo operation
```

### Parser Implementation
The parser uses recursive descent with separate functions for each precedence level:
- `parsePrimary`: Handles numbers, variables, and parentheses
- `parseFactors`: Processes multiplication, division, modulo
- `parseExpressionTokens`: Handles addition and subtraction

Notes and caveats:
- Tokenizer restricts inputs to digits/letters/operators/whitespace; unsupported characters are reported precisely.
- Division and modulo are evaluated during constant-folding; add a MinVal constraint to prevent zero divisors where needed.
  
Code: `src/Renderer/Common/ParameterTypes.fs` (`parseExpression`, tokenizer regex, and helpers)

## Parameter Scoping & Precedence

### Scope Levels
1. **Sheet-level parameters**: Defined in sheet properties, scope limited to that sheet
2. **Instance parameters**: Override sheet defaults for specific component instances
3. **Custom component parameters**: Inherited from parent sheet, can be overridden per instance

### Precedence Rules (highest to lowest)
1. Instance-specific bindings
2. Parent sheet parameters
3. Component default parameters

### Example Scenario
```
Sheet A defines: WIDTH = 8
Custom Component B has default: WIDTH = 16
Instance of B in A with override: WIDTH = 32

Result: Instance uses WIDTH = 32
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

## Three-Tier Evaluation Architecture

The system implements three evaluation strategies optimized for different contexts:

### 1. Full Resolution (`ParameterTypes.evaluateParamExpression`)
- **Purpose**: User interface feedback
- **Features**: 
  - Complete arithmetic support
  - Detailed error messages
  - Returns `Result<int, string>`
- **Usage**: Parameter input fields, validation messages

### 2. Graph Resolution (`GraphMerger.evalExpr`)
- **Purpose**: Simulation graph construction
- **Features**:
  - Returns `Option<int>` instead of `Result`
  - Optimized for batch processing
  - No error messages needed
- **Usage**: Simulation preparation

### 3. Validation Resolution (`CanvasStateAnalyser`)
- **Purpose**: Quick validation checks
- **Features**:
  - Minimal evaluator (PInt, PParameter only)
  - Just for I/O port widths
  - Maximum performance
- **Usage**: Component validation

## Persistence

Parameter data is stored across multiple locations:

### File Storage
- **`.dgm` files**: Sheet parameter definitions and slot bindings
  - Stored in `LCParameterSlots` field of LoadedComponent
  - JSON serialization of ParameterDefs type

### Runtime State
- **Model.LoadedComponent**: Current parameter values
- **Component.Type**: Resolved parameter values in components
- **CustomComponentType.ParameterBindings**: Instance overrides

## Component Support

### Currently Parameterizable Components

#### Width-Configurable Components
- Registers (`Register`, `RegisterE`)
- Adders (`NbitsAdder`, `NbitsAdderNoCin`, etc.)
- Logic Gates (`NbitsAnd`, `NbitsOr`, `NbitsNot`)
- Multiplexers and Demultiplexers
- Bus components (`BusCompare`, `BusSelection`)
- Counters (all variants)

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

1. Add case to `compSlot_` lens in ParameterView.fs
2. Update component type in CommonTypes.fs
3. Add UI support in component properties
4. Handle in simulation resolution

## Usage Examples

### Example 1: Define Sheet Parameter
```fsharp
// User adds parameter "WIDTH" with value 8
1. Open sheet properties panel
2. Click "Add Parameter"
3. Enter name: "WIDTH"
4. Enter value: 8
5. Parameter available for use in expressions
```

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
1. Define parameters: BASE_ADDR = 0x1000, BLOCK_SIZE = 256
2. Create comparator with expression: "BASE_ADDR + (BLOCK_SIZE * 4)"
3. System evaluates to 0x1400
```

## Error Handling

The system provides comprehensive error handling at multiple levels:

### Parse Errors
- **Invalid syntax**: "Contains unsupported characters: [']'"
- **Empty input**: "Input Empty"
- **Unmatched parentheses**: "Mismatched parentheses"

### Evaluation Errors
- **Undefined parameters**: "Parameter 'X' is not defined"
- **Multiple undefined**: "Parameters X, Y, Z are not defined"

### Constraint Violations
- **Value too small**: Custom message from MinVal constraint
- **Value too large**: Custom message from MaxVal constraint

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

### Debug Helpers
```fsharp
// Enable parameter debug tracing
if Set.contains "params" JSHelpers.debugTraceUI then
    printfn "Parameter evaluation: %A -> %A" expr value
```

### Common Issues
1. **Forward references**: Resolved by two-stage simulation
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

#### Parameter Resolution
```fsharp
resolveParametersForComponent: ParamBindings -> Map<ParamSlot, ConstrainedExpr> -> Component -> Result<Component, string>
```

#### Constraint Checking
```fsharp
evaluateConstraints: ParamBindings -> ConstrainedExpr list -> (Msg -> unit) -> Result<Unit, ParamConstraint list>
```

## Resolution Mechanics Deep-Dive

- UI evaluation: `ParameterTypes.evaluateParamExpression` performs recursive substitution and constant-folding with detailed errors. Used by `ParameterView` for validation and preview.
- Graph evaluation: `GraphMerger.resolveParametersInSimulationGraph` uses internal `evalExpr` (returns `Option<int>`) and `applySlotValue` to write concrete values into `SimulationGraph` component types after merge.
- Validation evaluation: `CanvasStateAnalyser.checkCustomComponentForOkIOs` embeds a minimal evaluator supporting only `PInt` and `PParameter` to resolve port widths quickly for label checking.
- Slot access: Lenses `ParameterView.compSlot_` and `ParameterView.modelToSlot_` provide strongly typed access into `Component.Type` for `Buswidth`, `NGateInputs`, and `IO label`.

## Developer Notes (Files & Responsibilities)

- `src/Renderer/Common/ParameterTypes.fs`: Types (`ParamExpression`, `ParamConstraint`, `ParamSlot`, `ParameterDefs`), parser (`parseExpression`), evaluator (`evaluateParamExpression`), renderer (`renderParamExpression`).
- `src/Renderer/UI/ParameterView.fs`: Sheet defaults and slot bindings CRUD, constraint checking, component updates, and parameter UI fields/popups.
- `src/Renderer/UI/CatalogueView.fs`: Merges parent sheet defaults with sub-sheet defaults, resolves canvas before extracting `InputLabels`/`OutputLabels`, sets `ParameterBindings` on instances.
- `src/Renderer/Simulator/GraphMerger.fs`: Two-stage resolution during merge; instance bindings first, then sheet defaults; recursion into nested custom components.
- `src/Renderer/Simulator/CanvasStateAnalyser.fs`: Lightweight parameter resolution for port label validation.

## Development History

Key commits that shaped the current system (from `git log`):
- a67fa72f Fix parameter resolution in simulation graph creation
  - Passes `loadedDependencies` into merger; applies instance-specific `ParameterBindings` for custom components.
- 83bb0b0b Fix forward reference issue in parameter resolution
  - Introduces two-stage resolution: resolve custom component instance bindings first, then sheet-level defaults.
- b510fe4b Parameter System Redo
  - Reworks UI binding flow and simulation integration; clearer separation of concerns.
- edf61e87 Parameter System Support
  - Integrates `ParameterTypes.fs`, updates merger/validation, and adds comprehensive documentation.

For a side-by-side comparison with an earlier streamlined approach, see `PARAMETER_SYSTEM_COMPARISON.md`.

## Known Limitations

- Integer-only parameters today (`ParamInt = int`); very large constants may require future `bigint`.
- No explicit guard on divide/modulo by zero during constant-folding; enforce with constraints.
- Parameter names are unqualified; deeper inheritance across sheet hierarchies may require qualification if extended.

## Best Practices

1. **Use descriptive parameter names**: `DATA_WIDTH` instead of `W`
2. **Define constraints early**: Prevent invalid values at input time
3. **Document parameter meanings**: In component descriptions
4. **Test edge cases**: Min/max values, zero, negative numbers
5. **Keep expressions simple**: Complex logic in simulation, not parameters
6. **Use consistent naming**: Across sheets and components
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
