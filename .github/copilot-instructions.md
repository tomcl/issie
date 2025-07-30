# Issie - GitHub Copilot Instructions

## Project Overview

Issie is a functional reactive digital circuit design and simulation application built in F# using the Elmish MVU (Model-View-Update) pattern. It transpiles to JavaScript via Fable and runs as an Electron desktop app.

## Goal:
Implement the parameter system inside `ParameterView.fs` using only serial lines of clean code, minimal total lines.

### Context:
- Use functional programming style **always**:
  - **No** `for` loops.
  - **No** `mutable` variables or side effects.
- Reuse existing helper functions from the codebase where possible.
- Keep the implementation as **simple** and **concise** as possible.

### Requirements:
1. Write clean, serial code—one statement per line (no chaining or nesting unnecessarily).
2. No imperative constructs: only use `map`, `fold`, `filter`, `Seq`, `List`, pipeline, recursion.
3. Always immutable; prefer pattern matching and pipelining.
4. Show example input-output mapping if applicable.
5. Document assumptions in comments as short natural language.
6. After implementation, include a one-line explanation comment at end like `// Implemented in N lines`.

### Constraints:
- Avoid complexity; if a built-in function exists, use it instead of writing a helper.

## Essential Architecture

### Elmish MVU Pattern
- **Single source of truth**: All state lives in `ModelType.Model` (global immutable record)
- **Message dispatch**: All state changes via `Msg` union types processed in `Update.fs`
- **Optics everywhere**: State updates use lens/prism composition (`model |> set lens_ newValue`)
- **Commands**: Side effects via `Cmd<Msg>` returned from update functions

Example update pattern:
```fsharp
| SetSelectedComponent comp ->
    model
    |> set selectedComponent_ (Some comp)
    |> withNoMsg  // helper for (model, Cmd.none)
```

### Key Modules & Data Flow

1. **DrawBlock** (`/src/Renderer/DrawBlock/`): SVG schematic editor
   - `Symbol.fs`: Component rendering (900+ lines) - defines visual representation
   - `BusWire.fs`: Wire routing with auto-routing algorithms
   - `Sheet.fs`: Canvas management, selection, mouse interactions

2. **Simulator** (`/src/Renderer/Simulator/`): Circuit simulation engine
   - `CanvasExtractor.fs`: Converts visual design → simulation graph
   - `FastSim/`: Optimized bitwise simulation implementation
   - **Critical**: Visual canvas ≠ simulation graph - extractor bridges the gap

3. **VerilogComponent** (`/src/Renderer/VerilogComponent/`): Verilog import
   - `VerilogGrammar.ne`: Nearley parser grammar (compile with `npx nearleyc`)
   - F# bindings to JavaScript parser for Verilog → Issie components

## Development Workflows

### Building & Running
```bash
# Windows setup
build.cmd          # Full setup: installs deps + builds + dev mode with HMR

# Development commands (after setup)
npm run dev        # Hot reload development
npm run debug      # Debug mode (slower, has assertions)
npm run dist       # Production binaries

# Build system
dotnet fable       # F# → JS transpilation
build.fsx          # FAKE build script with FAKE 5 syntax
```

### Critical Build Details
- **FAKE 5**: Uses `#r "nuget:"` syntax, targets defined with `Target.create`
- **Fable transpilation**: Every `.fs` gets `.fs.js` + `.fs.js.map` files
- **Webpack bundling**: Separate configs for main/renderer processes
- **Dependencies**: Paket for F#, npm for JS, both must stay in sync

## Conventions & Patterns

### F# Functional Style
```fsharp
// Pattern matching everywhere
match model.Action with
| DragAndDrop -> (* handle drag *)
| Idle -> (* handle idle *)

// Option types, never null
let getComponent id model =
    Map.tryFind id model.Components  // returns Option<Component>

// Pipeline operators for readability
model
|> set selectedComponents_ []
|> map waveSim_ (updateWaveforms simulation)
```

### State Management with Optics
```fsharp
// Lenses defined in ModelType.fs for every record field
let selectedComponent_ = Lens.create (fun a -> a.SelectedComponent) (fun s a -> {a with SelectedComponent = s})

// Composition for nested updates
model |> Optic.set (sheet_ >-> symbols_ >-> symbolAt_ compId >-> label_) newLabel
```

### Message Handling Patterns
- **Nested messages**: `Sheet (Wire (Symbol msg))` - drill down through modules
- **Command batching**: `Cmd.batch [msg1; msg2]` for multiple side effects
- **Async operations**: `Cmd.OfAsyncImmediate.result` for file I/O, simulation

### File Structure Conventions
- **Types**: `*Types.fs` define data structures
- **Update**: `*Update.fs` handle messages for that module
- **Helpers**: `*Helpers.fs` pure functions, no state dependencies
- **View**: `*View.fs` React components (return `ReactElement`)

## Critical Integration Points

### Canvas ↔ Simulation Bridge
```fsharp
// CanvasExtractor.fs - THE critical data transformation
let extractReducedState (components, connections) : SimulationGraph =
    // Strips visual layout, creates electrical graph
```

### Component Creation Flow
1. `CatalogueView.fs` - user selects component type
2. `Sheet.fs` - handles mouse placement
3. `Symbol.fs` - creates visual representation + ports
4. `CanvasExtractor.fs` - converts to simulation node

### File Persistence
- **Project format**: `.dprj` (empty marker) + multiple `.dgm` files
- **Canvas state**: `(Component list * Connection list)` serialized as JSON
- **Auto-backup**: Continuous backup to `/backup/` subdirectory

## Testing & Debugging

### Debug Infrastructure
```fsharp
// JSHelpers.fs provides debug tracing
if Set.contains "update" JSHelpers.debugTraceUI then
    printfn "Message: %A" msg

// Memory monitoring (enabled via CheckMemory msg)
let heapInBytes = JSHelpers.getProcessPrivateMemory()
```

### Common Gotchas
- **Fable compilation**: JS output changes with F# - check `.fs.js` files for issues
- **Elmish timing**: Some updates need `Cmd.OfAsyncImmediate` with delays for UI sync
- **Wire routing**: Complex state machine - test thoroughly with edge cases
- **Memory components**: Special handling for RAM/ROM initialization from `.ram` files

## External Dependencies

- **Electron**: Desktop app host - all file I/O goes through main process
- **Nearley**: Verilog parser (JS library with F# bindings)
- **SVG rendering**: Direct SVG manipulation, not React-managed
- **Node.js tools**: Required for Fable + webpack build chain

Use existing patterns when adding features - the codebase is highly systematic and functional patterns are strictly enforced.
