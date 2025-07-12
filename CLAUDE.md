# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Issie (Interactive Schematic Simulator with Integrated Editor) is a digital circuit design and simulation application written in F# using functional programming paradigms. It transpiles to JavaScript via Fable and runs as an Electron desktop application.

## Essential Commands

### Building and Running
```bash
# Initial setup (installs dependencies and builds)
build.cmd    # Windows
build.sh     # Linux/Mac

# Development with hot reload
npm run dev

# Debug mode (includes assertions, slower)
npm run debug

# Build production executables
npm run dist

# Clean build artifacts
build clean
```

### Testing and Quality
```bash
# Run tests (note: tests are outdated and may not work)
dotnet test

# No specific lint command - F# compiler provides type checking
```

## Architecture Overview

The application follows the Elmish MVU (Model-View-Update) pattern:

1. **Model** (`/src/Renderer/Model/ModelType.fs`): Central application state
2. **View** (`/src/Renderer/UI/`): React components written in F#
3. **Update** (`/src/Renderer/UI/Update.fs`): State transitions via messages

### Key Modules

- **DrawBlock** (`/src/Renderer/DrawBlock/`): SVG-based schematic editor
  - `Symbol.fs`: Component rendering and logic
  - `BusWire.fs`: Wire routing and rendering
  - `Sheet.fs`: Canvas management

- **Simulator** (`/src/Renderer/Simulator/`): Circuit simulation engine
  - `FastSim/`: Optimized simulation implementation
  - `CanvasExtractor.fs`: Converts visual design to simulation graph

- **VerilogComponent** (`/src/Renderer/VerilogComponent/`): Verilog import functionality
  - Uses Nearley parser (`VerilogGrammar.ne`)
  - Converts Verilog to Issie components

### File Formats

- `.dgm`: Individual circuit diagram (JSON format)
- `.dprj`: Project marker file (empty)
- `.ram`: Memory initialization data

## Development Patterns

### F# and Functional Programming
- Immutable data structures throughout
- Pattern matching for control flow
- Option/Result types for error handling
- No null values - use Option types

### Elmish Message Handling
Messages flow through a central dispatch:
```fsharp
type Msg =
    | Wire of BusWire.Msg
    | Sheet of DrawModelType.SheetT.Msg
    | SimulationStart
    // etc.
```

### Component Creation
New Issie components are defined in `Symbol.fs` with:
- Port definitions
- Rendering logic (SVG generation)
- Simulation behavior

### Build System
- FAKE scripts handle complex build tasks
- Webpack bundles the transpiled JavaScript
- Paket manages F# dependencies
- npm manages JavaScript dependencies