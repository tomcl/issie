# Issie Test Suite

Comprehensive testing framework for the Issie digital circuit designer and simulator.

## Overview

The test suite provides comprehensive coverage for all major components of Issie:
- **Simulator Engine**: Circuit simulation, bus width inference, synchronous components
- **DrawBlock Components**: Symbol rendering, wire routing, canvas management  
- **Common Utilities**: Helper functions, extensions, optics
- **Verilog Integration**: Parsing, error checking, conversion to Issie components

## Test Framework

- **Framework**: Expecto 8.13.1 (F# testing framework)
- **Style**: Functional, pure tests with no side effects
- **Parallelization**: Tests run in parallel for faster execution
- **Coverage**: Unit tests, integration tests, property-based tests

## Running Tests

### Local Development

#### Windows
```bash
# Run all tests
.\run-tests.cmd

# Or use dotnet directly
dotnet run --project Tests\Tests.fsproj
```

#### Linux/Mac
```bash
# Run all tests
./run-tests.sh

# Or use dotnet directly
dotnet run --project Tests/Tests.fsproj
```

### GitHub Actions

Tests automatically run on:
- Every push to `master`, `main`, `develop`, or `test*` branches
- All pull requests
- Manual workflow dispatch

The CI pipeline tests on multiple configurations:
- **OS**: Ubuntu, Windows, macOS
- **.NET**: 6.0, 7.0, 8.0
- **Node.js**: 18.x, 20.x

## Test Structure

```
Tests/
├── README.md                    # This file
├── Tests.fsproj                 # Project file
├── Tests.fs                     # Main test runner
├── TestLib.fs                   # Test utilities
│
├── CanvasStates*.fs            # Test data for canvas states
├── SimulatorTests.fs           # Simulator unit tests
├── SimulatorSyncTests.fs       # Synchronous component tests
├── SimulatorMemoriesTests.fs   # Memory component tests
├── WidthInfererTests.fs        # Bus width inference tests
│
├── CommonTests.fs              # NEW: Common utilities tests
├── DrawBlockTests.fs           # NEW: Visual component tests
└── VerilogTests.fs            # NEW: Verilog integration tests
```

## Test Categories

### 1. Simulator Tests
Tests the core simulation engine:
- Combinatorial logic (AND, OR, NOT, XOR, etc.)
- Multiplexers and demultiplexers
- Bus operations (merge, split)
- Error detection (cycles, unconnected ports)
- Custom components

### 2. Synchronous Tests
Tests clocked components:
- D flip-flops
- Registers
- Counters
- State machines
- Clock cycle simulation

### 3. Memory Tests
Tests memory components:
- ROM functionality
- RAM read/write operations
- Address decoding
- Large memory arrays

### 4. Width Inference Tests
Tests automatic bus width calculation:
- Width propagation
- Width conflicts detection
- Partial connections
- Complex bus merging

### 5. DrawBlock Tests (NEW)
Tests visual components:
- **Symbol Module**: Component creation, port positioning, sizing
- **BusWire Module**: Wire routing, auto-routing, selection
- **Sheet Module**: Canvas operations, selection, copy/paste

### 6. Common Tests (NEW)
Tests utility functions:
- **Helpers**: Data manipulation, map operations
- **EEExtensions**: List extensions, string utilities
- **Optics**: Lens and prism operations
- **Position**: Coordinate calculations

### 7. Verilog Tests (NEW)
Tests Verilog integration:
- **Parsing**: Module parsing, port extraction
- **Error Checking**: Undeclared wires, width mismatches
- **Conversion**: Verilog to Issie component mapping

## Writing New Tests

### Test Template
```fsharp
module MyModuleTests

open Expecto
open MyModule

let testData = [
    "Test description",
    fun () ->
        let input = createTestInput()
        let result = functionUnderTest input
        Expect.equal result expectedValue "Error message"
]

[<Tests>]
let tests = 
    testList "MyModule Tests" (
        testData |> List.map (fun (name, test) ->
            testCase name test
        )
    )
```

### Best Practices
1. Keep tests pure and deterministic
2. Use descriptive test names
3. Test both success and failure cases
4. Group related tests together
5. Use property-based testing for complex invariants

## Coverage Goals

### Current Coverage
- ✅ Simulator: ~90% coverage
- ✅ Width Inferer: ~80% coverage
- ✅ Synchronous Components: ~85% coverage
- ✅ Memory Components: ~75% coverage
- ⚠️ DrawBlock: ~30% coverage (new tests added)
- ⚠️ Common Utilities: ~40% coverage (new tests added)
- ⚠️ Verilog: ~25% coverage (new tests added)

### Target Coverage
- All critical paths: 100%
- Core modules: >80%
- UI components: >60%
- Helper functions: >70%

## Continuous Integration

The GitHub Actions workflow (`test.yml`) provides:
- Multi-platform testing (Windows, Linux, macOS)
- Multiple .NET and Node.js versions
- Parallel test execution
- Test result artifacts
- Code coverage reports
- Linting and formatting checks

## Troubleshooting

### Common Issues

1. **Paket restore fails**
   ```bash
   dotnet tool restore
   dotnet paket install
   ```

2. **Fable compilation errors**
   ```bash
   dotnet fable clean
   npm run compile:parallel
   ```

3. **Test discovery issues**
   ```bash
   dotnet build Tests/Tests.fsproj
   ```

4. **Missing dependencies**
   ```bash
   npm install
   dotnet restore
   ```

## Contributing

When adding new features:
1. Write tests BEFORE implementing the feature (TDD)
2. Ensure all existing tests still pass
3. Add integration tests for complex features
4. Update this README if adding new test categories

## Performance Testing

For performance-critical code:
```fsharp
let performanceTest() =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    // Run operation many times
    for i in 1..10000 do
        operationUnderTest()
    stopwatch.Stop()
    Expect.isLessThan stopwatch.ElapsedMilliseconds 1000L 
        "Operation should complete in under 1 second"
```

## Future Improvements

- [ ] Add property-based testing with FsCheck
- [ ] Implement visual regression testing for UI
- [ ] Add performance benchmarks
- [ ] Create mutation testing
- [ ] Add test coverage badges
- [ ] Implement snapshot testing for complex outputs