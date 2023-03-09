# Test new simulator in JavaScript environment

This test bench compares the new simulator with the old one by running simulation on test cases in `simulator_tests/testcases`.

## Quick start

```bash
npm install # Install dependencies
npm run compile # Transpile ISSIE with both simulators, resulted .js files will be stored in ./temp
npm run test # Run the test bench
```

## Test bench structure

### Functionality tests

Simulation results of the new simulator is compared against the golden reference(the old simulator).
Simulation results from both simulators is compared using `assert.deepStrictEqual` to make sure the content of simulation results are exactly the same.

### Performance tests

#### Speed of simulation

For each test case, the simulators will be warmed up by running simulation `warmupIterations` times before the actual tests.
The actual test will then run simulation `testIterations` times to obtain arithmetic average of execution time for each test case.
The comparison between the two simulators is done by comparing the geometric mean of speed across all test cases.

## Profiling

```bash
CPU_PROF=1 bash inspect.sh # CPU profiling
HEAP_PROF=1 bash inspect.sh # Heap profiling
```

Results are stored in `./cpuprof` and `./heapprof` respectively.

## Results

- By removing one branch in `fastReduce` function (FData -> FastData), the new simulator obtained x40+ speedup on average.
