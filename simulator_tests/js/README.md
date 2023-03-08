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

#### Compare execution time

For each test case, the simulators will be warmed up by running simulation `warmupIterations` times before the actual tests.
The actual test will then run simulation `testIterations` times to obtain average execution time.

## Results

- By removing one branch in `fastReduce` function (FData -> FastData), the performance of the new simulator is improved by 98%.
