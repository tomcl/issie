import { performance } from "node:perf_hooks";
import test from "node:test";
import assert from "node:assert";
import chalk from "chalk";
import {
  SimulationError,
} from "./temp/src/Renderer/NewSimulator/SimulatorTypes.js";
import {
  FSharpList,
  loadAllComponentFiles,
  NewSimulator,
  OldSimulator,
} from "./utils.js";

function runSimulation(
  simulator,
  topComp,
  loadedComponents,
  timeOut,
  lastStepNeeded,
  simulationArraySize,
) {
  const canvasState = topComp.CanvasState;
  const diagramName = topComp.Name;
  const components = canvasState[0];
  const connections = canvasState[1];
  const color = simulator.type === "new" ? chalk.blue : chalk.green;
  console.error(color(
    `========== Running simulation for ${diagramName} with ${simulator.type} simulator ==========`,
  ));
  const simData = simulator.startCircuitSimulation(
    simulationArraySize,
    diagramName,
    components,
    connections,
    loadedComponents,
  ).fields[0];
  if (simData instanceof SimulationError) {
    console.warn(simData);
    return;
  }
  const fs = simData.FastSim; // FastSimulation
  // console.error(fs);
  const iterations = 200;
  let time = 0;
  for (let i = 0; i < iterations; i++) {
    const t0 = performance.now();
    simulator.runFastSimulation(timeOut, lastStepNeeded, fs);
    const t1 = performance.now();
    time += t1 - t0;
  }
  time /= iterations;
  console.log(
    color(
      `[${simulator.type}] : runFastSimulation took ${
        chalk.bold(
          time.toFixed(2),
        )
      } milliseconds.`,
    ),
  );
  return {
    result: simulator.extractDriversContent(fs),
    time: time,
  };
}

function runTest(
  topComp,
  loadedComponents,
  timeOut,
  lastStepNeeded,
  simulationArraySize,
) {
  console.log(
    chalk.red(
      `[test bench] : 🚧 Running test for ${chalk.bold(topComp.Name)}`,
    ),
  );
  let execTimeNew = 0;
  let execTimeOld = 0;
  const resultFromNew = runSimulation(
    NewSimulator,
    topComp,
    loadedComponents,
    timeOut,
    lastStepNeeded,
    simulationArraySize,
  );
  const resultFromOld = runSimulation(
    OldSimulator,
    topComp,
    loadedComponents,
    timeOut,
    lastStepNeeded,
    simulationArraySize,
  );
  execTimeNew = resultFromNew.time;
  execTimeOld = resultFromOld.time;
  test(topComp.Name, async (t) => {
    await t.test("Same output", (t) => {
      assert.deepStrictEqual(resultFromNew.result, resultFromOld.result);
      // console.log(chalk.red("[test bench] : Same output from both simulators"));
    });
  });
  console.log(chalk.red(
    `[test bench] : New simulator is ${
      chalk.bold(
        ((execTimeOld - execTimeNew) / execTimeOld * 100).toFixed(2),
      )
    }% faster`,
  ));
  return [execTimeNew, execTimeOld];
}

function main() {
  const testcasesPath = "../testcases";
  const simulationArraySize = 500;
  const lastStepNeeded = 10000;
  const timeOut = null;

  const loadedComponents = loadAllComponentFiles(testcasesPath);
  // console.error(loadedComponents);
  const ldcs = loadedComponents.reduceRight(
    (tail, head) => (new FSharpList(head, tail)),
    void 0,
  );
  // console.error(ldcs);
  const execTimes = loadedComponents.map((ldComp) =>
    runTest(
      ldComp,
      ldcs,
      timeOut,
      lastStepNeeded,
      simulationArraySize,
    )
  );
  // Geometric mean of execution times of all test cases for both simulators
  const avgExecTimeNew = Math.sqrt(
    execTimes.reduce((a, b) => a * b[0], 1),
    execTimes.length,
  );
  const avgExecTimeOld = Math.sqrt(
    execTimes.reduce((a, b) => a * b[1], 1),
    execTimes.length,
  );
  console.error(
    "Geometric mean of new simulator:",
    avgExecTimeNew,
  );
  console.error(
    "Geometric mean of old simulator:",
    avgExecTimeOld,
  );
  console.log(
    `New simulator is ${
      ((avgExecTimeOld - avgExecTimeNew) / avgExecTimeOld * 100).toFixed(2)
    }% faster`,
  );
}

main();
