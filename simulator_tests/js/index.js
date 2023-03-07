import {
  SimulationError,
} from "./temp/src/Renderer/NewSimulator/SimulatorTypes.js";
import {
  FSharpList,
  loadAllComponentFiles,
  NewSimulator,
  OldSimulator,
} from "./utils.js";
import { performance } from "node:perf_hooks";
import test from "node:test";
import assert from "node:assert";

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
  console.log(`========== Running simulation for ${diagramName} ==========`);

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
  console.log(`runFastSimulation took ${time} milliseconds.`);
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
  return test(topComp, async (t) => {
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
    console.debug(resultFromNew.time, resultFromOld.time);
    await t.test("Same output", (t) => {
      assert.deepStrictEqual(resultFromNew.result, resultFromOld.result);
    });
    await t.test("Performance of runFastSimulation", (t) => {
      assert.ok(resultFromNew.time <= resultFromOld.time);
    });
  });
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
  loadedComponents.map((ldComp) =>
    runTest(
      ldComp,
      ldcs,
      timeOut,
      lastStepNeeded,
      simulationArraySize,
    )
  );
}

main();
