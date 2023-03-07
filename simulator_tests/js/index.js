import {
  SimulationError,
} from "./temp/renderer/NewSimulator/SimulatorTypes.js";
import {
  FSharpList,
  loadAllComponentFiles,
  runFastSimulation,
  startCircuitSimulation,
} from "./utils.js";

function runSimulation(
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
  console.debug(`========== Running simulation for ${diagramName} ==========`);

  const simData = startCircuitSimulation(
    simulationArraySize,
    diagramName,
    components,
    connections,
    loadedComponents,
  ).fields[0];
  if (simData instanceof SimulationError) {
    console.error(simData);
    return;
  }
  const fs = simData.FastSim; // FastSimulation
  // console.debug(fs);
  runFastSimulation(timeOut, lastStepNeeded, fs);
  // console.debug(fs.Drivers);
  fs.Drivers.map((driver) => {
    if (driver == undefined || driver == null) return;
    const result = driver.DriverData.Step.map(
      (data) => data.Dat.fields[0],
    );
    console.debug(
      `Driver [Index=${driver.Index}, width=${driver.DriverWidth}] : [${
        result.join(", ")
      }]`,
    );
  });
}

function main() {
  const testcasesPath = "../testcases";
  const simulationArraySize = 200;
  const lastStepNeeded = 10000;
  const timeOut = null;

  const loadedComponents = loadAllComponentFiles(testcasesPath);
  //   console.debug(loadedComponents);
  const ldcs = loadedComponents.reduceRight(
    (tail, head) => (new FSharpList(head, tail)),
    void 0,
  );
  //   console.debug(ldcs);
  loadedComponents.map((ldComp) => {
    runSimulation(
      ldComp,
      ldcs,
      timeOut,
      lastStepNeeded,
      simulationArraySize,
    );
  });
}

main();
