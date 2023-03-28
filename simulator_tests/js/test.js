import assert from "node:assert";

import {
  FSharpList,
  loadAllComponentFiles,
  logger,
  NewSimulator,
  OldSimulator,
  simulationFactory,
  toPrecision,
} from "./utils.js";

describe("runFastSimulation()", function () {
  const testcasesPath = "../testcases";
  const simulationArraySize = 500;
  const lastStepNeeded = 2000;
  const timeOut = null;

  const details = { function: "runFastSimulation" };

  const loadedComponents = loadAllComponentFiles(testcasesPath);
  logger.verbose({
    label: "test bench",
    message:
      `Loaded ${loadedComponents.length} components from ${testcasesPath}\n` +
      loadedComponents.map((ldComp, i) => `${i}: ${ldComp.Name}`).join("\n"),
    detail: details,
  });
  const ldcs = loadedComponents.reduceRight(
    (tail, head) => new FSharpList(head, tail),
    void 0,
  );

  const runSimulation = simulationFactory(
    ldcs,
    timeOut,
    lastStepNeeded,
    simulationArraySize,
    details,
  );

  logger.verbose({
    label: "test bench",
    message: `Start test for runFastSimulation()`,
    detail: details,
  });
  const execTimes = loadedComponents.map((topComp) => {
    logger.verbose({
      label: "test bench",
      message: `🚧 Start test for ${topComp.Name}`,
      detail: details,
    });
    const resultFromNew = runSimulation(NewSimulator, topComp);
    const resultFromOld = runSimulation(OldSimulator, topComp);
    it(`compare simulation result of ${topComp.Name}`, function () {
      assert.deepStrictEqual(resultFromNew.result, resultFromOld.result);
    });
    return [resultFromNew.time, resultFromOld.time];
  });
  logger.verbose({
    label: "test bench",
    message: "Test for runFastSimulation() finished",
  });

  // Geometric mean of execution times of all test cases for both simulators
  const [avgExecTimeNew, avgExecTimeOld] = ["new", "old"].map((simulator, i) => {
    const avgExecTime = Math.sqrt(
      execTimes.reduce((a, b) => a * b[i], 1),
      execTimes.length,
    );
    logger.info({
      label: "test bench",
      message: `Geometric mean of execution times of ${simulator} runFastSimulation(): ${toPrecision(
        avgExecTime,
      )}`,
      detail: {
        ...details,
        type: "geometric mean",
        value: avgExecTime,
        simulator,
      },
    });
    return avgExecTime;
  });

  logger.info({
    label: "test bench",
    message: `New runFastSimulation() is ${toPrecision(
      ((avgExecTimeOld - avgExecTimeNew) / avgExecTimeOld) * 100,
    )}% faster`,
    detail: details,
  });
});
