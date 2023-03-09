#!/usr/bin/env node

import { argv } from "node:process";
import {
  drivers2String,
  FSharpList,
  loadAllComponentFiles,
  NewSimulator,
  OldSimulator,
  simulationFactory,
  transportList,
} from "./utils.js";

const test = (
  testcasesPath,
  simulationArraySize,
  lastStepNeeded,
  timeOut,
  testNumber,
  simulatior,
) => {
  const details = { "function": "runFastSimulation" };

  const loadedComponents = loadAllComponentFiles(testcasesPath);

  const ldcs = loadedComponents.reduceRight(
    (tail, head) => (new FSharpList(head, tail)),
    void 0,
  );

  const runSimulation = simulationFactory(
    ldcs,
    timeOut,
    lastStepNeeded,
    simulationArraySize,
    details,
  );

  const topComp = loadedComponents[testNumber];

  const simulationResult = runSimulation(simulatior, topComp);

  console.log(JSON.stringify({
    time: simulationResult.time,
    values: drivers2String(simulationResult.result),
    numComps: simulationResult.numComps,
  }));
};

const main = () => {
  const options = argv.slice(2);

  let i = 0;
  const simulator = options[i++] === "old" ? OldSimulator : NewSimulator;
  const testNumber = options[i++] || 0;
  const simulationArraySize = options[i++] || 500;
  const lastStepNeeded = options[i++] || 2000;
  const testcasesPath = options[i++] || "../testcases";
  const timeOut = options[i++] || null;

  // disable console output from logger
  transportList.console.level = "error";

  test(
    testcasesPath,
    simulationArraySize,
    lastStepNeeded,
    timeOut,
    testNumber,
    simulator,
  );
};

main();
