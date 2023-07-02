#!/usr/bin/env node

import { argv } from "node:process";
import { FSharpList, loadAllComponentFiles, runSimulation } from "./utils.js";

import { FSharpList_get_Empty } from "./temp/fable_modules/fable-library.4.1.4/List.js";

const test = (
  testcasesPath,
  simulationArraySize,
  lastStepNeeded,
  timeOut,
  testNumber,
) => {
  const loadedComponents = loadAllComponentFiles(testcasesPath);

  const ldcs = loadedComponents.reduce((tail, head) => {
    return new FSharpList(head, tail);
  }, FSharpList_get_Empty());

  const topComp = loadedComponents[testNumber];

  const simulationResult = runSimulation(
    topComp,
    ldcs,
    timeOut,
    lastStepNeeded,
    simulationArraySize,
  );

  console.log(
    JSON.stringify({
      time: simulationResult.time,
      result: simulationResult.result,
      numComps: simulationResult.numComps,
      sheetName: topComp.Name,
    }),
  );
};

const main = () => {
  const options = argv.slice(2);

  let i = 0;
  const testNumber = options[i++] || 0;
  const simulationArraySize = options[i++] || 1E6;
  const lastStepNeeded = options[i++] || 1E6;
  const testcasesPath = options[i++] || "../testcases";
  const timeOut = options[i++] || null;

  test(testcasesPath, simulationArraySize, lastStepNeeded, timeOut, testNumber);
};

main();
