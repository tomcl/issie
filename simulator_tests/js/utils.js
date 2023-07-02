import { existsSync, readdirSync, readFileSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";
import { performance } from "node:perf_hooks";
import lodash from "lodash";
import {
  JsonHelpers_jsonStringToState,
  JsonHelpers_SavedInfo__get_getSheetInfo,
  JsonHelpers_SavedInfo__get_getTimeStamp,
  JsonHelpers_SavedInfo__get_getWaveInfo,
} from "./temp/src/Renderer/Common/Helpers.js";
import { parseDiagramSignature } from "./temp/src/Renderer/Simulator/Extractor.js";
import { CCForm, LoadedComponent } from "./temp/src/Renderer/Common/CommonTypes.js";
import { FSharpList } from "./temp/fable_modules/fable-library.4.1.4/List.js";
import { length as seqLength } from "./temp/fable_modules/fable-library.4.1.4/Seq.js";
import { SimulationError } from "./temp/src/Renderer/Simulator/SimulatorTypes.js";
// Simulator
import { startCircuitSimulation } from "./temp/src/Renderer/Simulator/Simulator.js";
import { runFastSimulation } from "./temp/src/Renderer/Simulator/Fast/FastRun.js";

const toPrecision = (num) => num.toPrecision(3);

// Get the number of components involved in a FastSimulation
const getNumComponents = (fastSim) => {
  return seqLength(fastSim.FComps.values());
};

function tryLoadStateFromPath(filePath) {
  if (!existsSync(filePath)) {
    console.error(
      `Can't read file from ${filePath} because it does not seem to exist!`,
    );
  }

  try {
    const content = readFileSync(filePath, "utf8");
    return JsonHelpers_jsonStringToState(content).fields[0];
  } catch (err) {
    console.error(
      `could not convert file ${filePath} to a valid issie design sheet. Details: ${err}`,
    );
    return [];
  }
}

function getBaseNameNoExtension(filePath) {
  return basename(filePath, extname(filePath));
}

function getLatestCanvas(state) {
  return state.fields[0];
}

function makeLoadedComponentFromCanvasData(
  canvas,
  filePath,
  timeStamp,
  waveInfo,
  sheetInfo,
) {
  const projectPath = dirname(filePath);
  const patternInput = parseDiagramSignature(canvas[0], canvas[1]);
  const outputs = patternInput[1];
  const inputs = patternInput[0];
  const conns = canvas[1];
  const comps = canvas[0];
  // NOTE: checkMemoryContents is not implemented yet
  // const compsChecked = map(
  //   (comp) => checkMemoryContents(projectPath, comp),
  //   comps,
  // );
  const compsChecked = comps;
  const canvas_1 = [compsChecked, conns];
  let patternInput_1;
  if (sheetInfo != null) {
    const sI = sheetInfo;
    patternInput_1 = [sI.Form, sI.Description];
  } else {
    patternInput_1 = [new CCForm(0, []), void 0];
  }
  const form = patternInput_1[0];
  const description = patternInput_1[1];
  const ldc = new LoadedComponent(
    getBaseNameNoExtension(filePath),
    timeStamp,
    filePath,
    waveInfo,
    canvas_1,
    inputs,
    outputs,
    form,
    description,
  );
  return ldc;
}

function tryLoadComponentFromPath(filePath) {
  try {
    const state = tryLoadStateFromPath(filePath);
    const canvas = getLatestCanvas(state);
    return makeLoadedComponentFromCanvasData(
      canvas,
      filePath,
      JsonHelpers_SavedInfo__get_getTimeStamp(state),
      JsonHelpers_SavedInfo__get_getWaveInfo(state),
      JsonHelpers_SavedInfo__get_getSheetInfo(state),
    );
  } catch (err) {
    console.error(
      `Can't load component ${getBaseNameNoExtension(filePath)} because of Error: ${err.stack}`,
    );
  }
}

function loadAllComponentFiles(folderPath) {
  let files = [];
  try {
    files = readdirSync(folderPath).filter((file) => extname(file) === ".dgm");
  } catch (err) {
    console.error(
      `Error reading Issie project directory at ${folderPath}: ${err.stack}`,
    );
  }
  return files.map((file) => {
    // NOTE: fileNameIsBad checking is skipped
    const filePath = join(folderPath, file);
    console.info(`loading  ${filePath}`);
    const ldComp = tryLoadComponentFromPath(filePath);
    return ldComp;
  });
}

// For UInt32Arrays and BigIntArrays
function comps2String(fs) {
  return fs.FOrderedComps.map((comp) => {
    return {
      compType: comp.FType,
      fullName: comp.FullName,
      outputs: comp.Outputs.map((output) => {
        return Object.values(output.Width > 32 ? output.BigIntStep : output.UInt32Step);
      }),
    };
  });
}

function runSimulation(
  topComp,
  loadedComponents,
  timeOut,
  lastStepNeeded,
  simulationArraySize,
  warmupIterations = 5, // TODO: How is fastReduce optimized and deoptimized by TurboFan?
  testIterations = 10, // TODO
) {
  const canvasState = topComp.CanvasState;
  const diagramName = topComp.Name;
  const components = canvasState[0];
  const connections = canvasState[1];

  let time = 0;

  console.info(`Run simulation for ${diagramName}`);

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
  // inspect FastSimulation
  {
    const deepCopy = lodash.cloneDeep(fs);
    %DebugPrint(fs.FOrderedComps[0].Outputs[0].UInt32Step);
  }
  // warm up the JIT compiler
  for (let i = 0; i < warmupIterations; i++) {
    const deepCopy = lodash.cloneDeep(fs);
    runFastSimulation(timeOut, lastStepNeeded, deepCopy);
  }
  // run the actual test
  for (let i = 0; i < testIterations; i++) {
    const deepCopy = lodash.cloneDeep(fs);
    const t0 = performance.now();
    runFastSimulation(timeOut, lastStepNeeded, deepCopy);
    const t1 = performance.now();
    time += t1 - t0;
  }
  const numComps = getNumComponents(fs);
  time /= testIterations;
  console.debug(`runFastSimulation() took ${toPrecision(time)} milliseconds.`);
  return {
    result: comps2String(fs),
    time: time,
    numComps: numComps,
  };
}

export {
  FSharpList,
  getNumComponents,
  loadAllComponentFiles,
  runSimulation,
  toPrecision,
};
