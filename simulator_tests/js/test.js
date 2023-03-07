// import { readFilesFromDirectory } from "../../src/Renderer/Interface/FilesIO.fs.js"
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";
import {
  JsonHelpers_jsonStringToState,
  JsonHelpers_SavedInfo__get_getSheetInfo,
  JsonHelpers_SavedInfo__get_getTimeStamp,
  JsonHelpers_SavedInfo__get_getWaveInfo,
} from "./temp/renderer/Common/Helpers.js";
import { parseDiagramSignature } from "./temp/renderer/NewSimulator/Extractor.js";
import { LoadedComponent } from "./temp/renderer/Common/CommonTypes.js";
import { startCircuitSimulation } from "./temp/renderer/NewSimulator/Simulator.js";
import { runFastSimulation } from "./temp/renderer/NewSimulator/Fast/FastRun.js";
// import { map } from "../../src/Renderer/fable_modules/fable-library.4.0.0-theta-018/List.js"; // const {

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
    // console.debug(canvas);
    // makeLoadedComponentFromCanvasData(canvas)
    return makeLoadedComponentFromCanvasData(
      canvas,
      filePath,
      JsonHelpers_SavedInfo__get_getTimeStamp(state),
      JsonHelpers_SavedInfo__get_getWaveInfo(state),
      JsonHelpers_SavedInfo__get_getSheetInfo(state),
    );
  } catch (err) {
    console.error(
      `Can't load component ${
        getBaseNameNoExtension(filePath)
      } because of Error: ${err}`,
    );
  }
}

function loadAllComponentFiles(folderPath) {
  let files = [];
  try {
    files = readdirSync(folderPath).filter((file) => extname(file) === ".dgm");
  } catch (err) {
    console.error(
      `Error reading Issie project directory at ${folerPath}: ${err}`,
    );
  }
  return files.map((file) => {
    // NOTE: fileNameIsBad checking is skipped
    const filePath = join(folderPath, file);
    console.debug(`loading  ${filePath}`);
    const ldComp = tryLoadComponentFromPath(filePath);
    return ldComp;
  });
}

function main() {
  const testcasesPath = "../testcases";
  const simulationArraySize = 200;
  const lastStepNeeded = 10000;
  const timeOut = null;
  const diagramName = "";

  const loadedDependencies = loadAllComponentFiles(testcasesPath);
  // console.debug(loadedDependencies);
  const canvasState = loadedDependencies[1].CanvasState;
  const components = canvasState[0];
  const connections = canvasState[1];
  const simData = startCircuitSimulation(
    simulationArraySize,
    diagramName,
    components,
    connections,
    loadedDependencies,
  ).fields[0];
  // console.debug(simData);
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

main();
