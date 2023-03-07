import { existsSync, readdirSync, readFileSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";
import {
  JsonHelpers_jsonStringToState,
  JsonHelpers_SavedInfo__get_getSheetInfo,
  JsonHelpers_SavedInfo__get_getTimeStamp,
  JsonHelpers_SavedInfo__get_getWaveInfo,
} from "./temp/src/Renderer/Common/Helpers.js";
import { parseDiagramSignature } from "./temp/src/Renderer/NewSimulator/Extractor.js";
import {
  CCForm,
  LoadedComponent,
} from "./temp/src/Renderer/Common/CommonTypes.js";
import { startCircuitSimulation as newStartCircuitSimulation } from "./temp/src/Renderer/NewSimulator/Simulator.js";
import { runFastSimulation as newRunFastSimulation } from "./temp/src/Renderer/NewSimulator/Fast/FastRun.js";
import { startCircuitSimulation as oldStartCircuitSimulation } from "./temp/src/Renderer/Simulator/Simulator.js";
import { runFastSimulation as oldRunFastSimulation } from "./temp/src/Renderer/Simulator/Fast/FastRun.js";
import {
  FSharpList,
} from "./temp/fable_modules/fable-library.4.0.0-theta-018/List.js";

function tryLoadStateFromPath(filePath) {
  if (!existsSync(filePath)) {
    console.warn(
      `Can't read file from ${filePath} because it does not seem to exist!`,
    );
  }

  try {
    const content = readFileSync(filePath, "utf8");
    return JsonHelpers_jsonStringToState(content).fields[0];
  } catch (err) {
    console.warn(
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
    // console.error(canvas);
    return makeLoadedComponentFromCanvasData(
      canvas,
      filePath,
      JsonHelpers_SavedInfo__get_getTimeStamp(state),
      JsonHelpers_SavedInfo__get_getWaveInfo(state),
      JsonHelpers_SavedInfo__get_getSheetInfo(state),
    );
  } catch (err) {
    console.warn(
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
    console.warn(
      `Error reading Issie project directory at ${folerPath}: ${err}`,
    );
  }
  return files.map((file) => {
    // NOTE: fileNameIsBad checking is skipped
    const filePath = join(folderPath, file);
    console.error(`loading  ${filePath}`);
    const ldComp = tryLoadComponentFromPath(filePath);
    return ldComp;
  });
}

function extracContentOfDrivers(drivers) {
  return drivers.map((driver) => {
    if (driver == undefined || driver == null) return undefined;
    return {
      index: driver.Index,
      width: driver.DriverWidth,
      data: driver.DriverData.Step.map(
        (data) => data.Dat.fields[0],
      ),
    };
  }).filter((el) => el !== undefined);
}

function extracContentOfDriversFData(drivers) {
  return drivers.map((driver) => {
    if (driver == undefined || driver == null) return undefined;
    return {
      index: driver.Index,
      width: driver.DriverWidth,
      data: driver.DriverData.Step.map(
        (data) => data.fields[0].Dat.fields[0],
      ),
    };
  }).filter((el) => el !== undefined);
}

function drivers2String(drivers) {
  drivers.map((driver) =>
    `Driver [Index=${driver.index}, width=${driver.width}] : [${
      driver.data.join(", ")
    }]`
  );
}

const NewSimulator = {
  startCircuitSimulation: newStartCircuitSimulation,
  runFastSimulation: newRunFastSimulation,
  extractDriversContent: (FastSimulation) =>
    extracContentOfDrivers(FastSimulation.Drivers),
};

const OldSimulator = {
  startCircuitSimulation: oldStartCircuitSimulation,
  runFastSimulation: oldRunFastSimulation,
  extractDriversContent: (FastSimulation) =>
    extracContentOfDriversFData(FastSimulation.DriversFData),
};

export {
  drivers2String,
  FSharpList,
  loadAllComponentFiles,
  NewSimulator,
  OldSimulator,
};
