import { existsSync, readdirSync, readFileSync } from "node:fs";
import { basename, dirname, extname, join } from "node:path";
import { performance } from "node:perf_hooks";
import { createLogger, format, transports } from "winston";
import {
  JsonHelpers_jsonStringToState,
  JsonHelpers_SavedInfo__get_getSheetInfo,
  JsonHelpers_SavedInfo__get_getTimeStamp,
  JsonHelpers_SavedInfo__get_getWaveInfo,
} from "./temp/src/Renderer/Common/Helpers.js";
import { parseDiagramSignature } from "./temp/src/Renderer/NewSimulator/Extractor.js";
import { CCForm, LoadedComponent } from "./temp/src/Renderer/Common/CommonTypes.js";
import { FSharpList } from "./temp/fable_modules/fable-library.4.0.0-theta-018/List.js";
import { length as seqLength } from "./temp/fable_modules/fable-library.4.0.0-theta-018/Seq.js";
import { SimulationError } from "./temp/src/Renderer/NewSimulator/SimulatorTypes.js";
// New Simulator
import { startCircuitSimulation as newStartCircuitSimulation } from "./temp/src/Renderer/NewSimulator/Simulator.js";
import { runFastSimulation as newRunFastSimulation } from "./temp/src/Renderer/NewSimulator/Fast/FastRun.js";
import { emptyFastSimulation as newEmptyFastSimulation } from "./temp/src/Renderer/NewSimulator/Fast/FastCreate.js";
// Old Simulator
import { startCircuitSimulation as oldStartCircuitSimulation } from "./temp/src/Renderer/Simulator/Simulator.js";
import { runFastSimulation as oldRunFastSimulation } from "./temp/src/Renderer/Simulator/Fast/FastRun.js";
import { emptyFastSimulation as oldEmptyFastSimulation } from "./temp/src/Renderer/Simulator/Fast/FastCreate.js";

const toPrecision = (num) => num.toPrecision(3);

// Get the number of components involved in a FastSimulation
const getNumComponents = (fastSim) => {
  return seqLength(fastSim.FComps.values());
};

const transportList = {
  file: new transports.File({
    level: "info",
    format: format.json(),
    filename: "combined.log",
    options: { flags: "w" },
  }),
  console: new transports.Console({
    level: "verbose",
    format: format.combine(
      format.colorize(),
      format.splat(),
      format.simple(),
      format.timestamp(),
      format.printf(({ level, message, label, timestamp }) => {
        return `${timestamp} [${`${label}`.padEnd(15, " ")}] ${level}: ${message}`;
      }),
    ),
  }),
};

const logger = createLogger({
  transports: [transportList.file, transportList.console],
});

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
      `Can't load component ${getBaseNameNoExtension(
        filePath,
      )} because of Error: ${err}`,
    );
  }
}

function loadAllComponentFiles(folderPath) {
  let files = [];
  try {
    files = readdirSync(folderPath).filter((file) => extname(file) === ".dgm");
  } catch (err) {
    console.error(`Error reading Issie project directory at ${folderPath}: ${err}`);
  }
  return files.map((file) => {
    // NOTE: fileNameIsBad checking is skipped
    const filePath = join(folderPath, file);
    logger.verbose(`loading  ${filePath}`);
    const ldComp = tryLoadComponentFromPath(filePath);
    return ldComp;
  });
}

function extracContentOfDrivers(drivers) {
  return drivers
    .map((driver) => {
      if (driver == undefined || driver == null) return undefined;
      return {
        index: driver.Index,
        width: driver.DriverWidth,
        data: driver.DriverData.Step.map((data) => data.Dat.fields[0]),
      };
    })
    .filter((el) => el !== undefined);
}

function extracContentOfDriversFData(drivers) {
  return drivers
    .map((driver) => {
      if (driver == undefined || driver == null) return undefined;
      return {
        index: driver.Index,
        width: driver.DriverWidth,
        data: driver.DriverData.Step.map((data) => data.fields[0].Dat.fields[0]),
      };
    })
    .filter((el) => el !== undefined);
}

function drivers2String(drivers) {
  return drivers.map(
    (driver) =>
      `Driver [Index=${driver.index}, width=${driver.width}] : [${driver.data.join(
        ", ",
      )}]`,
  );
}

const NewSimulator = {
  type: "new",
  startCircuitSimulation: newStartCircuitSimulation,
  runFastSimulation: newRunFastSimulation,
  emptyFastSimulation: () => newEmptyFastSimulation(),
  extractDriversContent: (FastSimulation) =>
    extracContentOfDrivers(FastSimulation.Drivers),
};

const OldSimulator = {
  type: "old",
  startCircuitSimulation: oldStartCircuitSimulation,
  runFastSimulation: oldRunFastSimulation,
  emptyFastSimulation: () => oldEmptyFastSimulation(),
  extractDriversContent: (FastSimulation) =>
    extracContentOfDriversFData(FastSimulation.DriversFData),
};

function simulationFactory(
  loadedComponents,
  timeOut,
  lastStepNeeded,
  simulationArraySize,
  detail,
  warmupIterations = 20,
  testIterations = 100,
) {
  return (simulator, topComp) => {
    const canvasState = topComp.CanvasState;
    const diagramName = topComp.Name;
    const components = canvasState[0];
    const connections = canvasState[1];

    let time = 0;

    logger.verbose({
      label: `${simulator.type} simulator`,
      message: `Run simulation for ${diagramName}`,
    });

    const simData = simulator.startCircuitSimulation(
      simulationArraySize,
      diagramName,
      components,
      connections,
      loadedComponents,
    ).fields[0];
    if (simData instanceof SimulationError) {
      logger.error(simData);
      return;
    }
    const fs = simData.FastSim; // FastSimulation
    // warm up the JIT compiler
    for (let i = 0; i < warmupIterations; i++) {
      const cloned = { ...fs }; // deep clone of fs
      simulator.runFastSimulation(timeOut, lastStepNeeded, cloned);
    }
    // run the actual test
    for (let i = 0; i < testIterations; i++) {
      const cloned = { ...fs }; // deep clone of fs
      const t0 = performance.now();
      simulator.runFastSimulation(timeOut, lastStepNeeded, cloned);
      const t1 = performance.now();
      time += t1 - t0;
    }
    const numComps = getNumComponents(fs);
    time /= testIterations;
    logger.info({
      label: `${simulator.type} simulator`,
      message: `runFastSimulation() took ${toPrecision(time)} milliseconds.`,
      detail: {
        ...detail,
        testcase: diagramName,
        simulator: simulator.type,
        execTime: time,
        numComps: numComps,
      },
    });
    return {
      result: simulator.extractDriversContent(fs),
      time: time,
      numComps: numComps,
    };
  };
}

export {
  drivers2String,
  FSharpList,
  getNumComponents,
  loadAllComponentFiles,
  logger,
  NewSimulator,
  OldSimulator,
  simulationFactory,
  toPrecision,
  transportList,
};
