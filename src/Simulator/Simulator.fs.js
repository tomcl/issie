import { runCanvasStateChecksAndBuildGraph } from "./Builder.fs.js";
import { mergeDependencies } from "./DependencyMerger.fs.js";
import { extractSimulationIOs as extractSimulationIOs_1, feedClockTick as feedClockTick_1, feedSimulationInput as feedSimulationInput_1, InitialiseGraphWithZeros, getSimulationIOs } from "./Runner.fs.js";
import { analyseSimulationGraph } from "./SimulationGraphAnalyser.fs.js";
import { FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { hasSynchronousComponents } from "./SynchronousUtils.fs.js";
import { NumberBase } from "../Common/CommonTypes.fs.js";
import { SimulationComponentState, SimulationData } from "./SimulatorTypes.fs.js";
import { toList } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { filter, map } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";

export function prepareSimulation(diagramName, canvasState_0, canvasState_1, loadedDependencies) {
    const canvasState = [canvasState_0, canvasState_1];
    const matchValue = runCanvasStateChecksAndBuildGraph(canvasState[0], canvasState[1], loadedDependencies);
    if (matchValue.tag === 0) {
        const graph = matchValue.fields[0];
        const matchValue_1 = mergeDependencies(diagramName, graph, canvasState[0], canvasState[1], loadedDependencies);
        if (matchValue_1.tag === 0) {
            const graph_1 = matchValue_1.fields[0];
            const connections = canvasState[1];
            const components = canvasState[0];
            const patternInput = getSimulationIOs(components);
            const outputs = patternInput[1];
            const inputs = patternInput[0];
            const matchValue_2 = analyseSimulationGraph(diagramName, graph_1, connections);
            if (matchValue_2 == null) {
                return new FSharpResult$2(0, new SimulationData((InitialiseGraphWithZeros(inputs, graph_1)), inputs, outputs, hasSynchronousComponents(graph_1), new NumberBase(0), 0));
            }
            else {
                const err_2 = matchValue_2;
                return new FSharpResult$2(1, err_2);
            }
        }
        else {
            const err_1 = matchValue_1.fields[0];
            return new FSharpResult$2(1, err_1);
        }
    }
    else {
        const err = matchValue.fields[0];
        return new FSharpResult$2(1, err);
    }
}

export const feedSimulationInput = (graph) => ((inputId) => ((wireData) => feedSimulationInput_1(graph, inputId, wireData)));

export const feedClockTick = feedClockTick_1;

export const extractSimulationIOs = (simulationIOs) => ((graph) => extractSimulationIOs_1(simulationIOs, graph));

export function extractStatefulComponents(graph) {
    let list_1;
    let list;
    list = toList(graph);
    list_1 = map((tuple) => tuple[1], list);
    return filter((comp) => (!equalsSafe(comp.State, new SimulationComponentState(0))), list_1);
}

