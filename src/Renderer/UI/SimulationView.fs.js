import { toString, Record } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { record_type, union_type, tuple_type, list_type, string_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { JSDiagramMsg$2, ComponentId, NumberBase, Connection$reflection, Component$reflection } from "../../Common/CommonTypes.fs.js";
import { Bit, SimulationData, SimulationError$reflection, SimulationData$reflection } from "../../Simulator/SimulatorTypes.fs.js";
import { FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { collect, map, cons, length, ofArray, singleton, filter, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { empty as empty_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { equals, equalArrays, createAtom } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { extractReducedState } from "../Interface/Extractor.fs.js";
import { extractSimulationIOs, extractStatefulComponents, feedClockTick, feedSimulationInput, prepareSimulation } from "../../Simulator/Simulator.fs.js";
import { Draw2dWrapper__GetCanvasState } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { Msg } from "./ModelType.fs.js";
import { right, item, left, Level_Option, level } from "../.fable/Fulma.2.9.0/Layouts/Level.fs.js";
import { assertThat, cropToLength } from "../../Common/Helpers.fs.js";
import { toConsole, toFail, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Option, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { simulationNumberStyle, simulationBitStyle } from "./Style.fs.js";
import { Color_IColor } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { convertIntToWireData, strToIntCheckWidth, convertWireDataToInt, bitToString } from "../../Simulator/NumberHelpers.fs.js";
import { baseSelector, openMemoryDiffViewer, viewFilledNum, viewNum } from "./MemoryEditorView.fs.js";
import { IInputType, input, Option as Option_1 } from "../.fable/Fulma.2.9.0/Elements/Form/Input.fs.js";
import { DOMAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { getTextEventValue } from "../Interface/JSHelpers.fs.js";
import { errorPropsNotification } from "./PopupView.fs.js";
import * as react from "react";
import { Option as Option_2, h5 } from "../.fable/Fulma.2.9.0/Elements/Heading.fs.js";
import { simTrace } from "../../Simulator/Runner.fs.js";

export class SimCache extends Record {
    constructor(Name, StoredState, StoredResult) {
        super();
        this.Name = Name;
        this.StoredState = StoredState;
        this.StoredResult = StoredResult;
    }
}

export function SimCache$reflection() {
    return record_type("SimulationView.SimCache", [], SimCache, () => [["Name", string_type], ["StoredState", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["StoredResult", union_type("Microsoft.FSharp.Core.FSharpResult`2", [SimulationData$reflection(), SimulationError$reflection()], FSharpResult$2, () => [[["ResultValue", SimulationData$reflection()]], [["ErrorValue", SimulationError$reflection()]]])]]);
}

export function simCacheInit(name) {
    return new SimCache(name, [empty(), empty()], new FSharpResult$2(0, new SimulationData(empty_1(), empty(), empty(), false, new NumberBase(0), 0)));
}

export const simCache = createAtom(simCacheInit(""));

export function prepareSimulationMemoised(diagramName_mut, canvasState_0_mut, canvasState_1_mut, loadedDependencies_mut) {
    prepareSimulationMemoised:
    while (true) {
        const diagramName = diagramName_mut, canvasState_0 = canvasState_0_mut, canvasState_1 = canvasState_1_mut, loadedDependencies = loadedDependencies_mut;
        const canvasState = [canvasState_0, canvasState_1];
        const rState = extractReducedState(canvasState[0], canvasState[1]);
        if (diagramName !== simCache().Name) {
            simCache(simCacheInit(diagramName), true);
            diagramName_mut = diagramName;
            canvasState_0_mut = canvasState[0];
            canvasState_1_mut = canvasState[1];
            loadedDependencies_mut = loadedDependencies;
            continue prepareSimulationMemoised;
        }
        else {
            const isSame = equalArrays(rState, simCache().StoredState);
            if (isSame) {
                return [simCache().StoredResult, rState];
            }
            else {
                const simResult = prepareSimulation(diagramName, rState[0], rState[1], loadedDependencies);
                simCache(new SimCache(diagramName, rState, simResult), true);
                return [simResult, rState];
            }
        }
        break;
    }
}

export function makeSimData(model) {
    const matchValue = [Draw2dWrapper__GetCanvasState(model.Diagram), model.CurrentProj];
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            const jsState = matchValue[0];
            const project = matchValue[1];
            let otherComponents;
            otherComponents = filter((comp) => (comp.Name !== project.OpenFileName), project.LoadedComponents);
            let arg0;
            const tupledArg = jsState;
            arg0 = prepareSimulationMemoised(project.OpenFileName, tupledArg[0], tupledArg[1], otherComponents);
            return arg0;
        }
        else {
            return void 0;
        }
    }
    else {
        return void 0;
    }
}

export function changeBase(dispatch, numBase) {
    return dispatch((new Msg(8, numBase)));
}

function splittedLine(leftContent, rightConent) {
    return level(singleton(new Level_Option(0, singleton(["style", {
        marginBottom: "10px",
    }]))), ofArray([left(empty(), singleton(item(empty(), singleton(leftContent)))), right(empty(), singleton(item(empty(), singleton(rightConent))))]));
}

function makeIOLabel(label, width) {
    const label_1 = cropToLength(15, true, label);
    if (width === 1) {
        return label_1;
    }
    else {
        const w = width | 0;
        const clo1 = toText(printf("%s (%d bits)"));
        const clo2 = clo1(label_1);
        return clo2(w);
    }
}

function viewSimulationInputs(numberBase, simulationGraph, inputs, dispatch) {
    const makeInputLine = (tupledArg) => {
        let s, s_2;
        const _arg1 = tupledArg[0];
        const wireData = tupledArg[1];
        const width = _arg1[2] | 0;
        const inputLabel = _arg1[1].fields[0];
        const inputId = _arg1[0].fields[0];
        let msg;
        const arg30 = length(wireData) | 0;
        const clo1 = toText(printf("Inconsistent wireData length in viewSimulationInput for %s: expcted %d but got %d"));
        const clo2 = clo1(inputLabel);
        const clo3 = clo2(width);
        msg = clo3(arg30);
        const cond = length(wireData) === width;
        assertThat(cond, msg);
        let valueHandle;
        if (wireData.tail != null) {
            if (wireData.tail.tail == null) {
                const bit = wireData.head;
                valueHandle = button(ofArray([new Option(16, singleton(simulationBitStyle)), new Option(0, new Color_IColor(4)), (bit.tag === 1) ? (new Option(0, new Color_IColor(4))) : (new Option(4)), new Option(9, false), new Option(17, (_arg1_1) => {
                    let arg0;
                    const newBit = (bit.tag === 1) ? (new Bit(0)) : (new Bit(1));
                    dispatch((arg0 = feedSimulationInput(simulationGraph)(new ComponentId(0, inputId))(singleton(newBit)), (new Msg(7, arg0))));
                })]), singleton((s = bitToString(bit), (s))));
            }
            else {
                const bits = wireData;
                const defValue = viewNum(numberBase)(convertWireDataToInt(bits));
                const options = ofArray([new Option_1(9, toString(numberBase)), new Option_1(10, defValue), new Option_1(15, ofArray([simulationNumberStyle, new DOMAttr(9, (arg) => {
                    let arg0_1;
                    let text;
                    text = getTextEventValue(arg);
                    const matchValue = strToIntCheckWidth(text, width);
                    if (matchValue.tag === 0) {
                        const num = matchValue.fields[0];
                        const bits_1 = convertIntToWireData(width, num);
                        dispatch(new Msg(29));
                        dispatch((arg0_1 = feedSimulationInput(simulationGraph)(new ComponentId(0, inputId))(bits_1), (new Msg(7, arg0_1))));
                    }
                    else {
                        const err = matchValue.fields[0];
                        const note = errorPropsNotification(err);
                        dispatch(new Msg(28, note));
                    }
                })]))]);
                valueHandle = input(cons(new Option_1(1, new IInputType(0)), options));
            }
        }
        else {
            throw (new Error("what? Empty wireData while creating a line in simulation inputs."));
        }
        return splittedLine((s_2 = makeIOLabel(inputLabel, width), (s_2)), valueHandle);
    };
    const children = map(makeInputLine, inputs);
    return react.createElement("div", {}, ...children);
}

function staticBitButton(bit) {
    let s;
    return button(ofArray([new Option(16, singleton(simulationBitStyle)), new Option(0, new Color_IColor(4)), (bit.tag === 1) ? (new Option(0, new Color_IColor(4))) : (new Option(4)), new Option(9, false), new Option(15, true)]), singleton((s = bitToString(bit), (s))));
}

function staticNumberBox(numBase, bits) {
    const width = length(bits) | 0;
    const value = viewFilledNum(width, numBase)(convertWireDataToInt(bits));
    return input(ofArray([new Option_1(1, new IInputType(0)), new Option_1(5, true), new Option_1(8, value), new Option_1(15, singleton(simulationNumberStyle))]));
}

function viewSimulationOutputs(numBase, simOutputs) {
    const makeOutputLine = (tupledArg) => {
        let bit, bits, s;
        const _arg1 = tupledArg[0];
        const wireData = tupledArg[1];
        const width = _arg1[2] | 0;
        const outputLabel = _arg1[1].fields[0];
        let msg;
        const arg30 = length(wireData) | 0;
        const clo1 = toText(printf("Inconsistent wireData length in viewSimulationOutput for %s: expcted %d but got %d"));
        const clo2 = clo1(outputLabel);
        const clo3 = clo2(width);
        msg = clo3(arg30);
        const cond = length(wireData) === width;
        assertThat(cond, msg);
        let valueHandle;
        if (wireData.tail != null) {
            valueHandle = ((wireData.tail.tail == null) ? (bit = wireData.head, staticBitButton(bit)) : (bits = wireData, staticNumberBox(numBase, bits)));
        }
        else {
            throw (new Error("what? Empty wireData while creating a line in simulation output."));
        }
        return splittedLine((s = makeIOLabel(outputLabel, width), (s)), valueHandle);
    };
    const children = map(makeOutputLine, simOutputs);
    return react.createElement("div", {}, ...children);
}

function viewStatefulComponents(comps, numBase, model, dispatch) {
    const getWithDefault = (_arg1) => {
        const lab = _arg1.fields[0];
        if (lab === "") {
            return "no-label";
        }
        else {
            return lab;
        }
    };
    const makeStateLine = (comp) => {
        const matchValue = comp.State;
        switch (matchValue.tag) {
            case 2: {
                const bits = matchValue.fields[0];
                let label_1;
                const arg10_1 = getWithDefault(comp.Label);
                const arg20 = length(bits) | 0;
                const clo1_1 = toText(printf("Register: %s (%d bits)"));
                const clo2 = clo1_1(arg10_1);
                label_1 = clo2(arg20);
                return singleton(splittedLine(label_1, staticNumberBox(numBase, bits)));
            }
            case 3: {
                const mem = matchValue.fields[0];
                let label_2;
                const arg10_2 = getWithDefault(comp.Label);
                const clo1_2 = toText(printf("RAM: %s"));
                label_2 = clo1_2(arg10_2);
                const initialMem = (compType) => {
                    if (compType.tag === 25) {
                        const m = compType.fields[0];
                        return m;
                    }
                    else {
                        const clo1_3 = toFail(printf("what? viewStatefulComponents expected RAM component but got: %A"));
                        return clo1_3(compType);
                    }
                };
                const viewDiffBtn = button(ofArray([new Option(16, singleton(simulationBitStyle)), new Option(0, new Color_IColor(4)), new Option(17, (_arg1_1) => {
                    openMemoryDiffViewer(initialMem(comp.Type), mem, model, dispatch);
                })]), singleton("View"));
                return singleton(splittedLine(label_2, viewDiffBtn));
            }
            case 0: {
                return empty();
            }
            default: {
                const bit = matchValue.fields[0];
                let label;
                const arg10 = getWithDefault(comp.Label);
                const clo1 = toText(printf("DFF: %s"));
                label = clo1(arg10);
                return singleton(splittedLine(label, staticBitButton(bit)));
            }
        }
    };
    const children = collect(makeStateLine, comps);
    return react.createElement("div", {}, ...children);
}

export function viewSimulationError(simError) {
    let s_3;
    let error;
    const matchValue = simError.InDependency;
    if (matchValue != null) {
        const dep = matchValue;
        const children_2 = [(s_3 = (("Error found in dependency \"" + dep) + "\":"), (s_3)), react.createElement("br", {}), simError.Msg, react.createElement("br", {}), ("Please fix the error in the dependency and retry.")];
        error = react.createElement("div", {}, ...children_2);
    }
    else {
        const children = [simError.Msg, react.createElement("br", {}), ("Please fix the error and retry.")];
        error = react.createElement("div", {}, ...children);
    }
    const children_4 = [h5(singleton(new Option_2(9, singleton(["style", {
        marginTop: "15px",
    }]))))(singleton("Errors")), error];
    return react.createElement("div", {}, ...children_4);
}

function viewSimulationData(simData, model, dispatch) {
    let s, clo1, simOutputs;
    let hasMultiBitOutputs;
    let value;
    let list_1;
    list_1 = filter((tupledArg) => {
        const w = tupledArg[2] | 0;
        return w > 1;
    }, simData.Outputs);
    value = (list_1.tail == null);
    hasMultiBitOutputs = (!value);
    const maybeBaseSelector = hasMultiBitOutputs ? baseSelector(simData.NumberBase, (numBase) => {
        changeBase(dispatch, numBase);
    }) : react.createElement("div", {});
    const maybeClockTickBtn = simData.IsSynchronous ? button(ofArray([new Option(0, new Color_IColor(6)), new Option(17, (_arg3) => {
        let arg0;
        if (!equals(simTrace(), void 0)) {
            toConsole(printf("*********************Incrementing clock from simulator button******************************"));
            toConsole(printf("-------------------------------------------------------------------------------------------"));
        }
        dispatch((arg0 = feedClockTick(simData.Graph), (new Msg(7, arg0))));
        if (!equals(simTrace(), void 0)) {
            toConsole(printf("-------------------------------------------------------------------------------------------"));
            toConsole(printf("*******************************************************************************************"));
        }
        dispatch(new Msg(9));
    })]), singleton((s = (clo1 = toText(printf("Clock Tick %d")), clo1(simData.ClockTickNumber)), (s)))) : react.createElement("div", {});
    let maybeStatefulComponents;
    const stateful = extractStatefulComponents(simData.Graph);
    const matchValue_1 = stateful.tail == null;
    if (matchValue_1) {
        maybeStatefulComponents = react.createElement("div", {});
    }
    else {
        const children_6 = [h5(singleton(new Option_2(9, singleton(["style", {
            marginTop: "15px",
        }]))))(singleton("Stateful components")), viewStatefulComponents(extractStatefulComponents(simData.Graph), simData.NumberBase, model, dispatch)];
        maybeStatefulComponents = react.createElement("div", {}, ...children_6);
    }
    const children_8 = [splittedLine(maybeBaseSelector, maybeClockTickBtn), h5(singleton(new Option_2(9, singleton(["style", {
        marginTop: "15px",
    }]))))(singleton("Inputs")), viewSimulationInputs(simData.NumberBase, simData.Graph, extractSimulationIOs(simData.Inputs)(simData.Graph), dispatch), h5(singleton(new Option_2(9, singleton(["style", {
        marginTop: "15px",
    }]))))(singleton("Outputs")), (simOutputs = extractSimulationIOs(simData.Outputs)(simData.Graph), (viewSimulationOutputs(simData.NumberBase, simOutputs))), maybeStatefulComponents];
    return react.createElement("div", {}, ...children_8);
}

export function viewSimulation(model, dispatch) {
    let copyOfStruct_1, copyOfStruct_2, s_1;
    const JSState = Draw2dWrapper__GetCanvasState(model.Diagram);
    const startSimulation = () => {
        let arg0_1, _arg1, tupledArg, copyOfStruct, simError, state_1, tupledArg_1, simData, state;
        const matchValue = [JSState, model.CurrentProj];
        if (matchValue[0] != null) {
            if (matchValue[1] != null) {
                const jsState = matchValue[0];
                const project = matchValue[1];
                let otherComponents;
                otherComponents = filter((comp) => (comp.Name !== project.OpenFileName), project.LoadedComponents);
                dispatch((arg0_1 = (_arg1 = (tupledArg = jsState, (prepareSimulationMemoised(project.OpenFileName, tupledArg[0], tupledArg[1], otherComponents))), ((copyOfStruct = _arg1[0], (copyOfStruct.tag === 1) ? (simError = copyOfStruct.fields[0], state_1 = _arg1[1], ((simError.InDependency == null) ? dispatch((tupledArg_1 = [simError.ComponentsAffected, simError.ConnectionsAffected], (new Msg(13, tupledArg_1[0], tupledArg_1[1])))) : (void 0), new FSharpResult$2(1, simError))) : (simData = copyOfStruct.fields[0], state = _arg1[1], new FSharpResult$2(0, simData))))), (new Msg(2, arg0_1))));
            }
            else {
                throw (new Error("what? Cannot start a simulation without a project"));
            }
        }
    };
    const matchValue_1 = model.CurrentStepSimulationStep;
    if (matchValue_1 != null) {
        const sim = matchValue_1;
        let body;
        if (sim.tag === 0) {
            const simData_1 = sim.fields[0];
            body = viewSimulationData(simData_1, model, dispatch);
        }
        else {
            const simError_1 = sim.fields[0];
            body = viewSimulationError(simError_1);
        }
        const endSimulation = (_arg2) => {
            let arg0_2;
            dispatch(new Msg(29));
            dispatch(new Msg(13, empty(), empty()));
            dispatch(new Msg(10));
            dispatch((arg0_2 = (new JSDiagramMsg$2(3, void 0)), new Msg(0, arg0_2)));
        };
        const children_2 = [button(ofArray([new Option(0, new Color_IColor(8)), new Option(17, endSimulation)]), singleton("End simulation")), react.createElement("br", {}), react.createElement("br", {}), "The simulation uses the diagram as it was at the moment of\r\n                 pressing the \"Start simulation\" button.", react.createElement("hr", {}), body];
        return react.createElement("div", {}, ...children_2);
    }
    else {
        const simRes = makeSimData(model);
        const isSync = (simRes != null) ? (copyOfStruct_1 = simRes[0], (copyOfStruct_1.tag === 0) ? (copyOfStruct_1.fields[0].IsSynchronous ? false : false) : false) : false;
        const patternInput = (simRes != null) ? (copyOfStruct_2 = simRes[0], (copyOfStruct_2.tag === 1) ? [new Color_IColor(7), "See Problems"] : [new Color_IColor(6), "Start Simulation"]) : [new Color_IColor(3), ""];
        const buttonText = patternInput[1];
        const buttonColor = patternInput[0];
        const children = ["Simulate simple logic using this tab.", react.createElement("br", {}), (s_1 = (isSync ? "You can also use the Waveforms \u003e\u003e button to view waveforms" : ""), s_1), react.createElement("br", {}), react.createElement("br", {}), button(ofArray([new Option(0, buttonColor), new Option(17, (_arg1_1) => {
            startSimulation();
        })]), singleton(buttonText))];
        return react.createElement("div", {}, ...children);
    }
}

