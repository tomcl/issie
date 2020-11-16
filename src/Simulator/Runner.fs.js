import { uncurry, comparePrimitives, equalsSafe, structuralHash, equals, createAtom } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { assertThat } from "../Common/Helpers.fs.js";
import { ofList, toList, filter, add, toArray, tryFind, FSharpMap__Add, FSharpMap__TryFind, fold, empty, FSharpMap__get_Count } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { toFail, toConsole, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { replicate as replicate_1, singleton, filter as filter_1, map, empty as empty_1, cons, fold as fold_1, contains } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { Bit, OutputChange, SimulationComponent, ReducerInput, IsClockTick, shortPSComp } from "./SimulatorTypes.fs.js";
import { map as map_1, defaultArg } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { ComponentLabel, ComponentId, InputPortNumber, ComponentType, OutputPortNumber } from "../Common/CommonTypes.fs.js";
import { couldBeSynchronousComponent } from "./SynchronousUtils.fs.js";
import { replicate, fold as fold_2, sortBy } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { convertIntToWireData } from "./NumberHelpers.fs.js";
import { fromInteger } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";

export const simTrace = createAtom(void 0);

export function diffReducerInputsOrOutputs(newIO, oldIO) {
    let clo1, clo2;
    assertThat(FSharpMap__get_Count(oldIO) <= FSharpMap__get_Count(newIO), (clo1 = toText(printf("diffReducerInputsOrOutputs: (%A:%A)")), clo2 = clo1(oldIO), clo2(newIO)));
    const state = empty();
    return fold((diff, portNumber, wireData) => {
        let oldData, oldData_2;
        const matchValue = FSharpMap__TryFind(oldIO, portNumber);
        if (matchValue != null) {
            if (oldData = matchValue, !equals(oldData, wireData)) {
                const oldData_1 = matchValue;
                return FSharpMap__Add(diff, portNumber, wireData);
            }
            else {
                let pattern_matching_result;
                if (matchValue != null) {
                    if (oldData_2 = matchValue, equals(oldData_2, wireData)) {
                        pattern_matching_result = 0;
                    }
                    else {
                        pattern_matching_result = 1;
                    }
                }
                else {
                    pattern_matching_result = 1;
                }
                switch (pattern_matching_result) {
                    case 0: {
                        return diff;
                    }
                    case 1: {
                        throw (new Error("what? Impossible case in diffReducerInputsOrOutputs"));
                    }
                }
            }
        }
        else {
            return FSharpMap__Add(diff, portNumber, wireData);
        }
    }, state, newIO);
}

export function traceReduction(action, comp, reducerInput, reducerOutput) {
    let traceLabs, lab;
    const matchValue = [simTrace(), comp];
    if (matchValue[0] != null) {
        if (traceLabs = matchValue[0], (lab = matchValue[1].Label.fields[0], contains(lab, traceLabs, {
            Equals: (x, y) => (x === y),
            GetHashCode: structuralHash,
        }))) {
            const lab_1 = matchValue[1].Label.fields[0];
            const traceLabs_1 = matchValue[0];
            const arg20 = shortPSComp(comp);
            const clo1 = toConsole(printf("\n%s\u003e\u003e\u003e\u003e\u003e %A \n\toutputs: %A \n\tinputs=%A \n\tnewState=%A"));
            const clo2 = clo1(action);
            const clo3 = clo2(arg20);
            const clo4 = clo3(reducerOutput.Outputs);
            const clo5 = clo4(reducerInput.Inputs);
            clo5(reducerOutput.NewState);
        }
    }
}

function feedInput(graph, compId, input_0, input_1) {
    const input = [input_0, input_1];
    let comp;
    const matchValue = FSharpMap__TryFind(graph, compId);
    if (matchValue != null) {
        const c = matchValue;
        comp = c;
    }
    else {
        const clo1 = toFail(printf("what? Could not find component %A in simulationStep"));
        comp = clo1(compId);
    }
    const oldReducerInput = new ReducerInput(comp.Inputs, comp.CustomSimulationGraph, new IsClockTick(0));
    const oldReducerOutput = comp.Reducer(oldReducerInput);
    let comp_1;
    let Inputs;
    const tupledArg = input;
    Inputs = FSharpMap__Add(comp.Inputs, tupledArg[0], tupledArg[1]);
    comp_1 = (new SimulationComponent(comp.Id, comp.Type, comp.Label, Inputs, comp.Outputs, comp.OutputsPropagated, comp.CustomSimulationGraph, comp.State, comp.Reducer));
    const graph_1 = FSharpMap__Add(graph, comp_1.Id, comp_1);
    const reducerInput = new ReducerInput(comp_1.Inputs, comp_1.CustomSimulationGraph, new IsClockTick(0));
    const reducerOutput = comp_1.Reducer(reducerInput);
    traceReduction("comb-feedin", comp_1, reducerInput, reducerOutput);
    const matchValue_1 = reducerOutput.Outputs;
    if (matchValue_1 != null) {
        const outputMap = matchValue_1;
        const oldOutputMap = defaultArg(oldReducerOutput.Outputs, empty());
        const diffedOutputMap = diffReducerInputsOrOutputs(outputMap, oldOutputMap);
        const graph_2 = feedReducerOutput(comp_1, graph_1, diffedOutputMap);
        const comp_2 = new SimulationComponent(comp_1.Id, comp_1.Type, comp_1.Label, comp_1.Inputs, comp_1.Outputs, comp_1.OutputsPropagated, reducerOutput.NewCustomSimulationGraph, comp_1.State, comp_1.Reducer);
        return FSharpMap__Add(graph_2, comp_2.Id, comp_2);
    }
    else {
        return graph_1;
    }
}

function feedReducerOutput(comp, graph, outputMap) {
    return fold((graph_1, _arg1, wireData) => {
        const outPortNumber = _arg1;
        const opNum = outPortNumber.fields[0] | 0;
        const matchValue = tryFind(new OutputPortNumber(0, opNum), comp.Outputs);
        let pattern_matching_result;
        if (matchValue == null) {
            if (equalsSafe(comp.Type, new ComponentType(2))) {
                pattern_matching_result = 0;
            }
            else {
                pattern_matching_result = 1;
            }
        }
        else {
            pattern_matching_result = 1;
        }
        switch (pattern_matching_result) {
            case 0: {
                return graph_1;
            }
            case 1: {
                if (matchValue != null) {
                    const targets = matchValue;
                    comp.OutputsPropagated[opNum] = true;
                    return fold_1((graph_2, tupledArg) => {
                        const nextCompId = tupledArg[0];
                        const nextPortNumber = tupledArg[1];
                        const graph_3 = feedInput(graph_2, nextCompId, nextPortNumber, wireData);
                        return graph_3;
                    }, graph_1, targets);
                }
                else {
                    const clo1 = toFail(printf("what? Reducer produced inexistent output portNumber %A in component %A"));
                    const clo2 = clo1(outPortNumber);
                    return clo2(comp);
                }
            }
        }
    }, graph, outputMap);
}

export function clockedComps(graph) {
    let array_1;
    let array;
    array = toArray(graph);
    array_1 = array.filter((tupledArg) => {
        const comp = tupledArg[1];
        return couldBeSynchronousComponent(comp.Type);
    });
    return sortBy((tupledArg_1) => {
        const cid = tupledArg_1[0];
        const comp_1 = tupledArg_1[1];
        if (comp_1.Type.tag === 16) {
            return 0;
        }
        else {
            return 1;
        }
    }, array_1, {
        Compare: comparePrimitives,
    });
}

export function calculateStateChanges(graph) {
    const clockedCompsBeforeTick = clockedComps(graph);
    return fold_2(uncurry(2, (tupledArg) => {
        const graph_1 = tupledArg[0];
        const changes = tupledArg[1];
        return (tupledArg_1) => {
            let clo1;
            const compId = tupledArg_1[0];
            const comp = tupledArg_1[1];
            const reducerInput = new ReducerInput(comp.Inputs, comp.CustomSimulationGraph, new IsClockTick(1, comp.State));
            const reducerTickOutput = comp.Reducer(reducerInput);
            traceReduction((clo1 = toText(printf("clockTick %A")), clo1(reducerInput.IsClockTick)), comp, reducerInput, reducerTickOutput);
            const matchValue = reducerTickOutput.Outputs;
            if (matchValue != null) {
                const outputMap = matchValue;
                let comp_1;
                const OutputsPropagated = replicate(FSharpMap__get_Count(comp.Outputs), false);
                comp_1 = (new SimulationComponent(comp.Id, comp.Type, comp.Label, comp.Inputs, comp.Outputs, OutputsPropagated, reducerTickOutput.NewCustomSimulationGraph, reducerTickOutput.NewState, comp.Reducer));
                const change = new OutputChange(comp_1, outputMap);
                return [add(comp_1.Id, comp_1, graph_1), cons(change, changes)];
            }
            else {
                const clo1_1 = toFail(printf("what? A clocked component should ALWAYS produce outputs after a clock tick: %A"));
                return clo1_1(comp);
            }
        };
    }), [graph, empty_1()], clockedCompsBeforeTick);
}

export function propagateStateChanges(graph, changes) {
    return fold_1((graph_1, change) => {
        const comp = change.CComp;
        let outputMap;
        outputMap = filter((_arg1, wData) => {
            const n = _arg1.fields[0] | 0;
            const value = comp.OutputsPropagated[n];
            return !value;
        }, change.COutputs);
        if (!equals(simTrace(), void 0)) {
            const clo1 = toConsole(printf("|prop|----\u003e %A (%A)"));
            const clo2 = clo1(comp.Label);
            clo2(outputMap);
        }
        const graph_2 = feedReducerOutput(comp, graph_1, outputMap);
        return graph_2;
    }, graph, changes);
}

export function feedClockTick(graph) {
    const tupledArg = calculateStateChanges(graph);
    return propagateStateChanges(tupledArg[0], tupledArg[1]);
}

export function feedSimulationInput(graph, inputId, wireData) {
    return feedInput(graph, inputId, new InputPortNumber(0, 0), wireData);
}

export function feedSimulationConstants(graph) {
    let comps;
    let list;
    list = toList(graph);
    comps = map((tuple) => tuple[1], list);
    const getWireData = (comp) => {
        const matchValue = comp.Type;
        if (matchValue.tag === 4) {
            const w = matchValue.fields[0] | 0;
            const c = matchValue.fields[1] | 0;
            return convertIntToWireData(w, fromInteger(c, false, 2));
        }
        else {
            return toFail(printf("What? Problem with non-constant component used in feedSimulationConstants"));
        }
    };
    let cL;
    cL = filter_1((c_1) => {
        const matchValue_1 = c_1.Type;
        switch (matchValue_1.tag) {
            case 16: {
                const cComp = matchValue_1.fields[0];
                return true;
            }
            case 4: {
                return true;
            }
            default: {
                return false;
            }
        }
    }, comps);
    const feedConstant = (graph_1, comp_1) => {
        const matchValue_2 = comp_1.Type;
        switch (matchValue_2.tag) {
            case 4: {
                return feedReducerOutput(comp_1, graph_1, ofList(singleton([new OutputPortNumber(0, 0), getWireData(comp_1)])));
            }
            case 16: {
                const cComp_1 = matchValue_2.fields[0];
                let comp$0027;
                const graphOpt = map_1(feedSimulationConstants, comp_1.CustomSimulationGraph);
                comp$0027 = (new SimulationComponent(comp_1.Id, comp_1.Type, comp_1.Label, comp_1.Inputs, comp_1.Outputs, comp_1.OutputsPropagated, graphOpt, comp_1.State, comp_1.Reducer));
                return add(comp_1.Id, comp$0027, graph_1);
            }
            default: {
                return toFail(printf("What? other components are filtered out"));
            }
        }
    };
    return fold_1(feedConstant, graph, cL);
}

export function InitialiseGraphWithZeros(inputIds, graph) {
    const graph_1 = feedClockTick(graph);
    let graph_3;
    graph_3 = fold_1((graph_2, tupledArg) => {
        const inputId = tupledArg[0];
        const width = tupledArg[2] | 0;
        const data = replicate_1(width, new Bit(0));
        return feedSimulationInput(graph_2, inputId, data);
    }, graph_1, inputIds);
    return feedSimulationConstants(graph_3);
}

export function extractSimulationIOs(simulationIOs, graph) {
    const extractWireData = (inputs) => {
        let matchValue;
        matchValue = FSharpMap__TryFind(inputs, new InputPortNumber(0, 0));
        if (matchValue != null) {
            const bit = matchValue;
            return bit;
        }
        else {
            throw (new Error("what? IO bit not set"));
        }
    };
    return fold_1((result, tupledArg) => {
        const ioId = tupledArg[0];
        const ioLabel = tupledArg[1];
        const width = tupledArg[2] | 0;
        const matchValue_1 = FSharpMap__TryFind(graph, ioId);
        if (matchValue_1 != null) {
            const comp = matchValue_1;
            return cons([[ioId, ioLabel, width], extractWireData(comp.Inputs)], result);
        }
        else {
            const tupledArg_1 = [ioId, ioLabel];
            const clo1 = toFail(printf("what? Could not find io node: %A"));
            return clo1([tupledArg_1[0], tupledArg_1[1]]);
        }
    }, empty_1(), simulationIOs);
}

export function extractIncompleteSimulationIOs(simulationIOs, graph) {
    const extractWireData = (inputs) => FSharpMap__TryFind(inputs, new InputPortNumber(0, 0));
    return fold_1((result, tupledArg) => {
        const ioId = tupledArg[0];
        const ioLabel = tupledArg[1];
        const width = tupledArg[2] | 0;
        const matchValue = FSharpMap__TryFind(graph, ioId);
        if (matchValue != null) {
            const comp = matchValue;
            const matchValue_1 = extractWireData(comp.Inputs);
            if (matchValue_1 != null) {
                const wireData = matchValue_1;
                return cons([[ioId, ioLabel, width], wireData], result);
            }
            else {
                return result;
            }
        }
        else {
            const tupledArg_1 = [ioId, ioLabel, width];
            const clo1 = toFail(printf("what? Could not find io node: %A"));
            return clo1([tupledArg_1[0], tupledArg_1[1], tupledArg_1[2]]);
        }
    }, empty_1(), simulationIOs);
}

export function getSimulationIOs(components) {
    return fold_1(uncurry(2, (tupledArg) => {
        const inputs = tupledArg[0];
        const outputs = tupledArg[1];
        return (comp) => {
            const matchValue = comp.Type;
            switch (matchValue.tag) {
                case 0: {
                    const w = matchValue.fields[0] | 0;
                    return [cons([new ComponentId(0, comp.Id), new ComponentLabel(0, comp.Label), w], inputs), outputs];
                }
                case 1: {
                    const w_1 = matchValue.fields[0] | 0;
                    return [inputs, cons([new ComponentId(0, comp.Id), new ComponentLabel(0, comp.Label), w_1], outputs)];
                }
                default: {
                    return [inputs, outputs];
                }
            }
        };
    }), [empty_1(), empty_1()], components);
}

export function getSimulationIOsFromGraph(graph) {
    return fold(uncurry(3, (tupledArg) => {
        const inputs = tupledArg[0];
        const outputs = tupledArg[1];
        return (compId) => ((comp) => {
            const matchValue = comp.Type;
            switch (matchValue.tag) {
                case 0: {
                    const w = matchValue.fields[0] | 0;
                    return [cons([comp.Id, comp.Label, w], inputs), outputs];
                }
                case 1: {
                    const w_1 = matchValue.fields[0] | 0;
                    return [inputs, cons([comp.Id, comp.Label, w_1], outputs)];
                }
                default: {
                    return [inputs, outputs];
                }
            }
        });
    }), [empty_1(), empty_1()], graph);
}

