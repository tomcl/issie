import { find, item, tryFindIndex, length, singleton, cons, empty, tryFind, map, filter } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { toFail, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { SimulationComponent, ReducerOutput, SimulationComponentState, SimulationError } from "./SimulatorTypes.fs.js";
import { fold, FSharpMap__get_Count, map as map_1, empty as empty_1, ofList, FSharpMap__Add, FSharpMap__TryFind } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { Union } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { union_type, list_type, class_type, string_type } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { empty as empty_2, FSharpSet__Add, FSharpSet__Contains } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { runCanvasStateChecksAndBuildGraph } from "./Builder.fs.js";
import { equalsSafe, comparePrimitives } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { assertThat } from "../Common/Helpers.fs.js";
import { getSimulationIOsFromGraph, feedSimulationInput, diffReducerInputsOrOutputs, feedClockTick, extractSimulationIOs, extractIncompleteSimulationIOs } from "./Runner.fs.js";
import { OutputPortNumber, InputPortNumber } from "../Common/CommonTypes.fs.js";

function getComponentDependencies(state_0, state_1) {
    const state = [state_0, state_1];
    const components = state[0];
    let list_1;
    list_1 = filter((comp) => {
        if (comp.Type.tag === 16) {
            return true;
        }
        else {
            return false;
        }
    }, components);
    return map((comp_1) => {
        const matchValue_1 = comp_1.Type;
        if (matchValue_1.tag === 16) {
            const c = matchValue_1.fields[0];
            return c.Name;
        }
        else {
            throw (new Error("what? Impossible, getComponentDependency"));
        }
    }, list_1);
}

function getDependencyState(name, dependencies) {
    let clo1;
    let _arg1;
    _arg1 = tryFind((dep) => (dep.Name === name), dependencies);
    if (_arg1 == null) {
        return new FSharpResult$2(1, new SimulationError((clo1 = toText(printf("Could not resolve dependency: \"%s\". Make sure a dependency with such name exists in the current project.")), clo1(name)), void 0, empty(), empty()));
    }
    else {
        const dep_1 = _arg1;
        return new FSharpResult$2(0, dep_1.CanvasState);
    }
}

function buildDependencyGraph(componentName, state_0, state_1, dependencies, dependencyGraph) {
    const state = [state_0, state_1];
    const iterateChildren = (children_mut, dependencyGraph_1_mut) => {
        iterateChildren:
        while (true) {
            const children = children_mut, dependencyGraph_1 = dependencyGraph_1_mut;
            if (children.tail != null) {
                const children$0027 = children.tail;
                const child = children.head;
                const matchValue = FSharpMap__TryFind(dependencyGraph_1, child);
                if (matchValue == null) {
                    const matchValue_1 = getDependencyState(child, dependencies);
                    if (matchValue_1.tag === 0) {
                        const childState = matchValue_1.fields[0];
                        const matchValue_2 = buildDependencyGraph(child, childState[0], childState[1], dependencies, dependencyGraph_1);
                        if (matchValue_2.tag === 0) {
                            const dependencyGraph_2 = matchValue_2.fields[0];
                            children_mut = children$0027;
                            dependencyGraph_1_mut = dependencyGraph_2;
                            continue iterateChildren;
                        }
                        else {
                            const err_1 = matchValue_2.fields[0];
                            return new FSharpResult$2(1, err_1);
                        }
                    }
                    else {
                        const err = matchValue_1.fields[0];
                        return new FSharpResult$2(1, new SimulationError(err.Msg, componentName, err.ComponentsAffected, err.ConnectionsAffected));
                    }
                }
                else {
                    children_mut = children$0027;
                    dependencyGraph_1_mut = dependencyGraph_1;
                    continue iterateChildren;
                }
            }
            else {
                return new FSharpResult$2(0, dependencyGraph_1);
            }
            break;
        }
    };
    const children_1 = getComponentDependencies(state[0], state[1]);
    const dependencyGraph_3 = FSharpMap__Add(dependencyGraph, componentName, children_1);
    return iterateChildren(children_1, dependencyGraph_3);
}

class DfsType extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["NoCycle", "Backtracking", "Cycle"];
    }
}

function DfsType$reflection() {
    return union_type("DependencyMerger.DfsType", [], DfsType, () => [[["Item", class_type("Microsoft.FSharp.Collections.FSharpSet`1", [string_type])]], [["Item1", list_type(string_type)], ["Item2", string_type]], [["Item", list_type(string_type)]]]);
}

function checkDependencyCycle(currNode, depGraph, visited, currStack) {
    let matchValue_3, children_1, clo1;
    const exploreChildren = (visited_1_mut, currStack_1_mut, children_mut) => {
        exploreChildren:
        while (true) {
            const visited_1 = visited_1_mut, currStack_1 = currStack_1_mut, children = children_mut;
            if (children.tail != null) {
                const children$0027 = children.tail;
                const child = children.head;
                const matchValue = checkDependencyCycle(child, depGraph, visited_1, currStack_1);
                switch (matchValue.tag) {
                    case 1: {
                        const cycleEnd = matchValue.fields[1];
                        const cycle = matchValue.fields[0];
                        const matchValue_1 = cycleEnd === currNode;
                        if (matchValue_1) {
                            return new DfsType(2, cons(currNode, cycle));
                        }
                        else {
                            return new DfsType(1, cons(currNode, cycle), cycleEnd);
                        }
                    }
                    case 2: {
                        const cycle_1 = matchValue.fields[0];
                        return new DfsType(2, cycle_1);
                    }
                    default: {
                        const visited_2 = matchValue.fields[0];
                        visited_1_mut = visited_2;
                        currStack_1_mut = currStack_1;
                        children_mut = children$0027;
                        continue exploreChildren;
                    }
                }
            }
            else {
                return new DfsType(0, visited_1);
            }
            break;
        }
    };
    const matchValue_2 = [FSharpSet__Contains(currStack, currNode), FSharpSet__Contains(visited, currNode)];
    if (matchValue_2[0]) {
        if (matchValue_2[1]) {
            return new DfsType(1, singleton(currNode), currNode);
        }
        else {
            const clo1_1 = toFail(printf("what? Node never visited but in the stack, while detecting cycle: %s"));
            return clo1_1(currNode);
        }
    }
    else if (matchValue_2[1]) {
        return new DfsType(0, visited);
    }
    else {
        const visited_3 = FSharpSet__Add(visited, currNode);
        const currStack_2 = FSharpSet__Add(currStack, currNode);
        return exploreChildren(visited_3, currStack_2, (matchValue_3 = FSharpMap__TryFind(depGraph, currNode), (matchValue_3 != null) ? (children_1 = matchValue_3, children_1) : (clo1 = toFail(printf("what? Could not find dependency %s in cycle detection")), clo1(currNode))));
    }
}

function buildDependencyMap(loadedDependencies) {
    let dependenciesRes;
    dependenciesRes = map((dep) => {
        let tupledArg;
        return [dep.Name, (tupledArg = dep.CanvasState, runCanvasStateChecksAndBuildGraph(tupledArg[0], tupledArg[1], loadedDependencies))];
    }, loadedDependencies);
    const hasError = (tupledArg_1) => {
        const name = tupledArg_1[0];
        const res = tupledArg_1[1];
        if (res.tag === 0) {
            return false;
        }
        else {
            return true;
        }
    };
    const extractOk = (tupledArg_2) => {
        const name_1 = tupledArg_2[0];
        const res_1 = tupledArg_2[1];
        if (res_1.tag === 1) {
            const e = res_1.fields[0];
            const clo1 = toFail(printf("what? Dependency %s expected to be Ok, but has error %A"));
            const clo2 = clo1(name_1);
            return clo2(e);
        }
        else {
            const d = res_1.fields[0];
            return [name_1, d];
        }
    };
    const matchValue = tryFind(hasError, dependenciesRes);
    if (matchValue == null) {
        let arg0;
        let elements;
        elements = map(extractOk, dependenciesRes);
        arg0 = ofList(elements);
        return new FSharpResult$2(0, arg0);
    }
    else {
        const copyOfStruct = matchValue[1];
        if (copyOfStruct.tag === 1) {
            const err = copyOfStruct.fields[0];
            const name_2 = matchValue[0];
            return new FSharpResult$2(1, new SimulationError(err.Msg, name_2, empty(), empty()));
        }
        else {
            throw (new Error("what? Impossible case in buildDependencyMap"));
        }
    }
}

function checkDependenciesAndBuildMap(currDiagramName, state_0, state_1, dependencies) {
    let arg10_1, clo1_1;
    const state = [state_0, state_1];
    const prettyPrintCycle = (cycle) => {
        if (cycle.tail != null) {
            if (cycle.tail.tail == null) {
                const name = cycle.head;
                return ("\"" + name) + "\"";
            }
            else {
                const cycle$0027 = cycle.tail;
                const name_1 = cycle.head;
                return (("\"" + name_1) + "\" --\u003e ") + prettyPrintCycle(cycle$0027);
            }
        }
        else {
            return "";
        }
    };
    let matchValue;
    const dependencyGraph = empty_1();
    matchValue = buildDependencyGraph(currDiagramName, state[0], state[1], dependencies, dependencyGraph);
    if (matchValue.tag === 0) {
        const dependencyGraph_1 = matchValue.fields[0];
        const matchValue_1 = checkDependencyCycle(currDiagramName, dependencyGraph_1, empty_2({
            Compare: comparePrimitives,
        }), empty_2({
            Compare: comparePrimitives,
        }));
        switch (matchValue_1.tag) {
            case 2: {
                const cycle_1 = matchValue_1.fields[0];
                let msg;
                const clo1 = toText(printf("Cycle must have at least 2 dependencies: %A"));
                msg = clo1(cycle_1);
                const cond = length(cycle_1) >= 2;
                assertThat(cond, msg);
                return new FSharpResult$2(1, new SimulationError((arg10_1 = prettyPrintCycle(cycle_1), (clo1_1 = toText(printf("Found a cycle in dependencies: %s.")), clo1_1(arg10_1))), void 0, empty(), empty()));
            }
            case 0: {
                const depsUsed = matchValue_1.fields[0];
                let loadedDependencies;
                loadedDependencies = filter((dep) => FSharpSet__Contains(depsUsed, dep.Name), dependencies);
                return buildDependencyMap(loadedDependencies);
            }
            default: {
                throw (new Error("what? checkDependencyCycle finished while Backtracking"));
            }
        }
    }
    else {
        const err = matchValue.fields[0];
        return new FSharpResult$2(1, err);
    }
}

function labelToPortNumber(label, labels) {
    const matchValue = tryFindIndex((y) => (label === y), labels);
    if (matchValue != null) {
        const pNumber = matchValue | 0;
        return pNumber | 0;
    }
    else {
        const clo1 = toFail(printf("what? Label %s not present in %A"));
        const clo2 = clo1(label);
        return clo2(labels) | 0;
    }
}

function portNumberToLabel(_arg1, inputLabels) {
    const pNumber = _arg1.fields[0] | 0;
    assertThat(length(inputLabels) > pNumber, "portNumberToLabel");
    return item(pNumber, inputLabels);
}

function extractInputValuesAsMap(graph, graphInputs, inputLabels) {
    let elements;
    const list = extractIncompleteSimulationIOs(graphInputs, graph);
    elements = map((tupledArg) => {
        let arg0;
        const wireData = tupledArg[1];
        const compLabel = tupledArg[0][1].fields[0];
        return [(arg0 = (labelToPortNumber(compLabel, inputLabels) | 0), (new InputPortNumber(0, arg0))), wireData];
    }, list);
    return ofList(elements);
}

function extractOutputValuesAsMap(graph, graphOutputs, outputLabels) {
    let elements;
    const list = extractSimulationIOs(graphOutputs, graph);
    elements = map((tupledArg) => {
        let arg0;
        const wireData = tupledArg[1];
        const label = tupledArg[0][1].fields[0];
        return [(arg0 = (labelToPortNumber(label, outputLabels) | 0), (new OutputPortNumber(0, arg0))), wireData];
    }, list);
    return ofList(elements);
}

function assertConsistentCustomOutputs(outputs, oldOutputs) {
    let value;
    value = map_1((pNumber, _arg1) => {
        let msg;
        const clo1 = toText(printf("assertConsistentCustomOutputs, old %A, new %A"));
        const clo2 = clo1(oldOutputs);
        msg = clo2(outputs);
        let cond;
        const option = FSharpMap__TryFind(oldOutputs, pNumber);
        cond = (option != null);
        assertThat(cond, msg);
    }, outputs);
    void value;
}

function makeCustomReducer(custom, graphInputs, graphOutputs) {
    const inputLabels = map((tupledArg) => {
        const label = tupledArg[0];
        return label;
    }, custom.InputLabels);
    const outputLabels = map((tupledArg_1) => {
        const label_1 = tupledArg_1[0];
        return label_1;
    }, custom.OutputLabels);
    return (reducerInput) => {
        let graph_1;
        const matchValue = reducerInput.CustomSimulationGraph;
        if (matchValue != null) {
            const graph = matchValue;
            graph_1 = graph;
        }
        else {
            const clo1 = toFail(printf("what? CustomSimulationGraph should always be Some in Custom component: %s"));
            graph_1 = clo1(custom.Name);
        }
        const matchValue_1 = reducerInput.IsClockTick;
        if (matchValue_1.tag === 1) {
            const state_1 = matchValue_1.fields[0];
            let msg;
            const clo1_1 = toText(printf("Custom components should be stateles, but received state: %A"));
            msg = clo1_1(state_1);
            const cond = equalsSafe(state_1, new SimulationComponentState(0));
            assertThat(cond, msg);
            const graph_4 = feedClockTick(graph_1);
            const outputs_1 = extractOutputValuesAsMap(graph_4, graphOutputs, outputLabels);
            return new ReducerOutput(outputs_1, graph_4, new SimulationComponentState(0));
        }
        else {
            const inputs = reducerInput.Inputs;
            const matchValue_2 = FSharpMap__get_Count(inputs) === length(custom.InputLabels);
            if (matchValue_2) {
                const oldInputs = extractInputValuesAsMap(graph_1, graphInputs, inputLabels);
                const oldOutputs = extractOutputValuesAsMap(graph_1, graphOutputs, outputLabels);
                const diffedInputs = diffReducerInputsOrOutputs(inputs, oldInputs);
                let graph_3;
                graph_3 = fold((graph_2, inputPortNumber, wireData) => {
                    const inputLabel = portNumberToLabel(inputPortNumber, inputLabels);
                    let patternInput;
                    patternInput = find((tupledArg_2) => {
                        const inpLabel = tupledArg_2[1].fields[0];
                        return inpLabel === inputLabel;
                    }, graphInputs);
                    const inputId = patternInput[0];
                    return feedSimulationInput(graph_2, inputId, wireData);
                }, graph_1, diffedInputs);
                const outputs = extractOutputValuesAsMap(graph_3, graphOutputs, outputLabels);
                assertConsistentCustomOutputs(outputs, oldOutputs);
                const diffedOutputs = diffReducerInputsOrOutputs(outputs, oldOutputs);
                return new ReducerOutput(diffedOutputs, graph_3, new SimulationComponentState(0));
            }
            else {
                return new ReducerOutput(void 0, graph_1, new SimulationComponentState(0));
            }
        }
    };
}

function merger(currGraph, dependencyMap) {
    const currGraphCopy = currGraph;
    return fold((currGraph_1, compId, comp) => {
        const matchValue = comp.Type;
        if (matchValue.tag === 16) {
            const custom = matchValue.fields[0];
            let dependencyGraph_1;
            const matchValue_1 = FSharpMap__TryFind(dependencyMap, custom.Name);
            if (matchValue_1 != null) {
                const dependencyGraph = matchValue_1;
                dependencyGraph_1 = dependencyGraph;
            }
            else {
                const clo1 = toFail(printf("what? Could not find dependency %s in dependencyMap"));
                dependencyGraph_1 = clo1(custom.Name);
            }
            const dependencyGraph_2 = merger(dependencyGraph_1, dependencyMap);
            const patternInput = getSimulationIOsFromGraph(dependencyGraph_2);
            const graphOutputs = patternInput[1];
            const graphInputs = patternInput[0];
            let newComp;
            const Reducer = makeCustomReducer(custom, graphInputs, graphOutputs);
            newComp = (new SimulationComponent(comp.Id, comp.Type, comp.Label, comp.Inputs, comp.Outputs, comp.OutputsPropagated, dependencyGraph_2, comp.State, Reducer));
            return FSharpMap__Add(currGraph_1, compId, newComp);
        }
        else {
            return currGraph_1;
        }
    }, currGraph, currGraphCopy);
}

export function mergeDependencies(currDiagramName, graph, state_0, state_1, loadedDependencies) {
    const state = [state_0, state_1];
    const matchValue = checkDependenciesAndBuildMap(currDiagramName, state[0], state[1], loadedDependencies);
    if (matchValue.tag === 0) {
        const dependencyMap = matchValue.fields[0];
        const arg0 = merger(graph, dependencyMap);
        return new FSharpResult$2(0, arg0);
    }
    else {
        const e = matchValue.fields[0];
        return new FSharpResult$2(1, e);
    }
}

