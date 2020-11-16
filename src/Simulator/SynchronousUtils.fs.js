import { FSharpMap__ContainsKey, FSharpMap__Add, toList, empty, filter, FSharpMap__TryFind, tryPick, map } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { some } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { printf, toFail } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { empty as empty_2, map as map_1, collect, cons, contains, tryFindIndex } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { comparePrimitives, compareArrays, hashSafe, equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { empty as empty_1, FSharpSet__Add, FSharpSet__Contains } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { OutputPortNumber, InputPortNumber } from "../Common/CommonTypes.fs.js";

export function couldBeSynchronousComponent(compType) {
    switch (compType.tag) {
        case 20:
        case 21:
        case 22:
        case 24:
        case 25:
        case 16: {
            return true;
        }
        case 0:
        case 1:
        case 2:
        case 4:
        case 3:
        case 17:
        case 18:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11:
        case 13:
        case 14:
        case 15:
        case 12:
        case 23: {
            return false;
        }
        default: {
            return true;
        }
    }
}

export function hasSynchronousComponents(graph) {
    let _arg1;
    let table_1;
    table_1 = map((compId, comp) => {
        const matchValue = comp.Type;
        switch (matchValue.tag) {
            case 20:
            case 21:
            case 22:
            case 24:
            case 25: {
                return true;
            }
            case 16: {
                const graph_1 = comp.CustomSimulationGraph;
                return hasSynchronousComponents(graph_1);
            }
            case 0:
            case 1:
            case 2:
            case 3:
            case 17:
            case 18:
            case 5:
            case 6:
            case 7:
            case 8:
            case 9:
            case 10:
            case 11:
            case 13:
            case 14:
            case 15:
            case 12:
            case 23:
            case 4: {
                return false;
            }
            default: {
                return true;
            }
        }
    }, graph);
    _arg1 = tryPick((compId_1, isSync) => {
        if (isSync) {
            return some(void 0);
        }
        else {
            return void 0;
        }
    }, table_1);
    if (_arg1 == null) {
        return false;
    }
    else {
        return true;
    }
}

export function isInput(_arg1) {
    if (_arg1.tag === 0) {
        return true;
    }
    else {
        return false;
    }
}

export function isOutput(_arg1) {
    if (_arg1.tag === 1) {
        return true;
    }
    else {
        return false;
    }
}

export function isCustom(_arg1) {
    if (_arg1.tag === 16) {
        return true;
    }
    else {
        return false;
    }
}

export function getCustomName(_arg1) {
    if (_arg1.tag === 16) {
        const custom = _arg1.fields[0];
        return custom.Name;
    }
    else {
        return toFail(printf("what? getCustomName should only be called with custom components"));
    }
}

export function getCustomComponentType(_arg1) {
    if (_arg1.tag === 16) {
        const custom = _arg1.fields[0];
        return custom;
    }
    else {
        return toFail(printf("what? getCustomComponentType should only be called with custom components"));
    }
}

export function getNodeOrFail(graph, id) {
    const matchValue = FSharpMap__TryFind(graph, id);
    if (matchValue != null) {
        const comp = matchValue;
        return comp;
    }
    else {
        const clo1 = toFail(printf("what? getNodeOrFail received invalid component id: %A"));
        return clo1(id);
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

function getCustomCombinatorialOutputs(combRoutes, customNode, inputPortNumber) {
    let combOutputs;
    let matchValue;
    const arg00 = getCustomName(customNode.Type);
    matchValue = FSharpMap__TryFind(combRoutes, arg00);
    if (matchValue != null) {
        const routes = matchValue;
        const matchValue_1 = FSharpMap__TryFind(routes, inputPortNumber);
        if (matchValue_1 != null) {
            const outputs = matchValue_1;
            combOutputs = outputs;
        }
        else {
            combOutputs = toFail(printf("what? getCustomCombinatorialOutputs 2"));
        }
    }
    else {
        combOutputs = toFail(printf("what? getCustomCombinatorialOutputs 1"));
    }
    return filter((outputPortNumber, _arg1) => contains(outputPortNumber, combOutputs, {
        Equals: equalsSafe,
        GetHashCode: hashSafe,
    }), customNode.Outputs);
}

export function getCombinatorialOutputs(combRoutes, node, inputPortNumberOpt) {
    let comp;
    const matchValue = node.Type;
    if (matchValue.tag === 16) {
        const inputPortNumber = inputPortNumberOpt;
        return getCustomCombinatorialOutputs(combRoutes, node, inputPortNumber);
    }
    else if (comp = matchValue, couldBeSynchronousComponent(comp)) {
        const comp_1 = matchValue;
        return empty();
    }
    else {
        const comp_2 = matchValue;
        return node.Outputs;
    }
}

function dfs(graph, combPaths, currId, inputPortNumber, visited, outputsReached) {
    let list, table;
    const exploreChildren = (visited_1_mut, outputsReached_1_mut, children_mut) => {
        exploreChildren:
        while (true) {
            const visited_1 = visited_1_mut, outputsReached_1 = outputsReached_1_mut, children = children_mut;
            if (children.tail != null) {
                const children$0027 = children.tail;
                const childInpPNum = children.head[1];
                const childId = children.head[0];
                const patternInput = dfs(graph, combPaths, childId, childInpPNum, visited_1, outputsReached_1);
                const visited_2 = patternInput[0];
                const outputsReached_2 = patternInput[1];
                visited_1_mut = visited_2;
                outputsReached_1_mut = outputsReached_2;
                children_mut = children$0027;
                continue exploreChildren;
            }
            else {
                return [visited_1, outputsReached_1];
            }
            break;
        }
    };
    const currNode = getNodeOrFail(graph, currId);
    const inputPortNumber_1 = (currNode.Type.tag === 16) ? inputPortNumber : (void 0);
    const matchValue_1 = FSharpSet__Contains(visited, [currId, inputPortNumber_1]);
    if (matchValue_1) {
        return [visited, outputsReached];
    }
    else {
        const visited_3 = FSharpSet__Add(visited, [currId, inputPortNumber_1]);
        if (currNode.Type.tag === 1) {
            return [visited_3, cons(currNode.Label, outputsReached)];
        }
        else {
            return exploreChildren(visited_3, outputsReached, (list = (table = getCombinatorialOutputs(combPaths, currNode, inputPortNumber_1), (toList(table))), (collect((tupledArg) => {
                const portChildren = tupledArg[1];
                return portChildren;
            }, list))));
        }
    }
}

function findCombinatorialPaths(customComp, currGraph, combPaths) {
    let table_1;
    const labelToString = (_arg1) => {
        const label = _arg1.fields[0];
        return label;
    };
    const labelsToStrings = (labels) => map_1((tuple) => tuple[0], labels);
    const runDfs = (inputs) => {
        let arg0;
        if (inputs.tail != null) {
            const inputs$0027 = inputs.tail;
            const input = inputs.head[1];
            const patternInput = dfs(currGraph, combPaths, input.Id, new InputPortNumber(0, 0), empty_1({
                Compare: compareArrays,
            }), empty_2());
            const outputsReached = patternInput[1];
            const res = runDfs(inputs$0027);
            let outputsPNums;
            outputsPNums = map_1((arg) => {
                const out = labelToString(arg);
                return labelToPortNumber(out, labelsToStrings(customComp.OutputLabels)) | 0;
            }, outputsReached);
            return FSharpMap__Add(res, (arg0 = (labelToPortNumber(labelToString(input.Label), labelsToStrings(customComp.InputLabels)) | 0), (new InputPortNumber(0, arg0))), (map_1((arg0_1) => (new OutputPortNumber(0, arg0_1)), outputsPNums)));
        }
        else {
            return empty();
        }
    };
    return runDfs((table_1 = (filter((compId, comp) => isInput(comp.Type), currGraph)), (toList(table_1))));
}

function exploreNestedComponents(currGraph, currName, currStack, result) {
    let table_2, table_1;
    const currStack_1 = FSharpSet__Add(currStack, currName);
    const iterateNestedComponents = (res_mut, nested_mut) => {
        let nextGraph, nested$0027, customNode, nextGraph_2, nested$0027_2, customNode_2;
        iterateNestedComponents:
        while (true) {
            const res = res_mut, nested = nested_mut;
            if (nested.tail != null) {
                if (nextGraph = nested.head[1][0], (nested$0027 = nested.tail, (customNode = nested.head[1][1], res == null))) {
                    const customNode_1 = nested.head[1][1];
                    const nested$0027_1 = nested.tail;
                    const nextGraph_1 = nested.head[1][0];
                    return void 0;
                }
                else {
                    let pattern_matching_result, customNode_3, nested$0027_3, nextGraph_3;
                    if (nested.tail != null) {
                        if (nextGraph_2 = nested.head[1][0], (nested$0027_2 = nested.tail, (customNode_2 = nested.head[1][1], res != null))) {
                            pattern_matching_result = 0;
                            customNode_3 = nested.head[1][1];
                            nested$0027_3 = nested.tail;
                            nextGraph_3 = nested.head[1][0];
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
                            const result_1 = res;
                            const matchValue = calculateCustomCompCombPaths(nextGraph_3, getCustomName(customNode_3), getCustomComponentType(customNode_3), currStack_1, result_1);
                            if (matchValue != null) {
                                const result_2 = matchValue;
                                res_mut = result_2;
                                nested_mut = nested$0027_3;
                                continue iterateNestedComponents;
                            }
                            else {
                                return void 0;
                            }
                        }
                        case 1: {
                            return toFail(printf("what? Impossible case in iterateNestedComponents"));
                        }
                    }
                }
            }
            else {
                return res;
            }
            break;
        }
    };
    return iterateNestedComponents(result, (table_2 = (table_1 = (filter((compId, comp) => isCustom(comp.Type), currGraph)), (map((compId_1, comp_1) => [comp_1.CustomSimulationGraph, comp_1.Type], table_1))), (toList(table_2))));
}

function calculateCustomCompCombPaths(currGraph, currName, customComp, currStack, result) {
    const matchValue = [FSharpSet__Contains(currStack, currName), FSharpMap__ContainsKey(result, currName)];
    if (matchValue[0]) {
        return void 0;
    }
    else if (matchValue[1]) {
        return result;
    }
    else {
        const matchValue_1 = exploreNestedComponents(currGraph, currName, currStack, result);
        if (matchValue_1 != null) {
            const result_1 = matchValue_1;
            const arg0 = FSharpMap__Add(result_1, currName, findCombinatorialPaths(customComp, currGraph, result_1));
            return arg0;
        }
        else {
            return void 0;
        }
    }
}

export function calculateCustomComponentsCombinatorialPaths(diagramName, graph) {
    return exploreNestedComponents(graph, diagramName, empty_1({
        Compare: comparePrimitives,
    }), empty());
}

