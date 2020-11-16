import { Union } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { InputPortNumber, ConnectionId, ComponentId, InputPortNumber$reflection, ComponentId$reflection } from "../Common/CommonTypes.fs.js";
import { union_type, list_type, class_type, tuple_type, option_type } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { comparePrimitives, compare, compareArrays, partialApply, equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { empty as empty_1, map, item, mapIndexed, length, collect, singleton, cons } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { getCombinatorialOutputs, calculateCustomComponentsCombinatorialPaths, isCustom, getCustomName, getNodeOrFail } from "./SynchronousUtils.fs.js";
import { empty, FSharpSet__Add, FSharpSet__Contains } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { printf, toFail } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { filter, toList } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { SimulationError } from "./SimulatorTypes.fs.js";
import { FSharpResult$2, Result_Bind } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";

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
    return union_type("SimulationGraphAnalyser.DfsType", [], DfsType, () => [[["Item", class_type("Microsoft.FSharp.Collections.FSharpSet`1", [tuple_type(ComponentId$reflection(), option_type(InputPortNumber$reflection()))])]], [["Item1", list_type(ComponentId$reflection())], ["Item2", ComponentId$reflection()]], [["Item", list_type(ComponentId$reflection())]]]);
}

function dfs(currNodeId, inputPortNumber, graph, visited, currStack, getCombOuts) {
    let list, table;
    const exploreChildren = (visited_1_mut, currStack_1_mut, children_mut) => {
        exploreChildren:
        while (true) {
            const visited_1 = visited_1_mut, currStack_1 = currStack_1_mut, children = children_mut;
            if (children.tail != null) {
                const children$0027 = children.tail;
                const childPNum = children.head[1];
                const childId = children.head[0];
                const matchValue = dfs(childId, childPNum, graph, visited_1, currStack_1, getCombOuts);
                switch (matchValue.tag) {
                    case 1: {
                        const cycleEnd = matchValue.fields[1];
                        const cycle = matchValue.fields[0];
                        const matchValue_1 = equalsSafe(cycleEnd, currNodeId);
                        if (matchValue_1) {
                            return new DfsType(2, cycle);
                        }
                        else {
                            return new DfsType(1, cons(currNodeId, cycle), cycleEnd);
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
    const currNode = getNodeOrFail(graph, currNodeId);
    const inputPortNumber_1 = (currNode.Type.tag === 16) ? inputPortNumber : (void 0);
    const curr = [currNodeId, inputPortNumber_1];
    const matchValue_3 = [FSharpSet__Contains(currStack, curr), FSharpSet__Contains(visited, curr)];
    if (matchValue_3[0]) {
        if (matchValue_3[1]) {
            return new DfsType(1, singleton(currNodeId), currNodeId);
        }
        else {
            const clo1 = toFail(printf("what? Node never visited but in the stack, while detecting cycle: %A"));
            return clo1(currNodeId);
        }
    }
    else if (matchValue_3[1]) {
        return new DfsType(0, visited);
    }
    else {
        const visited_3 = FSharpSet__Add(visited, curr);
        const currStack_2 = FSharpSet__Add(currStack, curr);
        return exploreChildren(visited_3, currStack_2, (list = (table = getCombOuts(currNode, inputPortNumber_1), (toList(table))), (collect((tupledArg) => {
            const portChildren = tupledArg[1];
            return portChildren;
        }, list))));
    }
}

function calculateConnectionsAffected(connections, cycle) {
    const findConnection = (connections_1_mut, tupledArg_mut) => {
        findConnection:
        while (true) {
            const connections_1 = connections_1_mut, tupledArg = tupledArg_mut;
            const compIdFrom = tupledArg[0];
            const compIdTo = tupledArg[1];
            if (connections_1.tail != null) {
                const connections$0027 = connections_1.tail;
                const conn = connections_1.head;
                const matchValue = [equalsSafe(new ComponentId(0, conn.Source.HostId), compIdFrom), equalsSafe(new ComponentId(0, conn.Target.HostId), compIdTo)];
                let pattern_matching_result;
                if (matchValue[0]) {
                    if (matchValue[1]) {
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
                        return new ConnectionId(0, conn.Id);
                    }
                    case 1: {
                        connections_1_mut = connections$0027;
                        tupledArg_mut = [compIdFrom, compIdTo];
                        continue findConnection;
                    }
                }
            }
            else {
                const clo1 = toFail(printf("what? Could not find connection among %A and %A"));
                const clo2 = clo1(compIdFrom);
                return clo2(compIdTo);
            }
            break;
        }
    };
    if (length(cycle) < 2) {
        const clo1_1 = toFail(printf("what? Cycle with length less than 2: %A"));
        clo1_1(cycle);
    }
    let list_1;
    list_1 = mapIndexed((i, compId) => [compId, item((i + 1) % length(cycle), cycle)], cycle);
    const mapping_1 = partialApply(1, findConnection, [connections]);
    return map(mapping_1, list_1);
}

function checkCombinatorialCycle(graph, connectionsOpt, inDependency, getCombOuts) {
    const checkGraphForest = (nodeIdsAndPNums_mut, visited_mut) => {
        checkGraphForest:
        while (true) {
            const nodeIdsAndPNums = nodeIdsAndPNums_mut, visited = visited_mut;
            if (nodeIdsAndPNums.tail != null) {
                const pNum = nodeIdsAndPNums.head[1];
                const nodeIdsAndPNums$0027 = nodeIdsAndPNums.tail;
                const nodeId = nodeIdsAndPNums.head[0];
                const matchValue = dfs(nodeId, pNum, graph, visited, empty({
                    Compare: compareArrays,
                }), getCombOuts);
                switch (matchValue.tag) {
                    case 2: {
                        const cycle = matchValue.fields[0];
                        let connectionsAffected;
                        if (connectionsOpt != null) {
                            const conns = connectionsOpt;
                            connectionsAffected = calculateConnectionsAffected(conns, cycle);
                        }
                        else {
                            connectionsAffected = empty_1();
                        }
                        return new SimulationError("Cycle detected in combinatorial logic.", inDependency, cycle, connectionsAffected);
                    }
                    case 1: {
                        const ce = matchValue.fields[1];
                        const c = matchValue.fields[0];
                        const tupledArg = [c, ce];
                        const clo1 = toFail(printf("what? Dfs should never terminate while backtracking: %A"));
                        return clo1([tupledArg[0], tupledArg[1]]);
                    }
                    default: {
                        const visited_1 = matchValue.fields[0];
                        nodeIdsAndPNums_mut = nodeIdsAndPNums$0027;
                        visited_mut = visited_1;
                        continue checkGraphForest;
                    }
                }
            }
            else {
                return void 0;
            }
            break;
        }
    };
    const visited_2 = empty({
        Compare: compare,
    });
    let allIdsAndPNums;
    let list_1;
    list_1 = toList(graph);
    allIdsAndPNums = collect((tupledArg_1) => {
        const id = tupledArg_1[0];
        const comp = tupledArg_1[1];
        const matchValue_1 = comp.Type;
        if (matchValue_1.tag === 16) {
            const custom = matchValue_1.fields[0];
            return mapIndexed((i, _arg1) => [id, new InputPortNumber(0, i)], custom.InputLabels);
        }
        else {
            return singleton([id, new InputPortNumber(0, 0)]);
        }
    }, list_1);
    return checkGraphForest(allIdsAndPNums, visited_2);
}

function recursivelyCheckCombinatorialCycles(currGraph, connectionsOpt, dependencyName, alreadyChecked, getCombOuts) {
    let table_1;
    const iterateChildren = (alreadyChecked_1, children) => {
        if (children.tail != null) {
            const children$0027 = children.tail;
            const child = children.head[1];
            const childGraph = child.CustomSimulationGraph;
            const childName = getCustomName(child.Type);
            const result = recursivelyCheckCombinatorialCycles(childGraph, void 0, childName, alreadyChecked_1, getCombOuts);
            return Result_Bind((alreadyChecked_2) => iterateChildren(alreadyChecked_2, children$0027), result);
        }
        else {
            return new FSharpResult$2(0, alreadyChecked_1);
        }
    };
    const matchValue = FSharpSet__Contains(alreadyChecked, dependencyName);
    if (matchValue) {
        return new FSharpResult$2(0, alreadyChecked);
    }
    else {
        const alreadyChecked_3 = FSharpSet__Add(alreadyChecked, dependencyName);
        const result_1 = iterateChildren(alreadyChecked_3, (table_1 = (filter((compId, comp) => isCustom(comp.Type), currGraph)), (toList(table_1))));
        return Result_Bind((alreadyChecked_4) => {
            const inDependency = (connectionsOpt != null) ? (void 0) : dependencyName;
            const _arg1 = checkCombinatorialCycle(currGraph, connectionsOpt, inDependency, getCombOuts);
            if (_arg1 != null) {
                const err = _arg1;
                return new FSharpResult$2(1, err);
            }
            else {
                return new FSharpResult$2(0, alreadyChecked_4);
            }
        }, result_1);
    }
}

export function analyseSimulationGraph(diagramName, graph, connections) {
    const matchValue = calculateCustomComponentsCombinatorialPaths(diagramName, graph);
    if (matchValue != null) {
        const cccp = matchValue;
        const getCombOuts = (node, inputPortNumberOpt) => getCombinatorialOutputs(cccp, node, inputPortNumberOpt);
        const _arg1 = recursivelyCheckCombinatorialCycles(graph, connections, diagramName, empty({
            Compare: comparePrimitives,
        }), getCombOuts);
        if (_arg1.tag === 1) {
            const err = _arg1.fields[0];
            return err;
        }
        else {
            return void 0;
        }
    }
    else {
        return toFail(printf("what? calculateCustomComponentsCombinatorialPaths returned None whithin analyseSimulationGraph. This should never happen"));
    }
}

