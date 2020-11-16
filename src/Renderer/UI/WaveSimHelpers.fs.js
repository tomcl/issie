import { MatchFailureException, Record } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { class_type, tuple_type, list_type, string_type, record_type, int32_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { Draw2dWrapper__GetSelected, Draw2dWrapper__ChangeSelectionOfTheseConnections, Draw2dWrapper__GetCanvasState } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { extractState, extractConnection, extractReducedState } from "../Interface/Extractor.fs.js";
import { ofList, toArray, find, tryFind, exists, ofArray, FSharpMap__get_Item, toSeq, tryFindKey, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { getNetList } from "../../Common/Helpers.fs.js";
import { min as min_2, max as max_2, partialApply, comparePrimitives, int32ToString, uncurry, compare, compareSafe, equals, hashSafe, structuralHash, equalsSafe } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { value as value_1, map as map_3, defaultArg } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { append as append_2, replicate, takeWhile, fold as fold_1, tryFindIndexBack, rangeNumber, map as map_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { NetListComponent$reflection, ComponentLabel$reflection, Connection$reflection, Component$reflection, ComponentId$reflection, ConnectionId, ComponentId, InputPortNumber, LabelSegment, WaveLabel, NetGroup, OutputPortNumber, ComponentType, NLTarget$reflection } from "../../Common/CommonTypes.fs.js";
import { zip as zip_1, mapIndexed2, concat, map2, min as min_1, max as max_1, indexed, pairwise, transpose, fold2, append, last, equalsWith, mapFold, map as map_2, groupBy, collect } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { toText, join, toConsole, printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { exists as exists_1, pairwise as pairwise_1, getSlice, mapFoldBack, choose, min, max, unzip, ofSeq, zip, map as map_4, distinct, append as append_1, tryPick, fold, reverse, cons, collect as collect_1, ofArray as ofArray_1, singleton, tryFind as tryFind_1, length, item, sum, mapFold as mapFold_1, empty as empty_1, filter, contains } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { currWaveSimModel } from "./FileMenuView.fs.js";
import { feedClockTick } from "../../Simulator/Simulator.fs.js";
import { SimulationComponent$reflection, tryGetCompLabel, printSimGraph, SimulationData } from "../../Simulator/SimulatorTypes.fs.js";
import { SimParamsT, initWS, getCurrFile, getSheetWaveNetList, WaveSimModel, SVGCacheT, Msg, getCurrFileWSMod, Wire, Sample, getSheetWaveSimOpt } from "./ModelType.fs.js";
import { toString, op_Subtraction, fromZero, fromOne, op_LeftShift, op_BitwiseAnd, equals as equals_1, op_Addition, op_Multiply, fromInt32 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/BigInt.js";
import { makeSimData } from "./SimulationView.fs.js";
import { FSharpResult$2, Result_Map } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { contains as contains_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import { HTMLAttr, SVGAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import * as react from "react";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { cursorLeftPx, maxWavesColWidthFloat, waveCell, waveCellSvg, lwaveCell, clkRulerStyle, cursRectText, transLen, sigLineThick, inWaveLabel, maxBusValGap, spacing, sigHeight, vPos } from "./Style.fs.js";
import { Option, button as button_1 } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";

export const maxLastClk = 500;

export class WaveGapT extends Record {
    constructor(GapLen, GapStart) {
        super();
        this.GapLen = (GapLen | 0);
        this.GapStart = (GapStart | 0);
    }
}

export function WaveGapT$reflection() {
    return record_type("WaveSimHelpers.WaveGapT", [], WaveGapT, () => [["GapLen", int32_type], ["GapStart", int32_type]]);
}

export function getReducedCanvState(model) {
    const matchValue = Draw2dWrapper__GetCanvasState(model.Diagram);
    if (matchValue == null) {
        return void 0;
    }
    else {
        const cS = matchValue;
        const arg0 = extractReducedState(cS[0], cS[1]);
        return arg0;
    }
}

export function wsModel2netList(wsModel) {
    const matchValue = wsModel.LastCanvasState;
    if (matchValue == null) {
        return empty();
    }
    else {
        const canvState = matchValue;
        return getNetList(canvState[0], canvState[1]);
    }
}

export function waveNameOf(ws, ng) {
    const option = tryFindKey((_arg1, p) => equalsSafe(p, ng), ws.AllNets);
    return defaultArg(option, "ERROR");
}

export function mapKeys(map) {
    let source_1;
    let source;
    source = toSeq(map);
    source_1 = map_1((tuple) => tuple[0], source);
    return Array.from(source_1);
}

export function mapValues(map) {
    let source_1;
    let source;
    source = toSeq(map);
    source_1 = map_1((tuple) => tuple[1], source);
    return Array.from(source_1);
}

export function mapItems(map) {
    let source;
    source = toSeq(map);
    return Array.from(source);
}

function allNComps(netList) {
    return mapValues(netList);
}

export class NGrp extends Record {
    constructor(Driven, DriverLabel, Driver) {
        super();
        this.Driven = Driven;
        this.DriverLabel = DriverLabel;
        this.Driver = Driver;
    }
}

export function NGrp$reflection() {
    return record_type("WaveSimHelpers.NGrp", [], NGrp, () => [["Driven", list_type(string_type)], ["DriverLabel", list_type(string_type)], ["Driver", list_type(NLTarget$reflection())]]);
}

function makeAllNetGroups(netList) {
    const comps = allNComps(netList);
    let labelConnectedNets;
    let elements;
    let array_3;
    let array_1;
    array_1 = collect((comp) => {
        if (equalsSafe(comp.Type, new ComponentType(2))) {
            return [[comp.Label, FSharpMap__get_Item(comp.Outputs, new OutputPortNumber(0, 0))]];
        }
        else {
            return [];
        }
    }, comps);
    array_3 = groupBy((tupledArg) => {
        const label = tupledArg[0];
        return label;
    }, array_1, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    elements = map_2((tupledArg_1) => {
        const lab = tupledArg_1[0];
        const labOutArr = tupledArg_1[1];
        return [lab, (map_2((tuple) => tuple[1], labOutArr))];
    }, array_3);
    labelConnectedNets = ofArray(elements);
    const makeNetGroup = (targets) => {
        let connected;
        let array_4;
        array_4 = Array.from(targets);
        connected = collect((target) => {
            const comp_1 = FSharpMap__get_Item(netList, target.TargetCompId);
            if (equalsSafe(comp_1.Type, new ComponentType(2))) {
                return FSharpMap__get_Item(labelConnectedNets, comp_1.Label);
            }
            else {
                return [];
            }
        }, array_4);
        return new NetGroup(targets, connected);
    };
    let allNetGroups;
    allNetGroups = collect((comp_2) => {
        if (comp_2.Type.tag === 2) {
            return [];
        }
        else {
            const array_5 = mapValues(comp_2.Outputs);
            return map_2(makeNetGroup, array_5);
        }
    }, comps);
    return allNetGroups;
}

function getNetGroup(netList, targets) {
    return toFail(printf("this function is no longer implemented"));
}

function isNetListTrgtInNetList(netList, nlTrgt) {
    return exists((_arg2, nlComp) => exists((_arg1, nlTrgtLst) => contains(nlTrgt, nlTrgtLst, {
        Equals: equalsSafe,
        GetHashCode: hashSafe,
    }), nlComp.Outputs), netList);
}

function getReloadableNetGroups(model, netList) {
    const matchValue = currWaveSimModel(model);
    if (matchValue == null) {
        return [];
    }
    else {
        const wSModel = matchValue;
        let array_4;
        let array_3;
        let array_2;
        let array_1;
        const ws = wSModel;
        array_1 = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
        array_2 = map_2((netGroup) => netGroup.driverNet, array_1);
        const mapping_2 = (list) => filter((nlTrgt) => isNetListTrgtInNetList(netList, nlTrgt), list);
        array_3 = map_2(mapping_2, array_2);
        array_4 = array_3.filter((y) => (!equals(empty_1(), y)));
        return map_2((targets) => getNetGroup(netList, targets), array_4);
    }
}

function clkAdvance(sD) {
    const graph = feedClockTick(sD.Graph);
    const ClockTickNumber = (sD.ClockTickNumber + 1) | 0;
    return new SimulationData(graph, sD.Inputs, sD.Outputs, sD.IsSynchronous, sD.NumberBase, ClockTickNumber);
}

export function extractSimData(simData, nCycles) {
    let tuple;
    const array = Uint32Array.from(rangeNumber(1, 1, nCycles));
    tuple = mapFold((s, _arg1) => [clkAdvance(s), clkAdvance(s)], simData, array);
    return tuple[0];
}

function drivingOutput(netList, compId, inPortN) {
    return FSharpMap__get_Item(FSharpMap__get_Item(netList, compId).Inputs, inPortN);
}

export function netList2NetGroups(netList) {
    return makeAllNetGroups(netList);
}

export function availableNetGroups(model) {
    const matchValue = getSheetWaveSimOpt(model);
    if (matchValue != null) {
        const waveSim = matchValue;
        let netList;
        let tupledArg;
        tupledArg = defaultArg(waveSim.LastCanvasState, [empty_1(), empty_1()]);
        netList = getNetList(tupledArg[0], tupledArg[1]);
        return netList2NetGroups(netList);
    }
    else {
        return [];
    }
}

function simWireData2Wire(wireData) {
    let list_1;
    let tuple;
    const state = fromInt32(1);
    tuple = mapFold_1((weight, bit) => {
        const r = (bit.tag === 1) ? weight : fromInt32(0);
        return [r, op_Multiply(weight, fromInt32(2))];
    }, state, wireData);
    list_1 = tuple[0];
    return sum(list_1, {
        GetZero: () => fromInt32(0),
        Add: op_Addition,
    });
}

export function getSimTime(trgtLstGroups, simGraph) {
    const array = map_2((trgtLstGroup) => trgtLstGroup.driverNet, trgtLstGroups);
    return map_2((trgtLst) => {
        try {
            const compId = item(0, trgtLst).TargetCompId;
            const inputPorts = FSharpMap__get_Item(simGraph, compId).Inputs;
            const portNum = item(0, trgtLst).InputPort;
            let wD;
            const option = tryFind(portNum, inputPorts);
            wD = defaultArg(option, empty_1());
            return new Sample(0, new Wire(length(wD) >>> 0, simWireData2Wire(wD)));
        }
        catch (e) {
            const clo1 = toConsole(printf("Exception: %A"));
            clo1(e);
            printSimGraph(simGraph);
            const compId_1 = item(0, trgtLst).TargetCompId;
            const arg10_1 = tryGetCompLabel(compId_1, simGraph);
            const clo1_1 = toConsole(printf("\nComponent %s\n\n"));
            clo1_1(arg10_1);
            return toFail(printf("What? This error in getSimTime should not be possible"));
        }
    }, array);
}

export function getAllWaveSimDataBySample(wsMod) {
    let netGroups;
    const ws = wsMod;
    netGroups = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
    const array_1 = map_2((sD) => sD.Graph, wsMod.SimDataCache);
    return map_2((simGraph) => getSimTime(netGroups, simGraph), array_1);
}

export function getWaveSimDataOneSample(wsMod, sample) {
    let netGroups;
    const ws = wsMod;
    netGroups = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
    const simGraph = wsMod.SimDataCache[sample].Graph;
    return getSimTime(netGroups, simGraph);
}

function appendSimData(model, wSModel, nCycles) {
    const matchValue = wSModel.SimDataCache;
    if ((!equalsWith(compareSafe, matchValue, null)) ? (matchValue.length === 0) : false) {
        let option_1;
        const option = makeSimData(model);
        option_1 = map_3((tuple) => tuple[0], option);
        return map_3((result) => Result_Map((sd) => extractSimData(sd, nCycles), result), option_1);
    }
    else {
        const dat = matchValue;
        let arg0_1;
        let arg0;
        const array2 = extractSimData(last(dat), nCycles);
        arg0 = append(dat, array2);
        arg0_1 = (new FSharpResult$2(0, arg0));
        return arg0_1;
    }
}

function connId2JSConn(diagram, connId) {
    let _arg1;
    const matchValue = Draw2dWrapper__GetCanvasState(diagram);
    if (matchValue == null) {
        _arg1 = (void 0);
    }
    else {
        const jsConns = matchValue[1];
        _arg1 = tryFind_1((jsConn) => (extractConnection(jsConn).Id === connId), jsConns);
    }
    if (_arg1 == null) {
        return empty_1();
    }
    else {
        const jsConn_1 = _arg1;
        return singleton(jsConn_1);
    }
}

function wave2ConnIds(netGrp) {
    const array_1 = append([netGrp.driverNet], netGrp.connectedNets);
    return collect((net) => {
        const array = Array.from(net);
        return map_2((net_1) => net_1.TargetConnId, array);
    }, array_1);
}

export function selectNetGrpConns(diagram, netGrp, on) {
    let arg10;
    let list;
    const array = wave2ConnIds(netGrp);
    list = ofArray_1(array);
    arg10 = collect_1((_arg1) => {
        const cId = _arg1.fields[0];
        return connId2JSConn(diagram, cId);
    }, list);
    Draw2dWrapper__ChangeSelectionOfTheseConnections(diagram, on, arg10);
}

export function setSelNamesHighlighted(names, model, dispatch) {
    const matchValue = getCurrFileWSMod(model);
    if (matchValue != null) {
        const ws = matchValue;
        let connIds;
        let array_1;
        array_1 = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), names);
        connIds = collect(wave2ConnIds, array_1);
        dispatch(new Msg(14, connIds));
    }
}

export function selectNGConns(model, netGroups, on) {
    let arg10;
    let list;
    let array_1;
    array_1 = collect(wave2ConnIds, netGroups);
    list = ofArray_1(array_1);
    arg10 = collect_1((_arg1) => {
        const cId = _arg1.fields[0];
        return connId2JSConn(model.Diagram, cId);
    }, list);
    Draw2dWrapper__ChangeSelectionOfTheseConnections(model.Diagram, on, arg10);
}

export function findInstancesOf(sheet, lComp) {
    let list;
    list = lComp.CanvasState[0];
    return collect_1((_arg1) => {
        let sheet$0027, comp;
        let pattern_matching_result, comp_1, sheet$0027_1;
        if (_arg1.Type.tag === 16) {
            if (sheet$0027 = _arg1.Type.fields[0].Name, (comp = _arg1, sheet$0027 === sheet)) {
                pattern_matching_result = 0;
                comp_1 = _arg1;
                sheet$0027_1 = _arg1.Type.fields[0].Name;
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
                return singleton(comp_1.Label);
            }
            case 1: {
                return empty_1();
            }
        }
    }, list);
}

export function getRootSheet(model) {
    if (model.CurrentProj != null) {
        const proj = model.CurrentProj;
        const lComps = model.CurrentProj.LoadedComponents;
        const openSheet = proj.OpenFileName;
        const getParentData = (sheet) => {
            let list_1;
            list_1 = filter((lComp) => (lComp.Name !== sheet), lComps);
            return collect_1((lComp_1) => {
                const parentName = lComp_1.Name;
                const matchValue = findInstancesOf(sheet, lComp_1);
                if (matchValue.tail != null) {
                    if (matchValue.tail.tail == null) {
                        return singleton(parentName);
                    }
                    else {
                        return singleton(openSheet);
                    }
                }
                else {
                    return empty_1();
                }
            }, list_1);
        };
        const findRoot = (traversedSheets_mut, sheet_1_mut) => {
            findRoot:
            while (true) {
                const traversedSheets = traversedSheets_mut, sheet_1 = sheet_1_mut;
                if (contains(sheet_1, traversedSheets, {
                    Equals: (x, y) => (x === y),
                    GetHashCode: structuralHash,
                })) {
                    return openSheet;
                }
                else {
                    const matchValue_1 = getParentData(sheet_1);
                    if (matchValue_1.tail == null) {
                        return sheet_1;
                    }
                    else if (matchValue_1.tail.tail == null) {
                        const root = matchValue_1.head;
                        traversedSheets_mut = cons(sheet_1, traversedSheets);
                        sheet_1_mut = root;
                        continue findRoot;
                    }
                    else {
                        return openSheet;
                    }
                }
                break;
            }
        };
        const arg0 = findRoot(empty_1(), openSheet);
        return arg0;
    }
    else {
        return void 0;
    }
}

export function getCustoms(cid, comp) {
    const matchValue = comp.Type;
    if (matchValue.tag === 16) {
        const c = matchValue.fields[0];
        return singleton([cid, c.Name, comp]);
    }
    else {
        return empty_1();
    }
}

export function simGraphOrFail(comp) {
    const matchValue = comp.CustomSimulationGraph;
    if (matchValue == null) {
        const clo1 = toFail(printf("What? a Custom component %A appears to have no simulation graph"));
        return clo1(comp);
    }
    else {
        const x = matchValue;
        return x;
    }
}

export function getSimGraph(sheet, graph) {
    let clo1_1;
    const makeSelector = (cids) => {
        const list = reverse(cids);
        return fold((selFun, cid) => {
            const func = (graph_1) => simGraphOrFail(find(cid, graph_1));
            return (arg) => func(selFun(arg));
        }, (x) => x, list);
    };
    const getGraph = (customs, thisSheet, graph_2) => {
        if (sheet === thisSheet) {
            return [(x_1) => x_1];
        }
        else {
            let array_1;
            const array = toArray(graph_2);
            array_1 = collect((arg_1) => {
                let list_1;
                const tupledArg = arg_1;
                list_1 = getCustoms(tupledArg[0], tupledArg[1]);
                return Array.from(list_1);
            }, array);
            return collect((tupledArg_1) => {
                let clo1;
                const cid_2 = tupledArg_1[0];
                const sheet$0027 = tupledArg_1[1];
                const comp_1 = tupledArg_1[2];
                if (sheet$0027 === sheet) {
                    return [makeSelector(cons(cid_2, customs))];
                }
                else {
                    return getGraph(cons(cid_2, customs), sheet$0027, defaultArg(comp_1.CustomSimulationGraph, (clo1 = toFail(printf("What? %A should have a CustomSimulationGraph")), clo1(comp_1))));
                }
            }, array_1);
        }
    };
    const _arg1 = getGraph(empty_1(), sheet, graph);
    if ((!equalsWith(compare, _arg1, null)) ? (_arg1.length === 1) : false) {
        const f = _arg1[0];
        return f;
    }
    else {
        const x_3 = _arg1;
        return (clo1_1 = toFail(printf("Wrong number of candidates %A to find %s in Simgraph")), (arg10_1) => {
            const clo2 = clo1_1(arg10_1);
            return (arg20) => {
                const clo3 = clo2(arg20);
                return clo3;
            };
        })(x_3)(sheet);
    }
}

function nlTrgtLst2CommonNLSource(netList, nlTrgtLst) {
    return tryPick((nlTrgt) => {
        const matchValue = tryFind(nlTrgt.TargetCompId, netList);
        if (matchValue == null) {
            return void 0;
        }
        else {
            const comp = matchValue;
            const matchValue_1 = tryFind(nlTrgt.InputPort, comp.Inputs);
            let pattern_matching_result, src;
            if (matchValue_1 != null) {
                if (value_1(matchValue_1) != null) {
                    pattern_matching_result = 0;
                    src = value_1(matchValue_1);
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
                    return src;
                }
                case 1: {
                    return void 0;
                }
            }
        }
    }, nlTrgtLst);
}

function labelNoParenthesis(netList, compId) {
    const lbl = FSharpMap__get_Item(netList, compId).Label;
    const matchValue = tryFindIndexBack((y) => ("(" === y), lbl.split(""));
    if (matchValue == null) {
        return lbl;
    }
    else {
        const i = matchValue | 0;
        return lbl.slice(0, (i - 1) + 1);
    }
}

function outPortInt2int(outPortInt) {
    const pn = outPortInt.fields[0] | 0;
    return pn | 0;
}

export function net2outputsAndIOLabels(netList, netLst) {
    const nlTrgt2Lbls = (st, nlTrgt) => {
        const matchValue = tryFind(nlTrgt.TargetCompId, netList);
        if (matchValue == null) {
            return st;
        }
        else {
            const nlComp = matchValue;
            const matchValue_1 = nlComp.Type;
            switch (matchValue_1.tag) {
                case 2:
                case 1: {
                    return append_1(st, singleton(FSharpMap__get_Item(netList, nlTrgt.TargetCompId).Label));
                }
                default: {
                    return st;
                }
            }
        }
    };
    const list = fold(nlTrgt2Lbls, empty_1(), netLst);
    return distinct(list, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
}

export function netGroup2outputsAndIOLabels(netList, netGrp) {
    let list_1;
    let list;
    const array = append([netGrp.driverNet], netGrp.connectedNets);
    list = ofArray_1(array);
    list_1 = collect_1((netLst) => net2outputsAndIOLabels(netList, netLst), list);
    return distinct(list_1, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
}

function removeSubSeq(startC, endC, chars) {
    let strings;
    let list_1;
    let list;
    let tuple;
    tuple = fold_1(uncurry(2, (tupledArg) => {
        const removing = tupledArg[0];
        const res = tupledArg[1];
        return (ch) => {
            let ch_3, ch_1;
            const matchValue = [removing, ch];
            let pattern_matching_result;
            if (matchValue[0]) {
                if (ch_1 = matchValue[1], ch_1 === endC) {
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
                    const ch_2 = matchValue[1];
                    return [false, res];
                }
                case 1: {
                    if (matchValue[0]) {
                        return [true, res];
                    }
                    else if (ch_3 = matchValue[1], ch_3 === startC) {
                        const ch_4 = matchValue[1];
                        return [true, res];
                    }
                    else if (matchValue[0]) {
                        throw (new MatchFailureException("C:/github/issie/src/renderer/UI/WaveSimHelpers.fs", 394, 26));
                    }
                    else {
                        const ch_5 = matchValue[1];
                        return [false, cons(ch_5, res)];
                    }
                }
            }
        };
    }), [false, empty_1()], chars);
    list = tuple[1];
    list_1 = reverse(list);
    strings = map_4((value) => value, list_1);
    return join("", strings);
}

function simplifyName(name) {
    return removeSubSeq("(", ")", name);
}

function findName(compIds, graph, net, netGrp, nlTrgtList) {
    const matchValue = nlTrgtLst2CommonNLSource(net, nlTrgtList);
    if (matchValue != null) {
        const nlSource = matchValue;
        if (!contains_1(nlSource.SourceCompId, compIds)) {
            toConsole(printf("What? graph, net, netGrp, nltrgtList should all be consistent, compIds is deprecated"));
            return new WaveLabel(empty_1(), empty_1());
        }
        else {
            const compLbl = labelNoParenthesis(net, nlSource.SourceCompId);
            const outPortInt = outPortInt2int(nlSource.OutputPort) | 0;
            const drivingOutputName = (inPortN) => {
                const matchValue_1 = drivingOutput(net, nlSource.SourceCompId, inPortN);
                if (matchValue_1 == null) {
                    return new WaveLabel(empty_1(), empty_1());
                }
                else {
                    const nlSource$0027 = matchValue_1;
                    const nlTrgtList_1 = FSharpMap__get_Item(FSharpMap__get_Item(net, nlSource$0027.SourceCompId).Outputs, nlSource$0027.OutputPort);
                    return findName(compIds, graph, net, netGrp, nlTrgtList_1);
                }
            };
            let composingLbls;
            const matchValue_2 = FSharpMap__get_Item(net, nlSource.SourceCompId).Type;
            let pattern_matching_result, w, w_2, mem;
            switch (matchValue_2.tag) {
                case 6:
                case 7:
                case 8:
                case 9:
                case 10:
                case 11:
                case 12:
                case 13: {
                    pattern_matching_result = 0;
                    break;
                }
                case 0: {
                    pattern_matching_result = 1;
                    w = matchValue_2.fields[0];
                    break;
                }
                case 1: {
                    pattern_matching_result = 1;
                    w = matchValue_2.fields[0];
                    break;
                }
                case 4: {
                    pattern_matching_result = 1;
                    w = matchValue_2.fields[0];
                    break;
                }
                case 14: {
                    pattern_matching_result = 2;
                    break;
                }
                case 15: {
                    pattern_matching_result = 3;
                    break;
                }
                case 19:
                case 20: {
                    pattern_matching_result = 4;
                    break;
                }
                case 21: {
                    pattern_matching_result = 5;
                    w_2 = matchValue_2.fields[0];
                    break;
                }
                case 22: {
                    pattern_matching_result = 5;
                    w_2 = matchValue_2.fields[0];
                    break;
                }
                case 25: {
                    pattern_matching_result = 6;
                    mem = matchValue_2.fields[0];
                    break;
                }
                case 23: {
                    pattern_matching_result = 6;
                    mem = matchValue_2.fields[0];
                    break;
                }
                case 24: {
                    pattern_matching_result = 6;
                    mem = matchValue_2.fields[0];
                    break;
                }
                case 16: {
                    pattern_matching_result = 7;
                    break;
                }
                case 17: {
                    pattern_matching_result = 8;
                    break;
                }
                case 18: {
                    pattern_matching_result = 9;
                    break;
                }
                case 3: {
                    pattern_matching_result = 10;
                    break;
                }
                case 2: {
                    pattern_matching_result = 11;
                    break;
                }
                default: pattern_matching_result = 0}
            switch (pattern_matching_result) {
                case 0: {
                    composingLbls = singleton(new LabelSegment(compLbl, [0, 0]));
                    break;
                }
                case 1: {
                    composingLbls = singleton(new LabelSegment(compLbl, [w - 1, 0]));
                    break;
                }
                case 2: {
                    composingLbls = singleton(new LabelSegment((compLbl + ".") + int32ToString(outPortInt), [0, 0]));
                    break;
                }
                case 3: {
                    const w_1 = matchValue_2.fields[0] | 0;
                    composingLbls = ((outPortInt === 0) ? singleton(new LabelSegment(compLbl + ".Sum", [w_1 - 1, 0])) : singleton(new LabelSegment(compLbl + ".Cout", [w_1 - 1, 0])));
                    break;
                }
                case 4: {
                    composingLbls = singleton(new LabelSegment(compLbl + ".Q", [0, 0]));
                    break;
                }
                case 5: {
                    composingLbls = singleton(new LabelSegment(compLbl + ".Dout", [w_2 - 1, 0]));
                    break;
                }
                case 6: {
                    composingLbls = singleton(new LabelSegment(compLbl + ".Dout", [mem.WordWidth - 1, 0]));
                    break;
                }
                case 7: {
                    const c = matchValue_2.fields[0];
                    composingLbls = singleton(new LabelSegment((compLbl + ".") + item(outPortInt, c.OutputLabels)[0], [item(outPortInt, c.OutputLabels)[1] - 1, 0]));
                    break;
                }
                case 8: {
                    composingLbls = append_1(drivingOutputName(new InputPortNumber(0, 1)).ComposingLabels, drivingOutputName(new InputPortNumber(0, 0)).ComposingLabels);
                    break;
                }
                case 9: {
                    const w_3 = matchValue_2.fields[0] | 0;
                    const mostsigBranch = (tupledArg) => {
                        const b = tupledArg[1] | 0;
                        switch (outPortInt) {
                            case 0: {
                                return b >= (16 - w_3);
                            }
                            case 1: {
                                return b < (16 - w_3);
                            }
                            default: {
                                throw (new Error("SplitWire output port number greater than 1"));
                            }
                        }
                    };
                    const split = (_arg2, st) => {
                        const name = _arg2.LabName;
                        const msb = _arg2.BitLimits[0] | 0;
                        const lsb = _arg2.BitLimits[1] | 0;
                        let _arg3;
                        let list_1;
                        const list = zip(ofSeq(rangeNumber(lsb, 1, msb)), ofSeq(rangeNumber((st + msb) - lsb, -1, st)));
                        list_1 = filter(mostsigBranch, list);
                        _arg3 = unzip(list_1);
                        if (_arg3[0].tail == null) {
                            return void 0;
                        }
                        else {
                            const lst = _arg3[0];
                            return new LabelSegment(name, [max(lst, {
                                Compare: comparePrimitives,
                            }), min(lst, {
                                Compare: comparePrimitives,
                            })]);
                        }
                    };
                    const updateState = (_arg4, st_1) => {
                        const msb_1 = _arg4.BitLimits[0] | 0;
                        const lsb_1 = _arg4.BitLimits[1] | 0;
                        return (((st_1 + msb_1) - lsb_1) + 1) | 0;
                    };
                    let list_3;
                    let tuple;
                    const list_2 = drivingOutputName(new InputPortNumber(0, 0)).ComposingLabels;
                    tuple = mapFold_1((st_2, lstEl) => [split(lstEl, st_2), updateState(lstEl, st_2)], 0, list_2);
                    list_3 = tuple[0];
                    composingLbls = choose((x_2) => x_2, list_3);
                    break;
                }
                case 10: {
                    const w_4 = matchValue_2.fields[0] | 0;
                    const oLSB = matchValue_2.fields[1] | 0;
                    const filtSelec = (_arg5, st_3) => {
                        const name_1 = _arg5.LabName;
                        const msb_2 = _arg5.BitLimits[0] | 0;
                        const lsb_2 = _arg5.BitLimits[1] | 0;
                        let _arg6;
                        let list_5;
                        const list_4 = zip(ofSeq(rangeNumber(lsb_2, 1, msb_2)), ofSeq(rangeNumber(st_3, 1, (st_3 + msb_2) - lsb_2)));
                        list_5 = filter((tupledArg_1) => {
                            const b_1 = tupledArg_1[1] | 0;
                            if (oLSB <= b_1) {
                                return b_1 <= ((oLSB + w_4) - 1);
                            }
                            else {
                                return false;
                            }
                        }, list_4);
                        _arg6 = unzip(list_5);
                        if (_arg6[0].tail == null) {
                            return void 0;
                        }
                        else {
                            const lst_1 = _arg6[0];
                            return new LabelSegment(name_1, [max(lst_1, {
                                Compare: comparePrimitives,
                            }), min(lst_1, {
                                Compare: comparePrimitives,
                            })]);
                        }
                    };
                    const updateState_1 = (_arg7, st_4) => {
                        const msb_3 = _arg7.BitLimits[0] | 0;
                        const lsb_3 = _arg7.BitLimits[1] | 0;
                        return (((st_4 + msb_3) - lsb_3) + 1) | 0;
                    };
                    let list_8;
                    let list_7;
                    let tuple_1;
                    const list_6 = drivingOutputName(new InputPortNumber(0, 0)).ComposingLabels;
                    tuple_1 = mapFoldBack((lstEl_1, st_5) => [filtSelec(lstEl_1, st_5), updateState_1(lstEl_1, st_5)], list_6, 0);
                    list_7 = tuple_1[0];
                    list_8 = choose((x_5) => x_5, list_7);
                    composingLbls = reverse(list_8);
                    break;
                }
                case 11: {
                    const ioLblWidth = length(FSharpMap__get_Item(FSharpMap__get_Item(graph, item(0, nlTrgtList).TargetCompId).Inputs, item(0, nlTrgtList).InputPort)) | 0;
                    composingLbls = singleton(new LabelSegment(compLbl, [ioLblWidth - 1, 0]));
                    break;
                }
            }
            return new WaveLabel(netGroup2outputsAndIOLabels(net, netGrp), composingLbls);
        }
    }
    else {
        return new WaveLabel(empty_1(), empty_1());
    }
}

function bitLimsString(a, b) {
    let msb, lsb, msb_1, lsb_1;
    const matchValue = [a, b];
    let pattern_matching_result, lsb_2, msb_2;
    if (matchValue[0] === 0) {
        if (matchValue[1] === 0) {
            pattern_matching_result = 0;
        }
        else if (msb = (matchValue[0] | 0), (lsb = (matchValue[1] | 0), msb === lsb)) {
            pattern_matching_result = 1;
            lsb_2 = matchValue[1];
            msb_2 = matchValue[0];
        }
        else {
            pattern_matching_result = 2;
        }
    }
    else if (msb_1 = (matchValue[0] | 0), (lsb_1 = (matchValue[1] | 0), msb_1 === lsb_1)) {
        pattern_matching_result = 1;
        lsb_2 = matchValue[1];
        msb_2 = matchValue[0];
    }
    else {
        pattern_matching_result = 2;
    }
    switch (pattern_matching_result) {
        case 0: {
            return "";
        }
        case 1: {
            const clo1 = toText(printf("[%d]"));
            return clo1(msb_2);
        }
        case 2: {
            const msb_3 = matchValue[0] | 0;
            const lsb_3 = matchValue[1] | 0;
            const clo1_1 = toText(printf("[%d:%d]"));
            const clo2 = clo1_1(msb_3);
            return clo2(lsb_3);
        }
    }
}

export function netGroup2Label(compIds, graph, netList, netGrp) {
    let tupledArg, lst, lst_1;
    const waveLbl = findName(compIds, graph, netList, netGrp, netGrp.driverNet);
    let tl;
    const matchValue = waveLbl.ComposingLabels;
    let pattern_matching_result, el, lst_2;
    if (matchValue.tail != null) {
        if (matchValue.tail.tail == null) {
            pattern_matching_result = 0;
            el = matchValue.head;
        }
        else if (lst = matchValue, length(lst) > 0) {
            pattern_matching_result = 1;
            lst_2 = matchValue;
        }
        else {
            pattern_matching_result = 2;
        }
    }
    else if (lst_1 = matchValue, length(lst_1) > 0) {
        pattern_matching_result = 1;
        lst_2 = matchValue;
    }
    else {
        pattern_matching_result = 2;
    }
    switch (pattern_matching_result) {
        case 0: {
            tl = (el.LabName + (tupledArg = el.BitLimits, bitLimsString(tupledArg[0], tupledArg[1])));
            break;
        }
        case 1: {
            const appendName = (st, lblSeg) => {
                let tupledArg_1;
                return ((st + lblSeg.LabName) + (tupledArg_1 = lblSeg.BitLimits, bitLimsString(tupledArg_1[0], tupledArg_1[1]))) + ", ";
            };
            const lbl = fold(appendName, "{", lst_2);
            tl = (lbl.slice(0, (lbl.length - 3) + 1) + "}");
            break;
        }
        case 2: {
            tl = "";
            break;
        }
    }
    const appendName_1 = (st_1, name) => ((st_1 + name) + ", ");
    let name_1;
    const matchValue_1 = waveLbl.OutputsAndIOLabels;
    if (matchValue_1.tail == null) {
        name_1 = tl;
    }
    else {
        const hdLbls = matchValue_1;
        const hd = fold(appendName_1, "", hdLbls);
        name_1 = ((hd.slice(0, (hd.length - 3) + 1) + " : ") + tl);
    }
    return simplifyName(name_1.split(""));
}

export function removeSuffixFromWaveLabel(label) {
    let strings;
    let source_1;
    source_1 = takeWhile((ch) => (ch !== "."), label.split(""));
    strings = map_1((value) => value, source_1);
    return join("", strings);
}

export function setWSAllPorts(availablePorts, wSModel) {
    return toFail(printf("not implemented"));
}

export function makeLinePoints(style, x1, y1, x2, y2) {
    const props = append_1(style, ofArray_1([new SVGAttr(42, x1), new SVGAttr(55, y1), new SVGAttr(43, x2), new SVGAttr(56, y2)]));
    return react.createElement("line", keyValueList(props, 1));
}

export function makeSvg(style, elements) {
    return react.createElement("svg", keyValueList(style, 1), ...elements);
}

export function makeLine(style) {
    return react.createElement("line", keyValueList(style, 1));
}

export function makeText(style, t) {
    return react.createElement("text", keyValueList(style, 1), t);
}

export function backgroundSvg(model) {
    const clkLine = (x) => makeLinePoints(singleton(new HTMLAttr(65, "clkLineStyle")), x, vPos, x, (vPos + sigHeight) + spacing);
    const array = Uint32Array.from(rangeNumber(1, 1, model.SimParams.LastClkTime + 1));
    return map_2((arg) => clkLine((arg * model.SimParams.ClkSvgWidth)), array);
}

export function button(options, func, label) {
    return button_1(append_1(options, singleton(new Option(17, func))), singleton(label));
}

function charList2String(charLst) {
    let strings;
    const list = map_4((value) => value, charLst);
    strings = list;
    return join("", strings);
}

export function dec2bin(n, nBits) {
    let list_1;
    const list = ofSeq(rangeNumber(0, 1, nBits - 1));
    list_1 = reverse(list);
    return map_4((bitNum) => {
        if (equals_1(op_BitwiseAnd(n, op_LeftShift(fromOne(), ~(~bitNum))), fromZero())) {
            return "0";
        }
        else {
            return "1";
        }
    }, list_1);
}

export function dec2hex(n, nBits) {
    let seqPad;
    const times = ((4 - ((~(~nBits)) % 4)) % 4) | 0;
    seqPad = replicate(times, "0");
    let paddedBin;
    let source;
    const source2 = dec2bin(n, nBits);
    source = append_2(seqPad, source2);
    paddedBin = ofSeq(source);
    let fourBit2HexDig;
    const bit = (vec, n_1) => {
        if (((1 << n_1) & vec) === 0) {
            return "0";
        }
        else {
            return "1";
        }
    };
    const hexDigOf = (n_2) => {
        let clo1;
        return (clo1 = toText(printf("%x")), clo1(n_2))[0];
    };
    let elements;
    const list = ofSeq(rangeNumber(0, 1, 15));
    elements = map_4((dig) => [map_4(partialApply(1, bit, [dig]), ofSeq(rangeNumber(3, -1, 0))), hexDigOf(dig)], list);
    fourBit2HexDig = ofList(elements);
    let charLst;
    const list_1 = ofSeq(rangeNumber(0, 4, length(paddedBin) - 4));
    charLst = map_4((i) => FSharpMap__get_Item(fourBit2HexDig, getSlice(i, i + 3, paddedBin)), list_1);
    return charList2String(charLst);
}

export function dec2sdec(n, nBits) {
    const value = (item(0, dec2bin(n, nBits)) === "1") ? op_Subtraction(n, fromInt32(Math.pow(2, nBits))) : n;
    return toString(value);
}

export function n2StringOfRadix(hasRadixPrefix, n, nBits, rad) {
    const pref = (rad.tag === 2) ? "0b" : ((rad.tag === 0) ? "0x" : "");
    let numberRepStr;
    switch (rad.tag) {
        case 2: {
            const charLst = dec2bin(n, nBits);
            numberRepStr = charList2String(charLst);
            break;
        }
        case 0: {
            numberRepStr = dec2hex(n, nBits);
            break;
        }
        case 3: {
            numberRepStr = dec2sdec(n, nBits);
            break;
        }
        default: {
            numberRepStr = toString(n);
        }
    }
    return (hasRadixPrefix ? pref : "") + numberRepStr;
}

export function transitions(waveData) {
    const isDiff = (tupledArg) => {
        let b, a;
        const ws1 = tupledArg[0];
        const ws2 = tupledArg[1];
        const folder = (state, e1, e2) => {
            const matchValue = [state, equals(e1, e2)];
            let pattern_matching_result;
            if (matchValue[0] === 0) {
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
                    return 0;
                }
                case 1: {
                    return 1;
                }
            }
        };
        const matchValue_1 = [ws1, ws2];
        let pattern_matching_result_1, a_1, b_1, a_2, b_2;
        if (matchValue_1[0].tag === 1) {
            if (matchValue_1[1].tag === 1) {
                if (b = matchValue_1[1].fields[0], (a = matchValue_1[0].fields[0], a.length === b.length)) {
                    pattern_matching_result_1 = 1;
                    a_2 = matchValue_1[0].fields[0];
                    b_2 = matchValue_1[1].fields[0];
                }
                else {
                    pattern_matching_result_1 = 2;
                }
            }
            else {
                pattern_matching_result_1 = 2;
            }
        }
        else if (matchValue_1[1].tag === 0) {
            pattern_matching_result_1 = 0;
            a_1 = matchValue_1[0].fields[0];
            b_1 = matchValue_1[1].fields[0];
        }
        else {
            pattern_matching_result_1 = 2;
        }
        switch (pattern_matching_result_1) {
            case 0: {
                if (equals_1(a_1.BitData, b_1.BitData)) {
                    return 0;
                }
                else {
                    return 1;
                }
            }
            case 1: {
                return fold2(folder, 0, a_2, b_2) | 0;
            }
            case 2: {
                return 1;
            }
        }
    };
    const array_2 = transpose(waveData);
    return map_2((arg) => {
        let array_1;
        array_1 = pairwise(arg);
        return map_2(isDiff, array_1, Int32Array);
    }, array_2);
}

export function makeGaps(trans) {
    let array_3;
    let array_2;
    let array_1;
    let tuple;
    const array = append(trans, new Int32Array([1]), Int32Array);
    tuple = mapFold((tot, t) => [tot, tot + t], 0, array, Int32Array);
    array_1 = tuple[0];
    array_2 = indexed(array_1);
    array_3 = groupBy((tuple_1) => tuple_1[1], array_2, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    return map_2((tupledArg) => {
        const gL = tupledArg[1];
        const times = map_2((tuple_2) => tuple_2[0], gL, Int32Array);
        return new WaveGapT((max_1(times, {
            Compare: comparePrimitives,
        }) - min_1(times, {
            Compare: comparePrimitives,
        })) + 1, min_1(times, {
            Compare: comparePrimitives,
        }));
    }, array_3);
}

function busLabelPositions(wSModel, wave, gaps) {
    const clkWidth = wSModel.SimParams.ClkSvgWidth;
    const nSpaces = (gap) => (((gap.GapLen * clkWidth) / (maxBusValGap + 1)) + 2);
    const busLabelXPosition = (g, i) => (g.GapStart + ((i * g.GapLen) / nSpaces(g)));
    return map_2((gap_1) => ({
        WaveValue: wave[gap_1.GapStart],
        XPosArray: map_2(partialApply(1, busLabelXPosition, [gap_1]), Int32Array.from(rangeNumber(1, 1, (~(~nSpaces(gap_1))) - 1)), Float64Array),
    }), gaps);
}

function busLabels(wSModel, waveData) {
    const array1 = transpose(waveData);
    const array2 = map_2(makeGaps, transitions(waveData));
    return map2((wave, gaps) => busLabelPositions(wSModel, wave, gaps), array1, array2);
}

function busLabelRepeats(wsMod, busLabelValAndPos) {
    let w;
    const addLabel = (nLabels) => ((xInd) => {
        const style = inWaveLabel(nLabels, xInd, wsMod);
        return (t) => makeText(style, t);
    });
    const matchValue = busLabelValAndPos.WaveValue;
    let pattern_matching_result, w_1;
    if (matchValue.tag === 0) {
        if (w = matchValue.fields[0], w.NBits > 1) {
            pattern_matching_result = 0;
            w_1 = matchValue.fields[0];
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
            return map_2((xInd_1) => addLabel(1)(xInd_1)(n2StringOfRadix(true, w_1.BitData, w_1.NBits, wsMod.WaveViewerRadix)), busLabelValAndPos.XPosArray);
        }
        case 1: {
            return [];
        }
    }
}

function makeClkSegment(clkW, xInd) {
    const top = spacing;
    const bot = (top + sigHeight) - sigLineThick;
    const left = xInd * clkW;
    const right = left + clkW;
    const mid = left + (clkW / 2);
    let makeSigLine;
    const style = ofArray_1([new HTMLAttr(65, "sigLineStyle"), ["style", {
        stroke: "blue",
    }]]);
    makeSigLine = ((tupledArg) => ((tupledArg_1) => makeLinePoints(style, tupledArg[0], tupledArg[1], tupledArg_1[0], tupledArg_1[1])));
    const clkPoints = ofArray_1([[left, top], [mid, top], [mid, bot], [right, bot], [right, top]]);
    let list_2;
    let list_1;
    list_1 = pairwise_1(clkPoints);
    list_2 = map_4((tupledArg_2) => {
        const p1 = tupledArg_2[0];
        const p2 = tupledArg_2[1];
        return makeSigLine(p1)(p2);
    }, list_1);
    return Array.from(list_2);
}

function makeSegment(clkW, xInd, data, trans_0, trans_1) {
    let n, w;
    const trans = [trans_0, trans_1];
    const top = spacing;
    const bot = (top + sigHeight) - sigLineThick;
    const left = xInd * clkW;
    const right = left + clkW;
    let makeSigLine;
    const style = ofArray_1([new HTMLAttr(65, "sigLineStyle"), ["style", {
        stroke: "blue",
    }]]);
    makeSigLine = ((tupledArg) => ((tupledArg_1) => makeLinePoints(style, tupledArg[0], tupledArg[1], tupledArg_1[0], tupledArg_1[1])));
    let pattern_matching_result, w_1;
    if (data.tag === 0) {
        if (w = data.fields[0], w.NBits === 1) {
            pattern_matching_result = 0;
            w_1 = data.fields[0];
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
            let y;
            const matchValue = w_1.BitData;
            if (n = matchValue, equals_1(n, fromInt32(1))) {
                const n_1 = matchValue;
                y = top;
            }
            else {
                y = bot;
            }
            const sigLine = makeSigLine([left, y])([right, y]);
            let array2;
            const matchValue_1 = trans[1] | 0;
            switch (matchValue_1) {
                case 0: {
                    array2 = [];
                    break;
                }
                case 1: {
                    array2 = [makeSigLine([right, bot + (sigLineThick / 2)])([right, top - (sigLineThick / 2)])];
                    break;
                }
                default: {
                    throw (new Error("What? Transition has value other than 0 or 1"));
                }
            }
            const array1 = [sigLine];
            return append(array1, array2);
        }
        case 1: {
            const leftInner = (trans[0] === 1) ? (left + transLen) : left;
            const rightInner = (trans[1] === 1) ? (right - transLen) : right;
            const cen = (top + bot) / 2;
            const topL = makeSigLine([leftInner, top])([rightInner, top]);
            const botL = makeSigLine([leftInner, bot])([rightInner, bot]);
            const topLeft = makeSigLine([left, cen])([leftInner, top]);
            const botLeft = makeSigLine([left, cen])([leftInner, bot]);
            const topRight = makeSigLine([right, cen])([rightInner, top]);
            const botRight = makeSigLine([right, cen])([rightInner, bot]);
            let array2_1;
            let pattern_matching_result_1;
            if (trans[0] === 0) {
                if (trans[1] === 0) {
                    pattern_matching_result_1 = 3;
                }
                else if (trans[1] === 1) {
                    pattern_matching_result_1 = 2;
                }
                else {
                    pattern_matching_result_1 = 4;
                }
            }
            else if (trans[0] === 1) {
                if (trans[1] === 0) {
                    pattern_matching_result_1 = 1;
                }
                else if (trans[1] === 1) {
                    pattern_matching_result_1 = 0;
                }
                else {
                    pattern_matching_result_1 = 4;
                }
            }
            else {
                pattern_matching_result_1 = 4;
            }
            switch (pattern_matching_result_1) {
                case 0: {
                    array2_1 = [topLeft, botLeft, topRight, botRight];
                    break;
                }
                case 1: {
                    array2_1 = [topLeft, botLeft];
                    break;
                }
                case 2: {
                    array2_1 = [topRight, botRight];
                    break;
                }
                case 3: {
                    array2_1 = [];
                    break;
                }
                case 4: {
                    throw (new Error("What? Transition has value other than 0 or 1"));
                    break;
                }
            }
            const array1_1 = [topL, botL];
            return append(array1_1, array2_1);
        }
    }
}

export function clkRulerSvg(model) {
    const makeClkRulLbl = (i) => {
        let clkW;
        const matchValue = model.SimParams.ClkSvgWidth;
        if (clkW = matchValue, (clkW < 0.5) ? ((i % 5) !== 0) : false) {
            const clkW_1 = matchValue;
            return [];
        }
        else {
            return [makeText(cursRectText(model, i), int32ToString(i))];
        }
    };
    let elements;
    let array2;
    const array = Int32Array.from(rangeNumber(0, 1, ~(~model.SimParams.LastClkTime)));
    array2 = collect(makeClkRulLbl, array);
    const array1 = backgroundSvg(model);
    elements = append(array1, array2);
    const style = clkRulerStyle(model);
    return makeSvg(style, elements);
}

export function makeClkSvg(sampArr, wsMod) {
    let arrays;
    const array = Int32Array.from(rangeNumber(0, 1, sampArr.length - 1));
    let mapping;
    const clkW = wsMod.ClkWidth / 2;
    mapping = ((xInd) => makeClkSegment(clkW, xInd));
    arrays = map_2(mapping, array);
    return concat(arrays);
}

export function waveSvg(wsMod, waveData) {
    let valueLabels;
    const array_1 = busLabels(wsMod, waveData);
    valueLabels = map_2((array) => collect((busLabelValAndPos) => busLabelRepeats(wsMod.SimParams, busLabelValAndPos), array), array_1);
    const makeWaveSvg = (sampArr, transArr) => {
        let arrays;
        arrays = mapIndexed2((xInd, data, tupledArg) => makeSegment(wsMod.SimParams.ClkSvgWidth, xInd, data, tupledArg[0], tupledArg[1]), sampArr, transArr);
        return concat(arrays);
    };
    const padTrans = (t) => {
        const matchValue = t.length | 0;
        switch (matchValue) {
            case 0: {
                return [[1, 1]];
            }
            case 1: {
                return [[1, t[0]], [t[0], 1]];
            }
            default: {
                const pairs = pairwise(t);
                return concat([[[1, pairs[0][0]]], pairs, [[last(pairs)[1], 1]]]);
            }
        }
    };
    let array2_3;
    let array2_1;
    const array_2 = transitions(waveData);
    array2_1 = map_2(padTrans, array_2);
    const array1_1 = transpose(waveData);
    array2_3 = map2(makeWaveSvg, array1_1, array2_1);
    return map2(append, valueLabels, array2_3);
}

export function addSVGToWaveSimModel(wSModel) {
    let arg10, arg20, clo1, clo2, clo3, children_6, props_4, children_4;
    const waveData = getAllWaveSimDataBySample(wSModel);
    const waveTableRow = (rowClass, cellClass, svgClass, svgChildren) => {
        let children;
        const children_2 = [(children = [makeSvg(svgClass, svgChildren)], react.createElement("td", keyValueList(cellClass, 1), ...children))];
        return react.createElement("tr", keyValueList(rowClass, 1), ...children_2);
    };
    const bgSvg = backgroundSvg(wSModel);
    const lastRow = [(arg10 = lwaveCell(wSModel), arg20 = waveCellSvg(wSModel, true), (clo1 = partialApply(3, waveTableRow, [[new HTMLAttr(65, "fullHeight")]]), clo2 = clo1(arg10), clo3 = clo2(arg20), clo3(bgSvg)))];
    const firstRow = [(children_6 = [(props_4 = waveCell(wSModel), (children_4 = [clkRulerSvg(wSModel)], react.createElement("td", keyValueList(props_4, 1), ...children_4)))], react.createElement("tr", {
        className: "rowHeight",
    }, ...children_6))];
    let midRows;
    let elements;
    let array;
    const array2 = waveSvg(wSModel, waveData);
    array = zip_1(wSModel.SimParams.DispNames, array2);
    elements = map_2((tupledArg) => {
        const name = tupledArg[0];
        const wave = tupledArg[1];
        const waveWithBg = append(bgSvg, wave);
        let svg;
        const arg10_1 = waveCell(wSModel);
        const arg20_1 = waveCellSvg(wSModel, false);
        const clo1_1 = partialApply(3, waveTableRow, [[new HTMLAttr(65, "rowHeight")]]);
        const clo2_1 = clo1_1(arg10_1);
        const clo3_1 = clo2_1(arg20_1);
        svg = clo3_1(waveWithBg);
        return [name, svg];
    }, array);
    midRows = ofArray(elements);
    const svgs = new SVGCacheT(firstRow, midRows, lastRow);
    return new WaveSimModel(wSModel.InitWaveSimGraph, wSModel.SimParams, wSModel.AllWaveNames, wSModel.AllNets, svgs, wSModel.SimDataCache, wSModel.CursorBoxIsEmpty, wSModel.WSViewState, wSModel.WSTransition, wSModel.LastCanvasState);
}

export function initFileWS(model, dispatch) {
    let tupledArg;
    const netListOpt = getSheetWaveNetList(model);
    const matchValue = [getCurrFile(model), netListOpt];
    let pattern_matching_result, _netList, fileName;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 0;
            _netList = matchValue[1];
            fileName = matchValue[0];
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
            dispatch((tupledArg = [fileName, initWS([], empty())], (new Msg(6, tupledArg[0], tupledArg[1]))));
            break;
        }
        case 1: {
            break;
        }
    }
}

export function makeLabels(waveNames) {
    const makeLbl = (l) => react.createElement("label", {
        className: "waveLbl",
    }, l);
    return map_2(makeLbl, waveNames);
}

export function adjustPars(wsMod, pars, rightLim) {
    const currPars = wsMod.SimParams;
    let defRightLim;
    let matchValue;
    const ws = wsMod;
    matchValue = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
    defRightLim = (((!equalsWith(compareSafe, matchValue, null)) ? (matchValue.length === 0) : false) ? 600 : maxWavesColWidthFloat(wsMod));
    let rightLim_1;
    if (rightLim == null) {
        rightLim_1 = defRightLim;
    }
    else {
        const x_1 = rightLim;
        rightLim_1 = x_1;
    }
    let lastClkTime;
    let e2_2;
    let e2_1;
    let y_2;
    let e2;
    const value = rightLim_1 / (currPars.ClkSvgWidth * 40);
    e2 = (value >>> 0);
    y_2 = max_2(comparePrimitives, currPars.CursorTime, e2);
    e2_1 = (10 + y_2);
    e2_2 = max_2(comparePrimitives, pars.LastClkTime, e2_1);
    lastClkTime = min_2(comparePrimitives, maxLastClk, e2_2);
    return new SimParamsT(pars.WaveViewerRadix, lastClkTime, pars.CursorTime, pars.ClkSvgWidth, pars.DispNames, pars.LastScrollPos);
}

export function simulateAndMakeWaves(model, wsMod, par) {
    let newData;
    let _arg1;
    const nCycles = (par.LastClkTime + 1) - (wsMod.SimDataCache.length >>> 0);
    _arg1 = appendSimData(model, wsMod, nCycles);
    if (_arg1 == null) {
        newData = toFail(printf("No simulation data when Some are expected"));
    }
    else {
        const copyOfStruct = _arg1;
        if (copyOfStruct.tag === 1) {
            const e = copyOfStruct.fields[0];
            const clo1 = toFail(printf("%A"));
            newData = clo1(e);
        }
        else {
            const dat = copyOfStruct.fields[0];
            newData = dat;
        }
    }
    const par$0027 = new SimParamsT(par.WaveViewerRadix, par.LastClkTime, par.CursorTime, par.ClkSvgWidth, par.DispNames, par.LastScrollPos);
    const wSModel = new WaveSimModel(wsMod.InitWaveSimGraph, par$0027, wsMod.AllWaveNames, wsMod.AllNets, wsMod.DispWaveSVGCache, newData, wsMod.CursorBoxIsEmpty, wsMod.WSViewState, wsMod.WSTransition, wsMod.LastCanvasState);
    return addSVGToWaveSimModel(wSModel);
}

function isConnInNLTrgtLst(connId, nlTrgtLst) {
    return exists_1((nlTrgt) => equalsSafe(nlTrgt.TargetConnId, connId), nlTrgtLst);
}

function isCompInNLTrgtLst(nlComponent, nlTrgtLst) {
    if (exists((_arg1, compNLTrgtLst) => equals(compNLTrgtLst, nlTrgtLst), nlComponent.Outputs)) {
        return true;
    }
    else {
        return exists_1((nlTrgt) => equalsSafe(nlTrgt.TargetCompId, nlComponent.Id), nlTrgtLst);
    }
}

function isNLTrgtLstSelected(netList, _arg1_0, _arg1_1, nlTrgtLst) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    if (exists_1((comp) => {
        const matchValue = tryFind(new ComponentId(0, comp.Id), netList);
        if (matchValue == null) {
            return false;
        }
        else {
            const comp_1 = matchValue;
            return isCompInNLTrgtLst(comp_1, nlTrgtLst);
        }
    }, comps)) {
        return true;
    }
    else {
        return exists_1((conn) => isConnInNLTrgtLst(new ConnectionId(0, conn.Id), nlTrgtLst), conns);
    }
}

function isNetGroupSelected(netList, _arg1_0, _arg1_1, trgtLstGroup) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    const array = append([trgtLstGroup.driverNet], trgtLstGroup.connectedNets);
    return array.some((nlTrgtLst) => isNLTrgtLstSelected(netList, comps, conns, nlTrgtLst));
}

export function isWaveSelected(diagram, netList, netgrp) {
    const matchValue = Draw2dWrapper__GetSelected(diagram);
    if (matchValue != null) {
        const selectedCompsConnsJS = matchValue;
        const selectedCompsConns = extractState(selectedCompsConnsJS[0], selectedCompsConnsJS[1]);
        return isNetGroupSelected(netList, selectedCompsConns[0], selectedCompsConns[1], netgrp);
    }
    else {
        return false;
    }
}

export function showSimulationLoading(wsModel, dispatch) {
    const nv = wsModel.WSTransition;
    const v = wsModel.WSViewState;
    if (nv != null) {
        dispatch(new Msg(47));
        return true;
    }
    else {
        return false;
    }
}

export function getAllNetGroups(waveSim) {
    return mapValues(waveSim.AllNets);
}

export function highlightConnectionsFromNetGroups(model, dispatch) {
    const matchValue = currWaveSimModel(model);
    if (matchValue != null) {
        const wSModel = matchValue;
        let netList;
        let option_1;
        option_1 = map_3((tupledArg) => getNetList(tupledArg[0], tupledArg[1]), model.LastSimulatedCanvasState);
        const value = empty();
        netList = defaultArg(option_1, value);
        let netGroups;
        const matchValue_1 = wSModel.WSViewState;
        switch (matchValue_1.tag) {
            case 1: {
                netGroups = netList2NetGroups(netList);
                break;
            }
            case 3: {
                const ws = wSModel;
                netGroups = map_2((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
                break;
            }
            case 0: {
                netGroups = [];
                break;
            }
            default: {
                netGroups = netList2NetGroups(netList);
            }
        }
        const selectedConnectionIds = (ng) => {
            if (isWaveSelected(model.Diagram, netList, ng)) {
                return wave2ConnIds(ng);
            }
            else {
                return [];
            }
        };
        const selectedIds = collect(selectedConnectionIds, netGroups);
        dispatch(new Msg(14, selectedIds));
    }
}

export function fileMenuViewActions(model, dispatch) {
    if (model.ConnsOfSelectedWavesAreHighlighted) {
        highlightConnectionsFromNetGroups(model, dispatch);
    }
}

export function cursorValueStrings(wSMod) {
    const paras = wSMod.SimParams;
    const makeCursVal = (sample) => {
        let w;
        let pattern_matching_result, w_1;
        if (sample.tag === 0) {
            if (w = sample.fields[0], w.NBits > 1) {
                pattern_matching_result = 0;
                w_1 = sample.fields[0];
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
                return [n2StringOfRadix(true, w_1.BitData, w_1.NBits, paras.WaveViewerRadix)];
            }
            case 1: {
                if (sample.tag === 1) {
                    const s = sample.fields[0];
                    return s;
                }
                else {
                    const w_2 = sample.fields[0];
                    return [toString(w_2.BitData)];
                }
            }
        }
    };
    const matchValue = (~(~paras.CursorTime)) < wSMod.SimDataCache.length;
    if (matchValue) {
        return map_2(makeCursVal, getWaveSimDataOneSample(wSMod, ~(~paras.CursorTime)));
    }
    else {
        return [];
    }
}

export function isCursorVisible(wSMod, divWidth, scrollPos) {
    let cursLeftPos;
    cursLeftPos = cursorLeftPx(wSMod.SimParams, wSMod.SimParams.CursorTime);
    const cursMid = cursLeftPos + ((wSMod.SimParams.ClkSvgWidth * 40) / 2);
    const leftScreenLim = scrollPos;
    const rightScreenLim = leftScreenLim + divWidth;
    if (cursLeftPos >= cursMid) {
        return cursMid <= rightScreenLim;
    }
    else {
        return false;
    }
}

export function makeCursorVisiblePos(wSMod, divWidth) {
    let cursLeftPos;
    cursLeftPos = cursorLeftPx(wSMod.SimParams, wSMod.SimParams.CursorTime);
    const cursMid = cursLeftPos + ((wSMod.SimParams.ClkSvgWidth * 40) / 2);
    return cursMid - (divWidth / 2);
}

export class SheetInfo extends Record {
    constructor(SheetName, Canvas, SheetGraph, Instance, SheetPath, InstancePath, NetL) {
        super();
        this.SheetName = SheetName;
        this.Canvas = Canvas;
        this.SheetGraph = SheetGraph;
        this.Instance = Instance;
        this.SheetPath = SheetPath;
        this.InstancePath = InstancePath;
        this.NetL = NetL;
    }
}

export function SheetInfo$reflection() {
    return record_type("WaveSimHelpers.SheetInfo", [], SheetInfo, () => [["SheetName", ComponentId$reflection()], ["Canvas", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["SheetGraph", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()])], ["Instance", tuple_type(string_type, ComponentLabel$reflection())], ["SheetPath", list_type(ComponentId$reflection())], ["InstancePath", list_type(tuple_type(string_type, ComponentLabel$reflection()))], ["NetL", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), NetListComponent$reflection()])]]);
}

export function getSubSheets(sheets, instances, graph, model) {
    let list;
    const array = mapValues(graph);
    list = ofArray_1(array);
    return collect_1((sComp) => {
        const label = sComp.Label;
        const matchValue = sComp.Type;
        if (matchValue.tag === 16) {
            const cusComp = matchValue.fields[0];
            let canvas;
            const matchValue_1 = model.CurrentProj;
            if (matchValue_1 != null) {
                const lst = matchValue_1.LoadedComponents;
                const matchValue_2 = tryFind_1((ldComp) => (ldComp.Name === cusComp.Name), lst);
                if (matchValue_2 != null) {
                    const ldComp_1 = matchValue_2;
                    canvas = ldComp_1.CanvasState;
                }
                else {
                    canvas = toFail(printf("Can\u0027t find canvas for sheet"));
                }
            }
            else {
                canvas = toFail(printf("Can\u0027t find project"));
            }
            const matchValue_3 = sComp.CustomSimulationGraph;
            if (matchValue_3 != null) {
                const sg = matchValue_3;
                let thisSheet;
                const SheetPath = reverse(sheets);
                const InstancePath = reverse(instances);
                thisSheet = (new SheetInfo(sComp.Id, canvas, sg, [cusComp.Name, sComp.Label], SheetPath, InstancePath, getNetList(canvas[0], canvas[1])));
                const otherSheets = getSubSheets(cons(sComp.Id, sheets), cons(thisSheet.Instance, instances), graph, model);
                return cons(thisSheet, otherSheets);
            }
            else {
                const clo1 = toFail(printf("Custom compnent without simulation graph? InstancePath=%A, Instance=%A"));
                const clo2 = clo1(instances);
                return clo2(label);
            }
        }
        else {
            return empty_1();
        }
    }, list);
}

