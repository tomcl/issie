import { Union, Record } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { ConnectionId, PortType, Port, ComponentType, OutputPortId, ComponentId, InputPortId, OutputPortId$reflection, InputPortId$reflection, ComponentId$reflection, Port$reflection, Component$reflection, Connection$reflection } from "../Common/CommonTypes.fs.js";
import { union_type, tuple_type, int32_type, record_type, class_type, string_type, list_type } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { append, tryHead, length, tryFind, ofArray, tryPick, singleton, groupBy, empty, cons, fold, filter, map as map_1, collect } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { equals, structuralHash, uncurry, equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { FSharpMap__get_IsEmpty, filter as filter_1, FSharpMap__TryFind, toList, FSharpMap__Add, FSharpMap__get_Item, ofList } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { join, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { SimulationError } from "./SimulatorTypes.fs.js";
import { flatten, map as map_2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { tryFindError } from "../Common/Helpers.fs.js";
import { inferConnectionsWidth } from "../Common/WidthInferer.fs.js";

export class MapData extends Record {
    constructor(Connections, Components, LabComp, LabGroup, LabInputPorts, LabOutputPorts, LabTargetConns, OtherTargetConns, LabSourceConns, OtherSourceConns, OtherInputPorts, OtherOutputPorts, ToComp, ToInputPort, ToOutputPort) {
        super();
        this.Connections = Connections;
        this.Components = Components;
        this.LabComp = LabComp;
        this.LabGroup = LabGroup;
        this.LabInputPorts = LabInputPorts;
        this.LabOutputPorts = LabOutputPorts;
        this.LabTargetConns = LabTargetConns;
        this.OtherTargetConns = OtherTargetConns;
        this.LabSourceConns = LabSourceConns;
        this.OtherSourceConns = OtherSourceConns;
        this.OtherInputPorts = OtherInputPorts;
        this.OtherOutputPorts = OtherOutputPorts;
        this.ToComp = ToComp;
        this.ToInputPort = ToInputPort;
        this.ToOutputPort = ToOutputPort;
    }
}

export function MapData$reflection() {
    return record_type("CanvasStateAnalyser.MapData", [], MapData, () => [["Connections", list_type(Connection$reflection())], ["Components", list_type(Component$reflection())], ["LabComp", list_type(Component$reflection())], ["LabGroup", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, list_type(Component$reflection())])], ["LabInputPorts", list_type(Port$reflection())], ["LabOutputPorts", list_type(Port$reflection())], ["LabTargetConns", list_type(Connection$reflection())], ["OtherTargetConns", list_type(Connection$reflection())], ["LabSourceConns", list_type(Connection$reflection())], ["OtherSourceConns", list_type(Connection$reflection())], ["OtherInputPorts", list_type(Port$reflection())], ["OtherOutputPorts", list_type(Port$reflection())], ["ToComp", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), Component$reflection()])], ["ToInputPort", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [InputPortId$reflection(), Port$reflection()])], ["ToOutputPort", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [OutputPortId$reflection(), Port$reflection()])]]);
}

function getAllInputPortIds(components) {
    return collect((comp) => map_1((port) => [new InputPortId(0, port.Id), new ComponentId(0, comp.Id)], comp.InputPorts), components);
}

function getAllOutputPortIds(components) {
    return collect((comp) => map_1((port) => [new OutputPortId(0, port.Id), new ComponentId(0, comp.Id)], comp.OutputPorts), components);
}

function genMaps(_arg1_0, _arg1_1) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    const labComps = filter((co) => equalsSafe(co.Type, new ComponentType(2)), comps);
    let idToComp;
    let elements;
    elements = map_1((co_1) => [new ComponentId(0, co_1.Id), co_1], comps);
    idToComp = ofList(elements);
    const targetIsLabel = (c) => equalsSafe(FSharpMap__get_Item(idToComp, new ComponentId(0, c.Target.HostId)).Type, new ComponentType(2));
    const sourceIsLabel = (c_1) => equalsSafe(FSharpMap__get_Item(idToComp, new ComponentId(0, c_1.Source.HostId)).Type, new ComponentType(2));
    const splitBy = (pred, lst) => fold(uncurry(2, (tupledArg) => {
        const is = tupledArg[0];
        const isNot = tupledArg[1];
        return (x) => (pred(x) ? [cons(x, is), isNot] : [is, cons(x, isNot)]);
    }), [empty(), empty()], lst);
    const patternInput = splitBy(targetIsLabel, conns);
    const otherTargetConns = patternInput[1];
    const labTargetConns = patternInput[0];
    const patternInput_1 = splitBy(sourceIsLabel, conns);
    const otherSourceConns = patternInput_1[1];
    const labSourceConns = patternInput_1[0];
    const normalise = (p) => (new Port(p.Id, void 0, p.PortType, p.HostId));
    const normaliseL = (pL) => map_1(normalise, pL);
    let idToInputPort;
    let elements_1;
    let list_3;
    list_3 = collect((co_2) => co_2.InputPorts, comps);
    elements_1 = map_1((po) => [new InputPortId(0, po.Id), normalise(po)], list_3);
    idToInputPort = ofList(elements_1);
    let idToOutputPort;
    let elements_2;
    let list_5;
    list_5 = collect((co_3) => co_3.OutputPorts, comps);
    elements_2 = map_1((po_1) => [new OutputPortId(0, po_1.Id), normalise(po_1)], list_5);
    idToOutputPort = ofList(elements_2);
    let otherInputPorts;
    let list_7;
    list_7 = filter((co_4) => (!equalsSafe(co_4.Type, new ComponentType(2))), comps);
    otherInputPorts = collect((co_5) => normaliseL(co_5.InputPorts), list_7);
    let otherOutputPorts;
    let list_9;
    list_9 = filter((co_6) => (!equalsSafe(co_6.Type, new ComponentType(2))), comps);
    otherOutputPorts = collect((co_7) => normaliseL(co_7.OutputPorts), list_9);
    let labGroup;
    const elements_3 = groupBy((co_8) => co_8.Label, labComps, {
        Equals: (x_1, y) => (x_1 === y),
        GetHashCode: structuralHash,
    });
    labGroup = ofList(elements_3);
    let labInputPorts;
    labInputPorts = collect((co_9) => normaliseL(co_9.InputPorts), labComps);
    let labOutputPorts;
    labOutputPorts = collect((co_10) => normaliseL(co_10.OutputPorts), labComps);
    return new MapData(conns, comps, labComps, labGroup, labInputPorts, labOutputPorts, labTargetConns, otherTargetConns, labSourceConns, otherSourceConns, otherInputPorts, otherOutputPorts, idToComp, idToInputPort, idToOutputPort);
}

function checkPortTypesAreConsistent(canvasState_0, canvasState_1) {
    const canvasState = [canvasState_0, canvasState_1];
    const checkComponentPorts = (ports_mut, correctType_mut) => {
        let port, clo1, arg20, clo1_1, clo2, clo3, port_2;
        checkComponentPorts:
        while (true) {
            const ports = ports_mut, correctType = correctType_mut;
            if (ports.tail != null) {
                if (port = ports.head, equals(port.PortNumber, void 0)) {
                    const port_1 = ports.head;
                    return new SimulationError((clo1 = toText(printf("%A port appears to not have a port number")), clo1(correctType)), void 0, singleton(new ComponentId(0, port_1.HostId)), empty());
                }
                else {
                    let pattern_matching_result, port_3;
                    if (ports.tail != null) {
                        if (port_2 = ports.head, !equalsSafe(port_2.PortType, correctType)) {
                            pattern_matching_result = 0;
                            port_3 = ports.head;
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
                            return new SimulationError((arg20 = (port_3.PortNumber | 0), (clo1_1 = toText(printf("%A port %d appears to be an %A port")), clo2 = clo1_1(correctType), clo3 = clo2(arg20), clo3(port_3.PortType))), void 0, singleton(new ComponentId(0, port_3.HostId)), empty());
                        }
                        case 1: {
                            if (ports.tail != null) {
                                const ports$0027 = ports.tail;
                                ports_mut = ports$0027;
                                correctType_mut = correctType;
                                continue checkComponentPorts;
                            }
                            else {
                                throw (new Error("The match cases were incomplete"));
                            }
                        }
                    }
                }
            }
            else {
                return void 0;
            }
            break;
        }
    };
    const checkComponentsPorts = (components_mut) => {
        checkComponentsPorts:
        while (true) {
            const components = components_mut;
            if (components.tail != null) {
                const components$0027 = components.tail;
                const comp = components.head;
                const matchValue = [checkComponentPorts(comp.InputPorts, new PortType(0)), checkComponentPorts(comp.OutputPorts, new PortType(1))];
                let pattern_matching_result_1, err;
                if (matchValue[0] == null) {
                    if (matchValue[1] == null) {
                        pattern_matching_result_1 = 1;
                    }
                    else {
                        pattern_matching_result_1 = 0;
                        err = matchValue[1];
                    }
                }
                else {
                    pattern_matching_result_1 = 0;
                    err = matchValue[0];
                }
                switch (pattern_matching_result_1) {
                    case 0: {
                        return err;
                    }
                    case 1: {
                        components_mut = components$0027;
                        continue checkComponentsPorts;
                    }
                }
            }
            else {
                return void 0;
            }
            break;
        }
    };
    const checkConnectionPort = (port_4, correctType_1, connId) => {
        let clo1_3, clo2_2, clo1_2, clo2_1;
        const matchValue_1 = [equalsSafe(port_4.PortType, correctType_1), port_4.PortNumber];
        if (matchValue_1[0]) {
            if (matchValue_1[1] == null) {
                return void 0;
            }
            else {
                const pNumber = matchValue_1[1] | 0;
                return new SimulationError((clo1_3 = toText(printf("%A port appears to have a port number: %d")), clo2_2 = clo1_3(correctType_1), clo2_2(pNumber)), void 0, singleton(new ComponentId(0, port_4.HostId)), singleton(new ConnectionId(0, connId)));
            }
        }
        else {
            return new SimulationError((clo1_2 = toText(printf("%A port appears to be an %A port")), clo2_1 = clo1_2(correctType_1), clo2_1(port_4.PortType)), void 0, singleton(new ComponentId(0, port_4.HostId)), singleton(new ConnectionId(0, connId)));
        }
    };
    const checkConnectionsPorts = (connections_mut) => {
        checkConnectionsPorts:
        while (true) {
            const connections = connections_mut;
            if (connections.tail != null) {
                const connections$0027 = connections.tail;
                const conn = connections.head;
                const matchValue_2 = [checkConnectionPort(conn.Source, new PortType(1), conn.Id), checkConnectionPort(conn.Target, new PortType(0), conn.Id)];
                let pattern_matching_result_2, err_1;
                if (matchValue_2[0] == null) {
                    if (matchValue_2[1] == null) {
                        pattern_matching_result_2 = 1;
                    }
                    else {
                        pattern_matching_result_2 = 0;
                        err_1 = matchValue_2[1];
                    }
                }
                else {
                    pattern_matching_result_2 = 0;
                    err_1 = matchValue_2[0];
                }
                switch (pattern_matching_result_2) {
                    case 0: {
                        return err_1;
                    }
                    case 1: {
                        connections_mut = connections$0027;
                        continue checkConnectionsPorts;
                    }
                }
            }
            else {
                return void 0;
            }
            break;
        }
    };
    const connections_1 = canvasState[1];
    const components_1 = canvasState[0];
    const matchValue_3 = [checkComponentsPorts(components_1), checkConnectionsPorts(connections_1)];
    let pattern_matching_result_3, err_2;
    if (matchValue_3[0] == null) {
        if (matchValue_3[1] == null) {
            pattern_matching_result_3 = 1;
        }
        else {
            pattern_matching_result_3 = 0;
            err_2 = matchValue_3[1];
        }
    }
    else {
        pattern_matching_result_3 = 0;
        err_2 = matchValue_3[0];
    }
    switch (pattern_matching_result_3) {
        case 0: {
            return err_2;
        }
        case 1: {
            return void 0;
        }
    }
}

function checkEvery(counts, cond, errMsg) {
    return fold((maybeErr, tupledArg) => {
        const _arg1 = tupledArg[0];
        const count = tupledArg[1] | 0;
        const conns = _arg1[1];
        const comps = _arg1[0];
        if (maybeErr == null) {
            const matchValue = cond(count);
            if (matchValue) {
                return void 0;
            }
            else {
                return new SimulationError(toText(errMsg)(count), void 0, (map_1((comp) => (new ComponentId(0, comp.Id)), comps)), (map_1((conn) => (new ConnectionId(0, conn.Id)), conns)));
            }
        }
        else {
            const err = maybeErr;
            return err;
        }
    }, void 0, counts);
}

function countPortsConnections(conns, connMap, bins, binMap) {
    let elements;
    const countPortsConnections$0027 = (conns_1_mut, counts_mut) => {
        countPortsConnections$0027:
        while (true) {
            const conns_1 = conns_1_mut, counts = counts_mut;
            if (conns_1.tail != null) {
                const conns$0027 = conns_1.tail;
                const conn = conns_1.head;
                let countsRes;
                const key_1 = connMap(conn);
                const patternInput = FSharpMap__get_Item(counts, key_1);
                const binCount = patternInput[0] | 0;
                const binConns = patternInput[1];
                countsRes = FSharpMap__Add(counts, key_1, [binCount + 1, cons(conn, binConns)]);
                conns_1_mut = conns$0027;
                counts_mut = countsRes;
                continue countPortsConnections$0027;
            }
            else {
                let list;
                list = toList(counts);
                return map_1((tupledArg) => {
                    const key = tupledArg[0];
                    const _arg1 = tupledArg[1];
                    const count = _arg1[0] | 0;
                    const conns_2 = _arg1[1];
                    return [[binMap(key), conns_2], count];
                }, list);
            }
            break;
        }
    };
    return countPortsConnections$0027(conns, (elements = (map_1((b) => [b, [0, empty()]], bins)), (ofList(elements))));
}

function checkCounts(conns, connMap, bins, binMap, cond, errMsg) {
    const totals = countPortsConnections(conns, connMap, bins, binMap);
    return checkEvery(totals, cond, errMsg);
}

function checkConns(conns, m) {
    const compOfPort = (p) => FSharpMap__get_Item(m.ToComp, new ComponentId(0, p.HostId));
    let option;
    option = tryPick((conn) => {
        const s = compOfPort(conn.Source);
        const t = compOfPort(conn.Target);
        if (equalsSafe(s.Type, new ComponentType(2)) ? equalsSafe(t.Type, new ComponentType(2)) : false) {
            return [s, t, conn];
        }
        else {
            return void 0;
        }
    }, conns);
    return map_2((tupledArg) => {
        const s_1 = tupledArg[0];
        const t_1 = tupledArg[1];
        const conn_1 = tupledArg[2];
        const errMsg = toText(printf("You can\u0027t connect two Bus Labels with a wire. Delete the connecting wire. If you want to join two bus labels you need only give them the same name - then they will form a single net."));
        return new SimulationError(errMsg, void 0, ofArray([new ComponentId(0, s_1.Id), new ComponentId(0, t_1.Id)]), singleton(new ConnectionId(0, conn_1.Id)));
    }, option);
}

function checkPortsAreConnectedProperly(canvasState_0, canvasState_1) {
    let list_1;
    const canvasState = [canvasState_0, canvasState_1];
    const m = genMaps(canvasState[0], canvasState[1]);
    const conns = m.Connections;
    const portMap = (p) => singleton(FSharpMap__get_Item(m.ToComp, new ComponentId(0, p.HostId)));
    const inPIdMap = (pid) => FSharpMap__get_Item(m.ToInputPort, new InputPortId(0, pid));
    const labMap = (lab) => FSharpMap__get_Item(m.LabGroup, lab);
    const l2Pid = (lst) => map_1((x) => x.Id, lst);
    const list_2 = ofArray([checkCounts(m.OtherTargetConns, (conn) => conn.Target.Id, l2Pid(m.OtherInputPorts), (arg) => portMap(inPIdMap(arg)), (y) => (1 === y), printf("A component input port must have precisely one driving component, but %d were found. If you want to merge wires together use a MergeWires component")), checkCounts(m.LabTargetConns, (conn_1) => FSharpMap__get_Item(m.ToComp, new ComponentId(0, conn_1.Target.HostId)).Label, (list_1 = (toList(m.LabGroup)), (map_1((tuple) => tuple[0], list_1))), labMap, (y_1) => (1 === y_1), printf("A set of labelled wires must have precisely one driving component, but %d were found. If you want to merge wires together use a MergeWires component. If you are driving two labels from the same component delete one of them: a set of labels with the same name are all connected together and only one label in each same-name set must be driven.")), checkCounts(m.OtherSourceConns, (conn_2) => conn_2.Source, m.OtherOutputPorts, portMap, (y_2) => (0 < y_2), printf("A component output port must have at least one, not %d, connections. If the component output is meant to be disconnected you can add a wire label to stop this error")), checkConns(conns, m)]);
    return tryPick((x_4) => x_4, list_2);
}

function checkIOLabels(canvasState_0, canvasState_1) {
    const canvasState = [canvasState_0, canvasState_1];
    const checkDuplicate = (comps_mut, map_mut, ioType_mut) => {
        let compId, clo1, clo2;
        checkDuplicate:
        while (true) {
            const comps = comps_mut, map = map_mut, ioType = ioType_mut;
            if (comps.tail != null) {
                const comps$0027 = comps.tail;
                const comp = comps.head;
                const matchValue = FSharpMap__TryFind(map, comp.Label);
                if (matchValue != null) {
                    if (compId = matchValue, compId === comp.Id) {
                        const compId_1 = matchValue;
                        comps_mut = comps$0027;
                        map_mut = map;
                        ioType_mut = ioType;
                        continue checkDuplicate;
                    }
                    else if (matchValue != null) {
                        const compId_2 = matchValue;
                        return new SimulationError((clo1 = toText(printf("Two %s components cannot have the same label: %s.")), clo2 = clo1(ioType), clo2(comp.Label)), void 0, (map_1((arg0) => (new ComponentId(0, arg0)), ofArray([comp.Id, compId_2]))), empty());
                    }
                    else {
                        throw (new Error("The match cases were incomplete"));
                    }
                }
                else {
                    comps_mut = comps$0027;
                    map_mut = map;
                    ioType_mut = ioType;
                    continue checkDuplicate;
                }
            }
            else {
                return void 0;
            }
            break;
        }
    };
    const toMap = (comps_1) => {
        let elements;
        elements = map_1((comp_1) => [comp_1.Label, comp_1.Id], comps_1);
        return ofList(elements);
    };
    const components = canvasState[0];
    let inputs;
    inputs = filter((comp_2) => {
        if (comp_2.Type.tag === 0) {
            return true;
        }
        else {
            return false;
        }
    }, components);
    let outputs;
    outputs = filter((comp_3) => {
        if (comp_3.Type.tag === 1) {
            return true;
        }
        else {
            return false;
        }
    }, components);
    let labels;
    labels = filter((comp_4) => {
        if (comp_4.Type.tag === 2) {
            return true;
        }
        else {
            return false;
        }
    }, components);
    const matchValue_4 = [checkDuplicate(inputs, toMap(inputs), "Input"), checkDuplicate(outputs, toMap(outputs), "Output")];
    let pattern_matching_result, err;
    if (matchValue_4[0] == null) {
        if (matchValue_4[1] == null) {
            pattern_matching_result = 1;
        }
        else {
            pattern_matching_result = 0;
            err = matchValue_4[1];
        }
    }
    else {
        pattern_matching_result = 0;
        err = matchValue_4[0];
    }
    switch (pattern_matching_result) {
        case 0: {
            return err;
        }
        case 1: {
            return void 0;
        }
    }
}

export class CustomComponentError extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["NoSheet", "BadInputs", "BadOutputs"];
    }
}

export function CustomComponentError$reflection() {
    return union_type("CanvasStateAnalyser.CustomComponentError", [], CustomComponentError, () => [[["Item", string_type]], [["ComponentSheet", string_type], ["InstLists", list_type(tuple_type(string_type, int32_type))], ["CompLists", list_type(tuple_type(string_type, int32_type))]], [["ComponentSheet", string_type], ["InstLists", list_type(tuple_type(string_type, int32_type))], ["CompLists", list_type(tuple_type(string_type, int32_type))]]]);
}

export function checkCustomComponentForOkIOs(c, args, sheets) {
    const inouts = [args.InputLabels, args.OutputLabels];
    const name = args.Name;
    let _arg1;
    let option;
    option = tryFind((sheet) => (sheet.Name === name), sheets);
    _arg1 = map_2((sheet_1) => [sheet_1, equals(sheet_1.InputLabels, args.InputLabels), equals(sheet_1.OutputLabels, args.OutputLabels)], option);
    if (_arg1 != null) {
        if (_arg1[1]) {
            if (_arg1[2]) {
                return new FSharpResult$2(0, void 0);
            }
            else {
                const sheet_3 = _arg1[0];
                return new FSharpResult$2(1, [c, new CustomComponentError(2, name, sheet_3.OutputLabels, args.OutputLabels)]);
            }
        }
        else {
            const sheet_2 = _arg1[0];
            return new FSharpResult$2(1, [c, new CustomComponentError(1, name, sheet_2.InputLabels, args.InputLabels)]);
        }
    }
    else {
        return new FSharpResult$2(1, [c, new CustomComponentError(0, name)]);
    }
}

export function checkCustomComponentsOk(_arg1_0, _arg1_1, sheets) {
    let clo1_2, clo2, clo3, clo1_3, clo2_1, clo3_1, clo1_1;
    const comps = _arg1_0;
    const error = (c, msg) => (new SimulationError(msg, void 0, singleton(new ComponentId(0, c.Id)), empty()));
    const disp = (portL) => {
        let arg10;
        let strings;
        strings = map_1((tuple) => tuple[0], portL);
        arg10 = join(" , ", strings);
        const clo1 = toText(printf("%s"));
        return clo1(arg10);
    };
    let _arg3;
    let lst;
    lst = collect((_arg2) => {
        if (_arg2.Type.tag === 16) {
            const c_1 = _arg2;
            const args = _arg2.Type.fields[0];
            return singleton(checkCustomComponentForOkIOs(c_1, args, sheets));
        }
        else {
            return empty();
        }
    }, comps);
    _arg3 = tryFindError(lst);
    if (_arg3.tag === 1) {
        if (_arg3.fields[0][1].tag === 1) {
            const instIns = _arg3.fields[0][1].fields[1];
            const compIns = _arg3.fields[0][1].fields[2];
            const cName_1 = _arg3.fields[0][1].fields[0];
            const c_3 = _arg3.fields[0][0];
            const patternInput = [disp(instIns), disp(compIns)];
            const instIns_1 = patternInput[0];
            const compIns_1 = patternInput[1];
            return error(c_3, (clo1_2 = toText(printf("Sheet %s is used as a custom component. Instance In ports: %A are different from Component In ports: %A.")), clo2 = clo1_2(cName_1), clo3 = clo2(instIns_1), clo3(compIns_1)));
        }
        else if (_arg3.fields[0][1].tag === 2) {
            const instOuts = _arg3.fields[0][1].fields[1];
            const compOuts = _arg3.fields[0][1].fields[2];
            const cName_2 = _arg3.fields[0][1].fields[0];
            const c_4 = _arg3.fields[0][0];
            const patternInput_1 = [disp(instOuts), disp(compOuts)];
            const instOus = patternInput_1[0];
            const compOuts_1 = patternInput_1[1];
            return error(c_4, (clo1_3 = toText(printf("Sheet %s is used as a custom component. Instance Out ports: %A are different from Component Out ports: %A.")), clo2_1 = clo1_3(cName_2), clo3_1 = clo2_1(instOuts), clo3_1(compOuts_1)));
        }
        else {
            const cName = _arg3.fields[0][1].fields[0];
            const c_2 = _arg3.fields[0][0];
            return error(c_2, (clo1_1 = toText(printf("Can\u0027t find a design sheet named %s for the custom component of this name")), clo1_1(cName)));
        }
    }
    else {
        return void 0;
    }
}

function checkConnectionsWidths(canvasState_0, canvasState_1) {
    let ConnectionsAffected_1, list_1;
    const canvasState = [canvasState_0, canvasState_1];
    const convertConnId = (_arg1) => {
        const cId = _arg1.fields[0];
        return new ConnectionId(0, cId);
    };
    const convertError = (err) => {
        let ConnectionsAffected;
        ConnectionsAffected = map_1(convertConnId, err.ConnectionsAffected);
        return new SimulationError(err.Msg, void 0, empty(), ConnectionsAffected);
    };
    const matchValue = inferConnectionsWidth(canvasState[0], canvasState[1]);
    if (matchValue.tag === 0) {
        const connWidths = matchValue.fields[0];
        let faulty;
        faulty = filter_1((_arg1_1, width) => (width == null), connWidths);
        const matchValue_1 = FSharpMap__get_IsEmpty(faulty);
        if (matchValue_1) {
            return void 0;
        }
        else {
            return ConnectionsAffected_1 = (list_1 = (toList(faulty)), (map_1((tupledArg) => {
                const cId_1 = tupledArg[0];
                return convertConnId(cId_1);
            }, list_1))), new SimulationError("Could not infer all connections widths.", void 0, empty(), ConnectionsAffected_1);
        }
    }
    else {
        const err_1 = matchValue.fields[0];
        const arg0 = convertError(err_1);
        return arg0;
    }
}

export function checkComponentNamesAreOk(_arg1_0, _arg1_1) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    let badNameErrors;
    let list_3;
    let list_2;
    let list_1;
    list_1 = filter((_arg2) => {
        let pattern_matching_result;
        if (_arg2.Type.tag === 17) {
            pattern_matching_result = 0;
        }
        else if (_arg2.Type.tag === 18) {
            pattern_matching_result = 0;
        }
        else if (_arg2.Type.tag === 3) {
            pattern_matching_result = 0;
        }
        else {
            pattern_matching_result = 1;
        }
        switch (pattern_matching_result) {
            case 0: {
                return false;
            }
            case 1: {
                return true;
            }
        }
    }, comps);
    list_2 = collect((comp) => {
        const label = comp.Label.toLocaleUpperCase();
        switch (label) {
            case "CLK": {
                return singleton([comp, "Clk is not allowed as a name for a component or a Net. Use the properties tab to give a different name to the highlighted component(s)."]);
            }
            case "": {
                return singleton([comp, "All components must have a unique alphanumeric name (e.g. \u0027G1\u0027). An empty name is not allowed except for split and join.Use the properties tab to give a non-empty name to the highlighted component(s)."]);
            }
            default: {
                return empty();
            }
        }
    }, list_1);
    list_3 = groupBy((tuple) => tuple[1], list_2, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    badNameErrors = map_1((tupledArg) => {
        const msg = tupledArg[0];
        const eLst = tupledArg[1];
        return [map_1((tuple_1) => tuple_1[0], eLst), msg];
    }, list_3);
    let duplicateNameErrors;
    let list_7;
    let list_6;
    let list_5;
    list_5 = filter((_arg3) => {
        let pattern_matching_result_1;
        if (_arg3.Type.tag === 2) {
            pattern_matching_result_1 = 0;
        }
        else if (_arg3.Type.tag === 17) {
            pattern_matching_result_1 = 0;
        }
        else if (_arg3.Type.tag === 18) {
            pattern_matching_result_1 = 0;
        }
        else {
            pattern_matching_result_1 = 1;
        }
        switch (pattern_matching_result_1) {
            case 0: {
                return false;
            }
            case 1: {
                return true;
            }
        }
    }, comps);
    list_6 = groupBy((comp_1) => comp_1.Label, list_5, {
        Equals: (x_1, y_1) => (x_1 === y_1),
        GetHashCode: structuralHash,
    });
    list_7 = filter((tupledArg_1) => {
        const compL = tupledArg_1[1];
        return length(compL) > 1;
    }, list_6);
    duplicateNameErrors = map_1((tupledArg_2) => {
        const compL_1 = tupledArg_2[1];
        return [compL_1, "Component names must be distinct. Use the properties tab to give different names to the highlighted components"];
    }, list_7);
    const option = tryHead(append(badNameErrors, duplicateNameErrors));
    return map_2((tupledArg_3) => {
        const comps_1 = tupledArg_3[0];
        const msg_1 = tupledArg_3[1];
        return new SimulationError(msg_1, void 0, (map_1((comp_2) => (new ComponentId(0, comp_2.Id)), comps_1)), empty());
    }, option);
}

export function analyseState(state_0, state_1, ldComps) {
    const state = [state_0, state_1];
    let option_1;
    const list = ofArray([checkPortTypesAreConsistent(state[0], state[1]), checkPortsAreConnectedProperly(state[0], state[1]), checkIOLabels(state[0], state[1]), checkCustomComponentsOk(state[0], state[1], ldComps), checkConnectionsWidths(state[0], state[1]), checkComponentNamesAreOk(state[0], state[1])]);
    option_1 = tryFind((option) => (option != null), list);
    return flatten(option_1);
}

