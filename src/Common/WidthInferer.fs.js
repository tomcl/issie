import { fold as fold_1, FSharpMap__get_Item, empty as empty_1, FSharpMap__Add, ofList, ofSeq as ofSeq_1, tryFind as tryFind_1, FSharpMap__TryFind, FSharpMap__get_Count, toSeq } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { rangeNumber, append, map as map_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { concat, fold, mapIndexed, mapIndexed2, length, ofArray, singleton, tryPick, collect, map as map_2, groupBy, filter, item, empty, cons, tryFind, ofSeq } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { toText, printf, toFail } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { tryFindError, assertThat } from "./Helpers.fs.js";
import { ComponentId, ConnectionId, InputPortNumber, ComponentType, InputPortId, WidthInferError, OutputPortId } from "./CommonTypes.fs.js";
import { defaultArg, map as map_3, value as value_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { Result_Map, Result_Bind, FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { hashSafe, equals, partialApply, structuralHash, equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";

export function mapKeys(map) {
    let source_1;
    let source;
    source = toSeq(map);
    source_1 = map_1((tuple) => tuple[0], source);
    return ofSeq(source_1);
}

export function mapValues(map) {
    let source_1;
    let source;
    source = toSeq(map);
    source_1 = map_1((tuple) => tuple[1], source);
    return ofSeq(source_1);
}

export function mapItems(map) {
    let source;
    source = toSeq(map);
    return ofSeq(source);
}

function extractComponentPortNumber(port) {
    const matchValue = port.PortNumber;
    if (matchValue != null) {
        const pNumber = matchValue | 0;
        return pNumber | 0;
    }
    else {
        const clo1 = toFail(printf("what? extractComponentPortNumber should always be called with component ports: %A"));
        return clo1(port) | 0;
    }
}

function assertInputsSize(inputs, expected, comp) {
    let msg;
    const clo1 = toText(printf("assertInputsSize failed for: %A"));
    msg = clo1(comp);
    const cond = FSharpMap__get_Count(inputs) === expected;
    assertThat(cond, msg);
}

function getOutputPortId(comp, idx) {
    const matchValue = tryFind((p) => (extractComponentPortNumber(p) === idx), comp.OutputPorts);
    if (matchValue != null) {
        const port = matchValue;
        return new OutputPortId(0, port.Id);
    }
    else {
        const clo1 = toFail(printf("what? getOutputPortId called with inexistent port idx (%d): %A "));
        const clo2 = clo1(idx);
        return clo2(comp);
    }
}

function getWidthsForPorts(inputs, portNumbers) {
    if (portNumbers.tail != null) {
        const portNumbers$0027 = portNumbers.tail;
        const portNumber = portNumbers.head;
        const matchValue = FSharpMap__TryFind(inputs, portNumber);
        if (matchValue != null) {
            if (value_1(matchValue) == null) {
                return cons(void 0, getWidthsForPorts(inputs, portNumbers$0027));
            }
            else {
                const width = value_1(matchValue)[0];
                return cons(width, getWidthsForPorts(inputs, portNumbers$0027));
            }
        }
        else {
            const clo1 = toFail(printf("what? getWidthForPorts received a not extistent port: %A %A"));
            const clo2 = clo1(portNumber);
            return clo2(inputs);
        }
    }
    else {
        return empty();
    }
}

function getConnectionIdForPort(inputs, portNumber) {
    const matchValue = FSharpMap__TryFind(inputs, portNumber);
    if (matchValue != null) {
        if (value_1(matchValue) != null) {
            const connId = value_1(matchValue)[1];
            return connId;
        }
        else {
            const clo1_1 = toFail(printf("what? getConnectionIdForPort called with an unconnected port: %A %A"));
            const clo2_1 = clo1_1(portNumber);
            return clo2_1(inputs);
        }
    }
    else {
        const clo1 = toFail(printf("what? getConnectionIdForPort received a not extistent port: %A %A"));
        const clo2 = clo1(portNumber);
        return clo2(inputs);
    }
}

function makeWidthInferErrorEqual(expected, actual, connectionsAffected) {
    let clo1, clo2;
    return new FSharpResult$2(1, new WidthInferError((clo1 = toText(printf("Wrong wire width. Target port expects a %d bit(s) signal, but source port produces a %d bit(s) signal.")), clo2 = clo1(expected), clo2(actual)), connectionsAffected));
}

function makeWidthInferErrorAtLeast(atLeast, actual, connectionsAffected) {
    let clo1, clo2;
    return new FSharpResult$2(1, new WidthInferError((clo1 = toText(printf("Wrong wire width. Target port expects a signal with at least %d bits, but source port produces a %d bit(s) signal.")), clo2 = clo1(atLeast), clo2(actual)), connectionsAffected));
}

export function addVirtualBusLabelConnections(compIdToComp, inputPortsToConnectionIds) {
    const comps = mapValues(compIdToComp);
    const inputPort0Id = (comp) => (new InputPortId(0, item(0, comp.InputPorts).Id));
    let labelGroups;
    let list_1;
    list_1 = filter((comp_1) => equalsSafe(comp_1.Type, new ComponentType(2)), comps);
    labelGroups = groupBy((comp_2) => comp_2.Label, list_1, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    const createVirtualMappings = (compLst, connId) => map_2((comp_3) => [inputPort0Id(comp_3), connId], compLst);
    let extraLabelConns;
    extraLabelConns = collect((tupledArg) => {
        const name = tupledArg[0];
        const labComps = tupledArg[1];
        let option_1;
        let option;
        option = tryPick((comp_4) => tryFind_1(inputPort0Id(comp_4), inputPortsToConnectionIds), labComps);
        const mapping_1 = partialApply(1, createVirtualMappings, [labComps]);
        option_1 = map_3(mapping_1, option);
        return defaultArg(option_1, empty());
    }, labelGroups);
    let elements;
    let source2;
    source2 = toSeq(inputPortsToConnectionIds);
    let source1;
    source1 = extraLabelConns;
    elements = append(source1, source2);
    return ofSeq_1(elements);
}

function makeOutputPortsOfLabels(components) {
    let elements;
    let list_3;
    let list_1;
    list_1 = filter((_arg1) => {
        if (_arg1.Type.tag === 2) {
            return true;
        }
        else {
            return false;
        }
    }, components);
    list_3 = groupBy((c) => c.Label, list_1, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    elements = map_2((tupledArg) => {
        const label = tupledArg[0];
        const lst = tupledArg[1];
        return [label, (map_2((comp) => getOutputPortId(comp, 0), lst))];
    }, list_3);
    return ofList(elements);
}

function calculateOutputPortsWidth(comp, outputPortsOfBusLabels, inputConnectionsWidth) {
    let n_2, n, n_5, n_10, n_8, clo1_5, clo2, n_16, n_14, m_2, n_12, m, n_19, n_20, n_29, n_27, n_25, n_23, m_4, n_36, n_32, n_33, n_34, m_6, n_38, n_41, n_48, n_45, n_46, n_52, n_50, n_58, n_56, n_54, aw_2, aw, write_2, datain_2, addr_2, write, datain, addr;
    const getConnectionIdForPort_1 = (arg) => {
        let portNumber;
        portNumber = (new InputPortNumber(0, arg));
        return getConnectionIdForPort(inputConnectionsWidth, portNumber);
    };
    const matchValue = comp.Type;
    let pattern_matching_result, width, mem;
    switch (matchValue.tag) {
        case 4: {
            pattern_matching_result = 0;
            width = matchValue.fields[0];
            break;
        }
        case 1: {
            pattern_matching_result = 1;
            break;
        }
        case 2: {
            pattern_matching_result = 2;
            break;
        }
        case 3: {
            pattern_matching_result = 3;
            break;
        }
        case 5: {
            pattern_matching_result = 4;
            break;
        }
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11: {
            pattern_matching_result = 5;
            break;
        }
        case 13: {
            pattern_matching_result = 6;
            break;
        }
        case 14: {
            pattern_matching_result = 7;
            break;
        }
        case 15: {
            pattern_matching_result = 8;
            break;
        }
        case 12: {
            pattern_matching_result = 9;
            break;
        }
        case 16: {
            pattern_matching_result = 10;
            break;
        }
        case 17: {
            pattern_matching_result = 11;
            break;
        }
        case 18: {
            pattern_matching_result = 12;
            break;
        }
        case 19: {
            pattern_matching_result = 13;
            break;
        }
        case 20: {
            pattern_matching_result = 14;
            break;
        }
        case 21: {
            pattern_matching_result = 15;
            break;
        }
        case 22: {
            pattern_matching_result = 16;
            break;
        }
        case 23: {
            pattern_matching_result = 17;
            mem = matchValue.fields[0];
            break;
        }
        case 24: {
            pattern_matching_result = 17;
            mem = matchValue.fields[0];
            break;
        }
        case 25: {
            pattern_matching_result = 18;
            break;
        }
        default: {
            pattern_matching_result = 0;
            width = matchValue.fields[0];
        }
    }
    switch (pattern_matching_result) {
        case 0: {
            assertInputsSize(inputConnectionsWidth, 0, comp);
            const arg0_1 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width);
            return new FSharpResult$2(0, arg0_1);
        }
        case 1: {
            const width_1 = matchValue.fields[0] | 0;
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_1 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_1;
            if (matchValue_1.tail != null) {
                if (matchValue_1.head != null) {
                    if (matchValue_1.tail.tail == null) {
                        if (n = (matchValue_1.head | 0), n === width_1) {
                            pattern_matching_result_1 = 1;
                        }
                        else {
                            pattern_matching_result_1 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_1 = 2;
                    }
                }
                else if (matchValue_1.tail.tail == null) {
                    pattern_matching_result_1 = 0;
                }
                else {
                    pattern_matching_result_1 = 2;
                }
            }
            else {
                pattern_matching_result_1 = 2;
            }
            switch (pattern_matching_result_1) {
                case 0: {
                    return new FSharpResult$2(0, empty_1());
                }
                case 1: {
                    return new FSharpResult$2(0, empty_1());
                }
                case 2: {
                    let pattern_matching_result_2, n_3;
                    if (matchValue_1.tail != null) {
                        if (matchValue_1.head != null) {
                            if (matchValue_1.tail.tail == null) {
                                if (n_2 = (matchValue_1.head | 0), n_2 !== width_1) {
                                    pattern_matching_result_2 = 0;
                                    n_3 = matchValue_1.head;
                                }
                                else {
                                    pattern_matching_result_2 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_2 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_2 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_2 = 1;
                    }
                    switch (pattern_matching_result_2) {
                        case 0: {
                            return makeWidthInferErrorEqual(width_1, n_3, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            const clo1 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1(comp.Type);
                        }
                    }
                }
            }
        }
        case 2: {
            const matchValue_2 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_3, n_4;
            if (matchValue_2.tail != null) {
                if (matchValue_2.head != null) {
                    if (matchValue_2.tail.tail == null) {
                        pattern_matching_result_3 = 1;
                        n_4 = matchValue_2.head;
                    }
                    else {
                        pattern_matching_result_3 = 2;
                    }
                }
                else if (matchValue_2.tail.tail == null) {
                    pattern_matching_result_3 = 0;
                }
                else {
                    pattern_matching_result_3 = 2;
                }
            }
            else {
                pattern_matching_result_3 = 2;
            }
            switch (pattern_matching_result_3) {
                case 0: {
                    const arg0_2 = empty_1();
                    return new FSharpResult$2(0, arg0_2);
                }
                case 1: {
                    const outs = FSharpMap__get_Item(outputPortsOfBusLabels, comp.Label);
                    let arg0_3;
                    let elements;
                    elements = map_2((out) => [out, n_4], outs);
                    arg0_3 = ofList(elements);
                    return new FSharpResult$2(0, arg0_3);
                }
                case 2: {
                    const clo1_1 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                    return clo1_1(comp.Type);
                }
            }
        }
        case 3: {
            const width_2 = matchValue.fields[0] | 0;
            const lsBitNum = matchValue.fields[1] | 0;
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_3 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_4, n_6;
            if (matchValue_3.tail != null) {
                if (matchValue_3.head != null) {
                    if (matchValue_3.tail.tail == null) {
                        if (n_5 = (matchValue_3.head | 0), n_5 < (width_2 + lsBitNum)) {
                            pattern_matching_result_4 = 0;
                            n_6 = matchValue_3.head;
                        }
                        else {
                            pattern_matching_result_4 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_4 = 1;
                    }
                }
                else {
                    pattern_matching_result_4 = 1;
                }
            }
            else {
                pattern_matching_result_4 = 1;
            }
            switch (pattern_matching_result_4) {
                case 0: {
                    return makeWidthInferErrorAtLeast(lsBitNum + n_6, n_6, singleton(getConnectionIdForPort_1(0)));
                }
                case 1: {
                    let pattern_matching_result_5;
                    if (matchValue_3.tail != null) {
                        if (matchValue_3.head != null) {
                            if (matchValue_3.tail.tail == null) {
                                pattern_matching_result_5 = 0;
                            }
                            else {
                                pattern_matching_result_5 = 1;
                            }
                        }
                        else if (matchValue_3.tail.tail == null) {
                            pattern_matching_result_5 = 0;
                        }
                        else {
                            pattern_matching_result_5 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_5 = 1;
                    }
                    switch (pattern_matching_result_5) {
                        case 0: {
                            const arg0_4 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width_2);
                            return new FSharpResult$2(0, arg0_4);
                        }
                        case 1: {
                            const clo1_2 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_2(comp.Type);
                        }
                    }
                }
            }
        }
        case 4: {
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_4 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_6, n_7;
            if (matchValue_4.tail != null) {
                if (matchValue_4.head != null) {
                    if (matchValue_4.head === 1) {
                        if (matchValue_4.tail.tail == null) {
                            pattern_matching_result_6 = 0;
                        }
                        else {
                            pattern_matching_result_6 = 2;
                        }
                    }
                    else if (matchValue_4.tail.tail == null) {
                        pattern_matching_result_6 = 1;
                        n_7 = matchValue_4.head;
                    }
                    else {
                        pattern_matching_result_6 = 2;
                    }
                }
                else if (matchValue_4.tail.tail == null) {
                    pattern_matching_result_6 = 0;
                }
                else {
                    pattern_matching_result_6 = 2;
                }
            }
            else {
                pattern_matching_result_6 = 2;
            }
            switch (pattern_matching_result_6) {
                case 0: {
                    const arg0_5 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), 1);
                    return new FSharpResult$2(0, arg0_5);
                }
                case 1: {
                    return makeWidthInferErrorEqual(1, n_7, singleton(getConnectionIdForPort_1(0)));
                }
                case 2: {
                    const clo1_3 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                    return clo1_3(comp.Type);
                }
            }
        }
        case 5: {
            assertInputsSize(inputConnectionsWidth, 2, comp);
            const matchValue_5 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_7, n_9;
            if (matchValue_5.tail != null) {
                if (matchValue_5.head != null) {
                    if (matchValue_5.tail.tail != null) {
                        if (matchValue_5.tail.tail.tail == null) {
                            if (n_8 = (matchValue_5.head | 0), n_8 !== 1) {
                                pattern_matching_result_7 = 0;
                                n_9 = matchValue_5.head;
                            }
                            else {
                                pattern_matching_result_7 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_7 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_7 = 1;
                    }
                }
                else {
                    pattern_matching_result_7 = 1;
                }
            }
            else {
                pattern_matching_result_7 = 1;
            }
            switch (pattern_matching_result_7) {
                case 0: {
                    return makeWidthInferErrorEqual(1, n_9, singleton(getConnectionIdForPort_1(0)));
                }
                case 1: {
                    let pattern_matching_result_8, n_11;
                    if (matchValue_5.tail != null) {
                        if (matchValue_5.tail.tail != null) {
                            if (matchValue_5.tail.head != null) {
                                if (matchValue_5.tail.tail.tail == null) {
                                    if (n_10 = (matchValue_5.tail.head | 0), n_10 !== 1) {
                                        pattern_matching_result_8 = 0;
                                        n_11 = matchValue_5.tail.head;
                                    }
                                    else {
                                        pattern_matching_result_8 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_8 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_8 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_8 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_8 = 1;
                    }
                    switch (pattern_matching_result_8) {
                        case 0: {
                            return makeWidthInferErrorEqual(1, n_11, singleton(getConnectionIdForPort_1(1)));
                        }
                        case 1: {
                            let pattern_matching_result_9;
                            if (matchValue_5.tail != null) {
                                if (matchValue_5.head != null) {
                                    if (matchValue_5.tail.tail != null) {
                                        if (matchValue_5.tail.head != null) {
                                            if (matchValue_5.tail.head === 1) {
                                                if (matchValue_5.tail.tail.tail == null) {
                                                    if (matchValue_5.head === 1) {
                                                        pattern_matching_result_9 = 0;
                                                    }
                                                    else {
                                                        pattern_matching_result_9 = 1;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_9 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_9 = 1;
                                            }
                                        }
                                        else if (matchValue_5.tail.tail.tail == null) {
                                            pattern_matching_result_9 = 0;
                                        }
                                        else {
                                            pattern_matching_result_9 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_9 = 1;
                                    }
                                }
                                else if (matchValue_5.tail.tail != null) {
                                    if (matchValue_5.tail.tail.tail == null) {
                                        pattern_matching_result_9 = 0;
                                    }
                                    else {
                                        pattern_matching_result_9 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_9 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_9 = 1;
                            }
                            switch (pattern_matching_result_9) {
                                case 0: {
                                    const arg0_6 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), 1);
                                    return new FSharpResult$2(0, arg0_6);
                                }
                                case 1: {
                                    const clo1_4 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                                    return clo1_4(comp.Type);
                                }
                            }
                        }
                    }
                }
            }
        }
        case 6: {
            assertInputsSize(inputConnectionsWidth, 3, comp);
            const matchValue_6 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1), new InputPortNumber(0, 2)]));
            let pattern_matching_result_10, m_1, n_13;
            if (matchValue_6.tail != null) {
                if (matchValue_6.head != null) {
                    if (matchValue_6.tail.tail != null) {
                        if (matchValue_6.tail.head != null) {
                            if (matchValue_6.tail.tail.tail != null) {
                                if (matchValue_6.tail.tail.head != null) {
                                    if (matchValue_6.tail.tail.head === 1) {
                                        if (matchValue_6.tail.tail.tail.tail == null) {
                                            if (n_12 = (matchValue_6.head | 0), (m = (matchValue_6.tail.head | 0), n_12 === m)) {
                                                pattern_matching_result_10 = 0;
                                                m_1 = matchValue_6.tail.head;
                                                n_13 = matchValue_6.head;
                                            }
                                            else {
                                                pattern_matching_result_10 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_10 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_10 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_10 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_10 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_10 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_10 = 1;
                    }
                }
                else {
                    pattern_matching_result_10 = 1;
                }
            }
            else {
                pattern_matching_result_10 = 1;
            }
            switch (pattern_matching_result_10) {
                case 0: {
                    const arg0_7 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), n_13);
                    return new FSharpResult$2(0, arg0_7);
                }
                case 1: {
                    let pattern_matching_result_11, m_3, n_15;
                    if (matchValue_6.tail != null) {
                        if (matchValue_6.head != null) {
                            if (matchValue_6.tail.tail != null) {
                                if (matchValue_6.tail.head != null) {
                                    if (matchValue_6.tail.tail.tail != null) {
                                        if (matchValue_6.tail.tail.tail.tail == null) {
                                            if (n_14 = (matchValue_6.head | 0), (m_2 = (matchValue_6.tail.head | 0), n_14 !== m_2)) {
                                                pattern_matching_result_11 = 0;
                                                m_3 = matchValue_6.tail.head;
                                                n_15 = matchValue_6.head;
                                            }
                                            else {
                                                pattern_matching_result_11 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_11 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_11 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_11 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_11 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_11 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_11 = 1;
                    }
                    switch (pattern_matching_result_11) {
                        case 0: {
                            return new FSharpResult$2(1, new WidthInferError((clo1_5 = toText(printf("Wrong wire width. The two inputs to a multiplexer are expected to have the same width, but top input has %d bits and bottom input has %d bits.")), clo2 = clo1_5(n_15), clo2(m_3)), ofArray([getConnectionIdForPort_1(0), getConnectionIdForPort_1(1)])));
                        }
                        case 1: {
                            let pattern_matching_result_12, n_17;
                            if (matchValue_6.tail != null) {
                                if (matchValue_6.tail.tail != null) {
                                    if (matchValue_6.tail.tail.tail != null) {
                                        if (matchValue_6.tail.tail.head != null) {
                                            if (matchValue_6.tail.tail.tail.tail == null) {
                                                if (n_16 = (matchValue_6.tail.tail.head | 0), n_16 !== 1) {
                                                    pattern_matching_result_12 = 0;
                                                    n_17 = matchValue_6.tail.tail.head;
                                                }
                                                else {
                                                    pattern_matching_result_12 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_12 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_12 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_12 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_12 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_12 = 1;
                            }
                            switch (pattern_matching_result_12) {
                                case 0: {
                                    return makeWidthInferErrorEqual(1, n_17, singleton(getConnectionIdForPort_1(2)));
                                }
                                case 1: {
                                    let pattern_matching_result_13, n_18;
                                    if (matchValue_6.tail != null) {
                                        if (matchValue_6.head == null) {
                                            if (matchValue_6.tail.tail != null) {
                                                if (matchValue_6.tail.head != null) {
                                                    if (matchValue_6.tail.tail.tail != null) {
                                                        if (matchValue_6.tail.tail.tail.tail == null) {
                                                            pattern_matching_result_13 = 0;
                                                            n_18 = matchValue_6.tail.head;
                                                        }
                                                        else {
                                                            pattern_matching_result_13 = 2;
                                                        }
                                                    }
                                                    else {
                                                        pattern_matching_result_13 = 2;
                                                    }
                                                }
                                                else if (matchValue_6.tail.tail.tail != null) {
                                                    if (matchValue_6.tail.tail.tail.tail == null) {
                                                        pattern_matching_result_13 = 1;
                                                    }
                                                    else {
                                                        pattern_matching_result_13 = 2;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_13 = 2;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_13 = 2;
                                            }
                                        }
                                        else if (matchValue_6.tail.tail != null) {
                                            if (matchValue_6.tail.head == null) {
                                                if (matchValue_6.tail.tail.tail != null) {
                                                    if (matchValue_6.tail.tail.tail.tail == null) {
                                                        pattern_matching_result_13 = 0;
                                                        n_18 = matchValue_6.head;
                                                    }
                                                    else {
                                                        pattern_matching_result_13 = 2;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_13 = 2;
                                                }
                                            }
                                            else if (matchValue_6.tail.tail.tail != null) {
                                                if (matchValue_6.tail.tail.tail.tail == null) {
                                                    pattern_matching_result_13 = 1;
                                                }
                                                else {
                                                    pattern_matching_result_13 = 2;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_13 = 2;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_13 = 2;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_13 = 2;
                                    }
                                    switch (pattern_matching_result_13) {
                                        case 0: {
                                            const arg0_8 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), n_18);
                                            return new FSharpResult$2(0, arg0_8);
                                        }
                                        case 1: {
                                            return new FSharpResult$2(0, empty_1());
                                        }
                                        case 2: {
                                            const clo1_6 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                                            return clo1_6(comp.Type);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        case 7: {
            assertInputsSize(inputConnectionsWidth, 2, comp);
            const matchValue_7 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_14, n_21, n_22;
            if (matchValue_7.tail != null) {
                if (matchValue_7.head != null) {
                    if (matchValue_7.tail.tail != null) {
                        if (matchValue_7.tail.head == null) {
                            if (matchValue_7.tail.tail.tail == null) {
                                pattern_matching_result_14 = 0;
                                n_21 = matchValue_7.head;
                            }
                            else {
                                pattern_matching_result_14 = 2;
                            }
                        }
                        else if (matchValue_7.tail.head === 1) {
                            if (matchValue_7.tail.tail.tail == null) {
                                pattern_matching_result_14 = 0;
                                n_21 = matchValue_7.head;
                            }
                            else {
                                pattern_matching_result_14 = 2;
                            }
                        }
                        else if (matchValue_7.tail.tail.tail == null) {
                            if (n_19 = (matchValue_7.tail.head | 0), n_19 !== 1) {
                                pattern_matching_result_14 = 1;
                                n_22 = matchValue_7.tail.head;
                            }
                            else {
                                pattern_matching_result_14 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_14 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_14 = 2;
                    }
                }
                else if (matchValue_7.tail.tail != null) {
                    if (matchValue_7.tail.head != null) {
                        if (matchValue_7.tail.tail.tail == null) {
                            if (n_20 = (matchValue_7.tail.head | 0), n_20 !== 1) {
                                pattern_matching_result_14 = 1;
                                n_22 = matchValue_7.tail.head;
                            }
                            else {
                                pattern_matching_result_14 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_14 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_14 = 2;
                    }
                }
                else {
                    pattern_matching_result_14 = 2;
                }
            }
            else {
                pattern_matching_result_14 = 2;
            }
            switch (pattern_matching_result_14) {
                case 0: {
                    const out_1 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), n_21);
                    const out_2 = FSharpMap__Add(out_1, getOutputPortId(comp, 1), n_21);
                    return new FSharpResult$2(0, out_2);
                }
                case 1: {
                    return makeWidthInferErrorEqual(1, n_22, singleton(getConnectionIdForPort_1(1)));
                }
                case 2: {
                    let pattern_matching_result_15;
                    if (matchValue_7.tail != null) {
                        if (matchValue_7.tail.tail != null) {
                            if (matchValue_7.tail.tail.tail == null) {
                                pattern_matching_result_15 = 0;
                            }
                            else {
                                pattern_matching_result_15 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_15 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_15 = 1;
                    }
                    switch (pattern_matching_result_15) {
                        case 0: {
                            return new FSharpResult$2(0, empty_1());
                        }
                        case 1: {
                            const clo1_7 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_7(comp.Type);
                        }
                    }
                }
            }
        }
        case 8: {
            const numberOfBits = matchValue.fields[0] | 0;
            assertInputsSize(inputConnectionsWidth, 3, comp);
            let okOutMap;
            const out_3 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), numberOfBits);
            const out_4 = FSharpMap__Add(out_3, getOutputPortId(comp, 1), 1);
            okOutMap = (new FSharpResult$2(0, out_4));
            const matchValue_8 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1), new InputPortNumber(0, 2)]));
            let pattern_matching_result_16;
            if (matchValue_8.tail != null) {
                if (matchValue_8.head != null) {
                    if (matchValue_8.tail.tail != null) {
                        if (matchValue_8.tail.head != null) {
                            if (matchValue_8.tail.tail.tail == null) {
                                if (n_23 = (matchValue_8.head | 0), (m_4 = (matchValue_8.tail.head | 0), (n_23 === numberOfBits) ? (m_4 === numberOfBits) : false)) {
                                    pattern_matching_result_16 = 0;
                                }
                                else {
                                    pattern_matching_result_16 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_16 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_16 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_16 = 1;
                    }
                }
                else {
                    pattern_matching_result_16 = 1;
                }
            }
            else {
                pattern_matching_result_16 = 1;
            }
            switch (pattern_matching_result_16) {
                case 0: {
                    return okOutMap;
                }
                case 1: {
                    let pattern_matching_result_17, n_26;
                    if (matchValue_8.tail != null) {
                        if (matchValue_8.head != null) {
                            if (matchValue_8.tail.tail != null) {
                                if (matchValue_8.tail.tail.tail != null) {
                                    if (matchValue_8.tail.tail.tail.tail == null) {
                                        if (n_25 = (matchValue_8.head | 0), n_25 !== 1) {
                                            pattern_matching_result_17 = 0;
                                            n_26 = matchValue_8.head;
                                        }
                                        else {
                                            pattern_matching_result_17 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_17 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_17 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_17 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_17 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_17 = 1;
                    }
                    switch (pattern_matching_result_17) {
                        case 0: {
                            return makeWidthInferErrorEqual(1, n_26, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            let pattern_matching_result_18, n_28;
                            if (matchValue_8.tail != null) {
                                if (matchValue_8.tail.tail != null) {
                                    if (matchValue_8.tail.head != null) {
                                        if (matchValue_8.tail.tail.tail != null) {
                                            if (matchValue_8.tail.tail.tail.tail == null) {
                                                if (n_27 = (matchValue_8.tail.head | 0), n_27 !== numberOfBits) {
                                                    pattern_matching_result_18 = 0;
                                                    n_28 = matchValue_8.tail.head;
                                                }
                                                else {
                                                    pattern_matching_result_18 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_18 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_18 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_18 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_18 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_18 = 1;
                            }
                            switch (pattern_matching_result_18) {
                                case 0: {
                                    return makeWidthInferErrorEqual(numberOfBits, n_28, singleton(getConnectionIdForPort_1(1)));
                                }
                                case 1: {
                                    let pattern_matching_result_19, n_30;
                                    if (matchValue_8.tail != null) {
                                        if (matchValue_8.tail.tail != null) {
                                            if (matchValue_8.tail.tail.tail != null) {
                                                if (matchValue_8.tail.tail.head != null) {
                                                    if (matchValue_8.tail.tail.tail.tail == null) {
                                                        if (n_29 = (matchValue_8.tail.tail.head | 0), n_29 !== numberOfBits) {
                                                            pattern_matching_result_19 = 0;
                                                            n_30 = matchValue_8.tail.tail.head;
                                                        }
                                                        else {
                                                            pattern_matching_result_19 = 1;
                                                        }
                                                    }
                                                    else {
                                                        pattern_matching_result_19 = 1;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_19 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_19 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_19 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_19 = 1;
                                    }
                                    switch (pattern_matching_result_19) {
                                        case 0: {
                                            return makeWidthInferErrorEqual(numberOfBits, n_30, singleton(getConnectionIdForPort_1(2)));
                                        }
                                        case 1: {
                                            let pattern_matching_result_20, x;
                                            if (matchValue_8.tail != null) {
                                                if (matchValue_8.tail.tail != null) {
                                                    if (matchValue_8.tail.tail.tail != null) {
                                                        if (matchValue_8.tail.tail.tail.tail == null) {
                                                            pattern_matching_result_20 = 0;
                                                        }
                                                        else {
                                                            pattern_matching_result_20 = 1;
                                                            x = matchValue_8;
                                                        }
                                                    }
                                                    else {
                                                        pattern_matching_result_20 = 1;
                                                        x = matchValue_8;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_20 = 1;
                                                    x = matchValue_8;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_20 = 1;
                                                x = matchValue_8;
                                            }
                                            switch (pattern_matching_result_20) {
                                                case 0: {
                                                    return okOutMap;
                                                }
                                                case 1: {
                                                    const clo1_8 = toFail(printf("what? Impossible case (%A) in calculateOutputPortsWidth for: %A"));
                                                    const clo2_1 = clo1_8(x);
                                                    return clo2_1(comp.Type);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        case 9: {
            assertInputsSize(inputConnectionsWidth, 2, comp);
            let okOutMap_1;
            let arg0_9;
            let elements_1;
            const list_1 = ofSeq(rangeNumber(0, 1, 3));
            elements_1 = map_2((n_31) => [getOutputPortId(comp, n_31), 1], list_1);
            arg0_9 = ofList(elements_1);
            okOutMap_1 = (new FSharpResult$2(0, arg0_9));
            const matchValue_9 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_21, n_35;
            if (matchValue_9.tail != null) {
                if (matchValue_9.head != null) {
                    if (matchValue_9.head === 2) {
                        if (matchValue_9.tail.tail != null) {
                            if (matchValue_9.tail.head != null) {
                                if (matchValue_9.tail.head === 1) {
                                    if (matchValue_9.tail.tail.tail == null) {
                                        pattern_matching_result_21 = 0;
                                    }
                                    else {
                                        pattern_matching_result_21 = 2;
                                    }
                                }
                                else if (matchValue_9.tail.tail.tail == null) {
                                    if (n_32 = (matchValue_9.head | 0), n_32 !== 2) {
                                        pattern_matching_result_21 = 1;
                                        n_35 = matchValue_9.head;
                                    }
                                    else {
                                        pattern_matching_result_21 = 2;
                                    }
                                }
                                else {
                                    pattern_matching_result_21 = 2;
                                }
                            }
                            else if (matchValue_9.tail.tail.tail == null) {
                                if (n_33 = (matchValue_9.head | 0), n_33 !== 2) {
                                    pattern_matching_result_21 = 1;
                                    n_35 = matchValue_9.head;
                                }
                                else {
                                    pattern_matching_result_21 = 2;
                                }
                            }
                            else {
                                pattern_matching_result_21 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_21 = 2;
                        }
                    }
                    else if (matchValue_9.tail.tail != null) {
                        if (matchValue_9.tail.tail.tail == null) {
                            if (n_34 = (matchValue_9.head | 0), n_34 !== 2) {
                                pattern_matching_result_21 = 1;
                                n_35 = matchValue_9.head;
                            }
                            else {
                                pattern_matching_result_21 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_21 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_21 = 2;
                    }
                }
                else {
                    pattern_matching_result_21 = 2;
                }
            }
            else {
                pattern_matching_result_21 = 2;
            }
            switch (pattern_matching_result_21) {
                case 0: {
                    return okOutMap_1;
                }
                case 1: {
                    return makeWidthInferErrorEqual(2, n_35, singleton(getConnectionIdForPort_1(0)));
                }
                case 2: {
                    let pattern_matching_result_22, n_37;
                    if (matchValue_9.tail != null) {
                        if (matchValue_9.tail.tail != null) {
                            if (matchValue_9.tail.head != null) {
                                if (matchValue_9.tail.tail.tail == null) {
                                    if (n_36 = (matchValue_9.tail.head | 0), n_36 !== 1) {
                                        pattern_matching_result_22 = 0;
                                        n_37 = matchValue_9.tail.head;
                                    }
                                    else {
                                        pattern_matching_result_22 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_22 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_22 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_22 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_22 = 1;
                    }
                    switch (pattern_matching_result_22) {
                        case 0: {
                            return makeWidthInferErrorEqual(1, n_37, singleton(getConnectionIdForPort_1(1)));
                        }
                        case 1: {
                            let pattern_matching_result_23, x_1;
                            if (matchValue_9.tail != null) {
                                if (matchValue_9.tail.tail != null) {
                                    if (matchValue_9.tail.tail.tail == null) {
                                        pattern_matching_result_23 = 0;
                                    }
                                    else {
                                        pattern_matching_result_23 = 1;
                                        x_1 = matchValue_9;
                                    }
                                }
                                else {
                                    pattern_matching_result_23 = 1;
                                    x_1 = matchValue_9;
                                }
                            }
                            else {
                                pattern_matching_result_23 = 1;
                                x_1 = matchValue_9;
                            }
                            switch (pattern_matching_result_23) {
                                case 0: {
                                    return okOutMap_1;
                                }
                                case 1: {
                                    const clo1_9 = toFail(printf("what? Impossible case (%A) in case in calculateOutputPortsWidth for: %A"));
                                    const clo2_2 = clo1_9(x_1);
                                    return clo2_2(comp.Type);
                                }
                            }
                        }
                    }
                }
            }
        }
        case 10: {
            const custom = matchValue.fields[0];
            assertInputsSize(inputConnectionsWidth, length(custom.InputLabels), comp);
            let inputWidths;
            let portNumbers;
            const list_2 = ofSeq(rangeNumber(0, 1, length(custom.InputLabels) - 1));
            portNumbers = map_2((arg0_10) => (new InputPortNumber(0, arg0_10)), list_2);
            inputWidths = getWidthsForPorts(inputConnectionsWidth, portNumbers);
            let maybeError;
            maybeError = mapIndexed2((idx, actual, tupledArg) => {
                let w;
                const expected = tupledArg[1] | 0;
                if (actual != null) {
                    if (w = (actual | 0), w === expected) {
                        const w_1 = actual | 0;
                        return void 0;
                    }
                    else if (actual != null) {
                        const w_2 = actual | 0;
                        const arg0_11 = makeWidthInferErrorEqual(expected, w_2, singleton(getConnectionIdForPort_1(idx)));
                        return arg0_11;
                    }
                    else {
                        throw (new Error("The match cases were incomplete"));
                    }
                }
                else {
                    return void 0;
                }
            }, inputWidths, custom.InputLabels);
            const matchValue_10 = tryFind((el) => (!equals(el, void 0)), maybeError);
            if (matchValue_10 == null) {
                let arg0_12;
                let elements_2;
                elements_2 = mapIndexed((idx_1, tupledArg_1) => {
                    const w_3 = tupledArg_1[1] | 0;
                    return [getOutputPortId(comp, idx_1), w_3];
                }, custom.OutputLabels);
                arg0_12 = ofList(elements_2);
                return new FSharpResult$2(0, arg0_12);
            }
            else if (value_1(matchValue_10) != null) {
                const err = value_1(matchValue_10);
                return err;
            }
            else {
                const clo1_10 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                return clo1_10(comp.Type);
            }
        }
        case 11: {
            assertInputsSize(inputConnectionsWidth, 2, comp);
            const matchValue_11 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_24, n_39;
            if (matchValue_11.tail != null) {
                if (matchValue_11.head != null) {
                    if (matchValue_11.tail.tail != null) {
                        if (matchValue_11.tail.tail.tail == null) {
                            if (n_38 = (matchValue_11.head | 0), n_38 < 1) {
                                pattern_matching_result_24 = 0;
                                n_39 = matchValue_11.head;
                            }
                            else {
                                pattern_matching_result_24 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_24 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_24 = 1;
                    }
                }
                else {
                    pattern_matching_result_24 = 1;
                }
            }
            else {
                pattern_matching_result_24 = 1;
            }
            switch (pattern_matching_result_24) {
                case 0: {
                    return makeWidthInferErrorAtLeast(1, n_39, singleton(getConnectionIdForPort_1(0)));
                }
                case 1: {
                    let pattern_matching_result_25, m_7;
                    if (matchValue_11.tail != null) {
                        if (matchValue_11.tail.tail != null) {
                            if (matchValue_11.tail.head != null) {
                                if (matchValue_11.tail.tail.tail == null) {
                                    if (m_6 = (matchValue_11.tail.head | 0), m_6 < 1) {
                                        pattern_matching_result_25 = 0;
                                        m_7 = matchValue_11.tail.head;
                                    }
                                    else {
                                        pattern_matching_result_25 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_25 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_25 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_25 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_25 = 1;
                    }
                    switch (pattern_matching_result_25) {
                        case 0: {
                            return makeWidthInferErrorAtLeast(1, m_7, singleton(getConnectionIdForPort_1(1)));
                        }
                        case 1: {
                            let pattern_matching_result_26, m_8, n_40;
                            if (matchValue_11.tail != null) {
                                if (matchValue_11.head != null) {
                                    if (matchValue_11.tail.tail != null) {
                                        if (matchValue_11.tail.head != null) {
                                            if (matchValue_11.tail.tail.tail == null) {
                                                pattern_matching_result_26 = 1;
                                                m_8 = matchValue_11.tail.head;
                                                n_40 = matchValue_11.head;
                                            }
                                            else {
                                                pattern_matching_result_26 = 2;
                                            }
                                        }
                                        else if (matchValue_11.tail.tail.tail == null) {
                                            pattern_matching_result_26 = 0;
                                        }
                                        else {
                                            pattern_matching_result_26 = 2;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_26 = 2;
                                    }
                                }
                                else if (matchValue_11.tail.tail != null) {
                                    if (matchValue_11.tail.tail.tail == null) {
                                        pattern_matching_result_26 = 0;
                                    }
                                    else {
                                        pattern_matching_result_26 = 2;
                                    }
                                }
                                else {
                                    pattern_matching_result_26 = 2;
                                }
                            }
                            else {
                                pattern_matching_result_26 = 2;
                            }
                            switch (pattern_matching_result_26) {
                                case 0: {
                                    return new FSharpResult$2(0, empty_1());
                                }
                                case 1: {
                                    const arg0_13 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), n_40 + m_8);
                                    return new FSharpResult$2(0, arg0_13);
                                }
                                case 2: {
                                    const clo1_11 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                                    return clo1_11(comp.Type);
                                }
                            }
                        }
                    }
                }
            }
        }
        case 12: {
            const topWireWidth = matchValue.fields[0] | 0;
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_12 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_27, n_42;
            if (matchValue_12.tail != null) {
                if (matchValue_12.head != null) {
                    if (matchValue_12.tail.tail == null) {
                        if (n_41 = (matchValue_12.head | 0), n_41 < (topWireWidth + 1)) {
                            pattern_matching_result_27 = 1;
                            n_42 = matchValue_12.head;
                        }
                        else {
                            pattern_matching_result_27 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_27 = 2;
                    }
                }
                else if (matchValue_12.tail.tail == null) {
                    pattern_matching_result_27 = 0;
                }
                else {
                    pattern_matching_result_27 = 2;
                }
            }
            else {
                pattern_matching_result_27 = 2;
            }
            switch (pattern_matching_result_27) {
                case 0: {
                    return new FSharpResult$2(0, empty_1());
                }
                case 1: {
                    return makeWidthInferErrorAtLeast(topWireWidth + 1, n_42, singleton(getConnectionIdForPort_1(0)));
                }
                case 2: {
                    let pattern_matching_result_28, n_43;
                    if (matchValue_12.tail != null) {
                        if (matchValue_12.head != null) {
                            if (matchValue_12.tail.tail == null) {
                                pattern_matching_result_28 = 0;
                                n_43 = matchValue_12.head;
                            }
                            else {
                                pattern_matching_result_28 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_28 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_28 = 1;
                    }
                    switch (pattern_matching_result_28) {
                        case 0: {
                            const out_5 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), topWireWidth);
                            const out_6 = FSharpMap__Add(out_5, getOutputPortId(comp, 1), n_43 - topWireWidth);
                            return new FSharpResult$2(0, out_6);
                        }
                        case 1: {
                            const clo1_12 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_12(comp.Type);
                        }
                    }
                }
            }
        }
        case 13: {
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_13 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_29, n_44;
            if (matchValue_13.tail != null) {
                if (matchValue_13.head != null) {
                    if (matchValue_13.head === 1) {
                        if (matchValue_13.tail.tail == null) {
                            pattern_matching_result_29 = 0;
                        }
                        else {
                            pattern_matching_result_29 = 2;
                        }
                    }
                    else if (matchValue_13.tail.tail == null) {
                        pattern_matching_result_29 = 1;
                        n_44 = matchValue_13.head;
                    }
                    else {
                        pattern_matching_result_29 = 2;
                    }
                }
                else if (matchValue_13.tail.tail == null) {
                    pattern_matching_result_29 = 0;
                }
                else {
                    pattern_matching_result_29 = 2;
                }
            }
            else {
                pattern_matching_result_29 = 2;
            }
            switch (pattern_matching_result_29) {
                case 0: {
                    const arg0_14 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), 1);
                    return new FSharpResult$2(0, arg0_14);
                }
                case 1: {
                    return makeWidthInferErrorEqual(1, n_44, singleton(getConnectionIdForPort_1(0)));
                }
                case 2: {
                    const clo1_13 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                    return clo1_13(comp.Type);
                }
            }
        }
        case 14: {
            assertInputsSize(inputConnectionsWidth, 2, comp);
            const matchValue_14 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_30, n_47;
            if (matchValue_14.tail != null) {
                if (matchValue_14.head != null) {
                    if (matchValue_14.head === 1) {
                        if (matchValue_14.tail.tail != null) {
                            if (matchValue_14.tail.head != null) {
                                if (matchValue_14.tail.head === 1) {
                                    if (matchValue_14.tail.tail.tail == null) {
                                        pattern_matching_result_30 = 0;
                                    }
                                    else {
                                        pattern_matching_result_30 = 2;
                                    }
                                }
                                else if (matchValue_14.tail.tail.tail == null) {
                                    if (n_45 = (matchValue_14.head | 0), n_45 !== 1) {
                                        pattern_matching_result_30 = 1;
                                        n_47 = matchValue_14.head;
                                    }
                                    else {
                                        pattern_matching_result_30 = 2;
                                    }
                                }
                                else {
                                    pattern_matching_result_30 = 2;
                                }
                            }
                            else if (matchValue_14.tail.tail.tail == null) {
                                pattern_matching_result_30 = 0;
                            }
                            else {
                                pattern_matching_result_30 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_30 = 2;
                        }
                    }
                    else if (matchValue_14.tail.tail != null) {
                        if (matchValue_14.tail.tail.tail == null) {
                            if (n_46 = (matchValue_14.head | 0), n_46 !== 1) {
                                pattern_matching_result_30 = 1;
                                n_47 = matchValue_14.head;
                            }
                            else {
                                pattern_matching_result_30 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_30 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_30 = 2;
                    }
                }
                else if (matchValue_14.tail.tail != null) {
                    if (matchValue_14.tail.head != null) {
                        if (matchValue_14.tail.head === 1) {
                            if (matchValue_14.tail.tail.tail == null) {
                                pattern_matching_result_30 = 0;
                            }
                            else {
                                pattern_matching_result_30 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_30 = 2;
                        }
                    }
                    else if (matchValue_14.tail.tail.tail == null) {
                        pattern_matching_result_30 = 0;
                    }
                    else {
                        pattern_matching_result_30 = 2;
                    }
                }
                else {
                    pattern_matching_result_30 = 2;
                }
            }
            else {
                pattern_matching_result_30 = 2;
            }
            switch (pattern_matching_result_30) {
                case 0: {
                    const arg0_15 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), 1);
                    return new FSharpResult$2(0, arg0_15);
                }
                case 1: {
                    return makeWidthInferErrorEqual(1, n_47, singleton(getConnectionIdForPort_1(0)));
                }
                case 2: {
                    let pattern_matching_result_31, n_49;
                    if (matchValue_14.tail != null) {
                        if (matchValue_14.tail.tail != null) {
                            if (matchValue_14.tail.head != null) {
                                if (matchValue_14.tail.tail.tail == null) {
                                    if (n_48 = (matchValue_14.tail.head | 0), n_48 !== 1) {
                                        pattern_matching_result_31 = 0;
                                        n_49 = matchValue_14.tail.head;
                                    }
                                    else {
                                        pattern_matching_result_31 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_31 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_31 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_31 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_31 = 1;
                    }
                    switch (pattern_matching_result_31) {
                        case 0: {
                            return makeWidthInferErrorEqual(1, n_49, singleton(getConnectionIdForPort_1(1)));
                        }
                        case 1: {
                            const clo1_14 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_14(comp.Type);
                        }
                    }
                }
            }
        }
        case 15: {
            const width_3 = matchValue.fields[0] | 0;
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_15 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_32;
            if (matchValue_15.tail != null) {
                if (matchValue_15.head != null) {
                    if (matchValue_15.tail.tail == null) {
                        if (n_50 = (matchValue_15.head | 0), n_50 === width_3) {
                            pattern_matching_result_32 = 1;
                        }
                        else {
                            pattern_matching_result_32 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_32 = 2;
                    }
                }
                else if (matchValue_15.tail.tail == null) {
                    pattern_matching_result_32 = 0;
                }
                else {
                    pattern_matching_result_32 = 2;
                }
            }
            else {
                pattern_matching_result_32 = 2;
            }
            switch (pattern_matching_result_32) {
                case 0: {
                    const arg0_16 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width_3);
                    return new FSharpResult$2(0, arg0_16);
                }
                case 1: {
                    const arg0_17 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width_3);
                    return new FSharpResult$2(0, arg0_17);
                }
                case 2: {
                    let pattern_matching_result_33, n_53;
                    if (matchValue_15.tail != null) {
                        if (matchValue_15.head != null) {
                            if (matchValue_15.tail.tail == null) {
                                if (n_52 = (matchValue_15.head | 0), n_52 !== width_3) {
                                    pattern_matching_result_33 = 0;
                                    n_53 = matchValue_15.head;
                                }
                                else {
                                    pattern_matching_result_33 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_33 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_33 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_33 = 1;
                    }
                    switch (pattern_matching_result_33) {
                        case 0: {
                            return makeWidthInferErrorEqual(width_3, n_53, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            const clo1_15 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_15(comp.Type);
                        }
                    }
                }
            }
        }
        case 16: {
            const width_4 = matchValue.fields[0] | 0;
            assertInputsSize(inputConnectionsWidth, 2, comp);
            const matchValue_16 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
            let pattern_matching_result_34;
            if (matchValue_16.tail != null) {
                if (matchValue_16.head != null) {
                    if (matchValue_16.tail.tail != null) {
                        if (matchValue_16.tail.head != null) {
                            if (matchValue_16.tail.head === 1) {
                                if (matchValue_16.tail.tail.tail == null) {
                                    if (n_54 = (matchValue_16.head | 0), n_54 === width_4) {
                                        pattern_matching_result_34 = 0;
                                    }
                                    else {
                                        pattern_matching_result_34 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_34 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_34 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_34 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_34 = 1;
                    }
                }
                else {
                    pattern_matching_result_34 = 1;
                }
            }
            else {
                pattern_matching_result_34 = 1;
            }
            switch (pattern_matching_result_34) {
                case 0: {
                    const arg0_18 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width_4);
                    return new FSharpResult$2(0, arg0_18);
                }
                case 1: {
                    let pattern_matching_result_35, n_57;
                    if (matchValue_16.tail != null) {
                        if (matchValue_16.head != null) {
                            if (matchValue_16.tail.tail != null) {
                                if (matchValue_16.tail.tail.tail == null) {
                                    if (n_56 = (matchValue_16.head | 0), n_56 !== width_4) {
                                        pattern_matching_result_35 = 0;
                                        n_57 = matchValue_16.head;
                                    }
                                    else {
                                        pattern_matching_result_35 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_35 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_35 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_35 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_35 = 1;
                    }
                    switch (pattern_matching_result_35) {
                        case 0: {
                            return makeWidthInferErrorEqual(width_4, n_57, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            let pattern_matching_result_36, n_59;
                            if (matchValue_16.tail != null) {
                                if (matchValue_16.tail.tail != null) {
                                    if (matchValue_16.tail.head != null) {
                                        if (matchValue_16.tail.tail.tail == null) {
                                            if (n_58 = (matchValue_16.tail.head | 0), n_58 !== 1) {
                                                pattern_matching_result_36 = 0;
                                                n_59 = matchValue_16.tail.head;
                                            }
                                            else {
                                                pattern_matching_result_36 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_36 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_36 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_36 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_36 = 1;
                            }
                            switch (pattern_matching_result_36) {
                                case 0: {
                                    return makeWidthInferErrorEqual(1, n_59, singleton(getConnectionIdForPort_1(1)));
                                }
                                case 1: {
                                    let pattern_matching_result_37;
                                    if (matchValue_16.tail != null) {
                                        if (matchValue_16.tail.tail != null) {
                                            if (matchValue_16.tail.tail.tail == null) {
                                                pattern_matching_result_37 = 0;
                                            }
                                            else {
                                                pattern_matching_result_37 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_37 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_37 = 1;
                                    }
                                    switch (pattern_matching_result_37) {
                                        case 0: {
                                            const arg0_19 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), width_4);
                                            return new FSharpResult$2(0, arg0_19);
                                        }
                                        case 1: {
                                            const clo1_16 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                                            return clo1_16(comp.Type);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        case 17: {
            assertInputsSize(inputConnectionsWidth, 1, comp);
            const matchValue_17 = getWidthsForPorts(inputConnectionsWidth, singleton(new InputPortNumber(0, 0)));
            let pattern_matching_result_38;
            if (matchValue_17.tail != null) {
                if (matchValue_17.head != null) {
                    if (matchValue_17.tail.tail == null) {
                        if (aw = (matchValue_17.head | 0), aw === mem.AddressWidth) {
                            pattern_matching_result_38 = 1;
                        }
                        else {
                            pattern_matching_result_38 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_38 = 2;
                    }
                }
                else if (matchValue_17.tail.tail == null) {
                    pattern_matching_result_38 = 0;
                }
                else {
                    pattern_matching_result_38 = 2;
                }
            }
            else {
                pattern_matching_result_38 = 2;
            }
            switch (pattern_matching_result_38) {
                case 0: {
                    const arg0_20 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), mem.WordWidth);
                    return new FSharpResult$2(0, arg0_20);
                }
                case 1: {
                    const arg0_21 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), mem.WordWidth);
                    return new FSharpResult$2(0, arg0_21);
                }
                case 2: {
                    let pattern_matching_result_39, aw_3;
                    if (matchValue_17.tail != null) {
                        if (matchValue_17.head != null) {
                            if (matchValue_17.tail.tail == null) {
                                if (aw_2 = (matchValue_17.head | 0), aw_2 !== mem.AddressWidth) {
                                    pattern_matching_result_39 = 0;
                                    aw_3 = matchValue_17.head;
                                }
                                else {
                                    pattern_matching_result_39 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_39 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_39 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_39 = 1;
                    }
                    switch (pattern_matching_result_39) {
                        case 0: {
                            return makeWidthInferErrorEqual(mem.AddressWidth, aw_3, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            const clo1_17 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                            return clo1_17(comp.Type);
                        }
                    }
                }
            }
        }
        case 18: {
            const mem_1 = matchValue.fields[0];
            assertInputsSize(inputConnectionsWidth, 3, comp);
            const matchValue_18 = getWidthsForPorts(inputConnectionsWidth, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1), new InputPortNumber(0, 2)]));
            let pattern_matching_result_40;
            if (matchValue_18.tail != null) {
                if (matchValue_18.head != null) {
                    if (matchValue_18.tail.tail != null) {
                        if (matchValue_18.tail.head != null) {
                            if (matchValue_18.tail.tail.tail != null) {
                                if (matchValue_18.tail.tail.head != null) {
                                    if (matchValue_18.tail.tail.tail.tail == null) {
                                        if (write = (matchValue_18.tail.tail.head | 0), (datain = (matchValue_18.tail.head | 0), (addr = (matchValue_18.head | 0), ((addr === mem_1.AddressWidth) ? (datain === mem_1.WordWidth) : false) ? (write === 1) : false))) {
                                            pattern_matching_result_40 = 0;
                                        }
                                        else {
                                            pattern_matching_result_40 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_40 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_40 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_40 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_40 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_40 = 1;
                    }
                }
                else {
                    pattern_matching_result_40 = 1;
                }
            }
            else {
                pattern_matching_result_40 = 1;
            }
            switch (pattern_matching_result_40) {
                case 0: {
                    const arg0_22 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), mem_1.WordWidth);
                    return new FSharpResult$2(0, arg0_22);
                }
                case 1: {
                    let pattern_matching_result_41, addr_3;
                    if (matchValue_18.tail != null) {
                        if (matchValue_18.head != null) {
                            if (matchValue_18.tail.tail != null) {
                                if (matchValue_18.tail.tail.tail != null) {
                                    if (matchValue_18.tail.tail.tail.tail == null) {
                                        if (addr_2 = (matchValue_18.head | 0), addr_2 !== mem_1.AddressWidth) {
                                            pattern_matching_result_41 = 0;
                                            addr_3 = matchValue_18.head;
                                        }
                                        else {
                                            pattern_matching_result_41 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_41 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_41 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_41 = 1;
                            }
                        }
                        else {
                            pattern_matching_result_41 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_41 = 1;
                    }
                    switch (pattern_matching_result_41) {
                        case 0: {
                            return makeWidthInferErrorEqual(mem_1.AddressWidth, addr_3, singleton(getConnectionIdForPort_1(0)));
                        }
                        case 1: {
                            let pattern_matching_result_42, datain_3;
                            if (matchValue_18.tail != null) {
                                if (matchValue_18.tail.tail != null) {
                                    if (matchValue_18.tail.head != null) {
                                        if (matchValue_18.tail.tail.tail != null) {
                                            if (matchValue_18.tail.tail.tail.tail == null) {
                                                if (datain_2 = (matchValue_18.tail.head | 0), datain_2 !== mem_1.WordWidth) {
                                                    pattern_matching_result_42 = 0;
                                                    datain_3 = matchValue_18.tail.head;
                                                }
                                                else {
                                                    pattern_matching_result_42 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_42 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_42 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_42 = 1;
                                    }
                                }
                                else {
                                    pattern_matching_result_42 = 1;
                                }
                            }
                            else {
                                pattern_matching_result_42 = 1;
                            }
                            switch (pattern_matching_result_42) {
                                case 0: {
                                    return makeWidthInferErrorEqual(mem_1.WordWidth, datain_3, singleton(getConnectionIdForPort_1(1)));
                                }
                                case 1: {
                                    let pattern_matching_result_43, write_3;
                                    if (matchValue_18.tail != null) {
                                        if (matchValue_18.tail.tail != null) {
                                            if (matchValue_18.tail.tail.tail != null) {
                                                if (matchValue_18.tail.tail.head != null) {
                                                    if (matchValue_18.tail.tail.tail.tail == null) {
                                                        if (write_2 = (matchValue_18.tail.tail.head | 0), write_2 !== 1) {
                                                            pattern_matching_result_43 = 0;
                                                            write_3 = matchValue_18.tail.tail.head;
                                                        }
                                                        else {
                                                            pattern_matching_result_43 = 1;
                                                        }
                                                    }
                                                    else {
                                                        pattern_matching_result_43 = 1;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_43 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_43 = 1;
                                            }
                                        }
                                        else {
                                            pattern_matching_result_43 = 1;
                                        }
                                    }
                                    else {
                                        pattern_matching_result_43 = 1;
                                    }
                                    switch (pattern_matching_result_43) {
                                        case 0: {
                                            return makeWidthInferErrorEqual(1, write_3, singleton(getConnectionIdForPort_1(2)));
                                        }
                                        case 1: {
                                            let pattern_matching_result_44;
                                            if (matchValue_18.tail != null) {
                                                if (matchValue_18.tail.tail != null) {
                                                    if (matchValue_18.tail.tail.tail != null) {
                                                        if (matchValue_18.tail.tail.tail.tail == null) {
                                                            pattern_matching_result_44 = 0;
                                                        }
                                                        else {
                                                            pattern_matching_result_44 = 1;
                                                        }
                                                    }
                                                    else {
                                                        pattern_matching_result_44 = 1;
                                                    }
                                                }
                                                else {
                                                    pattern_matching_result_44 = 1;
                                                }
                                            }
                                            else {
                                                pattern_matching_result_44 = 1;
                                            }
                                            switch (pattern_matching_result_44) {
                                                case 0: {
                                                    const arg0_23 = FSharpMap__Add(empty_1(), getOutputPortId(comp, 0), mem_1.WordWidth);
                                                    return new FSharpResult$2(0, arg0_23);
                                                }
                                                case 1: {
                                                    const clo1_18 = toFail(printf("what? Impossible case in case in calculateOutputPortsWidth for: %A"));
                                                    return clo1_18(comp.Type);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

function findConnectionToInputPort(inputPortIdsToConnectionIds, portId) {
    return FSharpMap__TryFind(inputPortIdsToConnectionIds, portId);
}

function findConnectionsFromOutputPort(outputPortIdsToConnections, portId) {
    return FSharpMap__TryFind(outputPortIdsToConnections, portId);
}

export function getConnectionWidth(connectionsWidth, connId) {
    const matchValue = FSharpMap__TryFind(connectionsWidth, connId);
    if (matchValue != null) {
        const width = value_1(matchValue);
        return width;
    }
    else {
        const clo1 = toFail(printf("what? getConnectionWidth received inexistent connectionId: %A"));
        return clo1(connId);
    }
}

function getInputPortsConnectionsWidth(connectionsWidth, currNode, inputPortIdsToConnectionIds) {
    let elements;
    elements = map_2((inputPort) => {
        let arg0_1, arg0;
        let _arg1;
        _arg1 = findConnectionToInputPort(inputPortIdsToConnectionIds, new InputPortId(0, inputPort.Id));
        if (_arg1 == null) {
            return [(arg0_1 = (extractComponentPortNumber(inputPort) | 0), (new InputPortNumber(0, arg0_1))), void 0];
        }
        else {
            const connId = _arg1;
            return [(arg0 = (extractComponentPortNumber(inputPort) | 0), (new InputPortNumber(0, arg0))), [getConnectionWidth(connectionsWidth, connId), connId]];
        }
    }, currNode.InputPorts);
    return ofList(elements);
}

function setConnectionWidth(connectionId, connectionWidth, connectionsWidth) {
    return FSharpMap__Add(connectionsWidth, connectionId, connectionWidth);
}

function setConnectionsWidth(connections, connWidth, connectionsWidth) {
    return fold((res, conn) => Result_Bind((tupledArg) => {
        let oldWidth, clo1, clo2, oldWidth_2;
        const connectionsWidth_1 = tupledArg[0];
        const connectionsToReturn = tupledArg[1];
        const connId = new ConnectionId(0, conn.Id);
        const matchValue = getConnectionWidth(connectionsWidth_1, connId);
        if (matchValue != null) {
            if (oldWidth = (matchValue | 0), oldWidth === connWidth) {
                const oldWidth_1 = matchValue | 0;
                return new FSharpResult$2(0, [connectionsWidth_1, connectionsToReturn]);
            }
            else {
                let pattern_matching_result, oldWidth_3;
                if (matchValue != null) {
                    if (oldWidth_2 = (matchValue | 0), oldWidth_2 !== connWidth) {
                        pattern_matching_result = 0;
                        oldWidth_3 = matchValue;
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
                        return new FSharpResult$2(1, new WidthInferError((clo1 = toText(printf("Wire has been inferred to have two different widths: %d and %d. This is probably due to an error such as a combinatorial loop.")), clo2 = clo1(oldWidth_3), clo2(connWidth)), singleton(connId)));
                    }
                    case 1: {
                        return toFail(printf("what? Impossible case in setConnectionsWidth."));
                    }
                }
            }
        }
        else {
            return new FSharpResult$2(0, [setConnectionWidth(connId, connWidth, connectionsWidth_1), cons(conn, connectionsToReturn)]);
        }
    }, res), new FSharpResult$2(0, [connectionsWidth, empty()]), connections);
}

function getComponentFromId(compId, compIdsToComps) {
    const matchValue = FSharpMap__TryFind(compIdsToComps, compId);
    if (matchValue != null) {
        const comp = matchValue;
        return comp;
    }
    else {
        const clo1 = toFail(printf("what? getComponentFromId called with invalid componentId: %A"));
        return clo1(compId);
    }
}

function infer(_arg1_0, _arg1_1, _arg1_2, _arg1_3, currNode, connectionsWidth) {
    const staticMaps = [_arg1_0, _arg1_1, _arg1_2, _arg1_3];
    const outputPortsOfBusLabels = staticMaps[3];
    const outputPortIdsToConnections = staticMaps[1];
    const inputPortIdsToConnectionIds = staticMaps[0];
    const compIdsToComps = staticMaps[2];
    const iterateChildren = (outgoingConnections, connectionsWidth_1) => {
        let children;
        children = map_2((conn) => getComponentFromId(new ComponentId(0, conn.Target.HostId), compIdsToComps), outgoingConnections);
        return fold((connectionsWidthRes, child) => Result_Bind((connectionsWidth_2) => infer(staticMaps[0], staticMaps[1], staticMaps[2], staticMaps[3], child, connectionsWidth_2), connectionsWidthRes), new FSharpResult$2(0, connectionsWidth_1), children);
    };
    let result_3;
    const inputConnectionsWidth = getInputPortsConnectionsWidth(connectionsWidth, currNode, inputPortIdsToConnectionIds);
    result_3 = calculateOutputPortsWidth(currNode, outputPortsOfBusLabels, inputConnectionsWidth);
    return Result_Bind((outputPortsWidths) => fold_1((connectionsWidthRes_1, outPortId, connWidth) => Result_Bind((connectionsWidth_3) => {
        const matchValue = findConnectionsFromOutputPort(outputPortIdsToConnections, outPortId);
        if (matchValue != null) {
            const outgoingConnections_1 = matchValue;
            const result_1 = setConnectionsWidth(outgoingConnections_1, connWidth, connectionsWidth_3);
            return Result_Bind((tupledArg) => {
                const connectionsWidth_4 = tupledArg[0];
                const updatedConnections = tupledArg[1];
                return iterateChildren(updatedConnections, connectionsWidth_4);
            }, result_1);
        }
        else {
            return new FSharpResult$2(0, connectionsWidth_3);
        }
    }, connectionsWidthRes_1), new FSharpResult$2(0, connectionsWidth), outputPortsWidths), result_3);
}

function initialiseConnectionsWidth(connections) {
    let elements;
    elements = map_2((conn) => [new ConnectionId(0, conn.Id), void 0], connections);
    return ofList(elements);
}

function getAllInputNodes(components) {
    return filter((comp) => {
        if (comp.Type.tag === 0) {
            return true;
        }
        else {
            return false;
        }
    }, components);
}

function mapInputPortIdsToConnectionIds(connections) {
    const state = new FSharpResult$2(0, empty_1());
    return fold((mapRes, conn) => Result_Bind((map) => {
        const inputPortId = new InputPortId(0, conn.Target.Id);
        const connId = new ConnectionId(0, conn.Id);
        const matchValue = FSharpMap__TryFind(map, inputPortId);
        if (matchValue != null) {
            const otherConnId = matchValue;
            return new FSharpResult$2(1, new WidthInferError("A wire must have precisely one driving component. If you want to merge two wires together, use a MergeWires component.", ofArray([connId, otherConnId])));
        }
        else {
            const arg0 = FSharpMap__Add(map, inputPortId, connId);
            return new FSharpResult$2(0, arg0);
        }
    }, mapRes), state, connections);
}

function mapComponentIdsToComponents(components) {
    let elements;
    elements = map_2((comp) => [new ComponentId(0, comp.Id), comp], components);
    return ofList(elements);
}

function mapOutputPortIdsToConnections(connections) {
    let elements;
    elements = groupBy((conn) => (new OutputPortId(0, conn.Source.Id)), connections, {
        Equals: equalsSafe,
        GetHashCode: hashSafe,
    });
    return ofList(elements);
}

function mapInputPortIdsToVirtualConnectionIds(conns, comps) {
    const mapPortIdToConnId = mapInputPortIdsToConnectionIds(conns);
    let filteredComps;
    filteredComps = filter((comp) => equalsSafe(comp.Type, new ComponentType(2)), comps);
    let targetPortIdToConId;
    let elements;
    elements = map_2((conn) => [new InputPortId(0, conn.Target.Id), new ConnectionId(0, conn.Id)], conns);
    targetPortIdToConId = ofList(elements);
    const getBusLabelConns = (compLst) => collect((comp_1) => {
        const _arg1 = tryFind_1(new InputPortId(0, item(0, comp_1.InputPorts).Id), targetPortIdToConId);
        if (_arg1 != null) {
            const cId = _arg1;
            return singleton(cId);
        }
        else {
            return empty();
        }
    }, compLst);
    let mapLabels;
    let result;
    let lst;
    let list_4;
    list_4 = groupBy((comp_2) => comp_2.Label, filteredComps, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    lst = map_2((tupledArg) => {
        let arg20, clo1, clo2, h, h_1;
        const lab = tupledArg[0];
        const compLst_1 = tupledArg[1];
        const matchValue = getBusLabelConns(compLst_1);
        let pattern_matching_result, cId_1, h_2;
        if (matchValue.tail != null) {
            if (matchValue.tail.tail == null) {
                pattern_matching_result = 0;
                cId_1 = matchValue.head;
            }
            else if (h = matchValue, length(h) !== 1) {
                pattern_matching_result = 1;
                h_2 = matchValue;
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else if (h_1 = matchValue, length(h_1) !== 1) {
            pattern_matching_result = 1;
            h_2 = matchValue;
        }
        else {
            pattern_matching_result = 2;
        }
        switch (pattern_matching_result) {
            case 0: {
                const arg0 = map_2((comp_3) => [new InputPortId(0, item(0, comp_3.InputPorts).Id), cId_1], compLst_1);
                return new FSharpResult$2(0, arg0);
            }
            case 1: {
                return new FSharpResult$2(1, new WidthInferError((arg20 = (length(h_2) | 0), (clo1 = toText(printf("A Labelled wire must no more than one driving component. \u0027%s\u0027 labels have %d drivers")), clo2 = clo1(lab), clo2(arg20))), h_2));
            }
            case 2: {
                return new FSharpResult$2(0, empty());
            }
        }
    }, list_4);
    result = tryFindError(lst);
    mapLabels = Result_Map((arg) => {
        let elements_1;
        elements_1 = concat(arg);
        return ofList(elements_1);
    }, result);
    const matchValue_1 = [mapLabels, mapPortIdToConnId];
    let pattern_matching_result_1, e, map, mapL;
    const copyOfStruct = matchValue_1[1];
    if (copyOfStruct.tag === 0) {
        const copyOfStruct_1 = matchValue_1[0];
        if (copyOfStruct_1.tag === 0) {
            pattern_matching_result_1 = 1;
            map = copyOfStruct.fields[0];
            mapL = copyOfStruct_1.fields[0];
        }
        else {
            pattern_matching_result_1 = 0;
            e = copyOfStruct_1.fields[0];
        }
    }
    else {
        pattern_matching_result_1 = 0;
        e = copyOfStruct.fields[0];
    }
    switch (pattern_matching_result_1) {
        case 0: {
            return new FSharpResult$2(1, e);
        }
        case 1: {
            let arg0_1;
            let elements_2;
            let list_7;
            let list_6;
            list_6 = collect((comp_4) => comp_4.InputPorts, comps);
            list_7 = map_2((p) => (new InputPortId(0, p.Id)), list_6);
            elements_2 = collect((pId) => {
                const matchValue_2 = [tryFind_1(pId, map), tryFind_1(pId, mapL)];
                let pattern_matching_result_2, conn_1;
                if (matchValue_2[0] != null) {
                    if (matchValue_2[1] == null) {
                        pattern_matching_result_2 = 1;
                        conn_1 = matchValue_2[0];
                    }
                    else {
                        pattern_matching_result_2 = 1;
                        conn_1 = matchValue_2[1];
                    }
                }
                else if (matchValue_2[1] != null) {
                    pattern_matching_result_2 = 1;
                    conn_1 = matchValue_2[1];
                }
                else {
                    pattern_matching_result_2 = 0;
                }
                switch (pattern_matching_result_2) {
                    case 0: {
                        return empty();
                    }
                    case 1: {
                        return singleton([pId, conn_1]);
                    }
                }
            }, list_7);
            arg0_1 = ofList(elements_2);
            return new FSharpResult$2(0, arg0_1);
        }
    }
}

export function inferConnectionsWidth(_arg1_0, _arg1_1) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    const connectionsWidth = initialiseConnectionsWidth(conns);
    const matchValue = mapInputPortIdsToVirtualConnectionIds(conns, comps);
    if (matchValue.tag === 0) {
        const inputPortIdsToVirtualConnectionIds$0027 = matchValue.fields[0];
        const staticMapComponentIdsToComponents = mapComponentIdsToComponents(comps);
        const staticMaps = [inputPortIdsToVirtualConnectionIds$0027, mapOutputPortIdsToConnections(conns), staticMapComponentIdsToComponents, makeOutputPortsOfLabels(comps)];
        return fold((connectionsWidthRes, inputNode) => Result_Bind((connectionsWidth_1) => infer(staticMaps[0], staticMaps[1], staticMaps[2], staticMaps[3], inputNode, connectionsWidth_1), connectionsWidthRes), new FSharpResult$2(0, connectionsWidth), comps);
    }
    else {
        const e = matchValue.fields[0];
        return new FSharpResult$2(1, e);
    }
}

