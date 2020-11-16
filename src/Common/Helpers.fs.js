import { toConsole, toFail, join, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { rangeNumber, map, truncate } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { item as item_1, ofSeq, fold, cons, find, empty, singleton, append, splitAt, length, map as map_1, tryFind } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { fromValue, compare, fromBits, op_LeftShift } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { MatchFailureException } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { FSharpMap__get_Count, ofArray, add, FSharpMap__get_Item, ofList, tryFind as tryFind_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { defaultArg } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { NLSource, NLTarget, ConnectionId, NetListComponent, OutputPortNumber, InputPortNumber, ComponentId } from "./CommonTypes.fs.js";
import { partialApply } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { fold as fold_1, copy, map as map_2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";

export function shortPComp(comp) {
    const matchValue = comp.Type;
    if (matchValue.tag === 16) {
        const sc = matchValue.fields[0];
        const clo1 = toText(printf("%s:Custom.%s.%A-\u003e%A"));
        const clo2 = clo1(comp.Label);
        const clo3 = clo2(sc.Name);
        const clo4 = clo3(sc.InputLabels);
        return clo4(sc.OutputLabels);
    }
    else {
        const clo1_1 = toText(printf("%s:%A"));
        const clo2_1 = clo1_1(comp.Label);
        return clo2_1(comp.Type);
    }
}

export function sprintInitial(n, s) {
    let strings;
    let source_1;
    source_1 = truncate(n, s.split(""));
    strings = map((value) => value, source_1);
    return join("", strings);
}

export function assertThat(cond, msg) {
    if (!cond) {
        const clo1 = toFail(printf("what? assert failed: %s"));
        clo1(msg);
    }
}

export function tryFindError(lst) {
    const isError = (el) => {
        if (el.tag === 0) {
            return false;
        }
        else {
            return true;
        }
    };
    const extractOk = (el_1) => {
        if (el_1.tag === 1) {
            throw (new Error("what? Impossible case in tryFindError"));
        }
        else {
            const ok = el_1.fields[0];
            return ok;
        }
    };
    const matchValue = tryFind(isError, lst);
    if (matchValue == null) {
        const arg0 = map_1(extractOk, lst);
        return new FSharpResult$2(0, arg0);
    }
    else {
        const copyOfStruct = matchValue;
        if (copyOfStruct.tag === 1) {
            const err = copyOfStruct.fields[0];
            return new FSharpResult$2(1, err);
        }
        else {
            throw (new Error("what? Impossible case in tryFindError"));
        }
    }
}

export function pow2(exponent) {
    return 1 << exponent;
}

export function pow2int64(exponent) {
    return op_LeftShift(fromBits(1, 0, false), exponent);
}

export function listSet(lst, item, idx) {
    let msg;
    const arg20 = length(lst) | 0;
    const clo1 = toText(printf("Index out of range in listSet. Idx: %d, list length: %d"));
    const clo2 = clo1(idx);
    msg = clo2(arg20);
    const cond = (idx >= 0) ? (idx < length(lst)) : false;
    assertThat(cond, msg);
    const patternInput = splitAt(idx, lst);
    const p2 = patternInput[1];
    const p1 = patternInput[0];
    const patternInput_1 = splitAt(1, p2);
    const p2_1 = patternInput_1[1];
    return append(p1, append(singleton(item), p2_1));
}

export function cropToLength(len, fromStart, str) {
    const matchValue = str.length <= len;
    if (matchValue) {
        return str;
    }
    else if (fromStart) {
        return str.slice(void 0, (len - 1) + 1) + "...";
    }
    else if (matchValue) {
        throw (new MatchFailureException("C:/github/issie/src/Common/Helpers.fs", 61, 10));
    }
    else {
        return "..." + str.slice(str.length - len, str.length);
    }
}

export function getMemData(address, memData) {
    let clo1, clo2;
    assertThat((memData.AddressWidth > 63) ? true : (compare(op_LeftShift(fromBits(1, 0, true), memData.AddressWidth), fromValue(address, true)) > 0), (clo1 = toText(printf("Inconsistent memory access: address %A, memData %A")), clo2 = clo1(address), clo2(memData)));
    const option = tryFind_1(address, memData.Data);
    const value = fromBits(0, 0, false);
    return defaultArg(option, value);
}

export function getNetList(_arg1_0, _arg1_1) {
    const _arg1 = [_arg1_0, _arg1_1];
    const conns = _arg1[1];
    const comps = _arg1[0];
    const id2X = (f) => {
        let elements;
        elements = map_1(f, comps);
        return ofList(elements);
    };
    const id2Outs = id2X((c) => [new ComponentId(0, c.Id), c.OutputPorts]);
    const id2Ins = id2X((c_1) => [new ComponentId(0, c_1.Id), c_1.InputPorts]);
    const id2Comp = id2X((c_2) => [new ComponentId(0, c_2.Id), c_2]);
    const getPortInts = (sel, initV, ports) => {
        let elements_1;
        elements_1 = map_1((port) => {
            const matchValue = port.PortNumber;
            if (matchValue != null) {
                const pn = matchValue | 0;
                return [sel(pn), initV];
            }
            else {
                const clo1 = toFail(printf("Missing port in list %A"));
                return clo1(ports);
            }
        }, ports);
        return ofList(elements_1);
    };
    let initNets;
    let elements_2;
    let list_3;
    list_3 = map_1((comp) => (new NetListComponent(new ComponentId(0, comp.Id), comp.Type, comp.Label, getPortInts((arg0) => (new InputPortNumber(0, arg0)), void 0, comp.InputPorts), getPortInts((arg0_1) => (new OutputPortNumber(0, arg0_1)), empty(), comp.OutputPorts))), comps);
    elements_2 = map_1((comp_1) => [comp_1.Id, comp_1], list_3);
    initNets = ofList(elements_2);
    const getOutputPortNumber = (p) => {
        let arg0_2;
        let p_1;
        const list_4 = FSharpMap__get_Item(id2Outs, new ComponentId(0, p.HostId));
        p_1 = find((p1) => (p1.Id === p.Id), list_4);
        const matchValue_1 = p_1.PortNumber;
        if (matchValue_1 == null) {
            const clo1_1 = toFail(printf("Missing input port number on %A"));
            arg0_2 = clo1_1(p_1.HostId);
        }
        else {
            const n = matchValue_1 | 0;
            arg0_2 = n;
        }
        return new OutputPortNumber(0, arg0_2);
    };
    const getInputPortNumber = (p_2) => {
        let arg0_3;
        let p_3;
        const list_5 = FSharpMap__get_Item(id2Ins, new ComponentId(0, p_2.HostId));
        p_3 = find((p1_1) => (p1_1.Id === p_2.Id), list_5);
        const matchValue_2 = p_3.PortNumber;
        if (matchValue_2 == null) {
            const clo1_2 = toFail(printf("Missing input port number on %A"));
            arg0_3 = clo1_2(p_3.HostId);
        }
        else {
            const n_1 = matchValue_2 | 0;
            arg0_3 = n_1;
        }
        return new InputPortNumber(0, arg0_3);
    };
    const updateNComp = (compId, updateFn, nets) => add(compId, updateFn(FSharpMap__get_Item(nets, compId)), nets);
    const updateInputPorts = (pNum, src, comp_2) => {
        const Inputs = add(pNum, src, comp_2.Inputs);
        return new NetListComponent(comp_2.Id, comp_2.Type, comp_2.Label, Inputs, comp_2.Outputs);
    };
    const updateInputsComp = (compId_1, pNum_1, src_1, nets_1) => {
        const uFn = partialApply(1, updateInputPorts, [pNum_1, src_1]);
        return updateNComp(compId_1, uFn, nets_1);
    };
    const updateOutputPorts = (pNum_2, tgt, comp_3) => {
        const Outputs = add(pNum_2, cons(tgt, FSharpMap__get_Item(comp_3.Outputs, pNum_2)), comp_3.Outputs);
        return new NetListComponent(comp_3.Id, comp_3.Type, comp_3.Label, comp_3.Inputs, Outputs);
    };
    const updateOutputsComp = (compId_2, pNum_3, tgt_1, nets_2) => {
        const uFn_1 = partialApply(1, updateOutputPorts, [pNum_3, tgt_1]);
        return updateNComp(compId_2, uFn_1, nets_2);
    };
    const target = (conn) => (new NLTarget(new ComponentId(0, conn.Target.HostId), getInputPortNumber(conn.Target), new ConnectionId(0, conn.Id)));
    const source = (conn_1) => (new NLSource(new ComponentId(0, conn_1.Source.HostId), getOutputPortNumber(conn_1.Source), new ConnectionId(0, conn_1.Id)));
    const addConnectionsToNets = (nets_3, conn_2) => {
        const tgt_2 = target(conn_2);
        const src_2 = source(conn_2);
        const tComp = FSharpMap__get_Item(id2Comp, tgt_2.TargetCompId);
        const sComp = FSharpMap__get_Item(id2Comp, src_2.SourceCompId);
        return updateInputsComp(new ComponentId(0, tComp.Id), tgt_2.InputPort, src_2, updateOutputsComp(new ComponentId(0, sComp.Id), src_2.OutputPort, tgt_2, nets_3));
    };
    return fold(addConnectionsToNets, initNets, conns);
}

export function checkPerformance(m, n, startTimer, stopTimer) {
    const clo1 = toConsole(printf("Checking performance with size = %d, iterations = %d"));
    const clo2 = clo1(m);
    clo2(n);
    const arrayBuffer = () => {
        let buff;
        const array = Int32Array.from(rangeNumber(0, 1, m - 1));
        buff = map_2((i) => ((i + 1) % m), array, Int32Array);
        let index = 0;
        let el = 0;
        startTimer("Array");
        while (index < n) {
            index = (index + 1);
            el = buff[el];
        }
        const value = el | 0;
        void value;
        stopTimer("Array");
    };
    const mutableArrayBuffer = () => {
        let buff_1;
        const array_1 = Int32Array.from(rangeNumber(0, 1, m - 1));
        buff_1 = map_2((i_1) => ((i_1 + 1) % m), array_1, Int32Array);
        let index_1 = 0;
        let el_1 = 0;
        startTimer("Mutable Array");
        while (index_1 < n) {
            index_1 = (index_1 + 1);
            el_1 = (((el_1 + 1) < m) ? (el_1 + 1) : 0);
            buff_1[el_1] = index_1;
        }
        void buff_1;
        stopTimer("Mutable Array");
    };
    const updateArrayBuffer = () => {
        let buff_2;
        const array_2 = Int32Array.from(rangeNumber(0, 1, m - 1));
        buff_2 = map_2((i_2) => ((i_2 + 1) % m), array_2, Int32Array);
        let index_2 = 0;
        let el_2 = 0;
        let arr = buff_2;
        startTimer("Copy-update Array");
        let z;
        const list = ofSeq(rangeNumber(0, 1, n));
        z = fold((buff_3, i_3) => {
            const r = copy(buff_3);
            r[i_3 % m] = i_3;
            return r;
        }, buff_2, list);
        const value_2 = z[0] | 0;
        void value_2;
        stopTimer("Copy-update Array");
    };
    const listBuffer = () => {
        let buff_4;
        const list_1 = ofSeq(rangeNumber(0, 1, m - 1));
        buff_4 = map_1((i_4) => ((i_4 + 1) % m), list_1);
        let index_3 = 0;
        let el_3 = 0;
        startTimer("List");
        while (index_3 < n) {
            index_3 = (index_3 + 1);
            el_3 = item_1(el_3, buff_4);
        }
        const value_3 = el_3 | 0;
        void value_3;
        stopTimer("List");
    };
    const mapBuffer = () => {
        let buff_5;
        let elements;
        const array_3 = Int32Array.from(rangeNumber(0, 1, m - 1));
        elements = map_2((i_5) => [i_5, (i_5 + 1) % m], array_3);
        buff_5 = ofArray(elements);
        let index_4 = 0;
        let el_4 = 0;
        startTimer("Map");
        while (index_4 < n) {
            index_4 = (index_4 + 1);
            el_4 = FSharpMap__get_Item(buff_5, el_4);
        }
        const value_4 = index_4 | 0;
        void value_4;
        stopTimer("Map");
    };
    const updateMapBuffer = () => {
        let buff_6;
        let elements_1;
        const array_4 = Int32Array.from(rangeNumber(0, 1, m - 1));
        elements_1 = map_2((i_6) => [i_6, (i_6 + 1) % m], array_4);
        buff_6 = ofArray(elements_1);
        startTimer("UpdateMap");
        let buf;
        const array_5 = Int32Array.from(rangeNumber(0, 1, n - 1));
        buf = fold_1((buff_7, i_7) => add(i_7 % m, i_7, buff_7), buff_6, array_5);
        stopTimer("UpdateMap");
        const value_5 = FSharpMap__get_Count(buf) | 0;
        void value_5;
    };
    arrayBuffer();
    arrayBuffer();
    mutableArrayBuffer();
    mutableArrayBuffer();
    updateArrayBuffer();
    updateArrayBuffer();
    listBuffer();
    listBuffer();
    mapBuffer();
    mapBuffer();
    updateMapBuffer();
    updateMapBuffer();
}

