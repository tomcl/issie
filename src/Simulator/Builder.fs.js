import { value as value_2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { toText, printf, toFail } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { SimulationComponent, SimulationComponentState, ReducerOutput, IsClockTick, Bit } from "./SimulatorTypes.fs.js";
import { tryFind as tryFind_1, FSharpMap__get_Item, ofList, empty as empty_1, FSharpMap__Add, add, FSharpMap__TryFind, FSharpMap__get_Count } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { getMemData, assertThat } from "../Common/Helpers.fs.js";
import { structuralHash, equals, equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { indexed, groupBy, tryFind, filter, collect, fold, append, ofSeq, splitAt, reduce, map as map_2, replicate, getSlice, ofArray, singleton, item, empty, cons, length } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { convertIntToWireData, convertWireDataToInt } from "./NumberHelpers.fs.js";
import { Connection, ComponentLabel, InputPortId, ComponentId, OutputPortId, ComponentType, OutputPortNumber, InputPortNumber, Memory } from "../Common/CommonTypes.fs.js";
import { toInt, op_Addition, fromInteger } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { rangeNumber } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { replicate as replicate_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { analyseState } from "./CanvasStateAnalyser.fs.js";
import { FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";

function getPortNumberOrFail(port) {
    if (port != null) {
        const p = value_2(port);
        return p;
    }
    else {
        return toFail(printf("what? Component ports should always have a portNumber"));
    }
}

function bitNot(bit) {
    if (bit.tag === 1) {
        return new Bit(0);
    }
    else {
        return new Bit(1);
    }
}

function bitAnd(bit0, bit1) {
    const matchValue = [bit0, bit1];
    let pattern_matching_result;
    if (matchValue[0].tag === 1) {
        if (matchValue[1].tag === 1) {
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
            return new Bit(1);
        }
        case 1: {
            return new Bit(0);
        }
    }
}

function bitOr(bit0, bit1) {
    const matchValue = [bit0, bit1];
    let pattern_matching_result;
    if (matchValue[0].tag === 0) {
        if (matchValue[1].tag === 0) {
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
            return new Bit(0);
        }
        case 1: {
            return new Bit(1);
        }
    }
}

function bitXor(bit0, bit1) {
    const matchValue = [bit0, bit1];
    let pattern_matching_result;
    if (matchValue[0].tag === 1) {
        if (matchValue[1].tag === 0) {
            pattern_matching_result = 0;
        }
        else {
            pattern_matching_result = 1;
        }
    }
    else if (matchValue[1].tag === 1) {
        pattern_matching_result = 0;
    }
    else {
        pattern_matching_result = 1;
    }
    switch (pattern_matching_result) {
        case 0: {
            return new Bit(1);
        }
        case 1: {
            return new Bit(0);
        }
    }
}

function bitNand(bit0, bit1) {
    const bit = bitAnd(bit0, bit1);
    return bitNot(bit);
}

function bitNor(bit0, bit1) {
    const bit = bitOr(bit0, bit1);
    return bitNot(bit);
}

function bitXnor(bit0, bit1) {
    const bit = bitXor(bit0, bit1);
    return bitNot(bit);
}

function assertNotTooManyInputs(reducerInput, cType, expected) {
    let msg;
    const arg20 = FSharpMap__get_Count(reducerInput.Inputs) | 0;
    const clo1 = toText(printf("assertNotTooManyInputs failed for %A: %d \u003e %d"));
    const clo2 = clo1(cType);
    const clo3 = clo2(arg20);
    msg = clo3(expected);
    const cond = FSharpMap__get_Count(reducerInput.Inputs) <= expected;
    assertThat(cond, msg);
}

function assertNoClockTick(reducerInput, cType) {
    let msg;
    const clo1 = toText(printf("Unexpected IsClockTick = Yes in combinational logic reducer input for %A"));
    msg = clo1(cType);
    const cond = equalsSafe(reducerInput.IsClockTick, new IsClockTick(0));
    assertThat(cond, msg);
}

function assertValidBus(bus, minWidth, compType) {
    let msg;
    const arg20 = length(bus) | 0;
    const clo1 = toText(printf("%A bus has invalid width: %d \u003c %d"));
    const clo2 = clo1(compType);
    const clo3 = clo2(arg20);
    msg = clo3(minWidth);
    const cond = length(bus) >= minWidth;
    assertThat(cond, msg);
}

function getValuesForPorts(inputs, portNumbers) {
    if (portNumbers.tail != null) {
        const portNumbers$0027 = portNumbers.tail;
        const portNumber = portNumbers.head;
        const matchValue = FSharpMap__TryFind(inputs, portNumber);
        if (matchValue != null) {
            const wireData = matchValue;
            const matchValue_1 = getValuesForPorts(inputs, portNumbers$0027);
            if (matchValue_1 != null) {
                const values = matchValue_1;
                return cons(wireData, values);
            }
            else {
                return void 0;
            }
        }
        else {
            return void 0;
        }
    }
    else {
        return empty();
    }
}

function extractBit(wireData) {
    let msg;
    const clo1 = toText(printf("extractBit called with wireData: %A"));
    msg = clo1(wireData);
    const cond = length(wireData) === 1;
    assertThat(cond, msg);
    return item(0, wireData);
}

function packBit(bit) {
    return singleton(bit);
}

function readMemory(mem, address) {
    const intAddr = convertWireDataToInt(address);
    const outDataInt = getMemData(intAddr, mem);
    return convertIntToWireData(mem.WordWidth, outDataInt);
}

function writeMemory(mem, address, data) {
    const intAddr = convertWireDataToInt(address);
    const intData = convertWireDataToInt(data);
    const Data = add(intAddr, intData, mem.Data);
    return new Memory(mem.AddressWidth, mem.WordWidth, Data);
}

function notReadyReducerOutput(state) {
    return new ReducerOutput(void 0, void 0, state);
}

function makeReducerOutput(state, outputs) {
    return new ReducerOutput(outputs, void 0, state);
}

function getBinaryGateReducer(op, componentType, reducerInput) {
    assertNoClockTick(reducerInput, componentType);
    assertNotTooManyInputs(reducerInput, componentType, 2);
    const matchValue = getValuesForPorts(reducerInput.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
    let pattern_matching_result, bit0, bit1;
    if (matchValue != null) {
        if (matchValue.tail != null) {
            if (matchValue.tail.tail != null) {
                if (matchValue.tail.tail.tail == null) {
                    pattern_matching_result = 1;
                    bit0 = matchValue.head;
                    bit1 = matchValue.tail.head;
                }
                else {
                    pattern_matching_result = 2;
                }
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else {
            pattern_matching_result = 2;
        }
    }
    else {
        pattern_matching_result = 0;
    }
    switch (pattern_matching_result) {
        case 0: {
            return notReadyReducerOutput(new SimulationComponentState(0));
        }
        case 1: {
            const bit0_1 = extractBit(bit0);
            const bit1_1 = extractBit(bit1);
            const outputs = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), packBit(op(bit1_1, bit0_1)));
            return makeReducerOutput(new SimulationComponentState(0), outputs);
        }
        case 2: {
            const clo1 = toFail(printf("what? Unexpected inputs to %A: %A"));
            const clo2 = clo1(componentType);
            return clo2(reducerInput);
        }
    }
}

function getDffStateBit(state) {
    if (state.tag === 1) {
        const bit = state.fields[0];
        return bit;
    }
    else {
        const clo1 = toFail(printf("what? getDffStateBit called with an invalid state: %A"));
        return clo1(state);
    }
}

function getRegisterStateBits(state) {
    if (state.tag === 2) {
        const bits = state.fields[0];
        return bits;
    }
    else {
        const clo1 = toFail(printf("what? getRegisterStateBits called with an invalid state: %A"));
        return clo1(state);
    }
}

function getRamStateMemory(state) {
    if (state.tag === 3) {
        const memory = state.fields[0];
        return memory;
    }
    else {
        const clo1 = toFail(printf("what? getRamStateMemory called with an invalid state: %A"));
        return clo1(state);
    }
}

function getReducer(componentType) {
    switch (componentType.tag) {
        case 4: {
            const width_1 = componentType.fields[0] | 0;
            const cVal = componentType.fields[1] | 0;
            return (reducerInput_1) => {
                assertNoClockTick(reducerInput_1, componentType);
                assertNotTooManyInputs(reducerInput_1, componentType, 1);
                const outputs_1 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), convertIntToWireData(width_1, fromInteger(cVal >>> 0, false, 6)));
                return makeReducerOutput(new SimulationComponentState(0), outputs_1);
            };
        }
        case 1: {
            const width_2 = componentType.fields[0] | 0;
            return (reducerInput_2) => {
                assertNoClockTick(reducerInput_2, componentType);
                assertNotTooManyInputs(reducerInput_2, componentType, 1);
                const matchValue_1 = getValuesForPorts(reducerInput_2.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result, bits_1;
                if (matchValue_1 != null) {
                    if (matchValue_1.tail != null) {
                        if (matchValue_1.tail.tail == null) {
                            pattern_matching_result = 1;
                            bits_1 = matchValue_1.head;
                        }
                        else {
                            pattern_matching_result = 2;
                        }
                    }
                    else {
                        pattern_matching_result = 2;
                    }
                }
                else {
                    pattern_matching_result = 0;
                }
                switch (pattern_matching_result) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg_1;
                        const arg20_2 = length(bits_1) | 0;
                        const clo1_2 = toText(printf("Output node reducer received wrong number of bits: expected %d but got %d"));
                        const clo2_2 = clo1_2(width_2);
                        msg_1 = clo2_2(arg20_2);
                        const cond_1 = length(bits_1) === width_2;
                        assertThat(cond_1, msg_1);
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 2: {
                        const clo1_3 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_3 = clo1_3(componentType);
                        return clo2_3(reducerInput_2);
                    }
                }
            };
        }
        case 2: {
            return (reducerInput_3) => {
                assertNoClockTick(reducerInput_3, componentType);
                assertNotTooManyInputs(reducerInput_3, componentType, 1);
                const matchValue_2 = getValuesForPorts(reducerInput_3.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_1, bits_2;
                if (matchValue_2 != null) {
                    if (matchValue_2.tail != null) {
                        if (matchValue_2.tail.tail == null) {
                            pattern_matching_result_1 = 1;
                            bits_2 = matchValue_2.head;
                        }
                        else {
                            pattern_matching_result_1 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_1 = 2;
                    }
                }
                else {
                    pattern_matching_result_1 = 0;
                }
                switch (pattern_matching_result_1) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        const out = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), bits_2);
                        return makeReducerOutput(new SimulationComponentState(0), out);
                    }
                    case 2: {
                        const clo1_4 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_4 = clo1_4(componentType);
                        return clo2_4(reducerInput_3);
                    }
                }
            };
        }
        case 5: {
            return (reducerInput_4) => {
                assertNoClockTick(reducerInput_4, componentType);
                assertNotTooManyInputs(reducerInput_4, componentType, 1);
                const matchValue_3 = getValuesForPorts(reducerInput_4.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_2, bit;
                if (matchValue_3 != null) {
                    if (matchValue_3.tail != null) {
                        if (matchValue_3.tail.tail == null) {
                            pattern_matching_result_2 = 1;
                            bit = matchValue_3.head;
                        }
                        else {
                            pattern_matching_result_2 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_2 = 2;
                    }
                }
                else {
                    pattern_matching_result_2 = 0;
                }
                switch (pattern_matching_result_2) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        const bit_1 = extractBit(bit);
                        const outputs_2 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), packBit(bitNot(bit_1)));
                        return makeReducerOutput(new SimulationComponentState(0), outputs_2);
                    }
                    case 2: {
                        const clo1_5 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_5 = clo1_5(componentType);
                        return clo2_5(reducerInput_4);
                    }
                }
            };
        }
        case 3: {
            const width_3 = componentType.fields[0] | 0;
            const lsb = componentType.fields[1] | 0;
            return (reducerInput_5) => {
                assertNoClockTick(reducerInput_5, componentType);
                assertNotTooManyInputs(reducerInput_5, componentType, 1);
                const matchValue_4 = getValuesForPorts(reducerInput_5.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_3, bits_3;
                if (matchValue_4 != null) {
                    if (matchValue_4.tail != null) {
                        if (matchValue_4.tail.tail == null) {
                            pattern_matching_result_3 = 1;
                            bits_3 = matchValue_4.head;
                        }
                        else {
                            pattern_matching_result_3 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_3 = 2;
                    }
                }
                else {
                    pattern_matching_result_3 = 0;
                }
                switch (pattern_matching_result_3) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg_2;
                        const arg10_6 = (width_3 + lsb) | 0;
                        const arg20_6 = length(bits_3) | 0;
                        const clo1_6 = toText(printf("Bus Selection received too few bits: expected at least %d but got %d"));
                        const clo2_6 = clo1_6(arg10_6);
                        msg_2 = clo2_6(arg20_6);
                        const cond_2 = length(bits_3) >= (width_3 + lsb);
                        assertThat(cond_2, msg_2);
                        const outBits = getSlice(lsb, (lsb + width_3) - 1, bits_3);
                        const out_1 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), outBits);
                        return makeReducerOutput(new SimulationComponentState(0), out_1);
                    }
                    case 2: {
                        const clo1_7 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_7 = clo1_7(componentType);
                        return clo2_7(reducerInput_5);
                    }
                }
            };
        }
        case 6: {
            return (reducerInput_6) => getBinaryGateReducer(bitAnd, new ComponentType(6), reducerInput_6);
        }
        case 7: {
            return (reducerInput_7) => getBinaryGateReducer(bitOr, new ComponentType(7), reducerInput_7);
        }
        case 8: {
            return (reducerInput_8) => getBinaryGateReducer(bitXor, new ComponentType(8), reducerInput_8);
        }
        case 9: {
            return (reducerInput_9) => getBinaryGateReducer(bitNand, new ComponentType(9), reducerInput_9);
        }
        case 10: {
            return (reducerInput_10) => getBinaryGateReducer(bitNor, new ComponentType(10), reducerInput_10);
        }
        case 11: {
            return (reducerInput_11) => getBinaryGateReducer(bitXnor, new ComponentType(11), reducerInput_11);
        }
        case 13: {
            return (reducerInput_12) => {
                assertNoClockTick(reducerInput_12, componentType);
                assertNotTooManyInputs(reducerInput_12, componentType, 3);
                const matchValue_5 = getValuesForPorts(reducerInput_12.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1), new InputPortNumber(0, 2)]));
                let pattern_matching_result_4, bitSelect, bits0, bits1;
                if (matchValue_5 != null) {
                    if (matchValue_5.tail != null) {
                        if (matchValue_5.tail.tail != null) {
                            if (matchValue_5.tail.tail.tail != null) {
                                if (matchValue_5.tail.tail.tail.tail == null) {
                                    pattern_matching_result_4 = 1;
                                    bitSelect = matchValue_5.tail.tail.head;
                                    bits0 = matchValue_5.head;
                                    bits1 = matchValue_5.tail.head;
                                }
                                else {
                                    pattern_matching_result_4 = 2;
                                }
                            }
                            else {
                                pattern_matching_result_4 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_4 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_4 = 2;
                    }
                }
                else {
                    pattern_matching_result_4 = 0;
                }
                switch (pattern_matching_result_4) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg_3;
                        const clo1_8 = toText(printf("Mux received two inputs with different widths: %A and %A"));
                        const clo2_8 = clo1_8(bits0);
                        msg_3 = clo2_8(bits1);
                        const cond_3 = length(bits0) === length(bits1);
                        assertThat(cond_3, msg_3);
                        const out_2 = equalsSafe(extractBit(bitSelect), new Bit(0)) ? bits0 : bits1;
                        const outputs_3 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), out_2);
                        return makeReducerOutput(new SimulationComponentState(0), outputs_3);
                    }
                    case 2: {
                        const clo1_9 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_9 = clo1_9(componentType);
                        return clo2_9(reducerInput_12);
                    }
                }
            };
        }
        case 14: {
            return (reducerInput_13) => {
                assertNoClockTick(reducerInput_13, componentType);
                assertNotTooManyInputs(reducerInput_13, componentType, 2);
                const matchValue_6 = getValuesForPorts(reducerInput_13.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
                let pattern_matching_result_5, bitSelect_1, bitsIn;
                if (matchValue_6 != null) {
                    if (matchValue_6.tail != null) {
                        if (matchValue_6.tail.tail != null) {
                            if (matchValue_6.tail.tail.tail == null) {
                                pattern_matching_result_5 = 1;
                                bitSelect_1 = matchValue_6.tail.head;
                                bitsIn = matchValue_6.head;
                            }
                            else {
                                pattern_matching_result_5 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_5 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_5 = 2;
                    }
                }
                else {
                    pattern_matching_result_5 = 0;
                }
                switch (pattern_matching_result_5) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        const zeros = replicate(length(bitsIn), new Bit(0));
                        const patternInput = equalsSafe(extractBit(bitSelect_1), new Bit(0)) ? [bitsIn, zeros] : [zeros, bitsIn];
                        const out1 = patternInput[1];
                        const out0 = patternInput[0];
                        const out_3 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), out0);
                        const out_4 = FSharpMap__Add(out_3, new OutputPortNumber(0, 1), out1);
                        return makeReducerOutput(new SimulationComponentState(0), out_4);
                    }
                    case 2: {
                        const clo1_10 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_10 = clo1_10(componentType);
                        return clo2_10(reducerInput_13);
                    }
                }
            };
        }
        case 15: {
            const numberOfBits = componentType.fields[0] | 0;
            return (reducerInput_14) => {
                assertNoClockTick(reducerInput_14, componentType);
                assertNotTooManyInputs(reducerInput_14, componentType, 3);
                const matchValue_7 = getValuesForPorts(reducerInput_14.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1), new InputPortNumber(0, 2)]));
                let pattern_matching_result_6, A, B, cin;
                if (matchValue_7 != null) {
                    if (matchValue_7.tail != null) {
                        if (matchValue_7.tail.tail != null) {
                            if (matchValue_7.tail.tail.tail != null) {
                                if (matchValue_7.tail.tail.tail.tail == null) {
                                    pattern_matching_result_6 = 1;
                                    A = matchValue_7.tail.head;
                                    B = matchValue_7.tail.tail.head;
                                    cin = matchValue_7.head;
                                }
                                else {
                                    pattern_matching_result_6 = 2;
                                }
                            }
                            else {
                                pattern_matching_result_6 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_6 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_6 = 2;
                    }
                }
                else {
                    pattern_matching_result_6 = 0;
                }
                switch (pattern_matching_result_6) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let patternInput_1;
                        let list_2;
                        let num;
                        let list_1;
                        list_1 = map_2(convertWireDataToInt, ofArray([cin, A, B]));
                        num = reduce(op_Addition, list_1);
                        const width_4 = (numberOfBits + 1) | 0;
                        list_2 = convertIntToWireData(width_4, num);
                        patternInput_1 = splitAt(numberOfBits, list_2);
                        const sum = patternInput_1[0];
                        const cout = patternInput_1[1];
                        const out_5 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), sum);
                        const out_6 = FSharpMap__Add(out_5, new OutputPortNumber(0, 1), cout);
                        return makeReducerOutput(new SimulationComponentState(0), out_6);
                    }
                    case 2: {
                        const clo1_11 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_11 = clo1_11(componentType);
                        return clo2_11(reducerInput_14);
                    }
                }
            };
        }
        case 12: {
            return (reducerInput_15) => {
                assertNoClockTick(reducerInput_15, componentType);
                assertNotTooManyInputs(reducerInput_15, componentType, 2);
                const matchValue_8 = getValuesForPorts(reducerInput_15.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
                let pattern_matching_result_7, data, select;
                if (matchValue_8 != null) {
                    if (matchValue_8.tail != null) {
                        if (matchValue_8.tail.tail != null) {
                            if (matchValue_8.tail.tail.tail == null) {
                                pattern_matching_result_7 = 1;
                                data = matchValue_8.tail.head;
                                select = matchValue_8.head;
                            }
                            else {
                                pattern_matching_result_7 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_7 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_7 = 2;
                    }
                }
                else {
                    pattern_matching_result_7 = 0;
                }
                switch (pattern_matching_result_7) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let out_7;
                        let selN;
                        const value = convertWireDataToInt(select);
                        selN = (~(~toInt(value)));
                        let dataN;
                        const value_1 = convertWireDataToInt(data);
                        dataN = (~(~toInt(value_1)));
                        let elements;
                        const list_3 = ofSeq(rangeNumber(0, 1, 3));
                        elements = map_2((n) => {
                            const outBit = ((n === selN) ? dataN : 0) | 0;
                            return [new OutputPortNumber(0, n), convertIntToWireData(1, fromInteger(outBit, false, 2))];
                        }, list_3);
                        out_7 = ofList(elements);
                        return makeReducerOutput(new SimulationComponentState(0), out_7);
                    }
                    case 2: {
                        const clo1_12 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_12 = clo1_12(componentType);
                        return clo2_12(reducerInput_15);
                    }
                }
            };
        }
        case 16: {
            const c = componentType.fields[0];
            return (_arg1) => {
                const clo1_13 = toFail(printf("what? Custom components reducer should be overridden before using it in a simulation: %A"));
                return clo1_13(c);
            };
        }
        case 17: {
            return (reducerInput_16) => {
                assertNoClockTick(reducerInput_16, componentType);
                assertNotTooManyInputs(reducerInput_16, componentType, 2);
                const matchValue_9 = getValuesForPorts(reducerInput_16.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
                let pattern_matching_result_8, bits0_1, bits1_1;
                if (matchValue_9 != null) {
                    if (matchValue_9.tail != null) {
                        if (matchValue_9.tail.tail != null) {
                            if (matchValue_9.tail.tail.tail == null) {
                                pattern_matching_result_8 = 1;
                                bits0_1 = matchValue_9.head;
                                bits1_1 = matchValue_9.tail.head;
                            }
                            else {
                                pattern_matching_result_8 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_8 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_8 = 2;
                    }
                }
                else {
                    pattern_matching_result_8 = 0;
                }
                switch (pattern_matching_result_8) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        const outputs_4 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), append(bits0_1, bits1_1));
                        return makeReducerOutput(new SimulationComponentState(0), outputs_4);
                    }
                    case 2: {
                        const clo1_14 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_13 = clo1_14(componentType);
                        return clo2_13(reducerInput_16);
                    }
                }
            };
        }
        case 18: {
            const topWireWidth = componentType.fields[0] | 0;
            return (reducerInput_17) => {
                assertNoClockTick(reducerInput_17, componentType);
                assertNotTooManyInputs(reducerInput_17, componentType, 1);
                const matchValue_10 = getValuesForPorts(reducerInput_17.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_9, bits_5;
                if (matchValue_10 != null) {
                    if (matchValue_10.tail != null) {
                        if (matchValue_10.tail.tail == null) {
                            pattern_matching_result_9 = 1;
                            bits_5 = matchValue_10.head;
                        }
                        else {
                            pattern_matching_result_9 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_9 = 2;
                    }
                }
                else {
                    pattern_matching_result_9 = 0;
                }
                switch (pattern_matching_result_9) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg_4;
                        const arg10_15 = (topWireWidth + 1) | 0;
                        const arg20_14 = length(bits_5) | 0;
                        const clo1_15 = toText(printf("SplitWire received too little bits: expected at least %d but got %d"));
                        const clo2_14 = clo1_15(arg10_15);
                        msg_4 = clo2_14(arg20_14);
                        const cond_4 = length(bits_5) >= (topWireWidth + 1);
                        assertThat(cond_4, msg_4);
                        const patternInput_2 = splitAt(topWireWidth, bits_5);
                        const bits1_2 = patternInput_2[1];
                        const bits0_2 = patternInput_2[0];
                        const out_8 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), bits0_2);
                        const out_9 = FSharpMap__Add(out_8, new OutputPortNumber(0, 1), bits1_2);
                        return makeReducerOutput(new SimulationComponentState(0), out_9);
                    }
                    case 2: {
                        const clo1_16 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_15 = clo1_16(componentType);
                        return clo2_15(reducerInput_17);
                    }
                }
            };
        }
        case 19: {
            return (reducerInput_18) => {
                const matchValue_11 = reducerInput_18.IsClockTick;
                if (matchValue_11.tag === 1) {
                    const dffState = matchValue_11.fields[0];
                    const stateBit = getDffStateBit(dffState);
                    let newStateBit;
                    const matchValue_12 = getValuesForPorts(reducerInput_18.Inputs, singleton(new InputPortNumber(0, 0)));
                    let pattern_matching_result_10, bit_2;
                    if (matchValue_12 != null) {
                        if (matchValue_12.tail != null) {
                            if (matchValue_12.tail.tail == null) {
                                pattern_matching_result_10 = 1;
                                bit_2 = matchValue_12.head;
                            }
                            else {
                                pattern_matching_result_10 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_10 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_10 = 0;
                    }
                    switch (pattern_matching_result_10) {
                        case 0: {
                            newStateBit = stateBit;
                            break;
                        }
                        case 1: {
                            newStateBit = extractBit(bit_2);
                            break;
                        }
                        case 2: {
                            const clo1_17 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_16 = clo1_17(componentType);
                            newStateBit = clo2_16(reducerInput_18);
                            break;
                        }
                    }
                    const newState = new SimulationComponentState(1, newStateBit);
                    const outputs_5 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), packBit(newStateBit));
                    return makeReducerOutput(newState, outputs_5);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        case 20: {
            return (reducerInput_19) => {
                const matchValue_13 = reducerInput_19.IsClockTick;
                if (matchValue_13.tag === 1) {
                    const dffState_1 = matchValue_13.fields[0];
                    const stateBit_1 = getDffStateBit(dffState_1);
                    let newStateBit_1;
                    const matchValue_14 = getValuesForPorts(reducerInput_19.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
                    let pattern_matching_result_11, bit_3, enable;
                    if (matchValue_14 != null) {
                        if (matchValue_14.tail != null) {
                            if (matchValue_14.tail.tail != null) {
                                if (matchValue_14.tail.tail.tail == null) {
                                    pattern_matching_result_11 = 1;
                                    bit_3 = matchValue_14.head;
                                    enable = matchValue_14.tail.head;
                                }
                                else {
                                    pattern_matching_result_11 = 2;
                                }
                            }
                            else {
                                pattern_matching_result_11 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_11 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_11 = 0;
                    }
                    switch (pattern_matching_result_11) {
                        case 0: {
                            newStateBit_1 = stateBit_1;
                            break;
                        }
                        case 1: {
                            newStateBit_1 = (equalsSafe(extractBit(enable), new Bit(0)) ? stateBit_1 : extractBit(bit_3));
                            break;
                        }
                        case 2: {
                            const clo1_18 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_17 = clo1_18(componentType);
                            newStateBit_1 = clo2_17(reducerInput_19);
                            break;
                        }
                    }
                    const newState_1 = new SimulationComponentState(1, newStateBit_1);
                    const outputs_6 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), packBit(newStateBit_1));
                    return makeReducerOutput(newState_1, outputs_6);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        case 21: {
            const width_5 = componentType.fields[0] | 0;
            return (reducerInput_20) => {
                const matchValue_15 = reducerInput_20.IsClockTick;
                if (matchValue_15.tag === 1) {
                    const regState = matchValue_15.fields[0];
                    const stateBits = getRegisterStateBits(regState);
                    let newStateBits;
                    const matchValue_16 = getValuesForPorts(reducerInput_20.Inputs, singleton(new InputPortNumber(0, 0)));
                    let pattern_matching_result_12, bits_6;
                    if (matchValue_16 != null) {
                        if (matchValue_16.tail != null) {
                            if (matchValue_16.tail.tail == null) {
                                pattern_matching_result_12 = 1;
                                bits_6 = matchValue_16.head;
                            }
                            else {
                                pattern_matching_result_12 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_12 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_12 = 0;
                    }
                    switch (pattern_matching_result_12) {
                        case 0: {
                            newStateBits = stateBits;
                            break;
                        }
                        case 1: {
                            let msg_5;
                            const arg20_18 = length(bits_6) | 0;
                            const clo1_19 = toText(printf("Register received data with wrong width: expected %d but got %A"));
                            const clo2_18 = clo1_19(width_5);
                            msg_5 = clo2_18(arg20_18);
                            const cond_5 = length(bits_6) === width_5;
                            assertThat(cond_5, msg_5);
                            newStateBits = bits_6;
                            break;
                        }
                        case 2: {
                            const clo1_20 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_19 = clo1_20(componentType);
                            newStateBits = clo2_19(reducerInput_20);
                            break;
                        }
                    }
                    const newState_2 = new SimulationComponentState(2, newStateBits);
                    const outputs_7 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), newStateBits);
                    return makeReducerOutput(newState_2, outputs_7);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        case 22: {
            const width_6 = componentType.fields[0] | 0;
            return (reducerInput_21) => {
                const matchValue_17 = reducerInput_21.IsClockTick;
                if (matchValue_17.tag === 1) {
                    const regState_1 = matchValue_17.fields[0];
                    const stateBits_1 = getRegisterStateBits(regState_1);
                    let newStateBits_1;
                    const matchValue_18 = getValuesForPorts(reducerInput_21.Inputs, ofArray([new InputPortNumber(0, 0), new InputPortNumber(0, 1)]));
                    let pattern_matching_result_13, bits_7, enable_1;
                    if (matchValue_18 != null) {
                        if (matchValue_18.tail != null) {
                            if (matchValue_18.tail.tail != null) {
                                if (matchValue_18.tail.tail.tail == null) {
                                    pattern_matching_result_13 = 1;
                                    bits_7 = matchValue_18.head;
                                    enable_1 = matchValue_18.tail.head;
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
                        pattern_matching_result_13 = 0;
                    }
                    switch (pattern_matching_result_13) {
                        case 0: {
                            newStateBits_1 = stateBits_1;
                            break;
                        }
                        case 1: {
                            let msg_6;
                            const arg20_20 = length(bits_7) | 0;
                            const clo1_21 = toText(printf("RegisterE received data with wrong width: expected %d but got %A"));
                            const clo2_20 = clo1_21(width_6);
                            msg_6 = clo2_20(arg20_20);
                            const cond_6 = length(bits_7) === width_6;
                            assertThat(cond_6, msg_6);
                            newStateBits_1 = (equalsSafe(extractBit(enable_1), new Bit(0)) ? stateBits_1 : bits_7);
                            break;
                        }
                        case 2: {
                            const clo1_22 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_21 = clo1_22(componentType);
                            newStateBits_1 = clo2_21(reducerInput_21);
                            break;
                        }
                    }
                    const newState_3 = new SimulationComponentState(2, newStateBits_1);
                    const outputs_8 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), newStateBits_1);
                    return makeReducerOutput(newState_3, outputs_8);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        case 23: {
            const mem = componentType.fields[0];
            return (reducerInput_22) => {
                assertNoClockTick(reducerInput_22, componentType);
                assertNotTooManyInputs(reducerInput_22, componentType, 1);
                const matchValue_19 = getValuesForPorts(reducerInput_22.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_14, addr;
                if (matchValue_19 != null) {
                    if (matchValue_19.tail != null) {
                        if (matchValue_19.tail.tail == null) {
                            pattern_matching_result_14 = 1;
                            addr = matchValue_19.head;
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
                    pattern_matching_result_14 = 0;
                }
                switch (pattern_matching_result_14) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg_7;
                        const clo1_23 = toText(printf("ROM received address with wrong width: expected %d but got %A"));
                        const clo2_22 = clo1_23(mem.AddressWidth);
                        msg_7 = clo2_22(addr);
                        const cond_7 = length(addr) === mem.AddressWidth;
                        assertThat(cond_7, msg_7);
                        const outData = readMemory(mem, addr);
                        const outputs_9 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), outData);
                        return makeReducerOutput(new SimulationComponentState(0), outputs_9);
                    }
                    case 2: {
                        const clo1_24 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_23 = clo1_24(componentType);
                        return clo2_23(reducerInput_22);
                    }
                }
            };
        }
        case 24: {
            const mem_1 = componentType.fields[0];
            return (reducerInput_23) => {
                const matchValue_20 = reducerInput_23.IsClockTick;
                if (matchValue_20.tag === 1) {
                    const state_6 = matchValue_20.fields[0];
                    assertThat(equalsSafe(state_6, new SimulationComponentState(0)), "ROM component is stateless (only defined by initial data).");
                    let address;
                    const matchValue_21 = getValuesForPorts(reducerInput_23.Inputs, singleton(new InputPortNumber(0, 0)));
                    let pattern_matching_result_15, addr_1;
                    if (matchValue_21 != null) {
                        if (matchValue_21.tail != null) {
                            if (matchValue_21.tail.tail == null) {
                                pattern_matching_result_15 = 1;
                                addr_1 = matchValue_21.head;
                            }
                            else {
                                pattern_matching_result_15 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_15 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_15 = 0;
                    }
                    switch (pattern_matching_result_15) {
                        case 0: {
                            address = replicate(mem_1.AddressWidth, new Bit(0));
                            break;
                        }
                        case 1: {
                            let msg_8;
                            const clo1_25 = toText(printf("ROM received address with wrong width: expected %d but got %A"));
                            const clo2_24 = clo1_25(mem_1.AddressWidth);
                            msg_8 = clo2_24(addr_1);
                            const cond_8 = length(addr_1) === mem_1.AddressWidth;
                            assertThat(cond_8, msg_8);
                            address = addr_1;
                            break;
                        }
                        case 2: {
                            const clo1_26 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_25 = clo1_26(componentType);
                            address = clo2_25(reducerInput_23);
                            break;
                        }
                    }
                    const outData_1 = readMemory(mem_1, address);
                    const outputs_10 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), outData_1);
                    return makeReducerOutput(new SimulationComponentState(0), outputs_10);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        case 25: {
            return (reducerInput_24) => {
                const matchValue_22 = reducerInput_24.IsClockTick;
                if (matchValue_22.tag === 1) {
                    const state_8 = matchValue_22.fields[0];
                    const mem_2 = getRamStateMemory(state_8);
                    let address_1;
                    const matchValue_23 = getValuesForPorts(reducerInput_24.Inputs, singleton(new InputPortNumber(0, 0)));
                    let pattern_matching_result_16, addr_2;
                    if (matchValue_23 != null) {
                        if (matchValue_23.tail != null) {
                            if (matchValue_23.tail.tail == null) {
                                pattern_matching_result_16 = 1;
                                addr_2 = matchValue_23.head;
                            }
                            else {
                                pattern_matching_result_16 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_16 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_16 = 0;
                    }
                    switch (pattern_matching_result_16) {
                        case 0: {
                            address_1 = replicate(mem_2.AddressWidth, new Bit(0));
                            break;
                        }
                        case 1: {
                            let msg_9;
                            const clo1_27 = toText(printf("RAM received address with wrong width: expected %d but got %A"));
                            const clo2_26 = clo1_27(mem_2.AddressWidth);
                            msg_9 = clo2_26(addr_2);
                            const cond_9 = length(addr_2) === mem_2.AddressWidth;
                            assertThat(cond_9, msg_9);
                            address_1 = addr_2;
                            break;
                        }
                        case 2: {
                            const clo1_28 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_27 = clo1_28(componentType);
                            address_1 = clo2_27(reducerInput_24);
                            break;
                        }
                    }
                    let dataIn_1;
                    const matchValue_24 = getValuesForPorts(reducerInput_24.Inputs, singleton(new InputPortNumber(0, 1)));
                    let pattern_matching_result_17, dataIn;
                    if (matchValue_24 != null) {
                        if (matchValue_24.tail != null) {
                            if (matchValue_24.tail.tail == null) {
                                pattern_matching_result_17 = 1;
                                dataIn = matchValue_24.head;
                            }
                            else {
                                pattern_matching_result_17 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_17 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_17 = 0;
                    }
                    switch (pattern_matching_result_17) {
                        case 0: {
                            dataIn_1 = replicate(mem_2.WordWidth, new Bit(0));
                            break;
                        }
                        case 1: {
                            let msg_10;
                            const clo1_29 = toText(printf("RAM received data-in with wrong width: expected %d but got %A"));
                            const clo2_28 = clo1_29(mem_2.WordWidth);
                            msg_10 = clo2_28(dataIn);
                            const cond_10 = length(dataIn) === mem_2.WordWidth;
                            assertThat(cond_10, msg_10);
                            dataIn_1 = dataIn;
                            break;
                        }
                        case 2: {
                            const clo1_30 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_29 = clo1_30(componentType);
                            dataIn_1 = clo2_29(reducerInput_24);
                            break;
                        }
                    }
                    let write;
                    const matchValue_25 = getValuesForPorts(reducerInput_24.Inputs, singleton(new InputPortNumber(0, 2)));
                    let pattern_matching_result_18, bit_4;
                    if (matchValue_25 != null) {
                        if (matchValue_25.tail != null) {
                            if (matchValue_25.tail.tail == null) {
                                pattern_matching_result_18 = 1;
                                bit_4 = matchValue_25.head;
                            }
                            else {
                                pattern_matching_result_18 = 2;
                            }
                        }
                        else {
                            pattern_matching_result_18 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_18 = 0;
                    }
                    switch (pattern_matching_result_18) {
                        case 0: {
                            write = packBit(new Bit(0));
                            break;
                        }
                        case 1: {
                            let bit_5;
                            bit_5 = extractBit(bit_4);
                            write = packBit(bit_5);
                            break;
                        }
                        case 2: {
                            const clo1_31 = toFail(printf("what? Unexpected inputs to %A: %A"));
                            const clo2_30 = clo1_31(componentType);
                            write = clo2_30(reducerInput_24);
                            break;
                        }
                    }
                    let patternInput_3;
                    const matchValue_26 = extractBit(write);
                    patternInput_3 = ((matchValue_26.tag === 1) ? [writeMemory(mem_2, address_1, dataIn_1), dataIn_1] : [mem_2, readMemory(mem_2, address_1)]);
                    const mem_3 = patternInput_3[0];
                    const dataOut = patternInput_3[1];
                    const outputs_11 = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), dataOut);
                    return makeReducerOutput(new SimulationComponentState(3, mem_3), outputs_11);
                }
                else {
                    return notReadyReducerOutput(new SimulationComponentState(0));
                }
            };
        }
        default: {
            const width = componentType.fields[0] | 0;
            return (reducerInput) => {
                assertNoClockTick(reducerInput, componentType);
                assertNotTooManyInputs(reducerInput, componentType, 1);
                const matchValue = getValuesForPorts(reducerInput.Inputs, singleton(new InputPortNumber(0, 0)));
                let pattern_matching_result_19, bits;
                if (matchValue != null) {
                    if (matchValue.tail != null) {
                        if (matchValue.tail.tail == null) {
                            pattern_matching_result_19 = 1;
                            bits = matchValue.head;
                        }
                        else {
                            pattern_matching_result_19 = 2;
                        }
                    }
                    else {
                        pattern_matching_result_19 = 2;
                    }
                }
                else {
                    pattern_matching_result_19 = 0;
                }
                switch (pattern_matching_result_19) {
                    case 0: {
                        return notReadyReducerOutput(new SimulationComponentState(0));
                    }
                    case 1: {
                        let msg;
                        const arg20 = length(bits) | 0;
                        const clo1 = toText(printf("Input node reducer received wrong number of bits: expected %d but got %d"));
                        const clo2 = clo1(width);
                        msg = clo2(arg20);
                        const cond = length(bits) === width;
                        assertThat(cond, msg);
                        const outputs = FSharpMap__Add(empty_1(), new OutputPortNumber(0, 0), bits);
                        return makeReducerOutput(new SimulationComponentState(0), outputs);
                    }
                    case 2: {
                        const clo1_1 = toFail(printf("what? Unexpected inputs to %A: %A"));
                        const clo2_1 = clo1_1(componentType);
                        return clo2_1(reducerInput);
                    }
                }
            };
        }
    }
}

function buildSourceToTargetPortMap(connections) {
    const state = empty_1();
    return fold((map, conn) => {
        const key = new OutputPortId(0, conn.Source.Id);
        const target = [new ComponentId(0, conn.Target.HostId), new InputPortId(0, conn.Target.Id)];
        let newValue;
        const matchValue = FSharpMap__TryFind(map, key);
        if (matchValue != null) {
            const oldValue = matchValue;
            newValue = cons(target, oldValue);
        }
        else {
            newValue = singleton(target);
        }
        return FSharpMap__Add(map, key, newValue);
    }, state, connections);
}

function mapInputPortIdToPortNumber(components) {
    const state_1 = empty_1();
    return fold((map, comp) => fold((map_1, port) => FSharpMap__Add(map_1, new InputPortId(0, port.Id), new InputPortNumber(0, getPortNumberOrFail(port.PortNumber))), map, comp.InputPorts), state_1, components);
}

function getDefaultState(compType) {
    let pattern_matching_result, w_1;
    switch (compType.tag) {
        case 1:
        case 2:
        case 3:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11:
        case 13:
        case 12:
        case 14:
        case 15:
        case 16:
        case 17:
        case 18:
        case 24: {
            pattern_matching_result = 0;
            break;
        }
        case 4: {
            pattern_matching_result = 1;
            break;
        }
        case 23: {
            pattern_matching_result = 2;
            break;
        }
        case 19:
        case 20: {
            pattern_matching_result = 3;
            break;
        }
        case 21: {
            pattern_matching_result = 4;
            w_1 = compType.fields[0];
            break;
        }
        case 22: {
            pattern_matching_result = 4;
            w_1 = compType.fields[0];
            break;
        }
        case 25: {
            pattern_matching_result = 5;
            break;
        }
        default: pattern_matching_result = 0}
    switch (pattern_matching_result) {
        case 0: {
            return new SimulationComponentState(0);
        }
        case 1: {
            const w = compType.fields[0] | 0;
            const c = compType.fields[1] | 0;
            return new SimulationComponentState(0);
        }
        case 2: {
            return new SimulationComponentState(0);
        }
        case 3: {
            return new SimulationComponentState(1, new Bit(0));
        }
        case 4: {
            const arg0 = replicate(w_1, new Bit(0));
            return new SimulationComponentState(2, arg0);
        }
        case 5: {
            const memory = compType.fields[0];
            return new SimulationComponentState(3, memory);
        }
    }
}

function buildSimulationComponent(sourceToTargetPort, portIdToPortNumber, comp) {
    const mapPortIdsToPortNumbers = (targets) => map_2((tupledArg) => {
        const compId = tupledArg[0];
        const portId = tupledArg[1];
        let matchValue;
        matchValue = FSharpMap__TryFind(portIdToPortNumber, portId);
        if (matchValue != null) {
            const portNumber = matchValue;
            return [compId, portNumber];
        }
        else {
            const clo1 = toFail(printf("what? Input port with portId %A has no portNumber associated"));
            return clo1(portId);
        }
    }, targets);
    let outputs;
    let elements;
    elements = collect((port) => {
        let arg0;
        let matchValue_1;
        matchValue_1 = FSharpMap__TryFind(sourceToTargetPort, new OutputPortId(0, port.Id));
        let pattern_matching_result;
        if (matchValue_1 == null) {
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
                return empty();
            }
            case 1: {
                if (matchValue_1 != null) {
                    const targets_1 = matchValue_1;
                    return singleton([(arg0 = (getPortNumberOrFail(port.PortNumber) | 0), (new OutputPortNumber(0, arg0))), mapPortIdsToPortNumbers(targets_1)]);
                }
                else {
                    const clo1_1 = toFail(printf("what? Unconnected output port %s in comp %s"));
                    const clo2 = clo1_1(port.Id);
                    return clo2(comp.Id);
                }
            }
        }
    }, comp.OutputPorts);
    outputs = ofList(elements);
    let inputs;
    const matchValue_2 = comp.Type;
    if (matchValue_2.tag === 1) {
        const width = matchValue_2.fields[0] | 0;
        inputs = FSharpMap__Add(empty_1(), new InputPortNumber(0, 0), replicate(width, new Bit(0)));
    }
    else {
        inputs = empty_1();
    }
    return new SimulationComponent(new ComponentId(0, comp.Id), comp.Type, new ComponentLabel(0, comp.Label), inputs, outputs, replicate_1(0, false), void 0, getDefaultState(comp.Type), getReducer(comp.Type));
}

export function getLabelConnections(comps, conns) {
    let labels;
    labels = filter((co) => equalsSafe(co.Type, new ComponentType(2)), comps);
    let compIdMap;
    let elements;
    elements = map_2((co_1) => [new ComponentId(0, co_1.Id), co_1], labels);
    compIdMap = ofList(elements);
    const getComp = (n) => FSharpMap__get_Item(compIdMap, n);
    let targetMap;
    let elements_1;
    elements_1 = map_2((conn) => [new ComponentId(0, conn.Target.HostId), conn], conns);
    targetMap = ofList(elements_1);
    const getConnection = (compTarget) => FSharpMap__get_Item(targetMap, new ComponentId(0, compTarget.Id));
    const copyConnection = (conn_1, compTarget_1, tagNum) => {
        let clo1;
        const Target = item(0, compTarget_1.InputPorts);
        return new Connection((clo1 = toText(printf("iolab%d")), clo1(tagNum)) + conn_1.Id, conn_1.Source, Target, conn_1.Vertices);
    };
    const getDriverConnection = (comps_1) => {
        let _arg1;
        _arg1 = tryFind((co_2) => (!equals(tryFind_1(new ComponentId(0, co_2.Id), targetMap), void 0)), comps_1);
        if (_arg1 != null) {
            const comp = _arg1;
            return FSharpMap__get_Item(targetMap, new ComponentId(0, comp.Id));
        }
        else {
            const clo1_1 = toFail(printf("What? component cannot be found in %A"));
            return clo1_1(targetMap);
        }
    };
    let list_8;
    list_8 = groupBy((co_3) => co_3.Label, labels, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    return collect((tupledArg) => {
        const lab = tupledArg[0];
        const lst = tupledArg[1];
        const dConn = getDriverConnection(lst);
        let list_7;
        let list_6;
        list_6 = filter((co_4) => (co_4.Id !== dConn.Target.HostId), lst);
        list_7 = indexed(list_6);
        return map_2((tupledArg_1) => {
            const i = tupledArg_1[0] | 0;
            const co_5 = tupledArg_1[1];
            return copyConnection(dConn, co_5, i);
        }, list_7);
    }, list_8);
}

function buildSimulationGraph(canvasState_0, canvasState_1) {
    const canvasState = [canvasState_0, canvasState_1];
    const connections$0027 = canvasState[1];
    const components = canvasState[0];
    const labConns = getLabelConnections(components, connections$0027);
    const connections = append(labConns, connections$0027);
    const sourceToTargetPort = buildSourceToTargetPortMap(connections);
    const portIdToPortNumber = mapInputPortIdToPortNumber(components);
    const mapper = (comp) => buildSimulationComponent(sourceToTargetPort, portIdToPortNumber, comp);
    let m;
    let elements;
    elements = map_2((comp_1) => [new ComponentId(0, comp_1.Id), mapper(comp_1)], components);
    m = ofList(elements);
    return m;
}

export function runCanvasStateChecksAndBuildGraph(canvasState_0, canvasState_1, loadedComponents) {
    const canvasState = [canvasState_0, canvasState_1];
    const matchValue = analyseState(canvasState[0], canvasState[1], loadedComponents);
    if (matchValue == null) {
        const arg0 = buildSimulationGraph(canvasState[0], canvasState[1]);
        return new FSharpResult$2(0, arg0);
    }
    else {
        const err = matchValue;
        return new FSharpResult$2(1, err);
    }
}

