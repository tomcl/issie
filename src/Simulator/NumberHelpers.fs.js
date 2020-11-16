import { toText, format, replicate, printf, toFail } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { cons, singleton, replicate as replicate_1, append, truncate, length, ofSeq } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { parse, op_Addition, op_LeftShift, op_Subtraction, op_BitwiseAnd, fromBits, compare, op_Division, op_Modulus, toInt, fromInteger, fromValue, toString } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { Bit } from "./SimulatorTypes.fs.js";
import { pow2int64 } from "../Common/Helpers.fs.js";
import { Result_Bind, FSharpResult$2 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";

function hexToBin(hStr) {
    const convert = (h) => {
        if (h.tail != null) {
            const h$0027 = h.tail;
            const c = h.head;
            let digit;
            switch (c) {
                case "0": {
                    digit = "0000";
                    break;
                }
                case "1": {
                    digit = "0001";
                    break;
                }
                case "2": {
                    digit = "0010";
                    break;
                }
                case "3": {
                    digit = "0011";
                    break;
                }
                case "4": {
                    digit = "0100";
                    break;
                }
                case "5": {
                    digit = "0101";
                    break;
                }
                case "6": {
                    digit = "0110";
                    break;
                }
                case "7": {
                    digit = "0111";
                    break;
                }
                case "8": {
                    digit = "1000";
                    break;
                }
                case "9": {
                    digit = "1001";
                    break;
                }
                case "a": {
                    digit = "1010";
                    break;
                }
                case "b": {
                    digit = "1011";
                    break;
                }
                case "c": {
                    digit = "1100";
                    break;
                }
                case "d": {
                    digit = "1101";
                    break;
                }
                case "e": {
                    digit = "1110";
                    break;
                }
                case "f": {
                    digit = "1111";
                    break;
                }
                default: {
                    const c_1 = c;
                    const clo1 = toFail(printf("Invalid char %c while converting hex %s to binary"));
                    const clo2 = clo1(c_1);
                    digit = clo2(hStr);
                }
            }
            return digit + convert(h$0027);
        }
        else {
            return "";
        }
    };
    let chars;
    const source = hStr.toLocaleLowerCase();
    chars = ofSeq(source.split(""));
    if (chars.tail != null) {
        const chars$0027 = chars.tail;
        const c_2 = chars.head;
        let firstDigit;
        switch (c_2) {
            case "0": {
                firstDigit = "0";
                break;
            }
            case "1": {
                firstDigit = "1";
                break;
            }
            case "2": {
                firstDigit = "10";
                break;
            }
            case "3": {
                firstDigit = "11";
                break;
            }
            case "4": {
                firstDigit = "100";
                break;
            }
            case "5": {
                firstDigit = "101";
                break;
            }
            case "6": {
                firstDigit = "110";
                break;
            }
            case "7": {
                firstDigit = "111";
                break;
            }
            case "8": {
                firstDigit = "1000";
                break;
            }
            case "9": {
                firstDigit = "1001";
                break;
            }
            case "a": {
                firstDigit = "1010";
                break;
            }
            case "b": {
                firstDigit = "1011";
                break;
            }
            case "c": {
                firstDigit = "1100";
                break;
            }
            case "d": {
                firstDigit = "1101";
                break;
            }
            case "e": {
                firstDigit = "1110";
                break;
            }
            case "f": {
                firstDigit = "1111";
                break;
            }
            default: {
                const c_3 = c_2;
                const clo1_1 = toFail(printf("Invalid char %c while converting hex %s to binary"));
                const clo2_1 = clo1_1(c_3);
                firstDigit = clo2_1(hStr);
            }
        }
        return firstDigit + convert(chars$0027);
    }
    else {
        return "";
    }
}

export function addZeros64(width, pFun, n) {
    const s = pFun(n);
    let bits;
    const matchValue = s[1];
    switch (matchValue) {
        case "b": {
            bits = 1;
            break;
        }
        case "x": {
            bits = 4;
            break;
        }
        default: {
            const clo1 = toFail(printf("Wrong use of addZeros64: s = %s"));
            bits = clo1(s);
        }
    }
    const extra = (~(~((width - ((s.length - 2) * bits)) / bits))) | 0;
    return (s.slice(0, 1 + 1) + replicate(extra, "0")) + s.slice(2, s.length);
}

export function addZeros(width, pFun, n) {
    const s = pFun(n);
    let bits;
    const matchValue = s[1];
    switch (matchValue) {
        case "b": {
            bits = 1;
            break;
        }
        case "x": {
            bits = 4;
            break;
        }
        default: {
            const clo1 = toFail(printf("Wrong use of addZeros: s = %s"));
            bits = clo1(s);
        }
    }
    const extra = (~(~((((width - (s.length - 2)) * bits) + (2 << (bits - 1))) / bits))) | 0;
    return (s.slice(0, 1 + 1) + replicate(extra, "0")) + s.slice(2, s.length);
}

export function hex64(num) {
    return "0x" + format('{0:' + "X" + '}', num);
}

export function fillHex64(width) {
    return (n) => addZeros64(width, hex64, n);
}

export function bin64(num) {
    let hStr;
    return "0b" + (hStr = format('{0:' + "X" + '}', num), (hexToBin(hStr)));
}

export function sDec64(num) {
    return toString(num);
}

export function dec64(num) {
    let copyOfStruct = fromValue(num, true);
    return toString(copyOfStruct);
}

export function hex(num) {
    const num_1 = fromInteger(num, false, 2);
    return hex64(num_1);
}

export function fillHex(width) {
    return (n) => addZeros(width, hex, n);
}

export function bin(num) {
    const num_1 = fromInteger(num, false, 2);
    return bin64(num_1);
}

export function dec(num) {
    const num_1 = fromInteger(num, false, 2);
    return dec64(num_1);
}

export function fillBin64(width) {
    return (n) => addZeros64(width, bin64, n);
}

export function fillBin(width) {
    return (n) => addZeros(width, bin, n);
}

export function bitToString(bit) {
    if (bit.tag === 1) {
        return "1";
    }
    else {
        return "0";
    }
}

function padToWidth(width, bits) {
    if (length(bits) > width) {
        return truncate(width, bits);
    }
    else {
        return append(bits, replicate_1(width - length(bits), new Bit(0)));
    }
}

export function convertIntToWireData(width, num) {
    const toBit = (_arg1) => {
        switch (_arg1) {
            case 0: {
                return new Bit(0);
            }
            case 1: {
                return new Bit(1);
            }
            default: {
                throw (new Error("toBit only accepts 0 or 1"));
            }
        }
    };
    const intToBinary = (i) => {
        const matchValue = (~(~toInt(i))) | 0;
        switch (matchValue) {
            case 0:
            case 1: {
                return singleton(toBit(~(~toInt(i))));
            }
            default: {
                const bit = toBit(~(~toInt(op_Modulus(i, fromInteger(2, false, 2)))));
                return cons(bit, intToBinary(op_Division(i, fromInteger(2, false, 2))));
            }
        }
    };
    if (compare(num, fromBits(0, 0, false)) >= 0) {
        return padToWidth(width, intToBinary(num));
    }
    else {
        return padToWidth(width, intToBinary(op_BitwiseAnd(num, op_Subtraction(op_LeftShift(fromBits(1, 0, false), width), fromBits(1, 0, false)))));
    }
}

export function convertWireDataToInt(bits) {
    const convert = (bits_1_mut, idx_mut) => {
        convert:
        while (true) {
            const bits_1 = bits_1_mut, idx = idx_mut;
            if (bits_1.tail != null) {
                if (bits_1.head.tag === 1) {
                    const bits$0027_1 = bits_1.tail;
                    return op_Addition(pow2int64(idx), convert(bits$0027_1, idx + 1));
                }
                else {
                    const bits$0027 = bits_1.tail;
                    bits_1_mut = bits$0027;
                    idx_mut = (idx + 1);
                    continue convert;
                }
            }
            else {
                return fromInteger(0, false, 2);
            }
            break;
        }
    };
    return convert(bits, 0);
}

export function strToInt(str) {
    try {
        const arg0 = parse(str, 511, false, 64);
        return new FSharpResult$2(0, arg0);
    }
    catch (matchValue) {
        return new FSharpResult$2(1, "Invalid number.");
    }
}

function countBits(num) {
    let str;
    return (str = bin64(num), (str.length)) - 2;
}

function checkWidth(width, num) {
    const bitsCount = countBits(num) | 0;
    const matchValue = bitsCount <= width;
    if (matchValue) {
        return void 0;
    }
    else {
        let arg0;
        const clo1 = toText(printf("Too many bits. Expected up to %d but got %d."));
        const clo2 = clo1(width);
        arg0 = clo2(bitsCount);
        return arg0;
    }
}

export function strToIntCheckWidth(str, width) {
    const result = strToInt(str);
    return Result_Bind((num) => {
        const matchValue = checkWidth(width, num);
        if (matchValue != null) {
            const err = matchValue;
            return new FSharpResult$2(1, err);
        }
        else {
            return new FSharpResult$2(0, num);
        }
    }, result);
}

