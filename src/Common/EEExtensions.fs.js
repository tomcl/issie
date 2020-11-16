import { value as value_1, some } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { fold, singleton as singleton_1, sortBy, groupBy, ofSeq, reverse, map, tail, head, cons, empty } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { trimEnd, trimStart, trim, substring, startsWith, replace, endsWith, compare, join, split, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { create, match } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/RegExp.js";
import { ofArray, singleton, empty as empty_1, collect, append, rangeNumber, map as map_1, delay } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { uncurry, comparePrimitives, structuralHash } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { map as map_2, groupBy as groupBy_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { toArray, FSharpMap__TryFind } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";

export function FableReplacements_optionMap2(f, v1, v2) {
    const matchValue = [v1, v2];
    let pattern_matching_result, v1_1, v2_1;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 0;
            v1_1 = value_1(matchValue[0]);
            v2_1 = value_1(matchValue[1]);
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
            return some(f(v1_1, v2_1));
        }
        case 1: {
            return void 0;
        }
    }
}

export function FableReplacements_listChunkBySize(chunkSize, l) {
    const listChunkBySize$0027 = (state_mut, chunksLeft_mut, itemsRemaining_mut) => {
        listChunkBySize$0027:
        while (true) {
            const state = state_mut, chunksLeft = chunksLeft_mut, itemsRemaining = itemsRemaining_mut;
            const matchValue = [chunksLeft, itemsRemaining];
            if (matchValue[1].tail != null) {
                if (matchValue[0] === 0) {
                    state_mut = cons(empty(), state);
                    chunksLeft_mut = chunkSize;
                    itemsRemaining_mut = itemsRemaining;
                    continue listChunkBySize$0027;
                }
                else {
                    const itemsTail = matchValue[1].tail;
                    const nextItem = matchValue[1].head;
                    state_mut = cons(cons(nextItem, head(state)), tail(state));
                    chunksLeft_mut = (chunksLeft - 1);
                    itemsRemaining_mut = itemsTail;
                    continue listChunkBySize$0027;
                }
            }
            else {
                return state;
            }
            break;
        }
    };
    if (l.tail == null) {
        return empty();
    }
    else {
        let list_2;
        const list_1 = listChunkBySize$0027(empty(), 0, l);
        list_2 = map(reverse, list_1);
        return reverse(list_2);
    }
}

export function FableReplacements_hexToString(x) {
    const loop = (str_mut, _arg1_mut) => {
        let arg10, clo1;
        loop:
        while (true) {
            const str = str_mut, _arg1 = _arg1_mut;
            if (_arg1 === 0) {
                return str;
            }
            else {
                const num = _arg1;
                str_mut = ((arg10 = (num % 16), (clo1 = toText(printf("%X")), clo1(arg10))) + str);
                _arg1_mut = (~(~(num / 16)));
                continue loop;
            }
            break;
        }
    };
    if (x === 0) {
        return "0";
    }
    else {
        return loop("", x);
    }
}

export function StringModule_SplitOnWhitespace(text) {
    return split(text, [], null, 1);
}

export function StringModule_Concat(sep, strings) {
    return join(sep, strings);
}

export function StringModule_Length(str) {
    let str_2;
    const str_1 = str;
    str_2 = ((str_1 === null) ? "" : str_1);
    return str_2.length | 0;
}

export function StringModule_Contains(value, str) {
    return str.indexOf(value) >= 0;
}

export function StringModule_Compare(strB, strA) {
    return compare(strA, strB, 4);
}

export function StringModule_EndsWith(value, str) {
    return endsWith(str, value, 4);
}

export function StringModule_Equals(comparisonType, value, str) {
    return compare(str, value, comparisonType) === 0;
}

export function StringModule_ReplaceChar(oldChar, newChar, str) {
    return replace(str, oldChar, newChar);
}

export function StringModule_Replace(oldValue, newValue, str) {
    return replace(str, oldValue, newValue);
}

export function StringModule_Split(separator, str) {
    return split(str, separator, null, 0);
}

export function StringModule_SplitRemoveEmptyEntries(separator, str) {
    return split(str, separator, null, 1);
}

export function StringModule_SplitString(separator, str) {
    return split(str, separator, null, 0);
}

export function StringModule_SplitStringRemoveEmptyEntries(separator, str) {
    return split(str, separator, null, 1);
}

export function StringModule_StartsWith(value, str) {
    return startsWith(str, value, 4);
}

export function StringModule_SubstringLength(startIndex, length, str) {
    return substring(str, startIndex, length);
}

export function StringModule_Substring(startIndex, str) {
    return substring(str, startIndex);
}

export function StringModule_ToLower(str) {
    return str.toLowerCase();
}

export function StringModule_ToUpper(str) {
    return str.toUpperCase();
}

export function StringModule_Trim(str) {
    return str.trim();
}

export function StringModule_TrimChars(trimChars, str) {
    return trim(str, ...trimChars);
}

export function StringModule_TrimStart(trimChars, str) {
    return trimStart(str, ...trimChars);
}

export function StringModule_TrimEnd(trimChars, str) {
    return trimEnd(str, ...trimChars);
}

export function StringModule_RegexMatchGroups(regex, str) {
    const m = match(str, regex);
    if (m != null) {
        return ofSeq(delay(() => map_1((n) => (m[n] || ""), ofSeq(rangeNumber(1, 1, m.length)))));
    }
    else {
        return void 0;
    }
}

export function StringModule_RegexMatch(regex, str) {
    const m = match(create(regex), str);
    if (m != null) {
        return m[0];
    }
    else {
        return void 0;
    }
}

export function StringModule_TryParseWith(tryParseFunc) {
    return (arg) => {
        const _arg1 = tryParseFunc(arg);
        if (_arg1[0]) {
            const v = _arg1[1];
            return some(v);
        }
        else {
            return void 0;
        }
    };
}

export function ListModule_ToString(chars) {
    let arg00;
    arg00 = chars;
    return join("", arg00);
}

export function ListModule_ChunkAt1(pred, lst) {
    let i = 0;
    let list_3;
    let list_1;
    const list = ofSeq(delay(() => append(collect((el) => append(pred(el) ? (i = ((i + 1) | 0), empty_1()) : empty_1(), delay(() => singleton([i, el]))), lst), delay(() => []))));
    list_1 = groupBy((tuple) => tuple[0], list, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    list_3 = sortBy((tuple_1) => tuple_1[0], list_1, {
        Compare: comparePrimitives,
    });
    return map((arg) => {
        let list_2;
        list_2 = arg[1];
        return map((tuple_3) => tuple_3[1], list_2);
    }, list_3);
}

export function ListModule_ChunkAt(pred, list) {
    const loop = (chunk_mut, chunks_mut, list_1_mut) => {
        let xs, x, xs_2, x_2;
        loop:
        while (true) {
            const chunk = chunk_mut, chunks = chunks_mut, list_1 = list_1_mut;
            if (list_1.tail != null) {
                if (xs = list_1.tail, (x = list_1.head, pred(x) ? (chunk.tail == null) : false)) {
                    const x_1 = list_1.head;
                    const xs_1 = list_1.tail;
                    chunk_mut = singleton_1(x_1);
                    chunks_mut = chunks;
                    list_1_mut = xs_1;
                    continue loop;
                }
                else {
                    let pattern_matching_result, x_3, xs_3;
                    if (list_1.tail != null) {
                        if (xs_2 = list_1.tail, (x_2 = list_1.head, pred(x_2))) {
                            pattern_matching_result = 0;
                            x_3 = list_1.head;
                            xs_3 = list_1.tail;
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
                            chunk_mut = singleton_1(x_3);
                            chunks_mut = cons(reverse(chunk), chunks);
                            list_1_mut = xs_3;
                            continue loop;
                        }
                        case 1: {
                            if (list_1.tail != null) {
                                const xs_4 = list_1.tail;
                                const x_4 = list_1.head;
                                chunk_mut = cons(x_4, chunk);
                                chunks_mut = chunks;
                                list_1_mut = xs_4;
                                continue loop;
                            }
                            else {
                                throw (new Error("The match cases were incomplete"));
                            }
                        }
                    }
                }
            }
            else {
                return reverse(cons(reverse(chunk), chunks));
            }
            break;
        }
    };
    return loop(empty(), empty(), list);
}

export function ListModule_OkList(lst) {
    return ofSeq(delay(() => collect((x) => {
        if (x.tag === 0) {
            const y = x.fields[0];
            return singleton(y);
        }
        else {
            return [];
        }
    }, lst)));
}

export function ListModule_ErrorList(lst) {
    return ofSeq(delay(() => collect((x) => {
        if (x.tag === 1) {
            const y = x.fields[0];
            return singleton(y);
        }
        else {
            return [];
        }
    }, lst)));
}

export function ListModule_SplitResult(resL) {
    return fold(uncurry(2, (tupledArg) => {
        const rl = tupledArg[0];
        const el = tupledArg[1];
        return (_arg1) => {
            if (_arg1.tag === 0) {
                const r = _arg1.fields[0];
                return [cons(r, rl), el];
            }
            else {
                const e = _arg1.fields[0];
                return [rl, cons(e, el)];
            }
        };
    }), [empty(), empty()], resL);
}

export function ArrayModule_ToString(chars) {
    let arg00;
    arg00 = ofArray(chars);
    return join("", arg00);
}

export function ArrayModule_ChunkAt(pred, arr) {
    let i = 0;
    let array_2;
    const array = Array.from(delay(() => collect((x) => append(pred(x) ? (i = ((i + 1) | 0), empty_1()) : empty_1(), delay(() => singleton([i, x]))), arr)));
    array_2 = groupBy_1((tuple) => tuple[0], array, {
        Equals: (x_1, y) => (x_1 === y),
        GetHashCode: structuralHash,
    });
    return map_2((arg) => {
        let array_1;
        array_1 = arg[1];
        return map_2((tuple_2) => tuple_2[1], array_1);
    }, array_2);
}

export function MapModule_FindWithDefault(key, table, defaultValue) {
    const matchValue = FSharpMap__TryFind(table, key);
    if (matchValue == null) {
        return defaultValue;
    }
    else {
        const v = value_1(matchValue);
        return v;
    }
}

export function MapModule_Values(table) {
    let array;
    array = toArray(table);
    return map_2((tuple) => tuple[1], array);
}

export function MapModule_Keys(table) {
    let array;
    array = toArray(table);
    return map_2((tuple) => tuple[0], array);
}

