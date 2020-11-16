import { map, length, ofArray } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { join, printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { int32ToString } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";

export const VERSION = ofArray([1, 0, 7]);

export const VersionString = ((length(VERSION) !== 3) ? (() => {
    const clo1 = toFail(printf("Badly formatted version %A (VERSION must be list of 3 integers)"));
    clo1(VERSION);
})() : (void 0), (() => {
    let s;
    let strings;
    strings = map(int32ToString, VERSION);
    s = join(".", strings);
    return "v" + s;
})());

