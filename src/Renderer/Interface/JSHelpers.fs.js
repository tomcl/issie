import { join, toConsole, toFail, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { assertThat } from "../../Common/Helpers.fs.js";
import { contains as contains_1, empty, ofArray, map, ofSeq, singleton } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { rangeNumber } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { structuralHash, comparePrimitives, createAtom } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { contains, ofSeq as ofSeq_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Set.js";
import * as electron from "electron";
import { Union } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { union_type, class_type, obj_type, int32_type, bool_type, string_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { createSingleton as createSingleton_1 } from "tippy.js";
import tippy_1 from "tippy.js";
import * as material from "tippy.js/themes/material.css";
import { ofList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { isLetterOrDigit } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Char.js";
import { map as map_1 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import * as react from "react";

export function logString(msg) {
    let msg_1;
    const clo1 = toText(printf("%A"));
    msg_1 = clo1(msg);
    console.log(msg_1);
}

export function logChain(msg) {
    logString(msg);
    return msg;
}

export function assertNotNull(obj, msg) {
    let value;
    assertThat((value = ((obj == null || obj === 'undefined')), (!value)), "(assertNotNull) " + msg);
    return obj;
}

export function getFailIfNull(jsObj_mut, fields_mut) {
    getFailIfNull:
    while (true) {
        const jsObj = jsObj_mut, fields = fields_mut;
        const value = assertNotNull(jsObj, "jsObj is null in getFailIfNull");
        void value;
        if (fields.tail == null) {
            return toFail(printf("what? getFailIfNull called with no fields to get"));
        }
        else if (fields.tail.tail == null) {
            const lastField = fields.head;
            let msg;
            const clo1 = toText(printf("jsObj.%s is null in getFailIfNull"));
            msg = clo1(lastField);
            const obj = jsObj[lastField];
            return assertNotNull(obj, msg);
        }
        else {
            const fields$0027 = fields.tail;
            const nextField = fields.head;
            let jsObj$0027;
            let msg_1;
            const clo1_1 = toText(printf("jsObj.%s is null in getFailIfNull"));
            msg_1 = clo1_1(nextField);
            const obj_1 = jsObj[nextField];
            jsObj$0027 = assertNotNull(obj_1, msg_1);
            jsObj_mut = jsObj$0027;
            fields_mut = fields$0027;
            continue getFailIfNull;
        }
        break;
    }
}

export function jsListToFSharpList(jsList) {
    const len = getFailIfNull(jsList, singleton("length")) | 0;
    const list = ofSeq(rangeNumber(0, 1, len - 1));
    return map((i) => jsList[i], list);
}

export function fshaprListToJsList(list) {
    const jsList = [];
    let value;
    value = map((el) => (jsList.push(el)), list);
    void value;
    return jsList;
}

export function getTextEventValue(event) {
    const value = getFailIfNull(event.currentTarget, singleton("value"));
    return value;
}

export function getIntEventValue(event) {
    const value = getFailIfNull(event.currentTarget, singleton("value"));
    return value | 0;
}

export function getTextFocusEventValue(event) {
    const value = getFailIfNull(event, ofArray(["target", "value"]));
    return value;
}

export const debugLevel = createAtom(1);

export const debugTrace = createAtom(ofSeq_1([], {
    Compare: comparePrimitives,
}));

export function traceIf(traceCode, debugAction) {
    if (contains(traceCode, debugTrace())) {
        const format = debugAction();
        toConsole(format);
    }
}

export function setDebugLevel() {
    let argV;
    let list;
    let _arg1;
    const source = electron.remote.process.argv;
    _arg1 = ofSeq(source);
    if (_arg1.tail != null) {
        const args$0027 = _arg1.tail;
        list = args$0027;
    }
    else {
        list = empty();
    }
    argV = map((s) => s.toLocaleLowerCase(), list);
    const isArg = (s_1) => contains_1(s_1, argV, {
        Equals: (x, y) => (x === y),
        GetHashCode: structuralHash,
    });
    if (isArg("--debug") ? true : isArg("-d")) {
        debugLevel(2, true);
    }
    else if (isArg("-w")) {
        debugLevel(1, true);
    }
}

export function getColorString(col) {
    let clo1;
    return (clo1 = toText(printf("%A")), clo1(col)).toLocaleLowerCase();
}

export class TooltipsOpts extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Content", "Animation", "Arrow", "Theme", "Offset", "HideOnClick", "Placement", "Delay", "ZIndex", "Interactive", "Boundary", "PopperOptions", "MaxWidth", "AppendTo"];
    }
}

export function TooltipsOpts$reflection() {
    return union_type("JSHelpers.TooltipsOpts", [], TooltipsOpts, () => [[["Item", string_type]], [["Item", string_type]], [["Item", bool_type]], [["Item", string_type]], [["Item1", int32_type], ["Item2", int32_type]], [["Item", bool_type]], [["Item", string_type]], [["Item1", int32_type], ["Item2", int32_type]], [["Item", int32_type]], [["Item", bool_type]], [["Item", string_type]], [["Item", obj_type]], [["Item", int32_type]], [["Item", class_type("Browser.Types.HTMLElement")]]]);
}

export function tippyOptsWithContent(p, c) {
    const li = ofArray([new TooltipsOpts(7, 700, 100), new TooltipsOpts(6, p), new TooltipsOpts(1, "fade"), new TooltipsOpts(12, 250), new TooltipsOpts(2, true), new TooltipsOpts(3, "material"), new TooltipsOpts(9, true), new TooltipsOpts(13, document.body), new TooltipsOpts(5, true), new TooltipsOpts(0, c)]);
    return keyValueList(li, 1);
}

export function tippyOpts(p) {
    const li = ofArray([new TooltipsOpts(7, 700, 100), new TooltipsOpts(1, "fade"), new TooltipsOpts(6, p), new TooltipsOpts(12, 250), new TooltipsOpts(2, true), new TooltipsOpts(3, "material"), new TooltipsOpts(9, true), new TooltipsOpts(13, document.body), new TooltipsOpts(5, true)]);
    return keyValueList(li, 1);
}

export const tippy$0027 = tippy_1;

export const tippyDom = tippy_1;

export const createSingleton = createSingleton_1;


export function tippy(tippyOpts_1, rClass) {
    return tippy$0027(rClass, tippyOpts_1);
}

export function tippy1(rId, pos, mess) {
    return tippy(tippyOptsWithContent(pos, mess), "#" + rId);
}

export const tippyRecord = createAtom(ofList(empty()));

export function recordTippyInstance(prefix, tip) {
}

export function tipRef(prefix, pos, text, element, tip) {
    let strings, array_1, array;
    const ids = prefix + (strings = (array_1 = (array = (Array.from(text.split(""))), (array.filter(isLetterOrDigit))), (map_1((value) => value, array_1))), (join("", strings)));
    return react.createElement("div", {
        id: ids,
        ref: (element_1) => {
            if ((!((element_1 == null || element_1 === 'undefined'))) ? (!element_1.hasAttribute("data-tippy-content")) : false) {
                const tippyInst = tippy1(ids, pos, tip);
                recordTippyInstance(prefix, tippyInst);
            }
        },
    }, element);
}

export function tipStr(pos, text, tip) {
    return tipRef("Str_", pos, text, text, tip);
}

export const testCanvas = document.createElement("canvas");

export const canvasWidthContext = testCanvas.getContext('2d');

export function getTextWidthInPixels(txt, font) {
    canvasWidthContext.font = font;
    return canvasWidthContext.measureText(txt).width;
}

