import { Prop, DOMAttr, HTMLAttr, CSSProp } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { map, ofSeq, cons, append, empty, singleton, ofArray } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { partialApply, min, equalsSafe, int32ToString } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { op_UnaryNegation_Int32 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Int32.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { MemoryEditorData, Msg } from "./ModelType.fs.js";
import { showMemoryEditorPopup, errorNotification } from "./PopupView.fs.js";
import { fromInt, op_Addition, op_Subtraction, fromBits, op_LeftShift, compare, fromValue, fromInteger, equals } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { strToIntCheckWidth, strToInt, fillHex64, fillBin64, hex64, sDec64, bin64, dec64 } from "../../Simulator/NumberHelpers.fs.js";
import { right, left, Level_Option, level, Item_Option, item } from "../.fable/Fulma.2.9.0/Layouts/Level.fs.js";
import { Option, div } from "../.fable/Fulma.2.9.0/Elements/Form/Field.fs.js";
import { div as div_1 } from "../.fable/Fulma.2.9.0/Elements/Form/Control.fs.js";
import { Option as Option_1, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { NumberBase } from "../../Common/CommonTypes.fs.js";
import { Common_GenericOption, Color_IColor } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { assertThat, pow2int64 } from "../../Common/Helpers.fs.js";
import { printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import * as react from "react";
import { IInputType, input, Option as Option_2 } from "../.fable/Fulma.2.9.0/Elements/Form/Input.fs.js";
import { getTextEventValue } from "../Interface/JSHelpers.fs.js";
import { input as input_1, checkbox } from "../.fable/Fulma.2.9.0/Elements/Form/Checkbox.fs.js";
import { Draw2dWrapper__WriteMemoryLine, Draw2dWrapper__GetComponentById_Z721C83C5 } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { Result_Map } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { extractComponentType, extractComponent } from "../Interface/Extractor.fs.js";
import { toList, filter, add, tryFind } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { defaultArg } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { rangeLong, singleton as singleton_1, delay } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { toString } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { TableOption, table as table_3 } from "../.fable/Fulma.2.9.0/Elements/Table.fs.js";

const popupExtraStyle = ofArray([new CSSProp(395, "65%"), new CSSProp(189, "80%")]);

const headerHeight = 60;

const headerStyle = (() => {
    const css = ofArray([new CSSProp(291, "fixed"), new CSSProp(226, int32ToString(op_UnaryNegation_Int32(headerHeight) - 20) + "px"), new CSSProp(281, "20px"), new CSSProp(276, "60px"), new CSSProp(21, "white"), new CSSProp(395, "61%"), new CSSProp(189, headerHeight), new CSSProp(404, 32)]);
    return ["style", keyValueList(css, 1)];
})();

const bodyStyle = (() => {
    const css = singleton(new CSSProp(226, int32ToString(headerHeight) + "px"));
    return ["style", keyValueList(css, 1)];
})();

function showError(msg, dispatch) {
    dispatch((new Msg(33, (dispatch_1) => errorNotification(msg, new Msg(34), dispatch_1))));
}

function closeError(dispatch) {
    dispatch(new Msg(34));
}

function showRowWithAdrr(memoryEditorData, addr) {
    let a;
    const matchValue = memoryEditorData.Address;
    if (matchValue != null) {
        if (a = matchValue, equals(a, addr)) {
            const a_1 = matchValue;
            return true;
        }
        else {
            return false;
        }
    }
    else {
        return true;
    }
}

export function viewNum(numBase) {
    switch (numBase.tag) {
        case 1: {
            return dec64;
        }
        case 2: {
            return bin64;
        }
        case 3: {
            return sDec64;
        }
        default: {
            return hex64;
        }
    }
}

export function viewFilledNum(width, numBase) {
    switch (numBase.tag) {
        case 1: {
            return dec64;
        }
        case 2: {
            return fillBin64(width);
        }
        case 3: {
            return sDec64;
        }
        default: {
            return fillHex64(width);
        }
    }
}

export function baseSelector(numBase, changeBase_1) {
    return item(singleton(new Item_Option(1)), singleton(div(singleton(new Option(1)), ofArray([div_1(empty(), singleton(button(ofArray([new Option_1(0, equalsSafe(numBase, new NumberBase(0)) ? (new Color_IColor(4)) : (new Color_IColor(20))), new Option_1(17, (_arg1) => {
        changeBase_1(new NumberBase(0));
    })]), singleton("hex")))), div_1(empty(), singleton(button(ofArray([new Option_1(0, equalsSafe(numBase, new NumberBase(1)) ? (new Color_IColor(4)) : (new Color_IColor(20))), new Option_1(17, (_arg2) => {
        changeBase_1(new NumberBase(1));
    })]), singleton("dec")))), div_1(empty(), singleton(button(ofArray([new Option_1(0, equalsSafe(numBase, new NumberBase(2)) ? (new Color_IColor(4)) : (new Color_IColor(20))), new Option_1(17, (_arg3) => {
        changeBase_1(new NumberBase(2));
    })]), singleton("bin"))))]))));
}

export function changeBase(memoryEditorData, dispatch, numBase) {
    let arg0_1, arg0;
    return dispatch((arg0_1 = (arg0 = (new MemoryEditorData(memoryEditorData.OnlyDiff, memoryEditorData.Address, memoryEditorData.Start, numBase)), (arg0)), (new Msg(25, arg0_1))));
}

function makeEditorHeader(memory, isDiff, memoryEditorData, dispatch) {
    let s, arg10, clo1, s_2, clo1_1, options, arg0;
    const children = [level(empty(), append(ofArray([item(singleton(new Item_Option(1)), ofArray([(s = (arg10 = pow2int64(memory.AddressWidth), (clo1 = toText(printf("Number of elements: %d")), clo1(arg10))), (s)), react.createElement("br", {}), (s_2 = (clo1_1 = toText(printf("Word width: %d bit(s)")), clo1_1(memory.WordWidth)), (s_2))])), item(singleton(new Item_Option(1)), ofArray([("First Location Displayed"), (options = ofArray([new Option_2(15, singleton(["style", {
        marginLeft: "10px",
        width: "80px",
    }])), new Option_2(10, ""), (arg0 = viewNum(memoryEditorData.NumberBase)(fromInteger(0, false, 2)), (new Option_2(12, arg0))), new Option_2(13, (arg) => {
        let arg0_2, arg0_1, arg0_4, arg0_3, Address_1;
        let text;
        text = getTextEventValue(arg);
        if (text === "") {
            closeError(dispatch);
            dispatch((arg0_2 = (arg0_1 = (new MemoryEditorData(memoryEditorData.OnlyDiff, void 0, memoryEditorData.Start, memoryEditorData.NumberBase)), (arg0_1)), (new Msg(25, arg0_2))));
        }
        else {
            const t = text;
            const matchValue = strToInt(t);
            if (matchValue.tag === 0) {
                const addr = matchValue.fields[0];
                const addr_1 = fromValue(addr, true);
                const w = memory.AddressWidth | 0;
                if ((w < 64) ? (compare(addr_1, op_LeftShift(fromBits(1, 0, true), w)) >= 0) : false) {
                    showError("Address out of bounds.", dispatch);
                }
                else {
                    closeError(dispatch);
                    dispatch((arg0_4 = (arg0_3 = (Address_1 = fromValue(addr_1, false), new MemoryEditorData(memoryEditorData.OnlyDiff, Address_1, memoryEditorData.Start, memoryEditorData.NumberBase)), (arg0_3)), (new Msg(25, arg0_4))));
                }
            }
            else {
                const err = matchValue.fields[0];
                showError(err, dispatch);
            }
        }
    })]), input(cons(new Option_2(1, new IInputType(0)), options)))])), baseSelector(memoryEditorData.NumberBase, (numBase) => {
        changeBase(memoryEditorData, dispatch, numBase);
    })]), isDiff ? singleton(item(singleton(new Item_Option(1)), singleton(checkbox(empty(), ofArray([input_1(singleton(new Common_GenericOption(1, ofArray([["style", {
        marginRight: "5px",
    }], new HTMLAttr(62, memoryEditorData.OnlyDiff), new DOMAttr(9, (_arg1) => {
        let arg0_6, arg0_5;
        dispatch((arg0_6 = (arg0_5 = (new MemoryEditorData(!memoryEditorData.OnlyDiff, memoryEditorData.Address, memoryEditorData.Start, memoryEditorData.NumberBase)), (arg0_5)), (new Msg(25, arg0_6))));
    })])))), "Show only if changed"]))))) : empty()))];
    return react.createElement("div", keyValueList([headerStyle, new HTMLAttr(148, false)], 1), ...children);
}

function makeEditorBody(memory, compId, memoryEditorData, model, dispatch) {
    let children_12, children_10, children_14, list, mapping;
    const showRow = (addr) => showRowWithAdrr(memoryEditorData, addr);
    const viewNumD = viewFilledNum(memory.WordWidth, memoryEditorData.NumberBase);
    const viewNumA = viewFilledNum(memory.AddressWidth, memoryEditorData.NumberBase);
    const numLocsToDisplay = fromBits(16, 0, true);
    const maxLocAddr = op_Subtraction(op_LeftShift(fromBits(1, 0, true), memory.AddressWidth), fromBits(1, 0, true));
    let patternInput;
    const matchValue = memoryEditorData.Address;
    if (matchValue != null) {
        const a = matchValue;
        const a_1 = fromValue(a, true);
        const maxDispLocWrapped = op_Subtraction(op_Addition(a_1, numLocsToDisplay), fromBits(1, 0, true));
        const maxDispLoc = (compare(maxDispLocWrapped, a_1) > 0) ? maxDispLocWrapped : fromValue(fromBits(4294967295, 4294967295, false), true);
        patternInput = [a_1, min(compare, maxDispLoc, maxLocAddr)];
    }
    else {
        patternInput = [fromBits(0, 0, true), min(compare, maxLocAddr, numLocsToDisplay)];
    }
    const startLoc = patternInput[0];
    const endLoc = patternInput[1];
    let dynamicMem;
    let matchValue_1;
    const result = Draw2dWrapper__GetComponentById_Z721C83C5(model.Diagram, compId);
    matchValue_1 = Result_Map(extractComponent, result);
    let pattern_matching_result, mem;
    if (matchValue_1.tag === 0) {
        if (matchValue_1.fields[0].Type.tag === 25) {
            pattern_matching_result = 0;
            mem = matchValue_1.fields[0].Type.fields[0];
        }
        else if (matchValue_1.fields[0].Type.tag === 24) {
            pattern_matching_result = 0;
            mem = matchValue_1.fields[0].Type.fields[0];
        }
        else if (matchValue_1.fields[0].Type.tag === 23) {
            pattern_matching_result = 0;
            mem = matchValue_1.fields[0].Type.fields[0];
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
            dynamicMem = mem;
            break;
        }
        case 1: {
            dynamicMem = memory;
            break;
        }
    }
    const memory_1 = dynamicMem;
    const makeRow = (memData, addr_1) => {
        let css, children, s, children_2;
        const addr_2 = fromValue(addr_1, false);
        let content;
        const option = tryFind(fromValue(addr_2, false), memData);
        const value = fromBits(0, 0, false);
        content = defaultArg(option, value);
        const props_4 = [new HTMLAttr(148, false), (css = singleton(new CSSProp(125, "table-row")), ["style", keyValueList(css, 1)])];
        const children_4 = [(children = [(s = viewNumA(fromValue(addr_2, false)), (s))], react.createElement("td", {}, ...children)), (children_2 = ofSeq(delay(() => {
            let options, arg0, arg0_1;
            const handleInput = (ev) => {
                const text = getTextEventValue(ev);
                const matchValue_2 = strToIntCheckWidth(text, memory_1.WordWidth);
                if (matchValue_2.tag === 1) {
                    const err = matchValue_2.fields[0];
                    showError(err, dispatch);
                }
                else {
                    const value_1 = matchValue_2.fields[0];
                    closeError(dispatch);
                    let oldData;
                    let _arg1;
                    const result_1 = Draw2dWrapper__GetComponentById_Z721C83C5(model.Diagram, compId);
                    _arg1 = Result_Map(extractComponentType, result_1);
                    let pattern_matching_result_1, d;
                    if (_arg1.tag === 0) {
                        if (_arg1.fields[0].tag === 25) {
                            pattern_matching_result_1 = 0;
                            d = _arg1.fields[0].fields[0];
                        }
                        else if (_arg1.fields[0].tag === 24) {
                            pattern_matching_result_1 = 0;
                            d = _arg1.fields[0].fields[0];
                        }
                        else if (_arg1.fields[0].tag === 23) {
                            pattern_matching_result_1 = 0;
                            d = _arg1.fields[0].fields[0];
                        }
                        else {
                            pattern_matching_result_1 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_1 = 1;
                    }
                    switch (pattern_matching_result_1) {
                        case 0: {
                            oldData = d;
                            break;
                        }
                        case 1: {
                            oldData = memory_1;
                            break;
                        }
                    }
                    let arg10;
                    let table_2;
                    let table_1;
                    table_1 = add(addr_2, value_1, oldData.Data);
                    table_2 = filter((k, v) => (!equals(v, fromBits(0, 0, false))), table_1);
                    arg10 = toList(table_2);
                    Draw2dWrapper__WriteMemoryLine(model.Diagram, compId, arg10);
                    dispatch(new Msg(38, model.LastUsedDialogWidth));
                }
            };
            return singleton_1((options = ofArray([new Option_2(15, ofArray([new DOMAttr(8, handleInput), (arg0 = (toString(memoryEditorData.NumberBase) + toString([addr_2, content])), (new Prop(0, arg0)))])), (arg0_1 = viewNumD(content), (new Option_2(10, arg0_1))), (new Option_2(13, (arg) => {
                let text_1;
                text_1 = getTextEventValue(arg);
                const matchValue_3 = strToIntCheckWidth(text_1, memory_1.WordWidth);
                if (matchValue_3.tag === 0) {
                    closeError(dispatch);
                }
                else {
                    const err_1 = matchValue_3.fields[0];
                    showError(err_1, dispatch);
                }
            }))]), input(cons(new Option_2(1, new IInputType(0)), options))));
        })), react.createElement("td", {}, ...children_2))];
        return react.createElement("tr", keyValueList(props_4, 1), ...children_4);
    };
    const children_16 = [table_3(singleton(new TableOption(2)), ofArray([(children_12 = [(children_10 = [react.createElement("th", {}, "Address"), react.createElement("th", {}, "Content")], react.createElement("tr", {}, ...children_10))], react.createElement("thead", {}, ...children_12)), (children_14 = (list = ofSeq(rangeLong(startLoc, fromInt(1), endLoc, true)), (mapping = partialApply(1, makeRow, [memory_1.Data]), map(mapping, list))), react.createElement("tbody", {}, ...children_14))]))];
    return react.createElement("div", keyValueList([bodyStyle], 1), ...children_16);
}

function makeFoot(isDiffMode, dispatch, model) {
    const action = (_arg1) => {
        dispatch(new Msg(34));
        dispatch(new Msg(20));
        if (!isDiffMode) {
            dispatch(new Msg(38, model.LastUsedDialogWidth));
        }
    };
    return level(singleton(new Level_Option(0, singleton(["style", {
        width: "100%",
    }]))), ofArray([left(empty(), empty()), right(empty(), singleton(item(empty(), singleton(button(ofArray([new Option_1(0, new Color_IColor(4)), new Option_1(17, action)]), singleton("Done"))))))]));
}

function makeEditor(memory, compId, model, dispatch) {
    let dynamicMem;
    const matchValue = model.SelectedComponent;
    let pattern_matching_result, mem;
    if (matchValue != null) {
        if (matchValue.Type.tag === 25) {
            pattern_matching_result = 0;
            mem = matchValue.Type.fields[0];
        }
        else if (matchValue.Type.tag === 24) {
            pattern_matching_result = 0;
            mem = matchValue.Type.fields[0];
        }
        else if (matchValue.Type.tag === 23) {
            pattern_matching_result = 0;
            mem = matchValue.Type.fields[0];
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
            dynamicMem = mem;
            break;
        }
        case 1: {
            dynamicMem = memory;
            break;
        }
    }
    return (memoryEditorData) => {
        const children = [makeEditorHeader(dynamicMem, false, memoryEditorData, dispatch), makeEditorBody(dynamicMem, compId, memoryEditorData, model, dispatch)];
        return react.createElement("div", {}, ...children);
    };
}

export function openMemoryEditor(memory, compId, model, dispatch) {
    const title = "Memory editor";
    const body = makeEditor(memory, compId, model, dispatch);
    const foot = makeFoot(false, dispatch, model);
    showMemoryEditorPopup(title, body, foot, popupExtraStyle, dispatch);
}

function makeDiffViewerBody(memory1, memory2, memoryEditorData) {
    let children_16, children_14, children_18, list, mapping;
    const getData = (addr, memData) => {
        const option = tryFind(addr, memData);
        const value = fromBits(0, 0, false);
        return defaultArg(option, value);
    };
    const viewNum_1 = viewNum(memoryEditorData.NumberBase);
    const makeRow = (content1, content2, addr_1) => {
        let css, children, s, props_2, css_1, children_2, s_2, props_4, css_2, children_4, s_4;
        const hasChanged = !equals(content1, content2);
        const showRow = (addr_2) => {
            if (showRowWithAdrr(memoryEditorData, addr_2)) {
                if (!memoryEditorData.OnlyDiff) {
                    return true;
                }
                else if (memoryEditorData.OnlyDiff) {
                    return hasChanged;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        };
        const props_6 = [(css = singleton(new CSSProp(125, showRow(addr_1) ? "table-row" : "none")), ["style", keyValueList(css, 1)])];
        const children_6 = [(children = [(s = viewNum_1(fromValue(addr_1, false)), (s))], react.createElement("td", {}, ...children)), (props_2 = [(css_1 = singleton(new CSSProp(21, hasChanged ? "#ffc6d3" : "auto")), ["style", keyValueList(css_1, 1)])], (children_2 = [(s_2 = viewNum_1(content1), (s_2))], react.createElement("td", keyValueList(props_2, 1), ...children_2))), (props_4 = [(css_2 = singleton(new CSSProp(21, hasChanged ? "#baffd3" : "auto")), ["style", keyValueList(css_2, 1)])], (children_4 = [(s_4 = viewNum_1(content2), (s_4))], react.createElement("td", keyValueList(props_4, 1), ...children_4)))];
        return react.createElement("tr", keyValueList(props_6, 1), ...children_6);
    };
    const addr_3 = defaultArg(memoryEditorData.Address, fromBits(0, 0, false));
    const addr2 = op_Addition(addr_3, fromBits(15, 0, false));
    const children_20 = [table_3(singleton(new TableOption(2)), ofArray([(children_16 = [(children_14 = [react.createElement("th", {}, "Address"), react.createElement("th", {}, "Initial content"), react.createElement("th", {}, "Current content")], react.createElement("tr", {}, ...children_14))], react.createElement("thead", {}, ...children_16)), (children_18 = (list = ofSeq(rangeLong(addr_3, fromInt(1), addr2, false)), (mapping = partialApply(1, makeRow, [getData(addr_3, memory1.Data), getData(addr_3, memory2.Data)]), map(mapping, list))), react.createElement("tbody", {}, ...children_18))]))];
    return react.createElement("div", keyValueList([bodyStyle], 1), ...children_20);
}

function makeDiffViewer(memory1, memory2, dispatch, memoryEditorData) {
    const children = [makeEditorHeader(memory1, true, memoryEditorData, dispatch), makeDiffViewerBody(memory1, memory2, memoryEditorData)];
    return react.createElement("div", {}, ...children);
}

export function openMemoryDiffViewer(memory1, memory2, model, dispatch) {
    let msg;
    const clo1 = toText(printf("Memories in diffViewer do not match: %A\n%A"));
    const clo2 = clo1(memory1);
    msg = clo2(memory2);
    const cond = (memory1.AddressWidth === memory2.AddressWidth) ? (memory1.WordWidth === memory2.WordWidth) : false;
    assertThat(cond, msg);
    const title = "Memory diff viewer";
    const body = (memoryEditorData) => makeDiffViewer(memory1, memory2, dispatch, memoryEditorData);
    const foot = makeFoot(true, dispatch, model);
    showMemoryEditorPopup(title, body, foot, popupExtraStyle, dispatch);
}

