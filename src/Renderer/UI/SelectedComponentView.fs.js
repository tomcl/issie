import { div } from "../.fable/Fulma.2.9.0/Elements/Form/Field.fs.js";
import { ofSeq, map, cons, ofArray, singleton, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { label as label_1 } from "../.fable/Fulma.2.9.0/Elements/Form/Label.fs.js";
import { input, IInputType, Option } from "../.fable/Fulma.2.9.0/Elements/Form/Input.fs.js";
import { HTMLAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { getIntEventValue, getTextEventValue } from "../Interface/JSHelpers.fs.js";
import { toFail, printf, toText } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import * as react from "react";
import { op_UnaryNegation, fromInteger, compare, fromBits, op_LeftShift } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { Option as Option_1, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { Color_IColor } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { openMemoryEditor } from "./MemoryEditorView.fs.js";
import { formatLabel, setComponentLabel, extractLabelBase, setComponentLabelFromText, formatLabelAsBus, errorPropsNotification } from "./PopupView.fs.js";
import { Msg } from "./ModelType.fs.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { Draw2dWrapper__SetConstantNumber, Draw2dWrapper__SetLsbBitNumber, Draw2dWrapper__SetRegisterWidth, Draw2dWrapper__SetTopOutputWidth, Draw2dWrapper__SetNumberOfBits } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { singleton as singleton_1, append, delay } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";

function readOnlyFormField(name, body) {
    return div(empty(), ofArray([label_1(empty(), singleton(name)), body]));
}

function textFormField(isRequired, name, defaultValue, onChange) {
    let options;
    return div(empty(), ofArray([label_1(empty(), singleton(name)), (options = ofArray([new Option(15, ofArray([new HTMLAttr(148, false), new HTMLAttr(123, name), new HTMLAttr(55, true), ["style", {
        width: "200px",
    }]])), new Option(10, defaultValue), new Option(1, new IInputType(0)), new Option(12, isRequired ? "Name (required)" : "Name (optional)"), new Option(13, (arg) => {
        onChange((getTextEventValue(arg)));
    })]), input(cons(new Option(1, new IInputType(0)), options)))]));
}

function intFormField(name, defaultValue, minValue, onChange) {
    let options, arg0, clo1;
    return div(empty(), ofArray([label_1(empty(), singleton(name)), (options = ofArray([new Option(15, ofArray([["style", {
        width: "60px",
    }], new HTMLAttr(119, minValue)])), (arg0 = (clo1 = toText(printf("%d")), clo1(defaultValue)), (new Option(10, arg0))), new Option(13, (arg) => {
        onChange((getIntEventValue(arg)));
    })]), input(cons(new Option(1, new IInputType(7)), options)))]));
}

function intFormFieldNoMin(name, defaultValue, onChange) {
    let options, arg0, clo1;
    return div(empty(), ofArray([label_1(empty(), singleton(name)), (options = ofArray([new Option(15, singleton(["style", {
        width: "60px",
    }])), (arg0 = (clo1 = toText(printf("%d")), clo1(defaultValue)), (new Option(10, arg0))), new Option(13, (arg) => {
        onChange((getIntEventValue(arg)));
    })]), input(cons(new Option(1, new IInputType(7)), options)))]));
}

function makeMemoryInfo(descr, mem, compId, model, dispatch) {
    let s_1, clo1, s_3, arg10_1, clo1_1, s_5, clo1_2;
    const children = [descr, react.createElement("br", {}), react.createElement("br", {}), (s_1 = (clo1 = toText(printf("Address width: %d bit(s)")), clo1(mem.AddressWidth)), (s_1)), react.createElement("br", {}), (s_3 = (arg10_1 = op_LeftShift(fromBits(1, 0, true), mem.AddressWidth), (clo1_1 = toText(printf("Number of elements: %d")), clo1_1(arg10_1))), (s_3)), react.createElement("br", {}), (s_5 = (clo1_2 = toText(printf("Word width: %d bit(s)")), clo1_2(mem.WordWidth)), (s_5)), react.createElement("br", {}), react.createElement("br", {}), button(ofArray([new Option_1(0, new Color_IColor(4)), new Option_1(17, (_arg1) => {
        openMemoryEditor(mem, compId, model, dispatch);
    })]), singleton("View/Edit memory content"))];
    return react.createElement("div", {}, ...children);
}

function makeNumberOfBitsField(model, comp, text, setter, dispatch) {
    let patternInput;
    const matchValue = comp.Type;
    let pattern_matching_result, w;
    switch (matchValue.tag) {
        case 0: {
            pattern_matching_result = 0;
            w = matchValue.fields[0];
            break;
        }
        case 1: {
            pattern_matching_result = 0;
            w = matchValue.fields[0];
            break;
        }
        case 15: {
            pattern_matching_result = 0;
            w = matchValue.fields[0];
            break;
        }
        case 21: {
            pattern_matching_result = 0;
            w = matchValue.fields[0];
            break;
        }
        case 18: {
            pattern_matching_result = 1;
            break;
        }
        case 3: {
            pattern_matching_result = 2;
            break;
        }
        case 4: {
            pattern_matching_result = 3;
            break;
        }
        default: pattern_matching_result = 4}
    switch (pattern_matching_result) {
        case 0: {
            patternInput = ["Number of bits", w];
            break;
        }
        case 1: {
            const w_1 = matchValue.fields[0] | 0;
            patternInput = ["Number of bits in the top wire", w_1];
            break;
        }
        case 2: {
            const w_2 = matchValue.fields[0] | 0;
            patternInput = ["Number of bits selected: width", w_2];
            break;
        }
        case 3: {
            const w_3 = matchValue.fields[0] | 0;
            patternInput = ["Number of bits in the wire", w_3];
            break;
        }
        case 4: {
            const c = matchValue;
            const clo1 = toFail(printf("makeNumberOfBitsField called with invalid component: %A"));
            patternInput = clo1(c);
            break;
        }
    }
    const width = patternInput[1] | 0;
    const title = patternInput[0];
    return intFormField(title, width, 1, (newWidth) => {
        if (newWidth < 1) {
            const props = errorPropsNotification("Invalid number of bits.");
            dispatch(new Msg(35, props));
        }
        else {
            setter(comp.Id, newWidth);
            const text$0027 = (comp.Type.tag === 3) ? text : formatLabelAsBus(newWidth, text);
            setComponentLabelFromText(model, comp, text$0027);
            let lastUsedWidth;
            const matchValue_2 = comp.Type;
            switch (matchValue_2.tag) {
                case 18:
                case 3: {
                    lastUsedWidth = model.LastUsedDialogWidth;
                    break;
                }
                default: {
                    lastUsedWidth = newWidth;
                }
            }
            dispatch(new Msg(38, lastUsedWidth));
            dispatch(new Msg(36));
        }
    });
}

function makeConstantValueField(model, comp, setter, dispatch) {
    let patternInput;
    const matchValue = comp.Type;
    if (matchValue.tag === 4) {
        const width = matchValue.fields[0] | 0;
        const cVal = matchValue.fields[1] | 0;
        patternInput = [cVal, width];
    }
    else {
        const clo1 = toFail(printf("makeConstantValuefield called from %A"));
        patternInput = clo1(comp.Type);
    }
    const width_1 = patternInput[1] | 0;
    const cVal_1 = patternInput[0] | 0;
    if (width_1 > 32) {
        const note = errorPropsNotification("Invalid Constant width");
        dispatch(new Msg(35, note));
    }
    return intFormFieldNoMin("Value of the wire:", cVal_1, (newCVal) => {
        if ((compare(fromInteger(newCVal, false, 2), op_LeftShift(fromBits(1, 0, false), width_1)) >= 0) ? true : (compare(fromInteger(newCVal, false, 2), op_UnaryNegation(op_LeftShift(fromBits(1, 0, false), width_1 - 1))) < 0)) {
            let errMsg;
            const clo1_1 = toText(printf("Constant value too large for number of bits: %d requires more than %d bits"));
            const clo2 = clo1_1(newCVal);
            errMsg = clo2(width_1);
            const note_1 = errorPropsNotification(errMsg);
            dispatch(new Msg(35, note_1));
        }
        else {
            setter(comp.Id, newCVal);
            const lastUsedWidth = model.LastUsedDialogWidth | 0;
            dispatch(new Msg(38, lastUsedWidth));
            dispatch(new Msg(36));
        }
    });
}

function makeLsbBitNumberField(model, comp, setter, dispatch) {
    let lsbPos;
    const matchValue = comp.Type;
    if (matchValue.tag === 3) {
        const width = matchValue.fields[0] | 0;
        const lsb = matchValue.fields[1] | 0;
        lsbPos = lsb;
    }
    else {
        const clo1 = toFail(printf("makeLsbBitNumberfield called from %A"));
        lsbPos = clo1(comp.Type);
    }
    return intFormField("Least Significant Bit number selected: lsb", lsbPos, 0, (newLsb) => {
        if (newLsb < 0) {
            const note = errorPropsNotification("Invalid LSB bit position");
            dispatch(new Msg(35, note));
        }
        else {
            setter(comp.Id, newLsb);
            let lastUsedWidth;
            const matchValue_1 = comp.Type;
            switch (matchValue_1.tag) {
                case 18:
                case 3: {
                    lastUsedWidth = model.LastUsedDialogWidth;
                    break;
                }
                default: {
                    lastUsedWidth = newLsb;
                }
            }
            dispatch(new Msg(38, lastUsedWidth));
            dispatch(new Msg(36));
        }
    });
}

function makeDescription(comp, model, dispatch) {
    let s_10, clo1, s_16, clo1_1, s_22, clo1_3, props_34, children_20, children_22, props_38, children_24, children_26;
    const matchValue = comp.Type;
    switch (matchValue.tag) {
        case 4: {
            return "Constant Wire.";
        }
        case 1: {
            return "Output.";
        }
        case 3: {
            const children = ["Bus Selection.", react.createElement("br", {}), "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this selects one bit. Error if the input has less than width + lsb bits.", react.createElement("br", {}), react.createElement("br", {}), "Note that the output bit(s) are numbered from 0 even if the input range has LS bit number \u003e 0. The input bits connected are displayed in the schematic symbol"];
            return react.createElement("div", {}, ...children);
        }
        case 2: {
            const children_2 = ["Label on Wire or Bus. Labels with the same name connect wires. Each label has input on left and output on right. No output connection is required from a set of labels. Since a set represents one wire of bus, exactly one input connection is required. Labels can be used:", react.createElement("br", {}), "To name wires and document designs.", react.createElement("br", {}), "To join inputs and outputs without wires.", react.createElement("br", {}), "To prevent an unused output from giving an error."];
            return react.createElement("div", {}, ...children_2);
        }
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 10:
        case 11: {
            const children_4 = [(s_10 = (clo1 = toText(printf("%A gate.")), clo1(comp.Type)), (s_10))];
            return react.createElement("div", {}, ...children_4);
        }
        case 13: {
            return react.createElement("div", {}, "Multiplexer with two inputs and one output.");
        }
        case 14: {
            return react.createElement("div", {}, "Demultiplexer with one input and two outputs.");
        }
        case 17: {
            return react.createElement("div", {}, "Merge two wires of width n and m into a single wire of width n+m.");
        }
        case 18: {
            return react.createElement("div", {}, "Split a wire of width n+m into two wires of width n and m.");
        }
        case 15: {
            const numberOfBits = matchValue.fields[0] | 0;
            const children_14 = [(s_16 = (clo1_1 = toText(printf("%d bit(s) adder.")), clo1_1(numberOfBits)), (s_16))];
            return react.createElement("div", {}, ...children_14);
        }
        case 12: {
            const children_16 = [("4 bit decoder: Data is output on the Sel output, all other outputs are 0.")];
            return react.createElement("div", {}, ...children_16);
        }
        case 16: {
            const custom = matchValue.fields[0];
            const toHTMLList = (list) => map((tupledArg) => {
                let s_20, clo1_2, clo2;
                const label = tupledArg[0];
                const width = tupledArg[1] | 0;
                const children_18 = [(s_20 = (clo1_2 = toText(printf("%s: %d bit(s)")), clo2 = clo1_2(label), clo2(width)), (s_20))];
                return react.createElement("li", {}, ...children_18);
            }, list);
            const children_28 = [(s_22 = (clo1_3 = toText(printf("%s: user defined component.")), clo1_3(custom.Name)), (s_22)), react.createElement("br", {}), (props_34 = [["style", {
                fontStyle: "italic",
            }]], (children_20 = [("Inputs")], react.createElement("span", keyValueList(props_34, 1), ...children_20))), (children_22 = toHTMLList(custom.InputLabels), react.createElement("ul", {}, ...children_22)), (props_38 = [["style", {
                fontStyle: "italic",
            }]], (children_24 = [("Outputs")], react.createElement("span", keyValueList(props_38, 1), ...children_24))), (children_26 = toHTMLList(custom.OutputLabels), react.createElement("ul", {}, ...children_26))];
            return react.createElement("div", {}, ...children_28);
        }
        case 19: {
            return react.createElement("div", {}, "D-flip-flop. The component is implicitly connected to the global clock.");
        }
        case 20: {
            return react.createElement("div", {}, "D-flip-flop with enable. If the enable signal is high the state of\r\n             the D-flip-flop will be updated at the next clock cycle.\r\n             The component is implicitly connected to the global clock.");
        }
        case 21: {
            return react.createElement("div", {}, "Register. The component is implicitly connected to the global clock.");
        }
        case 22: {
            return react.createElement("div", {}, "Register with enable. If the enable signal is high the\r\n                      state of the Register will be updated at the next clock\r\n                      cycle. The component is implicitly connected to the global\r\n                      clock.");
        }
        case 23: {
            const mem = matchValue.fields[0];
            const descr = "Asynchronous ROM: the output is updated as soon as the address changes.";
            return makeMemoryInfo(descr, mem, comp.Id, model, dispatch);
        }
        case 24: {
            const mem_1 = matchValue.fields[0];
            const descr_1 = "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock.";
            return makeMemoryInfo(descr_1, mem_1, comp.Id, model, dispatch);
        }
        case 25: {
            const mem_2 = matchValue.fields[0];
            const descr_2 = "RAM memory. At every clock tick, the RAM can either read or write\r\n            the content of the memory location selected by the address. If the\r\n            write signal is high, the content of the selected memory location\r\n            is set to the value of data-in. This value will also be propagated\r\n            to data-out immediately. The component is implicitly connected to\r\n            the global clock.";
            return makeMemoryInfo(descr_2, mem_2, comp.Id, model, dispatch);
        }
        default: {
            return "Input.";
        }
    }
}

function makeExtraInfo(model, comp, text, dispatch) {
    const matchValue = comp.Type;
    switch (matchValue.tag) {
        case 0:
        case 1:
        case 15: {
            return makeNumberOfBitsField(model, comp, text, (arg00, arg10) => {
                Draw2dWrapper__SetNumberOfBits(model.Diagram, arg00, arg10);
            }, dispatch);
        }
        case 18: {
            return makeNumberOfBitsField(model, comp, text, (arg00_1, arg10_1) => {
                Draw2dWrapper__SetTopOutputWidth(model.Diagram, arg00_1, arg10_1);
            }, dispatch);
        }
        case 21: {
            return makeNumberOfBitsField(model, comp, text, (arg00_2, arg10_2) => {
                Draw2dWrapper__SetRegisterWidth(model.Diagram, arg00_2, arg10_2);
            }, dispatch);
        }
        case 3: {
            const children = [makeNumberOfBitsField(model, comp, text, (arg00_3, arg10_3) => {
                Draw2dWrapper__SetNumberOfBits(model.Diagram, arg00_3, arg10_3);
            }, dispatch), makeLsbBitNumberField(model, comp, (arg00_4, arg10_4) => {
                Draw2dWrapper__SetLsbBitNumber(model.Diagram, arg00_4, arg10_4);
            }, dispatch)];
            return react.createElement("div", {}, ...children);
        }
        case 4: {
            const children_2 = [makeNumberOfBitsField(model, comp, text, (arg00_5, arg10_5) => {
                Draw2dWrapper__SetNumberOfBits(model.Diagram, arg00_5, arg10_5);
            }, dispatch), makeConstantValueField(model, comp, (arg00_6, arg10_6) => {
                Draw2dWrapper__SetConstantNumber(model.Diagram, arg00_6, arg10_6);
            }, dispatch)];
            return react.createElement("div", {}, ...children_2);
        }
        default: {
            return react.createElement("div", {});
        }
    }
}

export function viewSelectedComponent(model, dispatch) {
    const matchValue = model.SelectedComponent;
    if (matchValue != null) {
        const comp = matchValue;
        const children_2 = ofSeq(delay(() => {
            let body;
            const label$0027 = extractLabelBase(comp.Label);
            return append(singleton_1((body = makeDescription(comp, model, dispatch), (readOnlyFormField("Description", body)))), delay(() => append(singleton_1(makeExtraInfo(model, comp, label$0027, dispatch)), delay(() => {
                let required;
                const matchValue_1 = comp.Type;
                switch (matchValue_1.tag) {
                    case 18:
                    case 17: {
                        required = false;
                        break;
                    }
                    default: {
                        required = true;
                    }
                }
                return singleton_1(textFormField(required, "Component Name", label$0027, (text) => {
                    setComponentLabel(model, comp, formatLabel(comp, text));
                    dispatch(new Msg(38, model.LastUsedDialogWidth));
                }));
            }))));
        }));
        return react.createElement("div", {
            key: comp.Id,
        }, ...children_2);
    }
    else {
        return react.createElement("div", {}, "Select a component in the diagram to view or change its properties, for example number of bits.");
    }
}

