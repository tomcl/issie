import * as electron from "electron";
import { map, filter, takeWhile } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { isLetterOrDigit } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Char.js";
import { printf, toText, join } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Draw2dWrapper__EditComponentLabel } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { value as value_1, defaultArg } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { fromBits } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Long.js";
import { NumberBase as NumberBase_1 } from "../../Common/CommonTypes.fs.js";
import { IntMode, Msg, MemoryEditorData } from "./ModelType.fs.js";
import { Card_body, Card_card, background, Option, modal, Card_foot, Card_title, Card_head } from "../.fable/Fulma.2.9.0/Components/Modal.fs.js";
import { tryPick, cons, ofArray, singleton, empty } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import * as react from "react";
import { TextWeight_Option, TextSize_Option, Screen, Modifier_IModifier, Text_span, Color_IColor, Common_GenericOption } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { CSSProp, HTMLAttr, DOMAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { Option as Option_1, delete$ } from "../.fable/Fulma.2.9.0/Elements/Delete.fs.js";
import { IInputType, Option as Option_2, input } from "../.fable/Fulma.2.9.0/Elements/Form/Input.fs.js";
import { getIntEventValue, getTextEventValue } from "../Interface/JSHelpers.fs.js";
import { pow2int64 } from "../../Common/Helpers.fs.js";
import { item, right, left, Level_Option, level } from "../.fable/Fulma.2.9.0/Layouts/Level.fs.js";
import { Option as Option_3, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { partialApply } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { Option as Option_4, notification as notification_1 } from "../.fable/Fulma.2.9.0/Elements/Notification.fs.js";
import { notificationStyle } from "./Style.fs.js";
import { VersionString } from "../Interface/Version.fs.js";

export function openInBrowser(url, _arg1) {
    const value = electron.shell.openExternal(url);
    void value;
}

export function extractLabelBase(text) {
    let strings;
    let source_2;
    let source_1;
    const source = text.toLocaleUpperCase();
    source_1 = takeWhile((ch) => (ch !== "("), source.split(""));
    source_2 = filter((ch_1) => {
        if (isLetterOrDigit(ch_1)) {
            return true;
        }
        else {
            return ch_1 === "_";
        }
    }, source_1);
    strings = map((ch_2) => ch_2, source_2);
    return join("", strings);
}

export function formatLabelAsBus(width, text) {
    const text$0027 = extractLabelBase(text);
    if (width === 1) {
        return text$0027;
    }
    else {
        const arg20 = (width - 1) | 0;
        const clo1 = toText(printf("%s(%d:%d)"));
        const clo2 = clo1(text$0027);
        const clo3 = clo2(arg20);
        return clo3(0);
    }
}

export function formatLabelFromType(compType, text) {
    const text$0027 = extractLabelBase(text);
    let pattern_matching_result, width;
    if (compType.tag === 0) {
        if (compType.fields[0] === 1) {
            pattern_matching_result = 0;
        }
        else {
            pattern_matching_result = 1;
            width = compType.fields[0];
        }
    }
    else if (compType.tag === 1) {
        if (compType.fields[0] === 1) {
            pattern_matching_result = 0;
        }
        else {
            pattern_matching_result = 1;
            width = compType.fields[0];
        }
    }
    else {
        pattern_matching_result = 2;
    }
    switch (pattern_matching_result) {
        case 0: {
            return text$0027;
        }
        case 1: {
            const arg20 = (width - 1) | 0;
            const clo1 = toText(printf("%s(%d:%d)"));
            const clo2 = clo1(text$0027);
            const clo3 = clo2(arg20);
            return clo3(0);
        }
        case 2: {
            return text$0027;
        }
    }
}

export function formatLabel(comp, text) {
    return formatLabelFromType(comp.Type, text);
}

export function setComponentLabel(model, comp, text) {
    const label = formatLabel(comp, text);
    Draw2dWrapper__EditComponentLabel(model.Diagram, comp.Id, label);
}

export function setComponentLabelFromText(model, comp, text) {
    Draw2dWrapper__EditComponentLabel(model.Diagram, comp.Id, text);
}

export function getText(dialogData) {
    return defaultArg(dialogData.Text, "");
}

export function getInt(dialogData) {
    return defaultArg(dialogData.Int, 1);
}

export function getInt2(dialogData) {
    return defaultArg(dialogData.Int2, 0);
}

export function getMemorySetup(dialogData) {
    return defaultArg(dialogData.MemorySetup, [1, 1]);
}

export function getMemoryEditor(dialogData) {
    return defaultArg(dialogData.MemoryEditorData, new MemoryEditorData(false, void 0, fromBits(0, 0, false), new NumberBase_1(0)));
}

export function unclosablePopup(maybeTitle, body, maybeFoot, extraStyle) {
    let head;
    if (maybeTitle != null) {
        const title = maybeTitle;
        head = Card_head(empty(), singleton(Card_title(empty(), singleton(title))));
    }
    else {
        head = react.createElement("div", {});
    }
    let foot_1;
    if (maybeFoot != null) {
        const foot = value_1(maybeFoot);
        foot_1 = Card_foot(empty(), singleton(foot));
    }
    else {
        foot_1 = react.createElement("div", {});
    }
    return modal(singleton(new Option(1, true)), ofArray([background(empty(), empty()), Card_card(singleton(new Common_GenericOption(1, singleton(["style", keyValueList(extraStyle, 1)]))), ofArray([head, Card_body(empty(), singleton(body)), foot_1]))]));
}

export function showMemoryEditorPopup(maybeTitle, body, maybeFoot, extraStyle, dispatch) {
    return dispatch((new Msg(19, (dialogData) => {
        const memoryEditorData = getMemoryEditor(dialogData);
        return unclosablePopup(maybeTitle, body(memoryEditorData), maybeFoot, extraStyle);
    })));
}

function buildPopup(title, body, foot, close, extraStyle, dialogData) {
    return modal(ofArray([new Option(1, true), new Option(2, "modal1")]), ofArray([background(singleton(new Common_GenericOption(1, singleton(new DOMAttr(40, close)))), empty()), Card_card(singleton(new Common_GenericOption(1, singleton(["style", keyValueList(extraStyle, 1)]))), ofArray([Card_head(empty(), ofArray([Card_title(empty(), singleton(title)), delete$(singleton(new Option_1(3, close)), empty())])), Card_body(empty(), singleton(body(dialogData))), Card_foot(empty(), singleton(foot(dialogData)))]))]));
}

function dynamicClosablePopup(title, body, foot, extraStyle, dispatch) {
    dispatch((new Msg(19, (dialogData) => buildPopup(title, body, foot, (_arg1) => {
        dispatch(new Msg(20));
    }, extraStyle, dialogData))));
}

export function closablePopup(title, body, foot, extraStyle, dispatch) {
    dynamicClosablePopup(title, (_arg1) => body, (_arg2) => foot, extraStyle, dispatch);
}

export function dialogPopupBodyOnlyText(before, placeholder, dispatch, dialogData) {
    const children = [before(dialogData), input(ofArray([new Option_2(1, new IInputType(0)), new Option_2(15, ofArray([new HTMLAttr(55, true), new HTMLAttr(148, false)])), new Option_2(12, placeholder), new Option_2(13, (arg_2) => {
        let arg0_1, arg0;
        dispatch((arg0_1 = (arg0 = (getTextEventValue(arg_2)), arg0), new Msg(21, arg0_1)));
    })]))];
    return react.createElement("div", {}, ...children);
}

export function dialogPopupBodyOnlyInt(beforeInt, intDefault, dispatch) {
    let arg0_1;
    dispatch((arg0_1 = (intDefault), (new Msg(22, arg0_1))));
    return (dialogData) => {
        let options, arg0_2, clo1;
        const children = [beforeInt(dialogData), react.createElement("br", {}), (options = ofArray([new Option_2(15, ofArray([["style", {
            width: "60px",
        }], new HTMLAttr(55, true)])), (arg0_2 = (clo1 = toText(printf("%d")), clo1(intDefault)), (new Option_2(10, arg0_2))), new Option_2(13, (arg_2) => {
            let arg0_4, arg0_3;
            dispatch((arg0_4 = (arg0_3 = ((getIntEventValue(arg_2)) | 0), arg0_3), new Msg(22, arg0_4)));
        })]), input(cons(new Option_2(1, new IInputType(7)), options)))];
        return react.createElement("div", {}, ...children);
    };
}

export function dialogPopupBodyTwoInts(beforeInt1, beforeInt2, intDefault1, intDefault2, dispatch) {
    const setPopupTwoInts = (whichInt, n) => {
        dispatch((new Msg(23, [n, whichInt])));
    };
    setPopupTwoInts(new IntMode(0), intDefault1);
    setPopupTwoInts(new IntMode(1), intDefault2);
    return (dialogData) => {
        let options, arg0_1, clo1, options_1, arg0_2, clo1_1;
        const children = [beforeInt1(dialogData), react.createElement("br", {}), (options = ofArray([new Option_2(15, ofArray([["style", {
            width: "60px",
        }], new HTMLAttr(55, true)])), (arg0_1 = (clo1 = toText(printf("%d")), clo1(intDefault1)), (new Option_2(10, arg0_1))), new Option_2(13, (arg) => {
            setPopupTwoInts(new IntMode(0), (getIntEventValue(arg)));
        })]), input(cons(new Option_2(1, new IInputType(7)), options))), react.createElement("br", {}), beforeInt2(dialogData), react.createElement("br", {}), (options_1 = ofArray([new Option_2(15, ofArray([["style", {
            width: "60px",
        }], new HTMLAttr(55, true)])), (arg0_2 = (clo1_1 = toText(printf("%d")), clo1_1(intDefault2)), (new Option_2(10, arg0_2))), new Option_2(13, (arg_1) => {
            setPopupTwoInts(new IntMode(1), (getIntEventValue(arg_1)));
        })]), input(cons(new Option_2(1, new IInputType(7)), options_1)))];
        return react.createElement("div", {}, ...children);
    };
}

export function dialogPopupBodyTextAndInt(beforeText, placeholder, beforeInt, intDefault, dispatch) {
    let arg0_1;
    dispatch((arg0_1 = (intDefault), (new Msg(22, arg0_1))));
    return (dialogData) => {
        let options_1, arg0_4, clo1;
        const children = [beforeText(dialogData), input(ofArray([new Option_2(1, new IInputType(0)), new Option_2(15, ofArray([new HTMLAttr(55, true), new HTMLAttr(148, false)])), new Option_2(12, placeholder), new Option_2(13, (arg_2) => {
            let arg0_3, arg0_2;
            dispatch((arg0_3 = (arg0_2 = (getTextEventValue(arg_2)), arg0_2), new Msg(21, arg0_3)));
        })])), react.createElement("br", {}), react.createElement("br", {}), beforeInt(dialogData), react.createElement("br", {}), (options_1 = ofArray([new Option_2(15, singleton(["style", {
            width: "60px",
        }])), (arg0_4 = (clo1 = toText(printf("%d")), clo1(intDefault)), (new Option_2(10, arg0_4))), new Option_2(13, (arg_5) => {
            let arg0_6, arg0_5;
            dispatch((arg0_6 = (arg0_5 = ((getIntEventValue(arg_5)) | 0), arg0_5), new Msg(22, arg0_6)));
        })]), input(cons(new Option_2(1, new IInputType(7)), options_1)))];
        return react.createElement("div", {}, ...children);
    };
}

export function dialogPopupBodyMemorySetup(intDefault, dispatch) {
    dispatch((new Msg(24, [4, intDefault])));
    return (dialogData) => {
        let s_1, arg20, clo1, clo2, options, clo1_1, options_1, clo1_2;
        const patternInput = getMemorySetup(dialogData);
        const wordWidth = patternInput[1] | 0;
        const addressWidth = patternInput[0] | 0;
        const children = ["How many bits should be used to address the data in memory?", react.createElement("br", {}), (s_1 = (arg20 = pow2int64(addressWidth), (clo1 = toText(printf("%d bits yield %d memory locations.")), clo2 = clo1(addressWidth), clo2(arg20))), (s_1)), react.createElement("br", {}), (options = ofArray([new Option_2(15, ofArray([["style", {
            width: "60px",
        }], new HTMLAttr(55, true)])), new Option_2(10, (clo1_1 = toText(printf("%d")), clo1_1(4))), new Option_2(13, (arg) => {
            let newAddrWidth;
            newAddrWidth = getIntEventValue(arg);
            dispatch((new Msg(24, [newAddrWidth, wordWidth])));
        })]), input(cons(new Option_2(1, new IInputType(7)), options))), react.createElement("br", {}), react.createElement("br", {}), "How many bits should each memory word contain?", react.createElement("br", {}), (options_1 = ofArray([new Option_2(15, singleton(["style", {
            width: "60px",
        }])), new Option_2(10, (clo1_2 = toText(printf("%d")), clo1_2(intDefault))), new Option_2(13, (arg_1) => {
            let newWordWidth;
            newWordWidth = getIntEventValue(arg_1);
            dispatch((new Msg(24, [addressWidth, newWordWidth])));
        })]), input(cons(new Option_2(1, new IInputType(7)), options_1))), react.createElement("br", {}), react.createElement("br", {}), "You will be able to set the content of the memory from the Component Properties menu."];
        return react.createElement("div", {}, ...children);
    };
}

export function dialogPopup(title, body, buttonText, buttonAction, isDisabled, dispatch) {
    const foot = (dialogData) => level(singleton(new Level_Option(0, singleton(["style", {
        width: "100%",
    }]))), ofArray([left(empty(), empty()), right(empty(), ofArray([item(empty(), singleton(button(ofArray([new Option_3(0, new Color_IColor(2)), new Option_3(17, (_arg1) => {
        dispatch(new Msg(20));
    })]), singleton("Cancel")))), item(empty(), singleton(button(ofArray([new Option_3(15, isDisabled(dialogData)), new Option_3(0, new Color_IColor(4)), new Option_3(17, (_arg2) => {
        buttonAction(dialogData);
    })]), singleton(buttonText))))]))]));
    dynamicClosablePopup(title, body, foot, empty(), dispatch);
}

export function confirmationPopup(title, body, buttonText, buttonAction, dispatch) {
    const foot = level(singleton(new Level_Option(0, singleton(["style", {
        width: "100%",
    }]))), ofArray([left(empty(), empty()), right(empty(), ofArray([item(empty(), singleton(button(ofArray([new Option_3(0, new Color_IColor(2)), new Option_3(17, (_arg1) => {
        dispatch(new Msg(20));
    })]), singleton("Cancel")))), item(empty(), singleton(button(ofArray([new Option_3(0, new Color_IColor(4)), new Option_3(17, buttonAction)]), singleton(buttonText))))]))]));
    closablePopup(title, body, foot, empty(), dispatch);
}

export function choicePopup(title, body, buttonTrueText, buttonFalseText, buttonAction, dispatch) {
    const foot = level(singleton(new Level_Option(0, singleton(["style", {
        width: "100%",
    }]))), ofArray([left(empty(), empty()), right(empty(), ofArray([item(empty(), singleton(button(ofArray([new Option_3(0, new Color_IColor(2)), new Option_3(17, partialApply(1, buttonAction, [false]))]), singleton(buttonFalseText)))), item(empty(), singleton(button(ofArray([new Option_3(0, new Color_IColor(4)), new Option_3(17, partialApply(1, buttonAction, [true]))]), singleton(buttonTrueText))))]))]));
    closablePopup(title, body, foot, empty(), dispatch);
}

export function viewPopup(model) {
    const matchValue = model.PopupViewFunc;
    if (matchValue != null) {
        const popup = matchValue;
        return popup(model.PopupDialogData);
    }
    else {
        return react.createElement("div", {});
    }
}

export function errorNotification(text, closeMsg, dispatch) {
    const close = (_arg1) => {
        dispatch(closeMsg);
    };
    return notification_1(ofArray([new Option_4(0, new Color_IColor(8)), new Option_4(3, singleton(notificationStyle))]), ofArray([delete$(singleton(new Option_1(3, close)), empty()), text]));
}

export function errorPropsNotification(text) {
    return (dispatch) => errorNotification(text, new Msg(36), dispatch);
}

export function errorFilesNotification(text) {
    return (dispatch) => errorNotification(text, new Msg(32), dispatch);
}

export function warningNotification(text, closeMsg, dispatch) {
    const close = (_arg1) => {
        dispatch(closeMsg);
    };
    return notification_1(ofArray([new Option_4(0, new Color_IColor(7)), new Option_4(3, singleton(notificationStyle))]), ofArray([delete$(singleton(new Option_1(3, close)), empty()), text]));
}

export function warningPropsNotification(text) {
    return (dispatch) => warningNotification(text, new Msg(36), dispatch);
}

export function warningSimNotification(text) {
    return (dispatch) => warningNotification(text, new Msg(29), dispatch);
}

export function viewNotifications(model, dispatch) {
    let _arg1;
    _arg1 = tryPick((x) => x, ofArray([model.Notifications.FromDiagram, model.Notifications.FromSimulation, model.Notifications.FromFiles, model.Notifications.FromMemoryEditor, model.Notifications.FromProperties]));
    if (_arg1 == null) {
        return react.createElement("div", {});
    }
    else {
        const notification = _arg1;
        return notification(dispatch);
    }
}

export function viewInfoPopup(dispatch) {
    let props_22, children_12;
    const makeH = (h) => Text_span(singleton(new Common_GenericOption(2, ofArray([new Modifier_IModifier(3, new Screen(1), new TextSize_Option(5)), new Modifier_IModifier(2, new TextWeight_Option(3))]))), ofArray([h, react.createElement("br", {})]));
    const title = "ISSIE: Interactive Schematic Simulator and Integrated Editor";
    let body;
    const children_14 = [makeH("Version"), VersionString, react.createElement("br", {}), react.createElement("br", {}), makeH("Acknowledgments"), "ISSIE was created by Marco Selvatici (EIE 3rd year) as his BEng final year project. The waveform viewer was created by Edoardo Santi (EEE 3rd year) during Summer UROP work.", react.createElement("br", {}), react.createElement("br", {}), makeH("Introduction"), "Issie designs are made of one or more sheets. Each sheet contains components and Input and Output Connectors. If you have a single sheet that is your complete design. Otherwise any sheet can include the hardware defined another by adding a custom component from My Project in the Catalog. Multiple copies of other sheets can be added.", react.createElement("br", {}), react.createElement("br", {}), "The Simulation Tab is used mainly for combinational logic and simple clocked logic: the top \u0027Waveforms \u003e\u003e\u0027 button works with clocked circuits and displays waveforms.", react.createElement("br", {}), react.createElement("br", {}), "In Issie all clocked components use the same clock signal. Clk connections are not shown: all clk ports are\r\n        automatically connected together. In the waveforms active clock edges are indicated by verticals line through all the waveforms that separate clock cycles. The clock is not shown.", react.createElement("br", {}), react.createElement("br", {}), (props_22 = [(new DOMAttr(40, (_arg1) => {
        openInBrowser("https://github.com/tomcl/ISSIE", _arg1);
    }))], react.createElement("button", keyValueList(props_22, 1), "See the Issie Github Repo for more information")), react.createElement("br", {}), react.createElement("br", {}), makeH("Keyboard shortcuts"), "On Mac use Command instead of Ctrl.", (children_12 = [react.createElement("li", {}, "Save: Ctrl + S"), react.createElement("li", {}, "Copy selected diagram items: Alt + C"), react.createElement("li", {}, "Paste diagram items: Alt + V"), react.createElement("li", {}, "Undo last diagram action: Alt + Z"), react.createElement("li", {}, "Redo last diagram action: Alt + Shift + Z")], react.createElement("ul", {}, ...children_12))];
    body = react.createElement("div", {}, ...children_14);
    const foot = react.createElement("div", {});
    closablePopup(title, body, foot, singleton(new CSSProp(395, 800)), dispatch);
}

