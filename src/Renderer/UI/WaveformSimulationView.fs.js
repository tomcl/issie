import { getTextWidthInPixels } from "../Interface/JSHelpers.fs.js";
import { initFileWS, mapKeys, netGroup2Label, netList2NetGroups, getAllNetGroups, isCursorVisible, makeCursorVisiblePos, showSimulationLoading, button as button_1, makeLabels, removeSuffixFromWaveLabel, mapValues, waveNameOf, maxLastClk, wsModel2netList, isWaveSelected, selectNetGrpConns, cursorValueStrings } from "./WaveSimHelpers.fs.js";
import { concat, zip, unzip, sortByDescending, contains, groupBy, collect, sortBy, fold, append, mapIndexed, max, map, compareWith, equalsWith } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Array.js";
import { equals, structuralHash, hashSafe, equalsSafe, uncurry, min, compareSafe, max as max_1, comparePrimitives } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { ofArray as ofArray_1, tryFindKey, ofList, FSharpMap__get_Item } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { minViewerWidth, cursRectStyle, maxWavesColWidth, maxZoom, minZoom, zoomFactor, maxWavesColWidthFloat } from "./Style.fs.js";
import { getCurrFileWSMod, getComponentIds, setEditorNextView, getWSModelOrFail, RightTab, WaveSimModel, setDispNames, WSViewT, Msg, SimParamsT, setSimParams } from "./ModelType.fs.js";
import { toText, join, printf, toConsole } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import * as react from "react";
import { CSSProp, Prop, DOMAttr, HTMLAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { JSDiagramMsg$2, NumberBase } from "../../Common/CommonTypes.fs.js";
import { map as map_1, empty, cons, singleton, ofArray } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { Option, tabs, Tab_Option, tab } from "../.fable/Fulma.2.9.0/Components/Tabs.fs.js";
import { Option as Option_1, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { IInputType, input, Option as Option_2 } from "../.fable/Fulma.2.9.0/Elements/Form/Input.fs.js";
import { currWaveSimModel } from "./FileMenuView.fs.js";
import { tryParse } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Int32.js";
import { Browser_Types_Event__Event_get_Value } from "../.fable/Fable.React.5.4.0/Fable.React.Extensions.fs.js";
import { FSharpRef } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { Color_IColor } from "../.fable/Fulma.2.9.0/Common.fs.js";
import { defaultArg } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";
import { h4 } from "../.fable/Fulma.2.9.0/Elements/Heading.fs.js";
import { errorPropsNotification, warningPropsNotification } from "./PopupView.fs.js";
import { getNetList } from "../../Common/Helpers.fs.js";
import { viewSimulationError, makeSimData } from "./SimulationView.fs.js";
import { hasSynchronousComponents } from "../../Simulator/SynchronousUtils.fs.js";

export function maxUsedViewerWidth(wSMod) {
    const strWidth = (s) => getTextWidthInPixels(s, "12px segoe ui");
    let curLblColWidth;
    const matchValue = cursorValueStrings(wSMod);
    if ((!equalsWith((x, y) => compareWith(comparePrimitives, x, y), matchValue, null)) ? (matchValue.length === 0) : false) {
        curLblColWidth = 0;
    }
    else {
        const cVS = matchValue;
        let e2;
        const array_2 = map((arg) => {
            let array_1;
            array_1 = map(strWidth, arg, Float64Array);
            return max(array_1, {
                Compare: comparePrimitives,
            });
        }, cVS, Float64Array);
        e2 = max(array_2, {
            Compare: comparePrimitives,
        });
        curLblColWidth = max_1(comparePrimitives, 25, e2);
    }
    let namesColWidth;
    const matchValue_1 = wSMod.SimParams.DispNames;
    if ((!equalsWith(comparePrimitives, matchValue_1, null)) ? (matchValue_1.length === 0) : false) {
        namesColWidth = 0;
    }
    else {
        const wN = matchValue_1;
        let e2_1;
        const array_3 = map(strWidth, wN, Float64Array);
        e2_1 = max(array_3, {
            Compare: comparePrimitives,
        });
        namesColWidth = max_1(comparePrimitives, 100, e2_1);
    }
    let svgWaveColWidth;
    let matchValue_2;
    const ws = wSMod;
    matchValue_2 = map((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
    svgWaveColWidth = (((!equalsWith(compareSafe, matchValue_2, null)) ? (matchValue_2.length === 0) : false) ? 600 : maxWavesColWidthFloat(wSMod));
    const checkBoxCol = 25;
    const extraWidth = 45;
    const value = (((curLblColWidth + namesColWidth) + svgWaveColWidth) + checkBoxCol) + extraWidth;
    return (~(~value)) | 0;
}

function changeNetGroupConnsSelect(selFun, diagram, wSModel, name) {
    const netGroup = FSharpMap__get_Item(wSModel.AllNets, name);
    const on = selFun(netGroup);
    selectNetGrpConns(diagram, netGroup, on);
}

function toggleNetGroupConnsSelect(diagram, wSMod, netList, name) {
    changeNetGroupConnsSelect((arg) => {
        let value;
        value = isWaveSelected(diagram, netList, arg);
        return !value;
    }, diagram, wSMod, name);
}

function selectAllOn(model, wSModel) {
    let ws;
    return map((trgtLstGroup) => {
        selectNetGrpConns(model, trgtLstGroup, true);
    }, (ws = wSModel, (map((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames))));
}

function changeViewerZoom(compIds, plus, m, wsModel, dispatch) {
    const adjustLastClk = (viewW_mut, wsModel_1_mut) => {
        adjustLastClk:
        while (true) {
            const viewW = viewW_mut, wsModel_1 = wsModel_1_mut;
            const pars = wsModel_1.SimParams;
            if ((viewW * 1.4) < maxUsedViewerWidth(wsModel_1)) {
                viewW_mut = viewW;
                wsModel_1_mut = setSimParams((sp) => {
                    const LastClkTime = pars.LastClkTime + 2;
                    return new SimParamsT(sp.WaveViewerRadix, LastClkTime, sp.CursorTime, sp.ClkSvgWidth, sp.DispNames, sp.LastScrollPos);
                }, wsModel_1);
                continue adjustLastClk;
            }
            else {
                const clo1 = toConsole(printf("New LastClk=%d"));
                clo1(pars.LastClkTime);
                return pars.LastClkTime;
            }
            break;
        }
    };
    const netList = wsModel2netList(wsModel);
    const pars_1 = wsModel.SimParams;
    let newClkW;
    let e2_1;
    let e2;
    const y = plus ? zoomFactor : (1 / zoomFactor);
    e2 = (pars_1.ClkSvgWidth * y);
    e2_1 = max_1(comparePrimitives, minZoom, e2);
    newClkW = min(comparePrimitives, maxZoom, e2_1);
    const wSModNewClk = setSimParams((sp_1) => (new SimParamsT(sp_1.WaveViewerRadix, sp_1.LastClkTime, sp_1.CursorTime, newClkW, sp_1.DispNames, sp_1.LastScrollPos)), wsModel);
    let newPars;
    const matchValue = (~(~m.WaveSimViewerWidth)) > maxUsedViewerWidth(wSModNewClk);
    if (matchValue) {
        const LastClkTime_1 = adjustLastClk(m.WaveSimViewerWidth, wSModNewClk);
        newPars = (new SimParamsT(pars_1.WaveViewerRadix, LastClkTime_1, pars_1.CursorTime, newClkW, pars_1.DispNames, pars_1.LastScrollPos));
    }
    else {
        newPars = (new SimParamsT(pars_1.WaveViewerRadix, pars_1.LastClkTime, pars_1.CursorTime, newClkW, pars_1.DispNames, pars_1.LastScrollPos));
    }
    return dispatch(new Msg(48, [new WSViewT(3), newPars]));
}

function changeCursorPos(wSModel, dispatch, newCursorPos) {
    let arg0;
    const pars = wSModel.SimParams;
    const curs$0027 = min(comparePrimitives, maxLastClk, newCursorPos);
    const matchValue = [0 <= curs$0027, curs$0027 <= pars.LastClkTime];
    if (matchValue[0]) {
        if (matchValue[1]) {
            dispatch((arg0 = (setSimParams((sp) => (new SimParamsT(sp.WaveViewerRadix, sp.LastClkTime, curs$0027, sp.ClkSvgWidth, sp.DispNames, sp.LastScrollPos)), wSModel)), (new Msg(4, arg0))));
            dispatch(new Msg(50, true));
        }
        else {
            const pars$0027 = new SimParamsT(pars.WaveViewerRadix, pars.LastClkTime, curs$0027, pars.ClkSvgWidth, pars.DispNames, pars.LastScrollPos);
            dispatch(new Msg(48, [new WSViewT(3), pars$0027]));
            dispatch(new Msg(50, true));
        }
    }
}

function cursorMove(increase, wSMod, dispatch) {
    const matchValue = [increase, wSMod.SimParams.CursorTime];
    if (matchValue[0]) {
        const n = matchValue[1];
        const newCursorPos = n + 1;
        changeCursorPos(wSMod, dispatch, newCursorPos);
    }
    else {
        const n_1 = matchValue[1];
        const newCursorPos_1 = n_1 - 1;
        changeCursorPos(wSMod, dispatch, newCursorPos_1);
    }
}

function moveWave(diagram, netList, wSMod, up) {
    const moveBy = up ? -1.5 : 1.5;
    const addLastPort = (arr, p) => mapIndexed((i, el) => ((i !== (arr.length - 1)) ? el : [el[0], append(el[1], [p])]), arr);
    const svgCache = wSMod.DispWaveSVGCache;
    let movedNames;
    let array_4;
    let array_3;
    let array_2;
    let tuple;
    let array_1;
    array_1 = map((name) => [isWaveSelected(diagram, netList, FSharpMap__get_Item(wSMod.AllNets, name)), name], wSMod.SimParams.DispNames);
    const state = [[], false];
    tuple = fold(uncurry(2, (tupledArg) => {
        const arr_1 = tupledArg[0];
        const prevSel = tupledArg[1];
        return (tupledArg_1) => {
            const sel = tupledArg_1[0];
            const p_1 = tupledArg_1[1];
            const matchValue = [sel, prevSel];
            let pattern_matching_result, s;
            if (matchValue[0]) {
                if (matchValue[1]) {
                    pattern_matching_result = 0;
                }
                else {
                    pattern_matching_result = 1;
                    s = matchValue[0];
                }
            }
            else {
                pattern_matching_result = 1;
                s = matchValue[0];
            }
            switch (pattern_matching_result) {
                case 0: {
                    return [addLastPort(arr_1, p_1), sel];
                }
                case 1: {
                    return [append(arr_1, [[s, [p_1]]]), s];
                }
            }
        };
    }), state, array_1);
    array_2 = tuple[0];
    array_3 = mapIndexed((i_1, tupledArg_2) => {
        const sel_1 = tupledArg_2[0];
        const ports = tupledArg_2[1];
        if (sel_1) {
            return [i_1 + moveBy, ports];
        }
        else {
            return [i_1, ports];
        }
    }, array_2);
    array_4 = sortBy((tuple_1) => tuple_1[0], array_3, {
        Compare: comparePrimitives,
    });
    movedNames = collect((tuple_2) => tuple_2[1], array_4);
    const arg0 = setDispNames(movedNames, wSMod);
    return new Msg(4, arg0);
}

export function standardOrderGroups(groups, wSModel) {
    let dispP;
    const ws = wSModel;
    dispP = map((name) => FSharpMap__get_Item(ws.AllNets, name), ws.SimParams.DispNames);
    let array_5;
    let array_4;
    let array_3;
    let array_2;
    array_2 = map((ng) => [waveNameOf(wSModel, ng), ng], groups);
    array_3 = groupBy((tupledArg) => {
        const wave = tupledArg[1];
        return contains(wave, dispP, {
            Equals: equalsSafe,
            GetHashCode: hashSafe,
        });
    }, array_2, {
        Equals: (x_1, y_1) => (x_1 === y_1),
        GetHashCode: structuralHash,
    });
    array_4 = sortByDescending((tuple) => tuple[0], array_3, {
        Compare: comparePrimitives,
    });
    array_5 = collect((tupledArg_1) => {
        const isDisp = tupledArg_1[0];
        const nameNgList = tupledArg_1[1];
        if (isDisp) {
            return nameNgList;
        }
        else {
            return sortBy((tuple_1) => tuple_1[0], nameNgList, {
                Compare: comparePrimitives,
            });
        }
    }, array_4);
    return unzip(array_5);
}

function standardWaveformOrderWaveAdder(wSModel) {
    let tupledArg;
    const groups = mapValues(wSModel.AllNets);
    tupledArg = standardOrderGroups(groups, wSModel);
    const names = tupledArg[0];
    const ports = tupledArg[1];
    return new WaveSimModel(wSModel.InitWaveSimGraph, wSModel.SimParams, names, wSModel.AllNets, wSModel.DispWaveSVGCache, wSModel.SimDataCache, wSModel.CursorBoxIsEmpty, wSModel.WSViewState, wSModel.WSTransition, wSModel.LastCanvasState);
}

function waveAdderSelectAll(model, netList, wSMod) {
    let setTo;
    const array = mapValues(wSMod.AllNets);
    setTo = array.every((netgrp) => isWaveSelected(model, netList, netgrp));
    let value;
    const array_1 = mapValues(wSMod.AllNets);
    value = map((netGrp) => {
        selectNetGrpConns(model, netGrp, !setTo);
    }, array_1);
    void value;
}

function makeCursVals(wsModel) {
    const string2Lbl = (array) => map((l) => react.createElement("label", {
        className: "cursVals",
    }, l), array);
    const array_1 = cursorValueStrings(wsModel);
    return map(string2Lbl, array_1);
}

function waveSimViewerRows(compIds, diagram, netList, wsMod, dispatch) {
    const allPorts = wsMod.AllNets;
    let labelCols;
    let array_1;
    let array2;
    let waveNames;
    waveNames = map(removeSuffixFromWaveLabel, wsMod.SimParams.DispNames);
    array2 = makeLabels(waveNames);
    array_1 = zip(wsMod.SimParams.DispNames, array2);
    labelCols = map((tupledArg) => {
        let children, props, arg0, props_4;
        const name = tupledArg[0];
        const lab = tupledArg[1];
        const children_4 = [(children = [(props = [new HTMLAttr(159, "checkbox"), new HTMLAttr(65, "check"), (arg0 = isWaveSelected(diagram, netList, FSharpMap__get_Item(allPorts, name)), (new HTMLAttr(62, arg0))), ["style", {
            float: "left",
        }], new DOMAttr(9, (_arg1) => {
            toggleNetGroupConnsSelect(diagram, wsMod, netList, name);
        })], react.createElement("input", keyValueList(props, 1)))], react.createElement("td", {
            className: "checkboxCol",
        }, ...children)), (props_4 = [new HTMLAttr(65, "waveNamesCol"), ["style", {
            textAlign: "right",
        }]], react.createElement("td", keyValueList(props_4, 1), lab))];
        return react.createElement("tr", {
            className: "rowHeight",
        }, ...children_4);
    }, array_1);
    let cursValCol;
    const array_2 = makeCursVals(wsMod);
    cursValCol = map((c) => {
        const children_8 = [react.createElement("td", {
            className: "cursValsCol",
        }, ...c)];
        return react.createElement("tr", {
            className: "rowHeight",
        }, ...children_8);
    }, array_2);
    return [wsMod.DispWaveSVGCache, labelCols, cursValCol];
}

function radixTabs(wsModel, dispatch) {
    let radixString;
    radixString = ofList(ofArray([[new NumberBase(1), "uDec"], [new NumberBase(2), "Bin"], [new NumberBase(0), "Hex"], [new NumberBase(3), "sDec"]]));
    const radTab = (rad) => {
        let props, children, s;
        return tab(ofArray([new Tab_Option(0, equalsSafe(wsModel.SimParams.WaveViewerRadix, rad)), new Tab_Option(2, singleton(["style", {
            width: "35px",
            height: "30px",
        }]))]), singleton((props = [["style", {
            padding: "0 0 0 0",
            height: "30px",
        }], new DOMAttr(40, (_arg1) => {
            let inputRecord;
            dispatch(new Msg(48, [new WSViewT(3), (inputRecord = wsModel.SimParams, new SimParamsT(rad, inputRecord.LastClkTime, inputRecord.CursorTime, inputRecord.ClkSvgWidth, inputRecord.DispNames, inputRecord.LastScrollPos))]));
        })], (children = [(s = FSharpMap__get_Item(radixString, rad), s)], react.createElement("a", keyValueList(props, 1), ...children)))));
    };
    return tabs(ofArray([new Option(4), new Option(8, singleton(["style", {
        width: "140px",
        height: "30px",
        fontSize: "80%",
        float: "right",
        margin: "0 10px 0 10px",
    }]))]), [radTab(new NumberBase(2)), radTab(new NumberBase(0)), radTab(new NumberBase(1)), radTab(new NumberBase(3))]);
}

function cursorButtons(model, wSMod, dispatch) {
    let options, arg0, matchValue, wSMod_1, wSMod_2;
    const children = [button(ofArray([new Option_1(18, "cursLeft"), new Option_1(17, (_arg1) => {
        cursorMove(false, wSMod, dispatch);
    })]), singleton("◀")), (options = ofArray([new Option_2(15, ofArray([new HTMLAttr(119, 0), new HTMLAttr(65, "cursor form"), new HTMLAttr(148, false), new HTMLAttr(154, 1)])), new Option_2(3, "cursor"), (arg0 = (matchValue = currWaveSimModel(model), (matchValue != null) ? ((wSMod_1 = matchValue, wSMod_1.CursorBoxIsEmpty === false) ? (wSMod_2 = matchValue, wSMod_2.SimParams.CursorTime.toString()) : ((matchValue == null) ? "0" : "")) : ((matchValue == null) ? "0" : "")), (new Option_2(8, arg0))), new Option_2(13, (c) => {
        let arg0_1, arg0_2, arg0_3, n;
        let matchValue_1;
        let outArg = 0;
        matchValue_1 = [tryParse(Browser_Types_Event__Event_get_Value(c), 511, false, 32, new FSharpRef(() => outArg, (v) => {
            outArg = v;
        })), outArg];
        let pattern_matching_result;
        if (matchValue_1[0]) {
            if (n = (matchValue_1[1] | 0), n >= 0) {
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
                const n_1 = matchValue_1[1] | 0;
                dispatch((arg0_1 = (new WaveSimModel(wSMod.InitWaveSimGraph, wSMod.SimParams, wSMod.AllWaveNames, wSMod.AllNets, wSMod.DispWaveSVGCache, wSMod.SimDataCache, false, wSMod.WSViewState, wSMod.WSTransition, wSMod.LastCanvasState)), (new Msg(4, arg0_1))));
                const newCursorPos = n_1 >>> 0;
                changeCursorPos(wSMod, dispatch, newCursorPos);
                break;
            }
            case 1: {
                let pattern_matching_result_1;
                if (matchValue_1[0]) {
                    pattern_matching_result_1 = 1;
                }
                else if (Browser_Types_Event__Event_get_Value(c) === "") {
                    pattern_matching_result_1 = 0;
                }
                else {
                    pattern_matching_result_1 = 1;
                }
                switch (pattern_matching_result_1) {
                    case 0: {
                        dispatch((arg0_2 = (new WaveSimModel(wSMod.InitWaveSimGraph, wSMod.SimParams, wSMod.AllWaveNames, wSMod.AllNets, wSMod.DispWaveSVGCache, wSMod.SimDataCache, true, wSMod.WSViewState, wSMod.WSTransition, wSMod.LastCanvasState)), (new Msg(4, arg0_2))));
                        changeCursorPos(wSMod, dispatch, 0);
                        break;
                    }
                    case 1: {
                        dispatch((arg0_3 = (new WaveSimModel(wSMod.InitWaveSimGraph, wSMod.SimParams, wSMod.AllWaveNames, wSMod.AllNets, wSMod.DispWaveSVGCache, wSMod.SimDataCache, false, wSMod.WSViewState, wSMod.WSTransition, wSMod.LastCanvasState)), (new Msg(4, arg0_3))));
                        break;
                    }
                }
                break;
            }
        }
    })]), input(cons(new Option_2(1, new IInputType(7)), options))), button_1(singleton(new Option_1(18, "cursRight")), (_arg2) => {
        cursorMove(true, wSMod, dispatch);
    }, "▶")];
    return react.createElement("div", {
        className: "cursor",
    }, ...children);
}

function loadingButton(wsMod, dispatch) {
    if (showSimulationLoading(wsMod, dispatch)) {
        return button_1(ofArray([new Option_1(0, new Color_IColor(10)), new Option_1(12, true)]), (_arg1) => {
        }, "");
    }
    else {
        return "";
    }
}

function viewWaveSimButtonsBar(model, wSMod, dispatch) {
    const props = [["style", {
        height: "45px",
    }]];
    const children = [loadingButton(wSMod, dispatch), radixTabs(wSMod, dispatch), cursorButtons(model, wSMod, dispatch)];
    return react.createElement("div", keyValueList(props, 1), ...children);
}

function cursorValuesCol(rows) {
    let children_2, children_6;
    const rightCol = append([(children_2 = [react.createElement("td", {
        className: "rowHeight",
    })], react.createElement("tr", {
        className: "rowHeight",
    }, ...children_2))], rows);
    const props_8 = [["style", {
        float: "right",
        height: "100%",
        borderTop: "2px solid rgb(219,219,219)",
        borderLeft: "2px solid rgb(219,219,219)",
    }]];
    const children_8 = [(children_6 = [react.createElement("tbody", {}, ...rightCol)], react.createElement("table", {}, ...children_6))];
    return react.createElement("div", keyValueList(props_8, 1), ...children_8);
}

function nameLabelsCol(model, netList, wsMod, labelRows, dispatch) {
    let children_6, children_4, children_2, children_12, children_16;
    let waveAddDelBut;
    const children = [button(ofArray([new Option_1(18, "newWaveButton"), new Option_1(0, new Color_IColor(6)), new Option_1(17, (_arg1) => {
        openEditorFromViewer(model, new WSViewT(2), dispatch);
    })]), singleton("Edit list..."))];
    waveAddDelBut = react.createElement("th", {
        className: "waveNamesCol",
    }, ...children);
    const top = [(children_6 = [(children_4 = [(children_2 = [button(ofArray([new Option_1(18, "updownBut"), new Option_1(17, (_arg2) => {
        dispatch(moveWave(model.Diagram, netList, wsMod, true));
    })]), singleton("▲")), button(ofArray([new Option_1(18, "updownBut"), new Option_1(17, (_arg3) => {
        dispatch(moveWave(model.Diagram, netList, wsMod, false));
    })]), singleton("▼"))], react.createElement("div", {
        className: "updownDiv",
    }, ...children_2))], react.createElement("th", {
        className: "checkboxCol",
    }, ...children_4)), waveAddDelBut], react.createElement("tr", {
        className: "rowHeight",
    }, ...children_6))];
    const bot = [(children_12 = [react.createElement("td", {
        className: "checkboxCol",
    }), react.createElement("td", {})], react.createElement("tr", {
        className: "fullHeight",
    }, ...children_12))];
    const leftCol = concat([top, labelRows, bot]);
    const props_18 = [["style", {
        float: "left",
        height: "100%",
    }]];
    const children_18 = [(children_16 = [react.createElement("tbody", {}, ...leftCol)], react.createElement("table", {
        className: "leftTable",
    }, ...children_16))];
    return react.createElement("div", keyValueList(props_18, 1), ...children_18);
}

function allWaveformsTableElement(model, wSModel, waveformSvgRows, dispatch) {
    let css, props, props_4, children_4, props_2, children_2;
    const pars = wSModel.SimParams;
    const element = new FSharpRef(void 0);
    const allWaveformsHtmlRef = (el) => {
        if (!(el == null)) {
            element.contents = el;
            const scrollPos = el.clientWidth + el.scrollLeft;
            if (!equals(scrollPos, pars.LastScrollPos)) {
                void (new Msg(51, scrollPos));
            }
        }
        const matchValue = element.contents;
        if (matchValue == null) {
            dispatch(new Msg(50, false));
        }
        else {
            const e = matchValue;
            let pattern_matching_result;
            if (model.CheckWaveformScrollPosition) {
                if (!isCursorVisible(wSModel, e.clientWidth, e.scrollLeft)) {
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
                    e.scrollLeft = makeCursorVisiblePos(wSModel, e.clientWidth);
                    dispatch(new Msg(50, false));
                    break;
                }
                case 1: {
                    break;
                }
            }
        }
    };
    const scrollFun = (ev) => {
        let value_1;
        const matchValue_2 = element.contents;
        if (matchValue_2 != null) {
            const e_1 = matchValue_2;
            if (((e_1.scrollWidth - e_1.clientWidth) - e_1.scrollLeft) < 10) {
                let pars$0027;
                let LastClkTime;
                let e2;
                const y_1 = max_1(comparePrimitives, (value_1 = ((pars.LastClkTime + 1) * 0.1), (value_1 >>> 0)), 10);
                e2 = (pars.LastClkTime + y_1);
                LastClkTime = min(comparePrimitives, maxLastClk, e2);
                pars$0027 = (new SimParamsT(pars.WaveViewerRadix, LastClkTime, pars.CursorTime, pars.ClkSvgWidth, pars.DispNames, pars.LastScrollPos));
                dispatch(new Msg(48, [new WSViewT(3), pars$0027]));
                toConsole(printf("working"));
            }
            else {
                toConsole(printf("not working"));
            }
        }
    };
    let waves;
    waves = map((name) => FSharpMap__get_Item(waveformSvgRows.Waves, name), wSModel.SimParams.DispNames);
    const props_6 = [new Prop(1, allWaveformsHtmlRef), new DOMAttr(63, scrollFun), (css = ofArray([new CSSProp(249, maxWavesColWidth(wSModel)), new CSSProp(251, "100%")]), ["style", keyValueList(css, 1)]), new HTMLAttr(65, "wavesTable")];
    const children_6 = [(props = [new HTMLAttr(65, "cursorRectStyle"), cursRectStyle(wSModel.SimParams)], react.createElement("div", keyValueList(props, 1), " ")), (props_4 = [["style", {
        height: "100%",
    }]], (children_4 = [(props_2 = [["style", {
        height: "100%",
    }]], (children_2 = concat([waveformSvgRows.Top, waves, waveformSvgRows.Bottom]), react.createElement("tbody", keyValueList(props_2, 1), ...children_2)))], react.createElement("table", keyValueList(props_4, 1), ...children_4)))];
    return react.createElement("div", keyValueList(props_6, 1), ...children_6);
}

function viewWaveformViewer(compIds, model, netList, wSMod, dispatch) {
    let props, children;
    const patternInput = waveSimViewerRows(compIds, model.Diagram, netList, wSMod, dispatch);
    const tableWaves = patternInput[0];
    const nameColMiddle = patternInput[1];
    const cursValsRows = patternInput[2];
    const props_2 = [["style", {
        height: "calc(100% - 45px)",
        width: "100%",
        overflowY: "auto",
    }]];
    const children_2 = [cursorValuesCol(cursValsRows), (props = [["style", {
        height: "100%",
    }]], (children = [nameLabelsCol(model, netList, wSMod, nameColMiddle, dispatch), allWaveformsTableElement(model, wSMod, tableWaves, dispatch)], react.createElement("div", keyValueList(props, 1), ...children)))];
    return react.createElement("div", keyValueList(props_2, 1), ...children_2);
}

function viewZoomDiv(compIds, model, wSMod, dispatch) {
    const children = [button_1(ofArray([new Option_1(4), new Option_1(18, "zoomButLeft")]), (_arg1) => {
        changeViewerZoom(compIds, false, model, wSMod, dispatch);
    }, "-"), button_1(singleton(new Option_1(18, "zoomButRight")), (_arg2) => {
        changeViewerZoom(compIds, true, model, wSMod, dispatch);
    }, "+")];
    return react.createElement("div", {
        className: "zoomDiv",
    }, ...children);
}

function waveEditorSelectAllRow(model, netList, wSModel) {
    let props_2, children, props, props_4;
    const props_6 = [new HTMLAttr(65, "rowHeight"), ["style", {
        verticalAlign: "middle",
    }]];
    const children_4 = [(props_2 = [new HTMLAttr(65, "wACheckboxCol"), new HTMLAttr(65, "rowHeight"), ["style", {
        verticalAlign: "middle",
    }]], (children = [(props = [new HTMLAttr(159, "checkbox"), new HTMLAttr(65, "check"), new HTMLAttr(62, getAllNetGroups(wSModel).every((netgrp) => isWaveSelected(model, netList, netgrp))), ["style", {
        float: "left",
    }], new DOMAttr(9, (_arg1) => {
        waveAdderSelectAll(model, netList, wSModel);
    })], react.createElement("input", keyValueList(props, 1)))], react.createElement("td", keyValueList(props_2, 1), ...children))), (props_4 = [["style", {
        fontWeight: "bold",
    }]], react.createElement("td", keyValueList(props_4, 1), "Select All"))];
    return react.createElement("tr", keyValueList(props_6, 1), ...children_4);
}

function waveEditorTickBoxAndNameRow(model, netList, wSModel, name, dispatch) {
    let props_2, children, props, arg0, children_4, children_2, s;
    const allPorts = wSModel.AllNets;
    const props_8 = [new HTMLAttr(65, "rowHeight"), ["style", {
        verticalAlign: "middle",
    }]];
    const children_6 = [(props_2 = [new HTMLAttr(65, "wAcheckboxCol"), new HTMLAttr(65, "rowHeight"), ["style", {
        verticalAlign: "middle",
    }]], (children = [(props = [new HTMLAttr(159, "checkbox"), new HTMLAttr(65, "check"), (arg0 = isWaveSelected(model.Diagram, netList, FSharpMap__get_Item(allPorts, name)), (new HTMLAttr(62, arg0))), ["style", {
        float: "left",
    }], new DOMAttr(9, (_arg1) => {
        toggleNetGroupConnsSelect(model.Diagram, wSModel, netList, name);
    })], react.createElement("input", keyValueList(props, 1)))], react.createElement("td", keyValueList(props_2, 1), ...children))), (children_4 = [(children_2 = [(s = removeSuffixFromWaveLabel(name), (s))], react.createElement("label", {}, ...children_2))], react.createElement("td", {}, ...children_4))];
    return react.createElement("tr", keyValueList(props_8, 1), ...children_6);
}

export function sortEditorNameOrder(wsModel) {
    let otherNames;
    otherNames = wsModel.AllWaveNames.filter((name) => {
        const value = contains(name, wsModel.SimParams.DispNames, {
            Equals: (x, y) => (x === y),
            GetHashCode: structuralHash,
        });
        return !value;
    });
    return append(wsModel.SimParams.DispNames, otherNames);
}

function waveEditorTickBoxRows(model, netList, wsModel, dispatch) {
    const editorNameOrder = sortEditorNameOrder(wsModel);
    const array = sortEditorNameOrder(wsModel);
    return map((name) => waveEditorTickBoxAndNameRow(model, netList, wsModel, name, dispatch), array);
}

function waveEditorTickBoxesAndNames(model, netList, wSModel, dispatch) {
    let children_2, children;
    const props_4 = [["style", {
        position: "absolute",
        top: "300px",
    }]];
    const children_4 = [(children_2 = [(children = append([waveEditorSelectAllRow(model.Diagram, netList, wSModel)], waveEditorTickBoxRows(model, netList, wSModel, dispatch)), react.createElement("tbody", {}, ...children))], react.createElement("table", {}, ...children_2))];
    return react.createElement("div", keyValueList(props_4, 1), ...children_4);
}

function waveEditorButtons(model, netList, wSModel, dispatch) {
    const isSelected = (ng) => isWaveSelected(model.Diagram, netList, ng);
    const closeWaveSimButtonAction = (_ev) => {
        dispatch(new Msg(4, new WaveSimModel(void 0, wSModel.SimParams, wSModel.AllWaveNames, wSModel.AllNets, wSModel.DispWaveSVGCache, wSModel.SimDataCache, wSModel.CursorBoxIsEmpty, new WSViewT(0), void 0, wSModel.LastCanvasState)));
        dispatch(new Msg(12, new RightTab(1)));
        dispatch(new Msg(44, true));
        dispatch(new Msg(36));
    };
    let waveEditorViewSimButtonAction;
    let viewableNetGroups;
    let array;
    let tuple;
    const gps = getAllNetGroups(wSModel);
    tuple = standardOrderGroups(gps, wSModel);
    array = tuple[1];
    viewableNetGroups = array.filter(isSelected);
    let lst;
    const matchValue = viewableNetGroups.length | 0;
    lst = ((matchValue === 0) ? singleton(new Option_1(18, "disabled")) : ofArray([new Option_1(0, new Color_IColor(6)), new Option_1(12, showSimulationLoading(wSModel, dispatch)), new Option_1(17, (_arg1) => {
        dispatch(new Msg(36));
        let par$0027;
        const inputRecord = wSModel.SimParams;
        const DispNames = map((port) => {
            const option = tryFindKey((k, v) => equalsSafe(v, port), wSModel.AllNets);
            return defaultArg(option, "name not found");
        }, viewableNetGroups);
        par$0027 = (new SimParamsT(inputRecord.WaveViewerRadix, inputRecord.LastClkTime, inputRecord.CursorTime, inputRecord.ClkSvgWidth, DispNames, inputRecord.LastScrollPos));
        dispatch(new Msg(48, [new WSViewT(3), par$0027]));
    })]));
    waveEditorViewSimButtonAction = cons(new Option_1(16, singleton(["style", {
        marginLeft: "10px",
    }])), lst);
    const cancelButton = button(ofArray([new Option_1(0, new Color_IColor(6)), new Option_1(17, closeWaveSimButtonAction)]), singleton("Close"));
    let actionButtons;
    let matchValue_1;
    const ws_1 = wSModel;
    matchValue_1 = map((name) => FSharpMap__get_Item(ws_1.AllNets, name), ws_1.SimParams.DispNames);
    actionButtons = (((!equalsWith(compareSafe, matchValue_1, null)) ? (matchValue_1.length === 0) : false) ? singleton(button(waveEditorViewSimButtonAction, singleton("View selected"))) : ofArray([cancelButton, button(waveEditorViewSimButtonAction, singleton("View"))]));
    const props = [["style", {
        display: "block",
    }]];
    return react.createElement("div", keyValueList(props, 1), ...actionButtons);
}

function waveEditorView(model, netList, wSMod, dispatch) {
    let props_4, children_2, children;
    return singleton((props_4 = [["style", {
        width: "90%",
        marginLeft: "5%",
        marginTop: "15px",
    }]], (children_2 = [h4(empty())(singleton("Waveform Simulation")), "Add nets to view waveforms by clicking connections in diagram or tick-boxes below.", "Test combinational logic using Simulate tab.", react.createElement("hr", {}), (children = [waveEditorButtons(model, netList, wSMod, dispatch), waveEditorTickBoxesAndNames(model, netList, wSMod, dispatch)], react.createElement("div", {}, ...children))], react.createElement("div", keyValueList(props_4, 1), ...children_2))));
}

function waveformsView(compIds, model, netList, wSMod, dispatch) {
    let props, children;
    return singleton((props = [["style", {
        width: "calc(100% - 10px)",
        height: "100%",
        marginLeft: "0%",
        marginTop: "0px",
        overflowX: "hidden",
    }]], (children = [viewWaveSimButtonsBar(model, wSMod, dispatch), viewWaveformViewer(compIds, model, netList, wSMod, dispatch), viewZoomDiv(compIds, model, wSMod, dispatch)], react.createElement("div", keyValueList(props, 1), ...children))));
}

export function SetSimErrorFeedback(simError, dispatch) {
    let tupledArg;
    if (simError.InDependency == null) {
        const thingsToHighlight = [simError.ComponentsAffected, simError.ConnectionsAffected];
        dispatch((tupledArg = thingsToHighlight, (new Msg(13, tupledArg[0], tupledArg[1]))));
    }
}

function openEditorFromViewer(model, editorState, dispatch) {
    const wsModel = getWSModelOrFail(model, "What? no wsModel in openCloseWaveEditor");
    let wsModel$0027;
    let wsModel_1;
    wsModel_1 = standardWaveformOrderWaveAdder(wsModel);
    wsModel$0027 = setEditorNextView(editorState, wsModel.SimParams, wsModel_1);
    dispatch(new Msg(4, wsModel$0027));
}

export function startWaveSim(compIds, rState_0, rState_1, simData, model, dispatch, _ev) {
    const rState = [rState_0, rState_1];
    const inputWarningPopup = (simData_1, dispatch_1) => {
        let clo1;
        if (!equals(simData_1.Inputs, empty())) {
            let inputs;
            let strings;
            strings = map_1((tupledArg) => {
                const lab = tupledArg[1].fields[0];
                return lab;
            }, simData_1.Inputs);
            inputs = join(",", strings);
            const popup = warningPropsNotification((clo1 = toText(printf("Inputs (%s) will be set to 0.")), clo1(inputs)));
            dispatch_1(new Msg(35, popup));
        }
    };
    let startingWsModel;
    const wsModel = getWSModelOrFail(model, "What? Can\u0027t get wsModel at start of new simulation");
    const netList = getNetList(rState[0], rState[1]);
    const netGroups = netList2NetGroups(netList);
    const nameOf = (ng) => netGroup2Label(compIds, simData.Graph, netList, ng);
    let allPorts;
    let elements;
    elements = mapIndexed((i, ng_1) => {
        let arg10_1, clo1_1, clo2;
        return [(arg10_1 = nameOf(ng_1), (clo1_1 = toText(printf("%s.%d")), clo2 = clo1_1(arg10_1), clo2(i))), ng_1];
    }, netGroups);
    allPorts = ofArray_1(elements);
    const allNames = mapKeys(allPorts);
    let dispNames;
    const pairWithRoot = (name) => [name, removeSuffixFromWaveLabel(name)];
    let allRoots;
    allRoots = map(pairWithRoot, allNames);
    let array_5;
    let array_4;
    let array_3;
    array_3 = map(pairWithRoot, wsModel.SimParams.DispNames);
    array_4 = array_3.filter((tupledArg_1) => {
        const name_1 = tupledArg_1[0];
        const root = tupledArg_1[1];
        return allRoots.some((tupledArg_2) => {
            const name$0027 = tupledArg_2[0];
            const root$0027 = tupledArg_2[1];
            return root$0027 === root;
        });
    });
    array_5 = collect((tupledArg_3) => {
        const root_1 = tupledArg_3[1];
        return allRoots.filter((tupledArg_4) => {
            const name$0027_1 = tupledArg_4[0];
            const root$0027_1 = tupledArg_4[1];
            return root$0027_1 === root_1;
        });
    }, array_4);
    dispNames = map((tuple) => tuple[0], array_5);
    netGroups.forEach((ng_2) => {
        selectNetGrpConns(model.Diagram, ng_2, false);
    });
    let SimParams;
    const inputRecord = wsModel.SimParams;
    SimParams = (new SimParamsT(inputRecord.WaveViewerRadix, inputRecord.LastClkTime, inputRecord.CursorTime, inputRecord.ClkSvgWidth, dispNames, inputRecord.LastScrollPos));
    const SimDataCache = [simData];
    startingWsModel = (new WaveSimModel(simData, SimParams, allNames, allPorts, wsModel.DispWaveSVGCache, SimDataCache, wsModel.CursorBoxIsEmpty, new WSViewT(2), void 0, rState));
    dispatch(new Msg(4, startingWsModel));
    dispatch(new Msg(40, minViewerWidth));
    dispatch(new Msg(49, rState));
    dispatch(new Msg(44, false));
    inputWarningPopup(simData, dispatch);
    dispatch(new Msg(12, new RightTab(3)));
}

export function WaveformButtonFunc(compIds, model, dispatch) {
    let simulationButton;
    const matchValue = currWaveSimModel(model);
    if (matchValue != null) {
        const wSModel = matchValue;
        const matchValue_2 = [wSModel.WSViewState, model.WaveSimulationIsOutOfDate, makeSimData(model)];
        let pattern_matching_result, rState, simData, err;
        if (matchValue_2[0].tag === 0) {
            if (matchValue_2[2] != null) {
                const copyOfStruct = matchValue_2[2][0];
                if (copyOfStruct.tag === 1) {
                    pattern_matching_result = 1;
                    err = copyOfStruct.fields[0];
                }
                else {
                    pattern_matching_result = 0;
                    rState = matchValue_2[2][1];
                    simData = copyOfStruct.fields[0];
                }
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else if (matchValue_2[1]) {
            if (matchValue_2[2] != null) {
                const copyOfStruct_1 = matchValue_2[2][0];
                if (copyOfStruct_1.tag === 1) {
                    pattern_matching_result = 1;
                    err = copyOfStruct_1.fields[0];
                }
                else {
                    pattern_matching_result = 0;
                    rState = matchValue_2[2][1];
                    simData = copyOfStruct_1.fields[0];
                }
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else {
            pattern_matching_result = 2;
        }
        switch (pattern_matching_result) {
            case 0: {
                const isClocked = hasSynchronousComponents(simData.Graph);
                simulationButton = (isClocked ? ((children_1) => button(ofArray([new Option_1(0, new Color_IColor(6)), new Option_1(17, (_ev) => {
                    startWaveSim(compIds, rState[0], rState[1], simData, model, dispatch, _ev);
                })]), children_1)) : ((children_2) => button(singleton(new Option_1(17, (_arg2) => {
                    const popup = errorPropsNotification("Combinational logic does not have waveforms");
                    dispatch(new Msg(35, popup));
                })), children_2)));
                break;
            }
            case 1: {
                simulationButton = ((children_3) => button(ofArray([new Option_1(0, new Color_IColor(7)), new Option_1(17, (_arg3) => {
                    dispatch(new Msg(5, err));
                    dispatch(new Msg(12, new RightTab(3)));
                    SetSimErrorFeedback(err, dispatch);
                })]), children_3));
                break;
            }
            case 2: {
                simulationButton = ((children_4) => button(singleton(new Option_1(17, (_arg4) => {
                    dispatch(new Msg(12, new RightTab(3)));
                })), children_4));
                break;
            }
        }
    }
    else {
        if (model.CurrentProj == null) {
        }
        else {
            initFileWS(model, dispatch);
        }
        simulationButton = ((children) => button(singleton(new Option_1(17, (_arg1) => {
            dispatch(new Msg(12, new RightTab(3)));
        })), children));
    }
    return simulationButton(singleton("Waveforms \u003e\u003e"));
}

export function viewWaveSim(model, dispatch) {
    let props_2, children_2;
    const compIds = getComponentIds(model);
    const matchValue = [currWaveSimModel(model), model.WaveSim[1]];
    if (matchValue[0] == null) {
        initFileWS(model, dispatch);
        return empty();
    }
    else if (matchValue[1] != null) {
        const simError = matchValue[1];
        return singleton((props_2 = [["style", {
            width: "90%",
            marginLeft: "5%",
            marginTop: "15px",
        }]], (children_2 = [viewSimulationError(simError), button_1(singleton(new Option_1(0, new Color_IColor(8))), (_arg1) => {
            let arg0;
            dispatch(new Msg(29));
            dispatch(new Msg(13, empty(), empty()));
            dispatch((arg0 = (new JSDiagramMsg$2(3, void 0)), new Msg(0, arg0)));
            dispatch(new Msg(5, void 0));
            const matchValue_2 = getCurrFileWSMod(model);
            if (matchValue_2 != null) {
                const ws = matchValue_2;
                dispatch(new Msg(4, new WaveSimModel(void 0, ws.SimParams, ws.AllWaveNames, ws.AllNets, ws.DispWaveSVGCache, ws.SimDataCache, ws.CursorBoxIsEmpty, ws.WSViewState, ws.WSTransition, ws.LastCanvasState)));
            }
            dispatch(new Msg(12, new RightTab(1)));
        }, "Ok")], react.createElement("div", keyValueList(props_2, 1), ...children_2))));
    }
    else {
        const wSModel = matchValue[0];
        let netList;
        const tupledArg = defaultArg(model.LastSimulatedCanvasState, [empty(), empty()]);
        netList = getNetList(tupledArg[0], tupledArg[1]);
        const matchValue_1 = [wSModel.WSViewState, wSModel.WSTransition];
        if (matchValue_1[0].tag === 2) {
            return waveEditorView(model, netList, wSModel, dispatch);
        }
        else if (matchValue_1[0].tag === 3) {
            return waveformsView(compIds, model, netList, wSModel, dispatch);
        }
        else {
            const prog = matchValue_1[1];
            const clo1 = toConsole(printf("ViewWaveSim should not be called when WaveSimEditorOpen =%A, inProgress = %A"));
            const clo2 = clo1(wSModel.WSViewState);
            clo2(prog);
            return singleton(react.createElement("div", {}));
        }
    }
}

