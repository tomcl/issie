import { Draw2dWrapper__CanvasReactElement, DisplayModeType, Draw2dWrapper_$ctor, Draw2dWrapper__Redo, Draw2dWrapper__Undo, Draw2dWrapper__LoadConnection, Draw2dWrapper__CreateComponent, Draw2dWrapper__GetSelected } from "../Draw2dWrapper/Draw2dWrapper.fs.js";
import { extractComponent, extractState } from "../Interface/Extractor.fs.js";
import { WSViewT, SimParamsT, getCurrFileWSMod, Model, DragMode, TopMenu, Notifications, PopupDialogData, RightTab, AsyncTasksT, AutoSaveT, Msg } from "./ModelType.fs.js";
import { min, comparePrimitives, max, equalsSafe, equals } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { assertThat } from "../../Common/Helpers.fs.js";
import { empty as empty_1, singleton, ofArray, ofSeq, concat, map, append, map2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { empty, ofList, FSharpMap__TryFind } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Connection, Port } from "../../Common/CommonTypes.fs.js";
import { singleton as singleton_1, append as append_1, delay } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { Option, button } from "../.fable/Fulma.2.9.0/Elements/Button.fs.js";
import { rightSectionStyle, canvasVisibleStyle, minEditorWidth, minViewerWidth, rightSectionWidthViewerDefault, canvasSmallMenuStyle, canvasSmallButtonStyle } from "./Style.fs.js";
import { CSSProp, DOMAttr } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import * as react from "react";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";
import { minValue } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Date.js";
import { h4 } from "../.fable/Fulma.2.9.0/Elements/Heading.fs.js";
import { viewSelectedComponent } from "./SelectedComponentView.fs.js";
import { viewSimulation } from "./SimulationView.fs.js";
import { WaveformButtonFunc, maxUsedViewerWidth, viewWaveSim } from "./WaveformSimulationView.fs.js";
import { firstTip, scrollData, viewCatalogue } from "./CatalogueView.fs.js";
import { tipStr, traceIf } from "../Interface/JSHelpers.fs.js";
import { viewTopMenu, viewNoProjectMenu, currWaveSimModel } from "./FileMenuView.fs.js";
import { modal } from "../.fable/Fulma.2.9.0/Components/Modal.fs.js";
import { viewNotifications, viewPopup } from "./PopupView.fs.js";
import { fileMenuViewActions } from "./WaveSimHelpers.fs.js";
import { Tab_Option, tab, Option as Option_1, tabs } from "../.fable/Fulma.2.9.0/Components/Tabs.fs.js";

function copyAction(model, dispatch) {
    let arg0;
    const matchValue = Draw2dWrapper__GetSelected(model.Diagram);
    if (matchValue != null) {
        const jsState = matchValue;
        dispatch((arg0 = extractState(jsState[0], jsState[1]), (new Msg(15, arg0))));
    }
}

function mapPorts(oldComp, newComp) {
    const mapPort = (oldPort, newPort) => {
        const cond = equals(oldPort.PortNumber, newPort.PortNumber);
        assertThat(cond, "cloned components have different port numbers");
        const cond_1 = equalsSafe(oldPort.PortType, newPort.PortType);
        assertThat(cond_1, "cloned components have different port types");
        return [oldPort.Id, newPort];
    };
    assertThat(oldComp.Id !== newComp.Id, "cloned component has same id as old one");
    assertThat(equalsSafe(oldComp.Type, newComp.Type), "cloned components have different types");
    assertThat(oldComp.Label === newComp.Label, "cloned components have different labels");
    let inputs;
    inputs = map2(mapPort, oldComp.InputPorts, newComp.InputPorts);
    let outputs;
    outputs = map2(mapPort, oldComp.OutputPorts, newComp.OutputPorts);
    return append(inputs, outputs);
}

function mapToNewConnection(portMappings, oldConnection) {
    let inputRecord, inputRecord_1;
    const mapGet = (pId) => {
        const matchValue = FSharpMap__TryFind(portMappings, pId);
        if (matchValue != null) {
            const port = matchValue;
            return port;
        }
        else {
            const clo1 = toFail(printf("what? Could not find port Id %s while cloning"));
            return clo1(pId);
        }
    };
    return new Connection("", (inputRecord = mapGet(oldConnection.Source.Id), new Port(inputRecord.Id, void 0, inputRecord.PortType, inputRecord.HostId)), (inputRecord_1 = mapGet(oldConnection.Target.Id), new Port(inputRecord_1.Id, void 0, inputRecord_1.PortType, inputRecord_1.HostId)), (map((tupledArg) => {
        const x = tupledArg[0];
        const y = tupledArg[1];
        return [x + 30, y + 30];
    }, oldConnection.Vertices)));
}

export function pasteAction(model) {
    const patternInput = model.Clipboard;
    const oldConnections = patternInput[1];
    const oldComponents = patternInput[0];
    let newComponents;
    newComponents = map((arg) => {
        let jsComponent;
        const comp = arg;
        let matchValue;
        const arg20 = (comp.X + 30) | 0;
        const arg30 = (comp.Y + 30) | 0;
        matchValue = Draw2dWrapper__CreateComponent(model.Diagram, comp.Type, comp.Label, arg20, arg30);
        if (matchValue != null) {
            const jsComp = matchValue;
            jsComponent = jsComp;
        }
        else {
            const clo1 = toFail(printf("what? Could not paste component %A"));
            jsComponent = clo1(comp);
        }
        return extractComponent(jsComponent);
    }, oldComponents);
    let portMappings;
    let elements;
    let lists;
    lists = map2(mapPorts, oldComponents, newComponents);
    elements = concat(lists);
    portMappings = ofList(elements);
    let value;
    value = map((arg_1) => {
        let arg10_2;
        arg10_2 = mapToNewConnection(portMappings, arg_1);
        Draw2dWrapper__LoadConnection(model.Diagram, false, arg10_2);
    }, oldConnections);
    void value;
}

export function viewOnDiagramButtons(model, dispatch) {
    const children = ofSeq(delay(() => {
        const canvasBut = (func, label) => button(singleton(new Option(16, ofArray([canvasSmallButtonStyle, new DOMAttr(40, func)]))), singleton(label));
        return append_1(singleton_1(canvasBut((_arg1) => {
            Draw2dWrapper__Undo(model.Diagram);
        }, "\u003c undo")), delay(() => append_1(singleton_1(canvasBut((_arg2) => {
            Draw2dWrapper__Redo(model.Diagram);
        }, "redo \u003e")), delay(() => append_1(singleton_1(canvasBut((_arg3) => {
            copyAction(model, dispatch);
        }, "copy")), delay(() => singleton_1(canvasBut((_arg4) => {
            pasteAction(model);
        }, "paste"))))))));
    }));
    return react.createElement("div", keyValueList([canvasSmallMenuStyle], 1), ...children);
}

export const initActivity = (() => {
    const LastSavedCanvasState = empty();
    const LastAutoSaveCheck = minValue();
    return new AsyncTasksT(new AutoSaveT(2), empty(), LastAutoSaveCheck, LastSavedCanvasState, false);
})();

export function init() {
    const Diagram = Draw2dWrapper_$ctor();
    const WaveSim = [empty(), void 0];
    return new Model(initActivity, WaveSim, Diagram, false, true, void 0, [empty_1(), empty_1()], [empty_1(), empty_1()], [empty_1(), empty_1()], 1, void 0, void 0, new RightTab(1), [[empty_1(), empty_1()], empty_1()], [empty_1(), empty_1()], void 0, false, void 0, void 0, new PopupDialogData(void 0, void 0, void 0, void 0, void 0), new Notifications(void 0, void 0, void 0, void 0, void 0, void 0), new TopMenu(0), new DragMode(1), rightSectionWidthViewerDefault, void 0, false, false);
}

export function makeSelectionChangeMsg(model, dispatch, ev) {
    dispatch(new Msg(43));
}

function viewRightTab(model, dispatch) {
    let props;
    const matchValue = model.RightPaneTabVisible;
    switch (matchValue.tag) {
        case 0: {
            const props_4 = [["style", {
                width: "90%",
                marginLeft: "5%",
                marginTop: "15px",
            }]];
            const children_4 = [h4(empty_1())(singleton("Component properties")), viewSelectedComponent(model, dispatch)];
            return react.createElement("div", keyValueList(props_4, 1), ...children_4);
        }
        case 2: {
            const props_6 = [["style", {
                width: "90%",
                marginLeft: "5%",
                marginTop: "15px",
            }]];
            const children_6 = [h4(empty_1())(singleton("Simulation")), viewSimulation(model, dispatch)];
            return react.createElement("div", keyValueList(props_6, 1), ...children_6);
        }
        case 3: {
            const props_8 = [["style", {
                width: "100%",
                height: "calc(100% - 48px)",
                marginTop: "15px",
            }]];
            const children_8 = viewWaveSim(model, dispatch);
            return react.createElement("div", keyValueList(props_8, 1), ...children_8);
        }
        default: {
            const props_2 = [["style", {
                width: "90%",
                marginLeft: "5%",
                marginTop: "15px",
            }]];
            const children_2 = [h4(empty_1())(singleton("Catalogue")), (props = [["style", {
                marginBottom: "15px",
            }]], react.createElement("div", keyValueList(props, 1), "Click on a component to add it to the diagram. Hover on components for details.")), viewCatalogue(model, dispatch)];
            return react.createElement("div", keyValueList(props_2, 1), ...children_2);
        }
    }
}

export function dividerbar(model, dispatch) {
    let css;
    const isDraggable = equalsSafe(model.RightPaneTabVisible, new RightTab(3));
    const variableStyle = isDraggable ? ofArray([new CSSProp(21, "grey"), new CSSProp(123, "grab"), new CSSProp(395, "10px")]) : ofArray([new CSSProp(21, "lightgrey"), new CSSProp(395, "2px")]);
    const commonStyle = ofArray([new CSSProp(189, "100%"), new CSSProp(144, "left")]);
    const props = [(css = append(commonStyle, variableStyle), (["style", keyValueList(css, 1)])), new DOMAttr(51, (ev) => {
        const model_1 = model;
        const dispatch_1 = dispatch;
        const ev_1 = ev;
        makeSelectionChangeMsg(model_1, dispatch_1, ev_1);
        const matchValue = [true, model_1.DividerDragMode];
        let pattern_matching_result;
        if (matchValue[0]) {
            if (matchValue[1].tag === 1) {
                pattern_matching_result = 0;
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else if (matchValue[1].tag === 0) {
            pattern_matching_result = 1;
        }
        else {
            pattern_matching_result = 2;
        }
        switch (pattern_matching_result) {
            case 0: {
                dispatch_1(new Msg(39, new DragMode(0, ~(~ev_1.clientX))));
                break;
            }
            case 1: {
                dispatch_1(new Msg(39, new DragMode(1)));
                break;
            }
            case 2: {
                break;
            }
        }
    })];
    return react.createElement("div", keyValueList(props, 1));
}

export function displayView(model, dispatch) {
    let arg10, arg0_1, props_12, children_12, props_10, children_10;
    traceIf("view", () => printf("View Function..."));
    const patternInput = [~(~self.innerWidth), ~(~self.innerHeight)];
    const windowY = patternInput[1] | 0;
    const windowX = patternInput[0] | 0;
    const sd = scrollData(model);
    const x$0027 = (sd.SheetLeft + sd.SheetX) | 0;
    const y$0027 = (sd.SheetTop + sd.SheetY) | 0;
    const wsModelOpt = getCurrFileWSMod(model);
    const props_14 = [new DOMAttr(57, (ev) => {
        const model_1 = model;
        const dispatch_1 = dispatch;
        const ev_1 = ev;
        makeSelectionChangeMsg(model_1, dispatch_1, ev_1);
        const matchValue = [false, model_1.DividerDragMode];
        let pattern_matching_result;
        if (matchValue[0]) {
            if (matchValue[1].tag === 1) {
                pattern_matching_result = 0;
            }
            else {
                pattern_matching_result = 2;
            }
        }
        else if (matchValue[1].tag === 0) {
            pattern_matching_result = 1;
        }
        else {
            pattern_matching_result = 2;
        }
        switch (pattern_matching_result) {
            case 0: {
                dispatch_1(new Msg(39, new DragMode(0, ~(~ev_1.clientX))));
                break;
            }
            case 1: {
                dispatch_1(new Msg(39, new DragMode(1)));
                break;
            }
        }
        dispatch(new Msg(43));
    }), new DOMAttr(51, (ev_2) => {
        makeSelectionChangeMsg(model, dispatch, ev_2);
    }), new DOMAttr(54, (ev_3) => {
        let wSMod;
        if (ev_3.buttons === 1) {
            dispatch(new Msg(43));
        }
        const matchValue_1 = [model.DividerDragMode, ev_3.buttons];
        if (matchValue_1[0].tag === 1) {
        }
        else if (matchValue_1[1] === 1) {
            const pos = matchValue_1[0].fields[0] | 0;
            const newWidth = ((model.WaveSimViewerWidth - (~(~ev_3.clientX))) + pos) | 0;
            let w;
            let e2_1;
            e2_1 = max(comparePrimitives, minViewerWidth, newWidth);
            const e1 = (windowX - minEditorWidth) | 0;
            w = min(comparePrimitives, e1, e2_1);
            dispatch(new Msg(40, w));
            const matchValue_2 = currWaveSimModel(model);
            let pattern_matching_result_1;
            if (matchValue_2 != null) {
                if (wSMod = matchValue_2, (w > maxUsedViewerWidth(wSMod)) ? equalsSafe(wSMod.WSViewState, new WSViewT(3)) : false) {
                    pattern_matching_result_1 = 0;
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
                    if (wsModelOpt != null) {
                        const ws = wsModelOpt;
                        let simProgressState;
                        const inputRecord = ws.SimParams;
                        const LastClkTime = ws.SimParams.LastClkTime + 10;
                        simProgressState = (new SimParamsT(inputRecord.WaveViewerRadix, LastClkTime, inputRecord.CursorTime, inputRecord.ClkSvgWidth, inputRecord.DispNames, inputRecord.LastScrollPos));
                        dispatch(new Msg(48, [new WSViewT(3), simProgressState]));
                    }
                    break;
                }
            }
            dispatch(new Msg(39, new DragMode(0, ~(~ev_3.clientX))));
        }
        else {
            dispatch(new Msg(39, new DragMode(1)));
        }
    }), ["style", {
        borderTop: "2px solid lightgray",
        borderBottom: "2px solid lightgray",
    }]];
    const children_14 = [viewNoProjectMenu(model, dispatch), modal(empty_1(), singleton("TEST")), viewPopup(model), viewTopMenu(model, (model_2, dispatch_2) => {
        fileMenuViewActions(model_2, dispatch_2);
    }, WaveformButtonFunc, dispatch), (arg10 = (arg0_1 = canvasVisibleStyle(model), (new DisplayModeType(0, arg0_1))), (Draw2dWrapper__CanvasReactElement(model.Diagram, (arg) => {
        dispatch((new Msg(0, arg)));
    }, arg10))), viewNotifications(model, dispatch), viewOnDiagramButtons(model, dispatch), (props_12 = [rightSectionStyle(model)], (children_12 = [dividerbar(model, dispatch), (props_10 = [["style", {
        height: "100%",
    }]], (children_10 = [tabs(ofArray([new Option_1(6), new Option_1(3), new Option_1(7, "rightSectionTabs"), new Option_1(8, singleton(["style", {
        margin: 0,
    }]))]), ofSeq(delay(() => {
        let children;
        return append_1(singleton_1(tab(singleton(new Tab_Option(0, equalsSafe(model.RightPaneTabVisible, new RightTab(1)))), singleton((children = [tipStr("bottom", "Catalogue", "List of components and custom components from other design sheets to add to this sheet")], react.createElement("a", {
            onClick: (_arg2) => {
                firstTip(true, true);
                dispatch(new Msg(12, new RightTab(1)));
            },
        }, ...children))))), delay(() => {
            let children_2;
            return append_1(singleton_1(tab(singleton(new Tab_Option(0, equalsSafe(model.RightPaneTabVisible, new RightTab(0)))), singleton((children_2 = [tipStr("bottom", "Properties", "View or change component name, width, etc")], react.createElement("a", {
                onClick: (_arg3) => {
                    dispatch(new Msg(12, new RightTab(0)));
                },
            }, ...children_2))))), delay(() => {
                let children_4;
                return append_1(singleton_1(tab(singleton(new Tab_Option(0, equalsSafe(model.RightPaneTabVisible, new RightTab(2)))), singleton((children_4 = [tipStr("bottom", "Simulation", "Simple simulation for combinational logic which allows inputs to be changed manually")], react.createElement("a", {
                    onClick: (_arg4) => {
                        dispatch(new Msg(12, new RightTab(2)));
                    },
                }, ...children_4))))), delay(() => {
                    const matchValue_3 = currWaveSimModel(model);
                    let pattern_matching_result_2;
                    if (matchValue_3 != null) {
                        if (matchValue_3.WSViewState.tag === 0) {
                            pattern_matching_result_2 = 0;
                        }
                        else {
                            pattern_matching_result_2 = 1;
                        }
                    }
                    else {
                        pattern_matching_result_2 = 1;
                    }
                    switch (pattern_matching_result_2) {
                        case 0: {
                            return singleton_1(react.createElement("div", {}));
                        }
                        case 1: {
                            return singleton_1(tab(singleton(new Tab_Option(0, equalsSafe(model.RightPaneTabVisible, new RightTab(3)))), singleton(react.createElement("a", {
                                onClick: (_arg5) => {
                                    dispatch(new Msg(12, new RightTab(3)));
                                },
                            }, "WaveSim"))));
                        }
                    }
                }));
            }));
        }));
    }))), viewRightTab(model, dispatch)], react.createElement("div", keyValueList(props_10, 1), ...children_10)))], react.createElement("div", keyValueList(props_12, 1), ...children_12)))];
    return react.createElement("div", keyValueList(props_14, 1), ...children_14);
}

