import * as draw2d_fsharp_interface from "./draw2d_fsharp_interface.js";
import { JSDiagramMsg$2, JSDiagramMsg$2$reflection, draw2dCanvasHeight, draw2dCanvasWidth } from "../../Common/CommonTypes.fs.js";
import { jsListToFSharpList, getColorString, assertNotNull, debugLevel, fshaprListToJsList, getFailIfNull } from "../Interface/JSHelpers.fs.js";
import { contains, filter, distinct, append, cons, ofArray, empty, item, map, ofSeq, length, singleton } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { toText, printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { rangeNumber } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Seq.js";
import { toList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { Result_Bind, FSharpResult$2 } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Choice.js";
import { hashSafe, equalsSafe, createObj, createAtom } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { Record, Union } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { HTMLAttr, HTMLAttr$reflection } from "../.fable/Fable.React.5.4.0/Fable.React.Props.fs.js";
import { class_type, record_type, lambda_type, unit_type, union_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { JSCanvas$reflection } from "../Interface/JSTypes.fs.js";
import { JSComponent$reflection } from "../../Simulator/SimulatorTypes.fs.js";
import { PureComponent } from "react";
import * as react from "react";
import { keyValueList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/MapUtil.js";

function createAndInitialiseCanvas(id) {
    let canvas;
    canvas = draw2d_fsharp_interface.createCanvas(id, draw2dCanvasWidth, draw2dCanvasHeight);
    draw2d_fsharp_interface.initialiseCanvas(canvas);
    return canvas;
}

function setPorts(ports, jsPorts) {
    const jsPortsLen = getFailIfNull(jsPorts, singleton("length")) | 0;
    if (jsPortsLen !== length(ports)) {
        toFail(printf("what? setPort called with mismatching number of ports"));
    }
    let value;
    const list = ofSeq(rangeNumber(0, 1, length(ports) - 1));
    value = map((i) => {
        const jsPort = jsPorts[i];
        const port = item(i, ports);
        draw2d_fsharp_interface.setPortId(jsPort, port.Id);
    }, list);
    void value;
}

function createComponent(canvas, dispatch, maybeId, componentType, label, maybeInputPorts, maybeOutputPorts, x, y) {
    let comp;
    switch (componentType.tag) {
        case 1: {
            const w_1 = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalOutput(x, y, w_1);
            break;
        }
        case 2: {
            comp = draw2d_fsharp_interface.createDigitalLabel(x, y);
            break;
        }
        case 3: {
            const w_2 = componentType.fields[0] | 0;
            const lsb = componentType.fields[1] | 0;
            comp = draw2d_fsharp_interface.createDigitalBusSelection(x, y, w_2, lsb);
            break;
        }
        case 4: {
            const w_3 = componentType.fields[0] | 0;
            const c = componentType.fields[1] | 0;
            comp = draw2d_fsharp_interface.createDigitalConstant(x, y, w_3, c);
            break;
        }
        case 5: {
            comp = draw2d_fsharp_interface.createDigitalNot(x, y);
            break;
        }
        case 6: {
            comp = draw2d_fsharp_interface.createDigitalAnd(x, y);
            break;
        }
        case 7: {
            comp = draw2d_fsharp_interface.createDigitalOr(x, y);
            break;
        }
        case 8: {
            comp = draw2d_fsharp_interface.createDigitalXor(x, y);
            break;
        }
        case 9: {
            comp = draw2d_fsharp_interface.createDigitalNand(x, y);
            break;
        }
        case 10: {
            comp = draw2d_fsharp_interface.createDigitalNor(x, y);
            break;
        }
        case 11: {
            comp = draw2d_fsharp_interface.createDigitalXnor(x, y);
            break;
        }
        case 12: {
            comp = draw2d_fsharp_interface.createDigitalDecode4(x, y);
            break;
        }
        case 13: {
            comp = draw2d_fsharp_interface.createDigitalMux2(x, y);
            break;
        }
        case 14: {
            comp = draw2d_fsharp_interface.createDigitalDemux2(x, y);
            break;
        }
        case 15: {
            const numberOfBits = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalNbitsAdder(x, y, numberOfBits);
            break;
        }
        case 16: {
            const custom = componentType.fields[0];
            const arg30_2 = fshaprListToJsList(custom.InputLabels);
            const arg40 = fshaprListToJsList(custom.OutputLabels);
            comp = draw2d_fsharp_interface.createDigitalCustom(x, y, custom.Name, arg30_2, arg40);
            break;
        }
        case 17: {
            comp = draw2d_fsharp_interface.createDigitalMergeWires(x, y);
            break;
        }
        case 18: {
            const topWireWidth = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalSplitWire(x, y, topWireWidth);
            break;
        }
        case 19: {
            comp = draw2d_fsharp_interface.createDigitalDFF(x, y);
            break;
        }
        case 20: {
            comp = draw2d_fsharp_interface.createDigitalDFFE(x, y);
            break;
        }
        case 21: {
            const width = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalRegister(x, y, width);
            break;
        }
        case 22: {
            const width_1 = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalRegisterE(x, y, width_1);
            break;
        }
        case 23: {
            const mem = componentType.fields[0];
            const arg40_1 = fshaprListToJsList((toList(mem.Data)));
            comp = draw2d_fsharp_interface.createDigitalAsyncROM(x, y, mem.AddressWidth, mem.WordWidth, arg40_1);
            break;
        }
        case 24: {
            const mem_1 = componentType.fields[0];
            const arg40_2 = fshaprListToJsList((toList(mem_1.Data)));
            comp = draw2d_fsharp_interface.createDigitalROM(x, y, mem_1.AddressWidth, mem_1.WordWidth, arg40_2);
            break;
        }
        case 25: {
            const mem_2 = componentType.fields[0];
            const arg40_3 = fshaprListToJsList((toList(mem_2.Data)));
            comp = draw2d_fsharp_interface.createDigitalRAM(x, y, mem_2.AddressWidth, mem_2.WordWidth, arg40_3);
            break;
        }
        default: {
            const w = componentType.fields[0] | 0;
            comp = draw2d_fsharp_interface.createDigitalInput(x, y, w);
        }
    }
    draw2d_fsharp_interface.addComponentLabel(comp, label);
    if (maybeId != null) {
        const id = maybeId;
        draw2d_fsharp_interface.setComponentId(comp, id);
    }
    const matchValue = [maybeInputPorts, maybeOutputPorts];
    let pattern_matching_result, ip, op;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 1;
            ip = matchValue[0];
            op = matchValue[1];
        }
        else {
            pattern_matching_result = 2;
        }
    }
    else if (matchValue[1] == null) {
        pattern_matching_result = 0;
    }
    else {
        pattern_matching_result = 2;
    }
    switch (pattern_matching_result) {
        case 1: {
            setPorts(ip, draw2d_fsharp_interface.getInputPorts(comp));
            setPorts(op, draw2d_fsharp_interface.getOutputPorts(comp));
            break;
        }
        case 2: {
            toFail(printf("what? createComponent called with incomplete of input/output ports"));
            break;
        }
    }
    draw2d_fsharp_interface.installSelectionPolicy(comp);
    draw2d_fsharp_interface.addComponentToCanvas(canvas, comp);
    return comp;
}

function trySetConnectionVertices(conn, vertices, jsList) {
    try {
        let arg0;
        arg0 = draw2d_fsharp_interface.setConnectionVertices(conn, jsList);
        return new FSharpResult$2(0, void 0);
    }
    catch (e) {
        let arg0_1;
        const clo1 = toText(printf("Draw2d Error in trySetConnection vertices:\n%A\nFailed to create connection."));
        arg0_1 = clo1(vertices);
        return new FSharpResult$2(1, arg0_1);
    }
}

export const createConnectionError = createAtom(empty());

export const createComponentError = createAtom(empty());

function createConnection(canvas, maybeId, vertices, source, target) {
    let conn;
    conn = draw2d_fsharp_interface.createDigitalConnection(source, target);
    if (maybeId != null) {
        const id = maybeId;
        draw2d_fsharp_interface.setConnectionId(conn, id);
    }
    let r_1;
    let result;
    let jsList;
    let list_1;
    list_1 = map((tupledArg) => {
        const x = tupledArg[0];
        const y = tupledArg[1];
        return createObj(ofArray([["x", x], ["y", y]]));
    }, vertices);
    jsList = fshaprListToJsList(list_1);
    result = trySetConnectionVertices(conn, vertices, jsList);
    r_1 = Result_Bind(() => {
        try {
            return new FSharpResult$2(0, (draw2d_fsharp_interface.addConnectionToCanvas(canvas, conn)));
        }
        catch (e) {
            return new FSharpResult$2(1, "Draw2D error: can\u0027t add connection to canvas!");
        }
    }, result);
    if (r_1.tag === 1) {
        const msg = r_1.fields[0];
        createConnectionError(cons(msg, createConnectionError()), true);
    }
    else {
        draw2d_fsharp_interface.addConnectionToCanvas(canvas, conn);
    }
}

function editComponentLabel(canvas, id, newLabel) {
    let jsComponent;
    jsComponent = draw2d_fsharp_interface.getComponentById(canvas, id);
    if ((jsComponent == null || jsComponent === 'undefined')) {
        const clo1 = toFail(printf("what? could not find diagram component with Id: %s"));
        clo1(id);
    }
    else {
        draw2d_fsharp_interface.setComponentLabel(jsComponent, newLabel);
    }
}

export class DisplayModeType extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["DispMode"];
    }
}

export function DisplayModeType$reflection() {
    return union_type("Draw2dWrapper.DisplayModeType", [], DisplayModeType, () => [[["Item", HTMLAttr$reflection()]]]);
}

class Draw2dReactProps extends Record {
    constructor(Dispatch, CanvasDisplayMode) {
        super();
        this.Dispatch = Dispatch;
        this.CanvasDisplayMode = CanvasDisplayMode;
    }
}

function Draw2dReactProps$reflection() {
    return record_type("Draw2dWrapper.Draw2dReactProps", [], Draw2dReactProps, () => [["Dispatch", lambda_type(JSDiagramMsg$2$reflection(JSCanvas$reflection(), JSComponent$reflection()), unit_type)], ["CanvasDisplayMode", DisplayModeType$reflection()]]);
}

class Draw2dReact extends PureComponent {
    constructor(initialProps) {
        super(initialProps);
        this.divId = "Draw2dCanvas";
    }
    componentDidMount() {
        let arg0;
        const this$ = this;
        console.log("Mounting Draw2dReact component");
        (this$.props).Dispatch((arg0 = createAndInitialiseCanvas(this$.divId), (new JSDiagramMsg$2(0, arg0))));
    }
    render() {
        const this$ = this;
        let style;
        const matchValue = (this$.props).CanvasDisplayMode;
        const s = matchValue.fields[0];
        style = s;
        return react.createElement("div", keyValueList([new HTMLAttr(99, this$.divId), style], 1));
    }
}

function Draw2dReact$reflection() {
    return class_type("Draw2dWrapper.Draw2dReact", void 0, Draw2dReact, class_type("Fable.React.PureStatelessComponent`1", [Draw2dReactProps$reflection()]));
}

function Draw2dReact_$ctor_Z5A69CE29(initialProps) {
    return new Draw2dReact(initialProps);
}

export class Draw2dWrapper {
    constructor() {
        this.canvas = (void 0);
        this.dispatch = (void 0);
    }
}

export function Draw2dWrapper$reflection() {
    return class_type("Draw2dWrapper.Draw2dWrapper", void 0, Draw2dWrapper);
}

export function Draw2dWrapper_$ctor() {
    return new Draw2dWrapper();
}

export function Draw2dWrapper__CanvasReactElement(this$, jsDiagramMsgDispatch, displayMode) {
    const reload = () => {
        this$.dispatch = jsDiagramMsgDispatch;
        draw2d_fsharp_interface.setDispatchMessages(() => {
            jsDiagramMsgDispatch((new JSDiagramMsg$2(3, void 0)));
        }, (arg_1) => {
            jsDiagramMsgDispatch((new JSDiagramMsg$2(1, arg_1)));
        }, () => {
            jsDiagramMsgDispatch((new JSDiagramMsg$2(2, void 0)));
        }, (arg_3) => {
            jsDiagramMsgDispatch((new JSDiagramMsg$2(4, arg_3)));
        });
    };
    const matchValue = this$.dispatch;
    if (matchValue != null) {
        if (debugLevel() !== 0) {
            reload();
        }
    }
    else {
        reload();
    }
    const props = new Draw2dReactProps(jsDiagramMsgDispatch, displayMode);
    let comp;
    comp = Draw2dReact;
    return react.createElement(comp, props);
}

export function Draw2dWrapper__InitCanvas_Z25B2DB98(this$, newCanvas) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        this$.canvas = newCanvas;
    }
    else {
        this$.canvas = newCanvas;
    }
}

export function Draw2dWrapper__printCanvas_7911749C(this$, handler) {
    Draw2dWrapper__tryActionWithCanvas(this$, "PrintCanvas", (canvas) => {
        const arg10 = handler;
        draw2d_fsharp_interface.printCanvas(canvas, arg10);
    });
}

export function Draw2dWrapper__ClearCanvas(this$) {
    Draw2dWrapper__tryActionWithCanvas(this$, "ClearCanvas", (arg00) => {
        draw2d_fsharp_interface.clearCanvas(arg00);
    });
}

export function Draw2dWrapper__GetScrollArea(this$) {
    let Width;
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        const a = draw2d_fsharp_interface.getScrollArea(c);
        return Width = (a[0] | 0), {
            Height: a[1],
            Left: a[2],
            Top: a[3],
            Width: Width,
        };
    }
    else {
        return void 0;
    }
}

export function Draw2dWrapper__GetZoom(this$) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        const arg0 = draw2d_fsharp_interface.getZoom(c);
        return arg0;
    }
    else {
        return void 0;
    }
}

export function Draw2dWrapper__SetScrollZoom(this$, scrollLeft, scrollTop, zoom) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        draw2d_fsharp_interface.setScrollZoom(c, scrollLeft, scrollTop, zoom);
    }
}

export function Draw2dWrapper__CreateComponent(this$, componentType, label, x, y) {
    const matchValue = [this$.canvas, this$.dispatch];
    let pattern_matching_result, c, d;
    if (matchValue[0] != null) {
        if (matchValue[1] != null) {
            pattern_matching_result = 1;
            c = matchValue[0];
            d = matchValue[1];
        }
        else {
            pattern_matching_result = 0;
        }
    }
    else {
        pattern_matching_result = 0;
    }
    switch (pattern_matching_result) {
        case 0: {
            console.log("Warning: Draw2dWrapper.CreateComponent called when canvas or dispatch is None");
            return void 0;
        }
        case 1: {
            const arg0 = createComponent(c, d, void 0, componentType, label, void 0, void 0, x, y);
            return arg0;
        }
    }
}

export function Draw2dWrapper__LoadComponent_596CF542(this$, comp) {
    try {
        let arg0;
        const matchValue = [this$.canvas, this$.dispatch];
        let pattern_matching_result, c, d;
        if (matchValue[0] != null) {
            if (matchValue[1] != null) {
                pattern_matching_result = 1;
                c = matchValue[0];
                d = matchValue[1];
            }
            else {
                pattern_matching_result = 0;
            }
        }
        else {
            pattern_matching_result = 0;
        }
        switch (pattern_matching_result) {
            case 0: {
                arg0 = (console.log("Warning: Draw2dWrapper.LoadComponent called when canvas or dispatch is None"));
                break;
            }
            case 1: {
                const value = createComponent(c, d, comp.Id, comp.Type, comp.Label, comp.InputPorts, comp.OutputPorts, comp.X, comp.Y);
                arg0 = (void value);
                break;
            }
        }
        return new FSharpResult$2(0, void 0);
    }
    catch (e) {
        let msg;
        const clo1 = toText(printf("Draw2d Error in LoadComponent:\n%A %A\nFailed to create component."));
        const clo2 = clo1(comp.Label);
        msg = clo2(comp.Type);
        createComponentError(cons(msg, createComponentError()), true);
        return new FSharpResult$2(1, msg);
    }
}

export function Draw2dWrapper__LoadConnection(this$, useId, conn) {
    Draw2dWrapper__tryActionWithCanvas(this$, "LoadConnection", (c) => {
        const sourceParentNode = assertNotNull((draw2d_fsharp_interface.getComponentById(c, conn.Source.HostId)), "sourceParentNode");
        const sourcePort = assertNotNull((draw2d_fsharp_interface.getPortById(sourceParentNode, conn.Source.Id)), "sourcePort");
        const targetParentNode = assertNotNull((draw2d_fsharp_interface.getComponentById(c, conn.Target.HostId)), "targetParentNode");
        const targetPort = assertNotNull((draw2d_fsharp_interface.getPortById(targetParentNode, conn.Target.Id)), "targetPort");
        const connId = useId ? conn.Id : (void 0);
        createConnection(c, connId, conn.Vertices, sourcePort, targetPort);
    });
}

export function Draw2dWrapper__GetAndClearLoadConnectionErrors(this$) {
    const errs = createConnectionError();
    createConnectionError(empty(), true);
    return errs;
}

export function Draw2dWrapper__GetAndClearLoadComponentErrors(this$) {
    const errs = createComponentError();
    createComponentError(empty(), true);
    return errs;
}

export function Draw2dWrapper__EditComponentLabel(this$, componentId, newLabel) {
    Draw2dWrapper__tryActionWithCanvas(this$, "EditComponentLabel", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, componentId)), "EditComponentLabel");
        draw2d_fsharp_interface.setComponentLabel(jsComp, newLabel);
    });
}

export function Draw2dWrapper__PaintConnection(this$, connectionId, width, colorOpt) {
    Draw2dWrapper__tryActionWithCanvas(this$, "PaintConnection", (c) => {
        let n, clo1, clo1_2;
        const jsConnection = assertNotNull((draw2d_fsharp_interface.getConnectionById(c, connectionId)), "PaintConnection");
        let patternInput;
        if (width === 1) {
            patternInput = ["", 1, "black"];
        }
        else if (n = (width | 0), n > 1) {
            const n_1 = width | 0;
            patternInput = [(clo1 = toText(printf("%d\n/")), clo1(n_1)), 3, "purple"];
        }
        else {
            const n_2 = width | 0;
            const clo1_1 = toFail(printf("what? PaintConnection called with width %d"));
            patternInput = clo1_1(n_2);
        }
        const stroke = patternInput[1] | 0;
        const label = patternInput[0];
        const color = patternInput[2];
        let color$0027;
        if (colorOpt == null) {
            color$0027 = color;
        }
        else {
            const newColor = colorOpt;
            color$0027 = (clo1_2 = toText(printf("%A")), clo1_2(newColor)).toLocaleLowerCase();
        }
        draw2d_fsharp_interface.setConnectionLabel(jsConnection, label);
        draw2d_fsharp_interface.setConnectionStroke(jsConnection, stroke);
        draw2d_fsharp_interface.setConnectionColor(jsConnection, color$0027);
    });
}

export function Draw2dWrapper__HighlightComponent(this$, color, componentId) {
    Draw2dWrapper__tryActionWithCanvas(this$, "HighlightComponent", (c) => {
        const comp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, componentId)), "HighlightComponent");
        const arg10_1 = getColorString(color);
        draw2d_fsharp_interface.setComponentBackground(comp, arg10_1);
    });
}

export function Draw2dWrapper__UnHighlightComponent_Z721C83C5(this$, componentId) {
    Draw2dWrapper__tryActionWithCanvas(this$, "UnHighlightComponent", (c) => {
        let comp;
        comp = draw2d_fsharp_interface.getComponentById(c, componentId);
        const matchValue = (comp == null || comp === 'undefined');
        if (matchValue) {
        }
        else {
            draw2d_fsharp_interface.setComponentBackground(comp, "lightgray");
        }
    });
}

export function Draw2dWrapper__HighlightConnection(this$, connectionId, color) {
    Draw2dWrapper__tryActionWithCanvas(this$, "HighlightConnection", (c) => {
        const conn = assertNotNull((draw2d_fsharp_interface.getConnectionById(c, connectionId)), "HighlightConnection");
        draw2d_fsharp_interface.setConnectionColor(conn, color);
        draw2d_fsharp_interface.setConnectionStroke(conn, 3);
    });
}

export function Draw2dWrapper__UnHighlightConnection_Z721C83C5(this$, connectionId) {
    Draw2dWrapper__tryActionWithCanvas(this$, "UnHighlightConnection", (c) => {
        let conn;
        conn = draw2d_fsharp_interface.getConnectionById(c, connectionId);
        const matchValue = (conn == null || conn === 'undefined');
        if (matchValue) {
        }
        else {
            draw2d_fsharp_interface.setConnectionColor(conn, "black");
            draw2d_fsharp_interface.setConnectionStroke(conn, 1);
        }
    });
}

export function Draw2dWrapper__GetCanvasState(this$) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        let comps;
        const jsList = draw2d_fsharp_interface.getAllJsComponents(c);
        comps = jsListToFSharpList(jsList);
        let conns;
        const jsList_1 = draw2d_fsharp_interface.getAllJsConnections(c);
        conns = jsListToFSharpList(jsList_1);
        return [comps, conns];
    }
    else {
        console.log("Warning: Draw2dWrapper.GetCanvasState called when canvas is None");
        return void 0;
    }
}

export function Draw2dWrapper__GetSelected(this$) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        let comps;
        const jsList = draw2d_fsharp_interface.getSelectedJsComponents(c);
        comps = jsListToFSharpList(jsList);
        let conns;
        const jsList_1 = draw2d_fsharp_interface.getSelectedJsConnections(c);
        conns = jsListToFSharpList(jsList_1);
        return [comps, conns];
    }
    else {
        console.log("Warning: Draw2dWrapper.GetSelected called when canvas is None");
        return void 0;
    }
}

export function Draw2dWrapper__Undo(this$) {
    Draw2dWrapper__tryActionWithCanvas(this$, "Undo", (arg00) => {
        draw2d_fsharp_interface.undoLastAction(arg00);
    });
}

export function Draw2dWrapper__Redo(this$) {
    Draw2dWrapper__tryActionWithCanvas(this$, "Redo", (arg00) => {
        draw2d_fsharp_interface.redoLastAction(arg00);
    });
}

export function Draw2dWrapper__FlushCommandStack(this$) {
    Draw2dWrapper__tryActionWithCanvas(this$, "FlushCommandStack", (arg00) => {
        draw2d_fsharp_interface.flushCommandStack(arg00);
    });
}

export function Draw2dWrapper__UpdateMergeWiresLabels(this$, compId, topInputWidth, bottomInputWidth, outputWidth) {
    Draw2dWrapper__tryActionWithCanvas(this$, "UpdateMergeWiresLabels", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "UpdateMergeWiresLabels");
        draw2d_fsharp_interface.updateMergeWiresLabels(jsComp, topInputWidth, bottomInputWidth, outputWidth);
    });
}

export function Draw2dWrapper__UpdateSplitWireLabels(this$, compId, inputWidth, topOutputWidth, bottomOutputWidth) {
    Draw2dWrapper__tryActionWithCanvas(this$, "UpdateSplitWireLabels", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "UpdateSplitWireLabels");
        draw2d_fsharp_interface.updateSplitWireLabels(jsComp, inputWidth, topOutputWidth, bottomOutputWidth);
    });
}

export function Draw2dWrapper__WriteMemoryLine(this$, compId, memData) {
    Draw2dWrapper__tryActionWithCanvas(this$, "WriteMemoryLine", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "WriteMemoryLine");
        draw2d_fsharp_interface.writeMemoryLine(jsComp, memData);
    });
}

export function Draw2dWrapper__SetNumberOfBits(this$, compId, numberOfBits) {
    Draw2dWrapper__tryActionWithCanvas(this$, "SetNumberOfBits", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "SetNumberOfBits");
        draw2d_fsharp_interface.setNumberOfBits(jsComp, numberOfBits);
    });
}

export function Draw2dWrapper__SetConstantNumber(this$, compId, cNum) {
    Draw2dWrapper__tryActionWithCanvas(this$, "SetConstantNumber", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "SetConstantNumber");
        draw2d_fsharp_interface.setConstantNumber(jsComp, cNum);
    });
}

export function Draw2dWrapper__SetLsbBitNumber(this$, compId, lsbBitNumber) {
    Draw2dWrapper__tryActionWithCanvas(this$, "SetLsbBitNumber", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "SetLsbBitNumber");
        draw2d_fsharp_interface.setLsbBitNumber(jsComp, lsbBitNumber);
    });
}

export function Draw2dWrapper__SetTopOutputWidth(this$, compId, topOutputWidth) {
    Draw2dWrapper__tryActionWithCanvas(this$, "SetTopOutputWidth", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "SetTopOutputWidth");
        draw2d_fsharp_interface.setTopOutputWidth(jsComp, topOutputWidth);
    });
}

export function Draw2dWrapper__SetRegisterWidth(this$, compId, regWidth) {
    Draw2dWrapper__tryActionWithCanvas(this$, "SetRegisterWidth", (c) => {
        const jsComp = assertNotNull((draw2d_fsharp_interface.getComponentById(c, compId)), "SetRegisterWidth");
        draw2d_fsharp_interface.setRegisterWidth(jsComp, regWidth);
    });
}

export function Draw2dWrapper__GetComponentById_Z721C83C5(this$, compId) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        let jsComp;
        jsComp = draw2d_fsharp_interface.getComponentById(c, compId);
        const matchValue_1 = (jsComp == null || jsComp === 'undefined');
        if (matchValue_1) {
            let arg0;
            const clo1 = toText(printf("Could not find component with Id: %s"));
            arg0 = clo1(compId);
            return new FSharpResult$2(1, arg0);
        }
        else {
            return new FSharpResult$2(0, jsComp);
        }
    }
    else {
        return new FSharpResult$2(1, "Draw2dWrapper.GetComponentById called when canvas is None");
    }
}

export function Draw2dWrapper__ChangeSelectionOfTheseConnections(this$, on, changeConns) {
    const matchValue = this$.canvas;
    if (matchValue == null) {
    }
    else {
        const c = matchValue;
        let patternInput;
        const matchValue_1 = Draw2dWrapper__GetSelected(this$);
        if (matchValue_1 == null) {
            patternInput = [empty(), empty()];
        }
        else {
            const b = matchValue_1[1];
            const a = matchValue_1[0];
            patternInput = [a, b];
        }
        const conns = patternInput[1];
        const comps = patternInput[0];
        let conns$0027;
        if (on) {
            const list = append(changeConns, conns);
            conns$0027 = distinct(list, {
                Equals: equalsSafe,
                GetHashCode: hashSafe,
            });
        }
        else {
            conns$0027 = filter((c_1) => {
                const value = contains(c_1, changeConns, {
                    Equals: equalsSafe,
                    GetHashCode: hashSafe,
                });
                return !value;
            }, conns);
        }
        draw2d_fsharp_interface.resetSelection(c);
        const value_1 = map((arg10) => {
            draw2d_fsharp_interface.addCompSelection(c, arg10);
        }, comps);
        void value_1;
        const value_2 = map((arg10_1) => {
            draw2d_fsharp_interface.addConnSelection(c, arg10_1);
        }, conns$0027);
        void value_2;
    }
}

export function Draw2dWrapper__ResetSelected(this$) {
    const matchValue = this$.canvas;
    if (matchValue == null) {
    }
    else {
        const c = matchValue;
        draw2d_fsharp_interface.resetSelection(c);
    }
}

export function Draw2dWrapper__DeleteSelected(this$) {
    const matchValue = this$.canvas;
    if (matchValue == null) {
    }
    else {
        const c = matchValue;
        draw2d_fsharp_interface.deleteSelection(c);
    }
}

function Draw2dWrapper__tryActionWithCanvas(this$, name, action) {
    const matchValue = this$.canvas;
    if (matchValue != null) {
        const c = matchValue;
        action(c);
    }
    else {
        let msg;
        const clo1 = toText(printf("Warning: Draw2dWrapper.%s called when canvas is None"));
        msg = clo1(name);
        console.log(msg);
    }
}

