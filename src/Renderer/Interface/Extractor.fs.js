import { jsListToFSharpList, getFailIfNull } from "./JSHelpers.fs.js";
import { sortBy, empty, map, mapIndexed, ofArray, singleton } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { printf, toFail } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { Connection, Component, CustomComponentType, ComponentType, Memory, Port, PortType } from "../../Common/CommonTypes.fs.js";
import { ofList } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { comparePrimitives } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";

function extractLabel(childrenArray) {
    const extract = (children_mut) => {
        extract:
        while (true) {
            const children = children_mut;
            if (children.tail != null) {
                const children$0027 = children.tail;
                const child = children.head;
                const childFig = child.figure;
                const matchValue = (childFig == null || childFig === 'undefined');
                if (matchValue) {
                    children_mut = children$0027;
                    continue extract;
                }
                else {
                    const matchValue_1 = getFailIfNull(childFig, singleton("cssClass"));
                    if (matchValue_1 === "draw2d_shape_basic_Label") {
                        return getFailIfNull(childFig, singleton("text"));
                    }
                    else {
                        children_mut = children$0027;
                        continue extract;
                    }
                }
            }
            else {
                return toFail(printf("what? No label found among the when extracting component."));
            }
            break;
        }
    };
    return extract(jsListToFSharpList(childrenArray));
}

function extractPort(maybeNumber, jsPort) {
    let portType;
    const matchValue = getFailIfNull(jsPort, singleton("cssClass"));
    switch (matchValue) {
        case "draw2d_InputPort": {
            portType = (new PortType(0));
            break;
        }
        case "draw2d_OutputPort": {
            portType = (new PortType(1));
            break;
        }
        default: {
            const p = matchValue;
            const clo1 = toFail(printf("what? oprt with cssClass %s"));
            portType = clo1(p);
        }
    }
    return new Port(getFailIfNull(jsPort, singleton("id")), maybeNumber, portType, getFailIfNull(jsPort, ofArray(["parent", "id"])));
}

function extractPorts(jsPorts) {
    const list = jsListToFSharpList(jsPorts);
    return mapIndexed(extractPort, list);
}

function extractMemoryData(jsComponent) {
    let data;
    return new Memory(getFailIfNull(jsComponent, singleton("addressWidth")), getFailIfNull(jsComponent, singleton("wordWidth")), (data = getFailIfNull(jsComponent, singleton("memData")), (ofList(data))));
}

export function extractComponentType(jsComponent) {
    const matchValue = getFailIfNull(jsComponent, singleton("componentType"));
    switch (matchValue) {
        case "Input": {
            const arg0 = getFailIfNull(jsComponent, singleton("numberOfBits")) | 0;
            return new ComponentType(0, arg0);
        }
        case "Constant": {
            const width = getFailIfNull(jsComponent, singleton("numberOfBits")) | 0;
            const constant = getFailIfNull(jsComponent, singleton("constValue")) | 0;
            return new ComponentType(4, width, constant);
        }
        case "Output": {
            const arg0_1 = getFailIfNull(jsComponent, singleton("numberOfBits")) | 0;
            return new ComponentType(1, arg0_1);
        }
        case "Not": {
            return new ComponentType(5);
        }
        case "And": {
            return new ComponentType(6);
        }
        case "Or": {
            return new ComponentType(7);
        }
        case "Xor": {
            return new ComponentType(8);
        }
        case "Nand": {
            return new ComponentType(9);
        }
        case "Nor": {
            return new ComponentType(10);
        }
        case "Xnor": {
            return new ComponentType(11);
        }
        case "Mux2": {
            return new ComponentType(13);
        }
        case "Demux2": {
            return new ComponentType(14);
        }
        case "Decode4": {
            return new ComponentType(12);
        }
        case "NbitsAdder": {
            const arg0_2 = getFailIfNull(jsComponent, singleton("numberOfBits")) | 0;
            return new ComponentType(15, arg0_2);
        }
        case "Custom": {
            return new ComponentType(16, new CustomComponentType(getFailIfNull(jsComponent, singleton("customComponentName")), jsListToFSharpList(getFailIfNull(jsComponent, singleton("inputs"))), jsListToFSharpList(getFailIfNull(jsComponent, singleton("outputs")))));
        }
        case "MergeWires": {
            return new ComponentType(17);
        }
        case "SplitWire": {
            const arg0_3 = getFailIfNull(jsComponent, singleton("topOutputWidth")) | 0;
            return new ComponentType(18, arg0_3);
        }
        case "BusSelection": {
            const width_1 = getFailIfNull(jsComponent, singleton("numberOfBits")) | 0;
            const lsb = getFailIfNull(jsComponent, singleton("lsbBitNumber")) | 0;
            const tupledArg = [width_1, lsb];
            return new ComponentType(3, tupledArg[0], tupledArg[1]);
        }
        case "DFF": {
            return new ComponentType(19);
        }
        case "DFFE": {
            return new ComponentType(20);
        }
        case "Register": {
            const arg0_5 = getFailIfNull(jsComponent, singleton("regWidth")) | 0;
            return new ComponentType(21, arg0_5);
        }
        case "RegisterE": {
            const arg0_6 = getFailIfNull(jsComponent, singleton("regWidth")) | 0;
            return new ComponentType(22, arg0_6);
        }
        case "AsyncROM": {
            const arg0_7 = extractMemoryData(jsComponent);
            return new ComponentType(23, arg0_7);
        }
        case "ROM": {
            const arg0_8 = extractMemoryData(jsComponent);
            return new ComponentType(24, arg0_8);
        }
        case "RAM": {
            const arg0_9 = extractMemoryData(jsComponent);
            return new ComponentType(25, arg0_9);
        }
        case "Label": {
            return new ComponentType(2);
        }
        default: {
            const ct = matchValue;
            const clo1 = toFail(printf("what? Component type %s does not exist: this must be added to extractor:extractComponentType"));
            return clo1(ct);
        }
    }
}

function extractVertices(jsVertices) {
    const list = jsListToFSharpList(jsVertices);
    return map((jsVertex) => [jsVertex.x, jsVertex.y], list);
}

export function extractComponent(jsComponent) {
    let childrenArray;
    const x = jsComponent.getOuterBoundingBox();
    const h = (x.getHeight()) | 0;
    const w = (x.getWidth()) | 0;
    const Id = getFailIfNull(jsComponent, singleton("id"));
    const Type = extractComponentType(jsComponent);
    let InputPorts;
    const jsPorts = getFailIfNull(jsComponent, ofArray(["inputPorts", "data"]));
    InputPorts = extractPorts(jsPorts);
    let OutputPorts;
    const jsPorts_1 = getFailIfNull(jsComponent, ofArray(["outputPorts", "data"]));
    OutputPorts = extractPorts(jsPorts_1);
    return new Component(Id, Type, (childrenArray = getFailIfNull(jsComponent, ofArray(["children", "data"])), (extractLabel(childrenArray))), InputPorts, OutputPorts, getFailIfNull(jsComponent, singleton("x")), getFailIfNull(jsComponent, singleton("y")), h, w);
}

export function extractReducedComponent(jsComponent) {
    let childrenArray;
    const x = jsComponent.getOuterBoundingBox();
    const h = x.getHeight();
    const w = x.getWidth();
    const Id = getFailIfNull(jsComponent, singleton("id"));
    const Type = extractComponentType(jsComponent);
    let InputPorts;
    const jsPorts = getFailIfNull(jsComponent, ofArray(["inputPorts", "data"]));
    InputPorts = extractPorts(jsPorts);
    let OutputPorts;
    const jsPorts_1 = getFailIfNull(jsComponent, ofArray(["outputPorts", "data"]));
    OutputPorts = extractPorts(jsPorts_1);
    return new Component(Id, Type, (childrenArray = getFailIfNull(jsComponent, ofArray(["children", "data"])), (extractLabel(childrenArray))), InputPorts, OutputPorts, 0, 0, 0, 0);
}

export function extractConnection(jsConnection) {
    let jsPort, jsPort_1, jsVertices;
    return new Connection(getFailIfNull(jsConnection, singleton("id")), (jsPort = getFailIfNull(jsConnection, singleton("sourcePort")), (extractPort(void 0, jsPort))), (jsPort_1 = getFailIfNull(jsConnection, singleton("targetPort")), (extractPort(void 0, jsPort_1))), (jsVertices = getFailIfNull(jsConnection, ofArray(["vertices", "data"])), (extractVertices(jsVertices))));
}

export function extractReducedConnection(jsConnection) {
    let jsPort, jsPort_1;
    return new Connection(getFailIfNull(jsConnection, singleton("id")), (jsPort = getFailIfNull(jsConnection, singleton("sourcePort")), (extractPort(void 0, jsPort))), (jsPort_1 = getFailIfNull(jsConnection, singleton("targetPort")), (extractPort(void 0, jsPort_1))), empty());
}

function sortComponents(comps) {
    return sortBy((comp) => (comp.X + comp.Y), comps, {
        Compare: comparePrimitives,
    });
}

export function extractState(state_0, state_1) {
    const state = [state_0, state_1];
    const connections = state[1];
    const components = state[0];
    const patternInput = [map(extractComponent, components), map(extractConnection, connections)];
    const conns = patternInput[1];
    const comps = patternInput[0];
    const comps_1 = sortComponents(comps);
    return [comps_1, conns];
}

export function extractReducedState(state_0, state_1) {
    const state = [state_0, state_1];
    const connections = state[1];
    const components = state[0];
    let comps;
    const list = map(extractReducedComponent, components);
    comps = sortBy((comp) => comp.Id, list, {
        Compare: comparePrimitives,
    });
    let conns;
    const list_1 = map(extractReducedConnection, connections);
    conns = sortBy((conn) => conn.Id, list_1, {
        Compare: comparePrimitives,
    });
    return [comps, conns];
}

