import { Record, Union } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { bool_type, unit_type, uint32_type, array_type, float64_type, class_type, list_type, tuple_type, record_type, option_type, int32_type, string_type, union_type } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";

export const draw2dCanvasWidth = 3000;

export const draw2dCanvasHeight = 2000;

export class PortType extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Input", "Output"];
    }
}

export function PortType$reflection() {
    return union_type("CommonTypes.PortType", [], PortType, () => [[], []]);
}

export class Port extends Record {
    constructor(Id, PortNumber, PortType, HostId) {
        super();
        this.Id = Id;
        this.PortNumber = PortNumber;
        this.PortType = PortType;
        this.HostId = HostId;
    }
}

export function Port$reflection() {
    return record_type("CommonTypes.Port", [], Port, () => [["Id", string_type], ["PortNumber", option_type(int32_type)], ["PortType", PortType$reflection()], ["HostId", string_type]]);
}

export class CustomComponentType extends Record {
    constructor(Name, InputLabels, OutputLabels) {
        super();
        this.Name = Name;
        this.InputLabels = InputLabels;
        this.OutputLabels = OutputLabels;
    }
}

export function CustomComponentType$reflection() {
    return record_type("CommonTypes.CustomComponentType", [], CustomComponentType, () => [["Name", string_type], ["InputLabels", list_type(tuple_type(string_type, int32_type))], ["OutputLabels", list_type(tuple_type(string_type, int32_type))]]);
}

export class Memory extends Record {
    constructor(AddressWidth, WordWidth, Data) {
        super();
        this.AddressWidth = (AddressWidth | 0);
        this.WordWidth = (WordWidth | 0);
        this.Data = Data;
    }
}

export function Memory$reflection() {
    return record_type("CommonTypes.Memory", [], Memory, () => [["AddressWidth", int32_type], ["WordWidth", int32_type], ["Data", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [class_type("System.Int64"), class_type("System.Int64")])]]);
}

export class ComponentType extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Input", "Output", "IOLabel", "BusSelection", "Constant", "Not", "And", "Or", "Xor", "Nand", "Nor", "Xnor", "Decode4", "Mux2", "Demux2", "NbitsAdder", "Custom", "MergeWires", "SplitWire", "DFF", "DFFE", "Register", "RegisterE", "AsyncROM", "ROM", "RAM"];
    }
}

export function ComponentType$reflection() {
    return union_type("CommonTypes.ComponentType", [], ComponentType, () => [[["BusWidth", int32_type]], [["BusWidth", int32_type]], [], [["OutputWidth", int32_type], ["OutputLSBit", int32_type]], [["Width", int32_type], ["ConstValue", int32_type]], [], [], [], [], [], [], [], [], [], [], [["BusWidth", int32_type]], [["Item", CustomComponentType$reflection()]], [], [["BusWidth", int32_type]], [], [], [["BusWidth", int32_type]], [["BusWidth", int32_type]], [["Item", Memory$reflection()]], [["Item", Memory$reflection()]], [["Item", Memory$reflection()]]]);
}

export class Component extends Record {
    constructor(Id, Type, Label, InputPorts, OutputPorts, X, Y, H, W) {
        super();
        this.Id = Id;
        this.Type = Type;
        this.Label = Label;
        this.InputPorts = InputPorts;
        this.OutputPorts = OutputPorts;
        this.X = (X | 0);
        this.Y = (Y | 0);
        this.H = (H | 0);
        this.W = (W | 0);
    }
}

export function Component$reflection() {
    return record_type("CommonTypes.Component", [], Component, () => [["Id", string_type], ["Type", ComponentType$reflection()], ["Label", string_type], ["InputPorts", list_type(Port$reflection())], ["OutputPorts", list_type(Port$reflection())], ["X", int32_type], ["Y", int32_type], ["H", int32_type], ["W", int32_type]]);
}

export class Connection extends Record {
    constructor(Id, Source, Target, Vertices) {
        super();
        this.Id = Id;
        this.Source = Source;
        this.Target = Target;
        this.Vertices = Vertices;
    }
}

export function Connection$reflection() {
    return record_type("CommonTypes.Connection", [], Connection, () => [["Id", string_type], ["Source", Port$reflection()], ["Target", Port$reflection()], ["Vertices", list_type(tuple_type(float64_type, float64_type))]]);
}

export class NumberBase extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Hex", "Dec", "Bin", "SDec"];
    }
}

export function NumberBase$reflection() {
    return union_type("CommonTypes.NumberBase", [], NumberBase, () => [[], [], [], []]);
}

export class HighLightColor extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Red", "Blue", "Yellow", "Green", "Orange"];
    }
}

export function HighLightColor$reflection() {
    return union_type("CommonTypes.HighLightColor", [], HighLightColor, () => [[], [], [], [], []]);
}

export class ComponentId extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["ComponentId"];
    }
}

export function ComponentId$reflection() {
    return union_type("CommonTypes.ComponentId", [], ComponentId, () => [[["Item", string_type]]]);
}

export class ConnectionId extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["ConnectionId"];
    }
}

export function ConnectionId$reflection() {
    return union_type("CommonTypes.ConnectionId", [], ConnectionId, () => [[["Item", string_type]]]);
}

export class ComponentLabel extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["ComponentLabel"];
    }
}

export function ComponentLabel$reflection() {
    return union_type("CommonTypes.ComponentLabel", [], ComponentLabel, () => [[["Item", string_type]]]);
}

export class InputPortId extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["InputPortId"];
    }
}

export function InputPortId$reflection() {
    return union_type("CommonTypes.InputPortId", [], InputPortId, () => [[["Item", string_type]]]);
}

export class OutputPortId extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["OutputPortId"];
    }
}

export function OutputPortId$reflection() {
    return union_type("CommonTypes.OutputPortId", [], OutputPortId, () => [[["Item", string_type]]]);
}

export class InputPortNumber extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["InputPortNumber"];
    }
}

export function InputPortNumber$reflection() {
    return union_type("CommonTypes.InputPortNumber", [], InputPortNumber, () => [[["Item", int32_type]]]);
}

export class OutputPortNumber extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["OutputPortNumber"];
    }
}

export function OutputPortNumber$reflection() {
    return union_type("CommonTypes.OutputPortNumber", [], OutputPortNumber, () => [[["Item", int32_type]]]);
}

export class NLTarget extends Record {
    constructor(TargetCompId, InputPort, TargetConnId) {
        super();
        this.TargetCompId = TargetCompId;
        this.InputPort = InputPort;
        this.TargetConnId = TargetConnId;
    }
}

export function NLTarget$reflection() {
    return record_type("CommonTypes.NLTarget", [], NLTarget, () => [["TargetCompId", ComponentId$reflection()], ["InputPort", InputPortNumber$reflection()], ["TargetConnId", ConnectionId$reflection()]]);
}

export class NLSource extends Record {
    constructor(SourceCompId, OutputPort, SourceConnId) {
        super();
        this.SourceCompId = SourceCompId;
        this.OutputPort = OutputPort;
        this.SourceConnId = SourceConnId;
    }
}

export function NLSource$reflection() {
    return record_type("CommonTypes.NLSource", [], NLSource, () => [["SourceCompId", ComponentId$reflection()], ["OutputPort", OutputPortNumber$reflection()], ["SourceConnId", ConnectionId$reflection()]]);
}

export class NetListComponent extends Record {
    constructor(Id, Type, Label, Inputs, Outputs) {
        super();
        this.Id = Id;
        this.Type = Type;
        this.Label = Label;
        this.Inputs = Inputs;
        this.Outputs = Outputs;
    }
}

export function NetListComponent$reflection() {
    return record_type("CommonTypes.NetListComponent", [], NetListComponent, () => [["Id", ComponentId$reflection()], ["Type", ComponentType$reflection()], ["Label", string_type], ["Inputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [InputPortNumber$reflection(), option_type(NLSource$reflection())])], ["Outputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [OutputPortNumber$reflection(), list_type(NLTarget$reflection())])]]);
}

export class NetGroup extends Record {
    constructor(driverNet, connectedNets) {
        super();
        this.driverNet = driverNet;
        this.connectedNets = connectedNets;
    }
}

export function NetGroup$reflection() {
    return record_type("CommonTypes.NetGroup", [], NetGroup, () => [["driverNet", list_type(NLTarget$reflection())], ["connectedNets", array_type(list_type(NLTarget$reflection()))]]);
}

export class SavedWaveInfo extends Record {
    constructor(ClkWidth, Cursor, Radix, LastClk, DisplayedPortIds) {
        super();
        this.ClkWidth = ClkWidth;
        this.Cursor = Cursor;
        this.Radix = Radix;
        this.LastClk = LastClk;
        this.DisplayedPortIds = DisplayedPortIds;
    }
}

export function SavedWaveInfo$reflection() {
    return record_type("CommonTypes.SavedWaveInfo", [], SavedWaveInfo, () => [["ClkWidth", float64_type], ["Cursor", uint32_type], ["Radix", NumberBase$reflection()], ["LastClk", uint32_type], ["DisplayedPortIds", array_type(string_type)]]);
}

export class LoadedComponent extends Record {
    constructor(Name, TimeStamp, FilePath, WaveInfo, CanvasState, InputLabels, OutputLabels) {
        super();
        this.Name = Name;
        this.TimeStamp = TimeStamp;
        this.FilePath = FilePath;
        this.WaveInfo = WaveInfo;
        this.CanvasState = CanvasState;
        this.InputLabels = InputLabels;
        this.OutputLabels = OutputLabels;
    }
}

export function LoadedComponent$reflection() {
    return record_type("CommonTypes.LoadedComponent", [], LoadedComponent, () => [["Name", string_type], ["TimeStamp", class_type("System.DateTime")], ["FilePath", string_type], ["WaveInfo", option_type(SavedWaveInfo$reflection())], ["CanvasState", tuple_type(list_type(Component$reflection()), list_type(Connection$reflection()))], ["InputLabels", list_type(tuple_type(string_type, int32_type))], ["OutputLabels", list_type(tuple_type(string_type, int32_type))]]);
}

export class LabelSegment extends Record {
    constructor(LabName, BitLimits) {
        super();
        this.LabName = LabName;
        this.BitLimits = BitLimits;
    }
}

export function LabelSegment$reflection() {
    return record_type("CommonTypes.LabelSegment", [], LabelSegment, () => [["LabName", string_type], ["BitLimits", tuple_type(int32_type, int32_type)]]);
}

export class WaveLabel extends Record {
    constructor(OutputsAndIOLabels, ComposingLabels) {
        super();
        this.OutputsAndIOLabels = OutputsAndIOLabels;
        this.ComposingLabels = ComposingLabels;
    }
}

export function WaveLabel$reflection() {
    return record_type("CommonTypes.WaveLabel", [], WaveLabel, () => [["OutputsAndIOLabels", list_type(string_type)], ["ComposingLabels", list_type(LabelSegment$reflection())]]);
}

export class WidthInferError extends Record {
    constructor(Msg, ConnectionsAffected) {
        super();
        this.Msg = Msg;
        this.ConnectionsAffected = ConnectionsAffected;
    }
}

export function WidthInferError$reflection() {
    return record_type("CommonTypes.WidthInferError", [], WidthInferError, () => [["Msg", string_type], ["ConnectionsAffected", list_type(ConnectionId$reflection())]]);
}

export class JSDiagramMsg$2 extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["InitCanvas", "SelectComponent", "UnselectComponent", "InferWidths", "SetHasUnsavedChanges"];
    }
}

export function JSDiagramMsg$2$reflection(gen0, gen1) {
    return union_type("CommonTypes.JSDiagramMsg`2", [gen0, gen1], JSDiagramMsg$2, () => [[["Item", gen0]], [["Item", gen1]], [["Item", unit_type]], [["Item", unit_type]], [["Item", bool_type]]]);
}

