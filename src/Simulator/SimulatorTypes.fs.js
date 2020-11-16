import { toString, Record, Union } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { obj_type, string_type, int32_type, record_type, lambda_type, option_type, array_type, bool_type, tuple_type, class_type, list_type, union_type } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";
import { ConnectionId$reflection, NumberBase$reflection, OutputPortNumber$reflection, InputPortNumber$reflection, ComponentLabel$reflection, ComponentType$reflection, ComponentId$reflection, Memory$reflection } from "../Common/CommonTypes.fs.js";
import { sprintInitial } from "../Common/Helpers.fs.js";
import { toConsole, join, printf, toText } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/String.js";
import { tryPick, toList } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Map.js";
import { map } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/List.js";
import { equalsSafe } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Util.js";
import { defaultArg, map as map_1 } from "../renderer/.fable/fable-library.3.0.0-nagareyama-rc-007/Option.js";

export class Bit extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Zero", "One"];
    }
}

export function Bit$reflection() {
    return union_type("SimulatorTypes.Bit", [], Bit, () => [[], []]);
}

export class SimulationComponentState extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["NoState", "DffState", "RegisterState", "RamState"];
    }
}

export function SimulationComponentState$reflection() {
    return union_type("SimulatorTypes.SimulationComponentState", [], SimulationComponentState, () => [[], [["Item", Bit$reflection()]], [["Item", list_type(Bit$reflection())]], [["Item", Memory$reflection()]]]);
}

export class IsClockTick extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["No", "Yes"];
    }
}

export function IsClockTick$reflection() {
    return union_type("SimulatorTypes.IsClockTick", [], IsClockTick, () => [[], [["Item", SimulationComponentState$reflection()]]]);
}

export class SimulationComponent extends Record {
    constructor(Id, Type, Label, Inputs, Outputs, OutputsPropagated, CustomSimulationGraph, State, Reducer) {
        super();
        this.Id = Id;
        this.Type = Type;
        this.Label = Label;
        this.Inputs = Inputs;
        this.Outputs = Outputs;
        this.OutputsPropagated = OutputsPropagated;
        this.CustomSimulationGraph = CustomSimulationGraph;
        this.State = State;
        this.Reducer = Reducer;
    }
}

export function SimulationComponent$reflection() {
    return record_type("SimulatorTypes.SimulationComponent", [], SimulationComponent, () => [["Id", ComponentId$reflection()], ["Type", ComponentType$reflection()], ["Label", ComponentLabel$reflection()], ["Inputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [InputPortNumber$reflection(), list_type(Bit$reflection())])], ["Outputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [OutputPortNumber$reflection(), list_type(tuple_type(ComponentId$reflection(), InputPortNumber$reflection()))])], ["OutputsPropagated", array_type(bool_type)], ["CustomSimulationGraph", option_type(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()]))], ["State", SimulationComponentState$reflection()], ["Reducer", lambda_type(ReducerInput$reflection(), ReducerOutput$reflection())]]);
}

export class ReducerInput extends Record {
    constructor(Inputs, CustomSimulationGraph, IsClockTick) {
        super();
        this.Inputs = Inputs;
        this.CustomSimulationGraph = CustomSimulationGraph;
        this.IsClockTick = IsClockTick;
    }
}

export function ReducerInput$reflection() {
    return record_type("SimulatorTypes.ReducerInput", [], ReducerInput, () => [["Inputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [InputPortNumber$reflection(), list_type(Bit$reflection())])], ["CustomSimulationGraph", option_type(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()]))], ["IsClockTick", IsClockTick$reflection()]]);
}

export class ReducerOutput extends Record {
    constructor(Outputs, NewCustomSimulationGraph, NewState) {
        super();
        this.Outputs = Outputs;
        this.NewCustomSimulationGraph = NewCustomSimulationGraph;
        this.NewState = NewState;
    }
}

export function ReducerOutput$reflection() {
    return record_type("SimulatorTypes.ReducerOutput", [], ReducerOutput, () => [["Outputs", option_type(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [OutputPortNumber$reflection(), list_type(Bit$reflection())]))], ["NewCustomSimulationGraph", option_type(class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()]))], ["NewState", SimulationComponentState$reflection()]]);
}

export class OutputChange extends Record {
    constructor(CComp, COutputs) {
        super();
        this.CComp = CComp;
        this.COutputs = COutputs;
    }
}

export function OutputChange$reflection() {
    return record_type("SimulatorTypes.OutputChange", [], OutputChange, () => [["CComp", SimulationComponent$reflection()], ["COutputs", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [OutputPortNumber$reflection(), list_type(Bit$reflection())])]]);
}

export class SimulationData extends Record {
    constructor(Graph, Inputs, Outputs, IsSynchronous, NumberBase, ClockTickNumber) {
        super();
        this.Graph = Graph;
        this.Inputs = Inputs;
        this.Outputs = Outputs;
        this.IsSynchronous = IsSynchronous;
        this.NumberBase = NumberBase;
        this.ClockTickNumber = (ClockTickNumber | 0);
    }
}

export function SimulationData$reflection() {
    return record_type("SimulatorTypes.SimulationData", [], SimulationData, () => [["Graph", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [ComponentId$reflection(), SimulationComponent$reflection()])], ["Inputs", list_type(tuple_type(ComponentId$reflection(), ComponentLabel$reflection(), int32_type))], ["Outputs", list_type(tuple_type(ComponentId$reflection(), ComponentLabel$reflection(), int32_type))], ["IsSynchronous", bool_type], ["NumberBase", NumberBase$reflection()], ["ClockTickNumber", int32_type]]);
}

export class SimulationError extends Record {
    constructor(Msg, InDependency, ComponentsAffected, ConnectionsAffected) {
        super();
        this.Msg = Msg;
        this.InDependency = InDependency;
        this.ComponentsAffected = ComponentsAffected;
        this.ConnectionsAffected = ConnectionsAffected;
    }
}

export function SimulationError$reflection() {
    return record_type("SimulatorTypes.SimulationError", [], SimulationError, () => [["Msg", string_type], ["InDependency", option_type(string_type)], ["ComponentsAffected", list_type(ComponentId$reflection())], ["ConnectionsAffected", list_type(ConnectionId$reflection())]]);
}

export class JSComponent extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSComponent"];
    }
}

export function JSComponent$reflection() {
    return union_type("SimulatorTypes.JSComponent", [], JSComponent, () => [[["Item", obj_type]]]);
}

export class JSConnection extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSConnection"];
    }
}

export function JSConnection$reflection() {
    return union_type("SimulatorTypes.JSConnection", [], JSConnection, () => [[["Item", obj_type]]]);
}

export function sprintSimComponent(sComp) {
    let arg20;
    const s = toString(sComp.Type);
    arg20 = sprintInitial(20, s);
    const clo1 = toText(printf("\u0027%A\u0027: %20s"));
    const clo2 = clo1(sComp.Label);
    return clo2(arg20);
}

export function shortPSComp(comp) {
    let lab;
    const lab$0027 = comp.Label.fields[0];
    lab = lab$0027;
    const matchValue_1 = comp.Type;
    if (matchValue_1.tag === 16) {
        const sc = matchValue_1.fields[0];
        const clo1 = toText(printf("%s:Custom.(%s.%A-\u003e%A)"));
        const clo2 = clo1(lab);
        const clo3 = clo2(sc.Name);
        const clo4 = clo3(sc.InputLabels);
        return clo4(sc.OutputLabels);
    }
    else {
        const clo1_1 = toText(printf("%s:%A.{%A}"));
        const clo2_1 = clo1_1(lab);
        const clo3_1 = clo2_1(comp.Type);
        return clo3_1(comp.State);
    }
}

export function printSimGraph(sg) {
    let list;
    const arg10 = join("\n", (list = (toList(sg)), (map((tupledArg) => {
        const comp = tupledArg[1];
        const id = tupledArg[0].fields[0];
        return sprintSimComponent(comp) + id;
    }, list))));
    const clo1 = toConsole(printf("%s"));
    clo1(arg10);
}

export function tryGetCompLabel(compId, sg) {
    let option_2;
    let option_1;
    const option = tryPick((k, v) => (equalsSafe(k, compId) ? v : (void 0)), sg);
    option_1 = map_1((comp) => comp.Label, option);
    option_2 = map_1((_arg1) => {
        const s = _arg1.fields[0];
        return s;
    }, option_1);
    return defaultArg(option_2, "\u0027Not in SimGraph\u0027");
}

