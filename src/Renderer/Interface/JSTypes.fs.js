import { Union } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Types.js";
import { union_type, obj_type } from "../.fable/fable-library.3.0.0-nagareyama-rc-007/Reflection.js";

export class JSCanvas extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSCanvas"];
    }
}

export function JSCanvas$reflection() {
    return union_type("JSTypes.JSCanvas", [], JSCanvas, () => [[["Item", obj_type]]]);
}

export class JSFigures extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSFigures"];
    }
}

export function JSFigures$reflection() {
    return union_type("JSTypes.JSFigures", [], JSFigures, () => [[["Item", obj_type]]]);
}

export class JSComponents extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSComponents"];
    }
}

export function JSComponents$reflection() {
    return union_type("JSTypes.JSComponents", [], JSComponents, () => [[["Item", obj_type]]]);
}

export class JSConnections extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSConnections"];
    }
}

export function JSConnections$reflection() {
    return union_type("JSTypes.JSConnections", [], JSConnections, () => [[["Item", obj_type]]]);
}

export class JSPort extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSPort"];
    }
}

export function JSPort$reflection() {
    return union_type("JSTypes.JSPort", [], JSPort, () => [[["Item", obj_type]]]);
}

export class JSPorts extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSPorts"];
    }
}

export function JSPorts$reflection() {
    return union_type("JSTypes.JSPorts", [], JSPorts, () => [[["Item", obj_type]]]);
}

export class JSVertices extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["JSVertices"];
    }
}

export function JSVertices$reflection() {
    return union_type("JSTypes.JSVertices", [], JSVertices, () => [[["Item", obj_type]]]);
}

