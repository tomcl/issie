
/// Setup circuit-like connections in the diagram.
let router = new draw2d.layout.connection.CircuitConnectionRouter();
// TODO: use InteractiveManhattanConnectionRouter instead?
router.abortRoutingOnFirstVertexNode = false;

function createDigitalConnection(sourcePort, targetPort) {
    let c = new draw2d.Connection({
        outlineColor: 'white',
        outlineStroke: 1,
        color: 'black',
        router: router,
        stroke: 1,
        radius: 2,
        selectable: true,
    });
    if (sourcePort) {
        c.setSource(sourcePort);
        c.setTarget(targetPort);
        // TODO: add check to make sure this connection does not exist
        // already?
    }
    return c;
}

function createCanvas(id, width, height) {
    let canvas = new draw2d.Canvas(id, width, height);
    canvas.setScrollArea('#'+id);
    // Make sure the canvas does not overflow the parent div.
    let style = document.getElementById(id).style;
    style.height = 'auto'; style.width = 'auto';
    return canvas;
}

function initialiseCanvas (canvas) {
    // Show canvas grid.
    canvas.installEditPolicy(new draw2d.policy.canvas.ShowGridEditPolicy());
    // Fadeout all decorations like ports, resize handles if the user didn't move
    // the mouse.
    canvas.installEditPolicy(new draw2d.policy.canvas.FadeoutDecorationPolicy());
    // Install a Connection create policy which matches to a 'circuit like'
    // connections.
    canvas.installEditPolicy( new draw2d.policy.connection.ComposedConnectionCreatePolicy(
        [
            // Create a connection via Drag&Drop of ports.
            new draw2d.policy.connection.DragConnectionCreatePolicy({
                createConnection: createDigitalConnection
            }),
            // Or via click and point.
            new draw2d.policy.connection.OrthogonalConnectionCreatePolicy({
                createConnection: createDigitalConnection
            })
        ])
    );
    // Install policy to allow to zoom with: SHIFT + mouse wheel.
    canvas.installEditPolicy(new draw2d.policy.canvas.WheelZoomPolicy());
}

function clearCanvas (canvas) {
    canvas.clear();
}

function addComponentToCanvas(canvas, comp) {
    canvas.add(comp);
}

function addConnectionToCanvas(canvas, conn) {
    canvas.add(conn);
}

function addLabel(comp, label) {
    comp.add(
        new draw2d.shape.basic.Label({text: label, stroke: 0}),
        new draw2d.layout.locator.TopLocator()
    );
}

function setComponentId(comp, id) {
    comp.setId(id);
}

function setConnectionId(conn, id) {
    conn.setId(id);
}

function setPortId(port, id) {
    port.setId(id);
}

function setComponentBackground(comp, color) {
    comp.setBackgroundColor(color);
}

function setConnectionColor(conn, color) {
    conn.setColor(color);
}

function setConnectionStroke(conn, w) {
    conn.setStroke(w);
}

function getInputPorts(comp) {
    return comp.getInputPorts().data;
}

function getOutputPorts(comp) {
    return comp.getOutputPorts().data;
}

function installSelectionPolicy(comp, onSelect, onUnselect) {
    comp.installEditPolicy(new draw2d.policy.figure.AntSelectionFeedbackPolicy({
        onSelect: function(canvas, comp, isPrimarySelection) {
            comp.setBackgroundColor('#00D1B2');
            onSelect(comp);
        },
        onUnselect: function(canvas, comp) {
            comp.setBackgroundColor('lightgray');
            onUnselect(comp);
        }
    }));
}

function createDigitalInput(x, y) {
    return new draw2d.shape.digital.Input({x:x,y:y,resizeable:false});
}

function createDigitalOutput(x, y) {
    return new draw2d.shape.digital.Output({x:x,y:y,resizeable:false});
}

function createDigitalNot(x, y) {
    return new draw2d.shape.digital.Not({x:x,y:y,resizeable:false});
}

function createDigitalAnd(x, y) {
    return new draw2d.shape.digital.And({x:x,y:y,resizeable:false});
}

function createDigitalOr(x, y) {
    return new draw2d.shape.digital.Or({x:x,y:y,resizeable:false});
}

function createDigitalXor(x, y) {
    return new draw2d.shape.digital.Xor({x:x,y:y,resizeable:false});
}

function createDigitalNand(x, y) {
    return new draw2d.shape.digital.Nand({x:x,y:y,resizeable:false});
}

function createDigitalNor(x, y) {
    return new draw2d.shape.digital.Nor({x:x,y:y,resizeable:false});
}

function createDigitalXnor(x, y) {
    return new draw2d.shape.digital.Xnor({x:x,y:y,resizeable:false});
}

function createDigitalMux2(x, y) {
    return new draw2d.shape.digital.Mux2({x:x,y:y,resizeable:false});
}

function getComponentById(canvas, id) {
    return canvas.getFigures().find(function(comp) {
        return comp.id === id;
    });
}

function getConnectionById(canvas, id) {
    return canvas.getLines().find(function(conn) {
        return conn.id === id;
    });
}

function getPortById(comp, id) {
    return comp.getPorts().find(function(port) {
        return port.id === id;
    });
}

function getAllJsComponents(canvas) {
    return canvas.getFigures().data;
}

function getAllJsConnections(canvas) {
    return canvas.getLines().data;
}

export {
    createCanvas,
    initialiseCanvas,
    clearCanvas,
    addComponentToCanvas,
    addConnectionToCanvas,
    addLabel,
    setComponentId,
    setConnectionId,
    setPortId,
    setComponentBackground,
    setConnectionColor,
    setConnectionStroke,
    getInputPorts,
    getOutputPorts,
    installSelectionPolicy,
    createDigitalInput,
    createDigitalOutput,
    createDigitalNot,
    createDigitalAnd,
    createDigitalOr,
    createDigitalXor,
    createDigitalNand,
    createDigitalNor,
    createDigitalXnor,
    createDigitalMux2,
    createDigitalConnection,
    getComponentById,
    getConnectionById,
    getPortById,
    getAllJsComponents,
    getAllJsConnections,
};
