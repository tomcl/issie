/*
 * Extension of the default draw2d connections.
 */

/// Setup circuit-like connections in the diagram.
let router = new draw2d.layout.connection.InteractiveManhattanConnectionRouter();
// TODO: use CircuitConnectionRouter instead?
router.abortRoutingOnFirstVertexNode = false;

function createDigitalConnection(sourcePort, targetPort) {
    if (sourcePort === "undefined" || targetPort === "undefined") {
        throw "CreateDigitalConnection called with sourcePort or targetPort set to undefined";
    }
    let isBus = false;
    if (sourcePort.isBusPort === true && targetPort.isBusPort === true) {
        isBus = true;
    } else if (sourcePort.isBusPort === true || targetPort.isBusPort === true) {
        // One of the two port is bus and the other one is not.
        // TODO: display legit message.
        throw "Attempting to connect a port that accepts a bus, to a port that accepts a single bit."
    }
    let c = new draw2d.Connection({
        outlineColor: 'white',
        outlineStroke: 1,
        color: isBus ? 'purple' : 'black',
        router: router,
        stroke: isBus ? 3 : 1,
        radius: 6,
        selectable: true,
    });
    c.setSource(sourcePort);
    c.setTarget(targetPort);
    // TODO: add check to make sure this connection does not exist
    // already?
    return c;
}

draw2d.Connection = draw2d.Connection.extend({

    NAME: "Connection",

    init: function (attr, setter, getter) {
        this._super(attr, setter, getter);
    },

    disconnect: function () {
        this._super()

        // Remove some decorations of the router.
        // This is necessary to remove the annoying dots that are left from
        // connections sometimes.
        if (typeof this.vertexNodes !== "undefined" && this.vertexNodes !== null) {
            this.vertexNodes.remove()
            delete this.vertexNodes
        }
    },
})

draw2d.policy.line.OrthogonalSelectionFeedbackPolicy = draw2d.policy.line.OrthogonalSelectionFeedbackPolicy.extend({

    NAME: "ConnectionSelectionFeedbackPolicy",

    init: function () {
        this._super()
    },

    // Block default broken menu from appearing.
    onRightMouseDown: function (conn, x, y, shiftKey, ctrlKey) {
        return; // Do nothing? TODO. One could add a segment.
    }
})
