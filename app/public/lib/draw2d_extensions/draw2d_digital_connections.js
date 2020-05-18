/*
 * Extension of the default draw2d connections.
 */

/// Custom locator for the bus label.
draw2d.layout.locator.BusLabelLocator = draw2d.layout.locator.ConnectionLocator.extend({
    NAME : "draw2d.layout.locator.BusLabelLocator",

    init: function() {
      this._super();
    },

    /**
     * Relocate the figure near the source vertice.
     * 
     * @param {Number} index child index of the target
     * @param {draw2d.Figure} target The figure to relocate
     **/
    relocate: function(index, target) {
       const conn = target.getParent();
       const points = conn.getVertices();
       const x = points.data[0].x;
       const y = points.data[0].y - 22;
       target.setPosition(x,y);
    }
});

/// Setup circuit-like connections in the diagram.
let router = new draw2d.layout.connection.InteractiveManhattanConnectionRouter();
// TODO: use CircuitConnectionRouter instead?
router.abortRoutingOnFirstVertexNode = false;

function createDigitalConnection(sourcePort, targetPort) {
    if (sourcePort === "undefined" || targetPort === "undefined") {
        throw "CreateDigitalConnection called with sourcePort or targetPort set to undefined";
    }
    // This is helpful because sometimes not all components can have their width
    // inferred, but this already allows buses to be purple. Once the width can
    // be inferred, the connection will be repainted accordingly.
    let isBus = false;
    if (sourcePort.isBusPort === true && targetPort.isBusPort === true) {
        isBus = true;
    } else if (sourcePort.isBusPort === true || targetPort.isBusPort === true) {
        // One of the two port is bus and the other one is not.
        // This problem will be caught by the infer width function.
        //throw "Attempting to connect a port that accepts a bus, to a port that accepts a single bit."
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
    // Add label.
    c.add(
        new draw2d.shape.basic.Label({text: '', stroke: 0}),
        new draw2d.layout.locator.BusLabelLocator()
    );
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

        // This event fires every time the ports connected to the connection
        // change. This may mean that another run of inference is necessary.
        // Data has keys "port" (the newly changed port) and "connection"
        // (the connection itself).
        this.on("connect", (_, data) => {
            this.sendInferWidthsMessage();
        });

        // Every time the connection is changed (i.e. rerouted), recalculate the
        // position of the connection label.
        this.on("change", (conn, type) => {
            let label = conn.children.data[0];
            label.locator.relocate(0, label.figure);
        });
    },

    // The dispatch lock ensures that a connection does not trigger the
    // InferWidth message multiple times in a short period of time.
    // It would since the "connect" event is fired multiple times when a 
    // connection is created (connect source and target).
    // This is just for performance.
    dispatchLock: "undefined", 

    sendInferWidthsMessage: function() {
        if (dispatchInferWidthsMessage !== "undefined" &&
            this.dispatchLock === "undefined") {
            // Dispatch the message after 20ms, to give the time to the
            // function to finish. This way the inference function will
            // access the version of the state that also contains the
            // connection.
            this.dispatchLock = setTimeout(() => {
                dispatchInferWidthsMessage();
                this.dispatchLock = "undefined"; // Clear timer.
            }, 20);
        } else if (this.dispatchTimer === "undefined") {
            console.log("Warning: connection trying to dispatch a JS InferWidths message but dispatcher is not defined.")
        }
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

        this.sendInferWidthsMessage();
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
