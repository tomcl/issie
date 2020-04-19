/*
 * Setup some functionalities of the draw2d library.
 */

/// Setup circuit-like connections in the diagram.
let router = new draw2d.layout.connection.CircuitConnectionRouter();
// TODO: use InteractiveManhattanConnectionRouter instead?
router.abortRoutingOnFirstVertexNode = false;
let createDigitalConnection = function(sourcePort, targetPort){
    let c = new draw2d.Connection({
        outlineColor: '#ffffff',
        outlineStroke: 1,
        color: '#000000',
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
};
