/*
 * Extension of the default draw2d connections.
 */

draw2d.Connection = draw2d.Connection.extend({

    NAME: "Connection",

    init: function (attr, setter, getter) {
        this._super(attr, setter, getter);
    },
  
    disconnect: function () {
        this._super()

        // Remove some decorations of the router.
        // This is necessayr to remove the annoying dots that are left from
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

    onRightMouseDown: function (conn, x, y, shiftKey, ctrlKey) {
        return; // Do nothing? TODO. One could add a segment.
    }
})
