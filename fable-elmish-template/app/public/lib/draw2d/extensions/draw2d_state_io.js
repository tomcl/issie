/*
 * Helper functions to read and write the state of a canvas.
 * These functions aim to replace draw2d.io.json.Reader and
 * draw2d.io.json.Writer functionalities with more powerful versions.
 * 
 * The original Reader and Writer can serialise the state of the diagram into
 * a string but only with persistent attributes. We also need non persistent
 * attributes such as labels.
 */

draw2dStateIO = (function() {
    /// TODO: fix this.
    function getFigureLabel(figure) {
        //try {
            return figure.children.data[0].figure.text;
        //} catch (e) {
        //    throw "Could not find label for figure (hacky implementation).";
        //}
    }

    /// Returns the base64 encoded json representing the canvas state.
    function getCanvasStateAsString(canvas) {
        let result = [];
        canvas.getFigures().each(function(i, figure){
            let attr = figure.getPersistentAttributes();
            // Save the label in userData so it can be recovered upon loading.
            attr.userData.stored = {
                label: getFigureLabel(figure)
            }
            result.push(attr);
        });
        canvas.getLines().each(function(i, element){
            result.push(element.getPersistentAttributes());
        });
        return draw2d.util.Base64.encode(JSON.stringify(result, null, 2));
    }

    /// Load canvas state produced by getCanvasStateAsString.
    function loadCanvasStateFromString (canvas, state) {
        const jsonState = JSON.parse(atob(state));
        let reader = new draw2d.io.json.Reader();
        reader.unmarshal(canvas, jsonState);
    }

    return {
        getCanvasStateAsString: getCanvasStateAsString,
        loadCanvasStateFromString: loadCanvasStateFromString,
    }
})()