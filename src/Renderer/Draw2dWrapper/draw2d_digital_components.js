/**
 * Draw2d extension to draw digital logic components.
 *
 * For every figure it is necessary to define:
 * - componentType, a string representing the type of the component (as it is
 *   expected in Extractor.fs).
 * - width of the svg element
 * - hieght of the svg element
 * - a function that returns the shapes that will form the svg element
 * - other optional parameters.
 * 
 * Some components are very similar, such as Register and RegisterE. It may be
 * a good idea to merge the similar parts?
 */

import * as draw2d from "draw2d"

draw2d.shape.digital = draw2d.SVGFigure.extend({

    NAME:"draw2d.shape.digital",

    init: function(attr, setter, getter ){
        this._super($.extend({bgColor: "lightgrey"}, attr), setter, getter);
        this.on("change", () => {
            if (dispatchHasUnsavedChangesMessage !== "undefined") {
                dispatchHasUnsavedChangesMessage(true);
            } else {
                console.log("Warning: trying to dispatch a JS HasChanges message but dispatcher is not defined.")
            }
        });
    },

    repaint: function(attributes) {
        if (this.repaintBlocked===true || this.shape === null) {
            return;
        }
        attributes= attributes || {};
        // Redirect the backgroundColor to an internal SVG node.
        // In this case only a small part of the shape are filled with the background color
        // and not the complete rectangle/bounding box.
        attributes["fill"] = "none";
        if (this.bgColor != null) {
            let svgElements = this.getSvgElements();
            for (let i = 0; i < svgElements.length; i++) {
                if (svgElements[i].toFill) {
                    this.svgNodes[i].attr({fill: this.bgColor.hash()});
                }
            }
        }
        this._super(attributes);
        return this;
    },

    getSVG: function() {
        let svgFigure = `<svg xmlns="http://www.w3.org/2000/svg" width="${this.svgWidth}" height="${this.svgHeight}" version="1.1">`;
        let svgElements = this.getSvgElements();
        for (let i = 0; i < svgElements.length; i++) {
            svgFigure += svgElements[i].path;
        }
        svgFigure += '</svg>'
        return svgFigure;
    },

    createDigitalPort: function(type, locator, isBusPort) {
        let port = this.createPort(type, locator);
        port.isBusPort = isBusPort;
    },
});

draw2d.shape.digital.Input = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Input",

    componentType : "Input",
    svgHeight : 20,
    svgWidth : 30,
    numberOfBits : 1,

    getSvgElements : function() {
        return [
            {path: '<polygon points="0,0 20,0 30,10 20,20 0,20" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true}
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBits === "number", "numberOfBits is not a number when creating an Input node");
        this.numberOfBits = attr.numberOfBits;

        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), this.numberOfBits > 1);
    },
});



draw2d.shape.digital.Output = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Output",

    componentType : "Output", 
    svgHeight : 20,
    svgWidth : 30,
    numberOfBits : 1, // TODO: is this necessary? It can be inferred.

    getSvgElements : function() {
        return [
            {path: '<polygon points="0,10 10,0 30,0 30,20 10,20" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true}
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBits === "number", "numberOfBits is not a number when creating an Output node");
        this.numberOfBits = attr.numberOfBits;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), this.numberOfBits > 1);
    },
});

draw2d.shape.digital.Label = draw2d.shape.digital.extend({

    NAME: "draw2d.shape.digital.Label",

    componentType: "Label",
    svgHeight: 20,
    svgWidth: 30,


    getSvgElements: function () {
        return [
            { path: '<polygon points="0,10 10,0 20,0 30,10 20,20 10,20" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true }
        ]
    },

    init: function (attr, setter, getter) {
        this._super(
            $.extend({ width: this.svgWidth, height: this.svgHeight }, attr),
            setter,
            getter
        );


        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Not = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Not",

    componentType : "Not",
    svgHeight : 42,
    svgWidth : 42,

    getSvgElements : function() {
        return [
            {path: '<polygon points="0,0 28,21 0,42" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
            {path: '<circle cx="35" cy="21" r="7" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.And = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.And",

    componentType : "And",
    svgHeight : 40,
    svgWidth : 40,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 0 L 20 0 A 20 20, 0, 0, 1, 20 40 L 0 40 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0,25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0,75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Or = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Or",

    componentType : "Or",
    svgHeight : 40,
    svgWidth : 40,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Xor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Xor",

    componentType : "Xor",
    svgHeight : 40,
    svgWidth : 40,

    getSvgElements : function() {
        return [
            {path: '<path d="M -5 0 Q 10 20 -5 40" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
            {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Nand = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Nand",

    componentType : "Nand",
    svgHeight : 40,
    svgWidth : 50,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 0 L 20 0 A 20 20, 0, 0, 1, 20 40 L 0 40 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<circle cx="45" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Nor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Nor",

    componentType : "Nor",
    svgHeight : 40,
    svgWidth : 48,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<circle cx="43" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Xnor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Xnor",

    componentType : "Xnor",
    svgHeight : 40,
    svgWidth : 48,

    getSvgElements : function() {
        return [
            {path: '<path d="M -5 0 Q 10 20 -5 40" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
            {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<circle cx="43" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 25), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 75), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});


draw2d.shape.digital.Mux2 = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Mux2",

    componentType : "Mux2",
    svgHeight : 50,
    svgWidth : 30,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 0 L 30 13 L 30 37 L 0 50 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="4" y="11" fill="black" font-family="monospace">0</text>', toFill: false},
            {path: '<text x="4" y="31" fill="black" font-family="monospace">1</text>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 30), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 70), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 90), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Demux2 = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Demux2",

    componentType : "Demux2",
    svgHeight : 50,
    svgWidth : 30,

    getSvgElements : function() {
        return [
            {path: '<path d="M 0 13 L 30 0 L 30 50 L 0 37 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="19" y="11" fill="black" font-family="monospace">0</text>', toFill: false},
            {path: '<text x="19" y="31" fill="black" font-family="monospace">1</text>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 90), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 30), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 70), false);
    },
});

draw2d.shape.digital.NbitsAdder = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.NbitsAdder",

    componentType : "NbitsAdder",
    svgHeight : 120,
    svgWidth : 100,
    numberOfBits : null, // int

    getSvgElements : function() {
        const title = this.numberOfBits == 1 ? "1-bit-adder" : `${this.numberOfBits}-bits-adder`
        return [
            {path: '<rect width="100" height="120" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: `<text x="14" y="3" fill="black" font-family="monospace">${title}</text>`, toFill: false},
            {path: '<text x="8" y="25" fill="black" font-family="monospace">Cin</text>', toFill: false},
            {path: `<text x="8" y="55" fill="black" font-family="monospace">A</text>`, toFill: false},
            {path: `<text x="8" y="85" fill="black" font-family="monospace">B</text>`, toFill: false},
            // Out.
            {path: `<text x="75" y="35" fill="black" font-family="monospace">Sum</text>`, toFill: false},
            {path: `<text x="70" y="75" fill="black" font-family="monospace">Cout</text>`, toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBits === "number", "numberOfBits is not a number when creating a NbitsAdder component");
        this.numberOfBits = attr.numberOfBits;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

/**
 * Custom components.
 */

draw2d.shape.digital.Custom = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Custom",

    componentType : "Custom",
    svgHeight : 0,
    svgWidth : 0,
    svgElements : [],

    getSvgElements : function() { return this.svgElements; },

    maxStringLen: function(arr) {
        let max = 0;
        for (let i = 0; i < arr.length; i++) {
            max = Math.max(max, arr[i][0].length);
        }
        return max;
    },

    init: function(attr, setter, getter){
        this._super(attr, setter, getter);
        this.inputs = attr.inputs;   // List of tuples: string * int.
        this.outputs = attr.outputs; // List of tuples: string * int.
        this.customComponentName = attr.name; // String.

        const portSpace = 30;
        const padding = 7;
        const fontHeight = 10;
        const fontWidth = 6;
        this.svgHeight = Math.max(this.inputs.length, this.outputs.length) * portSpace;
        this.svgWidth = Math.max(
            50,
            30 + (this.maxStringLen(this.inputs) + this.maxStringLen(this.outputs)) * fontWidth
        )

        this.svgElements = [{
            path: `<rect height="${this.svgHeight}" width="${this.svgWidth}" stroke="black" stroke-width="1" fill="lightgray"/>`,
            toFill: true
        }]

        for (let i = 0; i < this.inputs.length; i++) {
            const inputLabel = this.inputs[i][0];
            const inputWidth = this.inputs[i][1];
            this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), inputWidth > 1);
            const x = padding;
            const y = this.svgHeight / (this.inputs.length + 1) * (i + 1) - fontHeight / 2;
            this.svgElements.push({
                path: `<text x="${x}" y="${y}" fill="black" font-family="monospace">${inputLabel}</text>`,
                toFill: false
            });
        }
        for (let i = 0; i < this.outputs.length; i++) {
            const outputLabel = this.outputs[i][0];
            const outputWidth = this.outputs[i][1];
            this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), outputWidth > 1);
            const x = this.svgWidth - padding - fontWidth * outputLabel.length;
            const y = this.svgHeight / (this.outputs.length + 1) * (i + 1) - fontHeight / 2;
            this.svgElements.push({
                path: `<text x="${x}" y="${y}" fill="black" font-family="monospace">${outputLabel}</text>`,
                toFill: false
            });
        }
    },
});

/**
 * Buses.
 */

let bitStyle = 'stroke="black" stroke-width="1"';
let busStyle = 'stroke="purple" stroke-width="3"';

draw2d.shape.digital.MergeWires = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.MergeWires",

    componentType : "MergeWires",
    svgHeight : 30,
    svgWidth : 40,
    topInputWidth : null,
    bottomInputWidth : null,
    outputWidth : null,

    getSvgElements : function() {
        let topStyle = (this.topInputWidth && this.topInputWidth >= 2) ? busStyle : bitStyle;
        let bottomStyle = (this.bottomInputWidth && this.bottomInputWidth >= 2) ? busStyle : bitStyle;
        let svgElements = [
            // Horizontal line in center.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth}" y2="${this.svgHeight/2}" ${busStyle} />`, toFill: false},
            // Vertical to top.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="0" ${topStyle} />`, toFill: false},
            // Top horizontal.
            {path: `<line x1="0" y1="0" x2="${this.svgWidth/2}" y2="0" ${topStyle} />`, toFill: false},
            // Vertical to bottom.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" ${bottomStyle} />`, toFill: false},
            // Bottom horizontal.
            {path: `<line x1="0" y1="${this.svgHeight}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" ${bottomStyle} />`, toFill: false},
        ];
        if (this.topInputWidth && this.bottomInputWidth && this.outputWidth) {
            let topInputLabel =
                this.topInputWidth >= 2 ? `[${this.topInputWidth - 1}..0]`
                                        : '[0]';
            let bottomInputLabel =
                this.bottomInputWidth >= 2 ? `[${this.outputWidth - 1}..${this.topInputWidth}]`
                                           : `[${this.topInputWidth}]`;
            let outputLabel = `[${this.outputWidth - 1}..0]`;
            // Add labels on the wires.
            svgElements = svgElements.concat([
                {path: `<text x="0" y="-15" fill="black">${topInputLabel}</text>`, toFill: false},
                {path: `<text x="0" y="${this.svgHeight+2}" fill="black">${bottomInputLabel}</text>`, toFill: false},
                {path: `<text x="${this.svgWidth-15}" y="${this.svgHeight/2+2}" fill="black">${outputLabel}</text>`, toFill: false},
            ]);
        }
        return svgElements;
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 0), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 100), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 50), true);
    },
});

draw2d.shape.digital.SplitWire = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.SplitWire",

    componentType : "SplitWire",
    svgHeight : 30,
    svgWidth : 40,
    inputWidth : null,
    topOutputWidth : null, // Set by the user, not just inferred.
    bottomOutputWidth : null,

    getSvgElements : function() {
        let topStyle = (this.topOutputWidth && this.topOutputWidth >= 2) ? busStyle : bitStyle;
        let bottomStyle = (this.bottomOutputWidth && this.bottomOutputWidth >= 2) ? busStyle : bitStyle;
        let svgElements = [
            // Horizontal line in center.
            {path: `<line x1="0" y1="${this.svgHeight/2}" x2="20" y2="${this.svgHeight/2}" stroke="purple" stroke-width="3" />`, toFill: false },
            // Vertical to top.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="0" ${topStyle} />`, toFill: false },
            // Top horizontal.
            {path: `<line x1="${this.svgWidth/2}" y1="0" x2="${this.svgWidth}" y2="0" ${topStyle} />`, toFill: false },
            // Vertical to bottom.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" ${bottomStyle} />`, toFill: false },
            // Bottom horizontal.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight}" x2="${this.svgWidth}" y2="${this.svgHeight}" ${bottomStyle} />`, toFill: false },
        ];
        if (this.inputWidth && this.topOutputWidth && this.bottomOutputWidth) {
            let topOutputLabel =
                this.topOutputWidth >= 2 ? `[${this.topOutputWidth - 1}..0]`
                                         : '[0]';
            let bottomOutputLabel =
                this.bottomOutputWidth >= 2 ? `[${this.inputWidth - 1}..${this.topOutputWidth}]`
                                            : `[${this.topOutputWidth}]`;
            let inputLabel = `[${this.inputWidth - 1}..0]`;
            // Add labels on the wires.
            svgElements = svgElements.concat([
                {path: `<text x="${this.svgWidth-15}" y="1" fill="black">${topOutputLabel}</text>`, toFill: false},
                {path: `<text x="${this.svgWidth-15}" y="${this.svgHeight+2}" fill="black">${bottomOutputLabel}</text>`, toFill: false},
                {path: `<text x="-9" y="${this.svgHeight/2+2}" fill="black">${inputLabel}</text>`, toFill: false},
            ]);
        }
        return svgElements;
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.topOutputWidth === "number", "topOutputWidth is not a number when creating an SplitWire node");
        this.topOutputWidth = attr.topOutputWidth;

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 50), true);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 0), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 100), false);
    },
});

/**
 * Synchronous logic.
 */

draw2d.shape.digital.DFF = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.DFF",

    componentType : "DFF",
    svgHeight : 50,
    svgWidth : 50,

    getSvgElements : function() {
        return [         
            {path: '<rect width="50" height="50" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="15" y="3" fill="black" font-family="monospace">DFF</text>', toFill: false},
            {path: '<text x="8" y="20" fill="black" font-family="monospace">D</text>', toFill: false},
            {path: '<text x="38" y="20" fill="black" font-family="monospace">Q</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="37" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 38 L 6 43 L 0 48" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.DFFE = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.DFFE",

    componentType : "DFFE",
    svgHeight : 50,
    svgWidth : 80,

    getSvgElements : function() {
        return [         
            {path: '<rect width="80" height="50" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="28" y="3" fill="black" font-family="monospace">DFFE</text>', toFill: false},
            {path: '<text x="8" y="20" fill="black" font-family="monospace">D</text>', toFill: false},
            {path: '<text x="67" y="20" fill="black" font-family="monospace">Q</text>', toFill: false},
            {path: '<text x="35" y="37" fill="black" font-family="monospace">EN</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="37" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 38 L 6 43 L 0 48" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 100), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Register = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Register",

    componentType : "Register",
    svgHeight : 80,
    svgWidth : 120,
    regWidth : null, // int

    getSvgElements : function() {
        return [         
            {path: '<rect width="120" height="80" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: `<text x="50" y="3" fill="black" font-family="monospace">REG${this.regWidth}</text>`, toFill: false},
            {path: '<text x="8" y="35" fill="black" font-family="monospace">data-in</text>', toFill: false},
            {path: '<text x="66" y="35" fill="black" font-family="monospace">data-out</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="67" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 68 L 6 73 L 0 78" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.regWidth === "number", "regWidth is not a number when creating a register component");
        this.regWidth = attr.regWidth;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.RegisterE = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.RegisterE",

    componentType : "RegisterE",
    svgHeight : 80,
    svgWidth : 120,
    regWidth : null, // int

    getSvgElements : function() {
        return [         
            {path: '<rect width="120" height="80" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: `<text x="50" y="3" fill="black" font-family="monospace">REG${this.regWidth}</text>`, toFill: false},
            {path: '<text x="8" y="35" fill="black" font-family="monospace">data-in</text>', toFill: false},
            {path: '<text x="66" y="35" fill="black" font-family="monospace">data-out</text>', toFill: false},
            // Enable.
            {path: '<text x="55" y="67" fill="black" font-family="monospace">EN</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="67" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 68 L 6 73 L 0 78" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.regWidth === "number", "regWidth is not a number when creating a registerE component");
        this.regWidth = attr.regWidth;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 100), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.AsyncROM = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.AsyncROM",

    componentType : "AsyncROM",
    svgHeight : 100,
    svgWidth : 80,
    addressWidth : null, // int
    wordWidth : null,    // int
    memData : null,      // array of Longs (Longs are object created by the Fable compiler to mimic f# int64 type)

    getSvgElements : function() {
        return [         
            {path: '<rect width="80" height="100" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="14" y="3" fill="black" font-family="monospace">Async-ROM</text>', toFill: false},
            {path: '<text x="8" y="45" fill="black" font-family="monospace">addr</text>', toFill: false},
            {path: '<text x="50" y="45" fill="black" font-family="monospace">data</text>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.addressWidth === "number", "addressWidth is not a number when creating an ROM component");
        this.addressWidth = attr.addressWidth;
        console.assert(typeof attr.wordWidth === "number", "wordWidth is not a number when creating an ROM component");
        this.wordWidth = attr.wordWidth;
        console.assert(Array.isArray(attr.memData), "memData is not an array when creating an ROM component");
        this.memData = attr.memData;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.ROM = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.ROM",

    componentType : "ROM",
    svgHeight : 100,
    svgWidth : 80,
    addressWidth : null, // int
    wordWidth : null,    // int
    memData : null,      // array of Longs (Longs are object created by the Fable compiler to mimic f# int64 type)

    getSvgElements : function() {
        return [         
            {path: '<rect width="80" height="100" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="30" y="3" fill="black" font-family="monospace">ROM</text>', toFill: false},
            {path: '<text x="8" y="45" fill="black" font-family="monospace">addr</text>', toFill: false},
            {path: '<text x="50" y="45" fill="black" font-family="monospace">data</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="87" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 88 L 6 93 L 0 98" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.addressWidth === "number", "addressWidth is not a number when creating an ROM component");
        this.addressWidth = attr.addressWidth;
        console.assert(typeof attr.wordWidth === "number", "wordWidth is not a number when creating an ROM component");
        this.wordWidth = attr.wordWidth;
        console.assert(Array.isArray(attr.memData), "memData is not an array when creating an ROM component");
        this.memData = attr.memData;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.RAM = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.RAM",

    componentType : "RAM",
    svgHeight : 100,
    svgWidth : 130,
    addressWidth : null, // int
    wordWidth : null,    // int
    memData : null,      // array of Longs (Longs are object created by the Fable compiler to mimic f# int64 type)

    getSvgElements : function() {
        return [
            {path: '<rect width="130" height="100" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
            {path: '<text x="55" y="3" fill="black" font-family="monospace">RAM</text>', toFill: false},
            {path: '<text x="8" y="20" fill="black" font-family="monospace">addr</text>', toFill: false},
            {path: '<text x="8" y="45" fill="black" font-family="monospace">data-in</text>', toFill: false},
            {path: '<text x="8" y="70" fill="black" font-family="monospace">write</text>', toFill: false},
            {path: '<text x="76" y="45" fill="black" font-family="monospace">data-out</text>', toFill: false},
            // Clock.
            {path: '<text x="8" y="87" fill="black" font-family="monospace">clk</text>', toFill: false},
            {path: '<path d="M 0 88 L 6 93 L 0 98" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        ]
    },

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.addressWidth === "number", "addressWidth is not a number when creating an ROM component");
        this.addressWidth = attr.addressWidth;
        console.assert(typeof attr.wordWidth === "number", "wordWidth is not a number when creating an ROM component");
        this.wordWidth = attr.wordWidth;
        console.assert(Array.isArray(attr.memData), "memData is not an array when creating an ROM component");
        this.memData = attr.memData;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});
