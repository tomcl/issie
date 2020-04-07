// Extension to draw some of the digital logic components required.
// If necessary it is possible to override the portLocator class and use it here.
// TODO: add a type attribute so it is possible to understand which type of
// component this is.

// For every figure it is necessary to define:
// - width of the svg element
// - hieght of the svg element
// - shapes that will form the svg element
// - userData (data accessible from the outside), containing at least:
//   - componentType : string (e.g. "Not", "And", "Mux2", ...)

draw2d.shape.digital = draw2d.SVGFigure.extend({

    NAME:"draw2d.shape.digital",

    init: function(attr, setter, getter ){
        this._super(attr, setter, getter);
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
            for (let i = 0; i < this.svgElements.length; i++) {
                this.svgNodes[i].attr({fill: this.bgColor.hash()});
            }
        }
        this._super(attributes);
        return this;
    },

    getSVG: function() {
        let svgFigure = `<svg xmlns="http://www.w3.org/2000/svg" width="${this.svgWidth}" height="${this.svgHeight}" version="1.1">`;
        for (let i = 0; i < this.svgElements.length; i++) {
            svgFigure += this.svgElements[i];
        }
        svgFigure += '</svg>'
        return svgFigure;
    }
});

draw2d.shape.digital.Not = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Not",

    svgHeight : 30,
    svgWidth : 30,
    svgElements : [
        '<polygon points="0,0 20,15 0,30" stroke="black" stroke-width="1" fill="lightgray" />',
        '<circle cx="25" cy="15" r="5" stroke="black" stroke-width="1" fill="lightgray" />',
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.setUserData({
            componentType : "Not",
        });

        this.createPort("input", new draw2d.layout.locator.InputPortLocator());
        this.createPort("output", new draw2d.layout.locator.OutputPortLocator());
    },
});

draw2d.shape.digital.And = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.And",

    svgHeight : 30,
    svgWidth : 30,
    svgElements : [
        '<path d="M 0 0 L 15 0 A 15 15, 0, 0, 1, 15 30 L 0 30 Z" stroke="black" stroke-width="1" fill="lightgray"/>'
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.setUserData({
            componentType : "And",
        });

        this.createPort("input", new draw2d.layout.locator.InputPortLocator());
        this.createPort("input", new draw2d.layout.locator.InputPortLocator());
        this.createPort("output", new draw2d.layout.locator.OutputPortLocator());
    },
});

draw2d.shape.digital.Mux2 = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Mux2",

    svgHeight : 50,
    svgWidth : 30,
    svgElements : [
        '<path d="M 0 0 L 30 13 L 30 37 L 0 50 Z" stroke="black" stroke-width="1" fill="lightgray"/>'
    ],

    Mux2BottomPortLocator : draw2d.layout.locator.PortLocator.extend({
        init: function( ){
          this._super();
        },
        relocate: function(index, figure){
            var w = figure.getParent().getWidth();
            var h = figure.getParent().getHeight();
            figure.setPosition(w / 2, h - 7);
        }
    }),

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.setUserData({
            componentType : "Mux2",
        });

        this.createPort("input", new draw2d.layout.locator.InputPortLocator());
        this.createPort("input", new draw2d.layout.locator.InputPortLocator());
        this.createPort("input", new this.Mux2BottomPortLocator());
        this.createPort("output", new draw2d.layout.locator.OutputPortLocator());
    },
});

