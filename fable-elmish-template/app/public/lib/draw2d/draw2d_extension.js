draw2d.shape.digital_not = draw2d.SVGFigure.extend({

    NAME:"draw2d.shape.digital_not",

    svgHeight : 30,
    svgWidth: 30,
    svgElements: [
        '<polygon points="0,0 20,15 0,30" stroke="black" stroke-width="1" fill="lightgray" />',
        '<circle cx="25" cy="15" r="5" stroke="black" stroke-width="1" fill="lightgray" />',
    ],

    // Custom locator for the special design of the Input area.
    MyInputPortLocator : draw2d.layout.locator.PortLocator.extend({
        init: function( ){
            this._super();
        },
        relocate: function(index, figure){
            var h = figure.getParent().getHeight();
            figure.setPosition(0, h/2);
        }
    }),

    // custom locator for the special design of the Output area
    MyOutputPortLocator : draw2d.layout.locator.PortLocator.extend({
        init: function( ){
            this._super();
        },    
        relocate: function(index, figure){
            var w = figure.getParent().getWidth();
            var h = figure.getParent().getHeight();
            figure.setPosition(w, h/2);
        }
    }),

    init: function(attr, setter, getter ){
        this._super($.extend({width:this.svgWidth, height:this.svgHeight}, attr), setter, getter);

        this.createPort("input", new this.MyInputPortLocator());
        this.createPort("output", new this.MyOutputPortLocator());
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