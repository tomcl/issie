draw2d.shape.digital_not = draw2d.SVGFigure.extend({

    NAME:"draw2d.shape.digital_not",

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
        this._super($.extend({width:30, height:30},attr), setter, getter);

        this.createPort("hybrid", new this.MyInputPortLocator());
        this.createPort("hybrid", new this.MyOutputPortLocator());
    },

   getSVG: function(){
        return '<svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" version="1.1">' +
               '<polygon points="0,0 20,15 0,30" stroke="black" stroke-width="1" fill="lightgray" />' +
               '<circle cx="25" cy="15" r="5" stroke="black" stroke-width="1" fill="lightgray" />' +
               '</svg>';
    }
});