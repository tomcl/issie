function lineLineIntersection(line1, line2) {
    // Lines as vectors
    var dx1 = line1.p2.x - line1.p1.x;
    var dy1 = line1.p2.y - line1.p1.y;
    var dx2 = line2.p2.x - line2.p1.x;
    var dy2 = line2.p2.y - line2.p1.y;

    // Shift lines so line1.p1 is at 0
    var dx3 = line2.p1.x - line1.p1.x;
    var dy3 = line2.p1.y - line1.p1.y;
    
    var cross = dx2 * dy1 - dy2 * dx1;
    if (Math.abs(cross) < 1e-8) { return false; }

    // Find proportion along line 2
    var s = (dx1 * dy3 - dy1 * dx3) / cross;
    
    // Check point is on line 2
    if (s >= 0 && s <= 1) {
        // Find proportion along line 1
        var t = dx1 !== 0 ? (dx3 + dx2 * s) / dx1 : (dy3 + dy2 * s) / dy1;

        // Check point is on line 1
        if (t >= 0 && t <= 1) {
            // Return intersection
            return {
                x: line2.p1.x + dx2 * s,
                y: line2.p1.y + dy2 * s
            };
        }
    }
}

function defineCircleFromThreePoints(p1, p2, p3) {
    var dx1 = p1.x - p2.x;
    var dy1 = p1.y - p2.y;
    var dx2 = p2.x - p3.x;
    var dy2 = p2.y - p3.y;

    var cross = dx2 * dy1 - dy2 * dx1;

    // If points are colinear(ish), we have a line.
    if (Math.abs(cross) < 0.01) {
        var d = Math.sqrt(dx1 * dx1 + dy1 * dy1);
        // TODO: improve how points are picked
        dx1 *= 2000 / d;
        dy1 *= 2000 / d;
        
        return {
            p1: { x: p1.x + dx1, y: p1.y + dy1},
            p2: { x: p1.x - dx1, y: p1.y - dy1}
        };
    }

    // Mid-point coordinates
    var mx1 = (p1.x + p2.x) / 2;
    var my1 = (p1.y + p2.y) / 2;
    var mx2 = (p2.x + p3.x) / 2;
    var my2 = (p2.y + p3.y) / 2;

    // Find intersection of lines in terms of position along 2 bisector
    var s = (-dy1 * (my2 - my1) - dx1 * (mx2 - mx1)) / cross;

    // Center of circle is along the line from (mx2, my2) in the direction (dy2, dx2)
    var cx = mx2 - s * dy2;
    var cy = my2 + s * dx2;

    // Find radius
    var dx = cx - p1.x;
    var dy = cy - p1.y;
    var r = Math.sqrt(dx * dx + dy * dy);

    return {cx: cx, cy: cy, r: r};
}

function findMidPoint(A, B) {
    return { cx: (A.x + B.x) / 2, cy: (A.y + B.y) / 2 };
}

var InteractiveSVG = (function() {
    var xmlns = 'http://www.w3.org/2000/svg';

    /*************************************************
     *      SVG Element Object
     *  A object that wraps an SVG element.
    **************************************************/

    var SVGElement = function(svgObject, attributes, hiddenAttributes) {
        this.svg = svgObject;
        hiddenAttributes = ['static', 'label', 'draggable'].concat(hiddenAttributes || []);

        this.proxyAttributes = this.proxyAttributes || {};

        // Map attributes that this object to list of objects that share that attribute
        this.linkedAttributes = {};

        // hiddenAttributes are attributes for the SVGElement object, but not for SVG element itself.
        for (var i = 0; i < hiddenAttributes.length; i++) {
            var attributeName = hiddenAttributes[i];
            if (attributes[attributeName] !== undefined) {
                this[attributeName] = attributes[attributeName];
                delete attributes[attributeName];
            }
        }

        this.update(attributes);

        if (this.draggable) { svgObject._setAsDraggable(this); }

        if (this.label) { svgObject.elements[this.label] = this; }
    };

    // Update the object with a key, value map of attributes
    SVGElement.prototype.update = function(attributes) {
        // Update linked attributes
        for (var attributeName in attributes) {
            var value = attributes[attributeName];

            this.updateAttribute(attributeName, value);

            var linkedAttributes = this.linkedAttributes[attributeName];
            if (linkedAttributes) {
                for (var i = 0; i < linkedAttributes.length; i++) {
                    this.linkedAttributes[attributeName][i](value);
                }
            }
        }
    };

    // Update the object with a given attribute and value
    SVGElement.prototype.updateAttribute = function(attributeName, value) {
        // Update object attributes
        this[attributeName] = value;

        // Update SVG element attributes
        if (this.proxyAttributes[attributeName]) {
            this.proxyAttributes[attributeName](this.$element, value);
        } else {
            this.$element.attr(attributeName, value);
        }
    };

    SVGElement.prototype.translate = function(dx, dy) {
        this.update({ x: this.x + dx, y: this.y + dy });
    };

    SVGElement.prototype._setAttrIfNotYetSet = function(attributes) {
        var el = this.$element[0];
        for (var attributeName in attributes) {
            if (!el.hasAttribute(attributeName)) {
                this.$element.attr(attributeName, attributes[attributeName]);
            }
        }
    };

    /*************************************************
     *      InteractivePoint
     *  An SVG circle which can be draggable.
    **************************************************/

    var InteractivePoint = function(svgObject, attributes) {
        this.$element = svgObject.addElement('circle');
        this.draggable = !attributes.static;
        
        // Changing this object's x and y attributes changes its element's cx and cy attributes
        this.proxyAttributes = {
            x: function(el, value) { el.attr('cx', value); },
            y: function(el, value) { el.attr('cy', value); }
        };

        SVGElement.call(this, svgObject, attributes);
     
        // Set attributes
        this._setAttrIfNotYetSet({
            'r': this.draggable ? 6 : 3,
            'class': this.draggable ? "draggable draggable-point" : "static-point"
        });

        // Set classes
        this.$element.addClass("point");
    };
    InteractivePoint.prototype = Object.create(SVGElement.prototype);

    /*************************************************
     *      InteractiveLine
     *  A line between two draggable points
    **************************************************/

    var InteractiveLine = function(svgObject, attributes) {
        this.$element = svgObject.addElementToBottom('line');

        SVGElement.call(this, svgObject, attributes, ['p1', 'p2']);

        // Create points
        this.addPoint(svgObject, 1);
        this.addPoint(svgObject, 2);

        // Set class
        var className = ((this.p1 && this.p1.draggable) || (this.p2 && this.p2.draggable)) ? "controllable-line" : "static-line";
        this._setAttrIfNotYetSet({ 'class': className });
        this.$element.addClass("line");
    };
    InteractiveLine.prototype = Object.create(SVGElement.prototype);

    InteractiveLine.prototype.addPoint = function(svgObject, n) {
        var p = 'p' + n;

        if (this[p]) {
            p = svgObject.getElement(this[p]);
            this[p] = p;
            if (p instanceof SVGElement) {
                svgObject.linkAttributes(this, 'x' + n, p, 'x');
                svgObject.linkAttributes(this, 'y' + n, p, 'y');
            } else {
                this.updateAttribute('x' + n, p.x || 0);
                this.updateAttribute('y' + n, p.y || 0);
            }
        }
    };

    /*************************************************
     *      InteractiveBezier
     *  A cubic Bezier path between two draggable points
     *  with two draggable control points.
    **************************************************/

    var InteractiveBezier = function(svgObject, attributes) {
        this.tagName = "path";
        this.addBelow = true;
        var reservedAttributes = ['label', 'p1', 'p2', 'p3', 'p4', 'showHandles'];

        SVGElement.call(this, svgObject, reservedAttributes, attributes);
        
        this.p1 = svgObject.getElement(this.p1);
        this.p2 = svgObject.getElement(this.p2);
        this.p3 = svgObject.getElement(this.p3);
        this.p4 = svgObject.getElement(this.p4);

        if (this.p4) {
            // Cubic bezier
            this.addDependency(
                [this.p1, this.p2, this.p3, this.p4],
                function(p1, p2, p3, p4) {
                    var d = "M" + p1.x + " " + p1.y;
                    d += "C" + p2.x + " " + p2.y;
                    d += " " + p3.x + " " + p3.y;
                    d += " " + p4.x + " " + p4.y;
                    return { d: d };
            });

            if (this.showHandles) {
                svgObject.addLine({ p1: this.p1, p2: this.p2, class: "line static-line" });
                svgObject.addLine({ p1: this.p3, p2: this.p4, class: "line static-line" });
            }
        } else {
            // Quadratic bezier
            this.addDependency(
                [this.p1, this.p2, this.p3],
                function(p1, p2, p3) {
                    var d = "M" + p1.x + " " + p1.y;
                    d += "S" + p2.x + " " + p2.y;
                    d += " " + p3.x + " " + p3.y;
                    return { d: d };
            });

            if (this.showHandles) {
                svgObject.addLine({ p1: this.p1, p2: this.p2, class: "line static-line" });
                svgObject.addLine({ p1: this.p2, p2: this.p3, class: "line static-line" });
            }
        }

        this.$element.addClass("line");
        this.$element.addClass("controllable-line");
    };
    InteractiveBezier.prototype = Object.create(SVGElement.prototype);

    /*************************************************
     *      InteractiveCircle
     *  A circle which can be dragged by its center.
    **************************************************/

    var InteractiveCircle = function(svgObject, attributes) {
        this.tagName = 'circle';
        this.addBelow = true;
        var reservedAttributes = ['label', 'x', 'y', 'center', 'r'];

        SVGElement.call(this, svgObject, reservedAttributes, attributes);

        // Center can be defined by a center attribute, (x, y) attributes or (cx, cy) attributes
        if (this.center) {
            this.center = svgObject.getElement(this.center);
        } else if (this.x !== undefined && this.y !== undefined) {
            this.center = { x: this.x, y: this.y };
        } else {
            this.center = { x: attributes.cx || 0, y: attributes.cy || 0 };
        }

        // Circle coordinates depend on its center point
        this.addDependency(this.center, function(center) {
            return { cx: center.x, cy: center.y };
        });

        // Radius can be a number or determined by a points
        if (isNaN(this.r)) {
            this.r = svgObject.getElement(this.r);

            // Radius of the circle is dependent on point this.r
            this.addDependency(this.r, function(radiusPoint) {
                radiusPoint.dx = radiusPoint.x - this.center.x;
                radiusPoint.dy = radiusPoint.y - this.center.y;
                return { r: Math.sqrt(radiusPoint.dx * radiusPoint.dx + radiusPoint.dy * radiusPoint.dy) };
            });

            // Move the radius point when the center is moved
            this.r.addDependency(this.center, function(center) {
                return { cx: center.x + this.dx, cy: center.y + this.dy };
            });
        } else {
            this.update({ r: this.r });
        }

        // Set classes
        var className = (this.center.draggable || this.r.draggable) ? 'controllable-line' : 'static-line';
        this._setAttrIfNotYetSet({ 'class': className });
        this.$element.addClass("line");
    };
    InteractiveCircle.prototype = Object.create(SVGElement.prototype);

    /*************************************************
     *      InteractiveText
     *  A text element whose text content is variable.
    **************************************************/

    var InteractiveText = function(svgObject, attributes, parentTextElement) {
        if (parentTextElement) {
            this.$element = svgObject.addChildElement('tspan', parentTextElement);
        } else {
            this.$element = svgObject.addElement('text');
        }

        this.proxyAttributes = {
            value: function(el, value) { el.html(value); }
        };

        SVGElement.call(this, svgObject, attributes, ["scrubber", "scrubberScale", "hiddenValue"]);

        // Create text node
        this.$element.html(this.value === undefined ? "" : this.value);

        if (this.draggable || this.scrubber) {
            this.$element.addClass("draggable");

            // Hidden value used for scrubber
            if (this.scrubber) {
                this.scrubberScale = this.scrubberScale || 1;
                svgObject._setAsDraggable(this);
            }
        }
    };
    InteractiveText.prototype = Object.create(SVGElement.prototype);

    // Scrubbing numbers horizontally changes its value
    InteractiveText.prototype.translate = function(dx, dy) {
        if (this.draggable) {
            this.update({ x: this.x + dx, y: this.y + dy });
        }
        if (this.scrubber) {
            var n = parseFloat(this.value);
            if (!isNaN(n)) {
                n = Math.round((n + this.scrubberScale * dx) * 1000) / 1000;
                this.update({ value: n });
            }
        }
    };

    InteractiveText.prototype.addChild = function(attributes) {
        return new InteractiveText(this.svg, attributes, this.$element);
    };

    /*************************************************
     *      InteractiveSVG
     *  Main object for the whole SVG.
    **************************************************/

    var InteractiveSVG = function($container, width, height) {
        this.$svg = $(document.createElementNS(xmlns, 'svg'));
        this.$svg.attr({
            xmlns: xmlns,
            class: 'interactiveSVG',
            width: width || 400,
            height: height || 400,
        }).appendTo($container);

        this.elements = {};
        this.selected = false;
        this._addMouseEventHandlers();
        this.$background = this._addBackground();
    };

    InteractiveSVG.create = function(id, width, height) {
        var $container = $('#' + id);
        if (!$container) {
            console.error("No element found with id " + id);
            return;   
        }
        return new InteractiveSVG($container, width, height);
    };

    InteractiveSVG.createFromJSON = function(data) {
        if (!data.id) {
            console.error("No id given");
            return;
        }

        var svgObject = this.create(data.id, data.width, data.height);
        this._addFromJSON(svgObject.addPoint.bind(svgObject), data.points);
        this._addFromJSON(svgObject.addLine.bind(svgObject), data.lines);
        this._addFromJSON(svgObject.addCircle.bind(svgObject), data.circles);

        return svgObject;
    };

    InteractiveSVG._addFromJSON = function(addFunction, arr) {
        if (arr) {
            for (var i = 0; i < arr.length; i++) {
                addFunction(arr[i]);
            }
        }
    };

    InteractiveSVG.prototype._addBackground = function() {
        return this.addElement('rect').attr({
            class: 'background',
            width: this.$svg.attr('width'),
            height: this.$svg.attr('height')
        });
    };

    InteractiveSVG.prototype._addMouseEventHandlers = function() {
        var self = this;

        this.$svg.on('mousemove', function(evt) {
            if (self.selected) {
                evt.preventDefault();

                // Get dragging to work on touch device
                if (evt.type === 'touchmove') { evt = evt.touches[0]; }

                // Move based on change in mouse position
                self.selected.translate(
                    evt.clientX - self.dragX,
                    evt.clientY - self.dragY
                );

                // Update mouse position
                self.dragX = evt.clientX;
                self.dragY = evt.clientY;
            }
        });

        this.$svg.on('mouseup', function() {
            self.selected = false;
        });
    };

    InteractiveSVG.prototype._setAsDraggable = function(element) {
        var self = this;
        element.$element.on('mousedown', function(evt) {
            self.selected = element;

            // Get dragging to work on touch device
            if (evt.type === 'touchstart') { evt = evt.touches[0]; }
            self.dragX = evt.clientX;
            self.dragY = evt.clientY;
        });
    };

    InteractiveSVG.prototype.getElement = function(label) {
        // If label is a string then look it up in the points dictionary
        if (typeof label === 'string') {
            var element = this.elements[label];
            if (element) {
                return element;
            } else {
                console.error("No such element with name " + label);
            }
        } else {
            return label;
        }
    };

    // Link two attributes of two objects so they have the same value and maintain the same value
    InteractiveSVG.prototype.linkAttributes = function(object1, attribute1, object2, attribute2) {
        this._setLinkedAttributeFunction(object1, attribute1, object2, attribute2);
        // Call the second function to update object 1 attribute to match object 2
        this._setLinkedAttributeFunction(object2, attribute2, object1, attribute1 , true);
    };

    // Add a function to the linkedAttributes map on object1 so that when attribute1 of object1 is updated
    // attribute2 of object2 is also updated.
    InteractiveSVG.prototype._setLinkedAttributeFunction = function(object1, attribute1, object2, attribute2, toCall) {
        var updateFunction = function(value) {
            object2.updateAttribute(attribute2, value);
        };

        if (object1.linkedAttributes[attribute1]) {
            object1.linkedAttributes[attribute1].push(updateFunction);
        } else {
            object1.linkedAttributes[attribute1] = [updateFunction];
        }

        if (toCall) { updateFunction(object1[attribute1]); }
    };

    InteractiveSVG.prototype.addPoint = function(attributes) {
        if (arguments.length > 1) {
            attributes = { x: arguments[0], y: arguments[1] };
        }
        return new InteractivePoint(this, attributes);
    };

    InteractiveSVG.prototype.addStaticPoint = function(attributes) {
        if (arguments.length > 1) {
            attributes = { x: arguments[0], y: arguments[1] };
        }
        attributes.static = true;
        return new InteractivePoint(this, attributes);
    };

    InteractiveSVG.prototype.addMidPoint = function(point1, point2) {
        point1 = this.getElement(point1);
        point2 = this.getElement(point2);

        return this.addPoint({
            r: 4, static: true, class: 'generated-point'
        }).addDependency([point1, point2], findMidPoint);
    };

    InteractiveSVG.prototype.addLine = function(attributes) {
        if (Array.isArray(attributes)) {
            var i, lines = [];
            for (i = 1; i < attributes.length; i++) {
                lines.push(
                    new InteractiveLine(this, { p1: attributes[i - 1], p2: attributes[i] })
                );
            }
            return lines;
        }

        return new InteractiveLine(this, attributes);
    };

    InteractiveSVG.prototype.addBezier = function(attributes, attr2) {
        if (Array.isArray(attributes)) {
            var i, newAttr = attr2 || {};
            for (i = 0; i < attributes.length; i++) {
                newAttr['p' + (i + 1)] = attributes[i];
            }
            attributes = newAttr;
        }
        return new InteractiveBezier(this, attributes);
    };

    InteractiveSVG.prototype.addCircle = function(attributes) {
        return new InteractiveCircle(this, attributes);
    };

    InteractiveSVG.prototype.addText = function(attributes) {
        return new InteractiveText(this, attributes);
    };

    InteractiveSVG.prototype.addElement = function(tagName) {
        return $(document.createElementNS(xmlns, tagName)).appendTo(this.$svg);
    };

    InteractiveSVG.prototype.addElementToBottom = function(tagName) {
        return $(document.createElementNS(xmlns, tagName)).insertAfter(this.$background);
    };

    InteractiveSVG.prototype.addChildElement = function(tagName, $parent) {
        return $(document.createElementNS(xmlns, tagName)).appendTo($parent);
    };

    return InteractiveSVG;
})();
