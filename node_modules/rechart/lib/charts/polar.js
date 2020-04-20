var Chart, Core, Polar, React, _extend, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

_extend = require('lodash/object/extend');

Chart = require('../chart');

React = require('react');

Core = require('./core');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Polar = (function(superClass) {
  extend(Polar, superClass);

  function Polar() {
    this.render = bind(this.render, this);
    return Polar.__super__.constructor.apply(this, arguments);
  }

  Polar.propTypes = _extend({}, Core.propTypes, {
    scaleShowLabelBackdrop: bool,
    scaleBackdropColor: string,
    scaleBeginAtZero: bool,
    scaleBackdropPaddingY: number,
    scaleBackdropPaddingX: number,
    scaleShowLine: bool,
    segmentShowStroke: bool,
    segmentStrokeColor: string,
    segmentStrokeWidth: number,
    animationSteps: number,
    animationEasing: string,
    animateRotate: bool,
    animateScale: bool
  });

  Polar.defaultProps = _extend({}, Core.defaultProps, {
    scaleShowLabelBackdrop: true,
    scaleBackdropColor: "rgba(255,255,255,0.75)",
    scaleBeginAtZero: true,
    scaleBackdropPaddingY: 2,
    scaleBackdropPaddingX: 2,
    scaleShowLine: true,
    segmentShowStroke: true,
    segmentStrokeColor: "#fff",
    segmentStrokeWidth: 2,
    animationSteps: 100,
    animationEasing: "easeOutBounce",
    animateRotate: true,
    animateScale: false
  });

  Polar.prototype.draw = function() {
    return Chart.PolarArea(this.canvas, {
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
  };

  Polar.prototype.render = function() {
    return React.createElement(Core, React.__spread({}, this.props, {
      "draw": this.draw
    }));
  };

  return Polar;

})(React.Component);

module.exports = Polar;
