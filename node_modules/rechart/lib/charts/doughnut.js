var Chart, Core, Doughnut, React, _extend, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

_extend = require('lodash/object/extend');

Chart = require('../chart');

React = require('react');

Core = require('./core');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Doughnut = (function(superClass) {
  extend(Doughnut, superClass);

  function Doughnut() {
    this.render = bind(this.render, this);
    return Doughnut.__super__.constructor.apply(this, arguments);
  }

  Doughnut.propTypes = _extend({}, Core.propTypes, {
    segmentShowStroke: bool,
    segmentStrokeColor: string,
    segmentStrokeWidth: number,
    percentageInnerCutout: number,
    animationSteps: number,
    animationEasing: string,
    animateRotate: bool,
    animateScale: bool
  });

  Doughnut.defaultProps = _extend({}, Core.defautlProps, {
    segmentShowStroke: true,
    segmentStrokeColor: "#fff",
    segmentStrokeWidth: 2,
    percentageInnerCutout: 50,
    animationSteps: 100,
    animationEasing: "easeOutBounce",
    animateRotate: true,
    animateScale: false
  });

  Doughnut.prototype.draw = function() {
    return this.chart = new Chart(this.canvas, {
      type: 'doughnut',
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
  };

  Doughnut.prototype.render = function() {
    return React.createElement(Core, React.__spread({}, this.props, {
      "draw": this.draw
    }));
  };

  return Doughnut;

})(React.Component);

module.exports = Doughnut;
