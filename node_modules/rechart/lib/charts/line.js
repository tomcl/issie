var Chart, Core, Line, React, _extend, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

_extend = require('lodash/object/extend');

Chart = require('../chart');

React = require('react');

Core = require('./core');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Line = (function(superClass) {
  extend(Line, superClass);

  function Line() {
    this.render = bind(this.render, this);
    return Line.__super__.constructor.apply(this, arguments);
  }

  Line.propTypes = _extend({}, Core.propTypes, {
    scales: object,
    stacked: bool
  });

  Line.defaultProps = _extend({}, Core.defautlProps, {
    stacked: false
  });

  Line.prototype.draw = function() {
    Chart.Line(this.canvas, {
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
    return this.chart = new Chart(this.canvas, {
      type: 'line',
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
  };

  Line.prototype.render = function() {
    return React.createElement(Core, React.__spread({}, this.props, {
      "draw": this.draw
    }));
  };

  return Line;

})(React.Component);

module.exports = Line;
