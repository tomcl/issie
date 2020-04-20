var Chart, Core, Radar, React, _extend, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

_extend = require('lodash/object/extend');

Chart = require('../chart');

React = require('react');

Core = require('./core');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Radar = (function(superClass) {
  extend(Radar, superClass);

  function Radar() {
    this.render = bind(this.render, this);
    return Radar.__super__.constructor.apply(this, arguments);
  }

  Radar.propTypes = _extend({}, Core.propTypes, {
    angleLineColor: string,
    angleLineWidth: number,
    angleShowLineOut: bool,
    datasetFill: bool,
    dataStroke: bool,
    dataStrokeWidth: number,
    pointLabelFontColor: string,
    pointDot: bool,
    pointDotRadius: number,
    pointDotStrokeWidth: number,
    pointHitDetectionRadius: number,
    pointLabelFontFamily: string,
    pointLabelFontSize: number,
    pointLabelFontStyle: string,
    scaleBeginAtZero: bool,
    scaleShowLabels: bool,
    scaleShowLine: bool
  });

  Radar.defaultProps = _extend({}, Core.defaultProps, {
    angleLineColor: "rgba(0,0,0,.1)",
    angleLineWidth: 1,
    angleShowLineOut: true,
    datasetFill: true,
    dataStroke: true,
    dataStrokeWidth: 2,
    pointLabelFontColor: '#666',
    pointDot: true,
    pointDotRadius: 3,
    pointDotStrokeWidth: 1,
    pointHitDetectionRadius: 20,
    pointLabelFontFamily: "'Arial'",
    pointLabelFontSize: 10,
    pointLabelFontStyle: 'normal',
    scaleBeginAtZero: true,
    scaleShowLabels: false,
    scaleShowLine: true
  });

  Radar.prototype.draw = function() {
    return Chart.Radar(this.canvas, {
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
  };

  Radar.prototype.render = function() {
    return React.createElement(Core, React.__spread({}, this.props, {
      "draw": this.draw
    }));
  };

  return Radar;

})(React.Component);

module.exports = Radar;
