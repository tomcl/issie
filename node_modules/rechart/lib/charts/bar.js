var Bar, Chart, Core, React, _extend, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

_extend = require('lodash/object/extend');

Chart = require('../chart');

React = require('react');

Core = require('./core');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Bar = (function(superClass) {
  extend(Bar, superClass);

  function Bar() {
    this.render = bind(this.render, this);
    return Bar.__super__.constructor.apply(this, arguments);
  }

  Bar.propTypes = _extend({}, Core.propTypes, {
    barShowStroke: bool,
    scales: object,
    staked: bool
  });

  Bar.defaultProps = _extend({}, Core.defaultProps, {
    barShowStroke: true,
    stacked: false
  });

  Bar.prototype.draw = function() {
    return Chart.Bar(this.canvas, {
      data: {
        labels: this.props.labels,
        datasets: this.state.dataSets
      },
      options: this.buildOptions()
    });
  };

  Bar.prototype.render = function() {
    return React.createElement(Core, React.__spread({}, this.props, {
      "draw": this.draw
    }));
  };

  return Bar;

})(React.Component);

module.exports = Bar;
