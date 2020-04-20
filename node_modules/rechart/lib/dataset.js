var DataSet, React, array, bool, func, invariant, number, oneOfType, ref, string,
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

React = require('react');

invariant = require('invariant');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, oneOfType = ref.oneOfType;

DataSet = (function(superClass) {
  extend(DataSet, superClass);

  function DataSet() {
    return DataSet.__super__.constructor.apply(this, arguments);
  }

  DataSet.propTypes = {
    backgroundColor: oneOfType([string, array]),
    borderColor: string,
    borderCapStyle: string,
    borderDash: array,
    borderDashOffset: number,
    borderJoinStyle: string,
    data: array.isRequired,
    fill: bool,
    fillColor: string,
    label: string,
    pointBackgroundColor: string,
    pointBorderColor: string,
    pointBorderWidth: number,
    pointHighlightFill: string,
    pointHighlightStroke: string,
    pointHoverBackgroundColor: string,
    pointHoverBorderColor: string,
    pointHoverBorderWidth: number,
    pointHoverRadius: number,
    pointStrokeColor: string,
    yAxisID: string
  };

  DataSet.prototype.render = function() {
    return invariant(false, '<DataSet> elements are for passing data to the chart and shoudnt be rendered');
  };

  return DataSet;

})(React.Component);

module.exports = DataSet;
