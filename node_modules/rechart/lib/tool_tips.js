var React, ToolTips, array, bool, func, invariant, number, object, ref, string,
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

invariant = require('invariant');

React = require('react');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

ToolTips = (function(superClass) {
  extend(ToolTips, superClass);

  function ToolTips() {
    return ToolTips.__super__.constructor.apply(this, arguments);
  }

  ToolTips.propTypes = {
    backgroundColor: string,
    caratSize: number,
    cornerRadius: number,
    enabled: bool,
    fontColor: string,
    fontFamily: string,
    fontSize: number,
    fontStyle: string,
    mode: string,
    multiKeyBackground: string,
    titleFontColor: string,
    titleFontFamily: string,
    titleFontSize: number,
    titleFontStyle: string,
    xPadding: number,
    xOffset: number,
    yPadding: number
  };

  ToolTips.defaultProps = {
    backgroundColor: '#000',
    caratSize: 8,
    cornerRadius: 6,
    enabled: true,
    fontColor: '#fff',
    fontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
    fontSize: 10,
    fontStyle: 'normal',
    mode: 'label',
    multiKeyBackground: '#000',
    titleFontColor: '#fff',
    titleFontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
    titleFontSize: 12,
    titleFontStyle: 'bold',
    xPadding: 6,
    yPadding: 6,
    xOffset: 10
  };

  ToolTips.prototype.render = function() {
    return invariant(false, '<ToolTips> element passes tool tip parameters to the chart object and shouldnt be rendered');
  };

  return ToolTips;

})(React.Component);

module.exports = ToolTips;
