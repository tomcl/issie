var Legend, React, array, bool, func, number, object, ref, string,
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

React = require('react');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Legend = (function(superClass) {
  extend(Legend, superClass);

  function Legend() {
    return Legend.__super__.constructor.apply(this, arguments);
  }

  Legend.propTypes = {
    style: object
  };

  Legend.defaultProps = {
    style: true
  };

  Legend.prototype.render = function() {
    return React.createElement("div", {
      "className": 'rechart-legend',
      "style": this.props.style
    }, this.props.children);
  };

  return Legend;

})(React.Component);

module.exports = Legend;
