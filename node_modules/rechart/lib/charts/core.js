var Core, React, array, bool, func, number, object, ref, string,
  bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

React = require('react');

ref = React.PropTypes, array = ref.array, string = ref.string, bool = ref.bool, func = ref.func, number = ref.number, object = ref.object;

Core = (function(superClass) {
  extend(Core, superClass);

  function Core(props) {
    this.render = bind(this.render, this);
    this.setContext = bind(this.setContext, this);
    this.getToolTips = bind(this.getToolTips, this);
    this.getDataSets = bind(this.getDataSets, this);
    this.buildOptions = bind(this.buildOptions, this);
    var children;
    Core.__super__.constructor.apply(this, arguments);
    children = props.children;
    if (typeof children !== 'Array') {
      children = [children];
    }
    this.state = {
      dataSets: this.getDataSets(children),
      legend: this.getLegend(children),
      toolTips: this.getToolTips(children)
    };
  }

  Core.propTypes = {
    events: array,
    responsive: bool,
    responsiveAnimationDuration: number,
    maintainAspectRatio: bool,
    hover: object,
    labels: array.isRequired,
    onClick: func,
    defaultColor: string
  };

  Core.defaultProps = {
    events: ["mousemove", "mouseout", "click", "touchstart", "touchmove", "touchend"],
    hover: {
      mode: 'label'
    },
    maintainAspectRatio: true,
    responsive: true,
    responsiveAnimationDuration: 0,
    defaultColor: 'rgba(0,0,0,0.1)'
  };

  Core.prototype.buildOptions = function() {
    var k, options, ref1, v;
    options = {};
    ref1 = this.props;
    for (k in ref1) {
      v = ref1[k];
      options[k] = v;
    }
    options.tooltips = this.state.toolTips || {
      enabled: false
    };
    return options;
  };

  Core.prototype._getOptionsFromElement = function(el) {
    var k, options, ref1, v;
    options = el.type.defaultProps || {};
    ref1 = el.props;
    for (k in ref1) {
      v = ref1[k];
      options[k] = v;
    }
    return options;
  };

  Core.prototype._getChildrenByName = function(children, name, onlyFirst) {
    var child, childOptions, displayName, i, len;
    if (onlyFirst == null) {
      onlyFirst = false;
    }
    children = React.Children.toArray(children);
    childOptions = [];
    for (i = 0, len = children.length; i < len; i++) {
      child = children[i];
      displayName = child.type.displayName || child.type.name;
      if (displayName === name) {
        if (onlyFirst) {
          return child;
        }
        childOptions.push(child);
      }
    }
    if (childOptions.length) {
      return childOptions;
    }
    return null;
  };

  Core.prototype.getDataSets = function(children) {
    var c;
    return (function() {
      var i, len, ref1, results;
      ref1 = this._getChildrenByName(children, 'DataSet');
      results = [];
      for (i = 0, len = ref1.length; i < len; i++) {
        c = ref1[i];
        results.push(this._getOptionsFromElement(c));
      }
      return results;
    }).call(this);
  };

  Core.prototype.getToolTips = function(children) {
    var toolTips;
    toolTips = this._getChildrenByName(children, 'ToolTips', true);
    if (toolTips) {
      return this._getOptionsFromElement(toolTips);
    } else {
      return null;
    }
  };

  Core.prototype.getLegend = function(children) {
    return this._getChildrenByName(children, 'Legend', true);
  };

  Core.prototype.setContext = function(canvas) {
    this.canvas = canvas;
    return this.props.draw.call(this);
  };

  Core.prototype.render = function() {
    var style;
    style = this.props.style;
    if (!style.position) {
      style.position = 'relative';
    }
    return React.createElement("div", {
      "style": style
    }, this.state.legend, React.createElement("canvas", {
      "ref": this.setContext,
      "id": 'cjs-line-chart'
    }));
  };

  return Core;

})(React.Component);

module.exports = Core;
