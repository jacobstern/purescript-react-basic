"use strict";

var React = require("react");
var Fragment = React.Fragment || "div";

exports.useState_ = function(initialState) {
  var state = React.useState(initialState);
  return { value: state[0], setValue: state[1] };
};

exports.useEffect_ = function(effect, inputs) {
  React.useEffect(effect, inputs);
};

exports.empty = null;

exports.keyed_ = function(key, child) {
  return React.createElement(Fragment, { key: key }, child);
};

exports.element_ = function(component, props) {
  return React.createElement.apply(
    null,
    [component, props].concat((props && props.children) || null)
  );
};

exports.elementKeyed_ = exports.element_;

exports.fragment = function(children) {
  return React.createElement.apply(null, [Fragment, {}].concat(children));
};

exports.displayName = function(component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
};

exports.unsafeSetDisplayName = function(displayName, component) {
  component.displayName = displayName;
  component.toString = function() {
    return displayName;
  };
  return component;
};
