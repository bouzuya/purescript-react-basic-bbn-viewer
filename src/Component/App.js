"use strict";

var React = require('react');
var createReactClass = require('create-react-class');

exports.unsafeHtmlComponent = createReactClass({
  render: function () {
    return React.createElement(
      'div', { dangerouslySetInnerHTML: { __html: this.props.html } }
    );
  }
});
