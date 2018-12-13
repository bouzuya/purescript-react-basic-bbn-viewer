"use strict";

exports.toHtmlStringImpl = function (s) {
  return require('marked')(s);
};
