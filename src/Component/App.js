"use strict";

exports.unsafeSetInnerHTML = function (s) {
  return function (e) {
    return function () {
      e.innerHTML = s;
    };
  };
};
