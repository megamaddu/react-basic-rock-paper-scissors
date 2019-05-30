"use strict";

exports.randomInt = function(a) {
  return function(b) {
    return function() {
      var upper = a > b ? a : b;
      var lower = a > b ? b : a;
      var range = upper - lower;
      var randomInt = (Math.random() * range) | 0;
      return randomInt + lower;
    }
  }
};
