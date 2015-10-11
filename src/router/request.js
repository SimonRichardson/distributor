'use strict';

const daggy  = require('daggy'),

      Request = daggy.tagged('method', 'url');

Request.prototype.lmap = function(f) {
  return Request(f(this.method), this.url);
};

Request.prototype.rmap = function(f) {
  return Request(this.method, f(this.url));
};

module.exports = Request;
