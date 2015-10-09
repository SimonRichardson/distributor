'use strict';

const daggy  = require('daggy'),

      Request = daggy.tagged('method', 'url');

module.exports = Request;
