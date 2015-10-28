'use strict';

const C     = require('fantasy-combinators'),
      daggy = require('daggy'),
      Lens  = require('fantasy-lenses').Lens,

      doc = require('./../dsl'),

      compose  = C.compose,
      constant = C.constant;

module.exports = {
  ok          : res => doc.statusCode(200, res),
  created     : res => doc.statusCode(201, res),
  noContent   : res => doc.statusCode(204, res),
  resetContent: res => doc.statusCode(205, res),

  badRequest  : res => doc.statusCode(400, res),
  unauthorized: res => doc.statusCode(401, res),
  notFound    : res => doc.statusCode(404, res),

  internalServerError: res => doc.statusCode(500, res)
};