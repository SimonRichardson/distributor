'use strict';

const C     = require('fantasy-combinators'),
      daggy = require('daggy'),
      Lens  = require('fantasy-lenses').Lens,

      compose  = C.compose,
      constant = C.constant,

      lens = Lens.objectLens('statusCode');

module.exports = {
  ok          : compose(lens)(constant(200)),
  created     : compose(lens)(constant(201)),
  noContent   : compose(lens)(constant(204)),
  resetContent: compose(lens)(constant(205)),

  badRequest  : compose(lens)(constant(400)),
  unauthorized: compose(lens)(constant(401)),
  notFound    : compose(lens)(constant(403)),

  internalServerError: compose(lens)(constant(500))
};