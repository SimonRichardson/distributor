'use strict';

const router  = require('./../router/router'),
      program = require('./program'),
      errors  = require('./../documents/json/errors');

function match(y) {
  return (req, res) => {
    // Route matcher
    return router.compile(program.match(y, req)).fold(
      x => errors.notFound(req, res),
      y => y.getOrElse(errors.notFound)(req, res)
    );
  };
}

module.exports = {
  match : match,
};
