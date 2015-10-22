'use strict';

const router    = require('./../router/router'),
      program   = require('./program'),
      responses = require('./responses');

function match(y) {
  return (req, res) => {
    // Route matcher
    router.compile(program.match(y, req)).fold(
      x => responses.notFound(req, res),
      y => y.getOrElse(responses.notFound)(req, res)
    );
  };
}

module.exports = {
  match : match,
};
