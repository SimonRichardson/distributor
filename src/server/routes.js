'use strict';

const router  = require('./../router/router'),
      program = require('./program');

function match(f, y) {
  return (req, res) => {
    // Route matcher
    return router.compile(program.match(y, req)).fold(
      x => f(req, res),
      y => y.getOrElse(f)(req, res)
    );
  };
}

module.exports = {
  match : match,
};
