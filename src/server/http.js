'use strict';

const IO   = require('fantasy-io'),
      C    = require('fantasy-combinators'),
      Free = require('fantasy-frees').Free,

      http = require('http'),

      program   = require('./program'),
      routes    = require('./routes'),
      router    = require('./../router/router'),
      responses = require('./responses'),

      id       = C.identity,
      constant = C.constant;

function interpreter(free) {
  return free.cata({
    Options: options => {
      return IO.of({});
    },
    Compile: queries => {
      return IO(() => {
        return router.compile(program.create(queries));
      });
    },
    Create: (options, handle) => {
      return IO(() => {
        const directive = handle.cata({
          Left: responses.internalError,
          Right: x => routes.match(x)
        });
        return http.createServer((req, res) => directive(req, res));
      });
    },
    Listen: (x, port, on) => {
      return IO(() => {
        return x.listen(port, () => on.map(f => f(port)));
      });
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, IO)
};
