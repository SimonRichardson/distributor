'use strict';

const IO   = require('fantasy-io'),
      C    = require('fantasy-combinators'),
      Free = require('fantasy-frees').Free,

      http = require('http'),

      program = require('./program'),
      routes  = require('./routes'),
      async   = require('./../utils/async'),
      router  = require('./../router/router'),
      
      id       = C.identity,
      constant = C.constant;

function interpreter(errors) {
  return free => {
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
            Left: errors.internalError,
            Right: x => routes.match(errors.notFound, x)
          });
          return http.createServer((req, res) => {
            async(() => {
              return directive(req, res);
            });
          });
        });
      },
      Listen: (x, port, on) => {
        return IO(() => {
          return x.listen(port, () => on.map(f => f(port)));
        });
      }
    });
  };
}

module.exports = {
  run: (x, y) => Free.runFC(y, interpreter(x), IO)
};
