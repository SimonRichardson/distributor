'use strict';

const IO   = require('fantasy-io'),
      C    = require('fantasy-combinators'),
      Free = require('fantasy-frees').Free,

      fs    = require('fs'),
      https = require('https'),

      id = C.identity;

function interpreter(free) {
  return free.cata({
    Options: (options) => {
      return IO(() => {
        return {
          key : fs.readFileSync(options.key),
          cert: fs.readFileSync(options.cert)
        };
      });
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
        return https.createServer(options, (req, res) => directive(req, res));
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
