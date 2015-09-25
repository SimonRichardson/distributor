'use strict';

const IO   = require('fantasy-io'),
      C    = require('fantasy-combinators'),
      Free = require('fantasy-frees').Free,

      http = require('http'),

      id = C.identity;

function interpreter(free) {
  return free.cata({
    Create: (handle) => {
      return IO(() => {
        return http.createServer((req, res) => handle(req, res));
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
