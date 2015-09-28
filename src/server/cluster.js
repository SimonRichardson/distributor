'use strict';

const daggy = require('daggy'),
      IO    = require('fantasy-io'),
      C     = require('fantasy-combinators'),
      Free  = require('fantasy-frees').Free,
      Seq   = require('fantasy-arrays').Seq,

      os      = require('os'),
      http    = require('http'),
      cluster = require('cluster'),

      id = C.identity,

      BlankServer = daggy.tagged('x');

BlankServer.prototype.listen = function(port, f) {
  return f({});
};

function interpreter(free) {
  return free.cata({
    Options: (options) => {
      return IO.of({});
    },
    Create: (options, handle) => {
      return IO(() => {
        // Check if the cluster is the master.
        if (cluster.isMaster) {
          // Get the total number of cpus
          const numOfCpus = os.cpus().length;
          Seq.range(0, numOfCpus).map(x => {
            return cluster.fork();
          });
          // .. we should check if one in the cluster exists?
          return BlankServer({
            maser: isMaster
          });
        } else {
          return http.createServer((req, res) => handle(req, res));
        }
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
