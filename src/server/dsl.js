'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Server = daggy.taggedSum({
        Options: ['options'],
        Create : ['options', 'handle'],
        Listen : ['x', 'port', 'on']
      });

module.exports = {
  options: (options)         => Free.liftFC(Server.Options(options)),
  create : (options, handle) => Free.liftFC(Server.Create(options, handle)),
  listen : (x, port, on)     => Free.liftFC(Server.Listen(x, port, on))
};
