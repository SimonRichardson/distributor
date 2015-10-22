'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Server = daggy.taggedSum({
        Options: ['options'],
        Compile: ['routes'],
        Create : ['options', 'handle'],
        Listen : ['x', 'port', 'on']
      });

module.exports = {
  options: options           => Free.liftFC(Server.Options(options)),
  compile: routes            => Free.liftFC(Server.Compile(routes)),
  create : (options, handle) => Free.liftFC(Server.Create(options, handle)),
  listen : (x, port, on)     => Free.liftFC(Server.Listen(x, port, on))
};
