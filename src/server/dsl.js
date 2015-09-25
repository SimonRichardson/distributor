'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Server = daggy.taggedSum({
        Create: ['handle'],
        Listen: ['x', 'port', 'on']
      });

module.exports = {
  create: handle        => Free.liftFC(Server.Create(handle)),
  listen: (x, port, on) => Free.liftFC(Server.Listen(x, port, on))
};
