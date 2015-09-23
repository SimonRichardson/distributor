const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Server = daggy.taggedSum({
        Create: ['handle'],
        Listen: ['x', 'port', 'on']
      });

function create(handle) {
  return Free.liftFC(Server.Create(handle));
}

function listen(x, port, on) {
  return Free.liftFC(Server.Listen(x, port, on));
}

module.exports = {
  create: create,
  listen: listen
};
