'use strict';

const daggy    = require('daggy'),
      Free     = require('fantasy-frees').Free,

      Path = daggy.taggedSum({
        Filter: ['x'],
        Split : ['x'],
        Token : ['x'],
        Format: ['x'],
        Valid : ['x']
      });

module.exports = {
  filter: x => Free.liftFC(Path.Filter(x)),
  split : x => Free.liftFC(Path.Split(x)),
  token : x => Free.liftFC(Path.Token(x)),
  format: x => Free.liftFC(Path.Format(x)),
  valid : x => Free.liftFC(Path.Valid(x))
};
