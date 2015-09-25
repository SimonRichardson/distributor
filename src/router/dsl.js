'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Router = daggy.taggedSum({
        Compile: ['routes']
      });

module.exports = {
  compile: routes => Free.liftFC(Router.Compile(routes))
};
