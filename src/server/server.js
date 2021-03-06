'use strict';

const dsl     = require('./dsl'),
      http    = require('./http'),
      https   = require('./https');

module.exports = {
    dsl    : dsl,
    http   : http,
    https  : https,

    // Default run is to http.
    run: (x, y) => http.run(x, y)
};
