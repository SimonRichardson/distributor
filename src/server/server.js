'use strict';

const dsl     = require('./dsl'),
      http    = require('./http'),
      https   = require('./https'),
      cluster = require('./cluster');

module.exports = {
    dsl    : dsl,
    http   : http,
    https  : https,
    cluster: {
        http: cluster
    },

    // Default run is to http.
    run: x => http.run(x)
};
