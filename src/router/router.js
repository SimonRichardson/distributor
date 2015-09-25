'use strict';

const dsl      = require('./dsl'),
      compiler = require('./compiler');

module.exports = {
    dsl     : dsl,
    compiler: compiler,

    compile: x => compiler.run(x)
};
