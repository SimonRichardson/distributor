'use strict';

const dsl      = require('./dsl'),
      compiler = require('./compiler'),
      builder  = require('./builder');

module.exports = {
    dsl     : dsl,
    compiler: compiler,

    get : () => builder.build('get'),
    post: () => builder.build('post'),
    put : () => builder.build('put'),

    compile: x => compiler.run(x)
};
