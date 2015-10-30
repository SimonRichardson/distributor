'use strict';

const dsl      = require('./dsl'),
      compiler = require('./compiler'),
      builder  = require('./builder');

module.exports = {
    dsl     : dsl,
    compiler: compiler,

    del    : () => builder.build('delete'),
    get    : () => builder.build('get'),
    head   : () => builder.build('head'),
    patch  : () => builder.build('patch'),
    post   : () => builder.build('post'),
    put    : () => builder.build('put'),
    options: () => builder.build('options'),

    compile: x => compiler.run(x)
};
