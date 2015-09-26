'use strict';

const Free   = require('fantasy-frees').Free,
      Writer = require('fantasy-writers'),
      Seq      = require('fantasy-arrays').Seq,

      path = require('./path');

function interpreter(free) {
  return free.cata({
    Compile: routes => {
      console.log(routes);

      // for(var k in routes["get"]) {
      //   console.log(k, path.compile(k));
      // }

      return Writer.of({});
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
