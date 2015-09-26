'use strict';

const Free   = require('fantasy-frees').Free,
      Writer = require('fantasy-writers'),
      Seq      = require('fantasy-arrays').Seq,

      path = require('./path');

function interpreter(free) {
  return free.cata({
    Compile: routes => {
      const x = routes.rmap(x => {
        return path.compile(x);
      });
      // fold into a tree
      return Writer.of({});
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
