'use strict';

const Free   = require('fantasy-frees').Free,
      Option = require('fantasy-options'),
      Writer = require('fantasy-writers'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      constant = C.constant,
      identity = C.identity,

      Tuple2 = tuples.Tuple2,

      path = require('./path'),
      Tree = require('./tree');

function extract(x) {
  return x.foldl((acc, x) => {
    return x.cata({
      Right: y => {
        return acc.snoc(y);
      },
      Left: constant(acc)
    });
  }, Seq.empty());
}

function errors(x) {
  return x.foldl((acc, x) => {
    return x.cata({
      Right: constant(acc),
      Left: y => {
        return acc.snoc(y);
      }
    });
  }, Seq.empty());
}

function interpreter(free) {
  return free.cata({
    Compile: routes => {
      const trees = routes
        .rmap(path.compile)
        .rmap(x => {
          return x.map(x => {
            return x.reverse().foldl((acc, x) => {
              return Tree(Option.of(x), acc);
            }, Tree.empty());
          });
        }),
        all = trees.routes,
        extracted = extract(all);

      if (extracted.length() < all.length()) {
        return Writer.of(all)
          .tell("Compile errors.")
          .chain(x => Writer.of(errors(all)));
      } else {
        const y = extracted.foldl((acc, x) => {
          return acc.combine((a, b) => {
            console.log(a, b);
              return true;
          }, x);
        }, Tree.empty());
        // fold into a tree
        return Writer.of({});
      }
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
