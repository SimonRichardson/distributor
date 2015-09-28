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
  return x.foldl((acc, y) => {
    return y.cata({
      Right: constant(acc),
      Left: z => {
        return acc.tell(['Invalid: ' + z.x.toString()]).map(constant(x));
      }
    });
  }, Writer(() => Tuple2(x, ['Path compile errors.'])));
}

function clean(t) {
  const go = function(x) {
    return x.filter(x => {
      return typeof x !== 'object';
    }).x;
  };
  return Writer(() => Tuple2(t._1, go(Seq(t._2))))
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
        return clean(errors(all).run());
      } else {
        const y = extracted.foldl((acc, x) => {
          return acc.combine((a, b) => {
            return a === b ? Option.Some(a) : Option.None;
          }, Tree.root(x));
        }, Tree.empty());
        // fold into a tree
        return Writer.of({});
      }
    }
  });
}

/*

  [
    "a/b"
    "a/b/c"
    "x/y/z"
  ]

  [
    "[a/b]/c"
    "x/y/z"
  ]

*/


module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
