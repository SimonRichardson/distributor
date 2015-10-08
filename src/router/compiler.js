'use strict';

const Free   = require('fantasy-frees').Free,
      Option = require('fantasy-options'),
      Writer = require('fantasy-writers'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      path = require('./path'),
      Tree = require('./tree'),

      constant = C.constant,
      identity = C.identity,

      Tuple2 = tuples.Tuple2,

      Path = path.Path;

function extract(x) {
  return x.foldl((acc, x) => {
    return x.cata({
      Right: y => acc.snoc(y),
      Left: constant(acc)
    });
  }, Seq.empty());
}

function errors(x) {
  return x.zip(Seq.range(0, x.length())).foldl((acc, y) => {
    return y._1.cata({
      Right: constant(acc),
      Left: z => {
        return acc
          .tell(Seq.of('Invalid ' + z.x.toString() + ' at index ' + y._2))
          .map(constant(x));
      }
    });
  }, Writer.of(x).tell(Seq.of('Path compile errors.')));
}

function interpreter(free) {
  return free.cata({
    Compile: routes => {
      const trees = routes
        .rmap(path.compile)
        .rmap(x => {
          return x.map(x => {
            return x.reverse().foldl((acc, x) => {
              const nodes = acc.nonEmpty().fold(
                _ => Seq.of(acc),
                () => Seq.empty()
              );
              return Tree(Option.of(x), nodes);
            }, Tree.empty());
          });
        }),
        all = trees.routes,
        extracted = extract(all);

      if (extracted.length() < all.length()) {
        return errors(all);
      } else {

        // Make sure every one has root node.
        const root = extracted.foldl((acc, x) => {
          return acc.snoc(Tree(Option.None, Seq.of(x)));
        }, Seq.empty());

        const result = root.reducel((acc, x) => {
          return acc.merge(x);
        });

        console.log('-Final', result.toString());

        // fold into a tree
        return Writer.of(result);
      }
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
