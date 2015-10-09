'use strict';

const Free   = require('fantasy-frees').Free,
      Option = require('fantasy-options'),
      Writer = require('fantasy-writers'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      path    = require('./path'),
      url     = require('./url'),
      Tree    = require('./tree'),
      Request = require('./request'),

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
    ParseRoutes: routes => {
      const trees = routes
        .rmap(path.compile)
        .rmap(x => {
          return x.map(y => Tree.fromSeq(y));
        });

      return Writer.of(trees);
    },
    Compile: trees => {
      const all = trees.routes,
        extracted = extract(all);

      if (extracted.length() < all.length()) {
        return errors(all);
      } else {
        const root = extracted.foldl((acc, x) => {
            return acc.snoc(Tree(Option.None, Seq.of(x)));
          }, Seq.empty());

        // fold into a tree
        return Writer.of(root.reducel((acc, x) => acc.merge(x)));
      }
    },
    ParseRequest: request => {
      // This is so unsafe!
      const url = request.url,
            method = request.method;

      return Writer.of(Request(method, url));
    },
    ParseUrl: uri => {
      const to = url.compile(uri).map(x => Tree.fromSeq(x));
      return Writer.of(to);
    },
    Match: (routes, request) => {
      console.log(routes, request);
      return Writer.of(null);
    }
  });
}

// Move this to Either<Tree, Seq<String>>
module.exports = {
  run: x => Free.runFC(x, interpreter, Writer)
};
