'use strict';

const Free   = require('fantasy-frees').Free,
      Option = require('fantasy-options'),
      Either = require('fantasy-eithers'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      path    = require('./../path/path'),
      url     = require('./../path/url'),

      Tree    = require('./tree'),
      Request = require('./request'),

      constant = C.constant,
      identity = C.identity,

      Tuple2 = tuples.Tuple2;

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
      Left: z => {
        return acc.bimap(
          a => a.concat(Seq.of('Invalid `' + z.x.toString() + '` at index `' + y._2 + '`')),
          identity
        );
      },
      Right: constant(acc)
    });
  }, Either.Left(Seq.of('Path compile errors.')));
}

function interpreter(free) {
  return free.cata({
    ParseRoutes: routes => {
      const trees = routes
        .rmap(path.compile)
        .rmap(x => {
          return x.map(y => Tree.fromSeq(y));
        });

      return Either.of(trees);
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
        return root.reducel((acc, x) => acc.merge(x)).fold(
          x => Either.Right(x),
          () => Either.Left(Seq.of('Route compile error.'))
        );
      }
    },
    ParseRequest: request => {
      // This is so unsafe!
      const uri = request.url,
            method = request.method;

      return Either.of(Request(method, uri));
    },
    ParseUrl: request => {
      const to = request.rmap(x => {
        return url.compile(x)
          .map(y => Tree.fromSeq(y))
          .map(y => Tree(Option.None, y));
      });
      return to.rfold(x => {
        return x.map(y => {
          return Request(to.method, y);
        });
      });
    },
    Match: (routes, request) => {
      const match = routes.match(request.url);
      return match.fold(
        x => Either.Right(x),
        () => Either.Left(Seq.of('Route match error.'))
      );
    }
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Either)
};
