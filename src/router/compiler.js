'use strict';

const Free   = require('fantasy-frees').Free,
      Option = require('fantasy-options'),
      Either = require('fantasy-eithers'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      path    = require('./../path/path'),
      url     = require('./../path/url'),

      builder = require('./builder'),
      Tree    = require('./tree'),
      Request = require('./request'),

      Node = builder.Node,

      compose  = C.compose,
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

function sequenceCalls(x) {
  return x.map(node => {
    const route = node.route,
          calls = node.calls,
          match = (a, b) => {
            return a === b ? Option.Some(a) : Option.None;
          },
          go = (x, index, depth) => {
            const value = x.value.map(y => {
                return Node(
                  y, 
                  match(index, depth).map(constant(calls)).getOrElse(Seq.empty())
                );
              }),
              nodes = x.nodes.foldl((acc, x) => {
                return acc.snoc(go(x, index + 1, depth));
              }, Seq.empty());

            return Tree(value, nodes);
          };

    return route.map(tree => {
      return go(tree, 1, tree.size());
    });
  });
}

function sequence(x) {
  const go = (a, b) => {
    const value = a.map(node => Node(node, Seq.empty())),
          nodes = b.foldl((acc, x) => {
            return acc.snoc(go(x.value, x.nodes));
          }, Seq.empty());
    return Tree(value, nodes);
  };
  return go(x.value, x.nodes);
}

function errors(x) {
  return x.zip(Seq.range(0, x.length())).foldl((acc, y) => {
    const index = y._2;
    return y._1.lfold(z => {
      return z.cata({
        Left: z => {
          return acc.bimap(
            a => a.concat(Seq.of('Invalid `' + z.x.toString() + '` at index `' + index + '`')),
            identity
          );
        },
        Right: constant(acc)
      });
    });
  }, Either.Left(Seq.of('Path compile errors.')));
}

function interpreter(free) {
  return free.cata({
    ParseRoutes: routes => {
      const trees = routes.rmap(x => {
        return x.lmap(path.compile);
      }).rmap(x => {
        return x.lmap(y => {
          return y.map(z => {
            return Tree.fromSeq(z);
          });
        });
      });

      return Either.of(trees);
    },
    Compile: trees => {
      const all = trees.routes,
        extracted = compose(extract)(sequenceCalls)(all);

      if (extracted.length() < all.length()) {
        return errors(all);
      } else {
        const root = extracted.foldl((acc, x) => {
            return acc.snoc(Tree(Option.None, Seq.of(x)));
          }, Seq.empty());

        // fold into a tree
        return root.reducel((acc, x) => acc.merge(join, x)).fold(
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
          .map(compose(sequence)(Tree.fromSeq))
          .map(y => Tree(Option.None, Seq.of(y)));
      });
      return to.rfold(x => {
        return x.map(y => {
          return Request(to.method, y);
        });
      });
    },
    Match: (routes, request) => {
      const match = routes.match((a, b) => {
        return pathMatchUrl(a.route, b.route);
      }, extractCalls, request.url);
      return match.fold(
        x => Either.Right(x),
        () => Either.Left(Seq.of('Route match error.'))
      );
    },
    Caller: node => Either.Right(node.calls.head())
  });
}

function extractCalls(a) {
  return a.calls.length() < 1 ? Option.None : Option.of(a);
}

function join(a, b) {
  return a.map(x => {
    return x.rmap(y => {
      return b.chain(i => {
        return i.rfold(identity, j => {
          return y.concat(j);
        });
      });
    });
  });
}

function pathMatchUrl(a, b) {
  const eq = x => y => {
    return x.fold(
      a  => y.fold(b => a === b, constant(false)),
      () => y.fold(constant(false), constant(true))
    );
  };
  return a.cata({
    Name    : x => {
      return b.cata({
        Name    : eq(x),
        Empty   : constant(false),
      });
    },
    Variable: x => {
      return b.cata({
        Name    : constant(true),
        Empty   : constant(false),
      });
    },
    Wildcard: () => true,
    Empty   : () => false
  });
}

module.exports = {
  run: x => Free.runFC(x, interpreter, Either)
};
