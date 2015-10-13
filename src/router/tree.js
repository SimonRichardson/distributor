'use strict';

const daggy  = require('daggy'),
      Option = require('fantasy-options'),
      C      = require('fantasy-combinators'),
      arrays = require('fantasy-arrays'),
      tuples = require('fantasy-tuples'),

      compose  = C.compose,
      constant = C.constant,
      identity = C.identity,

      Seq    = arrays.Seq,
      unsafe = arrays.unsafe.seq,

      Tuple2 = tuples.Tuple2,
      Tuple3 = tuples.Tuple3,
        
      Tree = daggy.tagged('value', 'nodes');

Tree.of = x => Tree(Option.of(x), Seq.empty());

Tree.empty = () => Tree(Option.None, Seq.empty());

Tree.root = (x, y) => Tree(Option.Some(x), Seq.of(y));

Tree.fork = (x, y) => Tree(Option.None, Seq([x, y]));

Tree.fromSeq = x => {
  return x.reverse().foldl((acc, x) => {
    const nodes = acc.nonEmpty().fold(
      _ => Seq.of(acc),
      () => Seq.empty()
    );
    return Tree(Option.of(x), nodes);
  }, Tree.empty());
};

Tree.prototype.map = function(f) {
  return Tree(this.value.fold(f, identity), this.nodes.map(f));
};

Tree.prototype.length = function() {
  return this.nodes.length();
};

Tree.prototype.size = function() {
  return this.nodes.foldl((acc, x) => {
    return acc + x.size();
  }, 1);
};

Tree.prototype.depth = function() {
  const max = a => {
      return a.foldl((acc, x) => {
        return x > acc ? x : acc;
      }, 0);
    };
  return 1 + this.length() + max(this.nodes.foldl((acc, x) => {
    return acc.snoc(x.depth());
  }, Seq.empty()));
};

Tree.prototype.foldl = function(f, acc) {
  return this.nodes.foldl((acc, x) => {
    return x.foldl(f, acc);
  }, this.value.cata({
    Some: a => f(acc, a),
    None: constant(acc)
  }));
};

Tree.prototype.nonEmpty = function() {
  return this.value.map(constant(this));
};

Tree.prototype.merge = function(g, b) {
  return this.combine((a, b) => a.equals(b), g, b);
};

Tree.prototype.combine = function(f, g, b) {
  const equals = match(f),
    truthy = (a, b) => equals(a, b).fold(constant(true), constant(false)),
    difference = (a, b) => {
      return a.foldl((acc, x) => {
        return acc.snoc(b.partition(y => {
          return truthy(x.value, y.value);
        }));
      }, Seq.empty());
    },
    merge = a => b => {
      return a.foldl((acc, x) => {
        const index = acc.findIndex(y => truthy(x.value, y.value));
        return index.cata({
          Some: y => {
            // It's not actually unsafe, because we check the item exists at the index.
            const value = unsafe.unsafeIndex(acc, y),
                  r = acc.updateAt(y, Tree(g(x.value, value.value), x.nodes.concat(value.nodes)));
            return r.fold(identity, constant(acc));
          },
          None: () => {
            return x.value.fold(_ => acc.snoc(x), constant(acc));
          }
        });
      }, b);
    },
    concat = (a, b) => {
      const nonEmpty = a => {
        return a.foldl((acc, x) => {
          return acc.fold(constant(acc), () => x.value.map(_ => a));
        }, Option.None);
      };
      return nonEmpty(b).fold(x => a.concat(x), constant(a));
    },
    filter = (a, b) => {
      return a.foldl((acc, x) => {
        return b.find(y => truthy(x.value, y.value)).fold(
          constant(acc),
          () => acc.snoc(x)
        );
      }, Seq.empty());
    },
    go = (a, b) => {
      if (a.length() < 1) return b;
      else if (b.length() < 1) return a;
      else {
        const xx = difference(a, b);
        const sequence = xx.foldl((acc, x) => {
            return Tuple2(concat(acc._1, x._1), concat(acc._2, x._2));
          }, Tuple2(Seq.empty(), Seq.empty())),

          merged = compose(merge(a))(merge(sequence._1))(Seq.empty()),

          recursive = merged.foldl((acc, x) => {
            return acc.snoc(Tree(x.value, x.nodes.foldl((a, b) => {
              return go(a, Seq.of(b));
            }, Seq.empty())));
          }, Seq.empty());

        return recursive.concat(filter(sequence._2, recursive));
      }
    };

  return Tree(Option.None, go(trim(this), trim(b)));
};

Tree.prototype.match = function(f, g, b) {
  const equals = match(f),
    first = a => {
      return a.head().fold(b => Seq.of(b), () => Seq.empty());
    },
    go = (a, b) => {
      if (outsideRange(a, b) || outsideRange(b, a)) return Option.None;
      else {
        return a.foldl((acc, x) => {
          return acc.fold(constant(acc), () => {

            return b.foldl((acc, y) => {
              return acc.fold(constant(acc), () => {
                return equals(x.value, y.value).map(constant(Tuple3(x.value, x.nodes, y.nodes)));
              });
            }, acc);
          });
        }, Option.None).chain(tuple => {
          return maybe(
            () => tuple._1.chain(g), 
            () => go(tuple._2, tuple._3),
            tuple._3.length() < 1
          );
        });
      }
    };

  return go(trim(this), trim(b));
};

function outsideRange(a, b) {
  return a.length() < 1 && b.length() >= 1;
}

function maybe(f, g, b) {
  return b ? f() : g();
}

function match(f) {
  return (a, b) => {
    return a.chain(x => b.chain(y => {
      return f(x, y) ? Option.Some(x) : Option.None;
    }));
  };
}

function trim(x) {
  return x.value.fold(_ => Seq(x), () => x.nodes);
}

module.exports = Tree;
