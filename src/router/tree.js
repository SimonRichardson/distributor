'use strict';

const daggy  = require('daggy'),
      Option = require('fantasy-options'),
      These  = require('fantasy-these'),
      C      = require('fantasy-combinators'),
      arrays = require('fantasy-arrays'),
      tuples = require('fantasy-tuples'),

      compose  = C.compose,
      constant = C.constant,
      identity = C.identity,

      Seq    = arrays.Seq,
      unsafe = arrays.unsafe.seq,

      Tuple2 = tuples.Tuple2,
        
      Tree = daggy.tagged('value', 'nodes');

Tree.of = x => Tree(Option.of(x), Seq.empty());

Tree.empty = () => Tree(Option.None, Seq.empty());

Tree.root = (x, y) => Tree(Option.Some(x), Seq.of(y));

Tree.fork = (x, y) => Tree(Option.None, Seq([x, y]));

Tree.prototype.map = function(f) {
  return Tree(this.value.fold(f, identity), this.nodes.map(f));
};

Tree.prototype.length = function() {
  return this.nodes.length();
};

Tree.prototype.depth = function() {
  return this.nodes.foldl((acc, x) => {
    return acc + x.depth();
  }, 1);
};

Tree.prototype.modifyValue = function(x) {
  return Tree(x, this.nodes);
};

Tree.prototype.appendNode = function(x) {
  return Tree(this.value, this.nodes.cons(x));
};

Tree.prototype.appendNodes = function(x) {
  return Tree(this.value, this.nodes.concat(x));
};

Tree.prototype.foldl = function(f, acc) {
  return this.nodes.foldl((acc, x) => {
    return x.foldl(f, acc);
  }, this.value.cata({
    Some: a => f(acc, a),
    None: constant(acc)
  }));
};

Tree.prototype.match = function(f) {
  const go = function(l, t, c) {
    return t.length() < 1 ? l :
      f(a, t.x, c).cata({
        Some: x => {
          return t.y.foldl((acc, a) => {
            return a.length() < 1 ? acc : go(acc, a, c+1);
          });
        },
        None: constant(l)
      });
  };
  return go(Seq.empty(), this, 0);
};

Option.prototype.toString = function() {
  return this.cata({
    Some: x => 'Some(' + x + ')',
    None: constant('None')
  });
};

Seq.prototype.toString = function() {
  return 'Seq(' + this.foldl((acc, x) => {
    return acc.concat([x.toString()]);
  }, []).join(', ') + ')';
};

Tree.prototype.toString = function() {
  return 'Tree(' + this.value.toString() + ', ' + this.nodes.toString() + ')';
};

Tuple2.prototype.toString = function() {
  return 'Tuple2(' + this._1.toString() + ', ' + this._2.toString() + ')';
};

Tree.prototype.merge = function(b) {
  return this.combine((a, b) => a.equals(b), b);
};

Tree.prototype.combine = function(f, b) {
  const match = (a, b) => {
      return a.map(x => b.chain(y => {
        return f(x, y) ? Option.Some(x) : Option.None;
      }));
    },
    truthy = x => x.fold(constant(true), constant(false)),
    similar = (a, b) => {
      return a.foldl((acc, x) => {
        return acc.snoc(b.partition(y => {
          return truthy(match(x.value, y.value));
        }));
      }, Seq.empty());
    },
    merge = a => b => {
      return a.foldl((acc, x) => {
        const index = acc.findIndex(y => match(x.value, y.value));
        return index.cata({
          Some: y => {
            // It's not actually unsafe, because we check the item exists at the index.
            const value = unsafe.unsafeIndex(acc, y),
                  r = acc.updateAt(y, Tree(x.value, x.nodes.concat(value.nodes)));
            return r.fold(identity, constant(acc));
          },
          None: () => x.value.fold(_ => acc.snoc(x), constant(acc))
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
    go = function(a, b) {
      if(a.length() < 1) return b;
      else if(b.length() < 1) return a;

      const sequence = similar(a, b).foldl((acc, x) => {
        return Tuple2(concat(acc._1, x._1), concat(acc._2, x._2));
      }, Tuple2(Seq.empty(), Seq.empty()));

      // Merge the a and b streams together
      const x = merge(sequence._1)(Seq.empty()),
            y = merge(a)(x);

      const merged = y;

      console.log("Merged", x.toString(), y.toString());

      const recursive = merged.foldl((acc, x) => {
        return acc.snoc(Tree(x.value, x.nodes.foldl((a, b) => {
          return go(a, Seq.of(b));
        }, Seq.empty())));

      }, Seq.empty());


      console.log("Recursive", recursive.toString(), sequence._2.toString());

      return recursive.concat(sequence._2);
    };

  return Tree(Option.None, go(Seq.of(this), Seq.of(b)));
};

module.exports = Tree;
