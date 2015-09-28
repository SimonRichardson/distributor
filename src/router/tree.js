'use strict';

const daggy  = require('daggy'),
      Option = require('fantasy-options'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),

      constant = C.constant,
      identity = C.identity,
        
      Tree = daggy.tagged('value', 'nodes');

Tree.of = x => Tree(Option.of(x), Seq.empty());

Tree.empty = () => Tree(Option.None, Seq.empty());

Tree.root = x => Tree(Option.None, Seq.of(x));

Tree.prototype.map = function(f) {
  return Tree(this.value.fold(f, identity), this.nodes.map(f));
};

Tree.prototype.length = function() {
  return this.nodes.length();
};

Tree.prototype.appendNode = function(x) {
  return Tree(this.value, this.nodes.snoc(x));
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

Tree.prototype.combine = function(f, b) {
  const go = function(a, b) {
      return a.chain(x => {
        return x.length() < 1 ? Seq.of(x) : rec(x);
      });
    },
    rec = function(x) {
      const val = x.x,
            combined = b.foldl((acc, y) => {
              return y.length() < 0 ? acc :
                f(y.x, val).cata({
                  Some: x => Option.of(x),
                  None: constant(acc)
                });
            }, Option.None),
            merged = b.Partition(x => {
              return x.length() < 0 ? false :
                f(x.x, val).fold(
                  constant(true),
                  constant(false)
                );
            }),
            children = merged._1.foldl((acc, y) => {
              return acc.concat(y.y);
            });

      return Seq.of(Tree(
          Option.of(combined.getOrElse(constant(val))),
          go(x.y, children)
        )).concat(merged._2);
    };
  return this.length() < 0 ? b : 
    b.length() < 0 ? this :
    go(this, b);
};

module.exports = Tree;
