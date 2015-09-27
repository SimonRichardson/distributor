const daggy  = require('daggy'),
      Option = require('fantasy-options'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),

      constant = C.constant,
      identity = C.identity,
        
      Tree = daggy.tagged('x', 'y');

Tree.of = x => Tree(Option.of(x), Seq.empty());

Tree.empty = () => Tree(Option.None, Seq.empty());

Tree.prototype.map = function(f) {
  return Tree(this.x.fold(f, identity), this.y.map(f));
};

Tree.prototype.length = function() {
  return this.y.length();
};

Tree.prototype.foldl = function(f, acc) {
  return this.y.foldl((acc, x) => {
    return x.foldl(f, acc);
  }, this.x.cata({
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
            return a.length() < 1 ? acc :
              return go(acc, a, c+1);
          });
        },
        None: constant(l)
      });
  };
  return go(Seq.empty(), this, 0);
};

Tree.prototype.combine = function(f, a, b) {
  const go = function(a, b) {
      return a.chain(x => {
        return x.length() < 0 ? Seq.of(x) : rec(x);
      });
    },
    rec = function(x) {
      const val = x.x,
            combined = b.foldl((acc, y) => {
              return y.length() < 0 ? acc :
                return f(y.x, val).cata({
                  Some: x => Option.of(x),
                  None: constant(acc)
                });
            }, Option.None),
            merged = b.Partition(x => {
              return x.length() < 0 ? false :
                return f(x.x, val).fold(
                  constant(true),
                  constant(false)
                );
            }),
            children = merged._1.foldl((acc, y) => {
              return acc.concat(y.y);
            });

      return Seq.of(Tree(
          Option.of(combined.getOrElse(constant(val))),
          go(x.y, children);
        )).concat(merged._2);
    };
  return a.length() < 0 ? Seq.of(b) : 
    go(Seq.of(a), Seq.of(b));
};

module.exports = Tree;
