'use strict';

const daggy  = require('daggy'),
      Option = require('fantasy-options'),
      These  = require('fantasy-these'),
      Seq    = require('fantasy-arrays').Seq,
      C      = require('fantasy-combinators'),
      tuples = require('fantasy-tuples'),

      compose  = C.compose,
      constant = C.constant,
      identity = C.identity,

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
    go = function(a, b) {
      return a.chain(x => {
        return x.value.fold(
          _ => {
            const val = x.value,
                  nodes = x.nodes,
                  combined = b.foldl((acc, a) => {
                    return a.value.fold(
                      _ => {
                        return match(val, a.value).fold(
                          x => Option.of(x),
                          constant(acc)
                        );
                      },
                      constant(acc)
                    );
                  }, Option.None),
                  tuple = b.partition(x => {
                    return x.value.fold(
                      _ => truthy(match(val, x.value)),
                      constant(false)
                    );
                  }),
                  children = tuple._1.foldl((acc, a) => {
                    return acc.concat(a.nodes);
                  }, Seq.empty()),
                  unique = children.filter(x => {
                    return !truthy(nodes.foldl((acc, y) => {
                      return acc.fold(constant(acc), () => match(x.value, y.value));
                    }, Option.None));
                  });

            return Seq.of(Tree(
                Option.of(combined.getOrElse(val)),
                go(nodes, unique)
              )).concat(tuple._2);
          },
          () => Seq.of(x)
        );
      });
    },
    a = this,
    nodes = a.value.fold(
      x => go(Seq.of(a), Seq.of(b)),
      constant(Seq.of(b))
    );

  return Tree(Option.None, nodes);
};

module.exports = Tree;
