'use strict';

const C      = require('fantasy-combinators'),
      Option = require('fantasy-options'),
      Either = require('fantasy-eithers'),
      Seq    = require('fantasy-arrays').Seq,
      tuples = require('fantasy-tuples'),
      Tree   = require('./router/tree'),

      constant = C.constant,

      Tuple2 = tuples.Tuple2,
      Tuple3 = tuples.Tuple3;
    

Either.prototype.toString = function() {
  return this.cata({
    Left: x => 'Left(' + x.toString() + ')',
    Right: x => 'Right(' + x.toString() + ')'
  });
};

Option.prototype.toString = function() {
  return this.cata({
    Some: x => 'Some(' + x.toString() + ')',
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

Tuple3.prototype.toString = function() {
  return 'Tuple3(' + this._1.toString() + ', ' + this._2.toString() + ', ' + this._3.toString() + ')';
};
