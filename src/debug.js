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

function main() {
  // DEBUG
  setTimeout(() => {
    const http = require('http');
    
    http.get('http://127.0.0.1:8080/1/2/3/4', res => {
      console.log(res.statusCode);
      console.log(JSON.stringify(res.headers, null, 2));

      var body = '';
      res.on('data', chunk => {
        body += chunk;
      });
      res.on('end', () => console.log(JSON.stringify(JSON.parse(body), null, 2)));
    }).on('error', err => console.log(err));
  }, 100);
}

main();