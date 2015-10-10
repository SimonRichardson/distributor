'use strict';

const Option = require('fantasy-options'),
      C      = require('fantasy-combinators'),
      
      dsl = require('./dsl'),

      constant = C.constant,
      identity = C.identity;

function program(x) {
  return dsl.split(x).chain(x => {
    return dsl.filter(x).chain(x => {
      return dsl.token(x).chain(x => {
        return dsl.format(x).chain(x => {
          return dsl.valid(x);
        });
      });
    });
  });
}

function head(x) {
  return x.length > 0 ? Option.Some(x[0]) : Option.None;
}

function tail(x) {
  return x.length > 1 ? Option.Some(x.substr(1)) : Option.None;
}

function nil(x) {
  return x.length > 0 ? Option.Some(x) : Option.None;
}

function empty() {
  return x => {
    return x.cata({
      Some: constant(false),
      None: constant(true)
    });
  };
}

module.exports = {
    program: program,
    head   : head,
    tail   : tail,
    nil    : nil,
    empty  : empty
};