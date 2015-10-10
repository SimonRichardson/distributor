'use strict';

const daggy    = require('daggy'),
      Identity = require('fantasy-identities'),
      Free     = require('fantasy-frees').Free,
      Seq      = require('fantasy-arrays').Seq,
      C        = require('fantasy-combinators'),
      Either   = require('fantasy-eithers'),

      utils    = require('./utils'),

      constant = C.constant,
      identity = C.identity,

      Path = daggy.taggedSum({
        Name    : ['x'],
        Variable: ['x'],
        Wildcard: [],
        Empty   : []
      });

Path.prototype.equals = function(b) {
  const eq = x => y => {
    return x.fold(
      a  => y.fold(b => a === b, constant(false)),
      () => y.fold(constant(false), constant(true))
    );
  };
  return this.cata({
    Name    : x => {
      return b.cata({
        Name    : eq(x),
        Variable: constant(false),
        Wildcard: constant(false),
        Empty   : constant(false),
      });
    },
    Variable: x => {
      return b.cata({
        Name    : constant(false),
        Variable: eq(x),
        Wildcard: constant(false),
        Empty   : constant(false),
      });
    },
    Wildcard: () => {
      return b.cata({
        Name    : constant(false),
        Variable: constant(false),
        Wildcard: constant(true),
        Empty   : constant(false),
      });
    },
    Empty   : () => {
      return b.cata({
        Name    : constant(false),
        Variable: constant(false),
        Wildcard: constant(false),
        Empty   : constant(true),
      });
    }
  });
};

function normalise() {
  return x => {
    return x.cata({
      Name: y => Path.Name(y.map(z => {
        return z.toLowerCase();
      })),
      Empty: constant(x)
    });
  };
}

function type(x) {
  return utils.head(x).map(y => {
    return y === ':' ? Path.Variable(utils.tail(x)) : 
           y === '*' ? Path.Wildcard :
           Path.Name(utils.nil(x));
  }).getOrElse(Path.Empty);
};

function interpreter(free) {
  const result = free.cata({
    Filter: x => {
      return x.filter(x => x !== '')
    },
    Split: x => {
      return Seq(x.split('/'));
    },
    Token: x => {
      return x.map(type);
    },
    Format: x => {
      return x.map(normalise());
    },
    Valid: x => {
      const v = x.filter(x => {
        return x.cata({
          Name    : utils.empty(),
          Variable: utils.empty(),
          Wildcard: constant(false),
          Empty   : constant(true)
        });
      }, Seq);
      return v.length() < 1 ? Either.Right(x) : Either.Left(v);
    }
  });
  return Identity.of(result);
}

module.exports = {
  compile: x => Free.runFC(utils.program(x), interpreter, Identity).x
};
