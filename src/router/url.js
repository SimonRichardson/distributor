'use strict';

const daggy    = require('daggy'),
      Option   = require('fantasy-options'),
      Identity = require('fantasy-identities'),
      Free     = require('fantasy-frees').Free,
      Seq      = require('fantasy-arrays').Seq,
      C        = require('fantasy-combinators'),
      Either   = require('fantasy-eithers'),

      constant = C.constant,
      identity = C.identity,

      UrlNode = daggy.taggedSum({
        Name    : ['x'],
        Empty   : []
      }),

      Url = daggy.taggedSum({
        Filter: ['x'],
        Split : ['x'],
        Token : ['x'],
        Format: ['x'],
        Valid : ['x']
      });

UrlNode.prototype.equals = function(b) {
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
        Empty   : constant(false),
      });
    },
    Empty   : () => {
      return b.cata({
        Name    : constant(false),
        Empty   : constant(true),
      });
    }
  });
};

UrlNode.prototype.toString = function() {
  return this.cata({
    Name    : x => x.fold(a => 'Name(' + a + ')', () => 'Name'),
    Empty   : constant('Empty')
  });
};

function filter(x) {
  return Free.liftFC(Url.Filter(x));
}

function split(x) {
  return Free.liftFC(Url.Split(x));
}

function token(x) {
  return Free.liftFC(Url.Token(x));
}

function format(x) {
  return Free.liftFC(Url.Format(x));
}

function valid(x) {
  return Free.liftFC(Url.Valid(x));
}

function program(x) {
  return split(x).chain(x => {
    return filter(x).chain(x => {
      return token(x).chain(x => {
        return format(x).chain(x => {
          return valid(x);
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

function type(x) {
  return head(x).map(y => {
    return UrlNode.Name(nil(x));
  }).getOrElse(UrlNode.Empty);
}

function normalise(x) {
  return x.cata({
    Name: y => {
      return UrlNode.Name(y.map(z => decodeURI(z.toLowerCase())));
    },
    Empty: constant(x)
  });
}

function empty() {
  return x => {
    return x.cata({
      Some: constant(false),
      None: constant(true)
    });
  };
}

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
      return x.map(normalise);
    },
    Valid: x => {
      const v = x.filter(x => {
        return x.cata({
          Name    : empty(),
          Empty   : constant(true)
        });
      }, Seq);
      return v.length() < 1 ? Either.Right(x) : Either.Left(v);
    }
  });
  return Identity.of(result);
}

module.exports = {
  Url: UrlNode,

  compile: x => Free.runFC(program(x), interpreter, Identity).x
};
