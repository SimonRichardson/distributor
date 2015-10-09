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

      PathNode = daggy.taggedSum({
        Name    : ['x'],
        Variable: ['x'],
        Wildcard: [],
        Empty   : []
      }),

      Path = daggy.taggedSum({
        Filter: ['x'],
        Split : ['x'],
        Token : ['x'],
        Format: ['x'],
        Valid : ['x']
      });

UrlNode.type = function(x) {
  return head(x).map(y => {
    return UrlNode.Name(nil(x));
  }).getOrElse(UrlNode.Empty);
};

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

PathNode.type = function(x) {
  return head(x).map(y => {
    return y === ':' ? PathNode.Variable(tail(x)) : 
           y === '*' ? PathNode.Wildcard :
           PathNode.Name(nil(x));
  }).getOrElse(PathNode.Empty);
};

PathNode.prototype.equals = function(b) {
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

function filter(x) {
  return Free.liftFC(Path.Filter(x));
}

function split(x) {
  return Free.liftFC(Path.Split(x));
}

function token(x) {
  return Free.liftFC(Path.Token(x));
}

function format(x) {
  return Free.liftFC(Path.Format(x));
}

function valid(x) {
  return Free.liftFC(Path.Valid(x));
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

function normalise(f) {
  return x => {
    return x.cata({
      Name: y => {
        return PathNode.Name(y.map(f));
      },
      Variable: constant(x),
      Wildcard: constant(x),
      Empty: constant(x)
    });
  };
}

function empty() {
  return x => {
    return x.cata({
      Some: constant(false),
      None: constant(true)
    });
  };
}

function pathInterpreter(free) {
  const result = free.cata({
    Filter: x => {
      return x.filter(x => x !== '')
    },
    Split: x => {
      return Seq(x.split('/'));
    },
    Token: x => {
      return x.map(PathNode.type);
    },
    Format: x => {
      return x.map(normalise(x => x.toLowerCase()));
    },
    Valid: x => {
      const v = x.filter(x => {
        return x.cata({
          Name    : empty(),
          Variable: empty(),
          Wildcard: constant(false),
          Empty   : constant(true)
        });
      }, Seq);
      return v.length() < 1 ? Either.Right(x) : Either.Left(v);
    }
  });
  return Identity.of(result);
}

function uriInterpreter(free) {
  const result = free.cata({
    Filter: x => {
      return x.filter(x => x !== '')
    },
    Split: x => {
      return Seq(x.split('/'));
    },
    Token: x => {
      return x.map(UrlNode.type);
    },
    Format: x => {
      return x.map(normalise(x => decodeURI(x.toLowerCase())));
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
  Path: PathNode,
  Url : UrlNode,

  path: {
    compile: x => Free.runFC(program(x), pathInterpreter, Identity).x
  },
  url: {
    compile: x => Free.runFC(program(x), uriInterpreter, Identity).x
  }
};
