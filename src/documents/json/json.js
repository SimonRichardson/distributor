'use strict';

const Identity = require('fantasy-identities'),
      C        = require('fantasy-combinators'),
      Free     = require('fantasy-frees').Free,
      Lens     = require('fantasy-lenses').Lens,

      lenses = {
        headers   : Lens.objectLens('headers'),
        statusCode: Lens.objectLens('statusCode'),
        body      : Lens.objectLens('body'),
      },

      id       = C.identity,
      constant = C.constant;

function dissoc(l, n, o) {
    // This is wrong atm.
    var x = l.run(o).map(function(x) {
        delete x.headers[n];
        return x;
    });
    return x.extract();
}

function response() {
  return {
    headers   : {},
    statusCode: '',
    body      : {}
  };
}

function interpreter(free) {
  const result = free.cata({
    AddHeader: (header, value, res) => {
      const h = lenses.headers.andThen(Lens.objectLens(header));
      return h.run(res).set(value);
    },
    RemoveHeader: (header, res) => {
      const h = lenses.headers.andThen(Lens.objectLens(header));
      return dissoc(h, header, res);
    },
    StatusCode: (code, res) => {
      return lenses.statusCode.run(res).set(code);
    },
    Body: (body, res) => {
      return lenses.body.run(res).set(body);
    }
  });
  return Identity.of(result);
}

module.exports = {
  response: response,

  run: x => Free.runFC(x, interpreter, Identity).x
};