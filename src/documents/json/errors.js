'use strict';

const IO = require('fantasy-io'),

      http = require('http'),

      status = require('./status'),
      body   = require('./body'),
      json   = require('./json'),

      httpStatusCodes = http.STATUS_CODES;

function write(response, res) {
  const copy = x => {
      const res = {};
      for (var i in x) {
        res[i] = x[i];
      }
      return res;  
    },
    insertLength = (headers, len) => {
      const x = copy(headers);
      return !!headers['Content-Length'] ? x : (x['Content-Length'] = len, x);
    };
  return IO(function() {
    const headers = insertLength(response.headers, response.body.length);

    res.writeHead(response.statusCode, headers);
    res.write(response.body);
    res.end();
  });
}

function generic(f, key) {
  return (req, res) => {
    const program = f(json.response()).chain(x => {
      return body.json({
        code   : key,
        message: httpStatusCodes[key],
        data   : {}
      }, x);
    });
    return write(json.run(program), res);
  };
}

function internalError(reasons) {
  return (req, res) => {
    const key = 500,
      program = status.internalServerError(json.response()).chain(x => {
        return body.json({
          code   : key,
          message: httpStatusCodes[key],
          data   : {
            reason: reasons.reverse().toArray().join('\n')
          }
        }, x);
      });
    return write(json.run(program), res);
  };
}

module.exports = {
    // 4xx
    notFound: generic(status.notFound, 404),

    // 5xx
    internalError      : internalError,
    internalServerError: generic(status.internalServerError, 500)
};
