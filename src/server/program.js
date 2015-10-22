'use strict';

const router = require('./../router/router');

function create(routes) {
  const dsl = router.dsl;
  return dsl.parseRoutes(routes).chain(x => {
    return dsl.compile(x);
  });
}

function match(routes, request) {
  const dsl = router.dsl;
  return dsl.parseRequest(request).chain(x => {
    return dsl.parseUrl(x).chain(y => {
      return dsl.match(routes, y).chain(z => {
        return dsl.caller(z);
      });
    });
  });
}

module.exports = {
  create: create,
  match : match,
};
