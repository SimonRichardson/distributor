'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Router = daggy.taggedSum({
        ParseRoutes : ['routes'],
        Compile     : ['routes'],
        ParseRequest: ['request'],
        ParseUrl    : ['request'],
        Match       : ['routes', 'request']
      });

module.exports = {
  parseRoutes : routes  => Free.liftFC(Router.ParseRoutes(routes)),
  compile     : routes  => Free.liftFC(Router.Compile(routes)),
  parseRequest: request => Free.liftFC(Router.ParseRequest(request)),
  parseUrl    : request => Free.liftFC(Router.ParseUrl(request)),
  match       : (routes, request) => Free.liftFC(Router.Match(routes, request))
};
