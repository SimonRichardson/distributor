'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Router = daggy.taggedSum({
        ParseRoutes : ['routes'],
        Compile     : ['routes'],
        ParseRequest: ['request'],
        ParseUrl    : ['url'],
        Match       : ['routes', 'url']
      });

module.exports = {
  parseRoutes : routes  => Free.liftFC(Router.ParseRoutes(routes)),
  compile     : routes  => Free.liftFC(Router.Compile(routes)),
  parseRequest: request => Free.liftFC(Router.ParseRequest(request)),
  parseUrl    : url     => Free.liftFC(Router.ParseUrl(url)),
  match       : (routes, request) => Free.liftFC(Router.Match(routes, request))
};
