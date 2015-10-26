'use strict';

const doc    = require('./../dsl'),
      header = require('./../header');

function json(x, y) {
  return header.replaceHeader('Content-Type', 'application/json', y)
    .chain(z => doc.body(JSON.stringify(x), z));
}

function plain(x, y) {
  return header.replaceHeader('Content-Type', 'text/plain', y)
    .chain(z => doc.body(x, z));
}

module.exports = {
  json : json,
  plain: plain
};
