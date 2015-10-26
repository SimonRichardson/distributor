'use strict';

const doc = require('./dsl');

function replaceHeader(header, value, res) {
  return doc.removeHeader(header, res).chain(x => {
    return doc.addHeader(header, value, res);
  });
}

module.exports = {
  replaceHeader: replaceHeader
};
