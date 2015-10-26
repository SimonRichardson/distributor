'use strict';

const daggy = require('daggy'),
      Free  = require('fantasy-frees').Free,

      Document = daggy.taggedSum({
        AddHeader   : ['header', 'value', 'res'],
        RemoveHeader: ['header', 'res'],
        StatusCode  : ['code', 'res'],
        Body        : ['body', 'res']
      });

module.exports = {
  addHeader   : (header, value, res) => Free.liftFC(Document.AddHeader(header, value, res)),
  removeHeader: (header, res)        => Free.liftFC(Document.RemoveHeader(header, res)),
  statusCode  : (code, res)          => Free.liftFC(Document.StatusCode(code, res)),
  body        : (body, res)          => Free.liftFC(Document.Body(body, res))
};
