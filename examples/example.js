'use strict';

const errors = require('./../src/documents/json/errors'),
      router = require('./../src/router/router'),
      server = require('./../src/server/server'),
      
      debug = require('./debug'),

      IO     = require('fantasy-io'),
      Option = require('fantasy-options'),
      C      = require('fantasy-combinators'),

      constant = C.constant;

function createServer(routes, port, done) {
  const dsl = server.dsl;
  return dsl.options({}).chain(x => {
    return dsl.compile(routes).chain(handle => {
      return dsl.create(x, handle).chain(y => {
        return dsl.listen(y, port, done);
      });
    });
  });
}

function print(x) {
  return IO(() => console.log(x));
}

function main() {
    const get = router.get()
              .route('/1/2/:id', (req, res) => print('A'))
              .route('/1/2/3', (req, res) => print('B'))
              .route('/1/2/3', (req, res) => print('C')),
          head = get.lmap(constant('head')),

          routes = router.methods(get).concat(head),

          program = createServer(routes, 8080, Option.None);

    server.run(errors, program).unsafePerform()
}

main();
