'use strict';

const errors = require('./documents/json/errors'),
      router = require('./router/router'),
      server = require('./server/server'),
      
      debug = require('./debug'),

      IO     = require('fantasy-io'),
      Option = require('fantasy-options');

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
    const routes = router.get()
              .route('/1/2/a', (req, res) => print('A'))
              .route('/1/2/3', (req, res) => print('B'))
              .route('/1/2/3', (req, res) => print('C')),

          program = createServer(routes, 8080, Option.None);

    server.run(errors, program).unsafePerform()
}

main();
