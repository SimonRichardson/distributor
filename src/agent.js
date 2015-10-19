const router = require('./router/router'),
      server = require('./server/server'),
      
      debug = require('./debug'),

      IO     = require('fantasy-io'),
      Option = require('fantasy-options');

function createServer(handle, port, done) {
  const dsl = server.dsl;
  return dsl.options({}).chain(x => {
    return dsl.create(x, handle).chain(y => {
      return dsl.listen(y, port, done);
    });
  });
}

function createRoutes(routes) {
  const dsl = router.dsl;
  return dsl.parseRoutes(routes).chain(x => {
    return dsl.compile(x);
  });
}

function matchRoute(routes, request) {
  const dsl = router.dsl;
  return dsl.parseRequest(request).chain(x => {
    return dsl.parseUrl(x).chain(y => {
      return dsl.match(routes, y).chain(z => {
        return dsl.caller(z);
      });
    });
  });
}

function internalServerError(request, response) {
  console.log('InternalServerError');
}

function notFound(request, response) {
  console.log('NotFound');
}

function main() {
    const routes = router.get()
              .route('/1/2/a', () => console.log("A"))
              .route('/1/2/3', () => console.log("B"))
              .route('/1/2/3', () => console.log("C")),
          paths = router.compile(createRoutes(routes)),
          handle = (req, res) => {
            // Handle the paths
            paths.fold(
              x => internalServerError(req, res),
              y => {
                // Route matcher
                router.compile(matchRoute(y, req)).fold(
                  x => notFound(req, res),
                  y => y.getOrElse(notFound)(req, res)
                );
              }
            );
          },
          start = (port) => {
            console.log("Listening on port:", port);

            // DEBUG
            setTimeout(() => {
              var http = require('http');
              http.get('http://127.0.0.1:8080/1/2/3', () => {})
                .on('error', (err) => console.log(err));
            }, 100);
          },
          program = createServer(handle, 8080, Option.Some(start)),
          io      = paths.fold(
            x => IO(() => console.log(x.reverse().toArray().join("\n"))),
            y => server.run(program)
          );

    io.unsafePerform()
}

main();