const router = require('./router/router'),
      server = require('./server/server'),
      
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
    return dsl.parseUrl(x.url).chain(y => {
      return dsl.match(routes, y);
    });
  });
}

function main() {
    const routes = router.get()
              .route('/1/2/3')
              .route('/1/2/a')
              .route('/a/b/c'),
          paths = router.compile(createRoutes(routes)),
          handle = (req, res) => {
            router.compile(matchRoute(paths, req)).run();
          },
          start = (port) => {
            console.log("Listening on port:", port);

            // DEBUG
            setTimeout(() => {
              var http = require('http');
              http.get('http://127.0.0.1:8080/a/b/c', () => {});
            }, 100);
          },
          program = createServer(handle, 8080, Option.Some(start));

    // This should return an either!
    paths.run();
    server.run(program).unsafePerform();
}

main();