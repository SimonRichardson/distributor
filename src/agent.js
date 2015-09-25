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
  return dsl.compile(routes);
} 

function main() {
    const routes = {
            "get": {
              "/users": () => x => x,
              "/users/:id": () => x => x,
              "/users/:": () => x => x
            }
          },
          paths = router.compile(createRoutes(routes)),
          handle = (req, res) => {
            // TODO : match url against the compiled routes
          },
          start = (port) => {
            console.log("Listening on port:", port);
          },
          program = createServer(handle, 8080, Option.Some(start));

    server.run(program).unsafePerform();
}

main();