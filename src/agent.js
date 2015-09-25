const server = require('./server/dsl'),
      http   = require('./server/http'),

      Option = require('fantasy-options'),

      url = require('url');

function createServer(handle, port, done) {
    return server.create(handle).chain(x => {
      return server.listen(x, port, done);
    });
}

function main() {
    const handle = (req, res) => {
            /*
            var program = router.compile(x).chain(x => {

            })*/
            console.log("HERE", req.url);
          },
          start = (port) => {
            console.log("Listening on port:", port);
          },
          program = createServer(handle, 8080, Option.Some(start)),
          server  = http.run(program);

    server.unsafePerform();
}

main();