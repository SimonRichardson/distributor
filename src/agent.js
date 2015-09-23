const server = require('./server/dsl'),
      http   = require('./server/http');

function createServer(handle, port, done) {
    return server.create(handle).chain((x) => {
      return server.listen(x, port, done);
    });
}

function main() {
    const handle = (req, res) => {
            console.log("here", req, res);
          },
          start = (port) => {
            console.log("Listening on port:", port);
          },
          program = createServer(handle, 8080, start),
          server  = http.run(program);

    server.unsafePerform();
}

main();