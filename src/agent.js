var server = require('./server/dsl'),
    http   = require('./server/http');

function createServer(routes, port) {
    return server.create(routes).chain((x) => {
        return server.listen(x, port);
    });
}

function main() {
    var script = createServer({}, 8080);
    http.run(script).unsafePerform();
}

main();