var IO   = require('fantasy-io'),
    C    = require('fantasy-combinators'),
    Free = require('fantasy-frees').Free,

    http = require('http'),

    id = C.identity;

function interpreter(free) {
    return free.cata({
        Create: (x) => {
            return IO(() => {
                return http.createServer((req, res) => {
                    console.log(req, res);
                });
            });
        },
        Listen: (x, y) => {
            return IO(() => {
                x.listen(y);
            });
        }
    });
}

module.exports = {
    run: (x) => {
        return Free.runFC(x, interpreter, IO);
    }
};
