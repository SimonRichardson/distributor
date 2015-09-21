var daggy = require('daggy'),
    Free  = require('fantasy-frees').Free,

    Server = daggy.taggedSum({
        Create: ['routes'],
        Listen: ['x', 'port']
    });

function create(x) {
    return Free.liftFC(Server.Create(x));
}

function listen(x, y) {
    return Free.liftFC(Server.Listen(x, y));
}

module.exports = {
    create: create,
    listen: listen
};
