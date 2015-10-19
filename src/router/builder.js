'use struct';

var daggy = require('daggy'),
    Seq   = require('fantasy-arrays').Seq,
    
    Builder = daggy.tagged('type', 'routes'),
    Node    = daggy.tagged('route', 'calls');

Builder.of = (type) => Builder(type, Seq.empty());

Builder.prototype.route = function(x, f) {
    return Builder(this.type, this.routes.snoc(Node(x, Seq.of(f))));
};

Builder.prototype.lmap = function(f) {
    return Builder(f(this.type), this.routes);
}

Builder.prototype.rmap = function(f) {
    return Builder(this.type, this.routes.map(f));
};

Builder.prototype.dimap = function(f, g) {
    return Builder(f(this.type), this.routes.map(g));
};

Node.prototype.equals = function(b) {
    return this.route.equals(b.route);
};

Node.prototype.lmap = function(f) {
    return Node(f(this.route), this.calls);
};

Node.prototype.rmap = function(f) {
    return Node(this.route, f(this.calls));
};

Node.prototype.lfold = function(f, g) {
    return f(this.route);
};

Node.prototype.rfold = function(f, g) {
    return g(this.calls);
};

Node.prototype.toString = function() {
    return 'Node(' + this.route.toString() + ', ' + this.calls.toString() + ')';
};

module.exports = {
    Node   : Node,
    Builder: Builder,
    
    build: type => Builder.of(type)
};
