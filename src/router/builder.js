'use struct';

var daggy = require('daggy'),
    Seq   = require('fantasy-arrays').Seq,
    
    Builder = daggy.tagged('type', 'routes');

Builder.of = (type) => Builder(type, Seq.empty());

Builder.prototype.route = function(x) {
    return Builder(this.type, this.routes.snoc(x));
};

Builder.prototype.lmap = function(f) {
    return Builder(f(this.type), this.routes);
}

Builder.prototype.rmap = function(f) {
    return Builder(this.type, this.routes.map(f));
};

module.exports = {
    build: type => Builder.of(type)
};
