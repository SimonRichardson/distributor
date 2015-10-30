'use struct';

var daggy   = require('daggy'),
    helpers = require('fantasy-helpers'),
    Seq   = require('fantasy-arrays').Seq,

    extend    = helpers.extend,
    singleton = helpers.singleton,
    
    Methods = daggy.tagged('methods'),
    Builder = daggy.tagged('type', 'routes'),
    Node    = daggy.tagged('route', 'calls');

Methods.of = builder => Methods(singleton(builder.type, builder));

Methods.prototype.rmap = function(f) {
  const methods = this.methods,
        keys = Seq(Object.keys(methods).sort()),
        result = keys.foldl((acc, x) => {
          return extend(acc, singleton(x, f(methods[x])));
        }, {});
  return Methods(result);
};

Methods.prototype.concat = function(b) {
    const go = (a, b) => {
      var i;
      for(i in b) {
        a[i] = (i in a) ? a[i].join(b[i]) : b[i];
      }
      return a;
    };
    return go(clone({}, this.methods), b.methods);
};

Builder.of = type => Builder(type, Seq.empty());

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

Builder.prototype.join = function(b) {
  return Builder(this.type, this.routes.concat(b.routes));
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

function clone(a, b) {
  var i;
  for(i in b) {
    a[i] = b[i];
  }
  return a;
}

module.exports = {
    Node   : Node,
    Builder: Builder,
    Methods: Methods,

    build  : type => Builder.of(type),
    methods: builder => Methods.of(builder)
};
