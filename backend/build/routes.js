"use strict";

(function () {
  'use strict';

  module.exports = function (app) {
    var crawl;
    crawl = require('./controller.min');
    app.use(function (req, res, next) {
      res.header('Access-Control-Allow-Origin', '*');
      res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
      return next();
    });
    app.route('/api/retrieve/:name').get(crawl.retrieve);
  };
}).call(void 0);
//# sourceMappingURL=routes.js.map
