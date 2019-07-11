"use strict";

(function () {
  'use strict';

  module.exports = function (app) {
    var controller;
    controller = require('./controller.min');
    app.use(function (req, res, next) {
      res.header('Access-Control-Allow-Origin', '*');
      res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
      return next();
    });
    app.route('/api/retrieve/:name').get(controller.retrieve);
    app.route('/api/ner').get(controller.ner);
  };
}).call(void 0);
//# sourceMappingURL=routes.js.map
