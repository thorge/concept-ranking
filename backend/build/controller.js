"use strict";

(function () {
  'use strict';

  var crawl;
  crawl = require('../../crawl/build/crawl.min.js');

  exports.retrieve = function (req, res) {
    var r;

    if (!req.params.name) {
      res.send("Wrong input.");
    }

    r = {
      "name": "".concat(req.params.name),
      "description": {
        "label": "description",
        "stopword": true
      },
      "properties": [{
        "name": "P19",
        "label": "placeofbirth"
      }, {
        "name": "P21",
        "label": "sex"
      }, {
        "name": "P20",
        "label": "placeofdeath"
      }, {
        "name": "P106",
        "label": "occupation"
      }, {
        "name": "P108",
        "label": "employer"
      }, {
        "name": "P463",
        "label": "memberof"
      }],
      "limit": 1000,
      "lang": "en"
    };
    crawl.retrieve(r, function (result) {
      res.json(result.body.results);
    });
  };
}).call(void 0);
//# sourceMappingURL=controller.js.map
