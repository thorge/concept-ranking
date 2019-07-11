"use strict";

(function () {
  'use strict';

  var crawl, spawn;
  crawl = require('../../crawl/build/crawl.min.js');
  spawn = require("child_process").spawn;

  exports.retrieve = function (req, res) {
    var r;

    if (!req.params.name) {
      res.send("Wrong input.");
      return;
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
        "label": "occupation",
        "stopword": true
      }, {
        "name": "P108",
        "label": "employer",
        "stopword": true,
        "unique": true
      }, {
        "name": "P463",
        "label": "memberof",
        "stopword": true,
        "unique": true
      }],
      "limit": 1000,
      "lang": "en",
      "flatten": true,
      "unique": true
    };
    crawl.retrieve(r, function (result) {
      res.json(result.body.results);
    });
  };

  exports.ner = function (req, res) {
    var pythonProcess, text;
    text = req.query['text'];

    if (!text) {
      res.send("Wrong input.");
      return;
    } // python ner


    pythonProcess = spawn('python3', ["/home/uno/repos/TextMining/ner/detect-persons.py", text]);
    pythonProcess.stdout.on('data', function (data) {
      res.send(data);
    });
  };
}).call(void 0);
//# sourceMappingURL=controller.js.map
