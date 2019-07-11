"use strict";

(function () {
  'use strict';

  var async, crawl, spawn;
  crawl = require('../../crawl/build/crawl.min.js');
  spawn = require("child_process").spawn;
  async = require("async");

  exports.retrieve = function (req, res) {
    var query;

    if (!req.params.name) {
      res.send("Wrong input.");
      return;
    }

    query = {
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
    crawl.retrieve(query, function (result) {
      res.json(result.body.results);
    });
  };

  exports.ner = function (req, res) {
    var pythonProcess, text;
    text = req.query['text'];

    if (!(text && Object.prototype.toString.call(text) === "[object String]")) {
      res.send("Wrong input.");
      return;
    } // python ner


    pythonProcess = spawn('python3', ["/home/uno/repos/TextMining/ner/detect-persons.py", text]);
    pythonProcess.stdout.on('data', function (data) {
      data = JSON.parse(data);
      res.send(data);
    });
  };

  exports.parse = function (req, res) {
    var pythonProcess, text;
    text = req.query['text'];

    if (!(text && Object.prototype.toString.call(text) === "[object String]")) {
      res.send("Wrong input.");
      return;
    }

    pythonProcess = spawn('python3', ["/home/uno/repos/TextMining/ner/detect-persons.py", text]);
    return pythonProcess.stdout.on('data', function (data) {
      var i, len, name, queries;
      data = JSON.parse(data);
      queries = [];

      for (i = 0, len = data.length; i < len; i++) {
        name = data[i];
        queries.push({
          "name": "".concat(name),
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
        });
      }

      return async.mapSeries(queries, function (data, callback) {
        return crawl.retrieve(data, function (wiki) {
          return callback(null, {
            "original": data.name,
            "results": wiki.body.results
          });
        });
      }, function (err, results) {
        res.json({
          "text": text,
          "names": results
        });
      });
    });
  };
}).call(void 0);
//# sourceMappingURL=controller.js.map
