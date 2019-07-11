"use strict";

(function () {
  'use strict';

  var async, crawl, sleep, spawn;
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

  sleep = function sleep(ms) {
    return new Promise(function (resolve) {
      setTimeout(function () {
        resolve();
      }, ms);
    });
  };

  exports.parse = function (req, res) {
    var completed, pythonProcess, queries, result, text;
    text = req.query['text'];

    if (!(text && Object.prototype.toString.call(text) === "[object String]")) {
      res.send("Wrong input.");
      return;
    } // python ner


    result = [];
    queries = [];
    completed = 0;
    pythonProcess = spawn('python3', ["/home/uno/repos/TextMining/ner/detect-persons.py", text]);
    return pythonProcess.stdout.on('data', function (data) {
      var i, len, name;
      data = JSON.parse(data);

      for (i = 0, len = data.length; i < len; i++) {
        name = data[i];
        queries.push([name, {
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
        }]);
      }

      return async.mapSeries(queries, function (data, callback) {
        return crawl.retrieve(data[1], function (wiki) {
          return callback(null, {
            "original": data[0],
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
  }; // tasks = []
  // for query in queries
  //   console.log query
  //   tasks.push (cb) ->
  //     crawl.retrieve query, (wiki) ->
  //       cb(null, wiki.body.results);
  //     return
  // console.log tasks
  //# async.series tasks, (err, results) ->
  //   if err
  //     return next(err)
  //   res.json results
  //async.each queries, ((query, callback) ->
  //  crawl.retrieve query, (wiki) ->
  //    result.push wiki.body.results
  //    callback()
  //    return
  //), (err) ->
  //  #If any of the user creation failed may throw error.
  //  if err
  //    # One of the iterations produced an error.
  //    # All processing will now stop.
  //    console.log 'unable to create user'
  //  else
  //    console.log 'All user created successfully'
  //    res.json result
  //  return
  // results : name1,name2,name3  

}).call(void 0);
//# sourceMappingURL=controller.js.map
