'use strict'
crawl = require('../../crawl/build/crawl.min.js')
spawn = require("child_process").spawn;


exports.retrieve = (req, res) ->
  unless req.params.name
    res.send "Wrong input."
    return
  r = {
    "name": "#{req.params.name}",
    "description": {
      "label": "description",
      "stopword": true
    },
    "properties": [
      { "name": "P19", "label": "placeofbirth"},
      { "name": "P21", "label": "sex"},
      { "name": "P20", "label": "placeofdeath"},
      { "name": "P106", "label": "occupation", "stopword": true},
      { "name": "P108", "label": "employer", "stopword": true, "unique": true},
      { "name": "P463", "label": "memberof", "stopword": true, "unique": true}
    ],
    "limit": 1000,
    "lang": "en",
    "flatten": true,
    "unique": true
  }
  crawl.retrieve r, (result) ->
    res.json result.body.results
    return
  return
  
exports.ner = (req, res) ->
  text = req.query['text']
  unless text
    res.send "Wrong input."
    return
  # python ner
  pythonProcess = spawn('python3',["/home/uno/repos/TextMining/ner/detect-persons.py", text]);
  pythonProcess.stdout.on 'data', (data) ->
    res.send data
    return
  return
