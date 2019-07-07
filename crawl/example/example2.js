/* example of query */

const crawl = require('../build/crawl.min.js');
const fs = require('fs');

// build request
const request = {
  "name": "Max Planck",
  "description": {
    "label": "description", "stopword": true
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
  "lang": "de",
  "flatten": true,
  "unique": true
}

// fire request
crawl.retrieve(request, function(res) {
  // log 
  console.log(res.query);
  // save possible results to file
  fs.writeFile("./result.json", JSON.stringify(res.body.results), function(err) {
    if(err) {
        return console.log(err);
    }
    console.log("The file was saved!");
  }); 

});
