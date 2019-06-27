/* example of query */

const crawl = require('../build/crawl.min.js');
const fs = require('fs');

// build request
const request = {
  name: 'Christian Albrecht',
  properties: [
    { name: 'P1477', label: 'birthname'},
    { name: 'P19', label: 'placeofbirth'},
    { name: 'P20', label: 'placeofdeath'},
    { name: 'P106', label: 'occupation', concat: true, delimiter: ','},
    { name: 'P108', label: 'employer', concat: true, delimiter: ','},
    { name: 'P463', label: 'memberof', concat: true, delimiter: ','}   
  ],
  limit: 1000,
  lang: 'en'
}

// fire request
crawl.retrieve(request, function(res) {
  // log 
  console.log(res.query);
  // save possible results to file
  fs.writeFile("./result.json", JSON.stringify(res.body.results.bindings), function(err) {
    if(err) {
        return console.log(err);
    }
    console.log("The file was saved!");
  }); 

});