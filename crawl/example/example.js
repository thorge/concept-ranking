/* example of federated query */

const crawl = require('../build/crawl');
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
  ]
}

// fire request
crawl.retrieve(request, function(query, body) {
  // save possible results to file
  fs.writeFile("./result.json", JSON.stringify(body.results.bindings), function(err) {
    if(err) {
        return console.log(err);
    }
    console.log("The file was saved!");
  }); 

});
