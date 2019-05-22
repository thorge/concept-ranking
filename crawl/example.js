const crawl = require('./crawl');
const fs = require('fs');


crawl.retrieve('Albert Einstein', function(body) {

  
  
  fs.writeFile("./result_"+ Date.now() +".json", JSON.stringify(body.results.bindings), function(err) {
    if(err) {
        return console.log(err);
    }

    console.log("The file was saved!");
  }); 

});
