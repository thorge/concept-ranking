# TextMining

![Process](doc/process.jpg?raw=true "Overview of process")

## Crawl
Querying wikidata for a person by name will most likely timeout, because of one minute execution time at sparql endpoint.
Since wikidata has no indexed data available, but dbpedia has (Virtuoso supports use of several free-text options with bif:contains) we build a federated query.

The nodejs Wikidata/DBPedia crawler can be found in *crawl* directory.

### Prerequisites
**Debian/Ubuntu**: Please make sure, you have nodejs and npm installed, if not:

    $ sudo apt install nodejs npm

Then you have to install the node modules from *crawl* directory:

    $ npm install

### Build
To build the application, please use *grunt* from within the *crawl* directory:

    $ cd crawl && grunt


### Retrieve data
Require the file in your js and retrieve data like this:
    
    const crawl = require('./build/crawl');
    
    // build request
    const request = {
      name: 'Christian Albrecht',
      properties: [
        { name: 'P1477', label: 'birthname'},
        { name: 'P20', label: 'placeofdeath'},
        { name: 'P106', label: 'occupation', concat: true, delimiter: ','},
        { name: 'P108', label: 'employer', concat: true, delimiter: ','},
      ]
    }

    // fire request
    crawl.retrieve(request, function(query, body) {
      console.log(query);
      console.log(body);
    });
    
### Example
You can run the example from within the *example* directory by:
    
    $ cd ./example/ && node example.js
