# TextMining

![Process](doc/process.jpg?raw=true "Overview of process")

## Crawl
Querying wikidata for a person by name will most likely timeout, because of one minute execution time at sparql endpoint.
Since wikidata has no indexed data available, but dbpedia has (Virtuoso supports use of several free-text options with bif:contains) it's better to (1) query dbpedia for a person by name (2) fetch dbpedia statement that holds the corresponding person's wikidata concept URI (if available) and finally (3) query wikidata for all metadata that we can ingest.

Since dbpedia's sparql service doesn't support federated queries and wikidata can't use dbpedias bif:contains predicate, we have to query both services sequentially.

The nodejs Wikidata/DBPedia crawler can be found in *crawl* directory.

### Prerequisites
**Debian/Ubuntu**: Please make sure, you have nodejs and npm installed, if not:

    $ sudo apt install nodejs npm

Then you have to install the node modules from *crawl* directory:

    $ npm install

### Retrieve data
Require the file in your js and retrieve data like this:
    
    const crawl = require('./crawl');
    
    // build request
    const request = {
      name: 'Max Planck',
      properties: [
        { name: 'P1477', label: 'birthname'},
        { name: 'P20', label: 'placeofdeath'},
        { name: 'P106', label: 'occupation', concat: true, delimiter: ','},
        { name: 'P108', label: 'employer', concat: true, delimiter: ','},
      ]
    }

    // fire request
    crawl.retrieve(request, function(entity, query, body) {
      console.log(entity);
      console.log(query);
      console.log(body);
    });
    
### Example
You can run the example by:
    
    $ node example.js
