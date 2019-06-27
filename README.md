# TextMining

![Process](doc/process.jpg?raw=true "Overview of process")

## Crawl
Querying wikidata for a person by name the common way will most likely timeout, because of one minute execution time at sparql endpoint. Since wikidata has no indexed data directly available, but we can make use of the Mediawiki API Service that's available by SPARQL request to use it for full text search.

The nodejs Wikidata crawler can be found in *crawl* directory.

### Prerequisites
**Debian/Ubuntu**: Please make sure, you have nodejs and npm installed, if not:

    $ sudo apt install nodejs npm

If you want to build or run the application, you have to install the corresponding npm modules from *crawl* directory:

    $ cd crawl && npm install

 You may choose to install without the development dependencies by using the *--production* flag:
 
    $ cd crawl && npm install --production

### Build
To build the application, please use *grunt* from within the *crawl* directory:

    $ cd crawl && grunt


### Retrieve data
Require the file in your js and retrieve data like this:
    
    const crawl = require('../build/crawl.min.js');

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
      lang: 'de'
    }

    // fire request
    crawl.retrieve(request, function(res) {
      console.log(res.query);
      console.log(res.body);
    });
    
### Example
You can run the example from within the *example* directory by:
    
    $ cd ./example/ && node example.js
