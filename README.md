# Concept ranking by context comparison with blended open data for personal names
## The idea behind
**Concept ranking by context comparison with blended open data for personal names**

What we want to achieve:

![Process](doc/idea.jpg?raw=true "Idea of what we want to achieve.")

## Get started

What we have to do:

1. Named Entity Recognition
2. Fetch Concept Candidates
3. Context Comparison
4. Ranking

![Process](doc/process.jpg?raw=true "Overview of process.")

### 1. Named Entity Recognition
TODO

### 2. Fetch Concept Candidates
We fetch concept candidates by querying a knowledge base (here: Wikidata) for possible entities with the corresponding names, that we found in the NER step. Querying wikidata for a person by name the common way will most likely timeout, because of one minute execution time at sparql endpoint. Since wikidata has no indexed data directly available, but we can make use of the Mediawiki API Service that's available by SPARQL request to use it for full text search.

We created a nodejs application to query Wikidata. You can find the code in *crawl* directory:

    .
    +-- crawl
    |   +-- build/            /* the built application code */
    |   |   +-- crawl.js      /* application js */
    |   |   +-- crawl.js.map  /* sourcemap */
    |   |   +-- crawl.min.js  /* minified application js */
    |   +-- example/          /* javascript examples */
    |   |   +-- example.js    /* simple query example */
    |   +-- src/              /* the coffeescript source files */
    |   |   +-- crawl.coffee  /* application code */
    |   +-- Gruntfile.coffee  /* the configuration for grunt task runner */
    |   +-- package.json      /* node package configuration */
    |   +-- post.html

#### Prerequisites
**Debian/Ubuntu**: Please make sure, you have nodejs and npm installed:

```bash
$ sudo apt install nodejs npm
```

If you want to build or run the application, you must install *grunt*, *coffeescript* and the corresponding npm modules from *crawl* directory:

```bash
$ cd crawl && npm install && sudo npm install -g grunt coffeescript
```

 You may choose to install without the development dependencies by using the *--production* flag:

```bash
$ cd crawl && npm install --production
```

#### Build
To build the application, please use *grunt* from within the *crawl* directory:

```bash
$ cd crawl && grunt
```

#### Retrieve data
Require the file in your js and retrieve data like this:
    
```javascript
const crawl = require('../build/crawl.min.js');

// build request
const request = {
  "name": "Christian Albrecht",
  "description": {
    "stopword": true,
    "delimiter": ","
  },
  "properties": [
    { "name": "P1477", "label": "birthname"},
    { "name": "P19", "label": "placeofbirth"},
    { "name": "P21", "label": "sex"},
    { "name": "P20", "label": "placeofdeath"},
    { "name": "P106", "label": "occupation", "concat": true, "delimiter": ",", "stopword": true, "unique": true},
    { "name": "P108", "label": "employer", "concat": true, "delimiter": ",", "stopword": true, "unique": true},
    { "name": "P463", "label": "memberof", "concat": true, "stopword": true, "unique": true}
  ],
  "limit": 1000,
  "lang": "de"
}

// fire request
crawl.retrieve(request, function(res) {
  console.log(res.query);
  console.log(res.body);
});
```

You have the option to remove stopwords from properties and description text. Stopwords are removed corresponding to selected language. If you want to join the stopwords you may set a delimiter (same delimiter that's used for concatenating). By the way, if you choose to remove stopwords, resultsets for corresponding property are always concatenated before stopword removal. If you want to have unique results from stopword removal, you can by setting unique flag. 

#### 3. Example

You can run the example from within the *example* directory by:
    
```bash
$ cd ./example/ && node example.js
```

You can run any request from command line by:

```bash
$ node build/crawl.js '{
    "name": "Christian Albrecht",
    "description": {
      "stopword": true,
      "delimiter": ","
    },
    "properties": [
      { "name": "P1477", "label": "birthname"},
      { "name": "P19", "label": "placeofbirth"},
      { "name": "P21", "label": "sex"},
      { "name": "P20", "label": "placeofdeath"},
      { "name": "P463", "label": "memberof", "concat": true, "stopword": true, "unique": true}
    ],
    "limit": 1000,
    "lang": "de"
  }'
```

### 4. Context Comparison
TODO

### Ranking
TODO
