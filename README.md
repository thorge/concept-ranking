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
### Build

Make sure you have *python3* and *pip* installed:

```bash
$ sudo apt install python3 pip3
```

And of course the corresponding python modules:


```bash
$ python3 -m pip install spacy
$ python3 -m spacy download en_core_web_sm 
$ python3 -m pip install nltk
$ python3 -m pip install pdfminer.six 
```


### 2. Fetch Concept Candidates
We fetch concept candidates by querying a knowledge base (here: Wikidata) for possible entities with the corresponding names, that we found in the NER step. Querying wikidata for a person by name the common way will most likely timeout, because of one minute execution time at sparql endpoint. Wikidata has no indexed data directly available, but we can make use of the Mediawiki API Service that's also available by SPARQL request and use it for full text search.

We created a nodejs application to query Wikidata. You can find the code in *crawl* directory:

    .
    +-- crawl
    |   +-- build/            /* the built application code */
    |   |   +-- crawl.js      /* application js */
    |   |   +-- crawl.js.map  /* sourcemap */
    |   |   +-- crawl.min.js  /* minified application js */
    |   +-- example/          /* javascript examples */
    |   |   +-- example.js    /* query example */
    |   |   +-- example2.js   /* flattened query example */
    |   +-- src/              /* the coffeescript source files */
    |   |   +-- crawl.coffee  /* application code */
    |   +-- Gruntfile.coffee  /* grunt task runner configuration */
    |   +-- .babelrc          /* babel configuration */
    |   +-- package.json      /* node package configuration */

#### Prerequisites
**Debian/Ubuntu**: Please make sure, you have nodejs and npm installed:

```bash
$ sudo apt install nodejs npm
```

If you want to run the application, you must install the corresponding npm modules:

```bash
$ cd crawl && npm install
```

 You may choose to install without the development dependencies by using the *--production* flag:

```bash
$ cd crawl && npm install --production
```

#### Build
If you want to build the application, you must install *grunt*, *coffeescript* and the corresponding npm modules:

```bash
$ cd crawl && npm install && sudo npm install -g grunt coffeescript
```

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
    "label": "description",
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
  "lang": "en"
}

// fire request
crawl.retrieve(request, function(res) {
  console.log(res.query);
  console.log(res.body);
});
```

You have the option to remove stopwords from properties and description text. Stopwords are removed corresponding to selected language. If you want to join the stopwords you may set a delimiter (same delimiter that's used for concatenating). By the way, if you choose to remove stopwords, resultsets for corresponding property are always concatenated before stopword removal. If you want to have unique results from stopword removal, you can by setting unique flag. 

#### Example 1 - Simple Query

You can run the example from within the *example* directory by:
    
```bash
$ cd ./example/ && node example.js
```

You can run any request from command line by:

**Query**
```bash
$ node build/crawl.js '{
    "name": "Max Planck",
    "description": {
      "label": "description",
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
    "lang": "en"
  }'
```

**Result**
```json
[
   {
      "item":{
         "type":"uri",
         "value":"http://www.wikidata.org/entity/Q9021"
      },
      "label":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Max Planck"
      },
      "description":{
         "xml:lang":"en",
         "type":"literal",
         "value":[
            "German",
            "theoretical",
            "physicist"
         ]
      },
      "birthname":{
         "type":"literal",
         "value":"Max Karl Ernst Ludwig Planck"
      },
      "placeofbirth":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Kiel"
      },
      "sex":{
         "xml:lang":"en",
         "type":"literal",
         "value":"male"
      },
      "placeofdeath":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Göttingen"
      },
      "occupation":{
         "type":"literal",
         "value":"university,teacher,theoretical,physicist"
      },
      "employer":{
         "type":"literal",
         "value":"Ludwig,Maximilian,University,Munich,Humboldt,Berlin,University,Kiel,Frederick,William"
      },
      "memberof":{
         "type":"literal",
         "value":"Russian Academy of Sciences, Royal Society, Royal Swedish Academy of Sciences, Royal Netherlands Academy of Arts and Sciences, Q292692, Hungarian Academy of Sciences, Royal Prussian Academy of Sciences, Lincean Academy, Saxon Academy of Sciences, American Academy of Arts and Sciences, American Philosophical Society, Bavarian Academy of Sciences and Humanities, Kaiser Wilhelm Society, Deutsche Physikalische Gesellschaft, Pontifical Academy of Sciences, Notgemeinschaft der Deutschen Wissenschaft, Q2301163, German Academy of Sciences Leopoldina, Academy of Sciences of the USSR, Academy of Sciences of Turin, Saint Petersburg Academy of Sciences"
      }
   },
   {
      "item":{
         "type":"uri",
         "value":"http://www.wikidata.org/entity/Q1913219"
      },
      "label":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Max Planck"
      },
      "description":{
         "xml:lang":"en",
         "type":"literal",
         "value":[
            "German",
            "classical",
            "philologist",
            "historian",
            "high",
            "school",
            "teacher"
         ]
      },
      "placeofbirth":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Feuerbach"
      },
      "sex":{
         "xml:lang":"en",
         "type":"literal",
         "value":"male"
      },
      "placeofdeath":{
         "xml:lang":"en",
         "type":"literal",
         "value":"Stuttgart"
      },
      "occupation":{
         "type":"literal",
         "value":"historian,pedagogue,high,school,teacher,classical,philologist"
      },
      "employer":{
         "type":"literal",
         "value":""
      },
      "memberof":{
         "type":"literal",
         "value":""
      }
   }
]
```

#### Example 2 - Flattened results

Example 2 returns flattened unique values for every concept found. You can run the example 2 from within the *example* directory by:
    
```bash
$ cd ./example/ && node example2.js
```

You can run the request also from command line by:

**Query**
```bash
$ node build/crawl.js '{
    "name": "Max Planck",
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
    "lang": "de",
    "flatten": true,
    "unique": true
  }'
```

**Result**
```json
[
   {
      "label":"Max Planck",
      "item":"http://www.wikidata.org/entity/Q9021",
      "value":[
         "deutscher",
         "Physiker",
         "Kiel",
         "männlich",
         "Göttingen",
         "Hochschullehrer",
         "theoretischer",
         "Ludwig-Maximilians-Universität",
         "München",
         "Humboldt-Universität",
         "Berlin",
         "Christian-Albrechts-Universität",
         "Friedrich-Wilhelms-Universität",
         "Königlich",
         "Schwedische",
         "Akademie",
         "Wissenschaften",
         "Russische",
         "Royal",
         "Society",
         "Niederländische",
         "Ungarische",
         "Preußische",
         "Akademischer",
         "Gesangverein",
         "Accademia",
         "Nazionale",
         "dei",
         "Lincei",
         "Sächsische",
         "American",
         "Academy",
         "of",
         "Arts",
         "and",
         "Sciences",
         "Philosophical",
         "Deutsche",
         "Naturforscher",
         "Leopoldina",
         "Kaiser-Wilhelm-Gesellschaft",
         "Förderung",
         "UdSSR",
         "Bayerische",
         "delle",
         "Scienze",
         "Turin",
         "Physikalische",
         "Gesellschaft",
         "Päpstliche",
         "Notgemeinschaft",
         "deutschen",
         "Wissenschaft",
         "Sondershäuser",
         "Verband",
         "Sankt",
         "Petersburger"
      ]
   },
   {
      "label":"Max Planck",
      "item":"http://www.wikidata.org/entity/Q1913219",
      "value":[
         "deutscher",
         "Klassischer",
         "Philologe",
         "Althistoriker",
         "Gymnasialdirektor",
         "Feuerbach",
         "männlich",
         "Stuttgart",
         "Historiker",
         "Pädagoge",
         "Gymnasiallehrer"
      ]
   }
]
```

#### Backend

You can also start a nodejs server from *backend* directory.

```bash
cd backend && npm install && npm run start
```

Then you can retrieve data like this: http://localhost:8081/api/retrieve/Max%20Planck

### 4. Context Comparison AND Ranking

Install the haskell-platform (ghci and cabal) 
```bash
$ sudo apt-get install haskell-platform
```

Install the needed haskell-libraries

```bash
$ cabal install string-conversions
$ cabal install text 
$ cabal install aeson


```


Take The QueryResult and compute the Rankings and 
Go into the RankNLink directory

```bash

$ /.Main "path/to/the/queryresult/file.json" "/path/to/the/outputdirectory/file.json" 

```


