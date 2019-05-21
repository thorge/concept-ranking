var Crawl = (function() {
  const request = require('request');

  var config = {
    dbpedia: "http://dbpedia.org/sparql",
    wikidata: "https://query.wikidata.org/sparql"
  };

  function retrieve(name, cb) {
    sparql_dbpedia(name, cb);
  }

  // query dbpedia for person by name
  function sparql_dbpedia(name, cb) {
    name = name.replace(' ', ' AND ');
    var query = [
      'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>',
      'PREFIX foaf: <http://xmlns.com/foaf/0.1/>',
      'PREFIX owl: <http://www.w3.org/2002/07/owl#>',
      'SELECT ?wd WHERE {',
      '  ?dbp a foaf:Person .',
      '  ?dbp rdfs:label ?l .',
      '  ?dbp owl:sameAs ?wd .',
      '  ?l bif:contains "' + name + '" .',
      '  FILTER(langMatches(lang(?l), "en")) .',
      '  FILTER(STRSTARTS(xsd:string(?wd), "http://www.wikidata.org/entity/")).',
      '}',
      'LIMIT 25'
    ].join(' ');

    request(config.dbpedia + "?query=" + encodeURIComponent(query), {
      json: true
    }, (err, res, body) => {
      if (err) {
        return console.log(err);
      }
      if (body.hasOwnProperty('results') && body.results.hasOwnProperty('bindings')) {
        body.results.bindings.forEach(function(binding) {
          sparql_wikidata(binding.wd.value, cb);
        });
      } else {
        return console.log("No results.");
      }
    });

    // query wikidata for person by name
    function sparql_wikidata(wdEntity, cb) {
      var query = [
        'PREFIX wd: <http://www.wikidata.org/entity/>',
        'PREFIX wdt: <http://www.wikidata.org/prop/direct/>',
        'SELECT * WHERE {',
        '  <' + wdEntity + '> ?p ?o .',
        '  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }',
        '}',
        'LIMIT 1000'
      ].join(' ');
      // query wikidata
      request(config.wikidata + "?query=" + encodeURIComponent(query), {
        json: true
      }, (err, res, body) => {
        if (err) {
          return console.log(err);
        }
        cb(body);
      });
    }
  };

  // public functions
  return {
    retrieve: retrieve
  };
}());

Crawl.retrieve('Albert Einstein', function(body) {
  console.log(body.results.bindings);
});
