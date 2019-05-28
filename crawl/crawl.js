const request = require('request');
var Crawl = (function() {

  // query dbpedia for person by name
  function retrieve(config, cb) {
    var s = config.name.trim().split(' ').join(' AND ');
    var query = [
      'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>',
      'PREFIX foaf: <http://xmlns.com/foaf/0.1/>',
      'PREFIX owl: <http://www.w3.org/2002/07/owl#>',
      'SELECT DISTINCT ?wd WHERE {',
      '  ?dbp a foaf:Person .',
      '  ?dbp rdfs:label ?l .',
      '  ?dbp owl:sameAs ?wd .',
      '  ?l bif:contains "' + s + '" .',
      //'  FILTER(langMatches(lang(?l), "en")) .',
      '  FILTER(STRSTARTS(xsd:string(?wd), "http://www.wikidata.org/entity/")).',
      '}',
      'LIMIT 25'
    ].join(' ');

    request("http://dbpedia.org/sparql?query=" + encodeURIComponent(query), {
      json: true
    }, (err, res, body) => {
      if (err) {
        return console.log(err);
      }
      if (body.hasOwnProperty('results') && body.results.hasOwnProperty('bindings')) {
        body.results.bindings.forEach(function(binding) {
          query_wikidata(binding.wd.value, config, cb);
        });
      } else {
        return console.log("No results.");
      }
    });
  };
  
  
  // query wikidata for person by name
  function query_wikidata(wdEntity, config, cb) {
    // build query
    var prefix = 'PREFIX wdt: <http://www.wikidata.org/prop/direct/> ';
    var select = 'SELECT DISTINCT ?label ?description ';
    var inner = '<' + wdEntity+ '> <http://schema.org/description> ?description . OPTIONAL{';
    var label = 'SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". ?Pdescription rdfs:label ?description . ';
    var groupby = 'GROUP BY ?label ?description ';
    config.properties.forEach(function (property, key) {
      if (property.concat === true) {
        select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + 's) ';
      } else {
        select += '?' + property.label + ' ';
        groupby += '?' + property.label + ' ';
      }
      inner += '<' + wdEntity+ '> wdt:' + property.name + ' ?P' + property.label + ' . ';
      label += '?P' + property.label + ' rdfs:label ?' + property.label + ' . ';
    });
    label += '<' + wdEntity+ '> rdfs:label ?label . ';
    inner += ' } '
    label += ' } ';
    select += 'WHERE { ';
    var limit = 'LIMIT 1000 ';
    var query = prefix + select + inner + 'FILTER(LANG(?description) = "en") ' + label + '}' + groupby + limit;
    
    
    // query wikidata
    request("https://query.wikidata.org/sparql?query=" + encodeURIComponent(query), {
      json: true
    }, (err, res, body) => {
      if (err) {
        return console.log(err);
      }
      var regex = /http\:\/\/www\.wikidata\.org\/entity\/([a-z0-9\-]+)\&?/i
;
      var wdEntityName = wdEntity.match(regex)[1];
      cb(wdEntityName, query, body);
    });
  }

  // public functions
  return {
    retrieve: retrieve,
    query_wikidata: query_wikidata
  };
}());

module.exports = Crawl
