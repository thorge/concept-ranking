var Crawl = (function() {
  const request = require('request');

  function retrieve(name, config, cb) {
    sparql_dbpedia(name, config, cb);
  }

  // query dbpedia for person by name
  function sparql_dbpedia(config, cb) {
    name = config.name.trim().split(' ').join(' AND ');
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

    request("http://dbpedia.org/sparql?query=" + encodeURIComponent(query), {
      json: true
    }, (err, res, body) => {
      if (err) {
        return console.log(err);
      }
      if (body.hasOwnProperty('results') && body.results.hasOwnProperty('bindings')) {
        body.results.bindings.forEach(function(binding) {
          sparql_wikidata(binding.wd.value, config, cb);
        });
      } else {
        return console.log("No results.");
      }
    });
  };
  
  // query wikidata for person by name
  function sparql_wikidata(wdEntity, config, cb) {
    // build query
    var query = [
      'PREFIX wd: <http://www.wikidata.org/entity/>',
      'PREFIX wdt: <http://www.wikidata.org/prop/direct/>',
    ]
    var select = 'SELECT DISTINCT ?description ';
    var inner = '<' + wdEntity+ '> <http://schema.org/description> ?description .';
    var label = 'SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". ?Pdescription rdfs:label ?description . ';
    var groupby = 'GROUP BY ?description ';
    config.properties.forEach(function (property, key) {
      if (property.concat === true) {
        select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + 's)';
      } else {
        select += '?' + property.label + ' ';
        groupby += '?' + property.label + ' ';
      }
      inner += 'OPTIONAL{ <' + wdEntity+ '> wdt:' + property.name + ' ?P' + property.label + ' . } ';
      label += '?P' + property.label + ' rdfs:label ?' + property.label + ' .';
    });
    label += ' }';
    select += 'WHERE {';
    query.push(select);
    query.push(inner);    
    query.push('FILTER(LANG(?description) = "en")');
    query.push(label);
    query.push('}');
    query.push(groupby);
    query.push('LIMIT 1000');
    query = query.join(' ');
    
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
    retrieve: retrieve
  };
}());

module.exports = Crawl
