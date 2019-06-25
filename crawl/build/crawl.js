"use strict";

(function () {
  var Crawl, request;
  request = require('request');

  Crawl = function () {
    var retrieve; //public functions
    // federated query over dbpedia & wikidata

    retrieve = function retrieve(config, cb) {
      var groupby, inner, label, limit, prefix, query, s, select; // join search parameters

      s = config.name.trim().split(' ').join(' AND '); // build query

      prefix = 'PREFIX wdt: <http://www.wikidata.org/prop/direct/> ';
      prefix += 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ';
      prefix += 'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> ';
      prefix += 'PREFIX foaf: <http://xmlns.com/foaf/0.1/> ';
      prefix += 'PREFIX owl: <http://www.w3.org/2002/07/owl#> ';
      prefix += 'PREFIX bif: <bif:> ';
      prefix += 'PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> ';
      select = 'SELECT DISTINCT ?wd ?label ?description ';
      inner = 'OPTIONAL{ ?wd <http://schema.org/description> ?description . }';
      label = 'SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". ?Pdescription rdfs:label ?description . ';
      groupby = 'GROUP BY ?wd ?label ?description ';
      config.properties.forEach(function (property, key) {
        if (property.concat == true) {
          select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + 's) ';
        } else {
          select += '?' + property.label + ' ';
          groupby += '?' + property.label + ' ';
        }

        inner += 'OPTIONAL{ ?wd wdt:' + property.name + ' ?P' + property.label + ' . }';
        label += '?P' + property.label + ' rdfs:label ?' + property.label + ' . ';
      });
      label += '?wd rdfs:label ?label . ';
      label += ' } ';
      select += 'WHERE { SERVICE <http://dbpedia.org/sparql> {?dbp a foaf:Person . ?dbp rdfs:label ?l . ?dbp owl:sameAs ?wd . ?l bif:contains "' + s + '" . FILTER(STRSTARTS(xsd:string(?wd), "http://www.wikidata.org/entity/")). }';
      limit = 'LIMIT 1000 ';
      query = prefix + select + inner + 'FILTER(LANG(?description) = "en") ' + label + '}' + groupby + limit; // query wikidata

      request('https://query.wikidata.org/sparql?query=' + encodeURIComponent(query), {
        json: true,
        headers: {
          'User-Agent': 'request'
        }
      }, function (err, res, body) {
        if (err) {
          return console.log(err);
        }

        cb(query, body);
      });
    };

    return {
      // export public functions
      retrieve: retrieve
    };
  }();

  module.exports = Crawl;
}).call(void 0);
//# sourceMappingURL=crawl.js.map
