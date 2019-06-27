"use strict";

(function () {
  var Crawl, request;
  request = require('request');

  Crawl = function () {
    var config, endpoint, retrieve;
    endpoint = 'https://query.wikidata.org/sparql?query=';
    config = {
      limit: 1000,
      lang: 'en'
    }; // Retrieve data from Wikidata
    // Makes use of Mediawiki API Service for full text search.

    retrieve = function retrieve(c, cb) {
      var groupby, label, optional, query, select, where;
      Object.assign(config, c);

      if (!config.name) {
        return [];
      } // build query


      select = "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\nSELECT DISTINCT ?item ?label ?description ";
      optional = '\n  OPTIONAL{ ?item <http://schema.org/description> ?description . }';
      label = "\n  SERVICE wikibase:label {\n    bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],".concat(config.lang, "\".\n    ?Pdescription rdfs:label ?description . ");
      groupby = '\nGROUP BY ?item ?label ?description ';

      if (config.properties) {
        config.properties.forEach(function (property, key) {
          if (property.concat === true) {
            select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + 's) ';
          } else {
            select += "?".concat(property.label, " ");
            groupby += "?".concat(property.label, " ");
          }

          optional += "\n  OPTIONAL{ ?item wdt:".concat(property.name, " ?P").concat(property.label, " . }");
          label += "\n    ?P".concat(property.label, " rdfs:label ?").concat(property.label, " . ");
        });
      }

      label += '\n    ?item rdfs:label ?label . ';
      label += '\n  } ';
      where = "WHERE {\n  SERVICE wikibase:mwapi {\n    bd:serviceParam wikibase:api \"EntitySearch\" .\n    bd:serviceParam wikibase:endpoint \"www.wikidata.org\" .\n    bd:serviceParam mwapi:search \"".concat(config.name, "\" .\n    bd:serviceParam mwapi:language \"en\" .\n    ?item wikibase:apiOutputItem mwapi:item .\n    ?num wikibase:apiOrdinal true .\n  }\n  ?item wdt:P31 wd:Q5. ");
      query = select + where + optional + '\n  FILTER(LANG(?description) = "' + config.lang + '") ' + label + '\n}' + groupby + '\nLIMIT ' + config.limit; // query wikidata

      request(endpoint + encodeURIComponent(query), {
        json: true,
        headers: {
          'User-Agent': 'request'
        }
      }, function (err, res, body) {
        if (err) {
          return console.log(err);
        }

        cb({
          query: query,
          body: body
        });
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
