"use strict";

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

(function () {
  var Crawl, request, sw;
  request = require('request');
  sw = require('stopword');

  Crawl = function () {
    var config, endpoint, retrieve, uniq;
    endpoint = 'https://query.wikidata.org/sparql?query=';
    config = {
      limit: 1000,
      lang: 'en'
    }; // Helper

    uniq = function uniq(a) {
      var objs, prims;
      prims = {
        'boolean': {},
        'number': {},
        'string': {}
      };
      objs = [];
      return a.filter(function (item) {
        var type;
        type = _typeof(item);

        if (type in prims) {
          if (prims[type].hasOwnProperty(item)) {
            return false;
          } else {
            return prims[type][item] = true;
          }
        } else {
          if (objs.indexOf(item) >= 0) {
            return false;
          } else {
            return objs.push(item);
          }
        }
      });
    }; // Retrieve data from Wikidata
    // Makes use of Mediawiki API Service for full text search.


    retrieve = function retrieve(c, cb) {
      var groupby, label, optional, query, select, where;
      Object.assign(config, c);

      if (!(config.name && config.properties && config.lang)) {
        return [];
      } // build query


      select = "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\nSELECT DISTINCT ?item ?label ?description ";
      optional = '\n  OPTIONAL{ ?item <http://schema.org/description> ?description . }';
      label = "\n  SERVICE wikibase:label {\n    bd:serviceParam wikibase:language \"[AUTO_LANGUAGE],".concat(config.lang, "\".\n    ?Pdescription rdfs:label ?description . ");
      groupby = '\nGROUP BY ?item ?label ?description ';
      config.properties.forEach(function (property, key) {
        if (property.concat === true) {
          select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + ') ';
        } else {
          select += "?".concat(property.label, " ");
          groupby += "?".concat(property.label, " ");
        }

        optional += "\n  OPTIONAL{ ?item wdt:".concat(property.name, " ?P").concat(property.label, " . }");
        label += "\n    ?P".concat(property.label, " rdfs:label ?").concat(property.label, " . ");
      });
      label += '\n    ?item rdfs:label ?label . ';
      label += '\n  } ';
      where = "WHERE {\n  SERVICE wikibase:mwapi {\n    bd:serviceParam wikibase:api \"EntitySearch\" .\n    bd:serviceParam wikibase:endpoint \"www.wikidata.org\" .\n    bd:serviceParam mwapi:search \"".concat(config.name, "\" .\n    bd:serviceParam mwapi:language \"").concat(config.lang, "\" .\n    ?item wikibase:apiOutputItem mwapi:item .\n    ?num wikibase:apiOrdinal true .\n  }\n  ?item wdt:P31 wd:Q5. ");
      query = select + where + optional + '\n  FILTER(LANG(?description) = "' + config.lang + '") ' + label + '\n}' + groupby + '\nLIMIT ' + config.limit; // query endpoint

      request(endpoint + encodeURIComponent(query), {
        json: true,
        headers: {
          'User-Agent': 'request'
        }
      }, function (err, res, body) {
        if (err) {
          return console.log(err);
        } // remove stopwords


        config.properties.forEach(function (property, key) {
          if (property.stopword === true) {
            return body.results.bindings.forEach(function (item) {
              var s;

              if (item[property.label].value) {
                s = sw.removeStopwords(item[property.label].value.match(/\b(\S+)\b/g), sw[config.lang]);

                if (property.unique === true) {
                  s = uniq(s);
                }

                if (property.delimiter) {
                  s = s.join(property.delimiter);
                }

                return item[property.label].value = s;
              }
            });
          }
        });

        if (config.description.stopword === true) {
          body.results.bindings.forEach(function (item) {
            var s;

            if (item.description.value) {
              s = sw.removeStopwords(item.description.value.match(/\b(\S+)\b/g), sw[config.lang]);

              if (config.description.unique === true) {
                s = uniq(s);
              }

              if (config.description.delimiter) {
                s = s.join(config.description.delimiter);
              }

              return item.description.value = s;
            }
          });
        }

        console.log(body.results.bindings);
        cb({
          query: query,
          body: body
        });
      });
    };

    return {
      // return public functions
      retrieve: retrieve
    };
  }();

  module.exports = Crawl;
}).call(void 0);
//# sourceMappingURL=crawl.js.map
