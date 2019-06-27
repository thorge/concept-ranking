request = require('request')
Crawl = do ->
  endpoint = 'https://query.wikidata.org/sparql?query='
  config = {
    limit: 1000,
    lang: 'en'
  }
  
  # Retrieve data from Wikidata
  # Makes use of Mediawiki API Service for full text search.
  retrieve = (c, cb) ->
    Object.assign config, c
    unless config.name
      return []
    # build query
    select = """
    PREFIX wdt: <http://www.wikidata.org/prop/direct/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n
    SELECT DISTINCT ?item ?label ?description 
    """
    optional = '\n  OPTIONAL{ ?item <http://schema.org/description> ?description . }'
    label = """
    \n  SERVICE wikibase:label {
        bd:serviceParam wikibase:language "[AUTO_LANGUAGE],#{config.lang}".
        ?Pdescription rdfs:label ?description . 
    """
    groupby = '\nGROUP BY ?item ?label ?description '
    if config.properties
      config.properties.forEach (property, key) ->
        if property.concat is true
          select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + 's) '
        else
          select += "?#{property.label} "
          groupby += "?#{property.label} "
        optional += "\n  OPTIONAL{ ?item wdt:#{property.name} ?P#{property.label} . }"
        label += "\n    ?P#{property.label} rdfs:label ?#{property.label} . "
        return
    label += '\n    ?item rdfs:label ?label . '
    label += '\n  } '
    where = """
    WHERE {
      SERVICE wikibase:mwapi {
        bd:serviceParam wikibase:api "EntitySearch" .
        bd:serviceParam wikibase:endpoint "www.wikidata.org" .
        bd:serviceParam mwapi:search "#{config.name}" .
        bd:serviceParam mwapi:language "#{config.lang}" .
        ?item wikibase:apiOutputItem mwapi:item .
        ?num wikibase:apiOrdinal true .
      }
      ?item wdt:P31 wd:Q5. 
    """
    query = select + where + optional + '\n  FILTER(LANG(?description) = "' + config.lang + '") ' + label + '\n}' + groupby + '\nLIMIT '+ config.limit
    
    # query endpoint
    request endpoint + encodeURIComponent(query), {
      json: true
      headers: 'User-Agent': 'request'
    }, (err, res, body) ->
      if err
        return console.log(err)
      cb { query: query, body: body }
      return
    return

  # return public functions
  { retrieve: retrieve }
module.exports = Crawl
