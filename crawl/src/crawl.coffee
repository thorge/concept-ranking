request = require('request')
sw = require('stopword')
Crawl = do ->
  endpoint = 'https://query.wikidata.org/sparql?query='
  config = {
    limit: 1000,
    lang: 'en'
  }

  # Helper function that returns array with only unique items
  uniq = (a) ->
    prims = 
      'boolean': {}
      'number': {}
      'string': {}
    objs = []
    a.filter (item) ->
      type = typeof item
      if type of prims
        if prims[type].hasOwnProperty(item) then false else (prims[type][item] = true)
      else
        if objs.indexOf(item) >= 0 then false else objs.push(item)
  
  # Retrieve data from Wikidata
  # Makes use of Mediawiki API Service for full text search.
  retrieve = (c, cb) ->
    Object.assign config, c
    unless config.name and config.properties and config.lang
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
    config.properties.forEach (property, key) ->
      if property.concat is true or property.stopword is true
        select += '(GROUP_CONCAT(DISTINCT ?' + property.label + '; SEPARATOR=", ") AS ?' + property.label + ') '
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
      # remove stopwords from properties
      config.properties.forEach (property, key) ->
        if property.stopword is true
          body.results.bindings.forEach (item) ->
            if item[property.label].value
              s = sw.removeStopwords item[property.label].value.match(/\b(\S+)\b/g), sw[config.lang]
              if property.unique is true
                s = uniq(s)
              if property.delimiter
                s = s.join property.delimiter
              item[property.label].value = s
      # remove stopwords from description
      if config.description.stopword is true
        body.results.bindings.forEach (item) ->
          if item.description.value
            s = sw.removeStopwords item.description.value.match(/\b(\S+)\b/g), sw[config.lang]
            if config.description.unique is true
              s = uniq(s)
            if config.description.delimiter
              s = s.join config.description.delimiter
            item.description.value = s
      # callback
      cb { query: query, body: body }
      return
    return
  
  # run from commandline
  args = process.argv.slice(2);
  if args[0]
    try
      retrieve JSON.parse(args[0]), (res) ->
        console.log JSON.stringify(res.body.results.bindings)
    catch err
      console.log ["error": "Wrong input format."]
      
  # return public functions
  { retrieve: retrieve }
module.exports = Crawl
