'use strict'

module.exports = (app) ->
  crawl = require('./controller.min')
  app.use (req, res, next) ->
    res.header 'Access-Control-Allow-Origin', '*'
    res.header 'Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept'
    next()
  app.route('/api/retrieve/:name').get(crawl.retrieve)
  
  return
