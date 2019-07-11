'use strict'

module.exports = (app) ->
  controller = require('./controller.min')
  app.use (req, res, next) ->
    res.header 'Access-Control-Allow-Origin', '*'
    res.header 'Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept'
    next()
  app.route('/api/retrieve/:name').get(controller.retrieve)
  app.route('/api/ner').get(controller.ner)
  app.route('/api/parse').get(controller.parse)
  
  return
