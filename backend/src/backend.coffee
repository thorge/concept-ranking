express = require('express')
bodyParser = require('body-parser')

app = express()

app.use bodyParser.urlencoded(extended: true)
app.use bodyParser.json()

# routing
routes = require('./routes.min')
routes app
app.listen 8081
console.log 'Backend is up and running (port: 8081)'
