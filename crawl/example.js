const crawl = require('./crawl');

crawl.retrieve('Albert Einstein', function(body) {
  console.log(body.results.bindings);
});
