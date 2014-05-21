var fs = require('fs');
var path = require('path');
var _ = require('lodash');
var router = require('express').Router();
var players = require('./players.js');

var homePage = getHomePage();

setViewRoutes();
setApiRoutes();

module.exports = router;

/// Read home page HTML from disk.
function getHomePage()
{
  var filePath = path.join(path.dirname(require.main.filename),
                           'public/index.html');
  return fs.readFileSync(filePath, {encoding: 'utf8'});
}

/// Define view routes, which all redirect to the home page.
function setViewRoutes()
{
  var viewRoutes =
  [
    '/',
    '/players',
    '/players/randomize',
    '/teams'
  ];
  _.forEach(viewRoutes, function(x) {router.get(x, sendToHomePage);});
}

/// Define JSON API routes
function setApiRoutes()
{
  router.get('/api/players', players.getAll);
  router.get('/api/players/randomize', players.randomize);
  router.post('/api/players/active', players.updateActive);
}

function sendToHomePage(req, res)
{
  res.send(homePage);
}
