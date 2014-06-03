var fs = require('fs');
var path = require('path');
var _ = require('lodash');
var router = require('express').Router();
var players = require('./players.js');

var homePage = getHomePage();

setViewRoutes();
setApiRoutes();
setErrorRoutes();

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
  _.forEach(
    viewRoutes,
    function(x)
    {
      router.get(x, function(req, res) {res.send(homePage);});
    });
}

/// Define JSON API routes.
function setApiRoutes()
{
  router.get('/api/players', players.getAll);
  router.get('/api/players/randomize', players.randomize);
  router.post('/api/players/active', players.updateActive);
}

/// Define error routes.
function setErrorRoutes()
{
  //router.get('/api/*', function(req, res) {res.send({message: notFoundMsg})});
  router.get(
    '*',
    function(req, res, next)
    {
      var err = new Error('*thiru test 404*');
      err.status = 404;
      next(err);
    });
}
