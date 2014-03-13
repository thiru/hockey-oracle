var colors = require('colors');
var express = require('express');
var http = require('http');
var path = require('path');

var routes = {};
routes.index = require('./routes/index');
routes.error = require('./routes/error');
routes.players = {};
routes.players.index = require('./routes/players/index');
routes.players.save = require('./routes/players/save');
routes.players.randomize = require('./routes/players/randomize');
routes.teams = {};
routes.teams.index = require('./routes/teams/index');
routes.teams.randomize = require('./routes/teams/randomize');

var defaultPort = 80;

console.log('[START] '.yellow + 'Team Generator start-up');

var app = initExpress();
http.createServer(app).listen(app.get('port'), onServerStarted);

// Configure and initialise Express.
function initExpress()
{
  console.log('[START] '.yellow +  'Express configuration');
  var app = express();
  app.set('title', 'Team Generator');
  app.set('port', process.env.PORT || defaultPort);
  app.set('views', path.join(__dirname, 'views'));
  app.set('view engine', 'jade');
  app.use(express.logger('dev'));
  app.use(express.favicon(path.join(__dirname, '/public/images/favicon.ico')));
  app.use(express.json());
  app.use(express.urlencoded());
  app.use(express.methodOverride());
  app.use(app.router);
  app.use(express.static('public'));
  //app.use(routes.error); // TODO: not working for some reason.

  if ('development' == app.get('env')) {
    console.log('Running in DEV mode.');
    app.use(express.errorHandler());
  }

  // Routes
  app.get('/', routes.index);
  app.get('/error', routes.error);
  app.get('/players', routes.players.index);
  app.post('/players', routes.players.save);
  app.get('/players/randomize', routes.players.randomize);
  app.get('/teams', routes.teams.index);
  app.get('/teams/randomize', routes.teams.randomize);

  console.log('[END] '.green + 'Express configuration');
  return app;
}

function onServerStarted()
{
  console.log('Web server started and listening on port ' + app.get('port') + '.');
  console.log('[END] '.green + 'Team Generator start-up');
}
