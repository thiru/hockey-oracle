var colors = require('colors');
var express = require('express');
var http = require('http');
var path = require('path');
var fs = require('fs');
var routes = require('./lib/routes.js');

var defaultPort = 3000;
var homePage = fs.readFileSync(path.join(__dirname, 'public/index.html'),
                               {encoding: 'utf8'});

console.log('[START] '.yellow + 'Hockey Oracle start-up');

var app = initExpress();
http.createServer(app).listen(app.get('port'), onServerStarted);

// Configure and initialise Express.
function initExpress()
{
  console.log('[START] '.yellow +  'Express configuration');
  var app = express();
  app.set('port', process.env.PORT || defaultPort);
  app.set('view options', {layout: false});
  app.use(express.logger('dev'));
  app.use(express.favicon(path.join(__dirname, '/public/images/favicon.ico')));
  app.use(express.json());
  app.use(express.urlencoded());
  app.use(express.methodOverride());
  app.use(app.router);
  app.use(express.static(path.join(__dirname, 'public')));
  //app.use(routes.error); // TODO: not working for some reason.

  if ('development' == app.get('env'))
  {
    console.log('Running in DEV mode.');
    app.use(express.errorHandler());
  }

  // View Routes
  app.get('/', sendHome);
  app.get('/players', sendHome);
  app.get('/players/randomize', sendHome);
  app.get('/teams', sendHome);
  function sendHome(req, res)
  {
    res.send(homePage);
  }

  // API Routes
  app.get('/api/players', routes.players);
  app.get('/api/players/randomize', routes.randomize);
  app.post('/api/players/active', routes.updateActivePlayers);

  console.log('[END] '.green + 'Express configuration');
  return app;
}

function onServerStarted()
{
  console.log('Web server started and listening on port ' + app.get('port') + '.');
  console.log('[END] '.green + 'Hockey Oracle start-up');
}
