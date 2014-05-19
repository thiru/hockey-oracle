var colors = require('colors');
var express = require('express');
var morgan = require('morgan');
var bodyParser = require('body-parser');
var errorHandler = require('errorhandler');
var methodOverride = require('method-override');
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
  app.set('isDev', app.get('env') == 'development');

  app.use(morgan(app.get('isDev') ? 'dev' : ''));
  app.use(bodyParser());
  app.use(methodOverride());
  app.use(express.static(path.join(__dirname, 'public')));

  if (app.get('isDev'))
  {
    console.log('Running in *DEVELOPMENT* mode.'.blue);
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

  // Error handling
  app.use(errorHandler());

  console.log('[END] '.green + 'Express configuration');
  return app;
}

function onServerStarted()
{
  console.log('Web server started and listening on port ' + app.get('port') + '.');
  console.log('[END] '.green + 'Hockey Oracle start-up');
}
