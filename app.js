var colors = require('colors');
var express = require('express');
var morgan = require('morgan');
var bodyParser = require('body-parser');
var errorHandler = require('errorhandler');
var methodOverride = require('method-override');
var path = require('path');
var routes = require('./lib/router');

var defaultPort = 3000;
var port = process.env.PORT || defaultPort;

console.log('[START] '.yellow + 'Hockey Oracle start-up');

var app = initExpress();
app.listen(port, onServerStarted);

// Configure and initialise Express.
function initExpress()
{
  console.log('[START] '.yellow +  'Express configuration');

  var app = express();

  app.set('view options', {layout: false});
  if (app.get('env') == 'development')
  {
    console.log('Running in *DEVELOPMENT* mode.'.blue);
    app.set('isDev', true);
  }

  app.use(morgan(app.get('isDev') ? 'dev' : ''));
  app.use(bodyParser());
  app.use(methodOverride());
  app.use(express.static(path.join(__dirname, 'public')));
  app.use('/', routes);

  // Error handling
  app.use(errorHandler());

  console.log('[END] '.green + 'Express configuration');
  return app;
}

function onServerStarted()
{
  console.log('Web server started and listening on port ' + port + '.');
  console.log('[END] '.green + 'Hockey Oracle start-up');
}
