var colors = require('colors');
var express = require('express');
var morgan = require('morgan');
var bodyParser = require('body-parser');
var errorHandler = require('errorhandler');
var methodOverride = require('method-override');
var path = require('path');
var routes = require('./lib/router');
var customErrHandler = require('./lib/ui/error-handler.js');

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
    app.enable('isDev');
  }

  app.use(morgan(app.enabled('isDev') ? 'dev' : ''));
  app.use(bodyParser());
  app.use(methodOverride());
  app.use(express.static(path.join(__dirname, 'public')));
  app.use('/', routes);

  // Error handling
  if (app.enabled('isDev'))
    app.use(errorHandler());
  else
    app.use(customErrHandler);

  console.log('[END] '.green + 'Express configuration');
  return app;
}

function onServerStarted()
{
  console.log('Web server started and listening on port ' + port + '.');
  console.log('[END] '.green + 'Hockey Oracle start-up');
}
