var result = require('../utils/result.js');
var homePage = require('./home-page-loader.js')();

var notFoundMsg = 'The requested resource could not be found.';
var serverErrMsg = 'Sorry, something went wrong in the backend.';

module.exports = function(err, req, res, next)
{
  var status = err.status ? err.status : 500;
  var msg = err.status == 404 ? notFoundMsg : serverErrMsg;

  if (req.accepts('html'))
    res.send(homePage, status);
  else if (req.accepts('json'))
    res.json(status, result.New('failure', msg));
  else
    res.type('txt').send(serverErrMsg, status);
}
