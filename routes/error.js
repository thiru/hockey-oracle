/*
 * GET error page.
 */
module.exports = function(req, res)
{
  res.render('error', {pretty: true});
  res.statusCode = 500;
};
/*
 * TODO: Proper error handling not working.
module.exports = function(err, req, res, next)
{
  console.error(err.stack);
  var msg = 'An unexpected error occurred.';
  if (err.type == 'user')
    msg = err.message;
  res.render('error', {pretty: true, msg: msg, status: res.statusCode});
};
*/
