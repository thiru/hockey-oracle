/*
 * GET team list page.
 */
module.exports = function(req, res)
{
  res.render('teams/index', {pretty: true});
};
