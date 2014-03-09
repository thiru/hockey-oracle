/*
 * GET team randomization page.
 */
module.exports = function(req, res)
{
  res.render('teams/randomize', {pretty: true});
};
