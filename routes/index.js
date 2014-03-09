var players = require('../lib/players');

/*
 * GET home page.
 */
module.exports = function(req, res)
{
  res.render('index', {pretty: true});
};
