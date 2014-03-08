var players = require('../lib/players');

/*
 * GET home page.
 */
module.exports = function(req, res)
{
  players.getRandomTeams
  (
    function(teams)
    {
      res.render('index', {pretty: true, teams: teams});
    }
  );
};
