var players = require('../../lib/players');

/*
 * GET player randomization page.
 */
module.exports = function(req, res)
{
  players.getTwoRandomTeams
  (
    function(teams)
    {
      res.render('players/randomize', {pretty: true, teams: teams});
    }
  );
};
