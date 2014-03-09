var players = require('../../lib/players');

/*
 * GET player randomization page.
 */
module.exports = function(req, res)
{
  players.getRandomTeams
  (
    // TODO: Pass two objects, black and white
    function(teams)
    {
      res.render('players/randomize', {pretty: true, teams: teams});
    }
  );
};
