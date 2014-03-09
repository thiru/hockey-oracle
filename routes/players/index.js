var players = require('../../lib/players');

/*
 * GET player list page.
 */
module.exports = function(req, res)
{
  players.getAll
  (
    function(playerList)
    {
      res.render('players/index', {pretty: true, players: playerList});
    }
  );
};
