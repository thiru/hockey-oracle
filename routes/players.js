var players = require('../lib/players');

/*
 * GET players page.
 */
module.exports = function(req, res)
{
  players.getAll
  (
    function(playerList)
    {
      res.render('players', {pretty: true, players: playerList});
    }
  );
};
