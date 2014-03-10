var players = require('../../lib/players');

/*
 * POST player save.
 */
module.exports = function(req, res)
{
  players.getAll
  (
    function(playerList)
    {
      var activePlayers = req.body.activePlayers;
      console.log(activePlayers);
      for (var i = 0; i < playerList.length; i++)
        playerList[i].isActive = activePlayers.indexOf(playerList[i].name) >= 0;

      players.save(playerList);
      res.redirect('/players/randomize');
    }
  );
};
