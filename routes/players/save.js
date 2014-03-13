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
      var activePlayerCount = 0;
      for (var i = 0; i < playerList.length; i++)
      {
        if (activePlayers.indexOf(playerList[i].name) >= 0)
        {
          activePlayerCount++;
          playerList[i].isActive = true;
        }
        else
        {
          playerList[i].isActive = false;
        }
      }

      if (activePlayerCount < 3)
      {
        res.redirect('/error');
        return;
      }
      players.save(playerList);
      res.redirect('/players/randomize');
    }
  );
};
