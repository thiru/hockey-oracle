var playerMgr = require('../players.js');

/*
 * Get all players.
 */
exports.getAll = function(req, res, next)
{
  playerMgr.getAll(function(playerList) {res.json(playerList);});
}

/*
 * Generate two teams (array of two) by randomizing all active players.
 */
exports.randomize = function(req, res, next)
{
  playerMgr.randomize(function(teams) {res.json(teams);});
}

/*
 * Update the active player list.
 */
exports.updateActive = function(req, res, next)
{
  console.log('Active player ids: ' + req.body.activePlayerIds);
  playerMgr.updateActivePlayers(
    req.body.activePlayerIds,
    function(success)
    {
      if (!success)
      {
        // TODO: Log error
        res.json({message: 'error'});
      }
      else
        res.json({message: 'success'});
    });
}
