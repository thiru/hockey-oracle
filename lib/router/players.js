var playerMgr = require('../players.js');

/*
 * Get all players.
 */
exports.getAll = function(req, res, next)
{
  playerMgr.getAll(
    function(err, playerList)
    {
      if (err) return next(err);
      res.json(playerList);
    });
}

/*
 * Generate two teams (array of two) by randomizing all active players.
 */
exports.randomize = function(req, res, next)
{
  playerMgr.randomize(
    function(err, teams)
    {
      if (err) return next(err);
      res.json(teams);
    });
}

/*
 * Update the active player list.
 */
exports.updateActive = function(req, res, next)
{
  console.log('Active player ids: ' + req.body.activePlayerIds);
  playerMgr.updateActivePlayers(
    req.body.activePlayerIds,
    function(err)
    {
      // TODO: Log error
      if (err) return next(err);
      res.json({message: 'success'});
    });
}
