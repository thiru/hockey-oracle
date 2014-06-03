var fs = require('fs');
var path = require('path');
var _ = require('lodash');

var filePath = './data/players.json';

/*
 * Get all players (from disk).
 */
exports.getAll = function(cb)
{
  console.log('Loading players from: ' + filePath);
  fs.exists
  (
    filePath,
    function(fileExists)
    {
      if (!fileExists) return cb(null, []);
      else
      {
        fs.readFile
        (
          filePath,
          'utf8',
          function(err, data)
          {
            if (err) return cb(err, []);
            var playerList = JSON.parse(data.toString() || '[]');
            return cb(null, playerList);
          }
        );
      }
    }
  );
}

/*
 * Generate two teams (array of two) by randomizing all active players.
 */
exports.randomize = function(cb)
{
  exports.getAll
  (
    function(err, playerList)
    {
      var teams = [];
      if (err) return cb(err, teams);
      if (!playerList || !playerList.length)
        return cb(new Error('No players in database.'), teams);

      // Players
      var activePlayers = [];
      activePlayers = _.filter(playerList, function(x) {return x.isActive && x.position != 'G';});
      activePlayers = _.shuffle(activePlayers);
      var halfWayPoint = Math.floor(activePlayers.length / 2);
      teams.push(_.sortBy(activePlayers.slice(0, halfWayPoint), ['firstName', 'lastName']));
      teams.push(_.sortBy(activePlayers.slice(halfWayPoint), ['firstName', 'lastName']));

      // Goalies
      var activeGoalies = [];
      activeGoalies = _.filter(playerList, function(x) {return x.isActive && x.position == 'G';});
      activeGoalies = _.shuffle(activeGoalies);
      for (var k = 0; k < teams.length; k++)
      {
        if (!activeGoalies[k]) break;
        teams[k].unshift(activeGoalies[k]);
      }

      return cb(null, teams);
    }
  );
}

/*
 * Update which players are active.
 */
exports.updateActivePlayers = function(playerIds, cb)
{
  console.log('About to update active players.');
  playerIds = playerIds || [];
  exports.getAll
  (
    function(err, playerList)
    {
      if (err) return cb(err);
      console.log('Successfully loaded players from DB.');
      for (var i = 0; i < playerList.length; i++)
        playerList[i].isActive = _.contains(playerIds, playerList[i].id);
      exports.save(playerList, function(err) {cb(err)});
    }
  );
}

/*
 * Save players (to disk).
 */
exports.save = function(playerList, cb)
{
  playerList = playerList || [];
  fs.writeFile
  (
    filePath,
    JSON.stringify(playerList),
    'utf8',
    function(err)
    {
      if (err) return cb(err);
      console.log('Successfully updated players to ' + JSON.stringify(playerList));
      cb();
    }
  );
}
