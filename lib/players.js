var fs = require('fs');
var path = require('path');
var utils = require('../lib/utils');

var filePath = './data/players.json';

/*
 * Get all players (from disk).
 */
exports.getAll = function(cb)
{
  console.log(filePath);
  fs.exists
  (
    filePath,
    function(fileExists)
    {
      if (!fileExists) cb([]);
      else
      {
        fs.readFile
        (
          filePath,
          'utf8',
          function(err, data)
          {
            if (err) throw err;
            var playerList = JSON.parse(data.toString() || '[]');
            cb(playerList);
          }
        );
      }
    }
  );
}

/*
 * Get an object containing two random teams named 'black' and 'white'.
 */
exports.getRandomTeams = function(cb)
{
  exports.getAll
  (
    function(playerList)
    {
      var teams = {};
      teams.black = [];
      teams.white = [];
      if (!playerList || playerList.length == 0)
        cb(teams);
      else
      {
        utils.shuffleArray(playerList);
        var halfWayPoint = Math.floor(playerList.length / 2);
        teams.black = playerList.slice(0, halfWayPoint);
        teams.white = playerList.slice(halfWayPoint);
        cb(teams);
      }
    }
  );
}

/*
 * Save players (to disk).
 */
exports.save = function(playerList)
{
  playerList = playerList || [];
  fs.writeFile
  (
    filePath,
    JSON.stringify(playerList),
    'utf8',
    function(err)
    {
      if (err) throw err;
      console.log('Updated players to ' + playerList);
    }
  );
}
