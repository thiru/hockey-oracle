var fs = require('fs');
var path = require('path');

var filePath = './data/players.json';

/*
 * Get all players.
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
