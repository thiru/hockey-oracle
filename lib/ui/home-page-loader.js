var fs = require('fs');
var path = require('path');

/// Read home page HTML from disk.
module.exports = function()
{
  var filePath = path.join(path.dirname(require.main.filename),
                           'public/index.html');
  return fs.readFileSync(filePath, {encoding: 'utf8'});
}
