/*
 * Radomize the items in the given array in-place.
 * This uses the Fisher-Yates shuffle algorithm.
 * Found here: http://stackoverflow.com/a/12646864/24318.
 */
exports.shuffleArray = function(arr)
{
  for (var i = arr.length - 1; i > 0; i--)
  {
    var j = Math.floor(Math.random() * (i + 1));
    var tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }

  return arr;
}
