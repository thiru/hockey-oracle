/*
 * This module encapsulates the Result type and related functionality.
 */

function Result(status, msg)
{
  this.status = status;
  this.msg = msg;
}
Result.prototype.succeeded = function()
{
  return this.status && this.status == 'success';
}
Result.prototype.failed = function()
{
  return !succeeded();
}

exports.New = function(status, msg)
{
  return new Result(status, msg);
};

exports.success = new Result('success', '');
exports.failure = new Result('failure', 'An unexpected error occurred.');
