/*
 * Generic utils.
 */

function Result(status, msg, data)
{
  this.status = status || 0;
  this.msg = msg || "";
  this.data = data || {};
}
Result.prototype.succeeded = function()
{
  return this.status >= 0;
}
Result.prototype.failed = function()
{
  return !this.succeeded();
}
Result.prototype.statusName = function()
{
  if (this.status <= -3)
    return "fatal";
  if (this.status == -2)
    return "error";
  if (this.status == -1)
    return "warning";
  if (this.status == 0)
    return "debug";
  if (this.status == 1)
    return "info";
  if (this.status >= 2)
    return "success";
}

Result.prototype.success = new Result(1, '');
Result.prototype.failure = new Result(-2, 'An unspecified error occurred');
