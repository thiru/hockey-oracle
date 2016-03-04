/*
 * Generic utils.
 */

// Language-Level
function Result(level, msg, data)
{
    this.level = level == undefined ? "info" : level;
    this.message = msg || "";
    this.data = data || {};
}
Result.prototype.succeeded = function()
{
    if (_.isNumber(this.level))
        return this.level >= 0;

    var levelStr = this.level.toLowerCase();
    return (levelStr == "success") || (levelStr == "info") || (levelStr == "debug");
}
Result.prototype.failed = function()
{
  return !this.succeeded();
}
Result.prototype.levelName = function()
{
    if (_.isString(this.level))
        return this.level;

    if (this.level >= 2)
        return "success";
    if (this.level == 1)
        return "info";
    if (this.level == 0)
        return "debug";
    if (this.level == -1)
        return "warning";
    if (this.level == -2)
        return "error";
    return "fatal";
}
Result.prototype.success = new Result(1, '');
Result.prototype.failure = new Result(-2, 'An unspecified error occurred');
// Language-Level ----------------------------------------------------------- END

// UI-Level
var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
};
function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}

function showResult(ele, result) {
    ele.html(result.message).addClass(result.levelName());
}

function showIconResult(ele, result) {
    ele.attr("title", result.message);
    ele.html("<i class='fa fa-check " + result.levelName() + "'></i>")
}
// UI-Level ----------------------------------------------------------------- END
