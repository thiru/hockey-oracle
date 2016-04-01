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

function isBlank(text) {
    return !text || text.trim().length <= 0;
}
// Language-Level ----------------------------------------------------------- END

// UI-Level
function get(id) {
    return document.getElementById(id);
}

var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
};
function escapeHtml(text) {
    return String(text).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}

function dataChanged(eleId) {
    if (isBlank(eleId))
        return false;

    var ele = document.getElementById(eleId);
    if (!ele)
        return false;

    var currVal = (ele.value || "").trim();
    var origVal = ele.dataset.origVal || "";

    return currVal !== origVal;
}

function showResult(ele, result) {
    ele.html(result.message).attr("class", result.levelName());
}

function showIconResult(ele, result) {
    var icon = result.succeeded() ? "fa-check" : "fa-exclamation-circle";
    ele.attr("title", result.message);
    ele.html("<i class='fa fa-check " + icon + " " + result.levelName() +
             "'></i>")
}
// UI-Level ----------------------------------------------------------------- END
