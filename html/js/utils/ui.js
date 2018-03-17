var UI = {
  entityMap: {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
  },

  ready: function(fn) {
    if (document.readyState != 'loading')
      fn();
    else
      document.addEventListener('DOMContentLoaded', fn);
  },

  get: function(id) {
    return document.getElementById(id);
  },
  getQ: function(query) {
    return document.querySelector(query);
  },

  clear: function(el) {
    el.innerHTML = '';
  },

  hide: function(el) {
    el.classList.add('hidden');
  },
  unhide: function(el) {
    el.classList.remove('hidden');
  },
  isHidden: function(el) {
    return el.classList.contains('hidden');
  },
  toggleHidden: function(el) {
    el.classList.toggle('hidden');
  },

  escapeHtml: function(text) {
    return String(text).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
        });
  },

  onEnter: function(event, func) {
    if (event && func && event.keyCode === 13)
      func();
  },

  dataChanged: function(eleId) {
    if (isBlank(eleId))
        return false;

    var ele = get(eleId);
    if (!ele)
        return false;

    var origVal = ele.dataset.origVal || "";

    var currVal;
    if (ele.type.toLowerCase() === "checkbox")
        currVal = ele.checked + "";
    else
        currVal = (ele.value || "").trim();

    return currVal !== origVal;
  },

  updateOrigDataVal: function(eleId) {
    var ele = get(eleId);
    if (!ele)
        throw new Error("Element with id '" + eleId + "' not found.");

    if (ele.type.toLowerCase() === "checkbox")
        ele.dataset.origVal = ele.checked;
    else
        ele.dataset.origVal = ele.value;
  },

  showLoading: function(jqSel, msg) {
    $(jqSel).html("<i class='fa fa-spinner fa-pulse'></i> " +
                  ( msg || "Updating..."));
  },

  showResult: function(ele, result) {
    ele.show().html(result.message).attr("class", result.levelName());
  },

  showIconResult: function(ele, result) {
    var icon = result.succeeded() ? "fa-check" : "fa-exclamation-circle";
    ele.attr("title", result.message);
    ele.html("<i class='fa fa-check " + icon + " " + result.levelName() +
             "'></i>")
  }
};
