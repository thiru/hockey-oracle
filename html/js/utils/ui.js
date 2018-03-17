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
};
