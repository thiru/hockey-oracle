UI.ready(function() {
  var loginSuccessEl = UI.get('login-successful');
  if (loginSuccessEl)
    Page.goBackTo(loginSuccessEl.dataset.goBackToUrl);
  UI.get('email').focus();
});

var Page = {
  goBackTo: function(url) {
    url = url || '/';
    setTimeout(
      function() { window.location.href = url },
      1000);
  }
}
