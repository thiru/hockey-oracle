/*
 * GET home page.
 */
exports.players = function(req, res) {
  res.render('players', {pretty: true});
};

/*
 * GET teams page.
 */
exports.teams = function(req, res) {
  res.render('teams', {pretty: true});
};

/*
 * GET about page.
 */
exports.about = function(req, res) {
  res.render('about', {pretty: true});
};
