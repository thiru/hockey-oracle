module.exports = function($routeProvider, $locationProvider)
{
  $locationProvider.html5Mode(true);

  $routeProvider
    .when('/players', {templateUrl: '/components/players/list.part.html'})
    .when('/players/randomize', {templateUrl: '/components/players/randomize.part.html'})
    .when('/teams', {templateUrl: '/components/teams/list.part.html'})
    .otherwise({templateUrl: '/components/players/list.part.html'});
};
