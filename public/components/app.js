angular.module('horacle',
               [
                 'ngRoute',
                 require('./players').name,
                 require('./teams').name
               ]
              )
  .config(require('./routes.js'))
  .controller('mainCtrlr', function($scope)
  {
    $scope.page = { title: 'Home' };
  });

