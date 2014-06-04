module.exports = angular.module('horacle.players', ['ngResource'])
  .controller('playerListCtrlr',
    function($scope, $resource, $http, $location)
    {
      $scope.page.title = 'Players';
      $scope.playerResource = $resource('/api/players/:id', {id: '@id'});
      $scope.data = {};
      $scope.data.players = $scope.playerResource.query();
      $scope.data.players.$promise.then(function() {$scope.data.finishedLoading = true;});

      $scope.onNameClick = function(player)
      {
        player.isActive = !player.isActive;
      };

      $scope.saveAndRandomize = function()
      {
        $scope.valError = '';
        $scope.saveError = '';

        var activePlayerIds = _.chain($scope.data.players)
          .filter(function(x) {return x.isActive;})
          .pluck('id')
          .value();

        $scope.isSaving = true;
        $http.post('/api/players/active', {activePlayerIds: activePlayerIds})
          .success(
            function(data, status)
            {
              $scope.isSaving = false;
              if (!data || !data.message || data.message != 'success')
              {
                // TODO: Log error details in data.
                $scope.saveError = true;
              }
              else
                $location.path('/players/randomize');
            })
          .error(
            function(data, status)
            {
              $scope.isSaving = false;
              // TODO: Log error details in data.
              $scope.saveError = true;
            });
      };
    })
  .controller('playerRandomCtrlr',
    function($scope, $resource, $route)
    {
      $scope.page.title = 'Player randomize';
      $scope.randomizeResource = $resource('/api/players/randomize');
      $scope.data = {};
      $scope.data.teams = $scope.randomizeResource.query();
      $scope.data.teams.$promise.then(
        function()
        {
          $scope.data.finishedLoading = true;
          // TODO: Remove hard-coded team names.
          $scope.data.teams[0].name = 'Cripplers';
          $scope.data.teams[1].name = 'Panthers';
        });

      $scope.reload = function()
      {
        $route.reload();
      };
    });

