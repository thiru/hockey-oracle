$(document).ready(function() {
  checkActivePlayers();

  function checkActivePlayers() {
    $(".player-item").each(function() {
      if ($(this).is("[data-player-active]")) {
        $(this).addClass("selected");
        $(this).find(".player-check")
               .removeClass("fa-circle-o")
               .addClass("fa-check-circle-o");
      }
    });
  }
});

function togglePlayerActive(ele)
{
  $(ele).toggleClass("selected");
  $(ele).find("i.player-check")
        .toggleClass("fa-check-circle-o")
        .toggleClass("fa-circle-o");
}

function makeTeams()
{
  var selPlayers = getActivePlayers();

  $("#player-list").hide();

  var teams = generateTeams(selPlayers);
  populateTeams(teams);
  
  $("#random-teams").show();

  function getActivePlayers() {
    var players = [];

    $("#player-list .player-item")
      .each(function() {
        if ($(this).hasClass("selected")) {
          var player = {};
          player.name = $(this).find(".player-name").text().trim();
          player.position = $(this).find(".player-position").text().trim();
          players.push(player);
        }
    });

    return players;
  }

  function generateTeams(activePlayers) {
    var teams = [];

    var goalies = _.filter(activePlayers, function(x) {return x.position == 'G'});
    if (goalies.length == 1) {
      alert(goalies.length + " goalie was selected. Please choose two.");
      return teams;
    }
    else if (goalies.length != 2) {
      alert(goalies.length + " goalies were selected. Please choose two.");
      return teams;
    }

    goalies = _.shuffle(goalies);

    teams.push({goalie: goalies[0], players: []});
    teams.push({goalie: goalies[1], players: []});

    var players = _.filter(activePlayers, function(x) {return x.position != 'G'});

    players = _.shuffle(players);
    var halfWayPoint = Math.floor(players.length / 2);

    teams[0].players = _.sortBy(players.slice(0, halfWayPoint), function(x) {return x.name});
    teams[1].players = _.sortBy(players.slice(halfWayPoint), function(x) {return x.name});

    return teams;
  }

  function populateTeams(teams) {
    var teamIdx = 0;

    $("#random-teams .team").each(function() {
      var playersUL = $(this).find(".team-players");
      playersUL.empty();
      playersUL.append("<li><b>" + teams[teamIdx].goalie.name + "</b></li>");
      for (var i = 0; i < teams[teamIdx].players.length; i++) {
        playersUL.append("<li>" + teams[teamIdx].players[i].name + "</li>");
      }
      teamIdx++;
    });
  }
}
