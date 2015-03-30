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
  var activePlayers = getActivePlayers();
  if (!activePlayers.length)
    return;

  var teams = generateTeams(activePlayers);
  if (!teams.length)
    return;

  $("#player-list").hide();
  populateTeams(teams);
  $("#random-teams").show();
  $("#pick-players").show();

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

    if (!players.length)
      alert('No players selected');

    return players;
  }

  function generateTeams(activePlayers) {
    var teams = [];

    var goalies = _.filter(activePlayers, function(x) {return x.position == 'G'});
    goalies = _.shuffle(goalies);

    teams.push({goalie: goalies[0] || {name: "None", position: "G"}, players: []});
    teams.push({goalie: goalies[1] || {name: "None", position: "G"}, players: []});

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

      playersUL.append(playerHtml(teams[teamIdx].goalie));

      for (var i = 0; i < teams[teamIdx].players.length; i++) {
        playersUL.append(playerHtml(teams[teamIdx].players[i]));
      }

      teamIdx++;
    });

    function playerHtml(player) {
      return "<tr class='player-item'><td>" + player.name + "</td>" +
             "<td>" + player.position + "</td></tr>";
    }
  }
}

function pickPlayers() {
  $("#random-teams").hide();
  $("#pick-players").hide();
  $("#player-list").show();
}
