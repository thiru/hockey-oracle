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
  var activePlayersRes = getActivePlayers();
  if (activePlayersRes.failed()) {
    alert(activePlayersRes.msg);
    return;
  }

  var teamsRes = generateTeams(2, activePlayersRes.data);
  if (teamsRes.failed()) {
    alert(teamsRes.msg);
    return;
  }
  $("#player-list").hide();
  populateTeams(teamsRes.data);
  $("#random-teams").show();
  $("#pick-players").show();

  function getActivePlayers() {
    var players = [];

    $("#player-list .player-item.selected")
      .each(function() {
        var player = {};
        // TODO: player.id
        player.name = $(this).find(".player-name").text().trim();
        player.position = $(this).find(".player-position").text().trim();
        players.push(player);
    });

    if (!players.length)
      return new Result(-1, "No players selected");

    return new Result(2, "", players);
  }

  /// Generate the specified number of team, given the specified "active"
  /// players.
  function generateTeams(numTeams, activePlayers) {
    // Validate input params
    if (numTeams < 1)
      return new Result(-1, "The number of teams should be a positive " +
                            "integer but was " + numTeams);
    if (!activePlayers || !activePlayers.length || activePlayers.length < 1)
      return new Result(-1, "No active players specified");
    if (numTeams > activePlayers.length)
      return new Result(-1, "More teams are required (" + numTeams + ") " +
                            "than the number of active players (" +
                            activePlayers.length + ")");

    // Create empty teams
    var teams = [];
    for (var i = 0; i < numTeams; i++)
      teams.push({players: []});

    // Get active goalies
    var goalies = _.filter(activePlayers,
                           function(x) {return x.position == 'G'});

    // If there are less than `numTeams` goalies, create placeholder goalies
    while (goalies.length < numTeams)
      goalies.push({id: 0, name: "NO GOALIE", position: "G"});

    // Pick randmon goalies (note that not all goalies will be selected to
    // play as a goalie - i.e. if there are more goalies than teams)
    goalies = _.shuffle(goalies);
    for (var i = 0; i < numTeams; i++)
      teams[i].players.push(goalies[i]);

    // Get active players that won't play as a goalie and shuffle
    var players = _.difference(activePlayers, goalies.slice(0, numTeams));
    players = _.shuffle(players);

    // Split the players into teams, sorting by name
    var teamSize = Math.ceil(players.length / numTeams);
    var dividedPlayers = _.chunk(players, teamSize);

    for (var i = 0; i < numTeams; i++) {
      dividedPlayers[i] = _.sortBy(dividedPlayers[i],
                                   function(x) {return x.name});
      teams[i].players = teams[i].players.concat(dividedPlayers[i]);
    }

    return new Result(2, "", teams);
  }

  function populateTeams(teams) {
    var teamIdx = 0;

    $("#random-teams .team").each(function() {
      var playersUL = $(this).find(".team-players");
      playersUL.empty();

      for (var i = 0; i < teams[teamIdx].players.length; i++)
        playersUL.append(playerHtml(teams[teamIdx].players[i]));

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
