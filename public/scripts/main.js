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
  $(ele).parents(".player-item").toggleClass("selected");
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
  $("#add-player").hide();
  populateTeams(teamsRes.data);
  $("#random-teams").show();
  $("#pick-players").show();

  function getActivePlayers() {
    var players = [];

    $("#player-list .player-item.selected")
      .each(function() {
        var player = {};
        player.id = parseInt($(this).attr("data-player-id"));
        player.name = $(this).find(".player-name").text().trim();
        player.position = $(this).find(".player-position").text().trim();
        players.push(player);
    });

    if (!players.length)
      return new Result(-1, "No players selected");

    return new Result(2, "", players);
  }

  /// Generate the specified number of teams, given the specified "active"
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
  $("#add-player").show();
}

function editClick(ev) {
  ev.stopPropagation();
}

function editPlayer(ele) {
  var playerRow = $(ele).parents("#player-list .player-item");
  var playerId = playerRow.attr("data-player-id");
  var currName = playerRow.find(".player-name").text().trim();
  var currPos = playerRow.find(".player-position").text().trim();
  var currActive = playerRow.hasClass("selected");

  $("#player-name-edit").val(currName);
  $("#player-pos-edit option").each(function() {
    if ($(this).val() == currPos)
      $(this).attr("selected", "selected");
    else
      $(this).removeAttr("selected");
  });
  $("#player-active-edit").prop("checked", currActive);

  $("#edit-dialog .save-btn").attr("data-player-id", playerId);

  $("#overlay").show();
  $("#edit-dialog").show();
}

function savePlayer() {
  // Determine player ID
  var playerId = parseInt($("#edit-dialog .save-btn").attr("data-player-id"));
  if (playerId < 0) {
    alert("Invalid player ID");
    return;
  }

  // Get player name
  var name = $("#player-name-edit").val().trim();
  if (!name || !name.length) {
    alert("Player name can't be blank");
    return;
  }

  // Get player position
  var position = $("#player-pos-edit :selected").text().trim();

  // Get player active state
  var isActive = $("#player-active-edit").is(":checked");

  // If we're adding a new player
  if (playerId == 0) {
    var newPlayerRow = $("#player-list .player-item").first().clone();

    var maxPlayerId = -1;
    $("#player-list .player-item").each(function() {
      var id = parseInt($(this).attr("data-player-id"));
      if (id > maxPlayerId)
        maxPlayerId = id;
    });
    newPlayerRow.attr("data-player-id", maxPlayerId + 1);
    newPlayerRow.addClass("selected");
    newPlayerRow.find("i.player-check")
                .addClass("fa-check-circle-o")
                .removeClass("fa-circle-o");
    newPlayerRow.find(".player-name").text(name);
    newPlayerRow.find(".player-position").text(position);
    newPlayerRow.appendTo("#player-list");
  }
  // Otherwise we're editing an existing player
  else {
    var playerRow =
      $("#player-list .player-item[data-player-id=" + playerId + "]");
    playerRow.find(".player-name").text(name);
    playerRow.find(".player-position").text(position);
    if (isActive) {
      playerRow.addClass("selected");
      playerRow.find("i.player-check")
               .addClass("fa-check-circle-o")
               .removeClass("fa-circle-o");
    }
    else {
      playerRow.removeClass("selected");
      playerRow.find("i.player-check")
               .addClass("fa-circle-o")
               .removeClass("fa-check-circle-o");
    }
  }

  $("#edit-dialog").hide();
  $("#overlay").hide();
}

function closeDialog() {
  $("#overlay").hide();
  $("#edit-dialog").hide();
}

function addPlayer() {
  $("#edit-dialog .save-btn").attr("data-player-id", 0);
  $("#player-name-edit").val("Extra 1");
  $("#player-pos-edit option").removeAttr("selected");
  $("#player-active-edit").prop("checked", true);
  $("#overlay").show();
  $("#edit-dialog").show();
}
