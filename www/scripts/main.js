/*
 * Domain-specific functionality.
 */

$(document).ready(function() {
    // Do nothing
});

function makeTeams() {
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
  $("#confirmed-players-section").hide();
  $("#unconfirmed-players-section").hide();
  $("#add-player").hide();
  populateTeams(teamsRes.data);
  $("#random-teams").show();
  $("#pick-players").show();

  function getActivePlayers() {
    var players = [];

    $("#confirmed-players .player-item")
      .each(function() {
        var player = $(this).data();
        if (player && player.name)
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
        var item = $("#random-teams .template-player-item .player-item")
                    .first().clone();
        item.find(".player-name").text(player.name);
        item.find(".player-position").text(player.position);
        return item;
    }
  }
}

function positionChanged(ele) {
    var playerEle = $(ele).parents(".player-item");
    playerEle.data().position = $(ele).val();
}

function confirmPlayer(ele) {
    var currPlayerEle = $(ele).parents(".player-item");
    var player = currPlayerEle.data();
    var moveToEle =
        $("#confirmed-players-section .template-player-item .player-item")
            .first().clone();

    moveToEle.data(player);
    moveToEle.find(".player-name").text(player.name);
    moveToEle.find(".confirm-time").text(player.responseTime);
    moveToEle.find(".player-position").val(player.position);

    moveToEle.appendTo("#confirmed-players");
    currPlayerEle.remove();

    $("#confirmed-heading .true").removeClass("hidden");
    $("#confirmed-heading .false").addClass("hidden");
    $("#confirmed-players").show();
    $("#make-teams").show();

    var unconfirmedPlayers = $("#unconfirmed-players .player-item");
    if (unconfirmedPlayers.length <= 0)
        $("#unconfirmed-players-section").hide();
}

function unconfirmPlayer(ele) {
    var currPlayerEle = $(ele).parents(".player-item");
    var player = currPlayerEle.data();
    var moveToEle =
        $("#unconfirmed-players-section .template-player-item .player-item")
            .first().clone();

    moveToEle.data(player);
    moveToEle.find(".player-name").text(player.name);
    moveToEle.find(".confirm-type").text(player.confirmType);
    moveToEle.find(".confirm-reason").text(player.reason);
    moveToEle.find(".confirm-time").text(player.responseTime);

    moveToEle.appendTo("#unconfirmed-players");
    currPlayerEle.remove();

    var confirmedPlayers = $("#confirmed-players .player-item");
    if (confirmedPlayers.length <= 0) {
        $("#confirmed-heading .true").addClass("hidden");
        $("#confirmed-heading .false").removeClass("hidden");
        $("#confirmed-players").hide();
        $("#make-teams").hide();
    }
    else {
        $("#unconfirmed-players-section").show();
    }
}

function pickPlayers() {
    $("#random-teams").hide();
    $("#pick-players").hide();
    $("#confirmed-players-section").show();
    $("#unconfirmed-players-section").show();
    $("#add-player").show();
}

function editClick(ev) {
  ev.stopPropagation();
}

function addPlayer() {
  $("#edit-dialog .save-btn").attr("data-player-id", 0);
  $("#player-name-edit").val("Extra");
  $("#player-pos-edit option").removeAttr("selected");
  $("#player-active-edit").prop("checked", true); // TODO: obsolete
  $("#overlay").show();
  $("#edit-dialog").show();
  $("#player-name-edit").focus().select();
}

function editPlayer(ele) {
  var playerRow = $(ele).parents("#confirmed-players .player-item");
  var playerId = playerRow.attr("data-player-id");
  var currName = playerRow.find(".player-name").text().trim();
  var currPos = playerRow.find(".player-position :selected").val();
  var currActive = true; // TODO: obsolete

  $("#player-name-edit").val(currName);
  $("#player-pos-edit").val(currPos);
  $("#player-active-edit").prop("checked", currActive);

  $("#edit-dialog .save-btn").attr("data-player-id", playerId);

  $("#overlay").show();
  $("#edit-dialog").show();
  $("#player-name-edit").focus().select();
}

function savePlayer() {
  var player = {};

  // Determine player ID
  player.playerId = parseInt($("#edit-dialog .save-btn").attr("data-player-id"));
  if (player.playerId < 0) {
    alert("Invalid player id. Expected non-negative but was: " +
          player.playerId + " .");
    return;
  }

  // Get player name
  player.name = $("#player-name-edit").val().trim();
  if (!player.name || !player.name.length) {
    alert("Player name can't be blank.");
    return;
  }

  // Get player position
  player.position = $("#player-pos-edit :selected").val();

  // If we're adding a new player
  if (player.playerId == 0) {
      var newPlayerRow =
          $("#confirmed-players-section .template-player-item .player-item")
            .first().clone();

    var maxPlayerId = -1;
    $("#confirmed-players .player-item").each(function() {
      var id = parseInt($(this).attr("data-player-id"));
      if (id > maxPlayerId)
        maxPlayerId = id;
    });
    player.playerId = maxPlayerId + 1;

    if (maxPlayerId < 0)
      maxPlayerId = 1;

    newPlayerRow.data(player);
    newPlayerRow.find(".player-name").text(player.name);
    newPlayerRow.find(".confirm-time").text(player.responseTime);
    newPlayerRow.find(".player-position").val(player.position);

    newPlayerRow.appendTo("#confirmed-players");
  }
  // Otherwise we're editing an existing player
  else {
    var playerRow =
      $("#confirmed-players .player-item[data-player-id=" + playerId + "]");
    playerRow.find(".player-name").text(name);
    playerRow.find(".player-position").val(position);
  }

  $("#edit-dialog").hide();
  $("#overlay").hide();
  $("#confirmed-players").show();
  $("#confirmed-heading .true").removeClass("hidden");
  $("#confirmed-heading .false").addClass("hidden");
  $("#make-teams").show();
}

function closeDialog() {
  $("#overlay").hide();
  $("#edit-dialog").hide();
}
