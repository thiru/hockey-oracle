// TODO: Replace alerts with inline message wherever feasible

/*
 * Core domain.
 */

var page= {};

// Global
$(document).ready(function() {
    page.userId = parseInt($("html").attr("data-user"));
    page.leagueName = $("html").attr("data-league");
    page.gameId = parseInt($("#game-info").attr("data-game"));

    page.showDialog = function(jqId) {
        $("#overlay").show();
        $(jqId).show();
    };
    page.closeDialog = function(jqId) {
        $("#overlay").hide();
        $(jqId).hide();
    };

    page.showLogin = function() {
        page.showDialog("#login-dialog");
        $("#login-user-name").focus().select();
    };
    page.closeLogin = function() {
        page.closeDialog("#login-dialog");
    };
    page.login = function() {
        $("#login-result").attr("class", "").html("");

        var result = null;
        var userName = get("login-user-name").value;
        var pwd = get("login-pwd").value;

        if (isBlank(userName))
            result = new Result("error", "No user name provided.");
        else if (isBlank(pwd))
            result = new Result("error", "No password provided.");

        if (result && result.failed()) {
            showResult($("#login-result"), result);
            return;
        }

        $("#login-result")
            .attr("class", "")
            .html("<i class='fa fa-spinner fa-pulse'></i> Logging in...");
        $("#login-btn").prop("disabled", true);
        $("#forgot-pwd").addClass("disabled");

        $.post("/api/login", { name: userName, pwd: pwd })
            .done(function (result) {
                if (!result)
                    result = new Result(-2, "No response from server.");
                else
                    result = _.create(Result.prototype, result);

                if (result.failed())
                    showResult($("#login-result"), result);
                else {
                    showResult($("#login-result"), result);
                    setTimeout(function() {
                        page.closeDialog("#login-dialog");
                        if (_.endsWith(window.location.pathname.toLowerCase(),
                                       "logout"))
                            window.location = "/";
                        else
                            window.location.reload(false);
                    }, 500);
                }

                $("#login-btn").prop("disabled", false);
                $("#forgot-pwd").removeClass("disabled");
            })
            .fail(function (data) {
                var result = new Result(-2,
                                        "Unexpected error. " + data.statusText +
                                        " (" + data.status + ").");
                showResult($("#login-result"), result);
                $("#login-btn").prop("disabled", false);
                $("#forgot-pwd").removeClass("disabled");
            });
    };
    page.forgotPwd = function() {
        $("#login-result").attr("class", "").html("");

        var result = null;
        var userName = get("login-user-name").value;

        if (isBlank(userName))
            result = new Result("error", "No user name provided.");

        if (result && result.failed()) {
            showResult($("#login-result"), result);
            return;
        }

        $("#login-result")
            .attr("class", "")
            .html("<i class='fa fa-spinner fa-pulse'></i> " +
                  "Requesting password reset...");

        $("#login-btn").prop("disabled", true);
        $("#forgot-pwd").addClass("disabled");

        $.post("/api/forgot-password", { name: userName })
            .done(function (result) {
                if (!result)
                    result = new Result(-2, "No response from server.");
                else
                    result = _.create(Result.prototype, result);

                showResult($("#login-result"), result);
                $("#login-btn").prop("disabled", false);
                $("#forgot-pwd").removeClass("disabled");
            })
            .fail(function (data) {
                var result = new Result(-2,
                                        "Unexpected error. " + data.statusText +
                                        " (" + data.status + ").");
                showResult($("#login-result"), result);
                $("#login-btn").prop("disabled", false);
                $("#forgot-pwd").removeClass("disabled");
            });
    };
    page.resetPwd = function() {
        $("#save-result").attr("class", "").html("");

        var player = {};
        player.id = get("save-btn").dataset.playerId;
        player.resetToken = get("save-btn").dataset.resetToken;

        player.pwd = get("pwd-new").value;
        if (isBlank(player.pwd)) {
            showResult($("#save-result"),
                       new Result("error", "Password can't be blank."));
            return;
        }

        var newPwdRepeat = get("pwd-new-repeat").value;
        if (player.pwd !== newPwdRepeat) {
            showResult($("#save-result"),
                       new Result("error", "Passwords don't match."));
            return;
        }

        $("#save-btn").prop("disabled", true);
        $("#save-result")
            .html("<i class='fa fa-spinner fa-pulse'></i> Updating password...");

        $.post("/api/reset-password", player)
            .done(function (result) {
                if (!result)
                    result = new Result(-2, "No response from server.");
                else
                    result = _.create(Result.prototype, result);

                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            })
            .fail(function (data) {
                var result = new Result(-2,
                                        "Unexpected error. " + data.statusText +
                                        " (" + data.status + ").");
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            });
    }
});
// Global ------------------------------------------------------------------- END

// User Detail Page
$(document).ready(function() {
    if (!$("main[id='user-detail-page']").length)
        return;

    page.saveSucceeded = false;

    page.inputChanged = function() {
        if (page.saveSucceeded)
            return;

        if (dataChanged("player-name-edit")
            || dataChanged("player-email-edit")
            || dataChanged("player-immediate-notify-edit")
            || dataChanged("player-pos-edit")
            || dataChanged("pwd-new-repeat"))
            $("#save-btn").show();
        else
            $("#save-btn").hide();
    };

    page.changePwd = function() {
        $("#pwd-group").show();
        $("#change-pwd-btn").hide();
    };

    page.saveUser = function() {
        var player = {};

        // Get player name
        player.name = $("#player-name-edit").val().trim();
        if (isBlank(player.name)) {
            alert("Player name can't be blank.");
            return;
        }

        // Get player email
        player.email = $("#player-email-edit").val().trim();

        // Get immediate email notifications option
        player.notifyImmediately = get("player-immediate-notify-edit").checked;

        // Get player position
        player.position = $("#player-pos-edit :selected").val();

        var newPwd = get("pwd-new").value;
        if (!isBlank(newPwd)) {
            var newPwdRepeat = get("pwd-new-repeat").value;
            if (newPwd !== newPwdRepeat) {
                alert("New passwords don't match.");
                return;
            }

            var currPwd = get("pwd-curr").value;
            if (isBlank(currPwd)) {
                alert("Original password not provided.");
                return;
            }

            player.currentPwd = currPwd;
            player.newPwd = newPwd;
        }

        // Saving...
        $("#save-btn").prop("disabled", true);
        $("#save-result")
            .attr("class", "")
            .html("<i class='fa fa-spinner fa-pulse'></i> Saving...");

        $.post("/api/users/me", player)
            .done(function (result) {
                if (!result)
                    result = new Result(-2, "No response from server.");
                else
                    result = _.create(Result.prototype, result);
                page.saveSucceeded = result.succeeded();
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            })
            .fail(function (data) {
                var result = new Result(-2,
                                        "Unexpected error. " + data.statusText +
                                        " (" + data.status + ").");
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            });
    };

    $("#player-name-edit").on("input", page.inputChanged);
    $("#player-email-edit").on("input", page.inputChanged);
    $("#player-immediate-notify-edit").change(page.inputChanged);
    $("#player-pos-edit").change(page.inputChanged);
    $("#pwd-new-repeat").on("input", page.inputChanged);
});
// User Detail Page --------------------------------------------------------- END

// Game Detail Page
function confirmTypeChanged(ele) {
    var selectedVal = $(ele).val() || "";
    if ("PLAYING" == selectedVal.toUpperCase()) {
        $("#reason-input-group").hide();
        $("#reason-input").val("");
    }
    else {
        $("#reason-input-group").show();
    }

    saveConfirmInfo();
}

function reasonTextChanged(ele) {
    var reasonLength = ($(ele).val() || "").length;
    var maxLength = $(ele).attr("maxlength");
    $("#reason-input-info").text((maxLength - reasonLength) + " chars left");
}

function saveConfirmInfo() {
    var selectedConfirmType = $("#game-confirm-opts").val();
    var confirmType = $("#game-confirm-opts").val();
    var confirmTypeTxt = $("#game-confirm-opts option:selected").text().trim();
    var reason = $("#reason-input").val();

    var originalInfo = $("#reason-input-info").html();
    $("#confirm-type-status")
        .html("<i class='fa fa-spinner fa-pulse'></i> Updating...");
    $("#reason-input-info")
        .html("<i class='fa fa-spinner fa-pulse'></i> Updating...");

    var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + page.gameId;
    $.post(url, { confirmType: confirmType, reason: reason})
        .done(function (result) {
            if (!result) {
                result = new Result(-2, "No response from server.");
                showResult($("#reason-input-info"), result, originalInfo);
            }
            else {
                result = _.create(Result.prototype, result);
                showIconResult($("#confirm-type-status"), result);
                showResult($("#reason-input-info"), result, originalInfo);
                $("#reason-input").val(result.data);
                updatePlayerConfirmOnPage(confirmTypeTxt, result.data);
            }
        })
        .fail(function(data) {
            var result = new Result(-2,
                                    "Unexpected error. " + data.statusText +
                                    " (" + data.status + ").");
            showResult($("#reason-input-info"), result);
        });
}

function updatePlayerConfirmOnPage(confirmType, reason) {
    // Find player row
    var playerEle = $(".player-item").filter(function() {
        var p = $(this).data();
        return p && (p.id == page.userId);
    });

    // Update player confirm info
    if (playerEle && playerEle.length) {
        playerEle.data().confirmType = "(" + confirmType + ")";
        playerEle.data().responseTime = "just updated";
        playerEle.data().reason = reason;

        playerEle.find(".confirm-type").html("(" + confirmType + ")");
        playerEle.find(".confirm-time").html("just updated");
        playerEle.find(".confirm-reason").html(escapeHtml(reason));
    }

    // Move to confirmed/unconfirmed section if necessary
    var parentSection = null;
    if (confirmType.toUpperCase() == "PLAYING") {
        parentSection = playerEle.parents("#unconfirmed-players-section");
        if (parentSection && parentSection.length)
            confirmPlayer(playerEle);
    }
    else {
        parentSection = playerEle.parents("#confirmed-players-section");
        if (parentSection && parentSection.length)
            unconfirmPlayer(playerEle);
    }
}

function makeTeams() {
  var activePlayersRes = getActivePlayers();
  if (activePlayersRes.failed()) {
    alert(activePlayersRes.message);
    return;
  }

  var teamsRes = generateTeams(2, activePlayersRes.data);
  if (teamsRes.failed()) {
    alert(teamsRes.message);
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
    var currPlayerEle = $(ele).hasClass("player-item")
        ? ele
        : $(ele).parents(".player-item");
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

    $("#confirmed-heading").addClass("blue-heading")
                           .removeClass("grey-heading");
    $("#confirmed-heading .true").removeClass("hidden");
    $("#confirmed-heading .false").addClass("hidden");
    $("#confirmed-players").show();
    $("#make-teams").show();

    var unconfirmedPlayers = $("#unconfirmed-players .player-item");
    if (unconfirmedPlayers.length <= 0)
        $("#unconfirmed-players-section").hide();
}

function unconfirmPlayer(ele) {
    var currPlayerEle = $(ele).hasClass("player-item")
        ? ele
        : $(ele).parents(".player-item");
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
        $("#confirmed-heading").removeClass("blue-heading")
                               .addClass("grey-heading");
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
  page.showDialog("#edit-dialog");
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

  page.showDialog("#edit-dialog");
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

  page.closeDialog("#edit-dialog");
  $("#confirmed-players").show();
  $("#confirmed-heading").removeClass("grey-heading")
                         .addClass("blue-heading");
  $("#confirmed-heading .true").removeClass("hidden");
  $("#confirmed-heading .false").addClass("hidden");
  $("#make-teams").show();
}
// Game Detail Page --------------------------------------------------------- END
