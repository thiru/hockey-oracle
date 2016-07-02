// TODO: Replace alerts with inline message wherever feasible

/*
 * Core domain.
 */

// Global
var page= {};

// NOTE: This format needs to be in sync with back-end date/time format
page.dateAndTimeFmt = "ddd, MMM D YYYY @ h:mm a";

$(document).ready(function() {
    page.init();
    if (get("user-detail-page"))
        page.initUserDetailPage();
    if (get("league-detail-page"))
        page.initLeagueDetailPage();
    if (get("manage-league-page"))
        page.initManageLeaguePage();
    if (get("game-list-page"))
        page.initGameListPage();
    if (get("game-detail-page"))
        page.initGameDetailPage();
    if (get("player-list-page"))
        page.initGameDetailPage();
});

page.init = function() {
    page.userId = parseInt(get("root").dataset.user);
    page.leagueName = get("root").dataset.league;

    page.toggleMainMenu = function() {
        var hamMenu = $("#ham-menu-group");
        if (hamMenu.hasClass("hidden"))
            hamMenu.removeClass("hidden");
        else
            hamMenu.addClass("hidden");
    };

    page.showDialog = function(jqId) {
        $("#overlay").show();
        $(jqId).show();
    };
    page.closeDialog = function(jqId) {
        $("#overlay").hide();
        $(jqId).hide();
    };

    page.parseDateTime = function(dateEleId, timeEleId) {
        var rawDate = get(dateEleId).value;
        var rawTime = get(timeEleId).value;

        var parsedDate = moment(rawDate,
                                ["MMM D YYYY", "MMMM D YYYY", "YYYY MM DD"],
                                /*stricMode:*/ false);
        var parsedTime = moment(rawTime,
                                ["h:mm a", "HH:mm"],
                                /*stricMode:*/ false);

        // Validate date & time
        if (!parsedDate.isValid() && !parsedTime.isValid()) {
            showResult($("#save-result"),
                       Result.error("Date and time are invalid."));
            return;
        }
        else if (!parsedDate.isValid()) {
            showResult($("#save-result"),
                       Result.error("Date is invalid."));
            return;
        }
        else if (!parsedTime.isValid()) {
            showResult($("#save-result"), Result.error("Time is invalid."));
            return;
        }

        var dateAndTime = moment(parsedDate.format("YYYY-MM-DD") + "T" +
                                 parsedTime.format("HH:mm"));

        return dateAndTime;
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
            result = Result.error("No user name provided.");
        else if (isBlank(pwd))
            result = Result.error("No password provided.");

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
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);

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
                var result = Result.error("Unexpected error. " + data.statusText +
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
            result = Result.error("No user name provided.");

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
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);

                showResult($("#login-result"), result);
                $("#login-btn").prop("disabled", false);
                $("#forgot-pwd").removeClass("disabled");
            })
            .fail(function (data) {
                var result = Result.error("Unexpected error. " + data.statusText +
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
                       Result.error("Password can't be blank."));
            return;
        }

        var newPwdRepeat = get("pwd-new-repeat").value;
        if (player.pwd !== newPwdRepeat) {
            showResult($("#save-result"),
                       Result.error("Passwords don't match."));
            return;
        }

        $("#save-btn").prop("disabled", true);
        $("#save-result")
            .html("<i class='fa fa-spinner fa-pulse'></i> Updating password...");

        $.post("/api/reset-password", player)
            .done(function (result) {
                if (!result)
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);

                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            })
            .fail(function (data) {
                var result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            });
    }
};
// Global ------------------------------------------------------------------- END

// User Detail Page
page.initUserDetailPage = function() {
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
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                page.saveSucceeded = result.succeeded();
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            })
            .fail(function (data) {
                var result = Result.error("Unexpected error. " + data.statusText +
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
};
// User Detail Page --------------------------------------------------------- END

// League Detail Page
page.initLeagueDetailPage = function() {
    $(".upcoming-game-item").each(function() {
        var gameTimeStamp = $(this).find(".upcoming-game-time").text().trim();
        var gameTimeRel = moment(gameTimeStamp, page.dateAndTimeFmt).fromNow();
        $(this).find(".upcoming-game-rel-time").text("(" + gameTimeRel + ")");
    });
};
// League Detail Page ------------------------------------------------------- END

// Manage League Page
page.initManageLeaguePage = function() {
    page.save = function() {
        var league = {};
        league.sendAutomatedEmails = get("send-automated-emails").checked;

        // Saving...
        $("#save-btn").prop("disabled", true);
        $("#save-result")
            .attr("class", "")
            .html("<i class='fa fa-spinner fa-pulse'></i> Saving...");

        $.post("/" + page.leagueName + "/api/leagues/save", league)
            .done(function (result) {
                if (!result)
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                page.saveSucceeded = result.succeeded();
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            })
            .fail(function (data) {
                var result = data.responseJSON;
                if (!result)
                    result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
            });
    }
};
// Manage League Page ------------------------------------------------------- END

// Game List Page
page.initGameListPage = function() {
    page.openGameEditor = function() {
        page.showDialog("#new-game-dialog");
        get("date-picker").focus();
        get("date-picker").value = moment().add(1, "day").format("YYYY-MM-DD");
        get("time-picker").value = moment().format("h:00 a");
        page.updateRelTime();
    };

    page.updateRelTime = function() {
        var gameTime = page.parseDateTime("date-picker", "time-picker");
        var relTime = gameTime.fromNow();
        var exactTime = gameTime.format(page.dateAndTimeFmt);

        if (!gameTime.isValid())
            return;

        showResult($("#save-result"),
                   Result.success("Game set to take place " + relTime +
                                  "<br/>" + "(" + exactTime + ")"))
    };

    page.saveGame = function() {
        var dateAndTime = page.parseDateTime("date-picker", "time-picker");

        // Request server to save new game
        var newGame = {
            date: dateAndTime.format()
        };

        var savingMsg =
            "Saving game to take place " + dateAndTime.fromNow() + "...";
        $("#save-result")
            .html("<i class='fa fa-spinner fa-pulse'></i> " + savingMsg);
        $("#save-game-btn").prop("disabled", true);

        var url = "/" + page.leagueName.toLowerCase() + "/api/games/new";
        $.post(url, newGame)
            .done(function (result) {
                if (!result) {
                    result = Result.error("No response from server.");
                    showResult($("#save-result"), result, originalInfo);
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showResult($("#save-result"), result);
                    if (result.succeeded()) {
                        var item = $("#template-game-item").clone();
                        item.find(".game-date")
                            .text(result.data[1])
                            .attr("href",
                                  item.find(".game-date")
                                  .attr("href")
                                  .replace("<GAME-ID>", result.data[0]));
                        $("#new-games-list").append(item);
                        $("#new-games-section").show();
                    }
                }

                $("#save-game-btn").prop("disabled", false);
            })
            .fail(function(data) {
                var result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                showResult($("#save-result"), result);
                $("#save-game-btn").prop("disabled", false);
            });
    };

    page.closeGameEditor = function() {
        page.closeDialog("#new-game-dialog");
    };
};
// Game List Page ----------------------------------------------------------- END

// Game Detail Page
page.initGameDetailPage = function() {
    page.editGame = function() {
        $("#time-status-rw").show();
        $("#save-game-info-btn").show();
    };

    page.saveGame = function() {
        var gameTime = page.parseDateTime("game-date-rw", "game-time-rw");
        if (!gameTime) {
            showResult($("#save-res"), Result.error("Game date/time invalid."))
            return;
        }
        gameTime = gameTime.format();

        var gameProgress = $("#game-status-ddl :selected").val().trim();

        showLoading("#save-res");

        var gameId = parseInt(get("game-info").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + gameId;
        $.post(url, { gameTime: gameTime, gameProgress: gameProgress})
            .done(function (result) {
                if (!result) {
                    showResult($("#save-res"),
                               Result.error("No response from server."));
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showResult($("#save-res"), result);
                }
            })
            .fail(function(data) {
                var result = Result.error("Unexpected error. " +
                                          data.statusText +
                                          " (" + data.status + ").");
                showResult($("#save-res"), result);
            });
    };

    page.updateGameTime = function() {
        var dateTime = page.parseDateTime("game-date-rw", "game-time-rw");
        if (!dateTime)
            $("#game-time-ro").text("Invalid date/time");
        else
            $("#game-time-ro").text(dateTime.format(page.dateAndTimeFmt));
    };

    page.updateGameState = function() {
        var gameStatus = $("#game-status-ddl :selected").html().trim();
        $("#game-state-ro").text(" - " + gameStatus);
    };

    page.confirmTypeChanged = function(ele) {
        var selectedVal = $(ele).val().toUpperCase();
        if ("NO-RESPONSE" == selectedVal) {
            $("#confirm-type-status").hide();
            return;
        }
        else if ("PLAYING" == selectedVal) {
            $("#reason-input-group").hide();
            $("#reason-input").val("");
        }
        else {
            $("#reason-input-group").show();
        }

        page.saveConfirmInfo();
    };

    page.reasonTextChanged = function(ele) {
        var reasonLength = ($(ele).val() || "").length;
        var maxLength = $(ele).attr("maxlength");
        $("#reason-input-info").text((maxLength - reasonLength) + " chars left");
    };

    page.saveConfirmInfo = function() {
        var selectedConfirmType = $("#game-confirm-opts").val();
        var confirmType = $("#game-confirm-opts").val().toUpperCase();

        if ("NO-RESPONSE" == confirmType) {
            showResult($("#reason-input-info"),
                       Result.warning("Please select an option other than " +
                                      "'No response'."));
            return;
        }

        var confirmTypeTxt =
            $("#game-confirm-opts option:selected").text().trim();
        var reason = $("#reason-input").val();

        var originalInfo = $("#reason-input-info").html();
        $("#confirm-type-status")
            .html("<i class='fa fa-spinner fa-pulse'></i> Updating...");
        $("#confirm-type-status").show();
        $("#reason-input-info")
            .html("<i class='fa fa-spinner fa-pulse'></i> Updating...");

        var gameId = parseInt(get("game-info").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + gameId;
        $.post(url, { confirmType: confirmType, reason: reason})
            .done(function (result) {
                if (!result) {
                    result = Result.error("No response from server.");
                    showResult($("#reason-input-info"), result, originalInfo);
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showIconResult($("#confirm-type-status"), result);
                    showResult($("#reason-input-info"), result, originalInfo);
                    $("#reason-input").val(result.data);
                    page.updatePlayerConfirmOnPage(confirmTypeTxt, result.data);
                }
            })
            .fail(function(data) {
                var result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                showResult($("#reason-input-info"), result);
            });
    };

    page.updatePlayerConfirmOnPage = function(confirmType, reason) {
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
                page.confirmPlayer(playerEle);
        }
        else {
            parentSection = playerEle.parents("#confirmed-players-section");
            if (parentSection && parentSection.length)
                page.unconfirmPlayer(playerEle);
        }
    };

    page.makeTeams = function() {
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
                return Result.warning("No players selected");

            return Result.success("", players);
        }

        /// Generate the specified number of teams, given the specified "active"
        /// players.
        function generateTeams(numTeams, activePlayers) {
            // Validate input params
            if (numTeams < 1)
                return Result.warning("The number of teams should be a " +
                                      "positive integer but was " + numTeams);
            if (!activePlayers || !activePlayers.length ||
                activePlayers.length < 1)
                return Result.warning("No active players specified");
            if (numTeams > activePlayers.length)
                return Result.warning("More teams are required (" +
                                      numTeams + ") than the number of active " +
                                      "players (" + activePlayers.length + ")");

            // Create empty teams
            var teams = [];
            for (var i = 0; i < numTeams; i++)
                teams.push({players: []});

            // Get active goalies
            var goalies = _.filter(activePlayers,
                                   function(x) {return x.position == 'G'});

            // If there are less than `numTeams` goalies, create placeholder
            // goalie(s)
            while (goalies.length < numTeams)
                goalies.push({id: 0, name: "NO GOALIE", position: "G"});

            // Pick randmon goalies (note that not all goalies will be selected
            // to play as a goalie - i.e. if there are more goalies than teams)
            goalies = _.shuffle(goalies);
            for (var i = 0; i < numTeams; i++)
                teams[i].players.push(goalies[i]);

            // Get active players that won't play as a goalie and shuffle
            var players = _.difference(activePlayers,
                                       goalies.slice(0, numTeams));
            players = _.shuffle(players);

            // Split the players into teams, sorting by name
            var teamSize = Math.ceil(players.length / numTeams);
            var dividedPlayers = _.chunk(players, teamSize);

            for (var i = 0; i < numTeams; i++) {
                dividedPlayers[i] = _.sortBy(dividedPlayers[i],
                                             function(x) {return x.name});
                teams[i].players = teams[i].players.concat(dividedPlayers[i]);
            }

            return Result.success("", teams);
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
    };

    page.positionChanged = function(ele) {
        var playerEle = $(ele).parents(".player-item");
        playerEle.data().position = $(ele).val();
    };

    page.updateConfirmedSection = function() {
        var confirmedPlayers = $("#confirmed-players .player-item");
        if (confirmedPlayers.length) {
            $("#confirmed-heading-many").show();
            $("#confirmed-heading-zero").hide();
            $("#confirmed-players").show();
            $("#confirmed-count").text("(" + confirmedPlayers.length + ")");
        }
        else {
            $("#confirmed-heading-many").hide();
            $("#confirmed-heading-zero").show();
            $("#confirmed-players").hide();
        }
    };

    page.updateUnconfirmedSection = function() {
        var unconfirmedPlayers = $("#unconfirmed-players .player-item");
        if (unconfirmedPlayers.length) {
            $("#unconfirmed-players-section").show();
            $("#unconfirmed-count").text("(" + unconfirmedPlayers.length + ")");
        }
        else {
            $("#unconfirmed-players-section").hide();
            $("#unconfirmed-count").text("");
        }
    };

    page.moveToPlayerList = function(listEle, moveToEle) {
        var players = listEle.find(".player-item");
        if (!players.length) {
            moveToEle.appendTo(listEle);
        }
        else {
            var name = moveToEle.data("name").toLowerCase();
            var inserted = false;
            players.each(function (idx) {
                var cmp = name.localeCompare($(this).data("name"));
                if (cmp <= -1) {
                    $(this).before(moveToEle);
                    inserted = true;
                    return false;
                }
            });
            if (!inserted)
                moveToEle.appendTo(listEle);
        }
    }

    page.confirmPlayer = function(ele) {
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

        page.moveToPlayerList($("#confirmed-players"), moveToEle);
        currPlayerEle.remove();

        page.updateConfirmedSection();
        page.updateUnconfirmedSection();
    };

    page.unconfirmPlayer = function(ele) {
        var currPlayerEle = $(ele).hasClass("player-item")
            ? ele
            : $(ele).parents(".player-item");
        var player = currPlayerEle.data();
        var moveToEle =
            $("#unconfirmed-players-section .template-player-item .player-item")
            .first().clone();

        moveToEle.data(player);
        moveToEle.find(".player-name").text(player.name);
        moveToEle.find(".player-position").text(player.position);
        moveToEle.find(".confirm-type").text(player.confirmType);
        moveToEle.find(".confirm-reason").text(player.reason);
        moveToEle.find(".confirm-time").text(player.responseTime);

        page.moveToPlayerList($("#unconfirmed-players"), moveToEle);
        currPlayerEle.remove();

        page.updateConfirmedSection();
        page.updateUnconfirmedSection();
    };

    page.pickPlayers = function() {
        $("#random-teams").hide();
        $("#pick-players").hide();
        $("#confirmed-players-section").show();
        $("#unconfirmed-players-section").show();
        $("#add-player").show();
    };

    page.addPlayer = function() {
        $("#edit-dialog .save-btn").data().id = 0;
        $("#player-name-edit").val("Extra");
        $("#player-pos-edit option").removeAttr("selected");
        page.showDialog("#edit-dialog");
        $("#player-name-edit").focus().select();
    };

    page.editPlayer = function(ele, playerListJQSel) {
        var playerRow = $(ele).parents(playerListJQSel + " .player-item");
        var player = playerRow.data();

        $("#player-name-edit").val(player.name);
        $("#player-pos-edit").val(player.position);

        $("#edit-dialog .save-btn").data().id = player.id;

        page.showDialog("#edit-dialog");
        $("#player-name-edit").focus().select();
    };

    page.savePlayer = function(playerListJQSel, templateItemJQSel) {
        var player = {};

        // Defaulting to game detail page specifics
        if (isBlank(playerListJQSel))
            playerListJQSel = "#confirmed-players";
        if (isBlank(templateItemJQSel))
            templateItemJQSel = "#confirmed-players-section " +
                                ".template-player-item .player-item";

        // Determine player ID
        player.id = $("#edit-dialog .save-btn").data().id;
        if (player.id < 0) {
            alert("Invalid player id. Expected non-negative but was: " +
                  player.id + " .");
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
        if (player.id == 0) {
            var newPlayerRow = $(templateItemJQSel).first().clone();

            var maxPlayerId = -1;
            $(playerListJQSel + " .player-item").each(function() {
                var id = parseInt($(this).data().id);
                if (id > maxPlayerId)
                    maxPlayerId = id;
            });
            player.id = maxPlayerId + 1;

            if (maxPlayerId < 0)
                maxPlayerId = 1;

            newPlayerRow.data(player);
            newPlayerRow.find(".player-name").text(player.name);
            newPlayerRow.find(".confirm-time").text(player.responseTime);
            var positionEle = newPlayerRow.find(".player-position");
            if (positionEle.is("select"))
                positionEle.val(player.position);
            else
                positionEle.text(player.position);

            page.moveToPlayerList($(playerListJQSel), newPlayerRow);
        }
        // Otherwise we're editing an existing player
        else {
            var playerRow =
                $(playerListJQSel + " .player-item[data-id=" +
                  player.id + "]");
            playerRow.data(player);
            playerRow.find(".player-name").text(player.name);
            var positionEle = playerRow.find(".player-position");
            if (positionEle.is("select"))
                positionEle.val(player.position);
            else
                positionEle.text(player.position);
        }

        page.closeDialog("#edit-dialog");
        page.updateConfirmedSection();
    };
};
// Game Detail Page --------------------------------------------------------- END
