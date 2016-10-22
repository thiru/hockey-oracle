// TODO: Replace alerts with inline message wherever feasible

/*
 * Core domain.
 */

// Global
var page= {};

// NOTE: This format needs to be in sync with the back-end date format
page.dateFmt = "ddd MMM D";
page.timeFmt = "h:mm a";
page.dateAndTimeFmt = "ddd MMM D, h:mm a";
page.inputDateFmts = ["MMM D YYYY", "MMMM D YYYY", "YYYY MM DD"];
page.inputTimeFmts = ["h:mm a", "HH:mm"];

page.dialogs = [];

$(document).ready(function() {
    page.init();
    if (get("user-detail-page"))
        page.initUserDetailPage();
    if (get("manage-league-page"))
        page.initManageLeaguePage();
    if (get("schedule-page"))
        page.initSchedulePage();
    if (get("game-detail-page"))
        page.initGameDetailPage();
    if (get("player-list-page"))
        page.initGameDetailPage();
});

page.init = function() {
    page.userId = parseInt(get("root").dataset.user);
    page.leagueName = get("root").dataset.league;

    page.toggleMainMenu = function() {
        $("#ham-menu-group").toggleClass("hidden");
    };

    $(document).keyup(function(event) {
        if (event.keyCode === 27) // Escape key
            page.closeAllDialogs();
    });

    page.openDialog = function(jqId) {
        $("#overlay").show();
        $(jqId).css("display", "flex");
        page.dialogs.push(jqId);
    };
    page.closeDialog = function(jqId, keepOverlay) {
        if (!keepOverlay) $("#overlay").hide();
        $(jqId).hide();
    };
    page.closeAllDialogs = function() {
        for (var i = 0; i < page.dialogs.length; i++)
            page.closeDialog(page.dialogs[i], /*keepOverlay*/ true);
        $("#overlay").hide();
    };

    page.parseTime = function(eleId) {
        var strTime = get(eleId).value;
        var parsedTime = moment(strTime,
                                page.inputTimeFmts,
                                /*strictMode*/ false);

        if (!parsedTime.isValid())
            return Result.error("Time is invalid.", strTime);

        return Result.success("", parsedTime.format("HH:mm"));
    };

    page.parseDateTime = function(dateEleId, timeEleId) {
        var rawDate = get(dateEleId).value;
        var rawTime = get(timeEleId).value;

        var parsedDate = moment(rawDate,
                                page.inputDateFmts,
                                /*strictMode:*/ false);
        var parsedTime = moment(rawTime,
                                page.inputTimeFmts,
                                /*strictMode:*/ false);

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
        page.openDialog("#login-dialog");
        $("#login-email-address").focus().select();
    };
    page.closeLogin = function() {
        page.closeDialog("#login-dialog");
    };
    page.login = function() {
        $("#login-result").attr("class", "").html("");

        var result = null;
        var email = get("login-email-address").value;
        var pwd = get("login-pwd").value;

        if (isBlank(email))
            result = Result.error("No email address provided.");
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

        $.post("/api/login", { email: email, pwd: pwd })
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
        var email = get("login-email-address").value;

        if (isBlank(email))
            result = Result.error("No email address provided.");

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

        $.post("/api/forgot-password", { email: email })
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
    page.isSaving = false;

    page.inputChanged = function() {
        if (page.isSaving) return;

        if (dataChanged("player-name-edit")
            || dataChanged("player-email-edit")
            || dataChanged("player-active-edit")
            || dataChanged("notify-on-player-status-change-edit")
            || dataChanged("notify-on-player-chat-edit")
            || dataChanged("player-pos-edit")
            || dataChanged("pwd-new-repeat")) {
            $("#save-btn").show();
            $("#save-result").empty();
        }
        else
            $("#save-btn").hide();
    };

    page.changePwd = function() {
        $("#pwd-group").show();
        $("#change-pwd-btn").hide();
    };

    page.saveUser = function() {
        var player = {};

        player.leagueName = page.leagueName;

        // Get player id
        player.id = get("player-name-edit").dataset.playerId;

        // Get player name
        player.name = $("#player-name-edit").val().trim();
        if (isBlank(player.name)) {
            alert("Name can't be blank.");
            $("#save-result").empty();
            return;
        }

        // Get player email
        player.email = $("#player-email-edit").val().trim();

        // Get active status
        if (get("player-active-edit"))
            player.active = get("player-active-edit").checked;
        else
            player.active = false;

        // Get notification options
        player.notifyOnPlayerStatusChange =
            get("notify-on-player-status-change-edit").checked;
        player.notifyOnPlayerChat =
            get("notify-on-player-chat-edit").checked;

        // Get player position
        player.position = $("#player-pos-edit :selected").val();

        var newPwd = get("pwd-new").value;
        if (!isBlank(newPwd)) {
            var newPwdRepeat = get("pwd-new-repeat").value;
            if (newPwd !== newPwdRepeat) {
                $("#save-result").empty();
                alert("New passwords don't match.");
                return;
            }

            var currPwd = "";
            if (get("pwd-curr") !== null) {
                currPwd = get("pwd-curr").value;
                if (isBlank(currPwd)) {
                    $("#save-result").empty();
                    alert("Original password not provided.");
                    return;
                }
            }

            player.currentPwd = currPwd;
            player.newPwd = newPwd;
        }

        // Saving...
        page.isSaving = true;
        $("#save-btn").prop("disabled", true);
        $("#save-result")
            .attr("class", "")
            .html("<i class='fa fa-spinner fa-pulse'></i> Saving...");

        $.post("/api/users/" + player.id, player)
            .done(function (result) {
                if (!result)
                    result = Result.error("No response from server.");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                showResult($("#save-result"), result);
                $("#save-btn").prop("disabled", false);
                if (result.succeeded()) {
                    updateOrigDataVal("player-name-edit");
                    updateOrigDataVal("player-email-edit");
                    updateOrigDataVal("player-active-edit");
                    updateOrigDataVal("notify-on-player-status-change-edit");
                    updateOrigDataVal("notify-on-player-chat-edit");
                    updateOrigDataVal("player-pos-edit");
                    $("#save-btn").hide();
                }
                page.isSaving = false;
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
                page.isSaving = false;
            });
    };

    $("#player-name-edit").on("input", page.inputChanged);
    $("#player-email-edit").on("input", page.inputChanged);
    $("#player-active-edit").change(page.inputChanged);
    $("#notify-on-player-status-change-edit").change(page.inputChanged);
    $("#notify-on-player-chat-edit").change(page.inputChanged);
    $("#player-pos-edit").change(page.inputChanged);
    $("#pwd-new-repeat").on("input", page.inputChanged);
};
// User Detail Page --------------------------------------------------------- END

// Manage League Page
page.initManageLeaguePage = function() {
    page.save = function() {
        var league = {
            sendAutomatedEmails: get("send-automated-emails").checked,
            gameReminderDayOffset: parseInt(
                get("email-reminder-day-offset").value)
        };

        if (isNaN(league.gameReminderDayOffset) ||
            league.gameReminderDayOffset < 0)
            return showResult($("#save-result"),
                              Result.error("Day offset for game email " +
                                           "reminder must be a positive " +
                                           "integer."));

        var parsedTime = page.parseTime("email-reminder-time");
        if (parsedTime.failed())
            return showResult($("#save-result"),
                              Result.error("Time of game email reminder is " +
                                           "not valid. Try one of the " +
                                           "following formats: " +
                                           page.inputTimeFmts.join(", ") + "."));

        league.gameReminderTime = parsedTime.data;

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

// Schedule Page
page.initSchedulePage = function() {
    // Show relative times for next 5 games
    var count = 5;
    $("#schedule-list .game-item").each(function() {
        if (count <= 0) return false;

        var gameTimeStamp = $(this).find(".game-time").text().trim();
        var gameTimeRel = moment(gameTimeStamp, page.dateAndTimeFmt).fromNow();
        $(this).find(".game-rel-time").text("(" + gameTimeRel + ")");
        count--;
    });

    page.openGameEditor = function() {
        page.openDialog("#new-game-dialog");
        get("date-picker").focus();
        get("date-picker").value = moment().add(1, "day").format("YYYY-MM-DD");
        get("time-picker").value = moment().format("h:00 a");
        page.updateRelTime();
    };

    page.gameTimeKeyUp = function(event) {
        page.updateRelTime();
        if (event.keyCode === 13) // Enter
            page.saveGame();
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
                        item.find(".game-time")
                            .text(result.data[1])
                            .attr("href",
                                  item.find(".game-time")
                                  .attr("href")
                                  .replace("<GAME-ID>", result.data[0]));
                        $("#new-games-list").append(item);
                        var newGamesCountData = $("#new-games-count").data();
                        newGamesCountData.count++;
                        $("#new-games-count").text("(" + newGamesCountData.count + ")");
                        $("#new-games-section").show();
                        $("#no-games").hide();
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
// Schedule Page ------------------------------------------------------------ END

// Game Detail Page
page.initGameDetailPage = function() {
    page.origReasonTxt = $("#reason-input").val();

    // Replace absolute times with relative
    $(".confirm-time, .msg-updated").each(function() {
        var absConfirmTime = $(this).text().trim();
        if (!isBlank(absConfirmTime)) {
            $(this).attr("title", absConfirmTime);
            var relConfirmTime =
                moment(absConfirmTime, page.dateAndTimeFmt).fromNow();
            $(this).text(relConfirmTime);
        }
    });

    // Go to last chat message if applicable
    if (window.location.hash.toLowerCase() == '#chat') {
        var lastMsg = $("#chat-msg-list li:last-child");
        if (lastMsg && lastMsg.length)
            lastMsg[0].scrollIntoView(true);
    }

    page.editGame = function() {
        $("#game-info-edit").toggle();
        $("#confirm").toggle();
        $("#save-res").empty();
        $("#quick-crud-res").empty();
    };

    page.showEmailRemindersSection = function() {
        $("#email-reminders-content").toggle();
        $("#email-reminders-toggle i").toggleClass("fa-chevron-circle-down");
        $("#email-reminders-toggle i").toggleClass("fa-chevron-circle-up");
    };

    page.sendEmailReminder = function() {
        if (!confirm("Are you sure you want to send all active players an " +
                     "email reminder of this game right now?"))
            return;

        $("#email-reminder-btn").prop("disabled", true);
        showLoading("#quick-crud-res", "Emailing...");

        var gameId = parseInt(get("game-info-edit").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + gameId;
        $.post(url, { sendEmailReminder: true })
            .done(function (result) {
                if (!result) {
                    showResult($("#quick-crud-res"),
                               Result.error("No response from server."));
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showResult($("#quick-crud-res"), result);
                }
                $("#email-reminder-btn").prop("disabled", false);
            })
            .fail(function(data) {
                var result = data.responseJSON;
                if (!result)
                    result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                showResult($("#quick-crud-res"), result);
                $("#email-reminder-btn").prop("disabled", false);
            });
    };

    page.deleteGame = function() {
        if (!confirm($("#delete-btn").data().deleteMsg))
            return;

        $(".crud-btn").prop("disabled", true);
        showLoading("#quick-crud-res");

        var gameId = parseInt(get("game-info-edit").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + gameId;
        $.post(url, { deleteGame: true })
            .done(function (result) {
                if (!result) {
                    showResult($("#quick-crud-res"),
                               Result.error("No response from server."));
                    $(".crud-btn").prop("disabled", false);
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showResult($("#quick-crud-res"), result);
                    $("#quick-crud-btns").hide();
                }
            })
            .fail(function(data) {
                var result = data.responseJSON;
                if (!result)
                    result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                showResult($("#quick-crud-res"), result);
                $(".crud-btn").prop("disabled", false);
            });
    }

    page.saveGame = function() {
        var gameInfo = {
            gameTime: page.parseDateTime("game-date-rw", "game-time-rw"),
        };

        if (!gameInfo.gameTime) {
            showResult($("#save-res"), Result.error("Game date/time invalid."))
            return;
        }

        gameInfo.gameTime = gameInfo.gameTime.format();
        gameInfo.gameProgress = $("#game-status-ddl :selected").val().trim();
        gameInfo.gameNotes = $("#game-notes-input").val();

        $("#edit-actions .button").prop("disabled", true);
        showLoading("#save-res");

        var gameId = parseInt(get("game-info-edit").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/" + gameId;
        $.post(url, gameInfo)
            .done(function (result) {
                if (!result) {
                    showResult($("#save-res"),
                               Result.error("No response from server."));
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);
                    showResult($("#save-res"), result);
                    if (isBlank(gameInfo.gameNotes))
                        $("#game-notes").addClass("hidden");
                    else
                        $("#game-notes").removeClass("hidden");
                    $("#game-notes-ro").text(escapeHtml(gameInfo.gameNotes));

                }
                $("#edit-actions .button").prop("disabled", false);
            })
            .fail(function(data) {
                var result = data.responseJSON;
                if (!result)
                    result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                else
                    result = new Result(result.level, result.message,
                                        result.data);
                showResult($("#save-res"), result);
                $("#edit-actions .button").prop("disabled", false);
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

        $("#reason-input-group").show();

        page.saveConfirmInfo();
    };

    page.reasonTextChanged = function(ele) {
        var reasonTxt = $("#reason-input").val();
        var saveBtn = $("#save-confirm-info-btn");
        if (page.origReasonTxt != reasonTxt)
            saveBtn.prop("disabled", false);
        else
            saveBtn.prop("disabled", true);

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

        var gameId = parseInt(get("game-info-edit").dataset.game);
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
                    if (result.succeeded()) {
                        $("#save-confirm-info-btn").prop("disabled", true);
                        page.origReasonTxt = $("#reason-input").val();
                    }

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

            playerEle.find(".confirm-time").html("just updated");
            playerEle.find(".confirm-reason").html(escapeHtml(reason));
        }

        // Update player to appropriate section
        var parentSection = playerEle.parents(".grouped-players-section");
        page.movePlayerTo(playerEle, confirmType);
    };

    page.showPlayerToggles = function() {
        var makeTeamsBtnData = $("#make-teams").data();

        if (!makeTeamsBtnData.isPressed) {
            makeTeamsBtnData.isPressed = true;
            $(".playing-toggle-col").show();
        }
    };

    page.makeTeams = function() {
        page.showPlayerToggles();

        $("#make-teams-msg").hide();

        var activePlayersRes = getActivePlayers();
        if (activePlayersRes.failed()) {
            showResult($("#make-teams-msg"), activePlayersRes);
            return;
        }

        var teamsRes = generateTeams(2, activePlayersRes.data);
        if (teamsRes.failed()) {
            showResult($("#make-teams-msg"), teamsRes);
            return;
        }

        $("#confirm").hide();
        $("#grouped-players-sections").hide();
        $("#add-player").hide();
        populateTeams(teamsRes.data);
        $("#random-teams").show();
        $("#pick-players").show();
        $("#chat").hide();

        function getActivePlayers() {
            var players = [];

            $("#playing-players-section .player-item")
                .each(function() {
                    var player = $(this).data();
                    if (player && player.name)
                        players.push(player);
                });

            if (!players.length)
                return Result.warning("No players selected.");

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
                activePlayers.length < 2)
                return Result.warning("At least two players must be selected.");
            if (numTeams > activePlayers.length)
                return Result.warning("Please select more players for the " +
                                      "game. Currently there are only " +
                                      activePlayers.length + " players " +
                                      "selected to be divided into " + numTeams +
                                      " teams.")

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

    page.updateGroupedPlayersSections = function() {
        $(".grouped-players-section").each(function() {
            var sectionEle = $(this);
            var playerCount = 0;
            var goalieCount = 0;
            var playerEles = $(this).find(".player-item").each(function() {
                if ($(this).data().position == "G")
                    goalieCount++;
                else
                    playerCount++;
            });
            if (!playerCount && !goalieCount) {
                sectionEle.hide();
                sectionEle.find(".player-count").text("");
            }
            else {
                // NOTE: this grammar needs to be in sync with backend
                var countsTxt =
                  "(" + playerCount + pluralize(" player", playerCount) +
                  ", " + goalieCount + pluralize(" goalie", goalieCount) + ")";
                sectionEle.show();
                sectionEle.find(".player-count").text(countsTxt);
            }

            if (sectionEle.attr("id") == "playing-players-section") {
                if (playerEles.length)
                    $("#no-confirmed-players-heading").hide();
                else
                    $("#no-confirmed-players-heading").show();
            }
        });
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

    page.movePlayerTo = function(ele, confirmType) {
        var currPlayerEle = $(ele).hasClass("player-item")
            ? ele
            : $(ele).parents(".player-item");
        var player = currPlayerEle.data();
        var moveToEle = $(".template-player-item .player-item").first().clone();

        moveToEle.data(player);
        moveToEle.find(".player-name")
            .text(player.name)
            .attr("href", player.uri);
        moveToEle.find(".player-position").val(player.position);
        moveToEle.find(".confirm-reason").text(player.reason);
        moveToEle.find(".confirm-time").text(player.responseTime);

        var playingToggle;

        confirmType = confirmType
            .replace(" ", "-")
            .replace(/[^a-zA-Z0-9-]/gi, "")
            .toUpperCase();

        if (confirmType == "PLAYING") {
            playingToggle = $("#playing-toggle-templates .confirmed").clone();
            page.moveToPlayerList($("#playing-players-section .player-confirms"),
                                  moveToEle);
        }
        else if (confirmType == "MAYBE") {
            playingToggle = $("#playing-toggle-templates .unconfirmed").clone();
            page.moveToPlayerList($("#maybe-players-section .player-confirms"),
                                  moveToEle);
        }
        else if (confirmType == "CANT-PLAY") {
            playingToggle = $("#playing-toggle-templates .unconfirmed").clone();
            page.moveToPlayerList($("#cant-play-players-section .player-confirms"),
                                  moveToEle);
        }
        else if (confirmType == "NO-RESPONSE") {
            playingToggle = $("#playing-toggle-templates .unconfirmed").clone();
            page.moveToPlayerList($("#cant-play-players-section .player-confirms"),
                                  moveToEle);
        }
        else {
            throw new Error("Unrecognised 'confirmType': \"" + confirmType + "\".");
        }

        moveToEle.find(".playing-toggle-col")
            .empty()
            .append(playingToggle);

        currPlayerEle.remove();

        page.updateGroupedPlayersSections();
    };

    page.pickPlayers = function() {
        $("#random-teams").hide();
        $("#pick-players").hide();
        $("#confirm").show();
        $("#grouped-players-sections").show();
        $("#add-player").show();
        $("#chat").show();
    };

    page.addPlayer = function() {
        $("#edit-player-dialog .save-btn").data().id = 0;
        $("#player-name-edit").val("Extra");
        $("#player-pos-edit option").removeAttr("selected");
        page.openDialog("#edit-player-dialog");
        $("#player-name-edit").focus().select();
    };

    page.savePlayer = function(playerListJQSel, templateItemJQSel) {
        page.showPlayerToggles();

        var player = {};

        // Defaulting to game detail page specifics
        if (isBlank(playerListJQSel))
            playerListJQSel = "#playing-players-section .player-confirms";
        if (isBlank(templateItemJQSel))
            templateItemJQSel = ".template-player-item .player-item";

        // Determine player ID
        player.id = $("#edit-player-dialog .save-btn").data().id;
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

        page.closeDialog("#edit-player-dialog");
        page.updateGroupedPlayersSections();
    };

    page.addChatMsg = function() {
        $("#chat-editor").show().focus();
        $("#chat-msg-char-count").show();
        $("#add-msg-btn").hide();
        $("#save-chat-msg-btn").show();
        $("#cancel-msg-btn").show();
        $("#save-chat-result").attr("class", "").html("").hide();
        $("#chat-editor")[0].scrollIntoView();
    };

    page.saveChatMsg = function() {
        var chatMsg = ($("#chat-editor").val() || "");

        var chatMsgLength = (chatMsg || "").length;
        var maxLength = $("#chat-editor").attr("maxlength");

        if (chatMsgLength >= maxLength) {
            showResult($("#save-chat-result"),
                       Result.warning("Please keep your message under " +
                                      maxLength + " characters."));
            return;
        }

        $("#save-chat-msg-btn").prop("disabled", true);
        $("#save-chat-result")
            .show()
            .html("<i class='fa fa-spinner fa-pulse'></i> Saving...");

        var gameId = parseInt(get("game-info-edit").dataset.game);
        var url = "/" + page.leagueName.toLowerCase() + "/api/games/chat/new";

        $.post(url, { gameId: gameId, msg: chatMsg})
            .done(function (result) {
                if (!result) {
                    result = Result.error("No response from server.");
                    $("#save-chat-msg-btn").prop("disabled", false);
                }
                else {
                    result = new Result(result.level, result.message,
                                        result.data);

                    if (result.succeeded()) {
                        $("#save-chat-msg-btn").prop("disabled", true);
                        page.myLastChatMsg = $("#chat-editor").val();
                        page.cancelChatMsg();
                        page.insertSavedChatMsg(chatMsg);
                    }
                    else {
                        $("#save-chat-msg-btn").prop("disabled", false);
                    }
                }

                showResult($("#save-chat-result"), result);
            })
            .fail(function(data) {
                var result = Result.error("Unexpected error. " + data.statusText +
                                          " (" + data.status + ").");
                showResult($("#save-chat-result"), result);
                $("#save-chat-msg-btn").prop("disabled", false);
            });
    };

    page.cancelChatMsg = function() {
        $("#chat-editor").val("").hide();
        page.onChatMsgChanged();
        $("#chat-msg-char-count").hide();
        $("#add-msg-btn").show();
        $("#save-chat-msg-btn").hide();
        $("#cancel-msg-btn").hide();
        $("#save-chat-result").attr("class", "").html("").hide();
    };

    page.onChatMsgChanged = function() {
        var chatMsg = ($("#chat-editor").val() || "");

        // TODO: store myLastChatMsg
        if ((page.myLastChatMsg || "") != chatMsg)
            $("#save-chat-msg-btn").prop("disabled", false);
        else
            $("#save-chat-msg-btn").prop("disabled", true);

        var chatMsgLength = (chatMsg || "").length;
        var maxLength = $("#chat-editor").attr("maxlength");
        $("#chat-msg-char-count")
            .text((maxLength - chatMsgLength) + " chars left");
    };

    page.insertSavedChatMsg = function(msg) {
        var msgRow = $("#chat-msg-template").clone();
        msgRow.find(".msg-content")
            .html(escapeHtml(msg).replace(/\n/g, "<br />"));
        msgRow.find(".msg-updated")
            .text("just now");
        $("#chat-msg-list").append(msgRow);
        var chatMsgCountData = $("#chat-msg-count").data();
        chatMsgCountData.count++;
        $("#chat-msg-count").text("(" + chatMsgCountData.count + ")");
    };
};
// Game Detail Page --------------------------------------------------------- END
