$(document).ready(function() {
  checkActivePlayers();

  function checkActivePlayers() {
    $("#player-list-page .player-item").each(function() {
      if ($(this).is("[data-player-active]")) {
        $(this).addClass("selected");
        $(this).find(".player-check").removeClass("fa-circle-o").addClass("fa-check-circle-o");
      }
    });
  }
});

function togglePlayerActive(ele)
{
  $(ele).toggleClass("selected");
  $(ele).find("i.player-check").toggleClass("fa-check-circle-o").toggleClass("fa-circle-o");
}

function makeTeams()
{
  var selPlayers = [];
  $(".data-table i.fa-check-circle-o + .player-name").each(function() {
    selPlayers.push($(this).text());
  });

  alert(selPlayers.length);
}
