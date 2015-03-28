function togglePlayerActive(ele)
{
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
