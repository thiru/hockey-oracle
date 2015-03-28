function makeTeams()
{
  var selPlayers = [];
  $(".data-table input:checked + .player-name").each(function() {
    selPlayers.push($(this).text());
  });

  alert(selPlayers.length);
}
