package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.model.PlayerRating

object LeaderboardRenderer {

  def render(ratings: List[PlayerRating]): String = {
    val rows = ratings.sortBy(-_.rating)
    val header = "| Player | Rating | RD | W-L |"
    val separator = "| --- | ---: | ---: | ---: |"
    val lines = rows.map(renderRow)
    (header :: separator :: lines).mkString("\n") + "\n"
  }

  private def renderRow(rating: PlayerRating): String = {
    val record = s"${rating.wins}-${rating.losses}"
    s"| ${rating.player.name} | ${rating.rating.round.toInt} | ${f"${rating.rd}%.1f"} | $record |"
  }
}
