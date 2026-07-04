package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.model.PlayerRating

object LeaderboardRenderer {

  def render(ratings: List[PlayerRating]): String = {
    val rows = ratings.sortBy(-_.rating)
    val nameWidth = math.max("Player".length, rows.map(_.player.name.length).maxOption.getOrElse(0))

    val header =
      s"${padRight("Player", nameWidth)}  ${padLeft("Rating", 6)}  ${padLeft("RD", 6)}  ${padLeft("W-L", 5)}"
    val lines = rows.map(renderRow(nameWidth, _))
    (header :: lines).mkString("\n")
  }

  private def renderRow(nameWidth: Int, rating: PlayerRating): String = {
    val record = s"${rating.wins}-${rating.losses}"
    s"${padRight(rating.player.name, nameWidth)}  ${padLeft(rating.rating.round.toInt.toString, 6)}  ${padLeft(f"${rating.rd}%.1f", 6)}  ${padLeft(record, 5)}"
  }

  private def padRight(value: String, width: Int): String =
    value.padTo(width, ' ')

  private def padLeft(value: String, width: Int): String = {
    val padded = value.reverse.padTo(width, ' ').reverse
    if (padded.length > width) padded.takeRight(width) else padded
  }
}
