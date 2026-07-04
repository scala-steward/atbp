package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.cli.MarkdownTable.Alignment
import ph.samson.atbp.liga.model.PlayerRating

object LeaderboardRenderer {

  private val columns = List(
    MarkdownTable.Column("Player", Alignment.Left),
    MarkdownTable.Column("Rating", Alignment.Right),
    MarkdownTable.Column("RD", Alignment.Right),
    MarkdownTable.Column("W-L", Alignment.Right)
  )

  def render(ratings: List[PlayerRating]): String = {
    val rows = ratings.sortBy(-_.rating).map(renderRow)
    MarkdownTable.render(columns, rows)
  }

  private def renderRow(rating: PlayerRating): List[String] = {
    val record = s"${rating.wins}-${rating.losses}"
    List(
      rating.player.name,
      rating.rating.round.toInt.toString,
      f"${rating.rd}%.1f",
      record
    )
  }
}
