package ph.samson.atbp.liga.glicko

import ph.samson.atbp.liga.glicko.Tuning.Default
import ph.samson.atbp.liga.model.*

/** Post-period rating and movement for last-period match participants. */
final case class LatestRating(
    player: Player,
    rating: Double,
    delta: Double
)

object LatestRatings {

  /** Players who played in the last period, with post-period rating and Δ. */
  def fromPeriods(periods: List[Period]): List[LatestRating] =
    periods.lastOption match {
      case None       => Nil
      case Some(last) =>
        val initRating = Default.initRating
        val beforeSnapshot =
          periods.init.foldLeft(Glicko2.empty) { (state, period) =>
            Glicko2.updateAfterPeriod(state, period)
          }
        val afterSnapshot = Glicko2.updateAfterPeriod(beforeSnapshot, last)
        val beforeByPlayer = ratingByPlayer(beforeSnapshot)
        val afterByPlayer = ratingByPlayer(afterSnapshot)
        val participants =
          last.matches
            .flatMap(m => List(m.playerA, m.playerB))
            .distinct
        participants
          .map { player =>
            val after = afterByPlayer(player)
            val before = beforeByPlayer.getOrElse(player, initRating)
            LatestRating(player, after, after - before)
          }
          .sortBy(-_.rating)
    }

  private def ratingByPlayer(snapshot: Glicko2.Snapshot): Map[Player, Double] =
    snapshot.map { case (player, ir) => player -> ir.rating }
}
