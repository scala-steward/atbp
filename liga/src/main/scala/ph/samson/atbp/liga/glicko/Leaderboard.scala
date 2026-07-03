package ph.samson.atbp.liga.glicko

import ph.samson.atbp.liga.model.*

object Leaderboard {

  def compute(periods: List[Period]): List[PlayerRating] = {
    val snapshot =
      periods.foldLeft(Glicko2.empty) { (state, period) =>
        period.matches.foldLeft(state)(Glicko2.updateAfterMatch)
      }
    Glicko2.leaderboard(snapshot.view.mapValues(_.toPlayerRating).toMap)
  }
}
