package ph.samson.atbp.liga.glicko

import ph.samson.atbp.liga.model.*

object Leaderboard {

  def compute(periods: List[Period]): List[PlayerRating] = {
    val snapshot =
      periods.foldLeft(Glicko2.empty) { (state, period) =>
        Glicko2.updateAfterPeriod(state, period)
      }
    Glicko2.leaderboard(snapshot.view.mapValues(_.toPlayerRating).toMap)
  }
}
