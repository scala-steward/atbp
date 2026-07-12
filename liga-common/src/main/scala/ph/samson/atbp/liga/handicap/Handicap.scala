package ph.samson.atbp.liga.handicap

import ph.samson.atbp.liga.model.*

/** Handicap suggestion for race-to-N billiards matches.
  *
  * Given two period-start ratings and a race-to-N, finds the integer spot `h`
  * for the weaker player such that P(weaker wins the match | h) ≈ 50%. The same
  * pure function is used by the CLI (`liga handicap`), the director UI, and the
  * server.
  */
object Handicap {

  def suggest(
      a: PlayerRating,
      b: PlayerRating,
      raceTo: Int
  ): HandicapSuggestion =
    if (a.rating == b.rating) {
      val weaker = List(a, b).minBy(_.player.name)
      HandicapSuggestion(weaker.player, handicap = 0, raceTo)
    } else {
      val (weaker, stronger) = weakerAndStronger(a, b)
      val cap = HandicapCap.capFor(raceTo)
      val handicap = searchHandicap(weaker, stronger, raceTo, cap)
      HandicapSuggestion(weaker.player, handicap, raceTo)
    }

  private def weakerAndStronger(
      a: PlayerRating,
      b: PlayerRating
  ): (PlayerRating, PlayerRating) =
    if (a.rating < b.rating) (a, b) else (b, a)

  private def searchHandicap(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      cap: Int
  ): Int = {
    if (cap == 0) {
      0
    } else {
      val lowerBound =
        binarySearchLowerBound(weaker, stronger, raceTo, cap, lo = 0, hi = cap)
      val candidates =
        List(lowerBound, lowerBound - 1).filter(h => h >= 0 && h <= cap)
      candidates.minBy { handicap =>
        val probability =
          WinProbability.matchWinProbability(weaker, stronger, raceTo, handicap)
        Math.abs(probability - 0.5)
      }
    }
  }

  private def binarySearchLowerBound(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      cap: Int,
      lo: Int,
      hi: Int
  ): Int = {
    if (lo >= hi) {
      lo
    } else {
      val mid = (lo + hi) / 2
      val probability =
        WinProbability.matchWinProbability(weaker, stronger, raceTo, mid)
      if (probability < 0.5) {
        binarySearchLowerBound(weaker, stronger, raceTo, cap, mid + 1, hi)
      } else {
        binarySearchLowerBound(weaker, stronger, raceTo, cap, lo, mid)
      }
    }
  }
}
