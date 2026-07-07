package ph.samson.atbp.liga.js.glicko

import ph.samson.atbp.liga.js.api.Models.HandicapSuggestion
import ph.samson.atbp.liga.js.api.Models.PlayerRating

/** Client-side handicap preview — mirrors `liga.handicap.Handicap` for instant
  * director UI feedback before the server confirms.
  */
object HandicapPreview {

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
      val cap = (0.75 * raceTo).floor.toInt
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
