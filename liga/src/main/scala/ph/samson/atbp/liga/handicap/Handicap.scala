package ph.samson.atbp.liga.handicap

import ph.samson.atbp.liga.model.*

/** Handicap suggestion for race-to-N billiards matches.
  *
  * Given two period-start ratings and a race-to-N, finds the integer spot `h`
  * for the weaker player such that P(weaker wins the match | h) ≈ 50%. The same
  * pure function is used by the CLI (`liga handicap`) and the director UI.
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
      // Spec cap: never spot more than 75% of the race length.
      val cap = (0.75 * raceTo).floor.toInt
      val handicap = searchHandicap(weaker, stronger, raceTo, cap)
      HandicapSuggestion(weaker.player, handicap, raceTo)
    }

  private def weakerAndStronger(
      a: PlayerRating,
      b: PlayerRating
  ): (PlayerRating, PlayerRating) =
    if (a.rating < b.rating) (a, b) else (b, a)

  /** Find the handicap closest to a 50% match-win probability for the weaker
    * player.
    *
    * Match-win probability is monotone in `h`: a larger spot never hurts the
    * weaker player, so P(h) increases (or stays flat) as h goes from 0 to cap.
    * That lets us binary-search for the smallest h with P(h) ≥ 0.5, then pick
    * whichever of {h, h−1} is numerically closer to 0.5 (handles discrete
    * steps).
    */
  private def searchHandicap(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      cap: Int
  ): Int = {
    if (cap == 0) {
      // Race-to-1 (or similar): no headroom for a spot under the 75% cap.
      0
    } else {
      val lowerBound =
        binarySearchLowerBound(weaker, stronger, raceTo, cap, lo = 0, hi = cap)
      // Compare the crossing point with one step below — discrete h may not land on exactly 0.5.
      val candidates =
        List(lowerBound, lowerBound - 1).filter(h => h >= 0 && h <= cap)
      candidates.minBy { handicap =>
        val probability =
          WinProbability.matchWinProbability(weaker, stronger, raceTo, handicap)
        Math.abs(probability - 0.5)
      }
    }
  }

  /** Smallest handicap h in [lo, hi] such that P(weaker wins | h) ≥ 0.5.
    *
    * Standard lower-bound binary search on a non-decreasing function. If even
    * the maximum spot (cap) leaves P < 0.5 — a huge rating gap — the search
    * returns cap and the caller still picks the best candidate from {cap,
    * cap−1}.
    */
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
        // Need a bigger spot — search the upper half (exclusive of mid).
        binarySearchLowerBound(weaker, stronger, raceTo, cap, mid + 1, hi)
      } else {
        // mid is feasible; try to find a smaller spot that still reaches 50%.
        binarySearchLowerBound(weaker, stronger, raceTo, cap, lo, mid)
      }
    }
  }
}
