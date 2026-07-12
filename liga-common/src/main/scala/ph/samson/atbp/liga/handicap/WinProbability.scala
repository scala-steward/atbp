package ph.samson.atbp.liga.handicap

import dimos.glicko2.glicko
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.PlayerRating

/** Win probability for handicapped race-to-N matches using Glicko2 per-game
  * expected scores.
  *
  * A billiards "race to N" ends when either player reaches N games on the
  * scoreboard. A handicap of `h` spots the weaker player `h` games at the start
  * (they begin at score `h`, the stronger player at 0). Each subsequent rack is
  * modelled as an independent game whose outcome probability comes from
  * Glicko2.
  */
object WinProbability {

  /** Probability the weaker player wins a single game against the stronger
    * player.
    *
    * Glicko2 works in a transformed rating scale (μ, φ). The library's
    * `epsilon(μ_weaker, μ_stronger, φ_stronger)` is the expected score for the
    * weaker player in one game — i.e. P(weaker wins that game). It is the
    * standard logistic form:
    *
    * E = 1 / (1 + exp(−g(φ_stronger) · (μ_weaker − μ_stronger)))
    *
    * We use the stronger player's RD because their uncertainty enters the
    * opponent term in the Glicko2 update formula.
    */
  def gameWinProbability(
      weaker: PlayerRating,
      stronger: PlayerRating
  ): Double = {
    val initRating = Tuning.Default.initRating
    // Convert human-scale ratings (≈1500) to Glicko2 μ scale.
    val miWeaker = glicko.mi(initRating)(weaker.rating)
    val miStronger = glicko.mi(initRating)(stronger.rating)
    // φ is rating deviation on the Glicko2 scale (RD / 173.7178).
    val phiStronger = glicko.pfi(stronger.rd)
    glicko.epsilon(miWeaker, miStronger, phiStronger)
  }

  /** Probability the weaker player wins a race-to-`raceTo` match when spotted
    * `handicap` games at the start.
    *
    * Example: race-to-7 with handicap 2 means the weaker player starts at 2-0.
    * They still need to reach 7 on the board, so they require
    * `raceTo − handicap` additional game wins. The stronger player starts at 0
    * and needs `raceTo` game wins. The match is therefore a race: weaker must
    * collect `weakerWinsNeeded` wins before the stronger collects
    * `strongerWinsNeeded`.
    */
  def matchWinProbability(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      handicap: Int
  ): Double = {
    val perGame = gameWinProbability(weaker, stronger)
    val weakerWinsNeeded = raceTo - handicap
    val strongerWinsNeeded = raceTo
    raceWinProbability(perGame, weakerWinsNeeded, strongerWinsNeeded)
  }

  /** Probability the weaker player wins a race where they need
    * `weakerWinsNeeded` game wins before the stronger player reaches
    * `strongerWinsNeeded`.
    *
    * Each game is an independent Bernoulli trial with success probability `p`
    * (weaker wins that game). The weaker player wins the match iff they reach
    * their target before the opponent does — a classic "race" or ballot
    * problem.
    *
    * Summing over how many games the stronger player wins before the weaker
    * player clinches (k = 0 … strongerWinsNeeded − 1):
    *
    * P(weaker wins) = Σₖ C(a+k−1, k) · pᵃ · (1−p)ᵏ
    *
    * where a = weakerWinsNeeded. The binomial coefficient counts the ways to
    * interleave the weaker player's `a` decisive wins with exactly `k` opponent
    * wins such that the weaker player's `a`-th win is the final game.
    */
  def raceWinProbability(
      perGameWinProbability: Double,
      weakerWinsNeeded: Int,
      strongerWinsNeeded: Int
  ): Double = {
    if (weakerWinsNeeded <= 0) {
      // Spot already covers the full race (or beyond) — weaker has effectively won.
      1.0
    } else if (strongerWinsNeeded <= 0) {
      0.0
    } else if (perGameWinProbability <= 0.0) {
      0.0
    } else if (perGameWinProbability >= 1.0) {
      1.0
    } else {
      val lossProbability = 1.0 - perGameWinProbability
      (0 until strongerWinsNeeded).map { opponentWins =>
        // Term for "weaker gets their a-th win while stronger has exactly k wins".
        binomial(weakerWinsNeeded + opponentWins - 1, opponentWins) *
          Math.pow(perGameWinProbability, weakerWinsNeeded) *
          Math.pow(lossProbability, opponentWins)
      }.sum
    }
  }

  /** Binomial coefficient C(n, k) computed iteratively to avoid factorial
    * overflow. Only used for small n (race-to-N caps keep this well within safe
    * range).
    */
  private def binomial(n: Int, k: Int): Double = {
    if (k < 0 || k > n) {
      0.0
    } else {
      (1 to k).foldLeft(1.0) { (acc, i) =>
        acc * (n - k + i) / i
      }
    }
  }
}
