package ph.samson.atbp.liga.handicap

import dimos.glicko2.glicko
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.PlayerRating

/** Win probability for handicapped race-to-N matches using Glicko2 per-game
  * expected scores.
  */
object WinProbability {

  /** Probability the weaker player wins a single game against the stronger
    * player.
    */
  def gameWinProbability(
      weaker: PlayerRating,
      stronger: PlayerRating
  ): Double = {
    val initRating = Tuning.Default.initRating
    val miWeaker = glicko.mi(initRating)(weaker.rating)
    val miStronger = glicko.mi(initRating)(stronger.rating)
    val phiStronger = glicko.pfi(stronger.rd)
    glicko.epsilon(miWeaker, miStronger, phiStronger)
  }

  /** Probability the weaker player wins a race-to-`raceTo` match when spotted
    * `handicap` games at the start.
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

  /** Probability player A reaches `a` game wins before player B reaches `b`
    * game wins in a sequence of independent games.
    */
  def raceWinProbability(
      perGameWinProbability: Double,
      weakerWinsNeeded: Int,
      strongerWinsNeeded: Int
  ): Double = {
    if (weakerWinsNeeded <= 0) {
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
        binomial(weakerWinsNeeded + opponentWins - 1, opponentWins) *
          Math.pow(perGameWinProbability, weakerWinsNeeded) *
          Math.pow(lossProbability, opponentWins)
      }.sum
    }
  }

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
