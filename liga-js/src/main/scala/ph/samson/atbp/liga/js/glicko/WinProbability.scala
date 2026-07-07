package ph.samson.atbp.liga.js.glicko

import dimos.glicko2.glicko
import ph.samson.atbp.liga.js.api.Models.PlayerRating

/** Win probability for handicapped race-to-N matches using Glicko2 per-game
  * expected scores.
  */
object WinProbability {

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
