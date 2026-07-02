package ph.samson.atbp.liga.model

/** Winner of one atomic game within a match. */
enum GameWinner {
  case PlayerA, PlayerB
}

/** Expand final match scores into atomic per-game outcomes for Glicko2 updates. */
object ScoreExpansion {

  /** Game order within the match is irrelevant; only win counts matter. */
  def expandGames(scoreA: Int, scoreB: Int): List[GameWinner] = {
    val aWins = List.fill(scoreA)(GameWinner.PlayerA)
    val bWins = List.fill(scoreB)(GameWinner.PlayerB)
    aWins ++ bWins
  }
}
