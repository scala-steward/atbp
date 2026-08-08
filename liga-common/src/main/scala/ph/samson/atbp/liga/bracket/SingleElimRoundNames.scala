package ph.samson.atbp.liga.bracket

/** Conventional display names for single-elimination rounds from players
  * remaining.
  */
object SingleElimRoundNames {

  def name(playersRemaining: Int): String = {
    require(
      playersRemaining >= 2 && isPowerOfTwo(playersRemaining),
      s"unsupported SE field: $playersRemaining"
    )
    playersRemaining match {
      case 2 => "Grand Final"
      case 4 => "Semifinals"
      case 8 => "Quarterfinals"
      case n => s"Round of $n"
    }
  }

  def name(round: Int, seRounds: Int): String = {
    require(
      seRounds >= 1 && round >= 1 && round <= seRounds,
      s"unsupported SE round: round=$round seRounds=$seRounds"
    )
    name(1 << (seRounds - round + 1))
  }
  private def isPowerOfTwo(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0
}
