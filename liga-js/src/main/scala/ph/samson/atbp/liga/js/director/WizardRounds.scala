package ph.samson.atbp.liga.js.director

/** Topology-derived race-to round keys (mirrors JVM BracketRounds). */
object WizardRounds {

  def requiredKeys(playerCount: Int): List[Int] = {
    val bracketSize =
      if (playerCount <= 8) {
        8
      } else if (playerCount <= 16) {
        16
      } else if (playerCount <= 32) {
        32
      } else {
        64
      }
    bracketSize match {
      case 8  => (1 to 4).toList
      case 16 => (1 to 6).toList
      case 32 => (1 to 8).toList
      case 64 => (1 to 10).toList
      case _  => (1 to 4).toList
    }
  }
}
