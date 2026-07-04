package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.PlayerRating

/** Rating-based seeding for double-elimination brackets. */
object Seeding {

  /** Players sorted best-first (seed 1 = highest rating). */
  def seedOrder(players: List[PlayerRating]): List[PlayerRating] =
    players.sortWith(compareRatings)

  /** Standard tournament bracket slot order (1-based seed numbers). */
  def bracketSlotSeeds(bracketSize: Int): List[Int] = {
    require(isPowerOfTwo(bracketSize), s"bracket size must be a power of two: $bracketSize")
    val rounds = log2(bracketSize)
    (1 until rounds).foldLeft(List(1, 2)) { (seeds, _) =>
      val nextSize = seeds.length * 2
      seeds.flatMap(seed => List(seed, nextSize + 1 - seed))
    }
  }

  /** Round player count up to the next power of two in [8, 64]. */
  def bracketSize(playerCount: Int): Int = {
    require(playerCount >= 8 && playerCount <= 64, s"player count must be 8–64: $playerCount")
    nextPowerOfTwo(playerCount)
  }

  private def compareRatings(a: PlayerRating, b: PlayerRating): Boolean = {
    val byRating = a.rating.compare(b.rating)
    if (byRating > 0) {
      true
    } else if (byRating < 0) {
      false
    } else {
      val byRd = a.rd.compare(b.rd)
      if (byRd < 0) {
        true
      } else if (byRd > 0) {
        false
      } else {
        a.player.name < b.player.name
      }
    }
  }

  private def isPowerOfTwo(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0

  private def nextPowerOfTwo(n: Int): Int =
    if (n <= 8) {
      8
    } else if (n <= 16) {
      16
    } else if (n <= 32) {
      32
    } else {
      64
    }

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
