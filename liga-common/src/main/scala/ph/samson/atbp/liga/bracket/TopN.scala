package ph.samson.atbp.liga.bracket

/** Legal Top N values and default for elimination cut tournaments. */
object TopN {

  /** `1` plus powers of two less than or equal to `rosterSize`. */
  def legalTopNs(rosterSize: Int): List[Int] =
    1 :: Iterator
      .iterate(2)(_ * 2)
      .takeWhile(_ <= rosterSize)
      .toList

  def defaultTopN(rosterSize: Int): Int = {
    val legal = legalTopNs(rosterSize)
    if (legal.contains(8)) 8 else legal.filter(_ > 1).maxOption.getOrElse(1)
  }
}
