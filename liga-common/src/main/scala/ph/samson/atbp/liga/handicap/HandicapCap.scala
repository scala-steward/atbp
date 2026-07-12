package ph.samson.atbp.liga.handicap

/** Billiards handicap cap: spot at most 75% of the race length. */
object HandicapCap {
  val RaceToFactor: Double = 0.75

  def capFor(raceTo: Int): Int =
    (RaceToFactor * raceTo).floor.toInt
}
