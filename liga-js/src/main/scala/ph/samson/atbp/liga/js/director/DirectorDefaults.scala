package ph.samson.atbp.liga.js.director

/** Defaults for director actions when the server has no explicit config. */
object DirectorDefaults {

  /** Race-to 7 for every bracket round (covers 8–64 player tournaments). */
  def defaultRoundRaceTo: Map[Int, Int] =
    (1 to 7).map(_ -> 7).toMap
}
