package ph.samson.atbp.liga.roster

import ph.samson.atbp.liga.glicko.Tuning

/** One row in the director define-step roster preview. */
final case class RosterEntry(name: String, rating: Double, guest: Boolean)

/** Parse signup paste and resolve against period ratings for define-step UI.
  *
  * GuestDisplayRating matches seed initRating for display/sort only.
  */
object RosterPaste {

  /** Same as JVM Glicko initRating; display-only — do not drift without asking.
    */
  val GuestDisplayRating: Double = Tuning.Default.initRating

  def parsePaste(raw: String): List[String] =
    raw.split("\n").iterator.map(_.trim).filter(_.nonEmpty).toList.distinct

  def resolveRoster(
      names: List[String],
      periodByName: Map[String, Double]
  ): List[RosterEntry] =
    resolveRoster(names, periodByName, GuestDisplayRating)

  def resolveRoster(
      names: List[String],
      periodByName: Map[String, Double],
      guestRating: Double
  ): List[RosterEntry] =
    names
      .map { name =>
        periodByName.get(name) match {
          case Some(rating) => RosterEntry(name, rating, guest = false)
          case None         => RosterEntry(name, guestRating, guest = true)
        }
      }
      .sortBy(e => (-e.rating, e.name))
}
