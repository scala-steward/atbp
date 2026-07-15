package ph.samson.atbp.liga.roster

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.Player
import ph.samson.atbp.liga.model.PlayerRating

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
    raw
      .split("\\r\\n|\\r|\\n")
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
      .distinct

  def resolveRoster(
      names: List[String],
      periodByName: Map[String, PlayerRating]
  ): List[RosterEntry] = {
    val tuning = Tuning.Default
    val resolved = names.map { name =>
      periodByName.get(name) match {
        case Some(rating) =>
          (rating, false)
        case None =>
          (
            PlayerRating(
              player = Player(name),
              rating = tuning.initRating,
              rd = tuning.maxDeviation,
              wins = 0,
              losses = 0
            ),
            true
          )
      }
    }
    resolved
      .sortWith((left, right) =>
        RatingOrder.compareBestFirst(left._1, right._1)
      )
      .map { case (rating, guest) =>
        RosterEntry(rating.player.name, rating.rating, guest = guest)
      }
  }
}
