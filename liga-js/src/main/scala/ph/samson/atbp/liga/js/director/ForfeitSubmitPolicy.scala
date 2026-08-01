package ph.samson.atbp.liga.js.director

/** Pure validation for director forfeit submission from MatchPanel. */
object ForfeitSubmitPolicy {

  final case class Submit(side: String, reason: String)

  sealed trait Outcome

  object Outcome {
    final case class Ready(submit: Submit) extends Outcome
    case object BlankReason extends Outcome
  }

  val blankReasonMessage: String = "Forfeit reason is required."

  /** Validates side and reason without mutating UI state. */
  def validate(side: String, reason: String): Outcome = {
    val trimmed = reason.trim
    if (trimmed.isEmpty) {
      Outcome.BlankReason
    } else {
      Outcome.Ready(Submit(side, trimmed))
    }
  }
}
