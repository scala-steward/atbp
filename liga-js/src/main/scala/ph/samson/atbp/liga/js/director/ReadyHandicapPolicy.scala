package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*

/** Ready-stage handicap UI policy for director MatchPanel. */
object ReadyHandicapPolicy {

  /** One Ready-stage handicap surface for MatchPanel to match on. */
  enum Surface {
    case Preview(preview: MatchHandicapPreview)
    case PreviewWaiting
    case Adjust(suggested: Int, preview: Option[MatchHandicapPreview])
  }

  val previewWaitingMessage: String =
    "Waiting for player ratings to compute a preview."

  def surface(
      matchDef: BracketMatch,
      preview: Option[MatchHandicapPreview]
  ): Surface =
    matchDef.handicapSuggested match {
      case Some(suggested) => Surface.Adjust(suggested, preview)
      case None            =>
        preview match {
          case Some(p) => Surface.Preview(p)
          case None    => Surface.PreviewWaiting
        }
    }
}
