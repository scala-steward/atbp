package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.api.PlayerRatingConversions

/** Ready-stage handicap UI policy for director MatchPanel. */
object ReadyHandicapPolicy {

  /** One Ready-stage handicap surface for MatchPanel to match on. */
  enum Surface {
    case Preview(preview: MatchHandicapPreview)
    case PreviewWaiting
    case Adjust(suggested: Int, preview: Option[MatchHandicapPreview])
    case ZeroLocked(suggested: Int, preview: Option[MatchHandicapPreview])
  }

  val previewWaitingMessage: String =
    "Waiting for player ratings to compute a preview."

  val zeroLockedEncouragement: String =
    "At least one player is unrated, so handicap stays 0. Play the full match " +
      "so we can rate them and offer a meaningful handicap next time."

  def requiresZeroHandicap(
      matchDef: BracketMatch,
      frozenRatings: List[PlayerRating]
  ): Boolean = {
    val ratingA =
      matchDef.playerA.flatMap(p => frozenRatings.find(_.player.name == p.name))
    val ratingB =
      matchDef.playerB.flatMap(p => frozenRatings.find(_.player.name == p.name))
    (ratingA, ratingB) match {
      case (Some(a), Some(b)) =>
        Handicap.requiresZeroHandicap(
          PlayerRatingConversions.toShared(a),
          PlayerRatingConversions.toShared(b)
        )
      case _ => false
    }
  }

  def surface(
      matchDef: BracketMatch,
      frozenRatings: List[PlayerRating],
      preview: Option[MatchHandicapPreview]
  ): Surface =
    if (
      requiresZeroHandicap(matchDef, frozenRatings) &&
      matchDef.handicapSuggested.isDefined
    ) {
      Surface.ZeroLocked(
        suggested = matchDef.handicapSuggested.get,
        preview = preview
      )
    } else {
      matchDef.handicapSuggested match {
        case Some(suggested) => Surface.Adjust(suggested, preview)
        case None            =>
          preview match {
            case Some(p) => Surface.Preview(p)
            case None    => Surface.PreviewWaiting
          }
      }
    }
}
