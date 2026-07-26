package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide

/** Composes applied-handicap placement from match state and handicap preview.
  */
object AppliedHandicapLabels {

  def appliedHandicapDisplay(
      matchDef: BracketMatch,
      preview: Option[MatchHandicapPreview]
  ): AppliedHandicapDisplay =
    if (!BracketLayout.showsAppliedHandicap(matchDef)) {
      AppliedHandicapDisplay.Hidden
    } else {
      val spot = matchDef.handicapApplied.getOrElse(0)
      preview match {
        case Some(p) =>
          val weakerName = p.weakerName
          weakerSide(matchDef, weakerName) match {
            case Some(side) =>
              AppliedHandicapDisplay.Placed(spot, side, weakerName)
            case None =>
              AppliedHandicapDisplay.Unresolved(spot)
          }
        case None =>
          AppliedHandicapDisplay.Unresolved(spot)
      }
    }

  def forMatch(
      context: BracketHandicapContext,
      matchDef: BracketMatch
  ): AppliedHandicapDisplay =
    appliedHandicapDisplay(
      matchDef,
      MatchHandicapPreview.forMatch(context, matchDef)
    )

  private def weakerSide(
      matchDef: BracketMatch,
      weakerName: String
  ): Option[AppliedHandicapSide] =
    (matchDef.playerA, matchDef.playerB) match {
      case (Some(a), _) if a.name == weakerName =>
        Some(AppliedHandicapSide.PlayerA)
      case (_, Some(b)) if b.name == weakerName =>
        Some(AppliedHandicapSide.PlayerB)
      case _ => None
    }

  def panelStatusMessage(
      display: AppliedHandicapDisplay,
      completed: Boolean
  ): Option[String] =
    display match {
      case AppliedHandicapDisplay.Hidden                      => None
      case AppliedHandicapDisplay.Placed(spot, _, weakerName) =>
        Some(
          if (completed) s"Handicap was +$spot to $weakerName"
          else s"Handicap applied: +$spot to $weakerName"
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        Some(
          if (completed)
            s"Handicap was +$spot — recipient unresolved; please file a bug."
          else
            s"Handicap applied: +$spot — recipient unresolved; please file a bug."
        )
    }

  def panelStatusIsError(display: AppliedHandicapDisplay): Boolean =
    display match {
      case AppliedHandicapDisplay.Unresolved(_) => true
      case _                                    => false
    }
}
