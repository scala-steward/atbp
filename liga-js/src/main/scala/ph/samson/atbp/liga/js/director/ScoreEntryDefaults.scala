package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.MatchResult
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide

/** Default score floors and blur clamp for director MatchPanel score entry. */
object ScoreEntryDefaults {

  final case class ScoreFloors(scoreA: Int, scoreB: Int)

  def initialScoreStrings(
      display: AppliedHandicapDisplay,
      result: Option[MatchResult]
  ): (String, String) =
    result match {
      case Some(r) => (r.scoreA.toString, r.scoreB.toString)
      case None    =>
        val f = floorsFromDisplay(display)
        (f.scoreA.toString, f.scoreB.toString)
    }

  def floorsFromDisplay(display: AppliedHandicapDisplay): ScoreFloors =
    display match {
      case AppliedHandicapDisplay.Placed(spot, side, _) =>
        floors(spot, Some(side))
      case AppliedHandicapDisplay.Hidden |
          AppliedHandicapDisplay.Unresolved(_) =>
        floors(0, None)
    }

  def floors(
      handicapApplied: Int,
      spotted: Option[AppliedHandicapSide]
  ): ScoreFloors =
    spotted match {
      case Some(AppliedHandicapSide.PlayerA) =>
        ScoreFloors(scoreA = handicapApplied, scoreB = 0)
      case Some(AppliedHandicapSide.PlayerB) =>
        ScoreFloors(scoreA = 0, scoreB = handicapApplied)
      case None =>
        ScoreFloors(scoreA = 0, scoreB = 0)
    }

  /** Snap score input to floor on blur; mid-edit values below floor are allowed
    * until blur.
    */
  def clampOnBlur(raw: String, floor: Int): String =
    raw.toIntOption match {
      case Some(n) if n < floor => floor.toString
      case Some(n)              => n.toString
      case None                 => floor.toString
    }

  /** Clamp both sides before submit so Record result does not bypass blur. */
  def clampPair(
      rawA: String,
      rawB: String,
      floors: ScoreFloors
  ): (String, String) =
    (clampOnBlur(rawA, floors.scoreA), clampOnBlur(rawB, floors.scoreB))
}
