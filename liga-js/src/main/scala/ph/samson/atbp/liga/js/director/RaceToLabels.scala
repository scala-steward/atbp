package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.Section

/** Composes bracket race-to resolve results with director-facing copy. */
object RaceToLabels {

  def roundHeaderLabel(
      section: Section,
      round: Int,
      raceToByScope: Map[String, Int],
      seRounds: Int
  ): String = {
    val group = BracketLayout.groupLabel(section, round, seRounds)
    BracketLayout.roundRaceToScope(section, round, raceToByScope) match {
      case Right(n)    => s"$group · ${BracketLayout.raceToLabel(n)}"
      case Left(scope) =>
        s"$group · ${DirectorGuidance.missingRaceToBugHint(scope)}"
    }
  }

  def roundHeaderIsError(
      section: Section,
      round: Int,
      raceToByScope: Map[String, Int]
  ): Boolean =
    BracketLayout.roundRaceToScope(section, round, raceToByScope).isLeft

  def matchRaceToLabel(
      matchDef: BracketMatch,
      raceToByScope: Map[String, Int]
  ): Either[String, String] =
    BracketLayout.matchRaceToScope(matchDef, raceToByScope) match {
      case Right(n)    => Right(BracketLayout.raceToLabel(n))
      case Left(scope) => Left(DirectorGuidance.missingRaceToBugHint(scope))
    }
}
