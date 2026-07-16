package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToWizard

/** Defaults for director actions when the server has no explicit config. */
object DirectorDefaults {

  def defaultRaceToByScope(playerCount: Int): Map[String, Int] =
    RaceToWizard.initialState(playerCount).raceToByScope
}
