package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.model.TournamentState

/** Wizard / runtime phase derived from replay state. */
enum TournamentPhase {
  case None, Defining, Locked, RaceTo, Active, Completed
}

object TournamentPhase {

  def derive(state: TournamentState, hasDir: Boolean): TournamentPhase =
    // `Locked` means roster locked but race-to not yet configured for every
    // round; `RaceTo` means race-to is complete and the bracket can be seeded.
    if (!hasDir) {
      TournamentPhase.None
    } else if (state.completed) {
      TournamentPhase.Completed
    } else if (state.bracket.nonEmpty) {
      TournamentPhase.Active
    } else if (!state.playersLocked) {
      TournamentPhase.Defining
    } else if (!raceToComplete(state)) {
      TournamentPhase.Locked
    } else {
      TournamentPhase.RaceTo
    }

  def raceToComplete(state: TournamentState): Boolean = {
    val required = RaceToScopes.requiredKeys(state.players.size)
    required.forall(scope => state.raceToByScope.contains(scope))
  }
}
