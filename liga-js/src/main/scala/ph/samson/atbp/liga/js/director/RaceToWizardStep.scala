package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.BracketPreview
import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.RaceToWizard
import ph.samson.atbp.liga.bracket.TopN
import ph.samson.atbp.liga.js.api.Models.*

/** Pure logic for the director race-to wizard step (Top N + scopes + preview).
  */
object RaceToWizardStep {

  def initialState(tournament: TournamentResponse): RaceToWizard.State = {
    val playerCount = tournament.players.size
    if (tournament.raceToByScope.nonEmpty) {
      RaceToWizard.loadState(
        tournament.raceToByScope,
        playerCount,
        tournament.topN
      )
    } else {
      RaceToWizard.initialState(playerCount)
    }
  }

  def legalTopNOptions(playerCount: Int): List[Int] =
    TopN.legalTopNs(playerCount)

  def requiredScopes(playerCount: Int, topN: Int): List[String] =
    RaceToScopes.requiredKeys(playerCount, topN)

  def visibleSections(
      playerCount: Int,
      topN: Int
  ): List[RaceToScopes.Section] = {
    val scopes = requiredScopes(playerCount, topN)
    RaceToScopes.Section.values.toList
      .filter(section =>
        scopes
          .exists(scope => RaceToScopes.scopeLabel(scope).section == section)
      )
      .sortBy(_.order)
  }

  def preview(playerCount: Int, topN: Int): BracketPreview.Preview =
    BracketPreview(playerCount, topN)

  def changeTopN(
      state: RaceToWizard.State,
      newTopN: Int,
      playerCount: Int
  ): RaceToWizard.State =
    RaceToWizard.changeTopN(state, newTopN, playerCount)

  def saveRequest(state: RaceToWizard.State): RaceToRequest =
    RaceToRequest(state.topN, state.raceToByScope)

  def formatRoundSummary(round: BracketPreview.RoundCounts): String = {
    val byePart = if (round.byes > 0) s", ${round.byes} byes" else ""
    s"Round ${round.round}: ${round.players} players, ${round.matches} matches$byePart"
  }

  def resetGrandFinalHint(
      section: BracketPreview.SectionPreview
  ): Option[String] =
    Option.when(section.resetGrandFinalPossible)(
      "Grand Final reset possible if losers bracket wins."
    )
}
