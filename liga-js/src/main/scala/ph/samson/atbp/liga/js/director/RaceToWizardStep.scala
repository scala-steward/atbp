package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.BracketPreview
import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.RaceToWizard
import ph.samson.atbp.liga.bracket.SingleElimRoundNames
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
        scopes.exists(scope => RaceToScopes.sectionOf(scope) == section)
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

  def formatRoundSummary(
      round: BracketPreview.RoundCounts,
      section: RaceToScopes.Section,
      seRounds: Int
  ): String = {
    val byePart = if (round.byes > 0) s", ${round.byes} byes" else ""
    val matchWord = if (round.matches == 1) "match" else "matches"
    val roundName = section match {
      case RaceToScopes.Section.SingleElimination =>
        SingleElimRoundNames.name(round.round, seRounds)
      case _ =>
        s"Round ${round.round}"
    }
    s"$roundName: ${round.players} players, ${round.matches} $matchWord$byePart"
  }

  def resetGrandFinalHint(
      section: BracketPreview.SectionPreview
  ): Option[String] =
    Option.when(section.resetGrandFinalPossible)(
      "Grand Final reset possible if losers bracket wins."
    )

  /** Label text for a race-to input: preview round summary, or `Grand Final`
    * for `gf`.
    */
  def inputLabel(
      scope: String,
      preview: BracketPreview.Preview,
      seRounds: Int
  ): String =
    if (scope == RaceToScopes.grandFinalScopeKey) {
      "Grand Final"
    } else {
      val section = RaceToScopes.sectionOf(scope)
      val roundNum = scope.split("-", 2)(1).toInt
      val sectionPreview =
        preview.sections.find(_.section == section).get
      val round = sectionPreview.rounds.find(_.round == roundNum).get
      formatRoundSummary(round, section, seRounds)
    }
}
