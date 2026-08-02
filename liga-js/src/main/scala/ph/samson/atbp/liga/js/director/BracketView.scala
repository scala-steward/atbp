package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

/** Bracket tree for the director console. */
object BracketView {

  def apply(
      bracket: Bracket,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      selectedMatchId: Signal[Option[String]],
      onSelect: Observer[String]
  ): Div = {
    val raceToByScope = handicapContext.raceToByScope
    val groups = BracketLayout.groupMatches(bracket.matches, bracket.size)
    div(
      cls := "bracket",
      groups.map { group =>
        div(
          cls := "bracket-section",
          h3(
            cls := {
              if (
                RaceToLabels.roundHeaderIsError(
                  group.section,
                  group.round,
                  raceToByScope
                )
              ) "race-to-error"
              else ""
            },
            RaceToLabels
              .roundHeaderLabel(group.section, group.round, raceToByScope)
          ),
          div(
            cls := "round-matches",
            group.matches.map { matchDef =>
              matchRow(
                handicapContext,
                resultsContext,
                matchDef,
                bracket.size,
                selectedMatchId,
                onSelect
              )
            }
          )
        )
      }
    )
  }

  private def matchRow(
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      matchDef: BracketMatch,
      bracketSize: Int,
      selectedMatchId: Signal[Option[String]],
      onSelect: Observer[String]
  ): Div = {
    val isSelected = selectedMatchId.map(_.contains(matchDef.id))
    val isActive = BracketLayout.isActionable(matchDef)
    div(
      cls <-- isSelected.map { selected =>
        val base = List("match-row")
        val withSelected = if (selected) base :+ "selected" else base
        if (isActive) (withSelected :+ "actionable").mkString(" ")
        else withSelected.mkString(" ")
      },
      title := (if (isActive) {
                  "Needs director action"
                } else {
                  BracketLayout.matchLabel(matchDef.id, bracketSize)
                }),
      onClick.mapTo(matchDef.id) --> onSelect,
      span(
        cls := "match-id",
        BracketLayout.matchLabel(matchDef.id, bracketSize)
      ),
      span(
        cls := "match-players",
        AppliedHandicapView.playersWithAppliedHandicap(
          matchDef,
          handicapContext,
          resultsContext,
          AppliedHandicapLabels.forMatch(handicapContext, matchDef),
          BracketLayout.winnerSide(matchDef)
        )
      ),
      span(
        cls := "match-state",
        BracketLayout.stateLabel(matchDef.state)
      ),
      MatchScoreView(matchDef)
    )
  }
}
