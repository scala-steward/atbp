package ph.samson.atbp.liga.js.audience

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.AppliedHandicapLabels
import ph.samson.atbp.liga.js.director.AppliedHandicapView
import ph.samson.atbp.liga.js.director.BracketHandicapContext
import ph.samson.atbp.liga.js.director.BracketLayout
import ph.samson.atbp.liga.js.director.BracketResultsContext
import ph.samson.atbp.liga.js.director.MatchScoreView
import ph.samson.atbp.liga.js.director.RaceToLabels

/** Read-only bracket display for the audience screen. */
object AudienceBracketView {

  def apply(
      bracket: Bracket,
      seRounds: Int,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext
  ): Div = {
    val raceToByScope = handicapContext.raceToByScope
    val groups = BracketLayout.groupMatches(bracket.matches, bracket.size)
    div(
      cls := "audience-bracket",
      groups.map { group =>
        div(
          cls := "bracket-section",
          h2(
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
              .roundHeaderLabel(
                group.section,
                group.round,
                raceToByScope,
                seRounds
              )
          ),
          div(
            cls := "round-matches",
            group.matches.map(matchRow(handicapContext, resultsContext, _))
          )
        )
      }
    )
  }

  private def matchRow(
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      matchDef: BracketMatch
  ): Div = {
    val isLive = matchDef.state == BracketMatchState.Started
    val isReady = matchDef.state == BracketMatchState.Ready
    div(
      cls := {
        val base = List("match-row")
        val withLive = if (isLive) base :+ "live" else base
        if (isReady) (withLive :+ "ready").mkString(" ")
        else withLive.mkString(" ")
      },
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
      span(cls := "match-state", BracketLayout.stateLabel(matchDef.state)),
      MatchScoreView(matchDef)
    )
  }
}
