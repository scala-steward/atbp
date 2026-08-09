package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

import java.time.Instant

/** Bracket tree for the director console. */
object BracketView {

  private val ElapsedTickMs = 15_000

  def apply(
      bracket: Bracket,
      seRounds: Int,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      selectedMatchId: Signal[Option[String]],
      onSelect: Observer[String]
  ): Div = {
    val raceToByScope = handicapContext.raceToByScope
    val sections =
      BracketLayout.directorGroupMatches(bracket.matches, bracket.size)
    val now = Var(Instant.now())
    val elapsedTicks = EventStream.periodic(intervalMs = ElapsedTickMs)
    div(
      elapsedTicks --> Observer(_ => now.set(Instant.now())),
      cls := "bracket",
      if (sections.liveStrip.nonEmpty) {
        div(
          cls := "bracket-section live-strip",
          h3(cls := "live-strip-header", "Live"),
          div(
            cls := "round-matches",
            sections.liveStrip.map { matchDef =>
              matchRow(
                handicapContext,
                resultsContext,
                matchDef,
                bracket.size,
                seRounds,
                selectedMatchId,
                onSelect,
                now.signal
              )
            }
          )
        )
      } else {
        emptyNode
      },
      if (sections.readyStrip.nonEmpty) {
        div(
          cls := "bracket-section ready-strip",
          h3(cls := "ready-strip-header", "Ready — longest wait first"),
          div(
            cls := "round-matches",
            sections.readyStrip.map { matchDef =>
              matchRow(
                handicapContext,
                resultsContext,
                matchDef,
                bracket.size,
                seRounds,
                selectedMatchId,
                onSelect,
                now.signal
              )
            }
          )
        )
      } else {
        emptyNode
      },
      sections.groups.map { group =>
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
              .roundHeaderLabel(
                group.section,
                group.round,
                raceToByScope,
                seRounds
              )
          ),
          div(
            cls := "round-matches",
            group.matches.map { matchDef =>
              matchRow(
                handicapContext,
                resultsContext,
                matchDef,
                bracket.size,
                seRounds,
                selectedMatchId,
                onSelect,
                now.signal
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
      seRounds: Int,
      selectedMatchId: Signal[Option[String]],
      onSelect: Observer[String],
      now: Signal[Instant]
  ): Div = {
    val isSelected = selectedMatchId.map(_.contains(matchDef.id))
    val isActive = BracketLayout.isActionable(matchDef)
    val timingChips =
      if (
        matchDef.state == BracketMatchState.Completed ||
        matchDef.state == BracketMatchState.Started
      ) {
        Signal.fromValue(BracketLayout.timingChipTexts(matchDef, Instant.EPOCH))
      } else {
        now.map(BracketLayout.timingChipTexts(matchDef, _))
      }
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
                  BracketLayout.matchLabel(matchDef.id, bracketSize, seRounds)
                }),
      onClick.mapTo(matchDef.id) --> onSelect,
      span(
        cls := "match-id",
        BracketLayout.matchLabel(matchDef.id, bracketSize, seRounds)
      ),
      span(
        cls := "match-players",
        AppliedHandicapView.playersWithAppliedHandicap(
          matchDef,
          handicapContext,
          resultsContext,
          AppliedHandicapLabels.forMatch(handicapContext, matchDef),
          BracketLayout.winnerSide(matchDef),
          showRatings = true,
          inlineMiddle = None
        )
      ),
      child <-- timingChips.map { chips =>
        if (chips.isEmpty) {
          emptyNode
        } else {
          span(
            cls := "match-timing-chips",
            chips.map(text => span(cls := "match-elapsed", text))
          )
        }
      },
      span(
        cls := "match-state",
        BracketLayout.stateLabel(matchDef.state)
      ),
      MatchScoreView(matchDef)
    )
  }
}
