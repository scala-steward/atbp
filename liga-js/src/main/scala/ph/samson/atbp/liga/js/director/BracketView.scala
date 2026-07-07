package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

/** Bracket tree for the director console. */
object BracketView {

  def apply(
      bracket: Bracket,
      selectedMatchId: Signal[Option[String]],
      onSelect: Observer[String]
  ): Div = {
    val groups = BracketLayout.groupMatches(bracket.matches)
    div(
      cls := "bracket",
      groups.map { group =>
        div(
          cls := "bracket-section",
          h3(s"${group.section.label} — round ${group.round}"),
          div(
            cls := "round-matches",
            group.matches.map { matchDef =>
              matchRow(matchDef, selectedMatchId, onSelect)
            }
          )
        )
      }
    )
  }

  private def matchRow(
      matchDef: BracketMatch,
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
      onClick.mapTo(matchDef.id) --> onSelect,
      span(cls := "match-id", matchDef.id),
      span(
        cls := "match-players",
        s"${BracketLayout.playerLabel(matchDef.playerA)} vs ${BracketLayout.playerLabel(matchDef.playerB)}"
      ),
      span(
        cls := "match-state",
        BracketLayout.stateLabel(matchDef.state)
      ),
      matchDef.result.map { result =>
        span(cls := "match-score", s"${result.scoreA}–${result.scoreB}")
      }
    )
  }
}
