package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide
import ph.samson.atbp.liga.js.director.BracketLayout.MatchWinnerSide

/** Laminar rendering for applied-handicap bracket row labels. */
object AppliedHandicapView {

  def playersWithAppliedHandicap(
      matchDef: BracketMatch,
      display: AppliedHandicapDisplay,
      winner: Option[MatchWinnerSide]
  ): HtmlElement = {
    val a = BracketLayout.playerLabel(matchDef.playerA)
    val b = BracketLayout.playerLabel(matchDef.playerB)
    display match {
      case AppliedHandicapDisplay.Hidden =>
        span(
          playerName(a, winner.contains(MatchWinnerSide.A)),
          " vs ",
          playerName(b, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerA,
            _
          ) =>
        span(
          playerName(a, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "match-vs-handicap", s"(+$spot) vs"),
          " ",
          playerName(b, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerB,
            _
          ) =>
        span(
          playerName(a, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "match-vs-handicap", s"vs (+$spot)"),
          " ",
          playerName(b, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        span(
          playerName(a, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "race-to-error", s"(+$spot) unresolved"),
          " ",
          playerName(b, winner.contains(MatchWinnerSide.B))
        )
    }
  }

  private def playerName(name: String, isWinner: Boolean): Node =
    if (isWinner) span(cls := "match-winner", name) else span(name)
}
