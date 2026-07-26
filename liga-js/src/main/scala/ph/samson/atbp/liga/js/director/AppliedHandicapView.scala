package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide

/** Laminar rendering for applied-handicap bracket row labels. */
object AppliedHandicapView {

  def playersWithAppliedHandicap(
      matchDef: BracketMatch,
      display: AppliedHandicapDisplay
  ): HtmlElement = {
    val a = BracketLayout.playerLabel(matchDef.playerA)
    val b = BracketLayout.playerLabel(matchDef.playerB)
    display match {
      case AppliedHandicapDisplay.Hidden =>
        span(s"$a vs $b")
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerA,
            _
          ) =>
        span(
          a,
          " ",
          span(cls := "match-vs-handicap", s"(+$spot) vs"),
          " ",
          b
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerB,
            _
          ) =>
        span(
          a,
          " ",
          span(cls := "match-vs-handicap", s"vs (+$spot)"),
          " ",
          b
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        span(
          a,
          " ",
          span(cls := "race-to-error", s"(+$spot) unresolved"),
          " ",
          b
        )
    }
  }
}
