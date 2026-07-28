package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide
import ph.samson.atbp.liga.js.director.BracketLayout.MatchWinnerSide
import ph.samson.atbp.liga.model.PlayerRatingLabel

/** Laminar rendering for applied-handicap bracket row labels. */
object AppliedHandicapView {

  def playersWithAppliedHandicap(
      matchDef: BracketMatch,
      handicapContext: BracketHandicapContext,
      display: AppliedHandicapDisplay,
      winner: Option[MatchWinnerSide]
  ): HtmlElement = {
    val a = BracketLayout.playerLabel(matchDef.playerA)
    val b = BracketLayout.playerLabel(matchDef.playerB)
    display match {
      case AppliedHandicapDisplay.Hidden =>
        span(
          playerCell(a, handicapContext, winner.contains(MatchWinnerSide.A)),
          " vs ",
          playerCell(b, handicapContext, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerA,
            _
          ) =>
        span(
          playerCell(a, handicapContext, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "match-vs-handicap", s"(+$spot) vs"),
          " ",
          playerCell(b, handicapContext, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerB,
            _
          ) =>
        span(
          playerCell(a, handicapContext, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "match-vs-handicap", s"vs (+$spot)"),
          " ",
          playerCell(b, handicapContext, winner.contains(MatchWinnerSide.B))
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        span(
          playerCell(a, handicapContext, winner.contains(MatchWinnerSide.A)),
          " ",
          span(cls := "race-to-error", s"(+$spot) unresolved"),
          " ",
          playerCell(b, handicapContext, winner.contains(MatchWinnerSide.B))
        )
    }
  }

  private def playerCell(
      name: String,
      handicapContext: BracketHandicapContext,
      isWinner: Boolean
  ): HtmlElement =
    span(
      cls := "player-cell",
      if (isWinner) span(cls := "match-winner", name) else span(name),
      handicapContext.ratingLabelFor(name).map {
        case PlayerRatingLabel.Rated(r) =>
          span(cls := "player-rating", f"${r}%.0f")
        case PlayerRatingLabel.Unrated =>
          span(cls := "player-rating", "unrated")
      }
    )
}
