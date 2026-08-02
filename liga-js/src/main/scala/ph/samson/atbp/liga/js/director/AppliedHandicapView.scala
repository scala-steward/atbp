package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide
import ph.samson.atbp.liga.js.director.BracketLayout.MatchWinnerSide
import ph.samson.atbp.liga.js.director.BracketResults.RatingMovementDisplay
import ph.samson.atbp.liga.js.director.BracketResults.ResultsCellDisplay
import ph.samson.atbp.liga.model.PlayerRatingLabel

/** Laminar rendering for applied-handicap bracket row labels. */
object AppliedHandicapView {

  def playersWithAppliedHandicap(
      matchDef: BracketMatch,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      display: AppliedHandicapDisplay,
      winner: Option[MatchWinnerSide]
  ): HtmlElement = {
    val a = BracketLayout.playerLabel(matchDef.playerA)
    val b = BracketLayout.playerLabel(matchDef.playerB)
    display match {
      case AppliedHandicapDisplay.Hidden =>
        span(
          playerCell(
            a,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.A)
          ),
          " vs ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B)
          )
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerA,
            _
          ) =>
        span(
          playerCell(
            a,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.A)
          ),
          " ",
          span(cls := "match-vs-handicap", s"(+$spot) vs"),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B)
          )
        )
      case AppliedHandicapDisplay.Placed(
            spot,
            AppliedHandicapSide.PlayerB,
            _
          ) =>
        span(
          playerCell(
            a,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.A)
          ),
          " ",
          span(cls := "match-vs-handicap", s"vs (+$spot)"),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B)
          )
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        span(
          playerCell(
            a,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.A)
          ),
          " ",
          span(cls := "race-to-error", s"(+$spot) unresolved"),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B)
          )
        )
    }
  }

  private def playerCell(
      name: String,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      matchId: String,
      isWinner: Boolean
  ): HtmlElement = {
    val frozenLabel = handicapContext.ratingLabelFor(name)
    val emphasis = resultsContext.nameEmphasis(name, matchId)
    val nameCls = BracketResults.nameClasses(emphasis, isWinner)
    resultsContext.cellDisplay(name, matchId, frozenLabel) match {
      case ResultsCellDisplay.Annotate(wins, losses, movement) =>
        val nameWithRecord = s"$name ($wins-$losses)"
        span(
          cls := "player-cell",
          if (nameCls.nonEmpty) span(cls := nameCls, nameWithRecord)
          else span(nameWithRecord),
          ratingMovementElement(movement)
        )
      case ResultsCellDisplay.Skip =>
        span(
          cls := "player-cell",
          if (nameCls.nonEmpty) span(cls := nameCls, name) else span(name),
          frozenLabel.map(liveRatingElement)
        )
    }
  }

  private def liveRatingElement(label: PlayerRatingLabel): HtmlElement =
    label match {
      case PlayerRatingLabel.Rated(r) =>
        span(cls := "player-rating", f"${r}%.0f")
      case PlayerRatingLabel.Unrated =>
        span(cls := "player-rating", "unrated")
    }

  private def ratingMovementElement(
      movement: RatingMovementDisplay
  ): HtmlElement =
    movement match {
      case RatingMovementDisplay.RatedDelta(frozen, delta) =>
        span(
          cls := List(
            "player-rating",
            BracketResults.ratedDeltaCssClasses(delta).mkString(" ")
          ).filter(_.nonEmpty).mkString(" "),
          BracketResults.formatRatedDeltaLine(frozen, delta)
        )
      case RatingMovementDisplay.NewRating(post) =>
        span(
          cls := "player-rating rating-new",
          BracketResults.formatNewRatingLine(post)
        )
    }
}
