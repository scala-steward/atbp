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
      winner: Option[MatchWinnerSide],
      showRatings: Boolean,
      inlineMiddle: Option[Node]
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
            winner.contains(MatchWinnerSide.A),
            showRatings
          ),
          playersSeparator(inlineMiddle, " vs "),
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B),
            showRatings
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
            winner.contains(MatchWinnerSide.A),
            showRatings
          ),
          " ",
          handicapSeparator(inlineMiddle, spot, AppliedHandicapSide.PlayerA),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B),
            showRatings
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
            winner.contains(MatchWinnerSide.A),
            showRatings
          ),
          " ",
          handicapSeparator(inlineMiddle, spot, AppliedHandicapSide.PlayerB),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B),
            showRatings
          )
        )
      case AppliedHandicapDisplay.Unresolved(spot) =>
        span(
          playerCell(
            a,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.A),
            showRatings
          ),
          " ",
          span(cls := "race-to-error", s"(+$spot) unresolved"),
          " ",
          playerCell(
            b,
            handicapContext,
            resultsContext,
            matchDef.id,
            winner.contains(MatchWinnerSide.B),
            showRatings
          )
        )
    }
  }

  def playerLabelText(
      name: String,
      cellDisplay: ResultsCellDisplay,
      showRatings: Boolean
  ): String =
    cellDisplay match {
      case ResultsCellDisplay.Annotate(wins, losses, _) if showRatings =>
        s"$name ($wins-$losses)"
      case ResultsCellDisplay.Annotate(_, _, _) | ResultsCellDisplay.Skip =>
        name
    }

  def includesRatingSubline(
      cellDisplay: ResultsCellDisplay,
      frozenLabel: Option[PlayerRatingLabel],
      showRatings: Boolean
  ): Boolean =
    showRatings && (cellDisplay match {
      case ResultsCellDisplay.Annotate(_, _, _) => true
      case ResultsCellDisplay.Skip              => frozenLabel.isDefined
    })

  private def playersSeparator(
      inlineMiddle: Option[Node],
      vsText: String
  ): Node =
    inlineMiddle match {
      case Some(middle) => span(" ", middle, " ")
      case None         => vsText
    }

  private def handicapSeparator(
      inlineMiddle: Option[Node],
      spot: Int,
      side: AppliedHandicapSide
  ): Node =
    inlineMiddle match {
      case Some(middle) =>
        side match {
          case AppliedHandicapSide.PlayerA =>
            span(
              span(cls := "match-vs-handicap", s"(+$spot)"),
              " ",
              middle
            )
          case AppliedHandicapSide.PlayerB =>
            span(
              middle,
              " ",
              span(cls := "match-vs-handicap", s"(+$spot)")
            )
        }
      case None =>
        side match {
          case AppliedHandicapSide.PlayerA =>
            span(cls := "match-vs-handicap", s"(+$spot) vs")
          case AppliedHandicapSide.PlayerB =>
            span(cls := "match-vs-handicap", s"vs (+$spot)")
        }
    }

  private def playerCell(
      name: String,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      matchId: String,
      isWinner: Boolean,
      showRatings: Boolean
  ): HtmlElement = {
    val frozenLabel = handicapContext.ratingLabelFor(name)
    val emphasis = resultsContext.nameEmphasis(name, matchId)
    val nameCls = BracketResults.nameClasses(emphasis, isWinner)
    val cellDisplay = resultsContext.cellDisplay(name, matchId, frozenLabel)
    val displayName = playerLabelText(name, cellDisplay, showRatings)
    cellDisplay match {
      case ResultsCellDisplay.Annotate(_, _, movement) =>
        span(
          cls := "player-cell",
          if (nameCls.nonEmpty) span(cls := nameCls, displayName)
          else span(displayName),
          if (includesRatingSubline(cellDisplay, frozenLabel, showRatings)) {
            ratingMovementElement(
              movement,
              resultsContext.earnedFor(name, matchId)
            )
          } else {
            emptyNode
          }
        )
      case ResultsCellDisplay.Skip =>
        span(
          cls := "player-cell",
          if (nameCls.nonEmpty) span(cls := nameCls, displayName)
          else span(displayName),
          if (includesRatingSubline(cellDisplay, frozenLabel, showRatings)) {
            liveRatingElement(
              frozenLabel.get,
              resultsContext.earnedFor(name, matchId)
            )
          } else {
            emptyNode
          }
        )
    }
  }

  private def liveRatingElement(
      label: PlayerRatingLabel,
      earned: Option[(Int, Int)]
  ): HtmlElement =
    span(
      cls := "player-rating",
      BracketEarnedRacks.formatLiveRatingLine(label, earned)
    )

  private def ratingMovementElement(
      movement: RatingMovementDisplay,
      earned: Option[(Int, Int)]
  ): HtmlElement =
    movement match {
      case RatingMovementDisplay.RatedDelta(frozen, delta) =>
        span(
          cls := List(
            "player-rating",
            BracketResults.ratedDeltaCssClasses(delta).mkString(" ")
          ).filter(_.nonEmpty).mkString(" "),
          BracketEarnedRacks.appendEarnedToMovementLine(
            BracketResults.formatRatedDeltaLine(frozen, delta),
            earned
          )
        )
      case RatingMovementDisplay.NewRating(post) =>
        span(
          cls := "player-rating rating-new",
          BracketEarnedRacks.appendEarnedToMovementLine(
            BracketResults.formatNewRatingLine(post),
            earned
          )
        )
    }
}
