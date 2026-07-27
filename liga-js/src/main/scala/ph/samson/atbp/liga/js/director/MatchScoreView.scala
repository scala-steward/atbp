package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.director.BracketLayout.MatchWinnerSide

/** Shared Laminar rendering for bracket row scores. */
object MatchScoreView {

  def apply(matchDef: BracketMatch): Option[Node] =
    if (matchDef.isBye) {
      BracketLayout
        .resultLabel(matchDef)
        .map(label => span(cls := "match-bye", label))
    } else {
      matchDef.result.map { result =>
        val winner = BracketLayout.winnerSide(matchDef)
        span(
          cls := "match-score",
          scoreNumber(result.scoreA, winner.contains(MatchWinnerSide.A)),
          "–",
          scoreNumber(result.scoreB, winner.contains(MatchWinnerSide.B))
        )
      }
    }

  private def scoreNumber(score: Int, isWinner: Boolean): Node =
    if (isWinner) span(cls := "match-winner", score.toString)
    else span(score.toString)
}
