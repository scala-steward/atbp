package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.handicap.WinProbability
import ph.samson.atbp.liga.model.*

final case class UnknownPlayer(name: String)
    extends Exception(s"Unknown player: $name")

final case class HandicapResult(
    weaker: PlayerRating,
    stronger: PlayerRating,
    suggestion: HandicapSuggestion
)

object HandicapRenderer {

  def suggest(
      ratings: List[PlayerRating],
      playerA: String,
      playerB: String,
      raceTo: Int
  ): Either[UnknownPlayer, HandicapResult] =
    for {
      a <- findPlayer(ratings, playerA)
      b <- findPlayer(ratings, playerB)
      suggestion = Handicap.suggest(a, b, raceTo)
      (weaker, stronger) =
        if (suggestion.weakerPlayer == a.player) (a, b) else (b, a)
    } yield HandicapResult(weaker, stronger, suggestion)

  def render(result: HandicapResult): String = {
    val suggestion = result.suggestion
    val suggested = suggestion.handicap
    val handicaps =
      List(0, math.max(0, suggested - 1), suggested, suggested + 1)
    val probabilities = handicaps.map(winProbability(result, _))
    val handicapHeaders = handicaps.map(formatSpotHeader).mkString(" | ")
    val probabilityCells = probabilities.map(formatProbability).mkString(" | ")
    val header = s"| Weaker player | Race-to | $handicapHeaders |"
    val separator =
      s"| --- | ---: | ${handicaps.map(_ => "---:").mkString(" | ")} |"
    val row =
      s"| ${suggestion.weakerPlayer.name} | ${suggestion.raceTo} | $probabilityCells |"
    (header :: separator :: row :: Nil).mkString("\n") + "\n"
  }

  private def winProbability(result: HandicapResult, handicap: Int): Double =
    WinProbability.matchWinProbability(
      result.weaker,
      result.stronger,
      result.suggestion.raceTo,
      handicap
    )

  private def formatSpotHeader(handicap: Int): String =
    s"+ $handicap"

  private def formatProbability(probability: Double): String =
    f"${probability * 100}%.1f%%"

  private def findPlayer(
      ratings: List[PlayerRating],
      name: String
  ): Either[UnknownPlayer, PlayerRating] =
    ratings.find(_.player.name == name).toRight(UnknownPlayer(name))
}
