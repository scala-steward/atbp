package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.model.*

final case class UnknownPlayer(name: String)
    extends Exception(s"Unknown player: $name")

object HandicapRenderer {

  def suggest(
      ratings: List[PlayerRating],
      playerA: String,
      playerB: String,
      raceTo: Int
  ): Either[UnknownPlayer, HandicapSuggestion] =
    for {
      a <- findPlayer(ratings, playerA)
      b <- findPlayer(ratings, playerB)
    } yield Handicap.suggest(a, b, raceTo)

  def render(suggestion: HandicapSuggestion): String = {
    val header = "| Weaker player | Handicap | Race-to |"
    val separator = "| --- | ---: | ---: |"
    val row =
      s"| ${suggestion.weakerPlayer.name} | ${suggestion.handicap} | ${suggestion.raceTo} |"
    (header :: separator :: row :: Nil).mkString("\n") + "\n"
  }

  private def findPlayer(
      ratings: List[PlayerRating],
      name: String
  ): Either[UnknownPlayer, PlayerRating] =
    ratings.find(_.player.name == name).toRight(UnknownPlayer(name))
}
