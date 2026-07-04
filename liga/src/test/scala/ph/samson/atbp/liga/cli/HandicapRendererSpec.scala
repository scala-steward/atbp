package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.handicap.WinProbability
import ph.samson.atbp.liga.model.*
import zio.test.*

object HandicapRendererSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def rating(player: Player, rating: Double, rd: Double): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  def spec = suite("HandicapRenderer")(
    test("renders weaker player, race-to, and win probabilities by handicap") {
      val weaker = rating(bob, 1450, 90)
      val stronger = rating(alice, 1700, 80)
      val suggestion = HandicapSuggestion(bob, handicap = 3, raceTo = 7)
      val result = HandicapResult(weaker, stronger, suggestion)
      val handicaps = List(0, 2, 3, 4)
      val probabilities = handicaps.map { handicap =>
        WinProbability.matchWinProbability(
          weaker,
          stronger,
          raceTo = 7,
          handicap = handicap
        )
      }
      val probabilityCells =
        probabilities.map(p => f"${p * 100}%.1f%%").mkString(" | ")
      val rendered = HandicapRenderer.render(result)
      assertTrue(
        rendered ==
          s"""| Weaker player | Race-to | + 0 | + 2 | + 3 | + 4 |
             #| --- | ---: | ---: | ---: | ---: | ---: |
             #| Bob | 7 | $probabilityCells |
             #""".stripMargin('#')
      )
    },
    test("unknown player name returns clear error") {
      val ratings = List(rating(alice, 1700, 100))
      val result = HandicapRenderer.suggest(ratings, "Alice", "Eve", raceTo = 7)
      assertTrue(
        result.isLeft,
        result.left.exists(_.getMessage == "Unknown player: Eve")
      )
    },
    test("suggest uses period-end ratings from discovered snapshot") {
      val ratings = List(
        rating(alice, 1700, 80),
        rating(bob, 1450, 90)
      )
      val result = HandicapRenderer.suggest(ratings, "Alice", "Bob", raceTo = 7)
      assertTrue(
        result.isRight,
        result.exists(_.suggestion.weakerPlayer == bob),
        result.exists(_.suggestion.raceTo == 7)
      )
    }
  )
}
