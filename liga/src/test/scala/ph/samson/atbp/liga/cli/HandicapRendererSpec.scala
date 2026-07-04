package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.model.*
import zio.test.*

object HandicapRendererSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def rating(player: Player, rating: Double, rd: Double): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  def spec = suite("HandicapRenderer")(
    test("renders weaker player, suggested handicap, and race-to") {
      val suggestion = HandicapSuggestion(bob, handicap = 3, raceTo = 7)
      val rendered = HandicapRenderer.render(suggestion)
      assertTrue(
        rendered ==
          """| Weaker player | Handicap | Race-to |
             #| --- | ---: | ---: |
             #| Bob | 3 | 7 |
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
        result.exists(_.weakerPlayer == bob),
        result.exists(_.raceTo == 7)
      )
    }
  )
}
