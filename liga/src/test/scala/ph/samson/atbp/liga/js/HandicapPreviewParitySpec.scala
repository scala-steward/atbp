package ph.samson.atbp.liga.js

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.model.*
import zio.test.*

/** JVM parity checks for client-side handicap preview (mirrors `Handicap`). */
object HandicapPreviewParitySpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def rating(player: Player, rating: Double, rd: Double): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  def spec = suite("HandicapPreviewParity")(
    test("equal ratings suggest handicap 0") {
      val a = rating(alice, 1500, rd = 100)
      val b = rating(bob, 1500, rd = 100)
      val suggestion = Handicap.suggest(a, b, raceTo = 7)
      assertTrue(
        suggestion.handicap == 0,
        suggestion.weakerPlayer == alice
      )
    },
    test("large rating gap hits the cap") {
      val a = rating(alice, 2200, rd = 50)
      val b = rating(bob, 1100, rd = 50)
      val suggestion = Handicap.suggest(a, b, raceTo = 7)
      assertTrue(
        suggestion.weakerPlayer == bob,
        suggestion.handicap == 5
      )
    },
    test("weaker player is lower rating") {
      val a = rating(alice, 1700, rd = 80)
      val b = rating(bob, 1450, rd = 90)
      val suggestion = Handicap.suggest(a, b, raceTo = 7)
      assertTrue(suggestion.weakerPlayer == bob)
    }
  )
}
