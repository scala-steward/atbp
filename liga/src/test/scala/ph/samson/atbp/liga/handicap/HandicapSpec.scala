package ph.samson.atbp.liga.handicap

import ph.samson.atbp.liga.model.*
import zio.test.*

object HandicapSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def rating(player: Player, rating: Double, rd: Double): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  def spec = suite("Handicap")(
    test("cap never exceeds floor(0.75 × race-to)") {
      val a = rating(alice, 2000, rd = 100)
      val b = rating(bob, 1000, rd = 100)
      val suggestion = Handicap.suggest(a, b, raceTo = 7)
      assertTrue(suggestion.handicap <= (0.75 * 7).floor.toInt)
    },
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
    test("suggestion targets roughly 50% win probability for the weaker player") {
      val a = rating(alice, 1700, rd = 80)
      val b = rating(bob, 1450, rd = 90)
      val suggestion = Handicap.suggest(a, b, raceTo = 7)
      val weaker =
        if (suggestion.weakerPlayer == alice) a else b
      val stronger =
        if (suggestion.weakerPlayer == alice) b else a
      val probability = WinProbability.matchWinProbability(
        weaker,
        stronger,
        raceTo = 7,
        handicap = suggestion.handicap
      )
      assertTrue(
        suggestion.weakerPlayer == bob,
        Math.abs(probability - 0.5) <= 0.05
      )
    },
    test("weaker player tie-break uses lower RD then alphabetical name") {
      val a = rating(alice, 1500, rd = 80)
      val b = rating(bob, 1500, rd = 120)
      val suggestion = Handicap.suggest(a, b, raceTo = 5)
      assertTrue(suggestion.weakerPlayer == alice)
    },
    test("suggest is symmetric regardless of argument order") {
      val a = rating(alice, 1650, rd = 90)
      val b = rating(bob, 1500, rd = 100)
      val ab = Handicap.suggest(a, b, raceTo = 7)
      val ba = Handicap.suggest(b, a, raceTo = 7)
      assertTrue(
        ab.weakerPlayer == ba.weakerPlayer,
        ab.handicap == ba.handicap,
        ab.raceTo == ba.raceTo
      )
    }
  )

}
