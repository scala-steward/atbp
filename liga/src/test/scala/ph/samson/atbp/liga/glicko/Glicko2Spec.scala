package ph.samson.atbp.liga.glicko

import dimos.glicko2.Player as GlickoPlayer
import dimos.glicko2.Result
import ph.samson.atbp.liga.model.*
import zio.test.*

object Glicko2Spec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def approx(expected: Double, actual: Double): Boolean =
    Math.abs(expected - actual) <= 0.001

  def spec = suite("Glicko2")(
    suite("Tuning")(
      test("default parameters match spec") {
        val t = Tuning.Default
        assertTrue(
          t.initRating == 1500,
          t.maxDeviation == 350,
          t.initVolatility == 0.06,
          t.tau == 0.5
        )
      }
    ),
    suite("golden vectors")(
      test("Example 1 — win vs lower-rated established opponent") {
        val a = GlickoPlayer(1500, 350, 0.06)
        val b = GlickoPlayer(1400, 30, 0.06)
        val updated = a.afterPeriod(Seq(Result.WonAgainst(b)), Tuning.Default)
        assertTrue(
          approx(1631.369, updated.rating),
          approx(252.160, updated.deviation)
        )
      },
      test("Example 2 — win then loss in same rating period") {
        val a = GlickoPlayer(1631.368919949588, 252.16000623738702, 0.06)
        val c = GlickoPlayer(1550, 100, 0.06)
        val d = GlickoPlayer(1700, 300, 0.06)
        val updated = a.afterPeriod(
          Seq(Result.WonAgainst(c), Result.DefeatedBy(d)),
          Tuning.Default
        )
        assertTrue(
          approx(1644.372, updated.rating),
          approx(192.020, updated.deviation)
        )
      },
      test("wrapper updateAfterGame matches library for single game") {
        val state =
          Glicko2.snapshot(alice, rating = 1500, rd = 350) ++
            Glicko2.snapshot(bob, rating = 1400, rd = 30)
        val after = Glicko2.updateAfterGame(state, alice, bob, GameWinner.PlayerA)
        val a = Glicko2.ratingOf(after, alice)
        assertTrue(
          approx(1631.369, a.rating),
          approx(252.160, a.rd)
        )
      }
    ),
    suite("match updates")(
      test("7-4 expands to 11 games with correct W-L and winner rated higher") {
        val periodMatch = PeriodMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 7,
          scoreB = 4,
          raceTo = 7,
          handicapSuggested = 0,
          handicapApplied = 0
        )
        val after = Glicko2.updateAfterMatch(Glicko2.empty, periodMatch)
        val a = Glicko2.ratingOf(after, alice)
        val b = Glicko2.ratingOf(after, bob)
        assertTrue(
          a.wins == 7,
          a.losses == 4,
          b.wins == 4,
          b.losses == 7,
          a.rating > b.rating
        )
      },
      test("game order within match does not change ratings") {
        val periodMatch = PeriodMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 7,
          scoreB = 4,
          raceTo = 7,
          handicapSuggested = 0,
          handicapApplied = 0
        )
        val once = Glicko2.updateAfterMatch(Glicko2.empty, periodMatch)
        val twice = Glicko2.updateAfterMatch(Glicko2.empty, periodMatch)
        val aOnce = Glicko2.ratingOf(once, alice)
        val aTwice = Glicko2.ratingOf(twice, alice)
        assertTrue(
          approx(aOnce.rating, aTwice.rating),
          approx(aOnce.rd, aTwice.rd)
        )
      }
    ),
    suite("leaderboard")(
      test("returns players sorted by name for deterministic output") {
        val state = Map(
          bob -> PlayerRating(bob, 1600, 100, 5, 3),
          alice -> PlayerRating(alice, 1700, 90, 8, 2)
        )
        val board = Glicko2.leaderboard(state)
        assertTrue(
          board.map(_.player.name) == List("Alice", "Bob")
        )
      }
    ),
    suite("properties")(
      test("RD decreases or stays after games played") {
        val periodMatch = PeriodMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 5,
          scoreB = 3,
          raceTo = 5,
          handicapSuggested = 0,
          handicapApplied = 0
        )
        val before = Glicko2.newPlayerRating(alice)
        val after = Glicko2.ratingOf(Glicko2.updateAfterMatch(Glicko2.empty, periodMatch), alice)
        assertTrue(after.rd <= before.rd)
      },
      test("ratings remain bounded for extreme rating gaps") {
        val state =
          Glicko2.snapshot(alice, rating = 1500, rd = 350) ++
            Glicko2.snapshot(bob, rating = 5000, rd = 50)
        val after = Glicko2.updateAfterGame(state, alice, bob, GameWinner.PlayerA)
        val a = Glicko2.ratingOf(after, alice)
        val b = Glicko2.ratingOf(after, bob)
        assertTrue(
          a.rating.isFinite,
          b.rating.isFinite,
          a.rating > 1500,
          b.rating < 5000,
          a.rd >= 0,
          b.rd >= 0
        )
      }
    )
  )
}
